open Core.Std
open Import

let debug = Debug.debug

module Priority = Jobs.Priority

type +'a deferred

type 'a handler =
  { execution_context : execution_context;
    run : 'a -> unit;
  }
and execution_context =
  { block_group : Block_group.t;
    monitor : monitor;
    priority : Priority.t;
  }
and 'a ivar = 'a ivar_cell ref
and 'a ivar_cell =
| Empty
| Empty_one_handler of ('a -> unit) * execution_context
| Empty_many_handlers of 'a handler Bag.t
| Indir of 'a ivar
| Full of 'a
and monitor =
  { name_opt : string option;
    id : int;
    parent : monitor option;
    errors : exn tail;
    mutable has_seen_error : bool;
    mutable someone_is_listening : bool;
  }
and 'a tail =
  { (* [next] points at the tail of the stream *)
    mutable next: 'a next ivar;
  }
and 'a stream = 'a next deferred
and 'a next = Nil | Cons of 'a * 'a stream
;;

module Ivar0 = struct
  type 'a t = 'a ivar

  let create () = ref Empty
  let create_full a = ref (Full a)

  let equal (t : _ t) t' = phys_equal t t'

  let rec sexp_of_t sexp_of_a i =
    match !i with
    | Full v -> Sexp.List [Sexp.Atom "Full"; sexp_of_a v]
    | Empty
    | Empty_one_handler _
    | Empty_many_handlers _ -> Sexp.Atom "Empty"
    | Indir v -> sexp_of_t sexp_of_a v
  ;;

  (* Returns a non-indirected ivar, and path compresses any indirections.  May overflow
     stack if you pass it an ivar chain that is too long. *)
  let rec squash t =
    match !t with
    | Indir r ->
      let r' = squash r in
      (* Don't bother mutating if it didn't change. *)
      if not (phys_equal r r') then t := Indir r';
      r'
    | _ -> t
  ;;

  let peek t =
    match !(squash t) with
    | Indir _ -> assert false (* fulfilled by squash *)
    | Full a -> Some a
    | Empty
    | Empty_one_handler _
    | Empty_many_handlers _ -> None
  ;;

  let is_empty t =
    match !(squash t) with
    | Indir _ -> assert false (* fulfilled by squash *)
    | Full _ -> false
    | Empty
    | Empty_one_handler _
    | Empty_many_handlers _ -> true
  ;;

  let is_full t = not (is_empty t)

end

module Tail = struct
  type 'a t = 'a tail = { mutable next: 'a next ivar }

  let create () = { next = Ivar0.create () }
end

module Monitor = struct
  type t = monitor =
    { name_opt : string option;
      id : int;
      parent : monitor option;
      errors : exn tail;
      mutable has_seen_error : bool;
      mutable someone_is_listening : bool;
    }
  with fields

  let bogus =
    { name_opt = None;
      id = -1;
      parent = None;
      errors = Tail.create ();
      has_seen_error = false;
      someone_is_listening = false;
    }
  ;;

  module Pretty = struct
    type one =
      { name : string option;
        id : int;
        has_seen_error : bool;
        someone_is_listening : bool
      }
    with sexp_of

    type t = one list
    with sexp_of
  end

  let rec to_pretty t =
    let rec loop t ac =
      let ac =
        { Pretty.
          name = t.name_opt;
          id = t.id;
          has_seen_error = t.has_seen_error;
          someone_is_listening = t.someone_is_listening;
        }
        :: ac
      in
      match t.parent with
      | None -> List.rev ac
      | Some t -> loop t ac
    in
    loop t []
  ;;

  let sexp_of_t t = Pretty.sexp_of_t (to_pretty t)

end

module Unregister = struct
  type t = unit -> unit

  let noop () = ()
  let create f = f
  let unregister f = f ()
end

module Clock_event = struct
  (* Clock events start in the [Uninitialized] state just for their creation (because of
     cyclic types), and then are immediately marked as [Waiting] or [Happened], depending
     on the time.  A [Waiting] event will then either [Happen] at the appropriate time,
     or be [Aborted] prior to when it would have [Happened].

     Uninitialized
     |           |
     v           v
     Waiting --> Happened
     |
     v
     Aborted *)
  type t =
    { mutable state : state;
    }
  and state =
  | Uninitialized
  | Aborted
  | Happened
  | Waiting of waiting
  and waiting =
    { event : t Events.Event.t;
      ready : [ `Happened | `Aborted ] Ivar0.t;
    }
  with sexp_of
end

module Scheduler = struct
  type t =
    { jobs : Jobs.t sexp_opaque;
      mutable current_execution_context : execution_context sexp_opaque;
      mutable main_execution_context : execution_context sexp_opaque;
      mutable max_num_jobs_per_priority_per_cycle : int;
      mutable uncaught_exception : exn option;
      mutable num_jobs_run : int;
      mutable cycle_count : int;
      mutable cycle_start : Time.t;
      mutable jobs_left : bool;
      cycle_times : Time.Span.t Tail.t;
      cycle_num_jobs : int Tail.t;
      events : Clock_event.t Events.t;
    }

  let t =
    let bogus_execution_context =
      { block_group = Block_group.bogus;
        monitor = Monitor.bogus;
        priority = Priority.normal;
      }
    in
    let now = Time.epoch in
    { jobs = Jobs.create ();
      current_execution_context = bogus_execution_context;
      main_execution_context = bogus_execution_context;
      max_num_jobs_per_priority_per_cycle = 500;
      uncaught_exception = None;
      num_jobs_run = 0;
      cycle_start = now;
      cycle_count = 0;
      cycle_times = Tail.create ();
      cycle_num_jobs = Tail.create ();
      events = Events.create ~now;
      jobs_left = false;
    }
  ;;

  let invariant () =
    Events.invariant t.events;
    try
      Events.iter t.events ~f:(fun events_event ->
        let event = Events.Event.value events_event in
        let module E = Clock_event in
        match event.E.state with
        | E.Waiting { E. event = events_event'; ready } ->
          assert (phys_equal events_event events_event');
          assert (Ivar0.is_empty ready);
        | _ -> assert false);
    with exn ->
      fail "events problem" (exn, t.events) <:sexp_of< exn * Clock_event.t Events.t >>;
  ;;

  let current_execution_context () = t.current_execution_context

  let set_execution_context execution_context =
    (* Avoid a caml_modify in most cases. *)
    if not (phys_equal execution_context t.current_execution_context) then
      t.current_execution_context <- execution_context;
  ;;

  let add_job execution_context work =
    let job = Job.create (fun () ->
      set_execution_context execution_context;
      work ())
    in
    let priority = execution_context.priority in
    if debug then
      Debug.print "enqueing job %d with priority %s" (Job.id job)
        (Priority.to_string priority);
    Jobs.add t.jobs priority job;
  ;;

end

module Handler = struct
  type 'a t = 'a handler =
    { execution_context : execution_context;
      run : 'a -> unit;
    }

  let create run =
    { execution_context = Scheduler.current_execution_context ();
      run;
    }
  ;;

  let schedule t v = Scheduler.add_job t.execution_context (fun () -> t.run v)

  let priority t = t.execution_context.priority

  let filter t ~f = { t with run = fun a -> if f a then t.run a }

  let prepend t ~f = { t with run = fun a -> t.run (f a) }
end

module Ivar = struct
  include Ivar0

  let read (type a) (t : a t) = (Obj.magic t : a deferred)

  let fill t v =
    let repr = squash t in
    match !repr with
    | Indir _ -> assert false (* fulfilled by squash *)
    | Full _ -> fail "Ivar.fill of full ivar" t <:sexp_of< a t >>
    | Empty -> repr := Full v
    | Empty_one_handler (run, context) ->
      repr := Full v;
      Scheduler.add_job context (fun () -> run v)
    | Empty_many_handlers handlers ->
      repr := Full v;
      Bag.iter handlers ~f:(fun h -> Handler.schedule h v);
  ;;

  let fill_if_empty t v = if not (is_full t) then fill t v

  include Bin_prot.Utils.Make_binable1 (struct
    module Binable = struct
      type 'a t = 'a option with bin_io
    end

    type 'a t = 'a ivar

    let to_binable t = peek t

    let of_binable = function
      | None -> create ()
      | Some a -> create_full a
    ;;
  end)

end

module Deferred = struct
  type 'a t = 'a deferred

  let repr (type a) (deferred : a deferred) = Ivar.squash (Obj.magic deferred : a ivar)

  let peek t = Ivar.peek (repr t)

  let sexp_of_t f t = Ivar.sexp_of_t f (repr t)

  let is_determined t = is_some (peek t)

  let debug_space_leaks = ref None

  let debug_bag_check bag =
    match !debug_space_leaks with
    | None -> ()
    | Some bound -> assert (Bag.length bag < bound)
  ;;

  let install_removable_handler =
    (* Don't pass 'cell', because it may have indirected in the meantime. *)
    let add_to_bag t bag handler =
      let elt = Bag.add bag handler in
      debug_bag_check bag;
      (* Invariant: Empty_many_handlers is never demoted to anything but Full. *)
      Unregister.create (fun () ->
        let cell = repr t in
        match !cell with
        | Empty_many_handlers bag' -> Bag.remove bag' elt
        | Full _ -> ()
        | _ -> assert false
      );
    in
    fun t handler ->
      let cell = repr t in
      match !cell with
      | Empty ->
        let bag = Bag.create () in
        cell := Empty_many_handlers bag;
        add_to_bag t bag handler;
      | Empty_one_handler (run, execution_context) ->
        let bag = Bag.create () in
        cell := Empty_many_handlers bag;
        ignore (Bag.add bag { execution_context; run });
        add_to_bag t bag handler;
      | Empty_many_handlers bag -> add_to_bag t bag handler
      | Full v ->
        let still_installed = ref true in
        let handler = Handler.filter handler ~f:(fun _ -> !still_installed) in
        Handler.schedule handler v;
        Unregister.create (fun () -> still_installed := false);
      | Indir _ -> assert false (* fulfilled by repr *)
  ;;

  let upon' t run = install_removable_handler t (Handler.create run)

  (* [upon] is conceptually the same as

     let upon t f = ignore (upon' t run)

     However, below is a more efficient implementation, which is worth doing because
     [upon] is very widely used and is so much more common than [upon'].  The below
     implementation avoids the use of the bag of handlers in the extremely common case of
     one handler for the deferred. *)
  let upon =
    fun t run ->
      let current_execution_context = Scheduler.current_execution_context () in
      let cell = repr t in
      match !cell with
      | Indir _ -> assert false (* fulfilled by repr *)
      | Full v -> Scheduler.add_job current_execution_context (fun () -> run v)
      | Empty -> cell := Empty_one_handler (run, current_execution_context)
      | Empty_one_handler (run', execution_context') ->
        let bag = Bag.create () in
        ignore (Bag.add bag { run;
                              execution_context = current_execution_context;
                            });
        ignore (Bag.add bag { run = run';
                              execution_context = execution_context';
                            });
        cell := Empty_many_handlers bag;
      | Empty_many_handlers bag ->
        ignore (Bag.add bag { run;
                              execution_context = current_execution_context;
                            });
   ;;

  (* [connect] takes an Ivar [ivar] and a Deferred [tdef] and, subject to some side
     conditions, makes [tdef] point to the destination of [ivar] as an [Indirection]. *)
  let connect ivar tdef =
    (* ivar and tdef can be chains upon entry to connect, since tdef is an arbitrary
       user-supplied deferred from the right-hand side of connect, and ivar is returned to
       the user prior to being used in the callback, and may be converted to an
       indirection in the case of right-nested binds. *)
    let i = Ivar.squash ivar in
    let t = repr tdef in
    (* i and ivar are the same internally, but have different types *)
    if not (phys_equal i t) then begin
      (* Strange order :-/ *)
      begin match !i, !t with
      | _ , Empty -> t := Indir ivar
      | _, Full v -> Ivar.fill ivar v
      | _, Indir _ -> assert false (* fulfilled by repr *)
      | Empty, tc -> (* do a swap *) i := tc; t := Indir ivar
      (* [connect] is only used in bind, whose ivar is only ever exported as a read-only
         deferred.  Thus, it is impossible for the client to fill the [ivar]. *)
      | Full _, _ -> assert false
      | Indir _, _ -> assert false (* fulfilled by Ivar.squash *)
      (* Tricky cases now. Assume invariant that bags are never shared. *)
      | Empty_one_handler (run1, ec1), Empty_one_handler (run2, ec2) ->
        let bag = Bag.create () in
        ignore (Bag.add bag { run = run1; execution_context = ec1});
        ignore (Bag.add bag { run = run2; execution_context = ec2});
        debug_bag_check bag;
        i := Empty_many_handlers bag;
        t := Indir ivar
      | Empty_many_handlers bag, Empty_one_handler (run, execution_context) ->
        ignore (Bag.add bag { run; execution_context });
        debug_bag_check bag;
        (* no need to rewrite [i], as it's a bag *)
        t := Indir ivar
      | Empty_one_handler (run, execution_context), Empty_many_handlers bag ->
        let handler = { run; execution_context } in
        ignore (Bag.add bag handler);
        debug_bag_check bag;
        i := !t; (* do a swap *)
        t := Indir ivar
      | Empty_many_handlers bag_i, Empty_many_handlers bag_t ->
        Bag.transfer ~src:bag_t ~dst:bag_i;
        debug_bag_check bag_i;
        t := Indir ivar
      end;
      (* Squash original tdef reference; otherwise, it may grow by two indirections rather
         than one.  This is the key step that eliminates any references to [ivar] if it
         was an indirection. *)
      ignore (repr tdef)
    end;
  ;;

  let create f =
    let result = Ivar.create () in
    f result;
    Ivar.read result;
  ;;

  let bind t f = create (fun i -> upon t (fun a -> connect i (f a)))

  let return a = Ivar.read (Ivar.create_full a)

(* PROOF OF BIND SPACE PERFORMANCE PROPERTIES

   Theorem: A bind-allocated Ivar allocated by [m >>= f] is garbage
   collected after [m] becomes available and all its callbacks are
   executed, if this bind was executed in a context where it was in
   tail-call position within the right-hand side of an outer bind.

   We divide this proof into two lemmas, the first of which establishes
   a tight condition for garbage collection of the bind-allocated Ivar
   given no outside references, and the second of which establishes a
   (mostly syntactic) program property that is sufficient (though
   not necessary) to fulfill these conditions.

   -------------------------------------------------------------------------------

   Definition: An Ivar [i] is an "root" if [!i] does not pattern match with
   [Indir _].  Otherwise, the Ivar is an "indirection".

   Definition: An IVar [i] is "full" if [!i] pattern matches with [Full _].
   Full Ivars are also roots.

   Definition: A "bind-allocated Ivar" is an Ivar allocated by the call
   to [create] in the function body of [bind].

   -------------------------------------------------------------------------------

   Lemma: Indirection chains are acyclic, that is, for all Ivars [i], and given
   [f i = match !(Ivar.squash_raw i) with Indir i' = i' | _ = i], there
   exists no natural number n such that f applied n times to i
   (e.g. [f (f (f ... (f i)))]), is physically equal to i.

   Proof: Induction on runtime invocations [Indir] allocating functions
   [Ivar.squash] and [connect].  We show that, if there all indirections are
   acyclic, after executing these functions, all indirections are acyclic.

   Case [Ivar.squash]: We have by simple induction that [Ivar.squash t] returns
   a root Ivar of an indirection chain (guaranteed to be acyclic by outer induction
   hypothesis.) Mutating an Ivar to point to a root Ivar does not create a cycle unless
   unless that Ivar was a root, but this contradicts the pattern match.

   Case [connect]: All allocated indirections will point to [ivar], so we consider
   only this case.  Can we say [ivar] is always a root Ivar?  Unfortunately no; after
   invocation of [Ivar.squash], we are only guaranteed it is an indirection to a root
   Ivar.  Mutating [t] (a root) to point to this Ivar does not create a cycle unless
   [Ivar.squash ivar == t] (that is, [t] is [ivar]'s root Ivar.)  However, this contradicts
   the [phys_equal t i] test.

   For the base case, there are no Ivars at the beginning of programming execution,
   so all indirections are trivially acyclic. QED.

   -------------------------------------------------------------------------------

   Lemma: If a bind-allocated Ivar [i] is an indirection or full and has no outside
   references to it beyond the reference in the closure [fun a -> connect i (f a)],
   it is garbage collected after this callback for [t] is finished executing.

   Proof: To show that [i] is garbage collected, we must demonstrate all
   references to it are dropped by the end of the execution of the callback.
   As the scheduler drops references to callbacks once they are executed,
   we only need to show that [connect i (f a)] does not introduce any new
   references to [i].  Note that this was not the case for the old implementation
   of bind [f a >>> fun y -> Ivar.fill i y], where [i] is further preserved
   in a newly allocated closure.

   Consider the implementation of [connect], letting [ivar = i] (for sake of
   clarity, we will not refer to [i], which is a different local variable in
   [connect].)  At most one new reference is constructed to [ivar], via the
   instruction [t := Indir ivar].  We now demonstrate the invocation of
   [repr tdef] eliminates this reference to [ivar] if [ivar] was an
   indirection.  [repr] performs path compression, and as
   [t = repr tdef], a call of [repr tdef] will result in
   a recursive call of [repr t].  In the case that a reference was
   retained to [ivar], we know [t] contains an [Indir ivar].  [t] is now overwritten
   with a new indirection, which is the root of this indirection chain.  This
   root cannot be [ivar], as by hypothesis, [ivar] is an indirection.  Thus,
   this reference is eliminated. As there are no other references to [ivar], it
   can be garbage collected. QED.

   -------------------------------------------------------------------------------

   Lemma: A bind-allocated Ivar is an indirection or full and has no outside
   references if its allocating bind is executed in a context where it is in
   tail-call position within the right-hand side of an outer bind.

   Proof: We demonstrate indirection and lack of outside references separately.

   Indirection or full: If the allocating bind is in tail-call position, its return value,
   the allocated Ivar, is passed to the right-hand side of the outer bind. This value is
   passed as the second argument [t] to [connect], which introduces an indirection from [t]
   to [i], unless [t] is full (satisfying our other condition.)  As the indirection
   introduction happens within the enclosing callback, there is no chance for preemption;
   thus, when control returns to the scheduler, this property is fulfilled.

   No outside references: As the bind is in tail-call position, no operations on
   the returned bind can be performed (including saving it or adding an extra
   callback to it) until it is passed as the right-hand argument to the outer bind.
   As the outer bind does not return references to its right-hand argument, no
   outside references are possible. QED. *)
end

module Stream = struct

  type 'a t = 'a stream
  type 'a next_ = 'a next = Nil | Cons of 'a * 'a stream
  type 'a next = 'a next_ = Nil | Cons of 'a * 'a stream

  let next t = t

  let sexp_of_t sexp_of_a t =
    let rec loop d ac =
      match Deferred.peek d with
      | None -> Sexp.List (List.rev (Sexp.Atom "..." :: ac))
      | Some Nil -> Sexp.List (List.rev ac)
      | Some (Cons (a, t)) -> loop t (sexp_of_a a :: ac)
    in
    loop t []
  ;;

  let iter t ~f =
    let rec loop t =
      Deferred.upon (next t) (function
        | Nil -> ()
        | Cons (v, t) -> f v; loop t)
    in
    loop t
  ;;

end

module Execution_context = struct
  type t = execution_context =
    { block_group : Block_group.t;
      monitor : Monitor.t;
      priority : Priority.t;
    }
  with fields, sexp_of
end
