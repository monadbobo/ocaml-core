open Core.Std
open Import

module Result = struct
  type ('a, 'b) t =
  | Input_closed
  | Input_closed_in_the_middle_of_data of ('a, 'b) Unpack_buffer.t
  | Output_closed of 'a Queue.t * ('a, 'b) Unpack_buffer.t
  | Unpack_error of Error.t
  with sexp_of

  let to_error = function
    | Input_closed -> Error.of_string "input closed"
    | Input_closed_in_the_middle_of_data _ ->
      Error.of_string "input closed in the middle of data"
    | Output_closed _ -> Error.of_string "output closed"
    | Unpack_error error -> Error.create "unpack error" error <:sexp_of< Error.t >>
  ;;
end

let unpack_from_reader unpack_buffer reader =
  let pipe_r, pipe_w = Pipe.create () in
  let stop a = return (`Stop a) in
  let handle_chunk buf ~pos ~len =
    match Unpack_buffer.feed unpack_buffer buf ~pos ~len with
    | Error error -> stop (Result.Unpack_error error)
    | Ok () ->
      match Unpack_buffer.unpack unpack_buffer with
      | Error error -> stop (Result.Unpack_error error)
      | Ok output_queue ->
        if Pipe.is_closed pipe_w then
          stop (Result.Output_closed (output_queue, unpack_buffer))
        else if Queue.is_empty output_queue then
          return `Continue
        else begin
          Pipe.write' pipe_w output_queue
          >>| fun () ->
          `Continue
        end
  in
  let result =
    Reader.read_one_chunk_at_a_time_until_eof reader ~handle_chunk
    >>| fun res ->
    Pipe.close pipe_w;
    match res with
    | `Stopped result -> result
    | `Eof ->
      match Unpack_buffer.is_empty unpack_buffer with
      | Error error -> Result.Unpack_error error
      | Ok true -> Result.Input_closed
      | Ok false -> Result.Input_closed_in_the_middle_of_data unpack_buffer
  in
  (pipe_r, result)
;;

let unpack_bin_prot_from_reader bin_prot_reader reader =
  unpack_from_reader (Unpack_buffer.create_bin_prot bin_prot_reader) reader
;;
