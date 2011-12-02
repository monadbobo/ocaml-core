INCLUDE "config.mlh"
include Mutex0
IFDEF MUTEX_TIMED_LOCK THEN
(* POSIX thread functions *)
external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"

let timedlock mtx time = mutex_timedlock mtx (Time.to_float time)
ENDIF
