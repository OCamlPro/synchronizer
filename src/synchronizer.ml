(* SPDX-License-Identifier: AGPL-3.0-or-later *)

type ('get, 'write) t =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; getter : unit -> 'get option
  ; writer : 'write -> unit
  ; pledges_count : int ref
  ; closed : bool ref
  }

let init getter writer =
  { mutex = Mutex.create ()
  ; cond = Condition.create ()
  ; getter
  ; writer
  ; pledges_count = ref 0
  ; closed = ref false
  }

let new_pledge synchro =
  Mutex.protect synchro.mutex (fun () ->
    incr synchro.pledges_count;
    Condition.broadcast synchro.cond )

let end_pledge synchro =
  Mutex.protect synchro.mutex (fun () ->
    decr synchro.pledges_count;
    Condition.broadcast synchro.cond )

let close synchro =
  Mutex.protect synchro.mutex (fun () ->
    synchro.closed := true;
    Condition.broadcast synchro.cond )

let get ~pledge synchro =
  let rec inner_loop pledge synchro =
    if !(synchro.closed) then None
    else
      match synchro.getter () with
      | None ->
        if !(synchro.pledges_count) = 0 then begin
          (* if someone else was waiting to read, it should try again now *)
          Condition.broadcast synchro.cond;
          None
        end
        else begin
          (* there was nothing to read but there may be more in the future
             we let other threads do stuff
             maybe one of them will write stuff we can read later *)
          Condition.wait synchro.cond synchro.mutex;
          inner_loop pledge synchro
        end
      | next_element ->
        if pledge then incr synchro.pledges_count;
        Condition.broadcast synchro.cond;
        next_element
  in
  Mutex.protect synchro.mutex (fun () -> inner_loop pledge synchro)

let write synchro v =
  Mutex.protect synchro.mutex (fun () ->
    if not !(synchro.closed) then begin
      synchro.writer v
    end;
    Condition.signal synchro.cond )

let rec work_while f q =
  match get ~pledge:true q with
  | None -> ()
  | Some v ->
    Fun.protect
      ~finally:(fun () ->
        (* If the function fails, we must end the pledge we created by `get`
           otherwise there will be a dangling pledge. *)
        end_pledge q )
      (fun () -> f v (write q));
    work_while f q
