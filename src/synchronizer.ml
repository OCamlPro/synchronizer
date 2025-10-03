(* SPDX-License-Identifier: AGPL-3.0-or-later *)

type ('get, 'write) t =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; getter : unit -> 'get option
  ; writer : 'write -> Condition.t -> unit
  ; mutable pledges : int
  ; mutable closed : bool
  }

let init getter writer =
  { mutex = Mutex.create ()
  ; cond = Condition.create ()
  ; getter
  ; writer
  ; pledges = 0
  ; closed = false
  }

let get ?(pledge = true) synchro =
  let rec inner_loop pledge synchro =
    match synchro.getter () with
    | None when synchro.pledges = 0 || synchro.closed ->
      Condition.broadcast synchro.cond;
      None
    | None ->
      Condition.wait synchro.cond synchro.mutex;
      inner_loop pledge synchro
    | Some _ as v ->
      if pledge then synchro.pledges <- synchro.pledges + 1;
      v
  in
  Mutex.protect synchro.mutex (fun () -> inner_loop pledge synchro)

let write v { writer; cond; mutex; _ } =
  Mutex.protect mutex (fun () -> writer v cond)

let make_pledge synchro =
  Mutex.lock synchro.mutex;
  synchro.pledges <- synchro.pledges + 1;
  Mutex.unlock synchro.mutex

let end_pledge synchro =
  Mutex.lock synchro.mutex;
  synchro.pledges <- synchro.pledges - 1;
  if Int.equal synchro.pledges 0 then Condition.broadcast synchro.cond;
  Mutex.unlock synchro.mutex

let close q =
  Mutex.lock q.mutex;
  q.closed <- true;
  Condition.broadcast q.cond;
  Mutex.unlock q.mutex

let rec work_while f q =
  match get q with
  | None -> ()
  | Some v ->
    let () = f v (fun v -> write v q) in
    end_pledge q;
    work_while f q
