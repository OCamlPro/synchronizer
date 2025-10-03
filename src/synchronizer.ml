(* SPDX-License-Identifier: AGPL-3.0-or-later *)

type _ synchro_builder =
  | Init :
      (unit -> 'state)
      -> < popper : unit ; writer : unit ; reader : unit ; state : 'state >
         synchro_builder
  | AddPopper :
      ('state -> 'get option)
      * < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state >
        synchro_builder
      -> < popper : 'get * 'p ; writer : 'w ; reader : 'r ; state : 'state >
         synchro_builder
  | AddWriter :
      ('write -> Condition.t -> 'state -> unit)
      * < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state >
        synchro_builder
      -> < popper : 'p ; writer : 'write * 'w ; reader : 'r ; state : 'state >
         synchro_builder
  | AddReader :
      ('state -> 'read)
      * < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state >
        synchro_builder
      -> < popper : 'p ; writer : 'w ; reader : 'read * 'r ; state : 'state >
         synchro_builder

type 'state synchro =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; state : 'state
  ; mutable pledges : int
  ; mutable closed : bool
  }

type (_, 'state) poppers =
  | [] : (unit, _) poppers
  | ( :: ) :
      (?pledge:bool -> 'state synchro -> 'get option) * ('ps, 'state) poppers
      -> ('get * 'ps, 'state) poppers

type (_, 'state) writers =
  | [] : (unit, _) writers
  | ( :: ) :
      ('write -> 'state synchro -> unit) * ('ws, 'state) writers
      -> ('read * 'ws, 'state) writers

type (_, 'state) readers =
  | [] : (unit, _) readers
  | ( :: ) :
      ('state synchro -> 'read) * ('rs, 'state) readers
      -> ('read * 'rs, 'state) readers

let rec build_all :
    type p w r state.
    < popper : p ; writer : w ; reader : r ; state : state > synchro_builder ->
    (unit -> state) * (p, state) poppers * (w, state) writers * (r, state) readers
    =
 fun builder ->
  match builder with
  | Init init_fn -> (init_fn, [], [], [])
  | AddPopper (getter, prev) ->
    let init_fn, prev_poppers, prev_writers, prev_readers = build_all prev in
    let popper ?(pledge = true) synchro =
      let rec inner_loop pledge synchro =
        match getter synchro.state with
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
    in
    (init_fn, popper :: prev_poppers, prev_writers, prev_readers)
  | AddWriter (writer, prev) ->
    let init_fn, prev_poppers, prev_writers, prev_readers = build_all prev in
    let write_fn value synchro =
      Mutex.protect synchro.mutex (fun () ->
          writer value synchro.cond synchro.state)
    in
    (init_fn, prev_poppers, write_fn :: prev_writers, prev_readers)
  | AddReader (reader, prev) ->
    let init_fn, prev_poppers, prev_writers, prev_readers = build_all prev in
    let read_fn synchro =
      Mutex.protect synchro.mutex (fun () -> reader synchro.state)
    in
    (init_fn, prev_poppers, prev_writers, read_fn :: prev_readers)

let init builder =
  let init_fn, poppers, writers, readers = build_all builder in
  let synchro =
    { mutex = Mutex.create ()
    ; cond = Condition.create ()
    ; state = init_fn ()
    ; pledges = 0
    ; closed = false
    }
  in
  object
    method synchro = synchro
    method poppers = poppers
    method readers = readers
    method writers = writers
  end

let make_pledge synchro =
  Mutex.lock synchro.mutex;
  synchro.pledges <- synchro.pledges + 1;
  Mutex.unlock synchro.mutex

let end_pledge synchro =
  Mutex.lock synchro.mutex;
  synchro.pledges <- synchro.pledges - 1;
  if Int.equal synchro.pledges 0 then Condition.broadcast synchro.cond;
  Mutex.unlock synchro.mutex

let close synchro =
  Mutex.lock synchro.mutex;
  synchro.closed <- true;
  Condition.broadcast synchro.cond;
  Mutex.unlock synchro.mutex
