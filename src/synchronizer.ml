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
  | Nil : (unit, _) poppers
  | Cons :
      (?pledge:bool -> 'state synchro -> 'get option) * ('ps, 'state) poppers
      -> ('get * 'ps, 'state) poppers

type (_, 'state) writers =
  | Nil : (unit, _) writers
  | Cons :
      ('write -> 'state synchro -> unit) * ('ws, 'state) writers
      -> ('read * 'ws, 'state) writers

type (_, 'state) readers =
  | Nil : (unit, _) readers
  | Cons :
      ('state synchro -> 'read) * ('rs, 'state) readers
      -> ('read * 'rs, 'state) readers

let rec build_poppers :
    type p w r state.
    < popper : p ; writer : w ; reader : r ; state : state > synchro_builder ->
    (p, state) poppers =
 fun builder ->
  match builder with
  | Init _ -> Nil
  | AddPopper (getter, prev) ->
    let prev_poppers = build_poppers prev in
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
    Cons (popper, prev_poppers)
  | AddWriter (_, prev) -> build_poppers prev
  | AddReader (_, prev) -> build_poppers prev

let rec build_writers :
    type p w r state.
    < popper : p ; writer : w ; reader : r ; state : state > synchro_builder ->
    (w, state) writers =
 fun builder ->
  match builder with
  | Init _ -> Nil
  | AddPopper (_, prev) -> build_writers prev
  | AddWriter (writer, prev) ->
    let prev_writers = build_writers prev in
    let write_fn value synchro =
      Mutex.protect synchro.mutex (fun () ->
          writer value synchro.cond synchro.state)
    in
    Cons (write_fn, prev_writers)
  | AddReader (_, prev) -> build_writers prev

let rec build_readers :
    type p w r state.
    < popper : p ; writer : w ; reader : r ; state : state > synchro_builder ->
    (r, state) readers =
 fun builder ->
  match builder with
  | Init _ -> Nil
  | AddPopper (_, prev) -> build_readers prev
  | AddWriter (_, prev) -> build_readers prev
  | AddReader (reader, prev) ->
    let prev_readers = build_readers prev in
    let read_fn synchro =
      Mutex.protect synchro.mutex (fun () -> reader synchro.state)
    in
    Cons (read_fn, prev_readers)

let rec get_init_fn :
    type p w r state.
    < popper : p ; writer : w ; reader : r ; state : state > synchro_builder ->
    unit -> state =
 fun builder ->
  match builder with
  | Init init_fn -> init_fn
  | AddPopper (_, prev) -> get_init_fn prev
  | AddWriter (_, prev) -> get_init_fn prev
  | AddReader (_, prev) -> get_init_fn prev

let init builder =
  let init_fn = get_init_fn builder in
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
    method poppers = build_poppers builder
    method readers = build_readers builder
    method writers = build_writers builder
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
