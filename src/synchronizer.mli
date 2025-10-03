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

type 'state synchro

type (_, _) poppers =
  | [] : (unit, _) poppers
  | ( :: ) :
      (?pledge:bool -> 'state synchro -> 'get option) * ('ps, 'state) poppers
      -> ('get * 'ps, 'state) poppers

type (_, _) writers =
  | [] : (unit, _) writers
  | ( :: ) :
      ('write -> 'state synchro -> unit) * ('ws, 'state) writers
      -> ('read * 'ws, 'state) writers

type (_, _) readers =
  | [] : (unit, _) readers
  | ( :: ) :
      ('state synchro -> 'read) * ('rs, 'state) readers
      -> ('read * 'rs, 'state) readers

type ('p, 'w, 'r, 'state) t =
  { poppers : ('p, 'state) poppers
  ; readers : ('r, 'state) readers
  ; writers : ('w, 'state) writers
  ; make_pledge : unit -> unit
  ; end_pledge : unit -> unit
  ; close : unit -> unit
  }

val init :
     < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state > synchro_builder
  -> ('p, 'w, 'r, 'state) t
