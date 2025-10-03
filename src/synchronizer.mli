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

type _ poppers =
  | [] : unit poppers
  | ( :: ) :
      (?pledge:bool -> unit -> 'get option) * 'ps poppers
      -> ('get * 'ps) poppers

type _ writers =
  | [] : unit writers
  | ( :: ) :
      ('write -> unit) * 'ws writers
      -> ('write * 'ws) writers

type _ readers =
  | [] : unit readers
  | ( :: ) :
      (unit -> 'read) * 'rs readers
      -> ('read * 'rs) readers

type ('p, 'w, 'r) t = Synchro :
  { poppers : 'p poppers
  ; readers : 'r readers
  ; writers : 'w writers
  ; make_pledge : unit -> unit
  ; end_pledge : unit -> unit
  ; close : unit -> unit
  } -> ('p, 'w, 'r) t

val init :
     < popper : 'p ; writer : 'w ; reader : 'r ; state : 'state > synchro_builder
  -> ('p, 'w, 'r) t
