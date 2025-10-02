(** This module offers a primitive which allows several threads to synchronize "getting" and "writing" work units to a common state keeper. 

The main idea of this module, is that it allows one to (more) easily go from a sequential algorithm to a parralel one.

{2 "Get-work-to-do/write-work-done" sequential algorithms}

The class of algorithms that this module helps make parallel are those that are built around a central "work to do" structure, and in a loop:

{ol
  {- {i get} one work unit from the "work to do" pool }
  {- Process it}
  {- During processing, discover updates to {i write} to the pool, and do so}
  {- End the processing of this work unit}
}

Most, if not all algorithm built around queues, stacks, and priority queues can be formulated this way: {i getting} a work unit is simply poping from the queue, and {i writing} updates is simply pushing new work units. However, this algorithm is more general: for example the work pool could be a tree, {i getting} a new unit of work would entail some search of the tree, and {i writing} updates would be adding nodes to the tree, or marking existing ones.

{2 Turning it into a parallel algorithm}

{3 Addings "work in progress" semantics to the work pool}

The first step to turn this sequential algorithm into a parallel one is to add a notion of "ongoing work" to our central data structure. When sequential, one cannot observe a work pool where some items are still being worked on, because each (sequential) iteration of the loop is the processing of exactly one work unit. Once parallel, the work pool should also (in most cases) encode a notion of the work tasks being processed.

{3 Creating a synchronizer}

Once it is done, the user should write two closures (notice that these two closures do not take the work pool as arguments, because they are closure, it is directly part of their environment).

{[
getter: unit -> get option
]}

[getter] returns the next work unit, or None if their is (currently) no work unit to be scheduled. This is only {i currently} because another thread may later enqueue some new work unit. The work pool does not have to deal with how many threads are still live this this done by the synchronizer itself (it is actually its main added value). The type [get] is the one of the work unit.


{[
writer: write -> Condition.t -> unit
]}

[writer] takes an update to apply to the work pool, of type [write], and a condition variable. If this update makes a single new work unit available, the condition variable should be signaled with [Condition.signal]. If more than one unit of work are made available, it should be broadcast-signaled with [Condition.broadcast].


Finally, one can create the synchronizer with 
{[
synchronizer = init getter writter
]}


{3 The notion of pledges}

As mentionned before, when going to non-sequential, the main difference is that work can now be "on going": threads mais hence still {i write} updated while processing a work unit. Knowing whether a work pool is empty is hence not sufficient to know whether or not one should stop the loop: we must also know whether someone else may re-enqueue new work. The synchronizer supports this via {i pledges}. A {i pledge} is made to indicate that new {i updates} may be done to the work pool. Each started pledge must be ended once the current work unit won't lead to more updates (corresponding to the end of the loop in the sequential algorithm).

{3 Rewriting the main loop}

TODO

*)

type (!'get, !'write) t

val init :
  (unit -> 'get option) -> ('write -> Condition.t -> unit) -> ('get, 'write) t

val get : ('get, 'write) t -> bool -> 'get option

val write : 'write -> ('get, 'write) t -> unit

val make_pledge : ('get, 'write) t -> unit

val end_pledge : ('get, 'write) t -> unit

val fail : ('get, 'write) t -> unit

val work_while : ('get -> ('write -> unit) -> unit) -> ('get, 'write) t -> unit
