Fern
====

__These docs are unfinished.__

A poor-man's Erlang, in Common Lisp. A quick rundown:

Fern gives you "processes." A process is a lightweight execution container. It
is mainly run via message passing. It executes all IO operations asynchronously,
never blocking (this is done using [cl-async](http://orthecreedence.github.io/cl-async)).

A scheduler is an object that runs in its own thread and looks processes that
need to be run. It grabs processes off a run queue and runs them. It makes sense
to create as many schedulers as you have cores.

A process is run by sending a message to it, either from your main thread or
from another process.

This architecture sort of models after Erlang, which some key differences:

- Erlang is better. Why? Because it runs in a VM that is able to intelligently
switch between processes to make sure none of them hog a CPU core. This *cannot*
be done in CL without implementing a VM on top of it. So don't run blocking code
inside of a Fern process because Fern can't save you if you do!
- Erlang has more of the features you want. I'm building this with almost no
knowledge of Erlang save for a handful of tutorials. I'm probably missing some
really important stuff.
- I haven't built anything with Fern *yet* so I can't tell you if it's great or
if it sucks.

Documentation
-------------

Ok now to the important stuff.

### scheduler (class)
This is an opaque object passed back by [create-scheduler](#create-scheduler).

It can be passed to [stop-scheduler](#stop-scheduler) to stop it.

### create-scheduler
```lisp
(defun create-scheduler (&key (name (random 999))))
  => scheduler
```

Create a new scheduler. This spawns a new (OS-level) thread which will check the
run queue continuously for processes that need running. Can be named for
convenience.

### stop-scheduler
```lisp
(defun stop-scheduler (scheduler &key force))
  => nil
```

Stops a scheduler from running any more processes. If `:force t` is passed, the
actual thread the scheduler owns will be killed, otherwise it is allowed to
finish the process it's working on and return gracefully.

### process (class)
This is an opaque object passed back by a few functions. Test for a process via
[processp](#processp).

### define-process
```lisp
(defmacro define-process (process-type (bind-process) &body body))
```

Defines a process *type*. A process type can be spawned multiple times. When a
process spawns, it gets a unique ID assigned to it, allowing easier message
passing to it.

Example:

```lisp
;; define a simpl process that prints whatever we send to it
(define-process printer (process)
  (receive process
    ((x) (format t "~a~%" x))))
```

### with-messages
```lisp
```

### receive

### spawn
```lisp
(defmacro spawn (process-type &optional (args nil args-supplied-p)))
  => process
```

Spawns a new instance of a process defined by [define-process](#define-process),
and returns the process object.

If `args` is specified, a message will be sent to the process with those args
on instantiation.

Example:

```lisp
;; spawn printer (but don't send it any messages)
(spawn printer)

;; spawn printer and print an initial message (note args are in a list)
(spawn printer ("printer reporting for duty"))
```

### terminate
```lisp
(defun terminate (process))
  => nil
```

Terminate a process. Doesn't actually stop the process from doing what it's
doing (if anything), but does mark the process as inactive, meaning it can't
process any new tasks.


### send
```lisp
(defun send (process &rest args))
  => nil
```

Send a message to a process. A message can consist of an arbitrary number of
arguments, and `process` can be a [process](#process-class), a process ID, or a
[process name](#register).

Messages are sent asynchronously, and sending marks the receiving process as
active, meaning it will be put into the run queue and grabbed by a schedule.

### processp
