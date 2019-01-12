# SOL - Quick and dirty proof of concept s-expression -> C++ compiler

Some people seemed interested so here it is. Nothing too interesting going on here. This is a racket program with main.rkt as the entry point.

\*.sol files give example inputs. Read the code (and its outdated comments) for more. Or ask questions.

It is capable of subclassing C++ classes and inserting arbitrary native strings. It has a basic module system and generates separate header and source files (no cheney-on-the-mta or other strange strategy).

Memory is not handled and it doesn't properly deal with objects and references yet (both for generation and deletion generation for classes - but there is a stub for the latter).

The next step, if I ever pick this up again, will be the modify apply to accept classes to be able to actually interact with them in the style of `let ((k (make-<point3D> 3 4 5))) (if (> (k x) 5) (k (some-function-of-k 'arg1 'arg2)) ...`.

After that and some cleanup, and macros, there should be enough to start building up core functions.
