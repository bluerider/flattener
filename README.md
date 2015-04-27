# flattener
Source Code Level Flattener for PLT Racket

PLT Racket (raco demod) currently flattens modules in bytecode form. Personally, this fails when trying to demodularize a racket/gui program.

Introducing flattener!

Flattener flattens modules at the source code level by importing non-built-in racket modules (specified as (require <path>)) into a *-mega.rkt file. This can then be compiled with raco make to produce a single bytecode file that's portable across platforms.

Usage:
```
> racket flattener.rkt test_file.rkt
  (returns) test_file-mega.rkt
> raco make test_file-mega.rkt
> racket test_file-mega.rkt
```
