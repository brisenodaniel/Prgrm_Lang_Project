### Concurrency in Haskell

Concurrency is often thought of as the same thing as parallelism. There are distinct differences between these two computing techniques.


 ```Parallelism```: Usually trying to improve performance by running a Haskell program on multiple processors.

```Concurrency```: implementing a program by using multiple I/O-performing threads. While a concurrent Haskell program can run on a parallel machine, the primary goal of using concurrency is not to gain performance, 
but rather because that is the simplest and most direct way to write the program. Since the threads perform I/O, the semantics of the program is necessarily non-deterministic.

GHC supports both concurrency and parallelism. It has a parallel garbage collector as well.

