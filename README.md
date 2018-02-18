# brainfuck

A brainfuck interpreter written in haskell as my first "practical" program. This interpreter supports arbitrary choice of cell size, including any finite size and unbounded. This interpreter has a doubly infinite tape.

## Building

The easiest way to build the interprete is to clone the reprository and then issue the command

    stack build

This requires an installation of the Stack build tool.

Building via Cabal should work but is untested.

## Running

The interpreter takes a filename and optionally a cell size as arguments. Example invocations are

    ./brainfuck 8 helloworld.bf
    ./brainfuck primes.bf

Not including a cell size will mean that unbounded cells are used
