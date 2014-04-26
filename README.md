# MVC v1.0.0

Use the `mvc` library to distill concurrent programs into pure and
single-threaded programs using the `Model`-`View`-`Controller` pattern.  The
`mvc` library makes pervasive purity practical.

`mvc` is guided by a single overarching design principle: application
architectures inspired by category theory scale in the large because they
reduce complexity and prevent proliferation of concepts.  Use `mvc` if you wish
to learn how to practically apply theory to tame large applications.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install mvc`
* Read the documentation in the `MVC` module and the `MVC.Prelude` module

Optionally begin from this program skeleton:

    import MVC
    import qualified MVC.Prelude as MVC
    import qualified Pipes.Prelude as Pipes
   
    external :: Managed (View String, Controller String)
    external = do
        c1 <- MVC.stdinLines
        c2 <- MVC.tick 1
        return (MVC.stdoutLines, c1 <> fmap show c2)
   
    model :: Model () String String
    model = asPipe (Pipes.takeWhile (/= "quit"))
        
    main :: IO ()
    main = runMVC () model external

## Features

* *Determinism*: Perform property-based testing on your model (like
  `QuickCheck`)

* *Purity*: Move a substantial amount of your application logic into pure code

* *Semantics*: Equationally reason about your concurrency-free and pure core

* *Best practices*: Statically enforce decoupling

* *Concise API*: Only four types, and 8 primitive functions

* *Elegant semantics*: Use practical category theory

* *Extensive Documentation*: The haddocks contain extensive tips and idioms

## Development Status

The API is stable because I do not plan to generalize the API further. Any
future generalizations will be released as separate libraries.  The goal of this
library is to serve as a stepping stone towards understanding more sophisticated
and general application architectures, so I wish to preserve its simplicity for
pedagogical reasons.

Future development will focus on building an ecosystem of pre-built `View`s and
`Controller`s that applications can use, with a focus on tools useful for
user interfaces and games.

## How to contribute

* Build derived libraries

* Write `mvc` tutorials

## License (BSD 3-clause)

Copyright (c) 2014 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
