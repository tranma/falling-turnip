Falling Turnip is an interactive particle simulation. Like others in the same genre (typically dubbed "falling sand games"), it has some degree of approximation for gravity, fluid flow and alchemical reactions. Unlike the others, it is based entirely on cellular automata and runs in parallel.

A short demo video is available [here](http://youtu.be/hlL9yi2hGx0).

In our simulation, each pixel is a cellular automation and all physical reactions are phrased as cellular automata rules. This approach enables us to take advantage of the massively distributed/parallel nature of cellular automata, however expressing gravity and fluid behaviour as automata rules has proven challenging.

Background
==========

Block Automaton
---------------

Physical phenomena in the game usually affect more than a single cell simultaneously, for example gravity might cause a cell to swap with the one below it. The rules for such effects are best expressed with a block cellular automaton, since they make it relatively simple to write simulation rules that respect conservation.

We partition the 2D lattice into 2x2 blocks that shift along each axis on alternate timesteps - i.e. Margolus neighbourhoods. Each neighbourhood is then transformed based on a pre-computed set of rules that define gravitational and fluid behaviour in the system. The shifting nature of the Margolus neighbourhoods effectively allows more scenarios to be encoded in a much smaller set of rules.

Stencil Convolution
-------------------

Parallelism in our simulation is achieved with parallel arrays provided by Repa - a Haskell library for data parallelism.

The 2D world is represented as a Repa array and transformations are done with stencil convolutions. A stencil provides a cursored focus into one cell in the array and the cells immediately adjacent to it, and thus can be easily made parallel (we are not concerned with how this is done under the hood). In `Step.hs`, `margStencil` defines how all cell values in the Margolus neighbourhood corresponding to the current focus are combined into a single value and placed in the focused cell. This value is used to compute the next state of the focus as affected by gravity, fluids, etc.

Simulation
==========

Gravity and Fluid Behaviour
---------------------------

The core of the gravitational simulation is based on the block automata rules specified in [3], adapted to fall straight down rather then diagonally. In [3], however, the authors are only concerned with heavy particles and light particles, and the displacement of light particles by falling heavy particles. Our simulation, on the other hand, has many more than two discrete levels of weight --- for example, oil falls through air, but floats above water. In order to accommodate this complication, we apply the gravity rules multiple times: First, the heaviest particle in the Margolus neighbourhood is considered to be "heavy" while all other particles are considered "light". Then, the second-heaviest particle is also considered "heavy" and so on. 

As empty space is simply represented as another type of particle, objects that defy gravity (for example steam and fire) can be considered "lighter" than empty space, which causes the empty space to displace them, forcing them upward.

These rules provide behaviour similar to falling grains of sand, however liquids must behave differently. Rather than form piles, liquids should expand to fill any containing vessel. We emulate this physical behaviour by adding additional rules that prevent liquid particles from forming piles. Combined with these rules, the ordinary gravity rules provide convincing fluid-like behaviour.

Alchemical Interactions
-----------------------

Compared to gravity and fluid flow, interaction among elements is straightforward. The alchemical rules are expressed as a binary relation with some probability of succeeding. This randomised behaviour adds some realism to the simulation in the absence of accurate heat and pressure models.

Performance
===========

The novelty of our simulation lies with the use of cellular automata --- a highly distributed/parallel structure to model physics. In this implementation we use data parallelism to take advantage of this property, however it is not difficult to translate the logic to fit in a distributed scenario.

This project also serves as a testament to the effectiveness of data parallelism in general and the Repa library in particular. Adding more processing cores improves performance without any added effort on the part of the programmer.


Notes
=====

Assuming you have GHC and cabal installed (if not, get the Haskell platform [here](http://www.haskell.org/platform/)), to build `falling-turnip` simply go `cabal configure && cabal build`. Alternatively, use `make`:

      cabal update
      cabal install gloss
      cabal install gloss-raster
      cabal install repa-3.2
      cabal install vector
      cabal install random
      cabal install JuicyPixels-repa
      make

Use the following run-time options for optimal performance:

      +RTS -N<number of cores> -qa -qg

For example:

      ./Main +RTS -N7 -qa -qg

The name "Falling Turnip" comes from Repa, which stands for Regular Parallel Arrays, and also means "turnip" in Russian.

Bibliography
============
[1] J. L. Schiff, Cellular Automata: A Discrete View of the World (Wiley Series in Discrete Mathematics and Optimization).

[2] T. Toffoli and N. Margolus, Cellular automata machines: a new environment for modeling. Cambridge, MA, USA: MIT Press, 1987.

[3] F. Gruau and J. T. Tromp, “Cellular gravity,” CWI (Centre for Mathematics and Computer Science), Amsterdam, The Netherlands, The Netherlands, 1999.

[4] G. Keller, M. M. T. Chakravarty, R. Leshchinskiy, S. Peyton Jones, and B. Lippmeier, “Regular, shape-polymorphic, parallel arrays in Haskell,” presented at the Proceedings of the 15th ACM SIGPLAN international conference on Functional programming, New York, NY, USA, 2010, pp. 261–272.

[5] B. Lippmeier and G. Keller, “Efficient parallel stencil convolution in Haskell,” presented at the Proceedings of the 4th ACM symposium on Haskell, New York, NY, USA, 2011, pp. 59–70.

[6] B. Lippmeier, M. Chakravarty, G. Keller, and S. Peyton Jones, “Guiding parallel array fusion with indexed types,” presented at the Proceedings of the 2012 symposium on Haskell symposium, New York, NY, USA, 2012, pp. 25–36.

