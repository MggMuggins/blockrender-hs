# Blockrender in Haskell
This is the third working iteration of this program that I've written. It's
my attempt to understand how computer graphics works with little to no formal
training. CSC424 essay below.

The code is a staggeringly complex mess... While the output looks like I want it
to, I'm not sure that I would be able to easily reason about the output given
some different set of surfaces. I'm pretty sure I got the axes mixed up or
backward in some way, I expect this could be a confusing factor.

---

I started out this project with a couple of helpful legs up:
- I had written the program I translated, and still remembered most of the
  important details when I started writing, so I didn't have to overcome the
  same technical and theoretical barriers that I did when I first wrote the
  program, or if I was translating a program I didn't write.
- Rust has some similarities to Haskell, and even though the original program is
  more imperative than some Rust code I've written, there were a lot of basic
  structural ideas that carried over fairly easily.
- The availability of Haskell libraries to do png formatting and vector algebra
  was incredibly helpful: they had essentially the same functionality that the
  Rust libraries did, so there was very little translation needed to shift
  stacks.

Haskell is unlike any language I have ever written code in. While it shares some
big ideas with other languages I've seen (currying in ReasonML), crazy powerful
strong typing and first class functions, the design patterns are so different
that I feel like I'm almost starting from square one with programming again (the
code really obviously reflects that...). One of the things that made Haskell
hard is that I was unabled to reason about my code by the way data is actually
represented on the machine (or some heuristic for the machine representation).
My unfamiliarity with the type system and the insistence on functional purity
meant that all bets were off when I tried to think about what data exactly was
going on: I suspect that laziness also made it more difficult to visualize my
data structures.

The syntactic differences played in to this problem as well: it took me a while
to get the hang of pattern matching instead of named parameters. I organize my
thoughts around inputs and outputs to functions, and when functions become
parameters in ways that are non-trivial (`vertexPixels` or `renderBounds` in the
code) I get very confused very quickly. With lots of higher-order functions and
partial applications flying around I had a hard time reasoning about what inputs
and outputs I was working with, let alone what to do with them.

My inconsistent naming scheme (if you can call it a scheme) did not help keep my
thoughts organized either. My inexperience with the problem space combined with
lack of idomatic Haskell naming knowledge resulted in a lot of poor names for
functions and variables. One of the big questions I'm left with is how Haskell
programmers manage complexity with the names of functions and types.

I've been writing code in Rust long enough that I feel comfortable with the
idioms and have a nice set of patterns that I use for solving problems. I found
my lack of experience with Haskell idioms more frustrating that I was expecting.
There were several times where I felt like there was probably a better way of
doing something but I didn't know enough of the idioms to figure it out (looking
at you, monads). However, I was able to apply the `Functor` and `Foldable`
typeclasses some, which did feel like a reasonable solution to eliminating
boilerplate. I remember writing a lot of boilerplate when solving this problem
in Rust.

There were other things I quite enjoyed in the development process: I very often
wrote code that worked or sorta worked the first time if it compiled. I'm sure I
spent less time debugging than I did in Rust. The code is also 224 lines, a
reduction of roughly 50% from the rust version. While being super concise is not
always a plus, it at least made it easier to traverse the (very complicated)
codebase when debugging and working towards a successful compile.

I also made use of one of the Haskell build systems (`cabal`) to include
dependencies: finding and using dependencies was a generally pleasant
experience, and documentation for the packages I used was readily available.
While I still don't have a good picture of where the dependencies are hosted,
I was happy with the experience.

The online documentation was also very helpful: even though the type system is
complicated, it is very possible to read some parts of the standard library
documentation with a beginner's exposure to the type system. I did not rely
much on StackOverflow and was able to use the documentation to produce most of
my code.

Altogether it was really cool to have an excuse to struggle through writing some
Haskell. I might be a bit hesitant to start another project in it because of the
learning curve, but it was good to see a different approach to writing code and
interesting to think about the ways that my design changed when I moved in to
Haskell.

