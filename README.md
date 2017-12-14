# redesign
Redesign for TorXakis

## Compilation

Warnings are treated as errors, however this might be inconvenient when
developing. To suppress the warnings while in the middle of developing a new
feature you can compile with the following option:

```sh
stack build --ghc-options -Wno-all
```

## Visualizing the dependencies

At this stage (redesign) it is important to keep an eye on the intra-module
dependencies.
See
[this article](http://evelinag.com/blog/2014/06-09-comparing-dependency-networks/#.WjK62PZrxhE) for
some guidelines on structuring functional projects.

To visualize the dependency graph, make sure to install `graphmod`:

```sh
stack install graphmod
```

and then generate the dependency graph by typing:

```sh
find sys/*/src -name "*.hs" | xargs graphmod -q  -p > torxakis-intra-deps.dot
dot torxakis-intra-deps.dot  -Tsvg -o torxakis-intra-deps.svg
```

for convenience a version of the dependencies is given in the [`docs`](docs/)
folder, but bear in mind that it might be updated. If you find it to be the
case, please regenerate the dependencies.

## Running the examples

Currently there are two examples provided that exercise the ideas developed so
far: `simpleFailingTest` and `simplePassingTest`. You can try this by typing:

```text
stack ghci
>>> simpleFailingTest
-- ... output
>>> simplePassingTest
-- ... output
```
