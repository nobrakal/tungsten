# Tungsten
[![Build Status](https://travis-ci.com/nobrakal/tungsten.svg?branch=master)](https://travis-ci.com/nobrakal/tungsten)

## Purpose

The `tungsten` library provides facilities to bring [deforestation](https://en.wikipedia.org/wiki/Deforestation_(computer_science)) to any recursive structure _for free_.

The only prerequisite is to define the concerned structure as a [fixed-point](https://en.wikipedia.org/wiki/Fixed_point_(mathematics)) using the provided `Fix` operator from the [`Tungsten.Fix`](src/Tungsten/Fix.hs) module. It also defines a generalization of the `foldr/build` rewrite rule, targeting _catamorphisms_ on any fixed-point structures.

Some examples of use can be seen in:

* [`Tungsten.Structure.List`](src/Tungsten/Structure/List.hs)
* [`Tungsten.Structure.Tree`](src/Tungsten/Structure/Tree.hs)
* [`Tungsten.Structure.Graph`](src/Tungsten/Structure/Graph.hs)

## Is this valid?

See a more-or-less formal proof in [`doc/validity.md`](doc/validity.md).

## Is this working?

Even if valid from a theoritical point of view, does it work?

Some tests can be found in the `test/` directory. They use the great [`inspection-testing` package](http://hackage.haskell.org/package/inspection-testing) to test rewrite-rules firing.

## Is does not work for me!

Be sure to compile with rewrite rules enabled and the `-fspec-constr` flag.

## The name

[Tungsten](https://en.wikipedia.org/wiki/Tungsten) is the metal with the highest fusion (or melting) point.
