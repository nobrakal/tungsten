# Tungsten
[![Build Status](https://travis-ci.com/nobrakal/tungsten.svg?branch=master)](https://travis-ci.com/nobrakal/tungsten)

## Purpose

The `tungsten` library provides facilities to bring [deforestation](https://en.wikipedia.org/wiki/Deforestation_(computer_science)) to any recursive datatype _for free_.

The only prerequisite is to define the concerned datatype as a [fixed-point](https://en.wikipedia.org/wiki/Fixed_point_(mathematics)) using the provided `Fix` operator from the `Tungsten.Fix` module.

Some examples of use can be seen in:

* `Tungsten.Structure.List`
* `Tungsten.Structure.Tree`

## Is this working?

### Proof

See a more-or-less formal proof in `doc/validity.md`.

### Tests

Even if valid from a theoritical point of view, does it work?

Some tests can be found in the `test/` directory. They use the great [`inspection-testing` package](http://hackage.haskell.org/package/inspection-testing) to test rewrite-rules firing.

## The name

[Tungsten](https://en.wikipedia.org/wiki/Tungsten) is the metal with the highest fusion (or melting) point.
