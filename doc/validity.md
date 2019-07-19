
Here is a more-or-less formal proof that the `cata/buildR` rule from `Tungsten.Fix` is valid.

The needed definitions to understand the following are:

```Haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = c where c = f . fmap c . unfix

type Cata f = forall a. (f a -> a) -> a

buildR :: Cata f -> Fix f
buildR g = g Fix
```

The rule states that:

```
∀ (f :: t b -> b) (g :: Cata t).
cata go (buildR g) = g go
```

The next theorem proves this equality, using the rank-2-types required in `Cata`'s type, but as we show in next section, it is totally valid only without the presence of `seq` and `⊥` (or `undefined`).

## The theorem

Let `F` be a fixed parametric type and a valid instance of `Functor`, and a type `A`.
Let `g` and `go` be functions with types:

- `g :: ∀ B. (F B -> B) -> B`
- `go :: F A -> A`

then, `cata go (buildR g) = g go`.

### Proof

This is a consequence of free theorems of Wadler (from [Theorems for Free!](http://doi.acm.org/10.1145/99370.99404), Proceedings of the Fourth International Conference on Functional Programming Languages and Computer Architecture) as for the `foldr/build` rule (explained in [A Short Cut to Deforestation](http://doi.acm.org/10.1145/165180.165214) by Gill et al. in Proceedings of the Conference on Functional Programming Languages and Computer Architecture).

The free theorem associated with `g`'s type is that, for all types `C` and `D`, `f :: C -> D`, `p :: F C -> C` and `q :: F D -> D`, then we have the following implication:
```
[∀ x :: C. f (p x) = q (fmap f x)]
=> f (g p) = g q
```
Now we can instantiate (using `fix` to denote the unique constructor of the `Fix` type):

- `C = Fix F, D = A`
- `f = cata go`
- `p = fix`
- `q = go`

then we have:

```
[∀ x :: F (Fix F).
 cata go (fix x) = go (fmap (cata go) x)]
=> cata go (g fix) = g go
```

Given the definition of `cata`, the premise trivially holds, so we have the conclusion:

```
cata go (g fix) = g go
```

Given the definition of `buildR`, this is exactly what we required.

## A note on `seq` and `⊥`

Free theorems are known to (partially) fail in presence of `seq` and `⊥` (`undefined` in Haskell). `seq` is defined in the [Haskell report](https://www.haskell.org/onlinereport/haskell2010/) as a function respecting the following equation:

```
seq ⊥ b = ⊥
seq a b = b (if a != ⊥)
```

`seq` is a function forcing the evaluation of its first argument and returning the second. This characterization is sufficient to break our rule (as it also [breaks the `foldr/build` one](https://wiki.haskell.org/Correctness_of_short_cut_fusion)).

Consider the following example:

```Haskell
data BinF a = L | R deriving (Functor)
type Bin = Fix BinF

bad :: Int
bad = cata go $ buildR $ \u -> u L `seq` u R
  where
    go L = undefined
    go R = 0
```

`Bin` is a non-recursive type with two constructors defined in terms of `Fix` (which allow us to use `cata` and `buildR`).

Now consider the `bad` expression: it constructs a `Bin` using `R` and then destructs it, with a partial function, defined only for `R`. Without rewrite rules, the function simply returns `0` (since `u` is replaced by `Fix`). Now with our `cata/buildR` rule, `u` is replaced by `go`, and we precisely force the definition of `go` on `L`, so the whole expression in now `undefined`.

Fortunately, as described by Johann and Voigtländer in [Free Theorems in the Presence of seq](http://www.janis-voigtlaender.eu/JV04.html) (31st Symposium on Principles of Programming Languages, Venice, Italy, Proceedings), one can recover free theorems by requiring more properties on the combination function (like strictness and totality), but this is beyond our scope.
