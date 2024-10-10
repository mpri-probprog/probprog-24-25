# Build Your Own Probabilistic Programming Language

## Install

The easiest way to install all the dependencies is via opam:

```
opam install . --deps-only
```

You can then test your installation with a simple:

```
dune build
```

Or try an example with:
```
dune exec ./examples/funny_bernoulli.exe
```

## Organization

The `Byoppl` library contains the following modules

- `Distribution`: Library of probability distributions and basic statistical functions.
- `Basic` (TODO): Basic inference with rejection sampling, importance sampling, and MCMC.
- `Infer` (TODO): Inference on Continuation Passing Style (CPS) models.
- `Cps_operators`: Syntactic sugar to write CPS style probabilistic models.

Examples can be found in the `examples` directory.
