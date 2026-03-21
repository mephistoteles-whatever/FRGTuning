# FRGTuning

Wolfram Language package for tuning FRG initial conditions by adaptive grid search over a corridor in coupling space.

## Structure

- `FRGTuning.wl`: package entry point and implementation
- `Kernel/init.wl`: standard Wolfram autoload hook
- `Examples/basic_usage.wl`: minimal example script

## Loading

```wl
Needs["FRGTuning`"]
```

## Main API

```wl
AdaptiveGridSearch[
  betaFunctions,
  nonTunedCouplings,
  fixedInitialConditions,
  tunedCouplings,
  hyperCubeBoundary,
  opts
][parameters]
```

The search tunes the initial values of `tunedCouplings` by solving the flow on a grid inside `hyperCubeBoundary`, checking sign changes of the chosen observables at the final RG time, and iteratively shrinking the corridor.
