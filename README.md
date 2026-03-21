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

## Paclet Install

Install directly from a local checkout:

```wl
PacletInstall["/path/to/FRGTuning"]
```

Or build a distributable paclet archive first:

```wl
PacletBuild["/path/to/FRGTuning"]
```

and then install the resulting `.paclet` file with `PacletInstall`.

For GitHub-based distribution, this repository is set up to attach a built `.paclet`
file to each published GitHub release. Users can then download that release asset
and install it with:

```wl
PacletInstall["/path/to/FRGTuning-x.y.z.paclet"]
```

or directly from the release asset URL:

```wl
PacletInstall["https://github.com/<owner>/FRGTuning/releases/download/vx.y.z/FRGTuning-x.y.z.paclet"]
```

To make the release workflow run in GitHub Actions, define the repository secret
`WOLFRAMSCRIPT_ENTITLEMENTID` with a valid Wolfram license entitlement for CI.

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
