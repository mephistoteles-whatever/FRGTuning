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

The search tunes the initial values of `tunedCouplings` by solving the flow on a
grid inside `hyperCubeBoundary`, checking sign changes of the chosen observables
at the final RG time, and iteratively shrinking the corridor.

## Argument Contract

```wl
betaFunctions_List
```

The full list of flow equations passed to `NDSolve`. These should be written as
differential equations in the flow time variable, for example:

```wl
{
  g'[t] == betaG[g[t], m[t]],
  m'[t] == betaM[g[t], m[t]]
}
```

The list must include equations for both tuned and non-tuned couplings.

```wl
nonTunedCouplings_List
```

The couplings whose initial values are fixed and not scanned over. These are given
by name, not by initial condition. Typical examples are:

```wl
{g, \[Lambda], y}
```

or, for indexed couplings,

```wl
{g[4], g[6]}
```

Internally these are interpreted as RG-time-dependent objects, e.g. `g -> g[t]` and
`g[4] -> g[4][t]`.

```wl
fixedInitialConditions_List
```

Initial conditions for the non-tuned couplings. These should be ordinary Wolfram
equations at the initial RG time, for example:

```wl
{
  g[0] == 1,
  \[Lambda][0] == 1/10
}
```

The number of such conditions should match the number of non-tuned couplings.

```wl
tunedCouplings_List
```

The couplings whose initial values are searched for. These are again given by
name, not by explicit time-dependent form. Examples:

```wl
{m}
```

or

```wl
{m, \[Xi]}
```

The number of tuned couplings defines the dimension of the tuning problem.

```wl
hyperCubeBoundary
```

The search corridor for the tuned UV parameters, always written as

```wl
{{min1, min2, ...}, {max1, max2, ...}}
```

Its dimension must match `Length[tunedCouplings]`. Examples:

```wl
{{-2}, {2}}
```

for one tuned coupling, or

```wl
{{-2, 0}, {2, 1}}
```

for two tuned couplings.

```wl
parameters
```

Optional replacement rules applied before solving. This is useful when the flow
equations and initial conditions depend on external symbolic parameters. Example:

```wl
search[a -> 3/5]
```

or

```wl
search[<|a -> 3/5, b -> 2|>]
```

## Conventions

- Couplings are entered by name, e.g. `m` or `g[4]`, and are converted internally
  to time-dependent solver variables.
- By default, the tuning observables are the tuned couplings themselves evaluated
  at the final RG time.
- In one tuning dimension the package uses a coarse scan to locate a sign-changing
  bracket and then performs bisection.
- In higher dimensions the package approximates the zero sets of the observables on
  a grid and shrinks the corridor to the intersection of those zero-crossing sets.

## Important Options

```wl
"TimeVariable" -> t
```

The flow-time variable used in the equations.

```wl
"TimeRange" -> {tInitial, tFinal}
```

The RG interval over which the flow is integrated. This must always be specified.

```wl
"TuningObservables" -> Automatic
```

The observables whose sign changes define the fixed-point condition. If left at
`Automatic`, the package uses the tuned couplings themselves.

```wl
"TargetGridSizePerStep" -> n
```

Controls the number of grid points used in each adaptive scan step.

```wl
"RelativeTolerance" -> tol
```

Stopping criterion for the relative width of the current tuning corridor.

```wl
WorkingPrecision -> wp
```

Precision used in `NDSolve` and in the search logic. If `wp` is larger than machine
precision, the package warns when it detects low-precision floating-point input.

## Minimal Example

```wl
search = AdaptiveGridSearch[
  {
    g'[t] == 0,
    m'[t] == 0
  },
  {g},
  {
    g[0] == 1
  },
  {m},
  {{0}, {1}},
  "TimeRange" -> {0, 1},
  "TuningObservables" -> {m[t] - 1/3}
];

result = search[];
```
