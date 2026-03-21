(* ::Package:: *)

BeginPackage["FRGTuning`"];

AdaptiveGridSearch::usage =
  "AdaptiveGridSearch[betaFunctions, nonTunedCouplings, fixedInitialConditions, tunedCouplings, hyperCubeBoundary, opts][parameters] tunes the initial values of tuned couplings so that the selected tuning observables hit a fixed point at the end of the flow. betaFunctions is the full list of flow equations, nonTunedCouplings names the couplings with fixed initial values, fixedInitialConditions supplies those fixed UV values, tunedCouplings names the UV parameters to be searched over, and hyperCubeBoundary = {{min1, ...}, {max1, ...}} specifies the search corridor. The returned function optionally accepts parameter replacement rules that are applied before solving.";

AdaptiveGridSearch::timerange =
  "Option \"TimeRange\" must be specified as {tInitial, tFinal}.";
AdaptiveGridSearch::shape =
  "The tuning corridor must be given as {{min1, min2, ...}, {max1, max2, ...}}.";
AdaptiveGridSearch::tuned =
  "The number of tuned couplings (`1`) must match the dimension of the tuning corridor (`2`).";
AdaptiveGridSearch::observables =
  "The number of tuning observables (`1`) must match the number of tuned couplings (`2`).";
AdaptiveGridSearch::plot2d =
  "VerbosePlot is only supported for two tuned couplings.";
AdaptiveGridSearch::lowprec =
  "Some inexact numeric inputs have precision below WorkingPrecision (`1`). Consider using exact numbers or higher-precision input.";

Options[AdaptiveGridSearch] = {
  "TimeVariable" -> t,
  "TimeRange" -> Automatic,
  "TuningObservables" -> Automatic,
  "TargetGridSizePerStep" -> 100,
  "MaxIterations" -> 100,
  "RelativeTolerance" -> 10^-1,
  "TubeEpsilon" -> 1,
  "SamplesPerStep" -> 100,
  "SharpGridPower" -> 2,
  WorkingPrecision -> MachinePrecision,
  MaxSteps -> 10^5,
  Method -> Automatic,
  "NDSolveAdditions" -> {},
  "NDSolveOptions" -> {},
  "Verbose" -> False,
  "VerbosePlot" -> False
};

Begin["`Private`"];

ClearAll[
  NormalizeParameterRules,
  NormalizeCoupling,
  NormalizeCouplingList,
  SpacingEqualNumberPointsInAllDirections,
  GridAxes,
  GridPoints,
  finalTime,
  RelativeBoxWidths,
  SameStrictSignQ,
  EdgeCrossingQ,
  BuildIndexTuples,
  GridSearch,
  FindZeroCrossings,
  FindZeroBracket1D,
  LowPrecisionInexactReals,
  SampledPoints,
  InPointCloudQ,
  InIntersectionQ,
  FindEdgesOfIntersection
];

NormalizeParameterRules[params_Association] := Normal[params];
NormalizeParameterRules[params_List] := params;
NormalizeParameterRules[params_Rule] := {params};
NormalizeParameterRules[params_RuleDelayed] := {params};
NormalizeParameterRules[None] := {};
NormalizeParameterRules[Automatic] := {};
NormalizeParameterRules[params_] := params;

(* Convert a user-facing coupling name into the time-dependent object expected by
   NDSolve. Examples: m -> m[t], g[4] -> g[4][t]. Already time-dependent input is
   left unchanged so callers may still pass fully explicit expressions if needed. *)
NormalizeCoupling[coupling_Symbol, timeVar_] := coupling[timeVar];
NormalizeCoupling[coupling_[args___], timeVar_] /; FreeQ[{args}, timeVar] := coupling[args][timeVar];
NormalizeCoupling[coupling_, _] := coupling;

(* Apply NormalizeCoupling elementwise to a list of couplings or observables. *)
NormalizeCouplingList[couplings_List, timeVar_] := NormalizeCoupling[#, timeVar] & /@ couplings;

(* Choose a per-direction grid spacing so that the Cartesian product grid has about
   targetGridSize points while using the same number of points along each axis. *)
SpacingEqualNumberPointsInAllDirections[hyperCubeBoundary_, targetGridSize_] := Module[
  {dimInitCondSpace, pointsPerDirection},
  dimInitCondSpace = Length[hyperCubeBoundary[[1]]];
  pointsPerDirection = Max[2, Ceiling[targetGridSize^(1/dimInitCondSpace)]];
  Table[
    (hyperCubeBoundary[[2, i]] - hyperCubeBoundary[[1, i]])/(pointsPerDirection - 1),
    {i, dimInitCondSpace}
  ]
];

(* Build the explicit coordinate values on each axis of the tuning hypercube. *)
GridAxes[hyperCubeBoundary_, targetGridSize_] := Module[
  {dimInitCondSpace, pointsPerDirection},
  dimInitCondSpace = Length[hyperCubeBoundary[[1]]];
  pointsPerDirection = Max[2, Ceiling[targetGridSize^(1/dimInitCondSpace)]];
  Table[
    Subdivide[
      hyperCubeBoundary[[1, i]],
      hyperCubeBoundary[[2, i]],
      pointsPerDirection - 1
    ],
    {i, dimInitCondSpace}
  ]
];

(* Enumerate all index tuples for a rectangular grid, e.g. {2,3} -> {{1,1},...,{2,3}}. *)
BuildIndexTuples[axisLengths_] := Tuples[Range /@ axisLengths];

(* Materialize the full Cartesian-product grid together with axis metadata used by
   the zero-crossing detection code. *)
GridPoints[hyperCubeBoundary_, targetGridSize_] := Module[
  {axes, axisLengths, indexTuples, points},
  axes = GridAxes[hyperCubeBoundary, targetGridSize];
  axisLengths = Length /@ axes;
  indexTuples = BuildIndexTuples[axisLengths];
  points = Map[MapThread[Part, {axes, #}] &, indexTuples];
  <|
    "Axes" -> axes,
    "AxisLengths" -> axisLengths,
    "IndexTuples" -> indexTuples,
    "Points" -> points
  |>
];

(* Extract the final integration time from the InterpolatingFunction structure
   returned by NDSolve. This indirect access is used for compatibility with kernels
   where the modern domain-query interface is unavailable. *)
finalTime[traj_] := traj[[1, 2, 0, 1, 1, 2]];

(* Measure the relative width of the current search box in each direction. This is
   the convergence criterion for both the 1D bisection path and the multi-D search. *)
RelativeBoxWidths[bound_] := Module[{mins, maxs},
  mins = bound[[1]];
  maxs = bound[[2]];
  Table[
    If[
      mins[[i]] == 0 && maxs[[i]] == 0,
      0,
      2 Abs[maxs[[i]] - mins[[i]]]/(Abs[maxs[[i]]] + Abs[mins[[i]]])
    ],
    {i, Length[mins]}
  ]
];

(* True only when all entries are strictly positive or all are strictly negative.
   A zero entry means we already touched the candidate fixed point and therefore do
   not classify the set as having a strict common sign. *)
SameStrictSignQ[values_] := AllTrue[values, Positive] || AllTrue[values, Negative];

(* Decide whether two neighboring sign values bracket a zero. A literal zero on
   either endpoint is treated as a crossing so exact hits are preserved. *)
EdgeCrossingQ[val1_, val2_] := val1 == 0 || val2 == 0 || val1 val2 == -1;

(* Evaluate all tuning observables on every grid point, after solving the flow to
   the final RG time for that point, and store only their signs. *)
GridSearch[
  gridData_,
  flowFunction_,
  tuningObservables_,
  timeVar_
] := Module[{traj, finalT},
  Table[
    traj = flowFunction[gridData["Points"][[i]]];
    finalT = finalTime[traj];
    Sign /@ (tuningObservables /. traj /. timeVar -> finalT),
    {i, Length[gridData["Points"]]}
  ]
];

(* For each observable, find the grid-edge midpoints where the sign flips between
   neighboring grid points. In more than one dimension these midpoints form a point
   cloud approximating the observable's zero set inside the current corridor. *)
FindZeroCrossings[signGrid_, gridData_] := Module[
  {
    nComp,
    axisLengths,
    indexTuples,
    indexLookup,
    crossingPoints,
    currentTuple,
    neighborTuple,
    currentPos,
    neighborPos
  },
  nComp = Length[First[signGrid]];
  axisLengths = gridData["AxisLengths"];
  indexTuples = gridData["IndexTuples"];
  indexLookup = AssociationThread[indexTuples -> Range[Length[indexTuples]]];
  crossingPoints = Table[{}, {nComp}];

  Do[
    Do[
      If[currentTuple[[dir]] < axisLengths[[dir]],
        neighborTuple = ReplacePart[currentTuple, dir -> currentTuple[[dir]] + 1];
        currentPos = indexLookup[currentTuple];
        neighborPos = indexLookup[neighborTuple];
        Do[
          If[
            EdgeCrossingQ[
              signGrid[[currentPos, comp]],
              signGrid[[neighborPos, comp]]
            ],
            AppendTo[
              crossingPoints[[comp]],
              Mean[
                {
                  gridData["Points"][[currentPos]],
                  gridData["Points"][[neighborPos]]
                }
              ]
            ]
          ],
          {comp, nComp}
        ]
      ],
      {dir, Length[axisLengths]}
    ],
    {currentTuple, indexTuples}
  ];

  DeleteDuplicates /@ crossingPoints
];

(* Specialized 1D bracket detection. We first look for an exact zero on the scan
   grid; if none exists, we return the first neighboring pair that changes sign. *)
FindZeroBracket1D[signGrid_, gridData_] := Module[
  {signs, points, zeroPos, crossingPos},
  signs = Flatten[signGrid];
  points = Flatten[gridData["Points"]];
  zeroPos = FirstPosition[signs, 0, Missing["NotFound"]];
  If[zeroPos =!= Missing["NotFound"],
    Return[{points[[zeroPos[[1]]]], points[[zeroPos[[1]]]]}]
  ];
  crossingPos = FirstCase[
    Range[Length[signs] - 1],
    i_ /; EdgeCrossingQ[signs[[i]], signs[[i + 1]]],
    Missing["NotFound"]
  ];
  If[crossingPos === Missing["NotFound"],
    Missing["NotFound"],
    {points[[crossingPos]], points[[crossingPos + 1]]}
  ]
];

(* Collect inexact real numbers whose stored precision is below the requested
   WorkingPrecision. Exact numbers are intentionally ignored. *)
LowPrecisionInexactReals[expr_, workingPrecision_] := DeleteDuplicates @ Cases[
  Unevaluated[expr],
  x_Real /; Precision[x] < workingPrecision,
  Infinity
];

(* Downsample a point cloud to at most n representative points while keeping the
   endpoints and spreading samples across the cloud. *)
SampledPoints[cloud_, n_: 100] := If[
  Length[cloud] <= n,
  cloud,
  cloud[[Round /@ Subdivide[1, Length[cloud], n]]]
];

(* Predicate that tests whether a point lies within an epsilon-sized tube around a
   sampled zero-crossing cloud, measured in units of the grid spacing. *)
InPointCloudQ[samplePoints_, spacing_, epsilon_][pt_] := AnyTrue[
  samplePoints,
  Norm[(pt - #)/spacing] <= epsilon &
];

(* Predicate that is true only when a point lies simultaneously inside the epsilon
   tube of every observable's sampled zero-crossing cloud. *)
InIntersectionQ[samplePoints_, spacing_, epsilon_: 1/2][pt_] := And @@ (
  InPointCloudQ[#, spacing, epsilon][pt] & /@ samplePoints
);

(* Given sampled zero-crossing clouds for all observables, locate the smallest box
   on a sharpened auxiliary grid that still lies in their common intersection. *)
FindEdgesOfIntersection[
  bound_,
  targetGridSizePerStep_,
  spacing_,
  samplePoints_,
  epsilon_: 1/2,
  sharpgridpower_: 1
] := Module[{sharpgrid, intpoints, coords},
  sharpgrid = GridPoints[bound, targetGridSizePerStep^sharpgridpower]["Points"];
  intpoints = Select[
    sharpgrid,
    InIntersectionQ[samplePoints, spacing, epsilon]
  ];
  If[intpoints === {},
    {{Infinity}, {Infinity}},
    coords = Transpose[intpoints];
    {
      Min /@ coords,
      Max /@ coords
    }
  ]
];

(* Main wrapper. The function is curried so that the expensive structural inputs
   defining the flow are fixed once, and optional parameter substitutions can be
   injected later for repeated scans over the same model family. *)
AdaptiveGridSearch[
  betaFunctions_List,
  nonTunedCouplings_List,
  fixedInitialConditions_List,
  tunedCouplings_List,
  hyperCubeBoundary_,
  opts : OptionsPattern[]
][parameterRules_: {}] := Module[
  {
    timeVar,
    timeRange,
    tInitial,
    tFinal,
    tuningObservables,
    targetGridSizePerStep,
    maxiter,
    reltol,
    epsilon,
    n,
    sharpgridpower,
    workingPrecision,
    maxSteps,
    method,
    ndsolveAdditions,
    ndsolveOptions,
    verbose,
    verbosePlot,
    dim,
    params,
    normalizedNonTunedCouplings,
    normalizedTunedCouplings,
    allCouplings,
    equationsTemplate,
    fixedICTemplate,
    observableTemplate,
    tunedInitialConditions,
    flow,
    computation,
    bound,
    oldbound,
    spacing,
    gridData,
    signGrid,
    zeroCrossings,
    samplePoints,
    relErrors = ConstantArray[Infinity, Length[hyperCubeBoundary[[1]]]],
    errors,
    error,
    LowWorkPrec,
    NoZeroCrossing,
    NoIntersection,
    mean,
    estimatederror,
    observableValue,
    signAt,
    bracket,
    left,
    right,
    leftSign,
    rightSign,
    mid,
    midSign,
    lowPrecisionInputs
  },
  timeVar = OptionValue["TimeVariable"];
  timeRange = OptionValue["TimeRange"];
  targetGridSizePerStep = OptionValue["TargetGridSizePerStep"];
  maxiter = OptionValue["MaxIterations"];
  reltol = OptionValue["RelativeTolerance"];
  epsilon = OptionValue["TubeEpsilon"];
  n = OptionValue["SamplesPerStep"];
  sharpgridpower = OptionValue["SharpGridPower"];
  workingPrecision = OptionValue[WorkingPrecision];
  maxSteps = OptionValue[MaxSteps];
  method = OptionValue[Method];
  ndsolveAdditions = OptionValue["NDSolveAdditions"];
  ndsolveOptions = OptionValue["NDSolveOptions"];
  verbose = OptionValue["Verbose"];
  verbosePlot = OptionValue["VerbosePlot"];

  normalizedNonTunedCouplings = NormalizeCouplingList[nonTunedCouplings, timeVar];
  normalizedTunedCouplings = NormalizeCouplingList[tunedCouplings, timeVar];

  If[timeRange === Automatic || !MatchQ[timeRange, {_, _}],
    Message[AdaptiveGridSearch::timerange];
    Return[$Failed];
  ];

  If[!MatchQ[hyperCubeBoundary, {{__}, {__}}],
    Message[AdaptiveGridSearch::shape];
    Return[$Failed];
  ];

  dim = Length[hyperCubeBoundary[[1]]];

  If[Length[hyperCubeBoundary[[2]]] =!= dim,
    Message[AdaptiveGridSearch::shape];
    Return[$Failed];
  ];

  If[Length[normalizedTunedCouplings] =!= dim,
    Message[AdaptiveGridSearch::tuned, Length[normalizedTunedCouplings], dim];
    Return[$Failed];
  ];

  allCouplings = Join[normalizedNonTunedCouplings, normalizedTunedCouplings];

  tuningObservables = Replace[
    OptionValue["TuningObservables"],
    Automatic -> normalizedTunedCouplings
  ];
  tuningObservables = NormalizeCouplingList[tuningObservables, timeVar];

  If[Length[tuningObservables] =!= dim,
    Message[
      AdaptiveGridSearch::observables,
      Length[tuningObservables],
      dim
    ];
    Return[$Failed];
  ];

  If[verbosePlot && dim =!= 2,
    Message[AdaptiveGridSearch::plot2d];
  ];

  params = NormalizeParameterRules[parameterRules];
  {tInitial, tFinal} = timeRange /. params;
  equationsTemplate = betaFunctions /. params;
  fixedICTemplate = fixedInitialConditions /. params;
  observableTemplate = tuningObservables /. params;
  ndsolveAdditions = ndsolveAdditions /. params;
  ndsolveOptions = ndsolveOptions /. params;

  If[workingPrecision =!= MachinePrecision,
    lowPrecisionInputs = LowPrecisionInexactReals[
      {
        hyperCubeBoundary /. params,
        equationsTemplate,
        fixedICTemplate,
        observableTemplate,
        ndsolveAdditions
      },
      workingPrecision
    ];
    If[lowPrecisionInputs =!= {},
      Message[AdaptiveGridSearch::lowprec, workingPrecision]
    ];
  ];

  tunedInitialConditions[tunedValues_List] := Thread[
    (normalizedTunedCouplings /. timeVar -> tInitial) == tunedValues
  ];

  flow[tunedValues_List] := flow[tunedValues] = NDSolve[
    Join[
      SetPrecision[equationsTemplate, workingPrecision],
      SetPrecision[fixedICTemplate, workingPrecision],
      SetPrecision[tunedInitialConditions[tunedValues], workingPrecision],
      SetPrecision[ndsolveAdditions, workingPrecision]
    ],
    allCouplings,
    {timeVar, tInitial, tFinal},
    WorkingPrecision -> workingPrecision,
    MaxSteps -> maxSteps,
    Method -> method,
    Sequence @@ ndsolveOptions
  ][[1]];

  observableValue[tunedValues_List] := Module[{traj, finalTLocal},
    traj = flow[tunedValues];
    finalTLocal = finalTime[traj];
    First[observableTemplate /. traj /. timeVar -> finalTLocal]
  ];

  (* The 1D case is handled separately: first find a sign-changing bracket on a
     coarse scan, then refine it by ordinary bisection. This is cheaper and clearer
     than running the full multidimensional point-cloud intersection logic. *)
  signAt[value_] := Sign[observableValue[{value}]];

  If[dim == 1,
    bound = N[hyperCubeBoundary /. params, workingPrecision + 10];
    gridData = GridPoints[bound, targetGridSizePerStep];
    signGrid = GridSearch[gridData, flow, observableTemplate, timeVar];

    If[SameStrictSignQ[Flatten[signGrid]],
      Return["Error: No zero-crossing detected for at least one observable"]
    ];

    bracket = FindZeroBracket1D[signGrid, gridData];

    If[bracket === Missing["NotFound"],
      Return["Error: No zero-crossing detected for at least one observable"]
    ];

    {left, right} = N[bracket, workingPrecision];
    leftSign = signAt[left];
    rightSign = signAt[right];

    If[leftSign == 0 || rightSign == 0,
      mean = {If[leftSign == 0, left, right]};
      estimatederror = {0};
      If[verbose,
        Print["Successful."];
        Print["Mean:"];
        Print[N[mean, workingPrecision]];
        Print["Estimated error:"];
        Print[N[estimatederror, workingPrecision]];
      ];
      Return[{mean, estimatederror}]
    ];

    Do[
      mid = SetPrecision[(left + right)/2, workingPrecision];

      If[mid == left || mid == right,
        Print["Warning: Noise reached the hypercube edge size. Consider increasing WorkingPrecision."];
        mean = {(left + right)/2};
        estimatederror = {Abs[right - left]/2};
        If[verbose,
          Print["Relative errors:"];
          Print[N[RelativeBoxWidths[{{left}, {right}}], workingPrecision]];
        ];
        Return[{mean, estimatederror}]
      ];

      midSign = signAt[mid];

      If[midSign == 0,
        mean = {mid};
        estimatederror = {0};
        If[verbose,
          Print["Successful."];
          Print["Mean:"];
          Print[N[mean, workingPrecision]];
          Print["Estimated error:"];
          Print[N[estimatederror, workingPrecision]];
        ];
        Return[{mean, estimatederror}]
      ];

      If[EdgeCrossingQ[leftSign, midSign],
        right = mid;
        rightSign = midSign,
        left = mid;
        leftSign = midSign
      ];

      relErrors = RelativeBoxWidths[{{left}, {right}}];
      If[AllTrue[relErrors, # < reltol &],
        mean = {(left + right)/2};
        estimatederror = {Abs[right - left]/2};
        If[verbose,
          Print["Successful."];
          Print["Mean:"];
          Print[N[mean, workingPrecision]];
          Print["Estimated error:"];
          Print[N[estimatederror, workingPrecision]];
        ];
        Return[{mean, estimatederror}]
      ];

      If[iter == maxiter,
        Print["Warning: Reached maximum number of iterations."]
      ],
      {iter, maxiter}
    ];

    mean = {(left + right)/2};
    estimatederror = {Abs[right - left]/2};
    If[verbose,
      Print["Successful."];
      Print["Mean:"];
      Print[N[mean, workingPrecision]];
      Print["Estimated error:"];
      Print[N[estimatederror, workingPrecision]];
    ];
    Return[{mean, estimatederror}]
  ];

  (* For two or more tuned couplings we iteratively approximate each observable's
     zero set on a grid, intersect those approximations, and shrink the corridor to
     the resulting box until the target relative width is reached. *)
  computation := (
    bound = N[hyperCubeBoundary /. params, workingPrecision + 10];
    errors = Catch[
      Do[
        spacing = SpacingEqualNumberPointsInAllDirections[bound, targetGridSizePerStep];
        gridData = GridPoints[bound, targetGridSizePerStep];
        signGrid = GridSearch[gridData, flow, observableTemplate, timeVar];

        If[AnyTrue[Transpose[signGrid], SameStrictSignQ],
          Throw[error[NoZeroCrossing]]
        ];

        zeroCrossings = FindZeroCrossings[signGrid, gridData];

        If[AnyTrue[zeroCrossings, # === {} &],
          Throw[error[NoZeroCrossing]]
        ];

        samplePoints = SampledPoints[#, n] & /@ zeroCrossings;
        oldbound = bound;
        bound = FindEdgesOfIntersection[
          bound,
          targetGridSizePerStep,
          spacing,
          samplePoints,
          epsilon,
          sharpgridpower
        ];

        If[bound[[1, 1]] === Infinity,
          Throw[error[NoIntersection]]
        ];

        relErrors = RelativeBoxWidths[bound];

        If[oldbound === bound,
          Throw[error[LowWorkPrec]]
        ];

        If[AllTrue[relErrors, # < reltol &],
          Break[]
        ];

        If[iter === maxiter,
          Print["Warning: Reached maximum number of iterations."]
        ],
        {iter, maxiter}
      ]
    ];
  );

  If[
    verbose && TrueQ[$Notebooks],
    Monitor[
      computation,
      Row[{
        "iteration: ",
        Dynamic[iter],
        " -- relative errors: ",
        Dynamic[N[relErrors]]
      }]
    ],
    computation
  ];

  Which[
    errors === error[NoZeroCrossing],
    "Error: No zero-crossing detected for at least one observable",

    errors === error[NoIntersection],
    "Error: The zero-crossing point clouds do not intersect",

    errors === error[LowWorkPrec],
    Print["Warning: Noise reached the hypercube edge size. Consider increasing WorkingPrecision."];
    mean = Mean[bound];
    estimatederror = Abs[bound[[2]] - mean];
    If[verbose,
      Print["Relative errors:"];
      Print[N[relErrors, workingPrecision]];
    ];
    {mean, estimatederror},

    True,
    mean = Mean[bound];
    estimatederror = Abs[bound[[2]] - mean];
    If[verbose,
      Print["Successful."];
      Print["Mean:"];
      Print[N[mean, workingPrecision]];
      Print["Estimated error:"];
      Print[N[estimatederror, workingPrecision]];
    ];
    {mean, estimatederror}
  ]
];

End[];

EndPackage[];
