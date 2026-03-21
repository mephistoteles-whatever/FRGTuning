Needs["FRGTuning`"];

VerificationTest[
  FRGTuning`Private`NormalizeParameterRules[<|"a" -> 1, "b" -> 2|>],
  {"a" -> 1, "b" -> 2},
  SameTest -> SameQ,
  TestID -> "NormalizeParameterRules-association"
]

VerificationTest[
  FRGTuning`Private`NormalizeParameterRules[{a -> 1, b -> 2}],
  {a -> 1, b -> 2},
  SameTest -> SameQ,
  TestID -> "NormalizeParameterRules-list"
]

VerificationTest[
  FRGTuning`Private`NormalizeParameterRules[a -> 1],
  {a -> 1},
  SameTest -> SameQ,
  TestID -> "NormalizeParameterRules-rule"
]

VerificationTest[
  FRGTuning`Private`NormalizeParameterRules[a :> 1],
  {a :> 1},
  SameTest -> SameQ,
  TestID -> "NormalizeParameterRules-rule-delayed"
]

VerificationTest[
  FRGTuning`Private`NormalizeParameterRules[None],
  {},
  SameTest -> SameQ,
  TestID -> "NormalizeParameterRules-none"
]

VerificationTest[
  FRGTuning`Private`NormalizeParameterRules[Automatic],
  {},
  SameTest -> SameQ,
  TestID -> "NormalizeParameterRules-automatic"
]

VerificationTest[
  FRGTuning`Private`NormalizeCoupling[m, t],
  m[t],
  SameTest -> SameQ,
  TestID -> "NormalizeCoupling-symbol"
]

VerificationTest[
  FRGTuning`Private`NormalizeCoupling[g[4], t],
  g[4][t],
  SameTest -> SameQ,
  TestID -> "NormalizeCoupling-indexed-symbol"
]

VerificationTest[
  FRGTuning`Private`NormalizeCoupling[m[t], t],
  m[t],
  SameTest -> SameQ,
  TestID -> "NormalizeCoupling-already-time-dependent"
]

VerificationTest[
  FRGTuning`Private`NormalizeCoupling[g[4][t], t],
  g[4][t],
  SameTest -> SameQ,
  TestID -> "NormalizeCoupling-indexed-already-time-dependent"
]

VerificationTest[
  FRGTuning`Private`NormalizeCouplingList[{g, m[2], x[t]}, t],
  {g[t], m[2][t], x[t]},
  SameTest -> SameQ,
  TestID -> "NormalizeCouplingList-mixed-input"
]

VerificationTest[
  FRGTuning`Private`SpacingEqualNumberPointsInAllDirections[{{0, 0}, {2, 4}}, 16],
  {2/3, 4/3},
  SameTest -> SameQ,
  TestID -> "SpacingEqualNumberPointsInAllDirections-2d"
]

VerificationTest[
  FRGTuning`Private`GridAxes[{{-1}, {1}}, 5],
  {{-1, -1/2, 0, 1/2, 1}},
  SameTest -> SameQ,
  TestID -> "GridAxes-1d"
]

VerificationTest[
  FRGTuning`Private`GridAxes[{{-1, -2}, {1, 2}}, 9],
  {{-1, 0, 1}, {-2, 0, 2}},
  SameTest -> SameQ,
  TestID -> "GridAxes-2d"
]

VerificationTest[
  FRGTuning`Private`BuildIndexTuples[{2, 3}],
  {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {2, 2}, {2, 3}},
  SameTest -> SameQ,
  TestID -> "BuildIndexTuples-2x3"
]

VerificationTest[
  FRGTuning`Private`GridPoints[{{-1, -1}, {1, 1}}, 4],
  <|
    "Axes" -> {{-1, 1}, {-1, 1}},
    "AxisLengths" -> {2, 2},
    "IndexTuples" -> {{1, 1}, {1, 2}, {2, 1}, {2, 2}},
    "Points" -> {{-1, -1}, {-1, 1}, {1, -1}, {1, 1}}
  |>,
  SameTest -> SameQ,
  TestID -> "GridPoints-2d-corners"
]

VerificationTest[
  Module[{traj},
    traj = NDSolve[
      {x'[t] == 1, x[0] == 0},
      {x[t]},
      {t, 0, 2}
    ][[1]];
    FRGTuning`Private`finalTime[traj]
  ],
  2.,
  SameTest -> (Abs[#1 - #2] < 10^-8 &),
  TestID -> "finalTime-extracts-endpoint"
]

VerificationTest[
  FRGTuning`Private`RelativeBoxWidths[{{1, -2}, {3, 2}}],
  {1, 2},
  SameTest -> SameQ,
  TestID -> "RelativeBoxWidths-general"
]

VerificationTest[
  FRGTuning`Private`RelativeBoxWidths[{{0}, {0}}],
  {0},
  SameTest -> SameQ,
  TestID -> "RelativeBoxWidths-zero-zero"
]

VerificationTest[
  FRGTuning`Private`SameStrictSignQ[{1, 2, 3}],
  True,
  SameTest -> SameQ,
  TestID -> "SameStrictSignQ-positive"
]

VerificationTest[
  FRGTuning`Private`SameStrictSignQ[{-1, -2}],
  True,
  SameTest -> SameQ,
  TestID -> "SameStrictSignQ-negative"
]

VerificationTest[
  FRGTuning`Private`SameStrictSignQ[{-1, 0, 1}],
  False,
  SameTest -> SameQ,
  TestID -> "SameStrictSignQ-mixed"
]

VerificationTest[
  FRGTuning`Private`EdgeCrossingQ[-1, 1],
  True,
  SameTest -> SameQ,
  TestID -> "EdgeCrossingQ-sign-flip"
]

VerificationTest[
  FRGTuning`Private`EdgeCrossingQ[0, 1],
  True,
  SameTest -> SameQ,
  TestID -> "EdgeCrossingQ-zero"
]

VerificationTest[
  FRGTuning`Private`EdgeCrossingQ[1, 1],
  False,
  SameTest -> SameQ,
  TestID -> "EdgeCrossingQ-no-crossing"
]

VerificationTest[
  Module[{gridData, flow},
    gridData = <|"Points" -> {{-1}, {1}}|>;
    flow = Function[{pt},
      NDSolve[
        {
          x'[t] == 0,
          y'[t] == 0,
          x[0] == pt[[1]],
          y[0] == -pt[[1]]
        },
        {x[t], y[t]},
        {t, 0, 1}
      ][[1]]
    ];
    FRGTuning`Private`GridSearch[gridData, flow, {x[t], y[t]}, t]
  ],
  {{-1, 1}, {1, -1}},
  SameTest -> SameQ,
  TestID -> "GridSearch-sign-evaluation"
]

VerificationTest[
  Module[{gridData, flow},
    gridData = <|"Points" -> {{0}}|>;
    flow = Function[{pt},
      NDSolve[
        {
          x'[t] == 0,
          x[0] == pt[[1]]
        },
        {x[t]},
        {t, 0, 1}
      ][[1]]
    ];
    FRGTuning`Private`GridSearch[gridData, flow, {x[t]}, t]
  ],
  {{0}},
  SameTest -> SameQ,
  TestID -> "GridSearch-zero-sign"
]

VerificationTest[
  Module[{gridData, signGrid},
    gridData = FRGTuning`Private`GridPoints[{{-1, -1}, {1, 1}}, 4];
    signGrid = {
      {-1, -1},
      {-1, 1},
      {1, -1},
      {1, 1}
    };
    Sort /@ FRGTuning`Private`FindZeroCrossings[signGrid, gridData]
  ],
  {
    Sort[{{0, -1}, {0, 1}}],
    Sort[{{-1, 0}, {1, 0}}]
  },
  SameTest -> SameQ,
  TestID -> "FindZeroCrossings-2d"
]

VerificationTest[
  Module[{gridData, signGrid},
    gridData = FRGTuning`Private`GridPoints[{{-1}, {1}}, 5];
    signGrid = {{-1}, {-1}, {0}, {1}, {1}};
    FRGTuning`Private`FindZeroCrossings[signGrid, gridData]
  ],
  {{{-1/4}, {1/4}}},
  SameTest -> SameQ,
  TestID -> "FindZeroCrossings-1d-with-zero"
]

VerificationTest[
  Module[{gridData, signGrid},
    gridData = FRGTuning`Private`GridPoints[{{-1}, {1}}, 5];
    signGrid = {{-1}, {-1}, {1}, {1}, {1}};
    FRGTuning`Private`FindZeroBracket1D[signGrid, gridData]
  ],
  {-1/2, 0},
  SameTest -> SameQ,
  TestID -> "FindZeroBracket1D-sign-change"
]

VerificationTest[
  Module[{gridData, signGrid},
    gridData = FRGTuning`Private`GridPoints[{{-1}, {1}}, 5];
    signGrid = {{-1}, {-1}, {0}, {1}, {1}};
    FRGTuning`Private`FindZeroBracket1D[signGrid, gridData]
  ],
  {0, 0},
  SameTest -> SameQ,
  TestID -> "FindZeroBracket1D-zero-grid-point"
]

VerificationTest[
  Module[{gridData, signGrid},
    gridData = FRGTuning`Private`GridPoints[{{-1}, {1}}, 5];
    signGrid = {{-1}, {-1}, {-1}, {1}, {1}};
    FRGTuning`Private`FindZeroBracket1D[signGrid, gridData]
  ],
  {0, 1/2},
  SameTest -> SameQ,
  TestID -> "FindZeroBracket1D-later-sign-change"
]

VerificationTest[
  Module[{gridData, signGrid},
    gridData = FRGTuning`Private`GridPoints[{{-1}, {1}}, 5];
    signGrid = {{-1}, {-1}, {-1}, {-1}, {-1}};
    FRGTuning`Private`FindZeroBracket1D[signGrid, gridData]
  ],
  Missing["NotFound"],
  SameTest -> SameQ,
  TestID -> "FindZeroBracket1D-no-crossing"
]

VerificationTest[
  FRGTuning`Private`SampledPoints[{{0}, {1}, {2}, {3}, {4}}, 2],
  {{0}, {2}, {4}},
  SameTest -> SameQ,
  TestID -> "SampledPoints-downsamples"
]

VerificationTest[
  FRGTuning`Private`SampledPoints[{{0}, {1}}, 10],
  {{0}, {1}},
  SameTest -> SameQ,
  TestID -> "SampledPoints-keeps-short-cloud"
]

VerificationTest[
  FRGTuning`Private`InPointCloudQ[{{0, 0}, {1, 1}}, {1, 1}, 0.2][{0.1, 0.1}],
  True,
  SameTest -> SameQ,
  TestID -> "InPointCloudQ-inside"
]

VerificationTest[
  FRGTuning`Private`InPointCloudQ[{{0, 0}}, {1, 1}, 0.2][{0.5, 0.5}],
  False,
  SameTest -> SameQ,
  TestID -> "InPointCloudQ-outside"
]

VerificationTest[
  FRGTuning`Private`InIntersectionQ[
    {
      {{0, 0}, {0.1, 0.1}},
      {{0.05, 0.05}}
    },
    {1, 1},
    0.2
  ][{0.05, 0.05}],
  True,
  SameTest -> SameQ,
  TestID -> "InIntersectionQ-true"
]

VerificationTest[
  FRGTuning`Private`InIntersectionQ[
    {
      {{0, 0}},
      {{2, 2}}
    },
    {1, 1},
    0.2
  ][{0, 0}],
  False,
  SameTest -> SameQ,
  TestID -> "InIntersectionQ-false"
]

VerificationTest[
  FRGTuning`Private`FindEdgesOfIntersection[
    {{-1}, {1}},
    9,
    {0.25},
    {
      {{0.0}},
      {{0.25}}
    },
    1,
    1
  ],
  {{0.}, {0.25}},
  SameTest -> (Max[Abs[Flatten[N[#1] - N[#2]]]] < 10^-8 &),
  TestID -> "FindEdgesOfIntersection-1d"
]

VerificationTest[
  FRGTuning`Private`FindEdgesOfIntersection[
    {{-1}, {1}},
    9,
    {0.1},
    {
      {{-0.8}},
      {{0.8}}
    },
    0.1,
    1
  ],
  {{Infinity}, {Infinity}},
  SameTest -> SameQ,
  TestID -> "FindEdgesOfIntersection-empty"
]
