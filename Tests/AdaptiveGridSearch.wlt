Needs["FRGTuning`"];

VerificationTest[
  Head[AdaptiveGridSearch],
  Symbol,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-exported"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {m'[t] == 0},
        {},
        {},
        {m},
        {{-1}, {1}},
        Automatic
      ][],
      "timerange",
      AdaptiveGridSearch::timerange
    ],
    AdaptiveGridSearch::timerange
  ],
  "timerange",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-validates-time-range"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {m'[t] == 0},
        {},
        {},
        {m},
        {-1, 1},
        {0, 1}
      ][],
      "shape",
      AdaptiveGridSearch::shape
    ],
    AdaptiveGridSearch::shape
  ],
  "shape",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-validates-corridor-shape"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {
          m'[t] == 0,
          x'[t] == 0
        },
        {},
        {},
        {m, x},
        {{-1}, {1}},
        {0, 1}
      ][],
      "tuned",
      AdaptiveGridSearch::tuned
    ],
    AdaptiveGridSearch::tuned
  ],
  "tuned",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-validates-tuned-dimension"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {m'[t] == 0},
        {},
        {},
        {m},
        {{-1}, {1}},
        {0, 1},
        "TuningObservables" -> {m[t], n[t]}
      ][],
      "observables",
      AdaptiveGridSearch::observables
    ],
    AdaptiveGridSearch::observables
  ],
  "observables",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-validates-observables-dimension"
]

VerificationTest[
  AdaptiveGridSearch[
    {m'[t] == 0},
    {},
    {},
    {m},
    {{1}, {2}},
    {0, 1},
    "TargetGridSizePerStep" -> 9
  ][],
  "Error: No zero-crossing detected for at least one observable",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-no-zero-crossing"
]

VerificationTest[
  AdaptiveGridSearch[
    {
      u'[t] == 0,
      v'[t] == 0
    },
    {},
    {},
    {u, v},
    {{-1, -1}, {1, 1}},
    {0, 1},
    "TuningObservables" -> {
      u[t]^2 + v[t]^2 - 1/10,
      (u[t] - 4/5)^2 + v[t]^2 - 1/10
    },
    "TargetGridSizePerStep" -> 81,
    "TubeEpsilon" -> 2/5,
    "SamplesPerStep" -> 40,
    "SharpGridPower" -> 1,
    WorkingPrecision -> 30
  ][],
  "Error: The zero-crossing point clouds do not intersect",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-no-intersection"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {m'[t] == 0.1},
        {},
        {},
        {m},
        {{0.0}, {1.0}},
        {0, 1},
        "TuningObservables" -> {m[t] - 1/2},
        "TargetGridSizePerStep" -> 2,
        WorkingPrecision -> 40
      ][];
      "no-warning",
      "warning",
      AdaptiveGridSearch::lowprec
    ],
    AdaptiveGridSearch::lowprec
  ],
  "warning",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-warns-on-low-precision-floats"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {m'[t] == 1/10},
        {},
        {},
        {m},
        {{0}, {1}},
        {0, 1},
        "TuningObservables" -> {m[t] - 1/2},
        "TargetGridSizePerStep" -> 2,
        WorkingPrecision -> 40
      ][];
      "no-warning",
      "warning",
      AdaptiveGridSearch::lowprec
    ],
    AdaptiveGridSearch::lowprec
  ],
  "no-warning",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-does-not-warn-on-exact-input"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - 1/2},
      "TargetGridSizePerStep" -> 2,
      "RelativeTolerance" -> 0.1,
      WorkingPrecision -> 30
    ][];
    {
      Length[result] == 2,
      Abs[result[[1, 1]] - 1/2] < 10^-12,
      0 <= result[[2, 1]] <= 1/2
    }
  ],
  {True, True, True},
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-1d-brackets-endpoints"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - 1/3},
      "TargetGridSizePerStep" -> 9,
      "MultipleZeroCrossingsCheck1D" -> True,
      "RelativeTolerance" -> 0.2,
      WorkingPrecision -> 30
    ][];
    {
      Abs[result[[1, 1]] - 1/3] < 0.05,
      0 < result[[2, 1]] < 0.1
    }
  ],
  {True, True},
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-1d-multiple-crossing-check-single-root"
]

VerificationTest[
  Module[{result},
    result = ToString @ AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{-1}, {1}},
      {0, 1},
      "TuningObservables" -> {(m[t] - 1/2) (m[t] + 1/2)},
      "TargetGridSizePerStep" -> 9,
      "MultipleZeroCrossingsCheck1D" -> True,
      WorkingPrecision -> 30
    ][];
    StringContainsQ[result, "Detected 2 candidate zero crossings"] &&
    StringContainsQ[result, "Candidate brackets:"] &&
    StringContainsQ[result, "Choose one bracket as the new tuning corridor and rerun with that."]
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-1d-multiple-crossing-check-reports-brackets"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - 1/3},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      "SamplesPerStep" -> 20,
      WorkingPrecision -> 30
    ][];
    {
      Abs[result[[1, 1]] - 1/3] < 0.05,
      0 < result[[2, 1]] < 0.1
    }
  ],
  {True, True},
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-converges-on-offset-zero-crossing"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - 1/3},
      "TargetGridSizePerStep" -> 2,
      "RelativeTolerance" -> 0.02,
      WorkingPrecision -> 40
    ][];
    {
      Abs[result[[1, 1]] - 1/3] < 0.01,
      0 < result[[2, 1]] < 0.01
    }
  ],
  {True, True},
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-1d-bisection-refines-bracket"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {
        g'[t] == 0,
        m'[t] == 0
      },
      {g},
      {
        g[0] == a
      },
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - a/2},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      WorkingPrecision -> 30
    ][a -> 3/5];
    Abs[result[[1, 1]] - 0.3] < 0.05
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-applies-parameter-rules"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - 1/3},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      "Verbose" -> True,
      WorkingPrecision -> 30
    ][];
    Abs[result[[1, 1]] - 1/3] < 0.05
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-verbose-mode"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 0},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 1},
      "TuningObservables" -> {m[t] - 1/3},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      "NDSolveOptions" -> {AccuracyGoal -> 30, PrecisionGoal -> 30},
      WorkingPrecision -> 40
    ][];
    Abs[result[[1, 1]] - 1/3] < 0.05
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-passes-ndsolve-options"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {m'[t] == 1},
      {},
      {},
      {m},
      {{0}, {1}},
      {0, 2},
      "TuningObservables" -> {m[t] - 7/10},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      "NDSolveAdditions" -> {
        WhenEvent[t >= 1/2, "StopIntegration"]
      },
      WorkingPrecision -> 30
    ][];
    Abs[result[[1, 1]] - 1/5] < 0.06
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-passes-ndsolve-additions"
]

VerificationTest[
  AdaptiveGridSearch[
    {
      u'[s] == 0,
      v'[s] == 0
    },
    {},
    {},
    {u, v},
    {{-1, -1}, {1, 1}},
    {0, 1},
    "TimeVariable" -> s,
    "TuningObservables" -> {u[s] - 1/3, v[s] - 2/3},
    "TargetGridSizePerStep" -> 9,
    "RelativeTolerance" -> 0.2,
    WorkingPrecision -> 30
  ] // Head,
  AdaptiveGridSearch,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-custom-time-variable-construction"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {
        u'[s] == 0,
        v'[s] == 0
      },
      {},
      {},
      {u, v},
      {{0, 0}, {1, 1}},
      {0, 1},
      "TimeVariable" -> s,
      "TuningObservables" -> {u[s] - 1/3, v[s] - 2/3},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      WorkingPrecision -> 30
    ][];
    Norm[result[[1]] - {1/3, 2/3}] < 0.08
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-custom-time-variable-execution"
]

VerificationTest[
  Module[{result},
    result = AdaptiveGridSearch[
      {
        u'[t] == 0,
        v'[t] == 0
      },
      {},
      {},
      {u, v},
      {{0, 0}, {1, 1}},
      {0, 1},
      "TargetGridSizePerStep" -> 9,
      "RelativeTolerance" -> 0.2,
      WorkingPrecision -> 30
    ][];
    Norm[result[[1]] - {0, 0}] < 0.08
  ],
  True,
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-default-observables-are-tuned-couplings"
]

VerificationTest[
  Quiet[
    Check[
      AdaptiveGridSearch[
        {u'[t] == 0},
        {},
        {},
        {u},
        {{0}, {1}},
        {0, 1},
        "VerbosePlot" -> True
      ][],
      "plot2d",
      AdaptiveGridSearch::plot2d
    ],
    AdaptiveGridSearch::plot2d
  ],
  "plot2d",
  SameTest -> SameQ,
  TestID -> "AdaptiveGridSearch-verboseplot-message-outside-2d"
]
