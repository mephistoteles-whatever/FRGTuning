Needs["FRGTuning`"];

search = AdaptiveGridSearch[
  {
    g'[t] == -g[t],
    m'[t] == -m[t]
  },
  {g},
  {
    g[0] == 1
  },
  {m},
  {{-2}, {2}},
  "TimeRange" -> {0, 10}
];

result = search[];
Print[result];
