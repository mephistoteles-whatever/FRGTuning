report = TestReport[
  FileNames["*.wlt", DirectoryName[$InputFileName]]
];

passed = Length[report["TestsSucceededKeys"]];
failedWrong = report["TestsFailedWrongResultsKeys"];
failedMessages = report["TestsFailedWithMessagesKeys"];
failedErrors = report["TestsFailedWithErrorsKeys"];
failed = Join[failedWrong, failedMessages, failedErrors];

Print["Passed: ", passed];
Print["Failed: ", Length[failed]];

If[failed =!= {},
  Print["Failing tests:"];
  Do[
    test = report["TestResults"][id];
    Print["- ", test["TestID"], " (", test["Outcome"], ")"];
    If[test["Outcome"] === "Failure",
      Print["  expected: ", test["ExpectedOutput"]];
      Print["  actual:   ", test["ActualOutput"]];
    ];
    If[test["ActualMessages"] =!= {},
      Print["  messages: ", test["ActualMessages"]];
    ];
    ,
    {id, failed}
  ];
];
