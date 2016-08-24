program TeeBI_Tests;

{$DEFINE USECONSOLE}

{.$DEFINE TESTINSIGHT}

{$IFNDEF TESTINSIGHT}
{$DEFINE USECONSOLE}
{$ENDIF}

{$IFDEF USECONSOLE}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}
uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  {$IF CompilerVersion>28}
  {$IFDEF USECONSOLE}
  DUnitX.Loggers.Console,
  {$ELSE}
  {$ENDIF }
  {$IF CompilerVersion>30}
  DUnitX.TestRunner, DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit,
  {$ELSE}
  DUnitX.Loggers.DUnitX.TestRunner,
  {$ENDIF }
  {$ELSE}
  DUnitTestRunner,
  GUITestRunner,
  {$ENDIF }

  BI.Tests.Expressions in 'BI.Tests.Expressions.pas',
  BI.Tests.TDataItem in 'BI.Tests.TDataItem.pas',
  BI.Tests.Exporting in 'BI.Tests.Exporting.pas',
  BI.Tests.Importing in 'BI.Tests.Importing.pas',
  BI.Tests.WebServer in 'BI.Tests.WebServer.pas',
  BI.Tests.Queries in 'BI.Tests.Queries.pas',
  BI.Tests.SQLParser in 'BI.Tests.SQLParser.pas',
  BI.Expressions.Samples in 'BI.Expressions.Samples.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  ReportMemoryLeaksOnShutdown:=True;
  //NeverSleepOnMMThreadContention:=True;

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;

    //Create the test runner
    runner := TDUnitX.CreateRunner;

    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;

    //tell the runner how we will log things

    //Log to the console window

    {$IFDEF USECONSOLE}
    logger := TDUnitXConsoleLogger.Create(true);
    {$ELSE}
    logger := TDUnitXGuiTestRunner.Create(nil);
    {$ENDIF}

    runner.AddLogger(logger);

    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.


