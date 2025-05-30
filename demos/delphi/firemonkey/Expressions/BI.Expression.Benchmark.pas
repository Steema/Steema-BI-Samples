{*********************************************}
{  TeeBI Software Library                     }
{  TExpression Benchmark methods              }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression.Benchmark;

interface

{$IFNDEF FPC}
{$IF CompilerVersion>27}
{$DEFINE THREADING}   // RAD XE7 and up
{$ENDIF}
{$ENDIF}

uses
  System.Classes, BI.Expression;

// TExpression benchmark speed.
// Parse and evaluate all test expressions multiple times.

type
  TBenchMode=(Evaluate,Parse,Both);

  TBenchmark=record
  private

    function Parse(const S:String):TExpression;

    {$IFDEF THREADING}
    procedure BothMulti;
    procedure EvaluateMulti;
    procedure ParseMulti;
    procedure RunMultiCPU;
    {$ENDIF}
    procedure RunSingleCPU;
  public
   const
      BenchmarkCount=10000;

   var
    Expressions : TExpressions;

    UseThreads : Boolean;

    Items : TStrings;

    Total,
    Count :Integer;

    Mode : TBenchMode;

    Creating,
    Testing : Int64;

    procedure DestroyAll;
    procedure Initialize(const AItems:TStrings; const MultiCPU:Boolean; const AMode:TBenchMode);
    procedure ParseAll;
    procedure ShowResults(const ALines:TStrings);
    procedure TestAll;
  end;

// Returns the S sub-strings at left and right of "->" symbol
function SplitTest(const S:String; out Left,Right:String):Boolean;

implementation

uses
  {$IFDEF THREADING}
  System.Threading,
  {$ENDIF}
  System.SysUtils, System.Diagnostics;

function TBenchmark.Parse(const S:String):TExpression;
var Left,
    Right : String;
begin
  if (S='') or (not SplitTest(S,Left,Right)) then
     result:=nil
  else
     result:=TExpression.FromString(Left);
end;

procedure TBenchmark.ParseAll;
var t : Integer;
    tmp : TExpression;
begin
  DestroyAll;

  Count:=0;

  for t:=0 to Items.Count-1 do
  begin
    tmp:=Parse(Items[t]);

    if tmp<>nil then
    begin
      Expressions[Count]:=tmp;
      Inc(Count);
    end;
  end;
end;

procedure TBenchmark.Initialize(const AItems:TStrings;
                                const MultiCPU:Boolean;
                                const AMode:TBenchMode);
var t1 : TStopWatch;
begin
  t1:=TStopwatch.StartNew;

  Items:=AItems;

  UseThreads:=MultiCPU;

  SetLength(Expressions,Items.Count);

  Mode:=AMode;

  ParseAll;

  Creating:=t1.ElapsedMilliseconds;

  // Just in case to skip blank lines
  SetLength(Expressions,Count);
end;

procedure TBenchmark.DestroyAll;
var t : Integer;
begin
  for t:=0 to High(Expressions) do
      Expressions[t].Free;
end;

{$IFDEF THREADING}

procedure TBenchmark.ParseMulti;
var tmpBench : TBenchmark;
begin
  tmpBench:=Self;

  TParallel.For(0,High(Expressions),procedure(Index:Integer)
  var tmp : TExpression;
      t : Integer;
      S : String;
  begin
    S:=tmpBench.Items[Index];

    for t:=1 to BenchmarkCount do
    begin
      tmp:=tmpBench.Parse(S);
      tmp.Free;
    end
  end);
end;

procedure TBenchmark.EvaluateMulti;
var tmpExp: TExpressions;
begin
  tmpExp:=Expressions;

  TParallel.For(0,High(Expressions),procedure(Index:Integer)
  var tmp : TExpression;
      tmpResult : TData;
      t : Integer;
  begin
    tmp:=tmpExp[Index];

    for t:=1 to BenchmarkCount do
        tmpResult:=tmp.Value;
  end);
end;

procedure TBenchmark.BothMulti;
var tmpBench : TBenchmark;
begin
  tmpBench:=Self;

  TParallel.For(0,High(Expressions),procedure(Index:Integer)
  var tmp : TExpression;
      tmpResult : TData;
      t : Integer;
      S : String;
  begin
    S:=tmpBench.Items[Index];

    for t:=1 to BenchmarkCount do
    begin
      tmp:=tmpBench.Parse(S);
      tmpResult:=tmp.Value;
      tmp.Free;
    end;
  end);
end;

procedure TBenchmark.RunMultiCPU;
begin
  if Mode=TBenchMode.Parse then
     ParseMulti
  else
  if Mode=TBenchMode.Evaluate then
     EvaluateMulti
  else
     BothMulti;
end;
{$ENDIF}

procedure TBenchmark.RunSingleCPU;

  procedure Test(const AExpression:TExpression);
  var tmp : TData;
      t : Integer;
  begin
    for t:=1 to BenchmarkCount do
        tmp:=AExpression.Value;
  end;

  procedure LoopParseAll;
  var t : Integer;
  begin
    for t:=1 to BenchmarkCount do
        ParseAll;
  end;

  procedure EvaluateAll;
  var t : Integer;
  begin
    for t:=0 to High(Expressions) do
        Test(Expressions[t]);
  end;

begin
  if Mode=TBenchMode.Evaluate then
     EvaluateAll
  else
  if Mode=TBenchMode.Parse then
     LoopParseAll
  else
  begin
    LoopParseAll;
    EvaluateAll;
  end;
end;

procedure TBenchmark.TestAll;
var t1 : TStopWatch;
begin
  t1:=TStopwatch.StartNew;

  {$IFDEF THREADING}
  if UseThreads then
     RunMultiCPU
  else
  {$ENDIF}
     RunSingleCPU;

  Testing:=t1.ElapsedMilliseconds;
end;

procedure TBenchmark.ShowResults(const ALines:TStrings);
var tmp : String;
begin
  ALines.BeginUpdate;
  try
    ALines.Clear;

    ALines.Add('Total creating time: '+Creating.ToString);
    ALines.Add('Total testing time: '+Testing.ToString);

    ALines.Add('Iterations: '+BenchmarkCount.ToString+' expressions: '+Length(Expressions).ToString);

    Total:=BenchmarkCount*Length(Expressions);

    case Mode of
      TBenchMode.Evaluate: tmp:='evaluated';
         TBenchMode.Parse: tmp:='parsed';
          TBenchMode.Both: tmp:='parsed and evaluated';
    end;

    ALines.Add('Total '+tmp+': '+FormatFloat('#,##0',Total));

    ALines.Add('Average per item: '+FormatFloat('0.#######',Testing / Total)+' msec.');
  finally
    ALines.EndUpdate;
  end;
end;

// Returns the S sub-strings at left and right of "->" symbol
function SplitTest(const S:String; out Left,Right:String):Boolean;
var i : Integer;
begin
   i:=Pos('->',S);

  if i>0 then
  begin
    Left:=Copy(S,1,i-1);
    Right:=Trim(Copy(S,i+2,Length(S)));

    result:=True;
  end
  else
    result:=False;
end;

end.
