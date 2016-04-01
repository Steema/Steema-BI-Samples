{*********************************************}
{  TeeBI Software Library                     }
{  TExpression Benchmark methods              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression.Benchmark;

interface

uses
  System.Classes, BI.Expression;

// TExpression benchmark speed.
// Parse and evaluate all test expressions multiple times.

type
  TBenchmark=record
  public
   const
      BenchmarkCount=100000;

   var
    Expressions : TArray<TExpression>;

    Total,
    Count :Integer;

    Creating,
    Evaluating : Int64;

    procedure DestroyAll;
    procedure EvaluateAll;
    procedure Initialize(const AItems:TStrings);
    procedure ShowResults(const ALines:TStrings);
  end;

// Returns the S sub-strings at left and right of "->" symbol
function SplitTest(const S:String; out Left,Right:String):Boolean;

implementation

uses
  System.SysUtils, System.Diagnostics;

procedure TBenchmark.Initialize(const AItems:TStrings);
var t1 : TStopWatch;
    t  : Integer;

    Left,
    Right : String;
    S : String;
begin
  t1:=TStopwatch.StartNew;

  SetLength(Expressions,AItems.Count);

  Count:=0;

  for t:=0 to AItems.Count-1 do
  begin
    S:=AItems[t];

    if S<>'' then
       if SplitTest(S,Left,Right) then
       begin
         Expressions[Count]:=TExpression.FromString(Left);
         Inc(Count);
       end;
  end;

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

procedure TBenchmark.EvaluateAll;
var t1 : TStopWatch;
    tmpResult : Variant;
    H,
    t,
    tt : Integer;
begin
  t1:=TStopwatch.StartNew;

  H:=High(Expressions);

  for t:=1 to BenchmarkCount do
      for tt:=0 to H do
          tmpResult:=Expressions[tt].Value;

  Evaluating:=t1.ElapsedMilliseconds;
end;

procedure TBenchmark.ShowResults(const ALines:TStrings);
begin
  ALines.BeginUpdate;
  try
    ALines.Clear;

    ALines.Add('Total creating time: '+Creating.ToString);
    ALines.Add('Total evaluating time: '+Evaluating.ToString);

    ALines.Add('Iterations: '+IntToStr(BenchmarkCount)+' expressions: '+IntToStr(Length(Expressions)));

    Total:=BenchmarkCount*Length(Expressions);

    ALines.Add('Total evaluated: '+FormatFloat('#,##0',Total));

    ALines.Add('Average per eval: '+FormatFloat('0.#######',Evaluating / Total)+' msec.');
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
