{*********************************************}
{  TeeBI Software Library                     }
{  TESTS Expression parser and evaluator      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Expressions;

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, DUnitX.TestFramework,
  BI.Expression;

type
  [TestFixture]
  TExpressions_Test=class(TObject)
  strict private
    List : TStrings;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure All;
    [Test]
    procedure Numeric;
    [Test]
    procedure Float;
    [Test]
    procedure InArray;
  end;

implementation

uses
  System.SysUtils, System.Variants, BI.Expressions.Samples;

{ TExpressions_Test }

procedure TExpressions_Test.All;

  function SplitTest(const S:String; out Left,Right:String):Boolean;
  var i : Integer;
  begin
     i:=Pos('->',S);

    if i>0 then
    begin
      Left:=Trim(Copy(S,1,i-1));
      Right:=Trim(Copy(S,i+2,Length(S)));

      result:=True;
    end
    else
      result:=False;
  end;

  function Test(const AIndex:Integer):String;
  begin
    result:='Expression test number: '+AIndex.ToString;
  end;

var t : Integer;
    Value,
    Left,
    Right : String;
    tmpResult : Variant;
    E : TExpression;
begin
  for t:=0 to List.Count-1 do
  begin
    Assert.IsTrue(SplitTest(List[t],Left,Right),Test(t)+' cannot split test');

    E:=TExpression.FromString(Left);
    try
      Assert.IsNotNull(E,Test(t)+' cannot parse string');

      tmpResult:=E.Value;
      Assert.IsNotNull(tmpResult,Test(t)+' evaluates to null');

      // Special case for Date, converting from Variant is not using the regional setting
      if Left='Date("5/5/2015")' then
         Value:=DateToStr(tmpResult)
      else
         Value:=E.AsString; // <-- already solves the "Split" function issue (returns OLE array)

      Assert.AreEqual(Right,Value,Test(t)+' '+Left+' -> ('+Right+') different value: '+Value);
    finally
      E.Free;
    end;
  end;
end;

procedure TExpressions_Test.Float;
const
  Text='123.456';
var N : TExpression;
begin
  N:=TExpression.FromString(Text);
  try
    Assert.IsTrue(N is TFloatExpression);
    Assert.IsTrue(N.Value=123.456);
    Assert.IsTrue(N.ToString=Text);
  finally
    N.Free;
  end;
end;

procedure TExpressions_Test.InArray;
var E : TExpression;
begin
  E:=TLogicalExpression.Create(
     TTextExpression.Create('X'),
     TLogicalOperand.&In,
     TArrayExpression.Create(['A','C','B','Y','X','Z']));
  try
    Assert.IsTrue(E is TLogicalExpression);
    Assert.IsTrue(E.Value);
    Assert.AreEqual(E.ToString,'"X" in ["A","C","B","Y","X","Z"]');
  finally
    E.Free;
  end;
end;

procedure TExpressions_Test.Numeric;
const
  Text='123';
var N : TExpression;
begin
  N:=TExpression.FromString(Text);
  try
    Assert.IsTrue(N is TIntegerExpression);
    Assert.IsTrue(N.Value=123);
    Assert.IsTrue(N.ToString=Text);
  finally
    N.Free;
  end;
end;

procedure TExpressions_Test.Setup;
begin
  List:=TStringList.Create;
  List.Text:=TSampleExpressions.Text;

  FormatSettings:=TFormatSettings.Invariant;
  FormatSettings.ShortTimeFormat:='hh:mm:ss';
  FormatSettings.ShortDateFormat:='M/d/yyyy';
end;

procedure TExpressions_Test.TearDown;
begin
  List.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TExpressions_Test);
end.
