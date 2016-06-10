{*********************************************}
{  TeeBI Software Library                     }
{  TESTS SQL parser                           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.SQLParser;

interface

uses
  BI.Arrays, BI.Data, DUnitX.TestFramework,
  BI.Data.SQL, BI.DataSource, BI.Persist, BI.Summary;

type
  [TestFixture]
  TSQLParser_Test=class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure SimpleSelect;
    [Test]
    procedure SimpleSummary;
    [Test]
    procedure SimpleWhere;

    [Test]
    procedure ParseAll;
  end;

implementation

uses
  BI.Tests.SummarySamples, BI.Tests.SelectSamples;

{ TSQLParser_Test }

procedure TSQLParser_Test.Setup;
begin
end;

procedure TSQLParser_Test.SimpleSelect;
var Data : TDataItem;
begin
  Data:=TBISQL.From(Samples.Demo,'select * from Customers');
  try
    Assert.IsNotNull(Data);

    Data.Load(Data.AsTable);

    Assert.IsTrue(Data.Count=91);
  finally
    Data.Free;
  end;
end;

procedure TSQLParser_Test.SimpleSummary;
const
  NinetyOne:Int64=91;

var Data : TDataItem;
begin
  Data:=TBISQL.From(Samples.Demo,'select count(*) from Customers');
  try
    Assert.IsNotNull(Data);

    Data.Load(Data.AsTable);

    Assert.AreEqual<Int64>(Data.Count,1);
    Assert.AreEqual(Data[0].Name,'Count of Customers');
    Assert.AreEqual<TDataKind>(Data[0].Kind,TDataKind.dkInt64);
    Assert.AreEqual<Int64>(Data[0].Int64Data[0],NinetyOne);
  finally
    Data.Free;
  end;
end;

procedure TSQLParser_Test.SimpleWhere;
var Data : TDataItem;
begin
  Data:=TBISQL.From(Samples.Demo,'select ProductName from Products where UnitPrice>100');
  try
    Assert.IsNotNull(Data);

    Data.Load(Data.AsTable);

    Assert.AreEqual<Int64>(Data.Count,2);
    Assert.AreEqual(Data[0].Name,'ProductName');
    Assert.AreEqual<TDataKind>(Data[0].Kind,TDataKind.dkText);

    Assert.AreEqual(Data[0].TextData[0],'Thuringer Rostbratwurst');
    Assert.AreEqual(Data[0].TextData[1],'Cote de Blaye');
  finally
    Data.Free;
  end;
end;

procedure TSQLParser_Test.TearDown;
begin
end;

procedure TSQLParser_Test.ParseAll;

  procedure TryExecuteSQL(const AData:TDataItem; const SQL:String);
  var tmp : TDataItem;
  begin
    {$IFNDEF FIX}
    tmp:=TBISQL.From(AData,SQL);
    try
    finally
      tmp.Free;
    end;
    {$ENDIF}
  end;

  // Convert all Select samples to SQL
  procedure ParseAllSelect;
  var t : Integer;
      tmp : TDataSelect;
      SQL : String;
  begin
    for t:=0 to TSelectSamples.Count-1 do
    begin
      tmp:=TSelectSamples.CreateSelect(nil,t);
      try
        SQL:=TBISQL.From(tmp);
        Assert.IsNotEmpty(SQL);

        if t<>21 then // <- fix todo: parse nested: "from (select...)"
        begin
          // Use different source data depending on the select example
          if t=22 then
             TryExecuteSQL(Samples.Movies,SQL)
          else
          if t=24 then
             TryExecuteSQL(Samples.SumData1['Sum of Quantity'],SQL)
          else
          if t=27 then
             TryExecuteSQL(Samples.SumData3['Count of Quantity'],SQL)
          else
          if t=28 then
             TryExecuteSQL(Samples.movies['movies'],SQL)
          else
             TryExecuteSQL(Samples.Demo,SQL);
        end;
      finally
        tmp.Free;
      end;
    end;
  end;

  // Convert all Summary samples to SQL
  procedure ParseAllSummary;
  var t : Integer;
      tmp : TSummary;
      SQL : String;
  begin
    for t:=0 to Samples.Count-1 do
    begin
      tmp:=Samples.CreateSummary(nil,t);
      try
        SQL:=TBISQL.From(tmp);
        Assert.IsNotEmpty(SQL);

        if not (t in [19]) then // <-- fix todo: "Order Details" not in "from" clause
           TryExecuteSQL(Samples.Demo,SQL);
      finally
        tmp.Free;
      end;
    end;
  end;

begin
  ParseAllSelect;
  ParseAllSummary;
end;

initialization
  TDUnitX.RegisterTestFixture(TSQLParser_Test);
end.
