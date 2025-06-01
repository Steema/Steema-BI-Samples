{*********************************************}
{  TeeBI Software Library                     }
{  TESTS SQL parser                           }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.SQLParser;

interface

uses
  BI.Arrays, BI.DataItem, DUnitX.TestFramework,
  BI.SQL, BI.DataSource, BI.Persist, BI.Summary, BI.Query, System.SysUtils; // Added BI.Query and System.SysUtils

type
  [TestFixture]
  TSQLParser_Test=class(TObject)
  private
    function GetSelectProviderFromSQL(SQL: String): TDataSelect; // Helper function
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

    [Test]
    procedure TestParse_TopNAbsolute;
    [Test]
    procedure TestParse_TopNPercent;
    [Test]
    procedure TestParse_TopN_WithOffset;
    [Test]
    procedure TestParse_LimitAndOffset_WithoutTop;
    [Test]
    procedure TestParse_TopTakesPrecedenceOverLimit;
    [Test]
    procedure TestParse_InvalidTop_Negative;
    [Test]
    procedure TestParse_InvalidTop_NoValue;
    [Test]
    procedure TestParse_InvalidTop_PercentNoValue;
  end;

implementation

uses
  BI.Tests.SummarySamples, BI.Tests.SelectSamples;

{ TSQLParser_Test }

function TSQLParser_Test.GetSelectProviderFromSQL(SQL: String): TDataSelect;
var
  Provider: TDataProvider;
begin
  Provider := TBISQL.ProviderFrom(Samples.Demo, SQL);
  if Provider = nil then
    Assert.Fail('TBISQL.ProviderFrom returned nil for SQL: ' + SQL);
  if not (Provider is TDataSelect) then
    Assert.Fail('Expected TDataSelect provider for: ' + SQL + ' but got ' + Provider.ClassName);
  Result := Provider as TDataSelect;
  // Caller is responsible for freeing the Provider (which is Result).
end;

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

procedure TSQLParser_Test.TestParse_TopNAbsolute;
var
  Query: TDataSelect;
begin
  Query := GetSelectProviderFromSQL('SELECT TOP 10 Name FROM Customers');
  try
    Assert.AreEqual(10, Query.Max, 'Max value for TOP 10');
    Assert.IsFalse(Query.TopIsPercent, 'TopIsPercent should be false for TOP 10');
  finally
    Query.Free;
  end;
end;

procedure TSQLParser_Test.TestParse_TopNPercent;
var
  Query: TDataSelect;
begin
  Query := GetSelectProviderFromSQL('SELECT TOP 50 PERCENT Name FROM Customers');
  try
    Assert.AreEqual(50, Query.Max, 'Max value for TOP 50 PERCENT');
    Assert.IsTrue(Query.TopIsPercent, 'TopIsPercent should be true for TOP 50 PERCENT');
  finally
    Query.Free;
  end;
end;

procedure TSQLParser_Test.TestParse_TopN_WithOffset;
var
  Query: TDataSelect;
begin
  Query := GetSelectProviderFromSQL('SELECT TOP 5 Name FROM Customers OFFSET 3');
  try
    Assert.AreEqual(5, Query.Max, 'Max value for TOP 5 OFFSET 3');
    Assert.IsFalse(Query.TopIsPercent, 'TopIsPercent should be false for TOP 5 OFFSET 3');
    Assert.AreEqual(3, Query.Start, 'Start value for TOP 5 OFFSET 3');
  finally
    Query.Free;
  end;
end;

procedure TSQLParser_Test.TestParse_LimitAndOffset_WithoutTop;
var
  Query: TDataSelect;
begin
  Query := GetSelectProviderFromSQL('SELECT Name FROM Customers LIMIT 7 OFFSET 2');
  try
    Assert.AreEqual(7, Query.Max, 'Max value for LIMIT 7 OFFSET 2');
    Assert.IsFalse(Query.TopIsPercent, 'TopIsPercent should be false for LIMIT 7 OFFSET 2');
    Assert.AreEqual(2, Query.Start, 'Start value for LIMIT 7 OFFSET 2');
  finally
    Query.Free;
  end;
end;

procedure TSQLParser_Test.TestParse_TopTakesPrecedenceOverLimit;
var
  Query: TDataSelect;
begin
  // TSQLParser.ParseText logic:
  // if Optional('top') then ... TopValue := ...; Limit := 0; // Limit variable in parser is reset
  // else if Optional('limit') then ... Limit := ...
  // Then in TSQLParser.CreateSelect:
  // if TopValue <> -1 then ASelect.Max := TopValue else if Limit > 0 then ASelect.Max := Limit;

  Query := GetSelectProviderFromSQL('SELECT TOP 15 Name FROM Customers LIMIT 7 OFFSET 1');
  try
    Assert.AreEqual(15, Query.Max, 'Max value for TOP 15 with LIMIT 7'); // TopValue should be used
    Assert.IsFalse(Query.TopIsPercent, 'TopIsPercent for TOP 15 with LIMIT 7');
    Assert.AreEqual(1, Query.Start, 'Start value for TOP 15 with LIMIT 7');
  finally
    Query.Free;
  end;

  // If LIMIT appears before TOP in an unusual SQL query:
  Query := GetSelectProviderFromSQL('SELECT Name FROM Customers LIMIT 7 TOP 15 OFFSET 1');
  try
    // The parser processes clauses sequentially. 'LIMIT 7' sets Limit=7. 'TOP 15' then sets TopValue=15 and resets internal Limit to 0.
    // So TopValue=15 takes precedence.
    Assert.AreEqual(15, Query.Max, 'Max value for LIMIT 7 TOP 15');
    Assert.IsFalse(Query.TopIsPercent, 'TopIsPercent for LIMIT 7 TOP 15');
    Assert.AreEqual(1, Query.Start, 'Start value for LIMIT 7 TOP 15');
  finally
    Query.Free;
  end;
end;

procedure TSQLParser_Test.TestParse_InvalidTop_Negative;
var
  Provider: TDataProvider;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      Provider := TBISQL.ProviderFrom(Samples.Demo, 'SELECT TOP -1 Name FROM Customers');
      if Provider <> nil then Provider.Free;
    end,
    'TOP value must be a non-negative integer');
end;

procedure TSQLParser_Test.TestParse_InvalidTop_NoValue;
var
  Provider: TDataProvider;
begin
  // StrToInt('') raises EConvertError. ESQLParser should wrap this.
  Assert.WillRaiseMatching(
    procedure
    begin
      Provider := TBISQL.ProviderFrom(Samples.Demo, 'SELECT TOP Name FROM Customers');
      if Provider <> nil then Provider.Free;
    end, EConvertError); // Or check for ESQLParser if it specifically wraps EConvertError
end;

procedure TSQLParser_Test.TestParse_InvalidTop_PercentNoValue;
var
  Provider: TDataProvider;
begin
  Assert.WillRaiseMatching(
    procedure
    begin
      Provider := TBISQL.ProviderFrom(Samples.Demo, 'SELECT TOP PERCENT Name FROM Customers');
      if Provider <> nil then Provider.Free;
    end, EConvertError);
end;

initialization
  TDUnitX.RegisterTestFixture(TSQLParser_Test);
end.
