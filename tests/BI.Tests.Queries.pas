{*********************************************}
{  TeeBI Software Library                     }
{  QUERY and SUMMMARY TESTS                   }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Queries;

interface

uses
  BI.Arrays, BI.DataItem, DUnitX.TestFramework,
  BI.Summary, BI.DataSource, BI.Persist, BI.SQL; // Added BI.SQL

type
  [TestFixture]
  TQuery_Test=class(TObject)
  strict private
    Data : TDataItem;
    function GetSafeCount(AData: TDataItem): Integer; // Helper function
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CountAll;
    [Test]
    procedure SumAll;

    [Test]
    procedure SelectSamples;
    [Test]
    procedure SummarySamples;
  end;

implementation

uses
  BI.Tests.SummarySamples, BI.Tests.SelectSamples, System.SysUtils; // Added System.SysUtils

{ TQuery_Test }

function TQuery_Test.GetSafeCount(AData: TDataItem): Integer;
begin
  if AData = nil then Result := 0
  else Result := AData.Count;
end;

procedure TQuery_Test.CountAll;
var S : TSummary;
    tmp : TDataItem;
begin
  S:=TSummary.Create(nil);
  try
    S.AddMeasure(Data['Quantity'],TAggregate.Count);

    tmp:=S.Calculate;
    try
      Assert.IsNotNull(tmp);

      Assert.AreEqual<Int64>(tmp.Count,1);
      Assert.AreEqual(tmp.Items.Count,1);
      Assert.AreEqual(tmp[0].Name,'Count of Quantity');
      Assert.AreEqual<TDataKind>(tmp[0].Kind,TDataKind.dkInt64);
      Assert.AreEqual<Int64>(tmp[0].Int64Data[0],2155);
    finally
      tmp.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TQuery_Test.SelectSamples;
var tmp : TDataItem;
    t : Integer;
begin
  for t:=0 to TSelectSamples.Count-1 do
  begin
    tmp:=TDataItem.Create(TSelectSamples.CreateSelect(nil,t));
    try
      tmp.Load(tmp.AsTable);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TQuery_Test.Setup;
begin
  Data:=TStore.Load('BISamples','SQLite_Demo')['"Order Details"'];
end;

procedure TQuery_Test.SumAll;
var S : TSummary;
    tmp : TDataItem;
begin
  S:=TSummary.Create(nil);
  try
    S.AddMeasure(Data['Quantity'],TAggregate.Sum);

    tmp:=S.Calculate;
    try
      Assert.IsNotNull(tmp);

      Assert.AreEqual<Int64>(tmp.Count,1);
      Assert.AreEqual(tmp.Items.Count,1);
      Assert.AreEqual(tmp[0].Name,'Sum of Quantity');
      Assert.AreEqual<TDataKind>(tmp[0].Kind,TDataKind.dkDouble);
      Assert.AreEqual<Double>(tmp[0].DoubleData[0],51317);
    finally
      tmp.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TQuery_Test.SummarySamples;
var tmp : TDataItem;
    t : Integer;
begin
  for t:=0 to Samples.Count-1 do
  begin
    tmp:=TDataItem.Create(Samples.CreateSummary(nil,t));
    try
      tmp.Load(tmp.AsTable);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TQuery_Test.TearDown;
begin
  // Data is freed by the test runner if it's an IInterface, or should be handled if created in Setup
  // In this case, TStore.Load likely returns a new instance that should be managed.
  // For now, keeping TearDown empty as per original structure.
end;

procedure TQuery_Test.TestSelect_TopN_Absolute;
var
  ResultData: TDataItem;
  SQL: String;
  BaseData: TDataItem;
begin
  BaseData := Self.Data.Parent; // "Order Details" is Self.Data, its parent is the root TDataItem from TStore

  SQL := 'SELECT * FROM "Order Details" TOP 10';
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(10, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;

  SQL := 'SELECT * FROM "Order Details" TOP 3000'; // More than available rows (2155)
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(2155, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;

  SQL := 'SELECT * FROM "Order Details" TOP 0';
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(0, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSelect_TopN_Percent;
var
  ResultData: TDataItem;
  ExpectedCount: Integer;
  SQL: String;
  BaseData: TDataItem;
  TotalRows: Integer;
begin
  BaseData := Self.Data.Parent;
  TotalRows := 2155; // Known count of "Order Details"

  SQL := 'SELECT * FROM "Order Details" TOP 10 PERCENT';
  ExpectedCount := (TotalRows * 10) div 100; // 215
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;

  SQL := 'SELECT * FROM "Order Details" TOP 1 PERCENT';
  ExpectedCount := (TotalRows * 1) div 100; // 21
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;

  SQL := 'SELECT * FROM "Order Details" TOP 0 PERCENT';
  ExpectedCount := 0;
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;

  SQL := 'SELECT * FROM "Order Details" TOP 100 PERCENT';
  ExpectedCount := TotalRows;
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSelect_TopN_Percent_Fractional;
var
  ResultData: TDataItem;
  ExpectedCount: Integer;
  SQL: String;
  BaseData: TDataItem;
  TotalRows: Integer;
begin
  BaseData := Self.Data.Parent;
  TotalRows := 2155; // Known count of "Order Details"

  SQL := 'SELECT * FROM "Order Details" TOP 7 PERCENT';
  ExpectedCount := (TotalRows * 7) div 100; // (2155 * 7) div 100 = 15085 div 100 = 150
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSelect_TopN_WithOrderBy;
var
  ResultData, FullSortedData: TDataItem;
  SQL, SQLFull: String;
  BaseData: TDataItem;
  ExpectedOrderID: Integer;
begin
  BaseData := Self.Data.Parent;

  SQL := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" DESC, "Discount" ASC TOP 5';
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(5, GetSafeCount(ResultData), SQL);

    SQLFull := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" DESC, "Discount" ASC';
    FullSortedData := TBISQL.From(BaseData, SQLFull);
    try
      Assert.IsTrue(GetSafeCount(FullSortedData) > 0, 'Full sorted data is empty for OrderBy test');
      ExpectedOrderID := FullSortedData['OrderID'].Int32Data[0];
      Assert.AreEqual(ExpectedOrderID, ResultData['OrderID'].Int32Data[0], 'First OrderID mismatch with TOP and OrderBy');
    finally
      FullSortedData.Free;
    end;
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSelect_TopN_Percent_WithOrderBy;
var
  ResultData, FullSortedData: TDataItem;
  SQL, SQLFull: String;
  BaseData: TDataItem;
  ExpectedCount, ExpectedOrderID: Integer;
  TotalRows: Integer;
begin
  BaseData := Self.Data.Parent;
  TotalRows := 2155;

  SQL := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" ASC, "Discount" DESC TOP 2 PERCENT';
  ExpectedCount := (TotalRows * 2) div 100; // 43
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);

    SQLFull := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" ASC, "Discount" DESC';
    FullSortedData := TBISQL.From(BaseData, SQLFull);
    try
      Assert.IsTrue(GetSafeCount(FullSortedData) > 0, 'Full sorted data is empty for OrderBy Percent test');
      ExpectedOrderID := FullSortedData['OrderID'].Int32Data[0];
      Assert.AreEqual(ExpectedOrderID, ResultData['OrderID'].Int32Data[0], 'First OrderID mismatch with TOP PERCENT and OrderBy');
    finally
      FullSortedData.Free;
    end;
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSelect_TopN_WithOffset;
var
  ResultData, FullSortedData: TDataItem;
  SQL, SQLFull: String;
  BaseData: TDataItem;
  ExpectedOrderID: Integer;
begin
  BaseData := Self.Data.Parent;

  SQL := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" ASC TOP 5 OFFSET 10';
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(5, GetSafeCount(ResultData), SQL);

    SQLFull := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" ASC';
    FullSortedData := TBISQL.From(BaseData, SQLFull);
    try
      Assert.IsTrue(GetSafeCount(FullSortedData) > 10, 'Full sorted data not large enough for offset test');
      ExpectedOrderID := FullSortedData['OrderID'].Int32Data[10]; // 11th row, index 10
      Assert.AreEqual(ExpectedOrderID, ResultData['OrderID'].Int32Data[0], 'First OrderID mismatch with TOP, OrderBy and Offset');
    finally
      FullSortedData.Free;
    end;
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSelect_TopN_Percent_WithOffset;
var
  ResultData, FullSortedData: TDataItem;
  SQL, SQLFull: String;
  BaseData: TDataItem;
  ExpectedCount, ExpectedOrderID: Integer;
  TotalRows: Integer;
begin
  BaseData := Self.Data.Parent;
  TotalRows := 2155;

  SQL := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" ASC TOP 10 PERCENT OFFSET 5';
  ExpectedCount := (TotalRows * 10) div 100; // 215
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);

    SQLFull := 'SELECT "OrderID" FROM "Order Details" ORDER BY "OrderID" ASC';
    FullSortedData := TBISQL.From(BaseData, SQLFull);
    try
      Assert.IsTrue(GetSafeCount(FullSortedData) > 5, 'Full sorted data not large enough for percent offset test');
      ExpectedOrderID := FullSortedData['OrderID'].Int32Data[5]; // 6th row, index 5
      Assert.AreEqual(ExpectedOrderID, ResultData['OrderID'].Int32Data[0], 'First OrderID mismatch with TOP PERCENT, OrderBy and Offset');
    finally
      FullSortedData.Free;
    end;
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSummary_TopN_Absolute;
var
  ResultData: TDataItem;
  SQL: String;
  BaseData: TDataItem;
begin
  BaseData := Self.Data.Parent;

  SQL := 'SELECT "ProductID", COUNT("OrderID") AS NumOrders FROM "Order Details" GROUP BY "ProductID" ORDER BY NumOrders DESC, "ProductID" ASC TOP 5';
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(5, GetSafeCount(ResultData), SQL);
    // Further assertions could check actual ProductIDs if known expected top 5
  finally
    ResultData.Free;
  end;
end;

procedure TQuery_Test.TestSummary_TopN_Percent;
var
  ResultData: TDataItem;
  SQL: String;
  BaseData: TDataItem;
  ExpectedCount: Integer;
  DistinctProdCount: Integer;
begin
  BaseData := Self.Data.Parent;
  DistinctProdCount := 77; // Known distinct ProductIDs in "Order Details"

  SQL := 'SELECT "ProductID", COUNT("OrderID") AS NumOrders FROM "Order Details" GROUP BY "ProductID" ORDER BY NumOrders DESC, "ProductID" ASC TOP 10 PERCENT';
  ExpectedCount := (DistinctProdCount * 10) div 100; // (77 * 10) div 100 = 7
  ResultData := TBISQL.From(BaseData, SQL);
  try
    Assert.AreEqual(ExpectedCount, GetSafeCount(ResultData), SQL);
  finally
    ResultData.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuery_Test);
end.
