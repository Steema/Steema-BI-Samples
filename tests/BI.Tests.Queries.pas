{*********************************************}
{  TeeBI Software Library                     }
{  QUERY and SUMMMARY TESTS                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Queries;

interface

uses
  BI.Arrays, BI.Data, DUnitX.TestFramework,
  BI.Summary, BI.DataSource, BI.Persist;

type
  [TestFixture]
  TQuery_Test=class(TObject)
  strict private
    Data : TDataItem;
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
  BI.Tests.SummarySamples, BI.Tests.SelectSamples;

{ TQuery_Test }

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
    tmp:=TDataItem.Create(TSelectSamples.Select(t));
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
    tmp:=TDataItem.Create(Samples.CreateSummary(t));
    try
      tmp.Load(tmp.AsTable);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TQuery_Test.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TQuery_Test);
end.
