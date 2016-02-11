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
  BI.Data.SQL, BI.DataSource, BI.Persist;

type
  [TestFixture]
  TSQLParser_Test=class(TObject)
  strict private
    Demo : TDataItem;
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
  end;

implementation

{ TSQLParser_Test }

procedure TSQLParser_Test.Setup;
begin
  Demo:=TStore.Load('BISamples','SQLite_Demo');
end;

procedure TSQLParser_Test.SimpleSelect;
var Data : TDataItem;
begin
  Data:=TBISQL.From(Demo,'select * from Customers');
  try
    Assert.IsNotNull(Data);
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
  Data:=TBISQL.From(Demo,'select count(*) from Customers');
  try
    Assert.IsNotNull(Data);
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
  Data:=TBISQL.From(Demo,'select ProductName from Products where UnitPrice>100');
  try
    Assert.IsNotNull(Data);
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
  Demo.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TSQLParser_Test);
end.
