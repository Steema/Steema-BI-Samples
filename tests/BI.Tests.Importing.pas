{*********************************************}
{  TeeBI Software Library                     }
{  TESTS Importing data                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Importing;

interface

uses
  BI.Arrays, BI.Data, DUnitX.TestFramework,
  BI.Data.CSV, BI.Data.XML, BI.Data.ClientDataset, BI.Data.JSON;

type
  [TestFixture]
  TImporting_Test=class(TObject)
  strict private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CSV;
    [Test]
    procedure CSVHeader;
    [Test]
    procedure CSVTabs;
  end;

implementation

{ TImporting_Test }

procedure TImporting_Test.CSV;
var Data : TDataItem;
begin
  Data:=TBICSV.FromText('1,2,3');
  try
    Assert.IsNotNull(Data);
    Assert.AreEqual<Int64>(Data.Count,1);
    Assert.AreEqual(Data.Items.Count,3);

    Assert.AreEqual<TDataKind>(Data[0].Kind,TDataKind.dkInt32);
    Assert.AreEqual<TDataKind>(Data[1].Kind,TDataKind.dkInt32);
    Assert.AreEqual<TDataKind>(Data[2].Kind,TDataKind.dkInt32);

    Assert.AreEqual(Data[0].Int32Data[0],1);
    Assert.AreEqual(Data[1].Int32Data[0],2);
    Assert.AreEqual(Data[2].Int32Data[0],3);
  finally
    Data.Free;
  end;
end;

const
  CRLF=#13#10;

procedure TImporting_Test.CSVHeader;
var Data : TDataItem;
begin
  Data:=TBICSV.FromText('A,B'+CRLF+'4.1,5.2');
  try
    Assert.IsNotNull(Data);
    Assert.AreEqual<Int64>(Data.Count,1);
    Assert.AreEqual(Data.Items.Count,2);

    Assert.AreEqual(Data[0].Name,'A');
    Assert.AreEqual(Data[1].Name,'B');

    Assert.AreEqual<TDataKind>(Data[0].Kind,TDataKind.dkSingle);
    Assert.AreEqual<TDataKind>(Data[1].Kind,TDataKind.dkSingle);

    Assert.AreEqual<Single>(Data[0].SingleData[0],4.1);
    Assert.AreEqual<Single>(Data[1].SingleData[0],5.2);
  finally
    Data.Free;
  end;
end;

const
  Tab=#9;

procedure TImporting_Test.CSVTabs;
var Data : TDataItem;
    CSV : TBICSV;
begin
  CSV:=TBICSV.Create;
  try
    CSV.Header.Headers:=TTextHeaders.No;

    Data:=CSV.ImportText('"A"'+Tab+'"B"');
    try
      Assert.IsNotNull(Data);
      Assert.AreEqual<Int64>(Data.Count,1);
      Assert.AreEqual(Data.Items.Count,2);

      Assert.AreEqual<TDataKind>(Data[0].Kind,TDataKind.dkText);
      Assert.AreEqual<TDataKind>(Data[1].Kind,TDataKind.dkText);

      Assert.AreEqual(Data[0].TextData[0],'A');
      Assert.AreEqual(Data[1].TextData[0],'B');
    finally
      Data.Free;
    end;
  finally
    CSV.Free;
  end;
end;

procedure TImporting_Test.Setup;
begin

end;

procedure TImporting_Test.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TImporting_Test);
end.
