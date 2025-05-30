{*********************************************}
{  TeeBI Software Library                     }
{  BIWeb Server TESTS                         }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.WebServer;

interface

uses
  BI.Arrays, BI.DataItem, DUnitX.TestFramework,
  BI.Web, BI.Persist;

type
  [TestFixture]
  TWebServer_Test=class(TObject)
  strict private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure RemoteQuery;
  end;

implementation

{ TWebServer_Test }

const
  Steema='Steema'; // localhost

procedure TWebServer_Test.RemoteQuery;
var Data : TDataItem;
begin
  Data:=TBIWebClient.Query(Steema,'SQLite_demo','select count(*) from Customers');
  try
    Assert.IsNotNull(Data);

    Data.Load(Data.AsTable);

    Assert.AreEqual<Int64>(Data.Count,0);
    Assert.AreEqual(Data.Items.Count,1);
    Assert.AreEqual<TDataKind>(Data[0].Kind, TDataKind.dkInt64);
    Assert.AreEqual<Int64>(Data[0].Int64Data[0], 91);
  finally
    Data.Free;
  end;
end;

procedure TWebServer_Test.Setup;
const
  SteemaHost=':web:steema.cat:zip';  // 'localhost'
begin
  if not TStores.Exists(Steema) then
     TStores.Add(Steema,SteemaHost);
end;

procedure TWebServer_Test.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TWebServer_Test);
end.
