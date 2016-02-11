{*********************************************}
{  TeeBI Software Library                     }
{  BIWeb Server TESTS                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.WebServer;

interface

uses
  BI.Arrays, BI.Data, DUnitX.TestFramework,
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
  Steema='Steema';

procedure TWebServer_Test.RemoteQuery;
var Data : TDataItem;
begin
  Data:=TBIWebClient.Query(Steema,'SQLite_demo','select count(*) of Customers');
  try
    Assert.IsNotNull(Data);
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
