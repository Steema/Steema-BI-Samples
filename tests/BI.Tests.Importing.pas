{*********************************************}
{  TeeBI Software Library                     }
{  TESTS Importing data                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Importing;

{
  Several tests about importing data into TDataItem, from different formats
}

// Remove "." to run tests using a different 3rd party XML engine than MSXML:
{.$DEFINE USE_OXML}
{.$DEFINE USE_OMNI}

// Remove "." to run tests using a different 3rd party JSON engine than System.JSON:
{.$DEFINE USE_SUPEROBJECT}

interface

uses
  BI.Arrays, BI.DataItem, DUnitX.TestFramework,

  {$IFDEF USE_OXML}
  BI.XMLData.OXml,
  {$ELSE}
  {$IFDEF USE_OMNI}
  BI.XMLData.Omni,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF USE_SUPEROBJECT}
  BI.JSON.SuperObject,
  {$ENDIF}

  BI.CSV, BI.XMLData, BI.ClientDataset, BI.JSON, BI.HTML;

type
  [TestFixture]
  TImporting_Test=class(TObject)
  strict private

    procedure AssertAB(const AData:TDataItem);
    procedure Assert123(const AData:TDataItem; const Inverted:Boolean=False);

    function FindHTMLTestFile:String;
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

    [Test]
    procedure JSON;

    {$IFDEF USE_OXML} // <-- Pending problem, standard MSXML raises exception
    [Test]
    {$ELSE}
    {$IFDEF USE_OMNI}
    [Test]
    {$ENDIF}
    {$ENDIF}
    procedure XML;

    {$IFDEF USE_OXML} // <-- Pending problem, standard MSXML raises exception
    [Test]
    {$ELSE}
    {$IFDEF USE_OMNI}
    [Test]
    {$ENDIF}
    {$ENDIF}
    procedure HTML;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TImporting_Test }

procedure TImporting_Test.Assert123(const AData:TDataItem; const Inverted:Boolean=False);
begin
  Assert.IsNotNull(AData);
  Assert.AreEqual<Int64>(AData.Count,1);
  Assert.AreEqual(AData.Items.Count,3);

  Assert.AreEqual<TDataKind>(AData[0].Kind,TDataKind.dkInt32);
  Assert.AreEqual<TDataKind>(AData[1].Kind,TDataKind.dkInt32);
  Assert.AreEqual<TDataKind>(AData[2].Kind,TDataKind.dkInt32);

  if Inverted then
  begin
    Assert.AreEqual(AData[0].Int32Data[0],3);
    Assert.AreEqual(AData[1].Int32Data[0],2);
    Assert.AreEqual(AData[2].Int32Data[0],1);
  end
  else
  begin
    Assert.AreEqual(AData[0].Int32Data[0],1);
    Assert.AreEqual(AData[1].Int32Data[0],2);
    Assert.AreEqual(AData[2].Int32Data[0],3);
  end;
end;

procedure TImporting_Test.CSV;
var Data : TDataItem;
begin
  Data:=TBICSV.FromText('1,2,3');
  try
    Assert123(Data);
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

procedure TImporting_Test.AssertAB(const AData:TDataItem);
begin
  Assert.IsNotNull(AData);
  Assert.AreEqual<Int64>(AData.Count,1);
  Assert.AreEqual(AData.Items.Count,2);

  Assert.AreEqual<TDataKind>(AData[0].Kind,TDataKind.dkText);
  Assert.AreEqual<TDataKind>(AData[1].Kind,TDataKind.dkText);

  Assert.AreEqual(AData[0].TextData[0],'A');
  Assert.AreEqual(AData[1].TextData[0],'B');
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
      AssertAB(Data);
    finally
      Data.Free;
    end;
  finally
    CSV.Free;
  end;
end;

function TImporting_Test.FindHTMLTestFile:String;
const
  HTMLFile='BI.Test.HTML.html';
begin
  result:=HTMLFile;

  if not TFile.Exists(result) then
  begin
    result:=TPath.Combine('.\..',result);

    if not TFile.Exists(result) then
    begin
      result:=TPath.Combine('.\..',result);

      Assert.IsTrue(TFile.Exists(result),'File: '+HTMLFile+' cannot be found');
    end;
  end;
end;

procedure TImporting_Test.HTML;
var Data : TDataItem;
begin
  Data:=TBIHTML.FromFile(FindHTMLTestFile);
  try
    Assert.IsNotNull(Data);
    Assert.AreEqual<Int64>(Data.Items.Count,15);
  finally
    Data.Free;
  end;
end;

procedure TImporting_Test.JSON;
var Data : TDataItem;
begin
  Data:=TBIJSON.FromText('{"1":1, "2":2, "3":3}');
  try
    Assert123(Data {$IFDEF USE_SUPEROBJECT},True{$ENDIF});
  finally
    Data.Free;
  end;
end;

procedure TImporting_Test.Setup;
begin
  // When using alternative 3rd party "engines", initialize them:

  {$IFDEF USE_OXML}
  TBIXML.EngineClass:=TOXml;
  {$ELSE}
  {$IFDEF USE_OMNI}
  TBIXML.EngineClass:=TOmniXML;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF USE_SUPEROBJECT}
  TBIJSON.EngineClass:=TSuperObjectJSON;
  {$ENDIF}
end;

procedure TImporting_Test.TearDown;
begin
end;

procedure TImporting_Test.XML;
const
  XMLHeader='<?xml version="1.0" encoding="UTF-8"?>';

var Data : TDataItem;
begin
  Data:=TBIXML.FromText('<sample><a>1</a><b>2</b><c>3</c></sample>');
  try
    Assert.IsNotNull(Data);
    Assert.AreEqual<Int64>(Data.Items.Count,1);

    Assert123(Data['sample']);
  finally
    Data.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TImporting_Test);
end.
