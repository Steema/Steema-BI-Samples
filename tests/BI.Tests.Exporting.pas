{*********************************************}
{  TeeBI Software Library                     }
{  TESTS Exporting data                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Exporting;

interface

uses
  BI.Arrays, BI.Data, DUnitX.TestFramework,
  BI.Data.CSV, BI.Data.XML, BI.Data.JSON, BI.Data.DB, BI.Data.HTML;

type
  [TestFixture]
  TExporting_Test=class(TObject)
  strict private
    Data : TDataItem;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CSV;
    [Test]
    procedure HTML;
    [Test]
    procedure XML;
    [Test]
    procedure JSON;
  end;

implementation

{ TExporting_Test }

const
  CRLF=#13#10;

procedure TExporting_Test.CSV;
var S : String;
begin
  S:=TBICSVExport.AsString(Data);
  Assert.AreEqual(S,'"Name","Quantity","Price","Units","HasStock"'+CRLF+
                    '"Tomato",1000,123.456,"Kg",True'+CRLF+
                    '"Apples",768,0.78,"Tn",False'+CRLF+
                    '"Bananas",42,-454.1,"Pounds",True'+CRLF
                    );
end;

procedure TExporting_Test.HTML;
var S : String;
begin
  S:=TBIHTMLExport.AsString(Data);
  Assert.AreEqual(S,'<table class="">'+CRLF+
CRLF+
'<thead><tr><th align="left">Name</th><th align="right">Quantity&#x25B2;</th><th align="right">Price</th><th align="left">Units</th><th align="center">HasStock</th></tr>'+CRLF+
'</thead>'+CRLF+
CRLF+
'<tbody>'+CRLF+
CRLF+
'<tr><td>Tomato</td><td align="right">1000</td><td align="right">123.456</td><td>Kg</td><td align="center"><input type="checkbox" checked="checked" disabled/></td></tr>'+CRLF+
'<tr><td>Apples</td><td align="right">768</td><td align="right">0.78</td><td>Tn</td><td align="center"><input type="checkbox" disabled/></td></tr>'+CRLF+
'<tr><td>Bananas</td><td align="right">42</td><td align="right">-454.1</td><td>Pounds</td><td align="center"><input type="checkbox" checked="checked" disabled/></td></tr>'+CRLF+
'</tbody>'+CRLF+
'</table>'+CRLF
                    );
end;

const
  Tab=#9;

procedure TExporting_Test.JSON;
var S : String;
begin
  S:=TBIJSONExport.AsString(Data);
  Assert.AreEqual(S,

'['+CRLF+Tab+'{'+CRLF+Tab+Tab+'"Name": "Tomato",'+CRLF+Tab+Tab+
'"Quantity": 1000,'+CRLF+Tab+Tab+'"Price": 123.456,'+
CRLF+Tab+Tab+'"Units": "Kg",'+
CRLF+Tab+Tab+'"HasStock": "TRUE"'+CRLF+Tab+'},'+CRLF+Tab+'{'+CRLF+Tab+
Tab+'"Name": "Apples",'+CRLF+Tab+Tab+
'"Quantity": 768,'+CRLF+Tab+Tab+
'"Price": 0.78,'+CRLF+Tab+Tab+'"Units": "Tn",'+CRLF+Tab+Tab+
'"HasStock": "FALSE"'+CRLF+Tab+'},'+CRLF+Tab+'{'+CRLF+Tab+Tab+'"Name": "Bananas",'+CRLF+
Tab+Tab+'"Quantity": 42,'+CRLF+Tab+Tab+'"Price": -454.1,'+CRLF+Tab+Tab+
'"Units": "Pounds",'+CRLF+Tab+Tab+'"HasStock": "TRUE"'+CRLF+Tab+'}'+CRLF+
']'+CRLF
  );
end;

procedure TExporting_Test.Setup;
begin
  Data:=TDataItem.Create(True);

  Data.Items.Add('Name',TDataKind.dkText);
  Data.Items.Add('Quantity',TDataKind.dkInt32);
  Data.Items.Add('Price',TDataKind.dkDouble);
  Data.Items.Add('Units',TDataKind.dkText);
  Data.Items.Add('HasStock',TDataKind.dkBoolean);

  Data.Resize(3);

  Data[0].TextData[0]:='Tomato';
  Data[0].TextData[1]:='Apples';
  Data[0].TextData[2]:='Bananas';

  Data[1].Int32Data[0]:=1000;
  Data[1].Int32Data[1]:=768;
  Data[1].Int32Data[2]:=42;

  Data[2].DoubleData[0]:=123.456;
  Data[2].DoubleData[1]:=0.78;
  Data[2].DoubleData[2]:=-454.1;

  Data[3].TextData[0]:='Kg';
  Data[3].TextData[1]:='Tn';
  Data[3].TextData[2]:='Pounds';

  Data[4].BooleanData[0]:=True;
  Data[4].BooleanData[1]:=False;
  Data[4].BooleanData[2]:=True;
end;

procedure TExporting_Test.TearDown;
begin
  Data.Free;
  Data:=nil;
end;

procedure TExporting_Test.XML;
var S : String;
begin
  S:=TBIXMLExport.AsString(Data);
  Assert.AreEqual(S,

  '<?xml version="1.0" encoding="UTF-8" standalone="no" ?>'+CRLF+
  '<_>'+CRLF+
  '<item>'+CRLF+
  Tab+Tab+'<Name>"Tomato"</Name>'+CRLF+
  Tab+Tab+'<Quantity>1000</Quantity>'+CRLF+
  Tab+Tab+'<Price>123.456</Price>'+CRLF+
  Tab+Tab+'<Units>"Kg"</Units>'+CRLF+
  Tab+Tab+'<HasStock>true</HasStock>'+CRLF+
  '</item>'+CRLF+
  '<item>'+CRLF+
  Tab+Tab+'<Name>"Apples"</Name>'+CRLF+
  Tab+Tab+'<Quantity>768</Quantity>'+CRLF+
  Tab+Tab+'<Price>0.78</Price>'+CRLF+
  Tab+Tab+'<Units>"Tn"</Units>'+CRLF+
  Tab+Tab+'<HasStock>false</HasStock>'+CRLF+
  '</item>'+CRLF+
  '<item>'+CRLF+
  Tab+Tab+'<Name>"Bananas"</Name>'+CRLF+
  Tab+Tab+'<Quantity>42</Quantity>'+CRLF+
  Tab+Tab+'<Price>-454.1</Price>'+CRLF+
  Tab+Tab+'<Units>"Pounds"</Units>'+CRLF+
  Tab+Tab+'<HasStock>true</HasStock>'+CRLF+
  '</item>'+CRLF+
  '</_>'+CRLF
                    );
end;

initialization
  TDUnitX.RegisterTestFixture(TExporting_Test);
end.
