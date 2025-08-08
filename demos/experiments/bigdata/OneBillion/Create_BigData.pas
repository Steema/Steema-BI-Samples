unit Create_BigData;

interface

{
  Create a sample database with several tables and fill them with random
  dummy data.

  The final TDataItem is saved to disk, so it can be reloaded later on.
}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IOUtils,

  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.Diagnostics,
  {$ENDIF}

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  BI.DataItem;

type
  TFormCreate = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ECustomers: TEdit;
    Label2: TLabel;
    EProducts: TEdit;
    Label3: TLabel;
    ESales: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    ButtonView: TButton;
    LFileName: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonViewClick(Sender: TObject);
  private
    { Private declarations }

    Customers,
    Categories,
    Products,
    Sales  : TDataItem;

  public
    { Public declarations }

    BigData : TDataItem;

    class procedure ShowDataSizes(const AData:TDataItem; const ALines:TStrings); static;

    function RandomCustomers(const AQuantity:Integer):TDataItem;
    function RandomProducts(const AQuantity:Integer):TDataItem;
    function RandomSales(const AQuantity:Integer):TDataItem;
  end;

implementation

{$R *.dfm}

uses
  BI.Persist, BI.Geographic, BI.Info, BI.UI, BI.DataSource,
  VCLBI.DataViewer;

function GetCategories:TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Name:='Categories';

  result.Items.Add('ID',TDataKind.dkInt32);
  result.Items.Add('Category',TDataKind.dkText);

  result.Resize(18);

  result[0].Int32Data:=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17];

  result[1].TextData:=['Dairy','Oils','Fats','Fruits','Vegetables','Cereals',
     'Bakery','Meat','Fish','Eggs','Sugars','Salts','Spices','Beverages','Snacks',
     'Desserts','Suplements','Infants'];
end;

function Random_Product_Name:String;
const
  Names:Array of String=['Flavor','Savory','Culinary','Taste','Gourmet','Dish','Food'];
  Suffix:Array of String=['Haven','Spectrum','Canvas','Odyssey','Grove','Premium','Delicious'];
begin
  result:=Names[Random(Length(Names))]+' '+
          Suffix[Random(Length(Suffix))]+' '+
          IntToStr(Random(100));
end;

function Random_Customer_Name:String;
const
  Names1:Array of String=['John','Acme','Hoppe','Mary','Trend','Luxe','Sleek'];
  Names2:Array of String=['Posh','Urban','Chic','Wear','Mystiq','Pulse','Glamor'];
  Suffix:Array of String=['Inc','Corp','PLC','LLC','Ltd','SA','BV','GmbH','Sarl'];
begin
  result:=Names1[Random(Length(Names1))]+' '+
          Names2[Random(Length(Names2))]+' '+
          Suffix[Random(Length(Suffix))];
end;

const
  BigCountries:Array of Integer=[
36,
76,
124,
156,
250,
276,
356,
360,
392,
484,
566,
643,
710,
724,
818,
826,
840];

function TFormCreate.RandomCustomers(const AQuantity:Integer):TDataItem;
var t : Integer;
    Countries : TDataItem;
    tmpCountry : Integer;
begin
  result:=TDataItem.Create(True);
  result.Name:='Customers';

  result.Items.Add('ID',TDataKind.dkInt32);
  result.Items.Add('Name',TDataKind.dkText);
  result.Items.Add('IsCompany',TDataKind.dkBoolean);
  result.Items.Add('Country',TDataKind.dkInt32);

  Countries:=TGeo.Country.Countries;

  result.Resize(AQuantity);

  for t:=0 to AQuantity-1 do
  begin
    result[0].Int32Data[t]:=t;
    result[1].TextData[t]:=Random_Customer_Name;
    result[2].BooleanData[t]:=Random(101)<50;

    // Give precedence to some countries, then the rest

    if Random(100)<50 then
       tmpCountry:=BigCountries[Random(Length(BigCountries))]
    else
    repeat
      tmpCountry:=Countries[5].Int32Data[Random(Countries.Count)];
    until tmpCountry<>0;

    result[3].Int32Data[t]:=tmpCountry;
  end;
end;

function TFormCreate.RandomProducts(const AQuantity:Integer):TDataItem;
var t : Integer;
begin
  result:=TDataItem.Create(True);
  result.Name:='Products';

  result.Items.Add('ID',TDataKind.dkInt32);
  result.Items.Add('Product',TDataKind.dkText);
  result.Items.Add('Category',TDataKind.dkInt32);

  result.Resize(AQuantity);

  for t:=0 to AQuantity-1 do
  begin
    result[0].Int32Data[t]:=t;
    result[1].TextData[t]:=Random_Product_Name;
    result[2].Int32Data[t]:=Random(Categories.Count);
  end;
end;

function CurrentYear:Word;
var Month,Day:Word;
begin
  DecodeDate(Now,result,Month,Day);
end;

function TFormCreate.RandomSales(const AQuantity: Integer): TDataItem;
var t : Integer;
    tmpDate : TDateTime;
    DeltaTime : Single;
begin
  result:=TDataItem.Create(True);
  result.Name:='Sales';

  result.Items.Add('Date',TDataKind.dkDateTime);
  result.Items.Add('Customer',TDataKind.dkInt32);
  result.Items.Add('Product',TDataKind.dkInt32);
  result.Items.Add('Quantity',TDataKind.dkSingle);
  result.Items.Add('Total',TDataKind.dkSingle);

  result.Resize(AQuantity);

  DeltaTime:=5000/AQuantity;

  tmpDate:=EncodeDate(CurrentYear-10,1,1);

  for t:=0 to AQuantity-1 do
  begin
    tmpDate:=tmpDate+DeltaTime;

    result[0].DateTimeData[t]:=tmpDate;
    result[1].Int32Data[t]:=Random(Customers.Count);
    result[2].Int32Data[t]:=Random(Products.Count);
    result[3].SingleData[t]:=1+(Random(10000)*0.01);
    result[4].SingleData[t]:=1+(Random(50000)*0.01);
  end;
end;

class procedure TFormCreate.ShowDataSizes(const AData: TDataItem;
  const ALines: TStrings);
var tmp : TDataInfo.TDataSizes;
begin
  tmp:=TDataInfo.Sizes(AData);

  ALines.Add(' Tables  : '+TCommonUI.ThousandsToString(tmp.Tables));
  ALines.Add(' Columns : '+TCommonUI.ThousandsToString(tmp.Columns));
  ALines.Add(' Rows    : '+TCommonUI.ThousandsToString(tmp.Rows));
  ALines.Add(' Cells   : '+TCommonUI.ThousandsToString(tmp.Cells));
end;

procedure TFormCreate.Button1Click(Sender: TObject);

  procedure BigDatabase;
  begin
    BigData.Free;

    BigData:=TDataItem.Create(True);
    BigData.Name:='One Billion Data';

    // Add dummy tables

    Customers:=RandomCustomers(StrToInt(ECustomers.Text));
    BigData.Items.Add(Customers);

    Categories:=GetCategories;
    BigData.Items.Add(Categories);

    Products:=RandomProducts(StrToInt(EProducts.Text));
    BigData.Items.Add(Products);

    Sales:=RandomSales(StrToInt(ESales.Text)*Customers.Count);
    BigData.Items.Add(Sales);

    // Link tables after adding them to BigData.
    // This enables querying columns from different tables transparently.

    Customers['Country'].Master:=TGeo.Country.Countries['ISO-3166-Num'];
    Products['Category'].Master:=Categories['ID'];
    Sales['Customer'].Master:=Customers['ID'];
    Sales['Product'].Master:=Products['ID'];
  end;

var s : TStopwatch;
begin
  Screen.Cursor:=crHourGlass;
  try
    TGeo.Check;

    Memo1.Clear;

    s:=TStopwatch.StartNew;
    BigDatabase;
    Memo1.Lines.Add('Time creating: '+TCommonUI.MSecToString(s.ElapsedMilliseconds));

    s:=TStopwatch.StartNew;
    TDataItemPersistence.Save(BigData,LFileName.Caption);
    Memo1.Lines.Add('Time saving: '+TCommonUI.MSecToString(s.ElapsedMilliseconds));

    Memo1.Lines.Add('Data size: '+TCommonUI.BytesToString(TBIFileSource.GetFileSize(LFileName.Caption)));

    Memo1.Lines.Add('Quantities:');

    ShowDataSizes(BigData,Memo1.Lines);

    ButtonView.Enabled:=True;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFormCreate.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TFormCreate.ButtonViewClick(Sender: TObject);
begin
  TDataViewer.View(Self,BigData);
end;

procedure TFormCreate.FormCreate(Sender: TObject);
begin
  LFileName.Caption:=TPath.Combine(TPath.GetTempPath,'big_data.bi');
end;

end.
