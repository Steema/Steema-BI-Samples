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
  System.Diagnostics,

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

    CreateData : TDataItem;

    function RandomCustomers(const AQuantity:Integer):TDataItem;
    function RandomProducts(const AQuantity:Integer):TDataItem;
    function RandomSales(const AQuantity:Integer):TDataItem;
  end;

var
  FormCreate: TFormCreate;

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

function TFormCreate.RandomCustomers(const AQuantity:Integer):TDataItem;
var t : Integer;
    Countries : TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Name:='Customers';

  Countries:=TGeo.Country.Countries;

  result.Items.Add('ID',TDataKind.dkInt32);
  result.Items.Add('Name',TDataKind.dkText);
  result.Items.Add('IsCompany',TDataKind.dkBoolean);
  result.Items.Add('Country',TDataKind.dkInt32).Master:=Countries['ISO-3166-Num'];

  result.Resize(AQuantity);

  for t:=0 to AQuantity-1 do
  begin
    result[0].Int32Data[t]:=t;
    result[1].TextData[t]:=Random_Customer_Name;
    result[2].BooleanData[t]:=Random(101)<50;
    result[3].Int32Data[t]:=Countries[5].Int32Data[Random(Countries.Count)];
  end;
end;

function TFormCreate.RandomProducts(const AQuantity:Integer):TDataItem;
var t : Integer;
begin
  result:=TDataItem.Create(True);
  result.Name:='Products';

  result.Items.Add('ID',TDataKind.dkInt32);
  result.Items.Add('Product',TDataKind.dkText);
  result.Items.Add('Category',TDataKind.dkInt32).Master:=Categories['ID'];

  result.Resize(AQuantity);

  for t:=0 to AQuantity-1 do
  begin
    result[0].Int32Data[t]:=t;
    result[1].TextData[t]:=Random_Product_Name;
    result[2].Int32Data[t]:=Random(Categories.Count);
  end;
end;

function TFormCreate.RandomSales(const AQuantity: Integer): TDataItem;
var t : Integer;
    tmpDate : TDateTime;
    DeltaTime : Single;
begin
  result:=TDataItem.Create(True);
  result.Name:='Sales';

  result.Items.Add('Date',TDataKind.dkDateTime);
  result.Items.Add('Customer',TDataKind.dkInt32).Master:=Customers[0];
  result.Items.Add('Product',TDataKind.dkInt32).Master:=Products[0];
  result.Items.Add('Quantity',TDataKind.dkSingle);
  result.Items.Add('Total',TDataKind.dkSingle);

  result.Resize(AQuantity);

  DeltaTime:=50000/AQuantity;

  tmpDate:=EncodeDate(2025,1,1);

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

procedure TFormCreate.Button1Click(Sender: TObject);

  procedure CreateDatabase;
  begin
    CreateData.Free;

    CreateData:=TDataItem.Create(True);
    CreateData.Name:='One Billion Data';

    Customers:=RandomCustomers(StrToInt(ECustomers.Text));
    CreateData.Items.Add(Customers);

    Categories:=GetCategories;
    CreateData.Items.Add(Categories);

    Products:=RandomProducts(StrToInt(EProducts.Text));
    CreateData.Items.Add(Products);

    Sales:=RandomSales(StrToInt(ESales.Text)*Customers.Count);
    CreateData.Items.Add(Sales);
  end;

var s : TStopwatch;
begin
  Screen.Cursor:=crHourGlass;
  try
    TGeo.Check;

    Memo1.Clear;

    s:=TStopwatch.StartNew;
    CreateDatabase;
    Memo1.Lines.Add('Time creating: '+TCommonUI.MSecToString(s.ElapsedMilliseconds));

    s:=TStopwatch.StartNew;
    TDataItemPersistence.Save(CreateData,LFileName.Caption);
    Memo1.Lines.Add('Time saving: '+TCommonUI.MSecToString(s.ElapsedMilliseconds));

    Memo1.Lines.Add('Data size: '+TCommonUI.BytesToString(TBIFileSource.GetFileSize(LFileName.Caption)));

    Memo1.Lines.Add('Quantity of rows: '+Format('%.0n',[TDataInfo.TotalCount(CreateData)+0.0]));

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
  TDataViewer.View(Self,CreateData);
end;

procedure TFormCreate.FormCreate(Sender: TObject);
begin
  LFileName.Caption:=TPath.Combine(TPath.GetTempPath,'big_data.bi');
end;

end.
