unit Unit15;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Data, BI.Data.RTTI, BI.VCL.Grid,
  Space_Flights, Vcl.StdCtrls, System.Diagnostics, Vcl.ExtCtrls,
  BI.VCL.DataControl;

type
  TRTTIDemo = class(TForm)
    BIGrid2: TBIGrid;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    Button8: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    Customers,
    Data : TDataItem;

    ORM : TTypeProvider<TFlight>;

    procedure AddData;
    procedure RefreshGrid;
  public
    { Public declarations }
  end;

var
  RTTIDemo: TRTTIDemo;

implementation

{$R *.dfm}

uses
  BI.Arrays, Customers, System.Rtti, BI.VCL.DataViewer;

procedure TRTTIDemo.RefreshGrid;
begin
  BIGrid1.RefreshData;
  Label1.Caption:='Count: '+ORM.Count.ToString;
end;

procedure TRTTIDemo.Button1Click(Sender: TObject);
begin
  ORM.Remove(Vostok1);
  RefreshGrid;
end;

procedure TRTTIDemo.Button2Click(Sender: TObject);
var N : TInteger;
    tmp : TFlight;
begin
  N:=ORM.Find(Apollo[2]);

  if N<>-1 then
  begin
    tmp:=Apollo[2];
    tmp.Remark:='UPDATED !';

    ORM.Update(N,tmp);

    RefreshGrid;
  end;
end;

procedure TRTTIDemo.Button3Click(Sender: TObject);
begin
  ORM.Clear;
  RefreshGrid;
end;

procedure TRTTIDemo.Button4Click(Sender: TObject);
begin
  AddData;
  RefreshGrid;
end;

type
  TORMAccess=class(TRTTIProvider);

procedure TRTTIDemo.Button5Click(Sender: TObject);
const
  Samples=1000;

var t1 : TStopwatch;
    tmp1,
    tmp2 : TFlight;
    t : Integer;
begin
  tmp1:=Vostok1;
  tmp2:=Apollo[2];

  t1:=TStopwatch.StartNew;

  for t:=1 to Samples do
  begin
    ORM.Add(tmp1);
    ORM.Add(tmp2);
  end;

  Caption:='Time to add '+Samples.ToString+' Objects: '+t1.ElapsedMilliseconds.ToString+' msec';

  RefreshGrid;
end;

procedure TRTTIDemo.Button6Click(Sender: TObject);
const
  Samples=100000;

var t1 : TStopwatch;
    tmp : TFlight;
    N,
    t : Integer;
begin
  tmp:=Vostok1;

  t1:=TStopwatch.StartNew;

  N:=Data.Count;
  Data.Resize(N+Samples);

  for t:=0 to Samples-1 do
      ORM.Update(N+t,tmp);

  Caption:='Time to update '+Samples.ToString+' rows: '+t1.ElapsedMilliseconds.ToString+' msec';

  RefreshGrid;
end;

var tmp : TFlight;
procedure TRTTIDemo.Button7Click(Sender: TObject);
begin
  if ORM.Count>4 then
  begin
    tmp:=ORM[4];

    Caption:='Found: '+ORM.Find(tmp).ToString;
  end
  else
    Caption:='Empty data. Not found';
end;

procedure TRTTIDemo.Button8Click(Sender: TObject);
begin
  TDataViewer.View(Self,Data);
end;

procedure TRTTIDemo.FormCreate(Sender: TObject);

  procedure SetupCustomers;
  var tmp : TTypeProvider<TCustomer>;
  begin
    tmp:=TTypeProvider<TCustomer>.Create(Self);

    tmp.Members:=TRttiMembers.Both; // Default = Both

    Customers:=tmp.Data;
    tmp.Primary:=Customers['ID'];

    tmp.Add(CustomerArray);

    BIGrid2.Data:=Customers;
  end;

  procedure SetupFlights;
  begin
    ORM:=TTypeProvider<TFlight>.Create(Self);
    Data:=ORM.Data;

    AddData;

    BIGrid1.Data:=Data;
  end;

begin
  SetupFlights;
  SetupCustomers;
end;

procedure TRTTIDemo.FormDestroy(Sender: TObject);
begin
  Customers.Free;
  Data.Free;
end;

procedure TRTTIDemo.AddData;
begin
  ORM.Add(Flights);
  ORM.Add(Vostok1);
  ORM.Add(Apollo);
  ORM.Add(Viking,'Data');
end;

end.
