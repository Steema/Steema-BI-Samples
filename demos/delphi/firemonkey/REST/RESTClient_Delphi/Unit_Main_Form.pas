unit Unit_Main_Form;

interface

// Example using Delphi REST Components calling a BIWeb server
// No TeeBI units are used in this example. Just REST Fire

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  // FireDAC units, just to use a Memory Table to show the results
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  // REST units
  IPPeerClient, REST.Response.Adapter, REST.Client,

  // FMX Grid and binding units
  Data.Bind.Components, Data.Bind.ObjectScope, System.Rtti,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Grid, Data.Bind.DBScope, FMX.Layouts, FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Edit;

type
  TREST_BIWeb = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    Grid1: TGrid;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1ChangeTracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  REST_BIWeb: TREST_BIWeb;

implementation

{$R *.fmx}

procedure TREST_BIWeb.Button1Click(Sender: TObject);
begin
  RESTClient1.BaseURL:=Edit1.Text+'&format=.json';
  RESTRequest1.Execute;
end;

procedure TREST_BIWeb.Edit1ChangeTracking(Sender: TObject);
begin
  Button1.Enabled:=Trim(Edit1.Text)<>'';
end;

procedure TREST_BIWeb.FormCreate(Sender: TObject);
begin
  RESTRequest1.Execute;

  Edit1.Text:='http://steema.cat:15015/?data=sqlite_demo|customers';
end;

end.
