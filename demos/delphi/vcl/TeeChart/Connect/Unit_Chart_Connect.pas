unit Unit_Chart_Connect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeeComma,
  Vcl.ExtCtrls, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.Series,
  Vcl.StdCtrls, BI.DataItem, VCLBI.Chart.Source;

type
  TFormTeeConnect = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    TeeCommander1: TTeeCommander;
    Series1: TBarSeries;
    Button1: TButton;
    TeeBISource1: TTeeBISource;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    function ByCode:TTeeBISource;
  public
    { Public declarations }
  end;

var
  FormTeeConnect: TFormTeeConnect;

implementation

{$R *.dfm}

uses
  BI.Persist, VCLBI.Editor.Chart.Source;

procedure TFormTeeConnect.Button1Click(Sender: TObject);
begin
  TBISourceEditor.Edit(Self,Series1.DataSource as TTeeBISource);
end;

// Example creating a TTeeBISource.
// Not necessary if using the design-time Series editor dialog
function TFormTeeConnect.ByCode:TTeeBISource;
var Animals : TDataItem;
begin
  Animals:=TStore.Load('Animals')['Animals'];

  result:=TTeeBISource.Create(Self);
  result.Data:=Animals;
  result.Open;
end;

procedure TFormTeeConnect.FormCreate(Sender: TObject);
begin
  // BI Data -> Series1 by code:

  // Series1.DataSource:=ByCode;

  // Series1.MandatoryValueList.ValueSource:='Weight';
  // Series1.XLabelsSource:='Name';
  // Series1.ColorSource:='Color';
end;

end.
