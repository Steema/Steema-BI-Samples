unit Unit_Grid_Detail;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.DataItem,
  Vcl.StdCtrls, Vcl.ExtCtrls, VCLBI.DataControl, VCLBI.Grid;

type
  TFormGridDetail = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    Label1: TLabel;
    RGDetail: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure RGDetailClick(Sender: TObject);
  private
    { Private declarations }

    procedure ShowDetail(const AData:TDataItem);
  public
    { Public declarations }
  end;

var
  FormGridDetail: TFormGridDetail;

implementation

{$R *.dfm}

uses
  VCLBI.Grid.DBGrid;

procedure TFormGridDetail.FormShow(Sender: TObject);
begin
  RGDetail.ItemIndex:=1;
  RGDetailClick(Self);
end;

procedure TFormGridDetail.RGDetailClick(Sender: TObject);
begin
  case RGDetail.ItemIndex of
    1: ShowDetail(BIGrid1.Data['address']['coord']);
    2: ShowDetail(BIGrid1.Data['grades']);
  else
    ShowDetail(nil);
  end;
end;

procedure TFormGridDetail.ShowDetail(const AData:TDataItem);
var Grid : TBIDBGrid;
begin
  Grid:=(BIGrid1.Plugin.GetObject as TBIDBGrid);

  Grid.Detail:=AData;
end;

end.
