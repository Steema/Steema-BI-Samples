unit VCLBI.GridForm;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.Grid, Data.DB, Vcl.ExtCtrls,
  Vcl.DBCtrls, BI.DataItem, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  VCLBI.DataControl;

type
  TBIGridForm = class(TForm)
    PanelNav: TPanel;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    LRow: TLabel;
    Grid: TBIGrid;
    procedure FormShow(Sender: TObject);
    procedure GridDataChange(Sender: TObject);
  private
    { Private declarations }

    function GetDataItem: TDataItem;
    procedure RefreshNavigator;
    procedure SetDataItem(const Value: TDataItem);
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AData:TDataItem):TBIGridForm;

    procedure MakeEditable;

    class function Present(const AOwner:TComponent; const AData:TDataItem):TModalResult; overload;

    property Data:TDataItem read GetDataItem write SetDataItem;
  end;

implementation

{$R *.dfm}

uses
  BI.DataSource;

class function TBIGridForm.Embedd(const AOwner: TComponent;
          const AParent:TWinControl; const AData: TDataItem): TBIGridForm;
begin
  result:=TBIGridForm.Create(AOwner);
  result.Grid.Data:=AData;

  TUICommon.AddForm(result,AParent);
end;

procedure TBIGridForm.RefreshNavigator;
begin
  DBNavigator1.DataSource:=Grid.DataSource;

  if Grid.Data=nil then
     LRow.Caption:='0/0';
end;

procedure TBIGridForm.FormShow(Sender: TObject);
begin
  RefreshNavigator;
end;

function TBIGridForm.GetDataItem: TDataItem;
begin
  result:=Grid.Data;
end;

procedure TBIGridForm.GridDataChange(Sender: TObject);
var tmp : TDataSet;
begin
  tmp:=Grid.DataSource.DataSet;
  LRow.Caption:=IntToStr(tmp.RecNo)+'/'+IntToStr(tmp.RecordCount);
end;

procedure TBIGridForm.MakeEditable;
begin
  Grid.ReadOnly:=False;

  DBNavigator1.VisibleButtons:=DBNavigator1.VisibleButtons+
                    [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
end;

class function TBIGridForm.Present(const AOwner:TComponent; const AData: TDataItem): TModalResult;
begin
  with TBIGridForm.Create(nil) do
  try
    Caption:=AData.Name;
    Grid.Data:=AData;

    result:=ShowModal;
  finally
    Free;
  end;
end;

procedure TBIGridForm.SetDataItem(const Value: TDataItem);
begin
  Grid.Data:=Value;
  RefreshNavigator;
end;

end.
