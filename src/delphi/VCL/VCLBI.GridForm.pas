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
