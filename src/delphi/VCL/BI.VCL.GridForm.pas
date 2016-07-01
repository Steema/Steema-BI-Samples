unit BI.VCL.GridForm;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.Grid, Data.DB, Vcl.ExtCtrls,
  Vcl.DBCtrls, BI.Data, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  BI.VCL.DataControl;

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
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AData:TDataItem):TBIGridForm;

    class function Present(const AOwner:TComponent; const AData:TDataItem):TModalResult; overload;
  end;

implementation
