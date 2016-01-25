unit BI.VCL.GridForm;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.Grid, Data.DB, Vcl.ExtCtrls,
  Vcl.DBCtrls, BI.Data, BI.DataSet, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids;

type
  TBIGridForm = class(TForm)
    DataSource1: TDataSource;
    PanelNav: TPanel;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    LRow: TLabel;
    Grid: TBIGrid;
    BIDataset1: TBIDataset;
    procedure FormShow(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
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
