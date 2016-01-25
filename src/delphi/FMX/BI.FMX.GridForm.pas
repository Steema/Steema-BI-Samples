unit BI.FMX.GridForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,

  {$IF CompilerVersion<=27}
  {$DEFINE FMX2}
  {$ENDIF}

  FMX.Controls, FMX.Forms,
  {$IF COMPILERVERSION>25}
  FMX.Graphics,
  {$ENDIF}
  {$IFNDEF FMX2}
  FMX.Controls.Presentation,
  {$ENDIF}
  FMX.Dialogs, Data.Bind.Controls, Data.DB, FMX.Layouts,
  Fmx.Bind.Navigator, FMX.Grid, BI.FMX.Grid, BI.Data, Data.Bind.Components,
  Data.Bind.DBScope, FMX.StdCtrls;

type
  TBIGridForm = class(TForm)
    Layout1: TLayout;
    BindNavigator1: TBindNavigator;
    Layout2: TLayout;
    LRow: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Grid : TBIGrid;

    procedure DataChange(Sender: TObject; Field: TField);
  public
    { Public declarations }

    class function Present(const AOwner:TComponent; const AData:TDataItem):TModalResult; overload;
  end;

implementation
