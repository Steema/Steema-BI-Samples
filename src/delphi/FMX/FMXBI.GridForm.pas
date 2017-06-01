unit FMXBI.GridForm;

interface

{
  A generic reusable / embeddable form with a BIGrid and a Navigator toolbar
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,

  FMX.Controls, FMX.Forms,

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, Data.Bind.Controls, Data.DB, FMX.Layouts,
  Fmx.Bind.Navigator, FMX.Grid, FMXBI.Grid, BI.DataItem, Data.Bind.Components,
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

    FOnDataChange : TNotifyEvent;

    procedure DataChange(Sender: TObject; Field: TField);
    procedure SetDataItem(const AData:TDataItem);
  public
    { Public declarations }

    Grid : TBIGrid;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AData:TDataItem):TBIGridForm; static;

    procedure MakeEditable;

    class function Present(const AOwner:TComponent; const AData:TDataItem):TModalResult; overload;

    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;
  end;

implementation
