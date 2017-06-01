unit FMXBI.Editor.Grid;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.StdCtrls, FMXBI.Grid, BI.Grid.Plugin;

type
  TBIGridEditor = class(TForm)
    TabControl1: TTabControl;
    TabOptions: TTabItem;
    TabPlugin: TTabItem;
    CBAlternate: TCheckBox;
    CBSort: TCheckBox;
    GroupBox1: TGroupBox;
    CBColorize: TCheckBox;
    procedure CBAlternateChange(Sender: TObject);
    procedure CBSortChange(Sender: TObject);
    procedure CBColorizeChange(Sender: TObject);
  private
    { Private declarations }

    Grid : TBIGrid;

    function GetPlugin:TBIGridPlugin;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const AGrid:TBIGrid):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AGrid:TBIGrid):TBIGridEditor; static;

    procedure Refresh(const AGrid:TBIGrid);
  end;

implementation
