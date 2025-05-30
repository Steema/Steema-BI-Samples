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

{$R *.fmx}

uses
  BI.UI;

function TBIGridEditor.GetPlugin:TBIGridPlugin;
begin
  result:=Grid.Plugin;
end;

procedure TBIGridEditor.CBAlternateChange(Sender: TObject);
begin
  Grid.Alternate.Enabled:=CBAlternate.IsChecked;
end;

procedure TBIGridEditor.CBColorizeChange(Sender: TObject);
begin
  if CBColorize.IsChecked then
     Grid.Colorize
  else
     Grid.Colorize(nil);
end;

procedure TBIGridEditor.CBSortChange(Sender: TObject);
var tmp : TBIGridPlugin;
begin
  tmp:=GetPlugin;

  if tmp<>nil then
     tmp.SortEnabled:=CBSort.IsChecked;
end;

class function TBIGridEditor.Edit(const AOwner: TComponent;
  const AGrid: TBIGrid): Boolean;
begin
  with TBIGridEditor.Create(AOwner) do
  try
    Refresh(AGrid);
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

class function TBIGridEditor.Embedd(const AOwner: TComponent;
  const AParent: TControl; const AGrid: TBIGrid): TBIGridEditor;
begin
  result:=TBIGridEditor.Create(AOwner);
  result.Refresh(AGrid);
  TUICommon.AddForm(result,AParent);
end;

procedure TBIGridEditor.Refresh(const AGrid: TBIGrid);
var tmp : TBIGridPlugin;
begin
  Grid:=AGrid;

  CBAlternate.IsChecked:=Grid.Alternate.Enabled;

  tmp:=GetPlugin;

  CBSort.Enabled:=(tmp<>nil) and tmp.CanSort;

  if CBSort.Enabled then
     CBSort.IsChecked:=tmp.SortEnabled;
end;

end.
