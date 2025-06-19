unit VCLBI.Editor.BIGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  VCLBI.Grid, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TBIGridEditor = class(TForm)
    PageControl1: TPageControl;
    TabBIGrid: TTabSheet;
    GroupBox1: TGroupBox;
    CBAltRows: TCheckBox;
    BAltColor: TButton;
    CBRowNumbers: TCheckBox;
    CBFilter: TCheckBox;
    CBSearch: TCheckBox;
    TabPlugin: TTabSheet;
    Label1: TLabel;
    CBShowItems: TComboBox;
    CBColorize: TCheckBox;
    procedure BAltColorClick(Sender: TObject);
    procedure CBAltRowsClick(Sender: TObject);
    procedure CBFilterClick(Sender: TObject);
    procedure CBRowNumbersClick(Sender: TObject);
    procedure CBSearchClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBShowItemsChange(Sender: TObject);
    procedure CBColorizeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    Grid : TBIGrid;
    ISetting : Boolean;

    IPlugin : TCustomForm;

    function PluginClass:TCustomFormClass;
    procedure ShowPluginTabs;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const ABIGrid:TBIGrid); static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGrid:TBIGrid):TBIGridEditor; static;

    procedure Refresh(const AGrid:TBIGrid);
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Editor.Grid; // <-- plugin editor

const
  RegistryGridEditor='GridEditor';

procedure TBIGridEditor.BAltColorClick(Sender: TObject);
var tmp : TColor;
begin
  if TUICommon.EditColor(Self,Grid.Alternate.Color,tmp) then
     Grid.Alternate.Color:=tmp;
end;

procedure TBIGridEditor.CBAltRowsClick(Sender: TObject);
begin
  Grid.Alternate.Enabled:=CBAltRows.Checked;
end;

procedure TBIGridEditor.CBColorizeClick(Sender: TObject);
begin
//  if not ISetting then
//     Grid.Colorize.Enabled:=CBColorize.Checked;
end;

procedure TBIGridEditor.CBFilterClick(Sender: TObject);
begin
  if not ISetting then
     Grid.Filters.Enabled:=CBFilter.Checked;
end;

procedure TBIGridEditor.CBRowNumbersClick(Sender: TObject);
begin
  if not ISetting then
  begin
    Grid.RowNumbers.Enabled:=CBRowNumbers.Checked;
    // DBGridEditor.FillColumns;
  end;
end;

procedure TBIGridEditor.CBSearchClick(Sender: TObject);
begin
  if not ISetting then
     Grid.Search.Enabled:=CBSearch.Checked;
end;

procedure TBIGridEditor.CBShowItemsChange(Sender: TObject);
begin
  Grid.ShowItems:=TGridShowItems(CBShowItems.ItemIndex);
end;

class procedure TBIGridEditor.Edit(const AOwner: TComponent;
  const ABIGrid: TBIGrid);
begin
  with TBIGridEditor.Create(AOwner) do
  try
    Refresh(ABIGrid);
    ShowModal;
  finally
    Free;
  end;
end;

class function TBIGridEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl; const AGrid: TBIGrid): TBIGridEditor;
begin
  result:=TBIGridEditor.Create(AOwner);
  result.Refresh(AGrid);

  TUICommon.AddForm(result,AParent);
end;

procedure TBIGridEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,RegistryGridEditor);
end;

procedure TBIGridEditor.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,RegistryGridEditor);

  TabPlugin.TabVisible:=PluginClass<>nil;
end;

procedure TBIGridEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabPlugin then
     if TabPlugin.ControlCount=0 then
        ShowPluginTabs;
end;

procedure TBIGridEditor.Refresh(const AGrid: TBIGrid);
begin
  Grid:=AGrid;

  ISetting:=True;

  CBAltRows.Checked:=Grid.Alternate.Enabled;
  CBRowNumbers.Checked:=Grid.RowNumbers.Enabled;
//  CBColorize.Checked:=Grid.Colorize.Enabled;
  CBFilter.Checked:=Grid.Filters.Enabled;
  CBSearch.Checked:=Grid.Search.Enabled;
  CBShowItems.ItemIndex:=Ord(Grid.ShowItems);

  ISetting:=False;
end;

function TBIGridEditor.PluginClass:TCustomFormClass;
var tmp : String;
    tmpClass : TPersistentClass;
begin
  result:=nil;

  tmp:=Grid.Plugin.EditorClass;

  if tmp<>'' then
  begin
    tmpClass:=GetClass(tmp);

    if tmpClass<>nil then
       result:=TCustomFormClass(tmpClass);
  end;
end;

procedure TBIGridEditor.ShowPluginTabs;
var tmp : TCustomFormClass;
begin
  tmp:=PluginClass;

  if tmp<>nil then
  begin
    IPlugin:=TCustomFormClass(tmp).Create(Self);
    IPlugin.Tag:=NativeInt(Grid.Plugin.GetObject);
    TUICommon.AddForm(IPlugin,TabPlugin);
  end;
end;

end.
