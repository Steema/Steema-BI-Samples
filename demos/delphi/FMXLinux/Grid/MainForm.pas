unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.TabControl,

  Data.Bind.Controls, Fmx.Bind.Navigator, FMX.Layouts,

  FMXBI.Grid.TeeGrid, FMXBI.Editor.Grid, FMXBI.Grid,
  FMXBI.DataManager, FMXBI.DataControl;

type
  TGridDemoForm = class(TForm)
    TabControl1: TTabControl;
    TabOptions: TTabItem;
    TabData: TTabItem;
    Splitter1: TSplitter;
    Layout1: TLayout;
    BIGrid1: TBIGrid;
    BindNavigator1: TBindNavigator;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControl1Resize(Sender: TObject);
  private
    { Private declarations }

    GridEditor : TBIGridEditor;

    IManager : TDataManager;

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  GridDemoForm: TGridDemoForm;

implementation

{$R *.fmx}

uses
  BI.Persist, BI.DataItem;

procedure TGridDemoForm.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab:=TabOptions;

  GridEditor:=TBIGridEditor.Embedd(Self,TabOptions,BIGrid1);

  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Products'];

//  BindNavigator1.DataSource:=BIGrid1.Plugin.DataSource;
end;

procedure TGridDemoForm.FormShow(Sender: TObject);
begin
  IManager:=TDataManager.Embed(Self,TabData,TDataManagerEmbedMode.Choose,'BISamples');
  IManager.OnSelect:=SelectedData;

  TabControl1Resize(Self);
end;

procedure TGridDemoForm.SelectedData(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager(Sender).Selected;

  if tmp<>nil then
     BIGrid1.Data:=tmp;
end;

procedure TGridDemoForm.TabControl1Resize(Sender: TObject);
begin
  // Cosmetic, set sources listview width
  if IManager<>nil then
     IManager.LayoutSources.Width:=TabControl1.Width*0.5;
end;

end.
