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

  FMX.Dialogs,
  Data.Bind.Controls, Fmx.Bind.Navigator, BI.FMX.Grid, FMX.Layouts,
  FMX.StdCtrls, FMX.TabControl,
  BI.FMX.Editor.Grid,
  BI.FMX.DataManager, BI.FMX.DataControl;

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
  BI.Persist, BI.Data;

procedure TGridDemoForm.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab:=TabOptions;

  GridEditor:=TBIGridEditor.Embedd(Self,TabOptions,BIGrid1);

  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Products'];

//  BindNavigator1.DataSource:=BIGrid1.Plugin.DataSource;
end;

procedure TGridDemoForm.FormShow(Sender: TObject);
begin
  IManager:=TDataManager.EmbedChoose(Self,TabData,'BISamples');
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
