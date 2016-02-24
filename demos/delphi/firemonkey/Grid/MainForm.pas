unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.Bind.Controls, Fmx.Bind.Navigator, BI.FMX.Grid, FMX.Layouts,
  FMX.StdCtrls, FMX.TabControl,
  //BI.FMX.Editor.Grid,
  BI.FMX.DataManager, FMX.Controls.Presentation;

type
  TGridDemoForm = class(TForm)
    TabControl1: TTabControl;
    TabOptions: TTabItem;
    TabData: TTabItem;
    Splitter1: TSplitter;
    Layout1: TLayout;
    BIGrid1: TBIGrid;
    BindNavigator1: TBindNavigator;
    CBAlternate: TCheckBox;
    CBSort: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBAlternateChange(Sender: TObject);
    procedure CBSortChange(Sender: TObject);
  private
    { Private declarations }

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  GridDemoForm: TGridDemoForm;

implementation

{$R *.fmx}

uses
  BI.Persist, BI.Data, BI.FMX.Grid.Grid;

procedure TGridDemoForm.CBAlternateChange(Sender: TObject);
begin
  BIGrid1.Alternate.Enabled:=CBAlternate.IsChecked;
end;

procedure TGridDemoForm.CBSortChange(Sender: TObject);
begin
  // Pending: Move up ColumnSort to parent TBIGrid class
  (BIGrid1.Plugin.GetObject as TBIFMXGrid).ColumnSort:=CBSort.IsChecked;
end;

procedure TGridDemoForm.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab:=TabOptions;

  //GridEditor:=TBIGridEditor.Embedd(Self,TabOptions,BIGrid1);

  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Customers'];

  TDataManager.EmbedChoose(Self,TabData,'BISamples' {,BIGrid1.Data}).OnSelect:=SelectedData;

  //BindNavigator1.DataSource:=BIGrid1.DataSource;

  //GridEditor.FillColumns;
end;

procedure TGridDemoForm.SelectedData(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager(Sender).Selected;

  if tmp<>nil then
  begin
    BIGrid1.Data:=tmp;
    //GridEditor.FillColumns;
  end;
end;

end.
