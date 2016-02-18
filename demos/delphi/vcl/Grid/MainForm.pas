unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.Grid, Vcl.ExtCtrls, Vcl.ComCtrls,
  BI.VCL.Editor.Grid, BI.VCL.DataManager, Vcl.DBCtrls;

type
  TGridDemoForm = class(TForm)
    PageControl1: TPageControl;
    TabOptions: TTabSheet;
    TabData: TTabSheet;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    Panel2: TPanel;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    GridEditor : TBIGridEditor;

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  GridDemoForm: TGridDemoForm;

implementation

{$R *.dfm}

uses
  BI.Persist, BI.Data;

procedure TGridDemoForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabOptions;

  GridEditor:=TBIGridEditor.Embedd(Self,TabOptions,BIGrid1);

  TDataManager.EmbedChoose(Self,TabData,'BISamples').OnSelect:=SelectedData;

  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Customers'];

  DBNavigator1.DataSource:=BIGrid1.DataSource;

  GridEditor.FillColumns;
end;

procedure TGridDemoForm.SelectedData(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager(Sender).Selected;

  if tmp<>nil then
  begin
    BIGrid1.Data:=tmp;
    GridEditor.FillColumns;
  end;
end;

end.
