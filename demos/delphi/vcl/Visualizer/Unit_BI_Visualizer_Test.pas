unit Unit_BI_Visualizer_Test;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  BI.DataItem, BI.Persist, BI.DataSource, VCLBI.Visualizer, VCLBI.Grid,
  Vcl.ComCtrls, VCLBI.Editor.Summary, BI.Summary, Vcl.StdCtrls,
  VCLBI.Editor.Visualizer, Vcl.Buttons, VCLBI.Editor.Grid,
  VCLBI.Visualizer.Chart, VCLBI.Editor.Visualizer.Chart,
  VCLBI.DataViewer,
  VCLBI.Grid.DBGrid, VCLBI.DataManager, VCLBI.DataControl, BI.Query;

type
  TFormViz = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSettings: TTabSheet;
    Splitter1: TSplitter;
    SpeedButton1: TSpeedButton;
    TabData: TTabSheet;
    Panel4: TPanel;
    CBSample: TCheckBox;
    PanelSummary: TPanel;
    BRefresh: TButton;
    BIVisualizer1: TBIComposer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    BIQuery1: TBIQuery;
    CBQuery: TCheckBox;
    BData: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBSampleClick(Sender: TObject);
    procedure BRefreshClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CBQueryClick(Sender: TObject);
    procedure BDataClick(Sender: TObject);
  private
    { Private declarations }

    Data : TDataItem;

    VizEditor : TVisualizerEditor;
    SummaryEditor : TSummaryEditor;
    Summary : TSummary;

    DataManager : TDataManager;

    procedure AddSummaryEditor(const AParent:TWinControl);
    procedure CreateSummary;
    procedure Recalculate(Sender: TObject);
    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormViz: TFormViz;

implementation

{$R *.dfm}

uses
  BI.UI, BI.Expression, VCLBI.Editor.ControlTree;

procedure TFormViz.FormDestroy(Sender: TObject);
begin
  Summary.Free;
  BIVisualizer1.Data.Free;
end;

var
  Simple : TDataItem=nil;

procedure CreateSimple;
begin
  Simple:=TDataItem.Create(True);

  Simple.Items.Add('Name',TDataKind.dkText);
  Simple.Items.Add('X',TDataKind.dkInt32);

  Simple.Resize(3);
  Simple[0].TextData:=['A','B','C'];
  Simple[1].Int32Data:=[1,2,3];
end;

procedure TFormViz.Recalculate(Sender: TObject);
begin
  BIVisualizer1.Data.Free;
  BIVisualizer1.Data:=nil;

  if Simple=nil then
     CreateSimple;
  BIVisualizer1.Data:=Simple;


  if CBQuery.Checked then
     BIVisualizer1.Provider:=BIQuery1
  else
  if CBSample.Checked then
     BIVisualizer1.Data:=Summary.Calculate
  else
     BIVisualizer1.Data:=DataManager.SelectedData;

  VizEditor.Refresh(BIVisualizer1);

  if CBSample.Checked then
     VizEditor.CheckDuplicates(Summary);
end;

procedure TFormViz.SpeedButton1Click(Sender: TObject);
begin
  Panel2.Visible:=not Panel2.Visible;
end;

procedure TFormViz.AddSummaryEditor(const AParent:TWinControl);
begin
  SummaryEditor:=TSummaryEditor.Create(Self);
  SummaryEditor.OnRecalculate:=Recalculate;

  SummaryEditor.Align:=alClient;

  TUICommon.AddForm(SummaryEditor,AParent);

  SummaryEditor.Refresh(Summary);
end;

procedure TFormViz.BDataClick(Sender: TObject);
begin
  TDataViewer.View(Self,BIVisualizer1.Data);
end;

procedure TFormViz.BRefreshClick(Sender: TObject);
begin
  Recalculate(Self);
end;

procedure TFormViz.Button1Click(Sender: TObject);
begin
  TBIControlTree.Edit(Self,BIVisualizer1)
end;

procedure TFormViz.Button2Click(Sender: TObject);
begin
  TDataManager.Edit(Self);
end;

procedure TFormViz.Button3Click(Sender: TObject);
begin
  TVisualizerEditor.Edit(Self,BIVisualizer1);
end;

procedure TFormViz.SelectedData(Sender: TObject);
begin
  BRefresh.Enabled:=DataManager.SelectedData<>nil;
end;

procedure TFormViz.CBSampleClick(Sender: TObject);
begin
  PanelSummary.Visible:=CBSample.Checked;
  BRefresh.Visible:=not CBSample.Checked;

  if CBSample.Checked then
  begin
    if DataManager<>nil then
       DataManager.Hide;
  end
  else
  begin
    if DataManager=nil then
    begin
      DataManager:=TDataManager.Embed(Self,TabData,Choose,'BISamples');
      DataManager.PanelStores.Visible:=True;
      DataManager.Align:=TAlign.alClient;
      DataManager.OnSelect:=SelectedData;
    end;

    DataManager.Visible:=True;

    SelectedData(Self);
  end;
end;

procedure TFormViz.CBQueryClick(Sender: TObject);
begin
  Recalculate(Self);
end;

procedure TFormViz.CreateSummary;
var Orders,
    Shippers,
    Categories,
    OrderDetails : TDataItem;
begin
  Data:=TStore.Load('BISamples','SQLite_Demo');

  OrderDetails:=Data['"Order Details"'];
  Orders:=Data['Orders'];
  Shippers:=Data['Shippers'];
  Categories:=Data['Categories'];

  Summary:=TSummary.Create(Self);

  Summary.Description:='Sum or Order Details Quantity';

  Summary.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

  Summary.AddGroupBy(Orders['OrderDate']).Layout:=TGroupByLayout.Rows;
  Summary.By[0].DatePart:=TDateTimePart.Year;

  Summary.AddGroupBy(Shippers['CompanyName']).Layout:=TGroupByLayout.Rows;

  Summary.AddGroupBy(Orders['OrderDate']).Layout:=TGroupByLayout.Items;
  Summary.By[2].DatePart:=TDateTimePart.Quarter;

  Summary.AddGroupBy(Categories['CategoryName']).Layout:=TGroupByLayout.Rows;
end;

procedure TFormViz.FormCreate(Sender: TObject);
begin
  //DoubleBuffered:=True;

  PageControl1.ActivePage:=TabSettings;

  CreateSummary;
  AddSummaryEditor(PanelSummary);

  VizEditor:=TVisualizerEditor.Embedd(Self,TabSettings);

  Recalculate(Self);
end;
initialization
ReportMemoryLeaksOnShutdown := True;
finalization
CheckSynchronize;
end.
