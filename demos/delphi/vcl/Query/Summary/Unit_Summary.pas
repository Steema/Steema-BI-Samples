{*********************************************}
{  TeeBI Software Library                     }
{  Summary Examples                           }
{  Copyright (c) 2015-2018 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_Summary;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.DataItem, BI.Summary, Data.DB, Vcl.CheckLst, System.Types,
  BI.Expression,

  // Detect TeeChart "Pro" or "Lite
  VCLTee.TeeConst, VCLTee.TeeProcs,

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  VCLBI.Chart,
  VCLBI.Editor.Summary, VCLBI.Editor.BIGrid, Vcl.Grids,
  BI.ClientDataset, System.UITypes,
  VCLBI.Visualizer, VCLBI.Visualizer.Chart, VCLBI.Editor.Visualizer.Chart,
  VCLBI.Editor.Visualizer, Vcl.Menus, BI.Summary.Persist,
  VCLBI.Editor.ControlTree, BI.SQL, VCLBI.GridForm,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Chart, VCLTee.TeeTools,
  VCLBI.Chart.Plugin, VCLBI.DataControl,
  VCLBI.Grid;

type
  TFormSummary = class(TForm)
    Panel1: TPanel;
    LBTest: TListBox;
    PageControl1: TPageControl;
    TabGrid: TTabSheet;
    Splitter1: TSplitter;
    PanelList: TPanel;
    CBHideDuplicates: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    CBStyle: TComboBox;
    TabTree: TTabSheet;
    TreeView1: TTreeView;
    TreeView2: TTreeView;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Label3: TLabel;
    EQuarter: TEdit;
    CBChart: TCheckBox;
    CBStore: TComboBox;
    Button4: TButton;
    BIGrid1: TBIGrid;
    Button5: TButton;
    Button6: TButton;
    SplitterChart: TSplitter;
    Button7: TButton;
    Panel3: TPanel;
    Label1: TLabel;
    EFilter: TEdit;
    CBFilter: TCheckBox;
    LFilterError: TLabel;
    CBColorize: TComboBox;
    Label2: TLabel;
    CBAutoTextColor: TCheckBox;
    CBFillColor: TComboBox;
    TabVisualizer: TTabSheet;
    PageControl2: TPageControl;
    TabSummary: TTabSheet;
    TabVizEditor: TTabSheet;
    PopupMenu1: TPopupMenu;
    Savesummaries1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Panel4: TPanel;
    Button8: TButton;
    Viz: TBIComposer;
    TabSQL: TTabSheet;
    Panel5: TPanel;
    MemoSQL: TMemo;
    BExecuteSQL: TButton;
    BIGridSQL: TBIGrid;
    TabTotals: TTabSheet;
    BIGridTotals: TBIGrid;
    LSQLError: TLabel;
    Button10: TButton;
    CBAlternate: TCheckBox;
    CBMultiCPU: TCheckBox;
    CBAutoExecute: TCheckBox;
    BEditChart: TButton;
    BITChart1: TBITChart;
    BIChart1: TBIChart;
    Splitter2: TSplitter;
    Button3: TButton;
    procedure LBTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBHideDuplicatesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure CBStoreChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure EQuarterChange(Sender: TObject);
    procedure CBChartClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CBFilterClick(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
    procedure CBColorizeChange(Sender: TObject);
    procedure CBAutoTextColorClick(Sender: TObject);
    procedure CBFillColorChange(Sender: TObject);
    procedure Savesummaries1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure BExecuteSQLClick(Sender: TObject);
    procedure MemoSQLChange(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure CBAlternateClick(Sender: TObject);
    procedure BEditChartClick(Sender: TObject);
    procedure BIGridTotalsDataChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }

    Chart : TBIChart;

    TotalData,
    Data : TDataItem;

    Summary : TSummary;
    SummaryEditor : TSummaryEditor;

    VizEditor : TVisualizerEditor;

    procedure AddStores;
    procedure ClearTrees;
    procedure ColorizeCells;
    function CreateSummary(const AIndex:Integer; const AName:String):TSummary;
    procedure GridClick(Sender: TObject);
    procedure HideDuplicates;
    procedure Recalculate(Sender:TObject);
    procedure RefreshTrees;
    procedure ShowTotals;
    procedure TotalsHideDuplicates;
    procedure TryFillChart;
  public
    { Public declarations }
  end;

var
  FormSummary: TFormSummary;

implementation

{$R *.dfm}

uses
  VCLBI.DataViewer, System.Diagnostics, VCL.Themes,
  VCLBI.DataTree, BI.Tests.SummarySamples, BI.Persist, VCLBI.Editor.Stores,
  VCLBI.DataManager, VCLBI.Grid.DBGrid, VCLBI.Editor.Chart,

  {$IF CompilerVersion>27} // RAD XE7 and up
  System.Threading,
  {$ENDIF}

  VCLBI.Editor.Hops,

  BI.JSON, BI.XMLData, BI.CSV, BI.Excel, BI.Grid.Plugin,
  BI.Languages.English, BI.UI, BI.Summary.Totals, BI.Expressions;

procedure AddStyles(const Items:TStrings);
var s : String;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    Items.Add('Windows');

    for s in TStyleManager.StyleNames do
        Items.Add(s);
  finally
    Items.EndUpdate;
  end;
end;

procedure TFormSummary.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabGrid;

  EQuarter.Text:=TDateTimePart.QuarterFormat;

  AddStores;
  CBStoreChange(Self);

  AddStyles(CBStyle.Items);
  CBStyle.ItemIndex:=0;

  (BIGrid1.Plugin.GetObject as TBIDBGrid).OnDblClick:=GridClick;

  TSampleSummaries.AddExamples(LBTest.Items);
end;

procedure TFormSummary.GridClick(Sender: TObject);
begin
  Caption:=(BIGrid1.Plugin.GetObject as TBIDBGrid).TopRow.ToString;
end;

procedure TFormSummary.FormDestroy(Sender: TObject);
begin
  Summary.Free;

  Data.Free;
  TotalData.Free;

  BIGridSQL.Data.Free;
  BIGridTotals.Data.Free;
end;

type
  TTest=class(TDataItem)
  private
    FSummary : TFormSummary;
    procedure DoBenchmark(const AIndex:Integer);
  public
  const
    NumIters=1000;

    Constructor Create(const ASummary:TFormSummary);

    procedure CalcElapsed(const AElapsed:Int64; const AIndex:Integer);
    procedure Run(const MultiCPU:Boolean);
  end;

Constructor TTest.Create(const ASummary:TFormSummary);
var t : Integer;
begin
  inherited Create(True);

  FSummary:=ASummary;

  Items.Add('Test',TDataKind.dkText);
  Items.Add('Iterations',TDataKind.dkInt32);
  Items.Add('Elapsed',TDataKind.dkInt64);
  Items.Add('Per Second',TDataKind.dkInt32);

  Resize(FSummary.LBTest.Count);

  for t:=0 to Count-1 do
  begin
    Items[0].TextData[t]:=FSummary.LBTest.Items[t];
    Items[1].Int32Data[t]:=NumIters;
  end;
end;

procedure TTest.CalcElapsed(const AElapsed:Int64; const AIndex:Integer);
var PerSecond : Integer;
begin
  Items[2].Int64Data[AIndex]:=AElapsed;

  if AElapsed=0 then
     PerSecond:=0
  else
     PerSecond:=Round(1000*NumIters/AElapsed);

  Items[3].Int32Data[AIndex]:=PerSecond;
end;

procedure TTest.DoBenchmark(const AIndex:Integer);
var tmp : TSummary;
    t : Integer;
begin
  tmp:=FSummary.CreateSummary(AIndex,Items[0].TextData[AIndex]);
  try
    for t:=1 to NumIters do
        tmp.Calculate.Free;
  finally
    tmp.Free;
  end;
end;

procedure TTest.Run(const MultiCPU:Boolean);
var t : Integer;
    t1 : TStopwatch;
begin

  {$IF CompilerVersion>27} // RAD XE7 and up
  if MultiCPU then
  begin
    t1:=TStopwatch.StartNew;

    TParallel.&For(0,Count-1,procedure(Index:Integer)
    begin
      DoBenchmark(Index);
    end);

    // TStopWatch cannot be used inside threads (misleading results)
    Name:='Total: '+t1.ElapsedMilliseconds.ToString+' msec';
  end
  else
  {$ENDIF}

  begin
    for t:=0 to Count-1 do
    begin
      t1:=TStopwatch.StartNew;
      DoBenchmark(t);
      CalcElapsed(t1.ElapsedMilliseconds,t);
    end;

    Name:='Total: '+Item[2].Int64Data.Sum.ToString+' msec';
  end;
end;

procedure TFormSummary.Button10Click(Sender: TObject);
var Test : TTest;
begin
  Test:=TTest.Create(Self);
  try
    Test.Run(CBMultiCPU.Checked);
    TBIGridForm.Present(Self,Test);
  finally
    Test.Free;
  end;
end;

procedure TFormSummary.Button1Click(Sender: TObject);
begin
  TDataViewer.View(Self,Data);
end;

procedure TFormSummary.Button2Click(Sender: TObject);
var t : Integer;
    t1 : TStopwatch;
begin
  t1:=TStopwatch.StartNew;

  for t:=0 to LBTest.Count-1 do
  begin
    LBTest.ItemIndex:=t;
    LBTestClick(Self);
  end;

  Caption:='Time: '+IntToStr(t1.ElapsedMilliseconds)+' msec';
end;

procedure TFormSummary.Button3Click(Sender: TObject);
var tmp : TDataHops;
begin
  tmp:=THopsViewer.HopsFrom(Summary);
  THopsViewer.View(Self,tmp);
end;

procedure TFormSummary.BEditChartClick(Sender: TObject);
begin
  TBIChartEditor.Edit(Self, Chart);
end;

procedure TFormSummary.Button4Click(Sender: TObject);
begin
  TStoreEditor.Edit(Self);
end;

procedure TFormSummary.Button5Click(Sender: TObject);
begin
  TDataManager.Edit(Self);
end;

procedure TFormSummary.Button6Click(Sender: TObject);
begin
  TBIGridEditor.Edit(Self,BIGrid1);
end;

procedure TFormSummary.Button7Click(Sender: TObject);
begin
  TDataViewer.View(Self,Samples.Demo);
end;

procedure TFormSummary.Button8Click(Sender: TObject);
begin
  TBIControlTree.Edit(Self,Viz);
end;

procedure TFormSummary.BExecuteSQLClick(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TBISQL.From(Samples.Demo,MemoSQL.Text);

  if tmp<>nil then
  begin
    BIGridSQL.Data.Free;
    BIGridSQL.Data:=tmp;
  end;
end;

type
  TBIGridAccess=class(TBIGrid);

procedure TFormSummary.BIGridTotalsDataChange(Sender: TObject);
begin
  TotalsHideDuplicates;
  BIChart1.Data:=TBIGridAccess(BIGridTotals).SubItem;
end;

procedure TFormSummary.CBAutoTextColorClick(Sender: TObject);
begin
  Recalculate(Self);
end;

procedure TFormSummary.CBChartClick(Sender: TObject);
begin
  if Chart=nil then
  begin
    Chart:=TBIChart.Create(Self);
    Chart.Align:=alBottom;
    Chart.Height:=TabGrid.Height div 2;
    Chart.Parent:=TabGrid;

    BEditChart.Enabled:=True;
  end;

  Chart.Visible:=CBChart.Checked;
  SplitterChart.Visible:=Chart.Visible;

  TryFillChart;
end;

procedure TFormSummary.CBColorizeChange(Sender: TObject);
begin
  Recalculate(Self);
end;

procedure TFormSummary.CBFillColorChange(Sender: TObject);
begin
  Recalculate(Self);
end;

procedure TFormSummary.CBFilterClick(Sender: TObject);
var tmpOk : Boolean;
    tmpFilter : TExpression;
begin
  LFilterError.Caption:='';

  if Summary<>nil then
  begin
    if CBFilter.Checked then
    begin
      if EFilter.Text='' then
      begin
        Summary.Filter:=nil;
        Recalculate(Self);
      end
      else
      begin
        tmpOk:=True;

        // Problem: in simplified form ("Customers.CompanyName"), we should pass
        // a child item of "Demo" sqlite_demo data. This is not good.
        tmpFilter:=TDataFilter.FromString(Samples.Customers,EFilter.Text,
            function(const APos:Integer; const AMessage:String):Boolean
            begin
              tmpOk:=False;
              LFilterError.Caption:=Format(BIMsg_ExpressionError,[AMessage,APos]);
              result:=True;
            end);

        if tmpOk then
        begin
          Summary.Filter:=tmpFilter;
          Recalculate(Self);
        end;
      end;
    end
    else
    if Summary.Filter<>nil then
    begin
      Summary.Filter:=nil;
      Recalculate(Self);
    end;
  end;
end;

procedure TFormSummary.CBHideDuplicatesClick(Sender: TObject);
begin
  HideDuplicates;
end;

procedure TFormSummary.CBStoreChange(Sender: TObject);
begin
  Samples.LoadData(CBStore.Items[CBStore.ItemIndex]);
end;

procedure TFormSummary.CBStyleChange(Sender: TObject);
begin
  if CBStyle.ItemIndex=0 then
     TStyleManager.SetStyle(TStyleManager.SystemStyle)
  else
     TStyleManager.TrySetStyle(CBStyle.Items[CBStyle.ItemIndex]);
end;

procedure TFormSummary.CBAlternateClick(Sender: TObject);
begin
  BIGrid1.Alternate.Enabled:=CBAlternate.Checked;
end;

procedure TFormSummary.EFilterChange(Sender: TObject);
begin
  CBFilterClick(Self);
end;

procedure TFormSummary.EQuarterChange(Sender: TObject);
begin
  TDateTimePart.QuarterFormat:=EQuarter.Text;
end;

procedure TFormSummary.ClearTrees;
begin
  TreeView1.Items.Clear;
  TreeView2.Items.Clear;
end;

function TFormSummary.CreateSummary(const AIndex:Integer; const AName:String):TSummary;
begin
  result:=Samples.CreateSummary(Self,AIndex);
  result.Description:=AName;
end;

procedure TFormSummary.LBTestClick(Sender: TObject);
begin
  ClearTrees;

//  PageControl1.ActivePage:=TabGrid;

  Summary.Free;
  Summary:=CreateSummary(LBTest.ItemIndex,LBTest.Items[LBTest.ItemIndex]);

  Recalculate(Self);

  if SummaryEditor=nil then
  begin
    SummaryEditor:=TSummaryEditor.Create(Self);
    SummaryEditor.OnRecalculate:=Recalculate;
    TUICommon.AddForm(SummaryEditor,TabSummary);
  end;

  SummaryEditor.Refresh(Summary);

  if VizEditor=nil then
  begin
    VizEditor:=TVisualizerEditor.Create(Self);
    TUICommon.AddForm(VizEditor,TabVizEditor);
  end;

  VizEditor.Refresh(Viz);

  if PageControl1.ActivePage=TabSQL then
     MemoSQL.Text:=TBISQL.From(Summary);
end;

procedure TFormSummary.MemoSQLChange(Sender: TObject);
var S : TSQLParser;
begin
  LSQLError.Caption:='';

  BIGridSQL.Data:=nil;

  S:=TSQLParser.Create(Samples.Demo,MemoSQL.Text);
  try
    S.Parse(function(const Sender:TObject; const Error:String):Boolean
       begin
         LSQLError.Caption:=Error;
         result:=True;
       end);
  finally
    S.Free;
  end;

  if CBAutoExecute.Checked and (LSQLError.Caption='') then
     BExecuteSQLClick(Self);
end;

type
  TDataAccess=class(TDataItem);

procedure TFormSummary.Recalculate(Sender:TObject);
begin
  if Summary=nil then
     Exit;

  ClearTrees;

  Data.Free;

  Data:=TDataItem.Create(Summary);

  // Pending: Special issue in this demo, Summary should be preserved
  // when destroying Data
  TDataAccess(Data).KeepProvider:=True;

  BIGrid1.BindTo(Data);

  HideDuplicates;

  ColorizeCells;

  TryFillChart;

  Viz.Data:=Data;

  if PageControl1.ActivePage=TabTotals then
     ShowTotals;
end;

procedure TFormSummary.ColorizeCells;
var t : Integer;
    Colorizers : TDataColorizers;
begin
  if CBColorize.ItemIndex>0 then
  begin
    SetLength(Colorizers,Summary.Measures.Count);

    for t:=0 to Summary.Measures.Count-1 do
    begin
      Colorizers[t].Data:=Summary.Measures[t].DestData;

      Colorizers[t].Colors.Inverted:=(CBColorize.ItemIndex=2);

      if CBAutoTextColor.Checked then
         Colorizers[t].TextColor:=TColorizeTextColor.Automatic
      else
         Colorizers[t].TextColor:=TColorizeTextColor.Fixed;

      if CBFillColor.ItemIndex=0 then
         Colorizers[t].Mode:=TColorizeMode.Full
      else
         Colorizers[t].Mode:=TColorizeMode.Left;

      if t>0 then
      begin
        Colorizers[t].Colors.Clear;
        Colorizers[t].Colors.Add(0,TAlphaColors.White);
        Colorizers[t].Colors.Add(1,TAlphaColors.Darkblue);
      end;
    end;

    BIGrid1.Colorize(Colorizers);
  end
  else
    BIGrid1.Colorize(nil);
end;

procedure TFormSummary.TryFillChart;
begin
  if CBChart.Checked then
  begin
    Chart.Data:=Data;
    {
    Chart.Clear;

    if Data<>nil then
       Chart.Fill(Data.Items);
    }
  end;
end;

procedure HideDuplicatesOf(const AGrid:TBIGrid; const Hide:Boolean);

  procedure DoHideGrid(const AData:TDataItem);
  var t : Integer;
      tmp : TSummary;
  begin
    if (AData<>nil) and (AData.Provider is TSummary) then
    begin
      tmp:=TSummary(AData.Provider);

      for t:=0 to High(tmp.By) do
          if tmp.By[t].DestData<>nil then
             AGrid.Duplicates(tmp.By[t].DestData, Hide);
    end;
  end;

  procedure DoHidePlugin(const AData:TDataItem; const APlugin:TBIGridPlugin);
  var t : Integer;
      tmp : TSummary;
  begin
    if (AData<>nil) and (AData.Provider is TSummary) then
    begin
      tmp:=TSummary(AData.Provider);

      for t:=0 to High(tmp.By) do
          if tmp.By[t].DestData<>nil then
             APlugin.Duplicates(tmp.By[t].DestData, Hide);
    end;
  end;

var tmp : TDataItem;
begin
  tmp:=TBIGridAccess(AGrid).SubItem;

  if tmp=nil then
     DoHideGrid(AGrid.Data)
  else
     DoHidePlugin(tmp,TBIGridAccess(AGrid).SubGrid);
end;

procedure TFormSummary.HideDuplicates;
begin
  HideDuplicatesOf(BIGrid1,CBHideDuplicates.Checked);
  TotalsHideDuplicates;
end;

procedure TFormSummary.AddStores;
begin
  TStores.AllTo(CBStore.Items);

  CBStore.ItemIndex:=CBStore.Items.IndexOf('BISamples');
end;

procedure TFormSummary.RefreshTrees;
begin
  if TreeView1.Items.Count=0 then
     TDataTree.Fill(Data,True,TreeView1);

  if TreeView2.Items.Count=0 then
     TDataTree.Fill(Data,False,TreeView2);
end;

procedure TFormSummary.Savesummaries1Click(Sender: TObject);
var S : TSummaries;
    t : Integer;
begin
  if SaveDialog1.Execute then
  begin
    S:=TSummaries.Create;
    try
      for t:=0 to LBTest.Count-1 do
          S.Add(CreateSummary(t,LBTest.Items[t]));

      S.Save(SaveDialog1.FileName);
    finally
      S.Free;
    end;
  end;
end;

procedure TFormSummary.PageControl1Change(Sender: TObject);
begin
  if Summary<>nil then
  begin
    if PageControl1.ActivePage=TabTree then
       RefreshTrees
    else
    if PageControl1.ActivePage=TabSQL then
       MemoSQL.Text:=TBISQL.From(Summary)
    else
    if PageControl1.ActivePage=TabTotals then
       ShowTotals;
  end;
end;

procedure TFormSummary.TotalsHideDuplicates;
begin
  HideDuplicatesOf(BIGridTotals,CBHideDuplicates.Checked);
end;

procedure TFormSummary.ShowTotals;
begin
  BIGridTotals.Data.Free; // <-- future pending, avoid the need of destroying it

  BIGridTotals.Data:=TDataItem.Create(TSummaryTotals.CreateSummary(Self,Summary));

  TotalsHideDuplicates
end;

end.
