unit Unit15;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Data, BI.VCL.Grid, BI.Summary, Data.DB, Vcl.CheckLst, System.Types,
  BI.Expression, BI.VCL.LinkDiagram, BI.VCL.Chart,
  BI.VCL.Editor.Summary, BI.VCL.Editor.Grid, Vcl.Grids,
  BI.Data.ClientDataset;

type
  TForm15 = class(TForm)
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
    Splitter2: TSplitter;
    TabTree: TTabSheet;
    TreeView1: TTreeView;
    Button3: TButton;
    TreeView2: TTreeView;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Label3: TLabel;
    EQuarter: TEdit;
    CBChart: TCheckBox;
    CBStore: TComboBox;
    Button4: TButton;
    G: TBIGrid;
    Button5: TButton;
    Button6: TButton;
    SplitterChart: TSplitter;
    Button7: TButton;
    Panel3: TPanel;
    Label1: TLabel;
    EFilter: TEdit;
    CBFilter: TCheckBox;
    LFilterError: TLabel;
    procedure LBTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBHideDuplicatesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure CBStoreChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure EQuarterChange(Sender: TObject);
    procedure CBChartClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CBFilterClick(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
  private
    { Private declarations }
    //Totals : TBIGrid;

    Chart : TBIChart;

    TotalData,
    Data : TDataItem;

    S : TSummary;
    SummaryEditor : TSummaryEditor;

    procedure AddStores;
    procedure BoldTotals;
    procedure ClearTrees;
    procedure HideDuplicates;
    procedure Recalculate(Sender:TObject);
    procedure RefreshTrees;
    //procedure ShowTotals(const ByCol:TDataItem; const Items:TAggregates);
    procedure TryFillChart;
  public
    { Public declarations }
  end;

var
  Form15: TForm15;

implementation

{$R *.dfm}

uses
  BI.VCL.DataViewer, System.Diagnostics, VCL.Themes,
  BI.VCL.DataTree, Sample_Summaries, BI.Persist, BI.VCL.Editor.Stores,
  BI.VCL.DataManager, BI.VCL.Grid.DBGrid,

  BI.Data.JSON, BI.Data.XML, BI.Data.CSV, BI.Data.Excel,
  BI.Languages.English;

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

procedure TForm15.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabGrid;

  EQuarter.Text:=TDateTimePart.QuarterFormat;

  AddStores;
  CBStoreChange(Self);

  AddStyles(CBStyle.Items);
  CBStyle.ItemIndex:=0;
end;

procedure TForm15.FormDestroy(Sender: TObject);
begin
  S.Free;
  Data.Free;
  TotalData.Free;
end;

procedure TForm15.Button1Click(Sender: TObject);
begin
  TDataViewer.View(Self,Data);
end;

procedure TForm15.Button2Click(Sender: TObject);
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

procedure TForm15.Button3Click(Sender: TObject);
begin
  TDataDiagram.Show(Self,Samples.Demo);
end;

procedure TForm15.Button4Click(Sender: TObject);
begin
  TStoreEditor.Edit(Self);
end;

procedure TForm15.Button5Click(Sender: TObject);
begin
  TDataManager.Edit(Self);
end;

procedure TForm15.Button6Click(Sender: TObject);
begin
  TBIGridEditor.Edit(Self,G.Plugin.GetObject as TBIDBGrid);
end;

procedure TForm15.Button7Click(Sender: TObject);
begin
  TDataViewer.View(Self,Samples.Demo);
end;

procedure TForm15.CBChartClick(Sender: TObject);
begin
  if Chart=nil then
  begin
    Chart:=TBIChart.Create(Self);
    Chart.Align:=alRight;
    Chart.Width:=(Width-PanelList.Width) div 2;
    Chart.Parent:=Self;
  end;

  Chart.Visible:=CBChart.Checked;
  SplitterChart.Visible:=Chart.Visible;

  TryFillChart;
end;

procedure TForm15.CBFilterClick(Sender: TObject);
var tmpOk : Boolean;
    tmpFilter : TExpression;
begin
  LFilterError.Caption:='';

  if S<>nil then
  begin
    if CBFilter.Checked then
    begin
      if EFilter.Text='' then
      begin
        S.Filter:=nil;
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
          S.Filter:=tmpFilter;
          Recalculate(Self);
        end;
      end;
    end
    else
    if S.Filter<>nil then
    begin
      S.Filter:=nil;
      Recalculate(Self);
    end;
  end;
end;

procedure TForm15.CBHideDuplicatesClick(Sender: TObject);
begin
  HideDuplicates;
end;

procedure TForm15.CBStoreChange(Sender: TObject);
begin
  Samples.LoadData(CBStore.Items[CBStore.ItemIndex]);
end;

procedure TForm15.CBStyleChange(Sender: TObject);
begin
  if CBStyle.ItemIndex=0 then
     TStyleManager.SetStyle(TStyleManager.SystemStyle)
  else
     TStyleManager.TrySetStyle(CBStyle.Items[CBStyle.ItemIndex]);
end;

procedure TForm15.EFilterChange(Sender: TObject);
begin
  CBFilterClick(Self);
end;

procedure TForm15.EQuarterChange(Sender: TObject);
begin
  TDateTimePart.QuarterFormat:=EQuarter.Text;
end;

procedure TForm15.ClearTrees;
begin
  TreeView1.Items.Clear;
  TreeView2.Items.Clear;
end;

procedure TForm15.LBTestClick(Sender: TObject);
begin
  ClearTrees;
  PageControl1.ActivePage:=TabGrid;

  S.Free;
  S:=Samples.CreateSummary(LBTest.ItemIndex);
  S.Description:=LBTest.Items[LBTest.ItemIndex];

  Recalculate(Self);

  if SummaryEditor=nil then
  begin
    SummaryEditor:=TSummaryEditor.Create(Self);
    SummaryEditor.OnRecalculate:=Recalculate;
    TUICommon.AddForm(SummaryEditor,PanelList);
  end;

  SummaryEditor.Refresh(S);
end;

procedure TForm15.BoldTotals;
var t : Integer;
begin
  for t:=0 to High(S.By) do
      if S.By[t].Totals<>[] then
{         G.Traverse(G.ColumnOf(S.By[t].TotalsColumn),
             procedure(AColumn:TColumn)
             begin
               AColumn.Font.Style:=[fsBold];
               AColumn.Title.Font.Style:=[fsBold];
             end);}
end;

procedure TForm15.Recalculate(Sender:TObject);
begin
  ClearTrees;

  Data.Free;
  Data:=nil;

  Data:=S.Calculate;

  G.BindTo(Data);

  HideDuplicates;

  BoldTotals;

  TryFillChart;
end;

procedure TForm15.TryFillChart;
begin
  if CBChart.Checked then
  begin
    Chart.ClearChart;

    if Data<>nil then
       Chart.Fill(Data.Items);
  end;
end;

procedure TForm15.HideDuplicates;
var t : Integer;
begin
  if S<>nil then
     for t:=0 to High(S.By) do
         if S.By[t].Dest<>nil then
            G.Duplicates(S.By[t].Dest, CBHideDuplicates.Checked);
end;

procedure TForm15.AddStores;
begin
  TStores.AllTo(CBStore.Items);

  CBStore.ItemIndex:=CBStore.Items.IndexOf(TStore.DefaultName);
end;

procedure TForm15.RefreshTrees;
begin
  if TreeView1.Items.Count=0 then
     TDataTree.Fill(Data,True,TreeView1);

  if TreeView2.Items.Count=0 then
     TDataTree.Fill(Data,False,TreeView2);
end;

(*
procedure TForm15.ShowTotals(const ByCol: TDataItem;
  const Items: TAggregates);
begin
  if Totals=nil then
  begin
    Totals:=TBIGrid.Create(Self);
    Totals.Align:=alBottom;
    Totals.Parent:=TabGrid;

    Splitter2.Visible:=True;
  end;

  TotalData.Free;
//  TotalData:=SummaryEditor.CalculateTotals(SummaryEditor.;

  Totals.BindTo(TotalData);
end;
*)

procedure TForm15.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabTree then
     if S<>nil then
        RefreshTrees;
end;

end.
