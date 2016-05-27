unit Unit_BI_Datasets;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, BI.FMX.DataManager, BI.FMX.Grid, BI.Persist, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, BI.Data,
  FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, BI.Arrays,

  BI.Algorithm.Model, BI.Algorithm, FMX.ScrollBox, FMX.Memo,
  BI.FMX.R.Console,

  // Two different R "engines" (cmd batch and native)
  BI.Plugins.R.Command, BI.Plugins.R.opaR, Data.DB, BI.Dataset;

type
  TRDatasetsDemo = class(TForm)
    Layout1: TLayout;
    TabControl1: TTabControl;
    TabDatas: TTabItem;
    LDatasets: TListView;
    TabControl2: TTabControl;
    TabItem1: TTabItem;
    BIGrid1: TBIGrid;
    Layout2: TLayout;
    Layout3: TLayout;
    CBTarget: TComboBox;
    Label1: TLabel;
    LAttributes: TListBox;
    LModel: TListBox;
    BFit: TButton;
    TabControl3: TTabControl;
    TabItem2: TTabItem;
    PredictedGrid: TBIGrid;
    TabItem3: TTabItem;
    ConfusionGrid: TBIGrid;
    Layout4: TLayout;
    Label2: TLabel;
    LScore: TLabel;
    Button1: TButton;
    TabLog: TTabItem;
    LTime: TLabel;
    TabItem4: TTabItem;
    TrainGrid: TBIGrid;
    BIDataset1: TBIDataset;
    procedure FormCreate(Sender: TObject);
    procedure LDatasetsChange(Sender: TObject);
    procedure CBTargetChange(Sender: TObject);
    procedure LAttributesChangeCheck(Sender: TObject);
    procedure BFitClick(Sender: TObject);
    procedure LModelChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    Data : TDataItem;

    RConsole : TBIRConsole;

    procedure CheckEngine;

    function CurrentData:TDataItem;
    procedure EnableFit;
    procedure FillDatas;
    procedure FillModels;

    function SelectedAttributes:TDataArray;
    function SelectedTarget:TDataItem;

    procedure SelectDefaultAlgorithm;
    procedure SelectDefaultDatas;
    function SelectedModelClass:TSupervisedModelClass;

    procedure ShowResults(const AModel:TSupervisedModel);
    procedure ShowTrainData(const AModel:TSupervisedModel);
  public
    { Public declarations }
  end;

var
  RDatasetsDemo: TRDatasetsDemo;

implementation

{$R *.fmx}

uses
  System.Diagnostics,
  BI.Algorithm.Classify, BI.Plugins.Python, BI.Plugins.R;

function TRDatasetsDemo.SelectedAttributes:TDataArray;
var t : Integer;
begin
  result:=nil;

  for t:=0 to LAttributes.Count-1 do
      if LAttributes.ListItems[t].IsChecked then
         result.Add(TDataItem(LAttributes.Items.Objects[t]));
end;

function TRDatasetsDemo.SelectedTarget:TDataItem;
begin
  if CBTarget.ItemIndex=-1 then
     result:=nil
  else
     result:=TDataItem(CBTarget.Items.Objects[CBTarget.ItemIndex])
end;

function TRDatasetsDemo.SelectedModelClass:TSupervisedModelClass;
begin
  result:=TSupervisedModelClass(LModel.Items.Objects[LModel.ItemIndex]);
end;

type
  TBIDataSetAccess=class(TBIDataset);

procedure TRDatasetsDemo.ShowTrainData(const AModel:TSupervisedModel);
begin
  BIDataSet1.Data:=AModel.Attributes[0].Parent;
  BIDataSet1.Cursor.SetItems(AModel.Attributes);
  BIDataSet1.Cursor.Add(AModel.Target);

  // Pending, right now Index is protected
  TBIDataSetAccess(BIDataSet1).Index:=AModel.Indices.A;

  TrainGrid.DataSet:=BIDataset1;
end;

procedure TRDatasetsDemo.ShowResults(const AModel:TSupervisedModel);
begin
  PredictedGrid.BindTo(AModel.Predicted);
  ConfusionGrid.BindTo(AModel.Predicted.Confusion);

  if AModel.Predicted.Count=0 then
     LScore.Text:='?%'
  else
     LScore.Text:=FormatFloat('0.##%',100*AModel.Predicted.Correct/AModel.Predicted.Count);

  ShowTrainData(AModel);
end;

procedure TRDatasetsDemo.CheckEngine;
begin
  if TBIREngine.Engine=nil then
  begin
   //  TBIREngine.Engine:=TRCommand.Create; <-- obsolete
    TBIREngine.Engine:=TopaR.Create;
  end;
end;

procedure TRDatasetsDemo.BFitClick(Sender: TObject);
var Model : TSupervisedModel;
    t1 : TStopWatch;
begin
  CheckEngine;

  Model:=SelectedModelClass.Create;  //  (CurrentData)
  try
    Model.Target:=SelectedTarget;
    Model.Attributes:=SelectedAttributes;

    Model.Split(2/3);

    t1:=TStopwatch.StartNew;

    Model.Calculate;

    LTime.Text:=t1.ElapsedMilliseconds.ToString+' msec';

    ShowResults(Model);
  finally
    Model.Free;
  end;
end;

procedure TRDatasetsDemo.Button1Click(Sender: TObject);
begin
  TDataManager.Edit(Self);
end;

procedure TRDatasetsDemo.CBTargetChange(Sender: TObject);
var tmp : Integer;
begin
  if CBTarget.Selected<>nil then
  begin
    tmp:=LAttributes.Items.IndexOf(CBTarget.Selected.Text);

    if tmp<>-1 then
       LAttributes.ListItems[tmp].IsChecked:=False;
  end;

  EnableFit;
end;

procedure TRDatasetsDemo.EnableFit;

  function CheckedAttributes:Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to LAttributes.Count-1 do
        if LAttributes.ListItems[t].IsChecked then
           Inc(result);
  end;

begin
  BFit.Enabled:=(CBTarget.ItemIndex<>-1) and (CheckedAttributes>0) and
                (LModel.ItemIndex<>-1);
end;

procedure TRDatasetsDemo.LAttributesChangeCheck(Sender: TObject);
begin
  EnableFit;
end;

procedure TRDatasetsDemo.FillDatas;
var Item : TDataItem;
    tmp : TListViewItem;
begin
  LDatasets.BeginUpdate;
  try
    LDatasets.Items.Clear;

    for Item in Data.Items.AsArray do
    begin
      tmp:=LDatasets.Items.Add;
      tmp.Text:=Item.Name;
      tmp.Detail:=IntToStr(Item.Count);
    end;
  finally
    LDatasets.EndUpdate;
  end;
end;

procedure TRDatasetsDemo.FillModels;
begin
  TAllModels.AllModels(LModel.Items);
end;

procedure TRDatasetsDemo.SelectDefaultDatas;
var index: Integer;
begin
  index:=Data.Items.AsArray.IndexOf('cane');

  if index <> -1 then
  begin
    LDatasets.ItemIndex:=index;
    LDatasetsChange(Self);

    CBTarget.ItemIndex:=5;
  end;

  LAttributes.ListItems[1].IsChecked:=True;
  LAttributes.ListItems[2].IsChecked:=True;
  LAttributes.ListItems[3].IsChecked:=True;
  LAttributes.ListItems[4].IsChecked:=True;
end;

procedure TRDatasetsDemo.SelectDefaultAlgorithm;
begin
  LModel.ItemIndex:=LModel.Items.IndexOfObject(TObject(TBINearestNeighbour)); // kNN
end;

procedure TRDatasetsDemo.FormCreate(Sender: TObject);
begin
  Data:=TStore.Load('BISamples','R Datasets');

  FillDatas;
  FillModels;

  // Initial default setup
  SelectDefaultAlgorithm;
  SelectDefaultDatas;

  EnableFit;
end;

procedure TRDatasetsDemo.FormShow(Sender: TObject);
begin
  RConsole:=TBIRConsole.Create(Self);
  TUICommon.AddForm(RConsole,TabLog);

  TBIREngine.Engine.Output:=RConsole.Memo.Lines;
  TBIPython.Output:=RConsole.Memo.Lines;
end;

function TRDatasetsDemo.CurrentData:TDataItem;
begin
  if LDatasets.ItemIndex=-1 then
     result:=nil
  else
     result:=Data[(LDatasets.Selected as TListViewItem).Text];
end;

procedure TRDatasetsDemo.LDatasetsChange(Sender: TObject);
var tmp : Integer;
    Item,
    tmpData : TDataItem;
begin
  CBTarget.Clear;
  LAttributes.Clear;

  tmp:=LDatasets.ItemIndex;

  if tmp=-1 then
  begin
    BIGrid1.BindTo(nil);
    PredictedGrid.BindTo(nil);
  end
  else
  begin
    tmpData:=CurrentData;

    BIGrid1.BindTo(tmpData);

    CBTarget.BeginUpdate;
    LAttributes.BeginUpdate;
    try
      tmpData.Stats;

      for Item in tmpData.Items.AsArray do
      begin
        Item.Stats;

        // Skip primary or unique-sorted items
        if Item.Primary or (Item.Unique and (Item.DataMap.Sorted=TDataOrder.Ascending)) then
        else
        begin
          CBTarget.Items.AddObject(Item.Name,Item);
          LAttributes.Items.AddObject(Item.Name,Item);
        end;
      end;
    finally
      CBTarget.EndUpdate;
      LAttributes.EndUpdate;
    end;
  end;
end;

procedure TRDatasetsDemo.LModelChange(Sender: TObject);
begin
  EnableFit;
end;

end.
