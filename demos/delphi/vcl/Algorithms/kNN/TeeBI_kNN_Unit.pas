unit TeeBI_kNN_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.DataItem, VclTee.TeeGDIPlus,
  VCLTee.TeEngine, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart,
  Vcl.ComCtrls, Vcl.StdCtrls, VCLTee.Series,
  VCLBI.R.Console,
  BI.Algorithm.Model, BI.DataSource, System.Diagnostics, VCLBI.DataViewer,
  VCLBI.Chart.Plugin, VCLBI.Chart,
  VCLBI.DataControl, VCLBI.Grid;

// Dependencies:
// To use the native R <--> Delphi access, the following libraries are required:

(*

Paths:

https://github.com/SigmaSciences/opaR

  opaR\Src
  opaR\Src\Devices

https://bitbucket.org/sglienke/spring4d

  Spring4D\Source\Base
  Spring4D\Source\Base\Collections

https://github.com/malcolmgroves/generics.tuples

  generics.tuples\src

*)

type
  TFormkNN = class(TForm)
    Panel1: TPanel;
    CBModel: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BIGrid1: TBIGrid;
    TabSheet2: TTabSheet;
    BIGrid3: TBIGrid;
    TabSheet3: TTabSheet;
    BIGrid4: TBIGrid;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    BIGrid2: TBIGrid;
    Splitter3: TSplitter;
    BIGrid5: TBIGrid;
    CBNativeR: TCheckBox;
    LTime: TLabel;
    Panel4: TPanel;
    Label2: TLabel;
    LCorrect: TLabel;
    PageControl2: TPageControl;
    TabSheet4: TTabSheet;
    TabConsole: TTabSheet;
    Panel3: TPanel;
    BIChart1: TBIChart;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CBNativeRClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }

    procedure SetupData;
    procedure FitAndPredict;
  public
    { Public declarations }

    RConsole : TBIRConsole;

    Data : TDataItem;
    Model : TSupervisedModel;

    Normalized : TDataItem;
    Attributes : TDataArray;
  end;

var
  FormkNN: TFormkNN;

implementation

{$R *.dfm}

uses
  BI.CSV, System.IOUtils, BI.UI, BI.Algorithm, BI.Tools,
  BI.Algorithm.Classify, BI.Algorithm.Regression, BI.Persist,
  BI.Plugins.R, BI.Plugins.R.opaR,

  {$WARN UNIT_DEPRECATED OFF}  // <-- avoid warning
  BI.Plugins.R.Command,


  BI.Algorithm.Register;

const
  IrisFile='Iris.csv';

function ExistIn(const Dir:String):Boolean;
begin
  result:=TFile.Exists(TPath.Combine(Dir,IrisFile));
end;

function IrisCSV:String;
var Dir : String;
begin
  Dir:=TDirectory.GetCurrentDirectory;

  if ExistIn(Dir) then
     result:=TPath.Combine(Dir,IrisFile)
  else
  begin
    Dir:=TDirectory.GetParent(Dir);

    if ExistIn(Dir) then
       result:=TPath.Combine(Dir,IrisFile)
    else
    begin
      Dir:=TDirectory.GetParent(Dir);

      if ExistIn(Dir) then
         result:=TPath.Combine(Dir,IrisFile)
      else
         raise EBICSV.Create('Error: Iris.csv file cannot be located');
    end
  end;
end;

// Create and visualize a "Cross Table" using current model output
procedure TFormkNN.Button1Click(Sender: TObject);
var Cross : TBICrossTable;
    Output : TDataItem;
begin
  Cross:=TBICrossTable.Create(nil);
  try
    Cross.Real:=Model.Predicted.Items[1];
    Cross.Predicted:=Model.Predicted.Items[2];

    Output:=TDataItem.Create(Cross);
    try
      TDataViewer.View(Self,Output);
    finally
      Output.Free;
    end;

  finally
    Cross.Free;
  end;
end;

// Returns an array of TDataItem, with all AItems from AData
function ArrayOfItems(const AData:TDataItem; const AItems:Array of String):TDataArray;
var s : String;
begin
  result:=nil;

  for s in AItems do
      result.Add(AData[s]);
end;

procedure TFormkNN.SetupData;
begin
  // Load data
  //Data:=TStore.Load('BISamples','iris')['iris'];

  Data:=TBICSV.FromFile(IrisCSV);

  // Make a copy of data because we are going to "normalize" part of it
  Normalized:=TDataClone.Clone(Data);

  // Choose 4 columns as "features" (attributes)
  Attributes:=Normalized.Items.Select(1,4);

  // Do normalization of numeric columns
  TDataNormalize.Normalize(Attributes);

  // Visualization

  BIGrid1.BindTo(Data);

  BIChart1.Fill(ArrayOfItems(Data,['Petal.Length','Petal.Width','Species']));

  BIGrid2.BindTo(TDataMapAsData.FromData(Data['Species']));
  BIGrid3.BindTo(Normalized);
end;

procedure TFormkNN.Button2Click(Sender: TObject);
begin
  FitAndPredict;
end;

procedure AddLinearRegression(const AChart:TChart; const X,Y:TDataItem);
const
  LinearTitle='Linear Regression';

  // Returns the series in AChart containing the linear regression output
  function FindLinearSeries:TChartSeries;
  var t : Integer;
  begin
    for t:=0 to AChart.SeriesCount-1 do
        if SameText(AChart[t].Title,LinearTitle) then
           Exit(AChart[t]);

    result:=nil;
  end;

  // Add two points at min and max using the Regression slope
  procedure AddPoints(const Regression:TLinearRegression;
                      const ASeries:TChartSeries);
  var MinX,
      MaxX : TChartValue;
  begin
    MinX:=AChart.MinXValue(AChart.Axes.Bottom);
    MaxX:=AChart.MaxXValue(AChart.Axes.Bottom);

    ASeries.AddXY(MinX,(Regression.M*MinX)+Regression.B);
    ASeries.AddXY(MaxX,(Regression.M*MaxX)+Regression.B);
  end;

var L : TLinearRegression;
    S : TChartSeries;
begin
  // Try to reuse existing Line series
  S:=FindLinearSeries;

  if S=nil then
  begin
    // Create Line series if it does not exist yet
    S:=AChart.AddSeries(TLineSeries);
    S.Title:=LinearTitle;
  end;

  // Calculate regression
  L:=TLinearRegression.Create(nil);
  try
    L.X:=X;
    L.Y:=Y;
    L.Calculate;

    AddPoints(L,S);
  finally
    L.Free;
  end;
end;

procedure TFormkNN.Button3Click(Sender: TObject);
begin
  // Add a new Line series to chart, with a Linear Regression calculation
  AddLinearRegression(BIChart1.Chart,Data['Petal.Length'],Data['Petal.Width']);
end;

procedure TFormkNN.Button4Click(Sender: TObject);
begin
  if Model is TRSupervisedModel then
     TRSupervisedModel(Model).Plot;
end;

procedure TFormkNN.CBNativeRClick(Sender: TObject);
begin
  // Choose between "native" R and "command line" R
  if CBNativeR.Checked then
     TBIREngine.Engine:=TopaR.Create
  else
     TBIREngine.Engine:=TRCommand.Create;

  // Redirect R output to our console memo
  TBIREngine.Engine.Output:=RConsole.Memo.Lines;
end;

procedure TFormkNN.FitAndPredict;
var t1 : TStopWatch;
begin
  Model.Free;

  // Create model
  Model:=TSupervisedModelClass(CBModel.Items.Objects[CBModel.ItemIndex]).Create(Self);

  // Default Parameter for kNN:
  if Model is TBINearestNeighbour then
     (Model as TBINearestNeighbour).K:=1;

  // Set the "target" (or "label") and features
  Model.Target:=Normalized['Species'];
  Model.Attributes:=Attributes;

  // Tell model we are going to use 2/3 of data to train, 1/3 to test
  Model.Split.Percent:=100*(2/3);

  // Do train and test
  t1:=TStopwatch.StartNew;

  Model.Calculate;

  LTime.Caption:=t1.ElapsedMilliseconds.ToString+' msec';

  // Visualization
  BIGrid4.BindTo(Model.Predicted);
  BIGrid5.BindTo(Model.Predicted.Confusion);

  LCorrect.Caption:=IntToStr(Model.Predicted.Correct)+' of '+Model.Predicted.Count.ToString;
end;

procedure TFormkNN.FormCreate(Sender: TObject);
begin
  // Create an R Console form
  RConsole:=TBIRConsole.Create(Self);
  TUICommon.AddForm(RConsole,TabConsole);

  // Initialize R engine
  CBNativeRClick(Self);

  // Add all registered Model classes to CBModel combobox
  TAllModels.AllModels(CBModel.Items);
  CBModel.ItemIndex:=CBModel.Items.IndexOfObject(TObject(TBINearestNeighbour));

  SetupData;
  FitAndPredict;

  PageControl1.ActivePage:=TabSheet1;
end;

procedure TFormkNN.FormDestroy(Sender: TObject);
begin
  // Clear all calculated data to avoid memory leaks
  Data.Free;
  Normalized.Free;
  BIGrid2.Data.Free;

  Model.Free;
end;

end.
