unit TeeBI_kNN_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Data, BI.VCL.Grid, VclTee.TeeGDIPlus,
  VCLTee.TeEngine, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart, BI.VCL.Chart,
  Vcl.ComCtrls, Vcl.StdCtrls, VCLTee.Series,
  BI.VCL.R.Console,
  BI.Algorithm.Model, BI.DataSource, System.Diagnostics, BI.VCL.DataViewer;

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
    CheckBox1: TCheckBox;
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
    procedure CheckBox1Click(Sender: TObject);
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
  BI.Data.CSV, System.IOUtils, BI.UI, BI.Algorithm,
  BI.Algorithm.Classify, BI.Algorithm.Regression, BI.Persist,
  BI.Plugins.R, BI.Plugins.R.opaR, BI.Plugins.R.Command;

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

procedure TFormkNN.Button1Click(Sender: TObject);
var Cross : TBICrossTable;
begin
  Cross:=TBICrossTable.Create;
  try
    Cross.Real:=Model.Predicted.Items[1];
    Cross.Predicted:=Model.Predicted.Items[2];

    Cross.Calculate;

    TDataViewer.View(Self,Cross.Output);
  finally
    Cross.Free;
  end;
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
  TModel.Normalize(Attributes);

  // Visualization

  BIGrid1.BindTo(Data);
  BIChart1.Fill([Data['Petal.Length'],Data['Petal.Width']], Data['Species']);
  BIGrid2.BindTo(TDataMapAsData.FromData(Data['Species']));
  BIGrid3.BindTo(Normalized);
end;

procedure TFormkNN.Button2Click(Sender: TObject);
begin
  FitAndPredict;
end;

procedure TFormkNN.Button3Click(Sender: TObject);
const
  LinearTitle='Linear Regression';

  // Returns the series in BIChart1 that will contain the linear regression
  // output.
  function FindLinearSeries:TChartSeries;
  var t : Integer;
  begin
    for t:=0 to BIChart1.SeriesCount-1 do
        if SameText(BIChart1[t].Title,LinearTitle) then
           Exit(BIChart1[t]);

    result:=nil;
  end;

var L : TLinearRegression;
    S : TChartSeries;
    MinX,
    MaxX : TChartValue;
begin
  S:=FindLinearSeries;

  if S<>nil then
     S.Free;

  L:=TLinearRegression.Create;
  try
    L.X:=Data['Petal.Length'];
    L.Y:=Data['Petal.Width'];

    L.Calculate;

    MinX:=BIChart1.MinXValue(BIChart1.Axes.Bottom);
    MaxX:=BIChart1.MaxXValue(BIChart1.Axes.Bottom);

    S:=BIChart1.AddSeries(TLineSeries);
    S.Title:=LinearTitle;

    S.AddXY(MinX,(L.M*MinX)+L.B);
    S.AddXY(MaxX,(L.M*MaxX)+L.B);
  finally
    L.Free;
  end;
end;

procedure TFormkNN.Button4Click(Sender: TObject);
begin
  if Model is TRSupervisedModel then
     TRSupervisedModel(Model).Plot;
end;

procedure TFormkNN.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
     TBIREngine.Engine:=TopaR.Create
  else
     TBIREngine.Engine:=TRCommand.Create;

  TBIREngine.Engine.Output:=RConsole.Memo.Lines;
end;

procedure TFormkNN.FitAndPredict;
var t1 : TStopWatch;
begin
  Model.Free;

  // Create model
  Model:=TSupervisedModelClass(CBModel.Items.Objects[CBModel.ItemIndex]).Create;

  // Default Parameter for kNN:
  if Model is TBINearestNeighbour then
     (Model as TBINearestNeighbour).K:=1;

  // Set the "target" (or "label") and features
  Model.Target:=Normalized['Species'];
  Model.Attributes:=Attributes;

  // Tell model we are going to use 2/3 of data to train, 1/3 to test
  Model.Split(2/3);

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
  RConsole:=TBIRConsole.Create(Self);
  TUICommon.AddForm(RConsole,TabConsole);

  CheckBox1Click(Self);

  TAllModels.AllModels(CBModel.Items);
  CBModel.ItemIndex:=CBModel.Items.IndexOfObject(TObject(TBINearestNeighbour));

  SetupData;
  FitAndPredict;

  PageControl1.ActivePage:=TabSheet1;
end;

procedure TFormkNN.FormDestroy(Sender: TObject);
begin
  Data.Free;
  Normalized.Free;
  BIGrid2.Data.Free;

  Model.Free;
end;

end.
