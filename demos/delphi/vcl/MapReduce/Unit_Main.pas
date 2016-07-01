unit Unit_Main;

interface

// Example of using TeeBI TMapReduce class

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BI.Data, BI.Persist,
  BI.DataSource, BI.VCL.DataControl, BI.VCL.Grid,
  System.Diagnostics, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TDemoForm = class(TForm)
    Panel1: TPanel;
    CBParallel: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    BIGrid2: TBIGrid;
    BIGrid1: TBIGrid;
    LBTest: TListBox;
    Label1: TLabel;
    LabelRows: TLabel;
    LabelTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBParallelClick(Sender: TObject);
    procedure LBTestClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    // Some sample data
    Movies : TDataItem;

    // Two fields in Movies data
    Year,
    Rating : TDataItem;

    function DoMapReduce:TDataItem;
    function Map_IsLeapYear:TDataItem;
    procedure RunTest;
  public
    { Public declarations }
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.dfm}

uses
  BI.Arrays, BI.MapReduce, BI.Summary;

procedure TDemoForm.CBParallelClick(Sender: TObject);
begin
  // Enable multi-cpu calculation using threads.

  // It only makes sense when the number of rows is huge (10 million or more),
  // or when the map/reduce functions are complex

  TMapReduce.Parallel:=CBParallel.Checked;
end;

type
  TDataAccess=class(TDataItem);

function TDemoForm.Map_IsLeapYear:TDataItem;
var tmpLeap : TDataItem;
begin
  tmpLeap:=TMapReduce<Boolean>.ForAll(Year,
            function(const Index:TInteger):Boolean
            begin
              result:=IsLeapYear(Year.Int32Data[Index]);
            end);

  tmpLeap.Name:='Is Leap Year?';

  result:=TDataItem.Create(True);
  result.Items.Add(TDataClone.Clone(Year));
  result.Items.Add(tmpLeap);
  TDataAccess(result).FCount:=Year.Count;
end;

function TDemoForm.DoMapReduce:TDataItem;
begin
  case LBTest.ItemIndex of

  // Easy mode:

    0: // Return the count of ocurrences for each distinct Year value
       result:=TMapReduce.Count(Year);

    1: // Return the average of Rating field values for each distinct Year value
       result:=TMapReduce.Aggregate(Year,Rating,TAggregate.Average);

    2: // Return the average of an expression values for each distinct Year value
       result:=TMapReduce.Aggregate(Year,'Votes/Length',TAggregate.Average);

    3: // Map all values in Year field, to calculate IsLeapYear
       result:=Map_IsLeapYear;
  else

  // Advanced mode, using one Map and one Reduce function

  result:=TMapReduce<Integer,Single>.From(Movies,

         // Map
         function(const Index:TInteger):Integer
         begin
           result:=Year.Int32Data[Index]; // <-- return "Year" as the key
         end,

         // Reduce
         function(const Key:Integer; const List:TIndices):Single
         begin
           result:=TMapReduce.Mean(Rating,List); // Use "List" indices to return a mean
         end);

  end;
end;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  // Load Movies sample database from default "BISamples" folder
  Movies:=TStore.Load('BISamples','MovieDB')['Movies'];

  // Cache these two fields, to use in the demo
  Year:=Movies['Year'];
  Rating:=Movies['Rating'];

  // Show full Movies data on BIGrid2
  Movies.Load;
  BIGrid2.Data:=Movies;

  LabelRows.Caption:=Movies.Count.ToString;

  PageControl1.ActivePage:=TabSheet1;
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  // Just release memory to avoid leak
  BIGrid1.Data.Free;
end;

procedure TDemoForm.RunTest;
var t : Integer;
begin
  for t:=0 to LBTest.Count-1 do
  begin
    LBTest.ItemIndex:=t;
    LBTestClick(Self);
  end;
end;

procedure TDemoForm.FormShow(Sender: TObject);
begin
  if TUICommon.AutoTest then
  begin
    RunTest;
    Close;
  end;
end;

procedure TDemoForm.LBTestClick(Sender: TObject);
var t1 : TStopWatch;
begin
  t1:=TStopwatch.StartNew;
  try
    BIGrid1.Data.Free;

    // Calculate map-reduce and show results at BIGrid1
    BIGrid1.Data:=DoMapReduce;
  finally
    LabelTime.Caption:='Time: '+t1.ElapsedMilliseconds.ToString+' msec';
  end;
end;

end.
