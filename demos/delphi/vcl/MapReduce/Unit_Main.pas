unit Unit_Main;

interface

// Example of using TeeBI TMapReduce class

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BI.Data, BI.Persist,
  BI.DataSource, BI.VCL.DataControl, BI.VCL.Grid,
  System.Diagnostics;

type
  TDemoForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    CBParallel: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBParallelClick(Sender: TObject);
  private
    { Private declarations }

    // Some sample data
    Movies : TDataItem;

    // Two fields in Movies data
    Year,
    Rating : TDataItem;

    function DoMapReduce:TDataItem;
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

function TDemoForm.DoMapReduce:TDataItem;
begin
  // Easy mode:

  // Return the count of ocurrences for each distinct Year value
  //  result:=TMapReduce.Count(Year);

  // Return the average of Rating field values for each distinct Year value
  //  result:=TMapReduce.Aggregate(Year,Rating,TAggregate.Average);

  // Return the average of an expression values for each distinct Year value
  //  result:=TMapReduce.Aggregate(Year,'Votes/Length',TAggregate.Average);

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

procedure TDemoForm.Button1Click(Sender: TObject);
var t1 : TStopWatch;
begin
  Memo1.Clear;

  Memo1.Lines.Add('Data: '+Movies.Name+' rows: '+Movies.Count.ToString);

  t1:=TStopwatch.StartNew;
  try
    BIGrid1.Data.Free;

    // Calculate map-reduce and show results at BIGrid1
    BIGrid1.Data:=DoMapReduce;

  finally
    Memo1.Lines.Add('Time: '+t1.ElapsedMilliseconds.ToString+' msec');
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
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  // Just release memory to avoid leak
  BIGrid1.Data.Free;
end;

end.
