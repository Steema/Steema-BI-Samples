unit Unit24;

interface

{.$DEFINE LEAKCHECK}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, BI.VCL.Editor.BIGrid,

  {$IFDEF LEAKCHECK}
  LeakCheck,
  {$ENDIF}

  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.DataSource, BI.VCL.Grid,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, BI.Data, BI.VCL.Visualizer,
  BI.VCL.Editor.Grid, System.Threading, BI.VCL.DataControl;

type
  TForm24 = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Splitter1: TSplitter;
    BViewData: TButton;
    Button3: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Memo1: TMemo;
    BExecSQL: TButton;
    Panel4: TPanel;
    LError: TLabel;
    BIGrid1: TBIGrid;
    Button5: TButton;
    Button6: TButton;
    TabSheet2: TTabSheet;
    BIVisualizer1: TBIComposer;
    TabSheet3: TTabSheet;
    CBVerifySQL: TCheckBox;
    TabSheet4: TTabSheet;
    Benchmark: TButton;
    Button4: TButton;
    Button7: TButton;
    CBMultiCPU: TCheckBox;
    CBLoopThread: TCheckBox;
    Button8: TButton;
    procedure ListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BenchmarkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BViewDataClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BExecSQLClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }

    IChanging,
    CustomSQL : Boolean;

    Query : TDataProvider;

    IGridEditor : TBIGridEditor;

    procedure ChangedEditor(Sender: TObject);
    function DoVerifySQL:Boolean;
    procedure Recalculate(const AQuery:TDataProvider);
    procedure VerifySQL;
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

{$R *.dfm}

uses
  BI.Persist, BI.VCL.DataManager, BI.Compare, System.Diagnostics,
  BI.VCL.DataViewer, BI.Data.SQL, BI.VCL.Editor.DataSelect, BI.Expression,
  BI.Summary,

  BI.VCL.Visualizer.Chart, BI.VCL.Editor.Visualizer.Chart,

  BI.VCL.Editor.Hops, BI.Data.Expressions, BI.Data.HTML,

  BI.VCL.GridForm, BI.Tests.SummarySamples, BI.Tests.SelectSamples,
  BI.Query, BI.VCL.Editor.Query, BI.Queries.Benchmark;

// Compare existing data output with the results of parsing SQL text.
// If they are different, show Data Viewer dialog with the differences.
function TForm24.DoVerifySQL:Boolean;
var tmpOut,
    tmpMain,
    tmp : TDataItem;
    tmpDiff : TDifference;
begin
  result:=True;

  if (ListBox1.ItemIndex=22) or (ListBox1.ItemIndex=28) then
     tmpMain:=Samples.Movies
  else
     tmpMain:=Samples.Demo;

  tmp:=TBISQL.From(tmpMain,Memo1.Text);

  if CustomSQL then
  begin
    BIGrid1.Data.Free;
    BIGrid1.Data:=tmp;
  end
  else
  begin
    tmpDiff:=TDifference.Create;
    try
      if TDataCompare.Same(BIGrid1.Data,tmp,tmpDiff) then
      begin
        BIGrid1.Data.Free;
        BIGrid1.Data:=tmp;
      end
      else
      begin
        tmpDiff.Name:='Differences';

        tmpOut:=TDataItem.Create;
        try
          tmpOut.Items.Add(BIGrid1.Data);
          tmpOut.Items.Add(tmp);
          tmpOut.Items.Add(tmpDiff);

          TDataViewer.View(Self,tmpOut);
        finally
          tmpOut.Items.Clear;
          tmpOut.Free;
        end;

        result:=False;
      end;
    finally
      tmpDiff.Free;
    end;
  end;
end;

procedure TForm24.BExecSQLClick(Sender: TObject);
begin
  DoVerifySQL;
end;

procedure TForm24.Button1Click(Sender: TObject);
begin
  TDataManager.Edit(Self);
end;

// Test All Queries
procedure TForm24.Button2Click(Sender: TObject);
var t : Integer;
    t1 : TStopWatch;
begin
  t1:=TStopwatch.StartNew;

  for t:=0 to ListBox1.Count-1 do
  begin
    // Execute Query
    ListBox1.ItemIndex:=t;
    ListBox1Click(Self);

    if CBVerifySQL.Checked then
       if not DoVerifySQL then
          break;
  end;

  Caption:='Time: '+t1.ElapsedMilliseconds.ToString+' msec';
end;

procedure TForm24.Button3Click(Sender: TObject);
var tmp : TBIQuery;
begin
  tmp:=TBIQuery.From(Self,Query as TDataSelect);
  try
    TBIQueryEditor.Edit(Self,tmp);
  finally
    tmp.Free;
  end;
end;

procedure TForm24.Button4Click(Sender: TObject);
var D : TDataItem;
begin
  D:=TQueryBenchmark.BenchmarkAll(ListBox1.Items);
  try
    TBIGridForm.Present(Self,D);
  finally
    D.Free;
  end;
end;

procedure TForm24.Button7Click(Sender: TObject);
var tmp : Int64;
begin
  tmp:=TQueryBenchmark.MultiCPU(CBMultiCPU.Checked,CBLoopThread.Checked);
  Caption:='Time: '+tmp.ToString+' msec';
end;

procedure TForm24.Button8Click(Sender: TObject);
var tmp : TDataHops;
begin
  tmp:=THopsViewer.HopsFrom(Query as TDataSelect);
  THopsViewer.View(Self,tmp);
end;

// Execute current selected Query a number of time to benchmark its speed
procedure TForm24.BenchmarkClick(Sender: TObject);
var tmp : Int64;
    Rows : Int64;
    Num : Integer;
begin
  if ListBox1.ItemIndex<>-1 then
  begin
    tmp:=TQueryBenchmark.Benchmark(ListBox1.ItemIndex,Num,Rows);
    Caption:='Num: '+Num.ToString+' Time: '+tmp.ToString+' msec Rows: '+Rows.ToString;
  end;
end;

procedure TForm24.BViewDataClick(Sender: TObject);
begin
  if BIGrid1.Data<>nil then
     TDataViewer.View(Self,BIGrid1.Data);
end;

// Pre-Load demo tables
procedure TForm24.FormCreate(Sender: TObject);
begin
  Samples.LoadData;

  PageControl1.ActivePage:=TabSheet1;
end;

procedure TForm24.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

procedure TForm24.Recalculate(const AQuery:TDataProvider);
begin
  BIVisualizer1.Data:=nil;

  BIGrid1.Data.Free;

  BIGrid1.Data:=TDataItem.Create(AQuery);

  IChanging:=True;
  try
    Memo1.Lines.Text:=TBISQL.From(AQuery);
    Caption:='Rows: '+IntToStr(BIGrid1.Data.Count);

    if CBVerifySQL.Checked then
       VerifySQL;

  finally
    IChanging:=False;
  end;

//  if IGridEditor<>nil then
//     IGridEditor.FillColumns;

  PageControl1Change(Self);
end;

procedure TForm24.ChangedEditor(Sender: TObject);
begin
  Recalculate( (Sender as TDataSelectEditor).Select);
end;

procedure TForm24.ListBox1Click(Sender: TObject);
var tmp : Integer;
begin
  tmp:=ListBox1.ItemIndex;

  Benchmark.Enabled:=tmp<>-1;
  BExecSQL.Enabled:=tmp<>-1;

  if tmp<>-1 then
  begin
    // Create example Query
    Query:=TSelectSamples.CreateSelect(Self,tmp);

    // Assign Query to grid
    Recalculate(Query);

    CustomSQL:=False;
  end;

  BViewData.Enabled:=BIGrid1.Data<>nil;
end;

function RemoveLineFeeds(const S:String):String;
var i : Integer;
begin
  result:=S;

  repeat
    i:=Pos(#13,result);

    if i>0 then
       result[i]:=' '
    else
    begin
      i:=Pos(#10,result);

      if i>0 then
         result[i]:=' ';
    end;

  until i=0;
end;

procedure TForm24.VerifySQL;
var tmp : TDataProvider;
    tmpParser : TSQLParser;
begin
  tmpParser:=TSQLParser.Create((Query as TDataSelect).Data{Demo},Memo1.Text);
  try
    LError.Caption:='';

    tmpParser.OnGetData:= procedure(const AName:String; out AData:TDataItem)
    begin
      if SameText(AName,'movies') then
         AData:=Samples.Movies['movies'];
    end;

    tmp:=tmpParser.Parse( function(const Sender:TObject; const Error:String):Boolean
       begin
         LError.Caption:=RemoveLineFeeds(Error);
         result:=True;
       end);

    tmp.Free;

    {$IFDEF LEAKCHECK}
    TLeakCheck.GetReport(nil);
    {$ENDIF}
  finally
    tmpParser.Free;
  end;
end;

// Reparse Memo SQL text to verify its syntax
procedure TForm24.Memo1Change(Sender: TObject);
begin
  if not IChanging then
  begin
    CustomSQL:=True;
    VerifySQL;
  end;
end;

procedure TForm24.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabSheet2 then
     BIVisualizer1.Data:=BIGrid1.Data
  else
  if PageControl1.ActivePage=TabSheet3 then
     if IGridEditor=nil then
        IGridEditor:=TBIGridEditor.Embedd(Self,TabSheet3,BIGrid1);
end;

end.
