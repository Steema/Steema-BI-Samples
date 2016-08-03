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
    Benchmark: TButton;
    BViewData: TButton;
    Button3: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Memo1: TMemo;
    BExecSQL: TButton;
    Panel4: TPanel;
    LError: TLabel;
    BIGrid1: TBIGrid;
    CBVerifySQL: TCheckBox;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    TabSheet2: TTabSheet;
    BIVisualizer1: TBIComposer;
    TabSheet3: TTabSheet;
    Button7: TButton;
    CBMultiCPU: TCheckBox;
    CBLoopThread: TCheckBox;
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
  private
    { Private declarations }

    IChanging,
    CustomSQL : Boolean;

    Query : TDataProvider;

    IGridEditor : TBIGridEditor;

    procedure ChangedEditor(Sender: TObject);
    function DoBenchmark(const AIndex:Integer; out AIterations:Integer; out Rows:Int64):Int64;
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
  BI.Persist, BI.VCL.DataManager, System.Diagnostics, BI.Compare,
  BI.VCL.DataViewer, BI.Data.SQL, BI.VCL.Editor.DataSelect, BI.Expression,
  BI.Summary,

  BI.VCL.Visualizer.Chart, BI.VCL.Editor.Visualizer.Chart,

  BI.VCL.GridForm, BI.Tests.SummarySamples, BI.Tests.SelectSamples,
  BI.Query, BI.VCL.Editor.Query;

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
  // Debug test, show the new "Pivot" Query editor:
  tmp:=TBIQuery.From(Self,Query as TDataSelect);
  try
    TBIQueryEditor.Edit(Self,tmp);
  finally
    tmp.Free;
  end;

//  if Query is TDataSelect then
//     TDataSelectEditor.Edit(Self,Query as TDataSelect,ChangedEditor);
end;

procedure TForm24.Button4Click(Sender: TObject);
var D : TDataItem;
    t : Integer;
    tmpIterations : Integer;
    tmpPerSecond : Single;

    Rows,
    tmpElapsed : Int64;
begin
  D:=TDataItem.Create(True);
  try
    D.Items.Add('#',TDataKind.dkInt32);
    D.Items.Add('Test',TDataKind.dkText);
    D.Items.Add('Iterations',TDataKind.dkInt32);
    D.Items.Add('Msec',TDataKind.dkInt64);
    D.Items.Add('Iter/Second',TDataKind.dkSingle);
    D.Items.Add('Rows',TDataKind.dkInt64);

    D.Resize(ListBox1.Count);

    for t:=0 to ListBox1.Count-1 do
    begin
      ListBox1.ItemIndex:=t;

      D.Item[0].Int32Data[t]:=t;
      D.Item[1].TextData[t]:=ListBox1.Items[t];

      if t<>21 then  // <-- 21 fails because the second time Average is calculated, it is an AsTable result !
      begin
        tmpElapsed:=DoBenchmark(t,tmpIterations,Rows);

        D.Item[3].Int64Data[t]:=tmpElapsed;
        D.Item[2].Int32Data[t]:=tmpIterations;

        if tmpElapsed=0 then
           tmpPerSecond:=0
        else
           tmpPerSecond:=1000*tmpIterations/tmpElapsed;

        D.Item[4].SingleData[t]:=tmpPerSecond;
        D.Item[5].Int64Data[t]:=Rows;
      end;
    end;

    D.Name:='Total: '+D.Item[3].Int64Data.Sum.ToString+' msec';

    TBIGridForm.Present(Self,D);
  finally
    D.Free;
  end;
end;

procedure TForm24.Button7Click(Sender: TObject);
var Data : Array of TDataItem;
    Query : Array of TDataSelect;
    tmpMax : Integer;

  procedure FreeDatas;
  var t : Integer;
  begin
    for t:=0 to tmpMax-1 do
    begin
      Data[t].Free;
      Query[t].Free;
    end;
  end;

const
  MaxLoop=1000;

  procedure BenchQuery(N:Integer);
  var Query : TDataSelect;
      Data : TDataItem;
      Loop : Integer;
  begin
    for Loop:=1 to MaxLoop do
    begin
      Query:=TSelectSamples.CreateSelect(Self,N);
      Data:=Query.Calculate;

      Data.Free;
      Query.Free;
    end;
  end;

var Loop,
    t : Integer;
    t1 : TStopWatch;
begin
  t1:=TStopwatch.StartNew;

  tmpMax:=23;//ListBox1.Count;

  SetLength(Data,tmpMax);
  SetLength(Query,tmpMax);

  if CBMultiCPU.Checked then
  begin
    if CBLoopThread.Checked then
       TParallel.&For(0,tmpMax-1, procedure(N:Integer)
       var Query : TDataSelect;
           Data : TDataItem;
           Loop : Integer;
       begin
         for Loop:=1 to MaxLoop do
         begin
           Query:=TSelectSamples.CreateSelect(Self,N);
           Data:=Query.Calculate;

           Data.Free;
           Query.Free;
         end;
       end)
    else
    begin
      for Loop:=1 to MaxLoop do
      begin
        TParallel.&For(0,tmpMax-1,procedure(N:Integer)
        begin
          Query[N]:=TSelectSamples.CreateSelect(Self,N);
          Data[N]:=Query[N].Calculate;
        end);

        FreeDatas;
      end;
    end;
  end
  else
  begin
    if CBLoopThread.Checked then
       for t:=0 to tmpMax-1 do
           BenchQuery(t)
    else
    begin
      for Loop:=1 to MaxLoop do
      begin
        for t:=0 to tmpMax-1 do
        begin
          Query[t]:=TSelectSamples.CreateSelect(Self,t);
          Data[t]:=Query[t].Calculate;
        end;

        FreeDatas;
      end;
    end;
  end;

  Caption:='Time: '+t1.ElapsedMilliseconds.ToString+' msec';
end;

function TForm24.DoBenchmark(const AIndex:Integer; out AIterations:Integer; out Rows:Int64):Int64;

  function GetIterations:Integer;
  begin
    if AIndex=25 then
       result:=100  // slow distinct ProductID, Discount
    else
    if AIndex=22 then
       result:=100  // slow "movies"
    else
    if AIndex=19 then
       result:=1000  // slow "Year"
    else
       result:=10000;
  end;

var t1 : TStopWatch;
    tmp : TDataSelect;
    Item : TDataItem;
    t : Integer;
begin
  t1:=TStopwatch.StartNew;

  tmp:=TSelectSamples.CreateSelect(Self,AIndex);
  try
    Item:=nil;
    try
      AIterations:=GetIterations;

      for t:=1 to AIterations do
      begin
        if Item=nil then
           Item:=tmp.Calculate
        else
        begin
          // Query is calculated "on top" of existing results instead of
          // recreating the results again, for a more stressed benchmark.

          Item.Resize(0);
          tmp.Calculate(Item);
        end;
      end;

      if Item=nil then
         Rows:=0
      else
         Rows:=Item.Count;
    finally
      Item.Free;
    end;
  finally
    tmp.Free;
  end;

  result:=t1.ElapsedMilliseconds;
end;

// Execute current selected Query a number of time to benchmark its speed
procedure TForm24.BenchmarkClick(Sender: TObject);
var tmp : Int64;
    Rows : Int64;
    Num : Integer;
begin
  if ListBox1.ItemIndex<>-1 then
  begin
    tmp:=DoBenchmark(ListBox1.ItemIndex,Num,Rows);
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
