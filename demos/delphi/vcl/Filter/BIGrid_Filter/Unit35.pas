unit Unit35;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,

  BI.Expression.Filter, BI.Data, BI.VCL.DataControl, BI.VCL.Grid,
  BI.VCL.Editor.DynamicFilter, BI.VCL.Editor.Filter.Item, BI.Expression;

type
  TForm35 = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabDate: TTabSheet;
    LBDateExamples: TListBox;
    TabNumber: TTabSheet;
    LBNumberExamples: TListBox;
    TabText: TTabSheet;
    LBTextExamples: TListBox;
    Panel5: TPanel;
    Panel2: TPanel;
    CBEnabled: TCheckBox;
    CBInverted: TCheckBox;
    PanelEditor: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Label1: TLabel;
    EFilter: TEdit;
    LRows: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBDateExamplesClick(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure LBNumberExamplesClick(Sender: TObject);
    procedure CBInvertedClick(Sender: TObject);
    procedure LBTextExamplesClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    IEditor : TFilterItemEditor;

    DateFilter,
    NumberFilter,
    TextFilter : TFilterItem;

    Filter : TBIFilter;

    procedure ChangedFilter(Sender: TObject);
    function CurrentFilter:TFilterItem;
    procedure RefreshGrid;

    procedure SetDateExample(const AFilter:TFilterItem);
    procedure SetNumberExample(const AFilter:TFilterItem);
    procedure SetTextExample(const AFilter:TFilterItem);

    procedure ShowRowCount;
  public
    { Public declarations }
  end;

var
  Form35: TForm35;

implementation

{$R *.dfm}

// Generate a table with random data:
function SampleData:TDataItem;
const
  Num=5000;
  SampleText:Array[0..5] of String=('Spring','Summer','Autumn','Winter','North','South');

var t: Integer;

    tmpDate,
    tmpText,
    tmpNum : TDataItem;
begin
  result:=TDataItem.Create(True);

  tmpDate:=result.Items.Add('Date',TDataKind.dkDateTime);
  tmpNum:=result.Items.Add('Number',TDataKind.dkDouble);
  tmpText:=result.Items.Add('Text',TDataKind.dkText);

  result.Resize(Num);

  for t:=0 to Num-1 do
  begin
    tmpDate.DateTimeData[t]:=Date+t-1000;
    tmpNum.DoubleData[t]:= -1000 + Random(2000);

    if Random(100)<3 then
       tmpText.TextData[t]:=''  // some empty strings
    else
       tmpText.TextData[t]:= SampleText[Random(Length(SampleText))];
  end;

  // Set fixed numbers, just to filter them later:

  tmpNum.DoubleData[100]:=1234;
  tmpNum.DoubleData[1000]:=1234;
end;

procedure TForm35.Button1Click(Sender: TObject);
begin
  TDynamicFilterEditor.Edit(Self,Filter,BIGrid1.Data);
end;

procedure TForm35.CBEnabledClick(Sender: TObject);
begin
  CurrentFilter.Enabled:=CBEnabled.Checked;
  RefreshGrid;
end;

procedure TForm35.FormCreate(Sender: TObject);
begin
  BIGrid1.Data:=SampleData;

  Filter:=TBIFilter.Create;

  DateFilter:=Filter.Add(BIGrid1.Data['Date']);
  NumberFilter:=Filter.Add(BIGrid1.Data['Number']);
  TextFilter:=Filter.Add(BIGrid1.Data['Text']);

  IEditor:=TFilterItemEditor.Embedd(Self,PanelEditor,CurrentFilter);
  IEditor.OnChange:=ChangedFilter;

  ShowRowCount;
end;

procedure TForm35.ChangedFilter(Sender: TObject);
begin
  RefreshGrid;
end;

procedure TForm35.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
  Filter.Free;
end;

procedure TForm35.CBInvertedClick(Sender: TObject);
begin
  CurrentFilter.Inverted:=CBInverted.Checked;
  RefreshGrid;
end;

procedure TForm35.SetNumberExample(const AFilter:TFilterItem);
var tmp : TNumericFilter;
begin
  AFilter.Reset;

  tmp:=AFilter.Numeric;

  case LBNumberExamples.ItemIndex of
    0: tmp.Selected.Value:=1234;

    1: begin
        tmp.Selected.Value:=1234;
        AFilter.Inverted:=True;
       end;

    2: tmp.FromValue.Value:=0;

    3: begin
         tmp.FromValue.Value:=200;
         tmp.FromValue.Equal:=False;
       end;

    4: begin
         tmp.FromValue.Value:= -500;
         tmp.FromValue.Equal:=False;

         tmp.ToValue.Value:= 500;
         tmp.ToValue.Equal:=False;
       end;

    5: begin
         tmp.FromValue.Value:= -300;
         tmp.ToValue.Value:= 300;
       end;
  end;
end;

procedure TForm35.SetTextExample(const AFilter: TFilterItem);
var tmp : TTextFilter;
begin
  AFilter.Reset;

  tmp:=AFilter.Text;

  case LBTextExamples.ItemIndex of
    0: begin
         tmp.Style:=TTextFilterStyle.Contains;
         tmp.Text:='nter';
       end;

    1: begin
         tmp.Style:=TTextFilterStyle.IsEqual;
         tmp.Text:='Summer';
       end;

    2: begin
         tmp.Style:=TTextFilterStyle.Starts;
         tmp.Text:='Win';
       end;

    3: begin
         tmp.Style:=TTextFilterStyle.Ends;
         tmp.Text:='ing';
       end;

    4: tmp.Style:=TTextFilterStyle.IsEmpty;
  end;
end;

procedure TForm35.SetDateExample(const AFilter:TFilterItem);
var tmp : TDateTimeFilter;
begin
  AFilter.Reset;

  tmp:=AFilter.DateTime;

  case LBDateExamples.ItemIndex of
    0: tmp.Style:=TDateTimeFilterStyle.Yesterday;
    1: tmp.Style:=TDateTimeFilterStyle.Today;
    2: tmp.Style:=TDateTimeFilterStyle.Tomorrow;

    3: begin
         tmp.Style:=TDateTimeFilterStyle.Last;
         tmp.Quantity:=5;
         tmp.Period:=TDateTimeSpan.Day;
       end;

    4: begin
         tmp.Style:=TDateTimeFilterStyle.Next;
         tmp.Quantity:=5;
         tmp.Period:=TDateTimeSpan.Day;
       end;

    5: begin
         tmp.Style:=TDateTimeFilterStyle.This;
         tmp.Period:=TDateTimeSpan.Day;
       end;

    6: begin
         tmp.Style:=TDateTimeFilterStyle.This;
         tmp.Period:=TDateTimeSpan.Month;
       end;

    7: begin
         tmp.Style:=TDateTimeFilterStyle.Last;
         tmp.Period:=TDateTimeSpan.Month;
       end;

    8: begin
         tmp.Style:=TDateTimeFilterStyle.Next;
         tmp.Period:=TDateTimeSpan.Month;
       end;

    9: begin
         tmp.Style:=TDateTimeFilterStyle.This;
         tmp.Period:=TDateTimeSpan.Year;
       end;

   10: begin
         tmp.Style:=TDateTimeFilterStyle.Last;
         tmp.Period:=TDateTimeSpan.Year;
       end;

   11: begin
         tmp.Style:=TDateTimeFilterStyle.Next;
         tmp.Period:=TDateTimeSpan.Year;
       end;

   12: tmp.Months.April:=True;

   13: begin
         tmp.Months.April:=True;
         tmp.Style:=TDateTimeFilterStyle.This;
         tmp.Period:=TDateTimeSpan.Year;
       end;

   14: tmp.Weekdays.Saturday:=True;

   15: begin
         tmp.Style:=TDateTimeFilterStyle.This;
         tmp.Period:=TDateTimeSpan.Week;
       end;

   16: begin
         tmp.Style:=TDateTimeFilterStyle.Last;
         tmp.Period:=TDateTimeSpan.Week;
       end;

   17: begin
         tmp.Style:=TDateTimeFilterStyle.Next;
         tmp.Period:=TDateTimeSpan.Week;
       end;

   18: begin
         tmp.Selected.Enabled:=True;
         tmp.Selected.Part:=TDateTimePart.DayOfMonth;
         tmp.Selected.Value:=13;
       end;

   19: begin
         tmp.Style:=TDateTimeFilterStyle.Custom;
         tmp.FromDate:=Now-5;
         tmp.ToDate:=Now+3;
       end;
  else
    tmp.Style:=TDateTimeFilterStyle.All;
  end;
end;

procedure TForm35.RefreshGrid;
var tmp : TExpression;
begin
  tmp:=Filter.Filter;

  if tmp=nil then
  begin
    EFilter.Text:='';
    BIGrid1.Filter:=nil;
  end
  else
  try
    EFilter.Text:=tmp.ToString;
    BIGrid1.Filter:=tmp;
  finally
    tmp.Free;
  end;

  ShowRowCount;
end;

procedure TForm35.ShowRowCount;
begin
  LRows.Caption:='Rows: '+IntToStr(BIGrid1.DataSource.DataSet.RecordCount);
end;

procedure TForm35.LBDateExamplesClick(Sender: TObject);
begin
  SetDateExample(DateFilter);
  IEditor.Refresh(DateFilter);
  RefreshGrid;
end;

procedure TForm35.LBNumberExamplesClick(Sender: TObject);
begin
  SetNumberExample(NumberFilter);
  IEditor.Refresh(NumberFilter);
  RefreshGrid;
end;

procedure TForm35.LBTextExamplesClick(Sender: TObject);
begin
  SetTextExample(TextFilter);
  IEditor.Refresh(TextFilter);
  RefreshGrid;
end;

function TForm35.CurrentFilter:TFilterItem;
begin
  if PageControl1.ActivePage=TabDate then
     result:=DateFilter
  else
  if PageControl1.ActivePage=TabNumber then
     result:=NumberFilter
  else
     result:=TextFilter;
end;

procedure TForm35.PageControl1Change(Sender: TObject);
begin
  IEditor.Refresh(CurrentFilter);

  CBEnabled.Checked:=CurrentFilter.Enabled;
  CBInverted.Checked:=CurrentFilter.Inverted;
end;

end.
