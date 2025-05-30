{*********************************************}
{  TeeBI Software Library                     }
{  DateTime range editor dialog               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DateTimeRange;

interface

(*
   Select a custom from/to DateTime range by different ways:

   1) Comboboxes (day, month, year)

   2) Calendar controls

   3) Trackbar from/to

   4) A specific DateTime part (day, week, etc)

*)

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  {$IFDEF FPC}
  Calendar,
  {$ENDIF}

  BI.DataItem, Vcl.ComCtrls, VCLBI.Editor.ListItems, Vcl.ExtCtrls,
  BI.Expression, VCLBI.Editor.NumericFromTo, BI.Arrays;

type
  TSelectedPart=record
  public
    Enabled : Boolean;
    Part : TDateTimePart;
    Value : Integer;
    DateTime : TDateTime;
  end;

  {$IFDEF FPC}
  TMonthCalendar=TCalendar;
  {$ENDIF}

  TDateTimeRangeEditor = class(TForm)
    PageControl1: TPageControl;
    TabRange: TTabSheet;
    PageRange: TPageControl;
    TabCombo: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    CBDay: TComboBox;
    CBMonth: TComboBox;
    CBYear: TComboBox;
    CBDayTo: TComboBox;
    CBMonthTo: TComboBox;
    CBYearTo: TComboBox;
    CBPeriod: TComboBox;
    CBPeriod2: TComboBox;
    TabCalendar: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    CalendarFrom: TMonthCalendar;
    CalendarTo: TMonthCalendar;
    TabFromTo: TTabSheet;
    TabSelected: TTabSheet;
    Panel1: TPanel;
    Label5: TLabel;
    CBPart: TComboBox;
    CBFromEqual: TComboBox;
    CBToEqual: TComboBox;
    procedure CBYearChange(Sender: TObject);
    procedure CBMonthChange(Sender: TObject);
    procedure CBYearToChange(Sender: TObject);
    procedure CBMonthToChange(Sender: TObject);
    procedure CBPeriodChange(Sender: TObject);
    procedure CBPeriod2Change(Sender: TObject);
    procedure CalendarFromClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBDayChange(Sender: TObject);
    procedure CBDayToChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageRangeChange(Sender: TObject);
    procedure CBPartChange(Sender: TObject);
    procedure CBFromEqualChange(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    FOnChanged : TNotifyEvent;

    IDatePart : TDateTimePart;

    IFromTo,
    ISelected : TNumericFromTo;

    procedure ChangedFromTo(Sender: TObject);
    procedure ChangedSelected(Sender: TObject);
    function DateFrom(const AYear,AMonth,ADay:TComboBox):TDateTime;
    procedure DoAddDays(const AYear,AMonth,ADay:TComboBox; First:Boolean);
    procedure DoAddMonths(const AYear,AMonth:TComboBox; First:Boolean);
    procedure Modified;
    procedure SelectCombos(const AFrom,ATo:TDateTime; const AFromEqual,AToEqual:Boolean);
    function SelectedPart:TDateTimePart;
    procedure SetCustom;
  public
    { Public declarations }

    Data : TDataItem;

    class procedure AddMonths(const AItems:TStrings; const AMin,AMax:Word); static;

    function FromEqual:Boolean;
    function FromDate:TDateTime;

    function ToEqual:Boolean;
    function ToDate:TDateTime;

    function Part:TSelectedPart;

    procedure Refresh(const AData:TDataItem);

    procedure SelectRange(const AFrom,ATo:TDateTime; const AFromEqual,AToEqual:Boolean); overload;
    procedure SelectRange(const AData:TDataItem); overload;

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

uses
  BI.Expressions, BI.Expression.DateTime,
  System.DateUtils, VCLBI.Grid;

type
  TDatePart=record
    Value : TDateTime;

    Day,
    Month,
    Year : Word;

    Equal : Boolean;

    procedure Init(const AValue:TDateTime; const AEqual:Boolean);
  end;

  TDateRange=record
  public
    Min,
    Max : TDatePart;

    procedure Init(const AFrom,ATo:TDateTime; const AFromEqual,AToEqual:Boolean); overload;
    procedure Init(const AFrom,ATo:TDateTime); overload; inline;

    procedure Init(const AData:TDataItem); overload;
  end;


procedure TDatePart.Init(const AValue:TDateTime; const AEqual:Boolean);
begin
  Value:=AValue;
  DecodeDate(AValue,Year,Month,Day);

  Equal:=AEqual;
end;

procedure TDateRange.Init(const AFrom,ATo:TDateTime; const AFromEqual,AToEqual:Boolean);
begin
  Min.Init(AFrom,AFromEqual);
  Max.Init(ATo,AToEqual);
end;

procedure TDateRange.Init(const AData: TDataItem);
var tmp : TDateTimeStats;
begin
  tmp:=TDateTimeStats(AData.Stats);

  // >= ... <=
  Init(tmp.Min,tmp.Max,True,True);
end;

procedure AddYears(const AItems:TStrings; const AMin,AMax:Word);
var t : Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    for t:=AMin to AMax do
        AItems.Add(IntToStr(t));
  finally
    AItems.EndUpdate;
  end;
end;

procedure TDateTimeRangeEditor.Refresh(const AData:TDataItem);
var tmp : TDateRange;
begin
  IChanging:=True;
  try
    Data:=AData;

    CBYear.Tag:=NativeInt(AData);

    tmp.Init(AData);

    AddYears(CBYear.Items,tmp.Min.Year,tmp.Max.Year);
    CBYear.ItemIndex:=0;
    CBYearChange(Self);

    AddYears(CBYearTo.Items,tmp.Min.Year,tmp.Max.Year);
    CBYearTo.ItemIndex:=CBYearTo.Items.Count-1;
    CBYearToChange(Self);

    // All Range
    CBPeriod.ItemIndex:=0;

    CalendarFrom.{$IFDEF FPC}DateTime{$ELSE}Date{$ENDIF}:=tmp.Min.Value;
    CalendarTo.{$IFDEF FPC}DateTime{$ELSE}Date{$ENDIF}:=tmp.Max.Value;

    if IFromTo<>nil then
       IFromTo.Refresh(AData);

    if ISelected<>nil then
       ISelected.Refresh(AData);

  finally
    IChanging:=False;
  end;
end;

procedure AddDays(const AItems:TStrings; const AMin,AMax:Word);
var t : Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    for t:=AMin to AMax do
        AItems.Add(IntToStr(t));
  finally
    AItems.EndUpdate;
  end;
end;

function TDateTimeRangeEditor.DateFrom(const AYear, AMonth,
  ADay: TComboBox): TDateTime;
begin
  result:=EncodeDate(
             StrToInt(AYear.Items[AYear.ItemIndex]),
             Integer(AMonth.Items.Objects[AMonth.ItemIndex]),
             StrToInt(ADay.Items[ADay.ItemIndex]));
end;

procedure TDateTimeRangeEditor.DoAddDays(const AYear,AMonth,ADay:TComboBox; First:Boolean);
var tmp : TDateRange;
    tmpMin,
    tmpMax : Word;
    Old : String;
    tmpY,
    tmpM,
    tmpIndex : Integer;
begin
  tmp.Init(Data);

  if (AMonth.ItemIndex=0) and (AYear.ItemIndex=0) then
     tmpMin:=tmp.Min.Day
  else
     tmpMin:=1;

  if (AMonth.ItemIndex=AMonth.Items.Count-1) and
     (AYear.ItemIndex=AYear.Items.Count-1) then
     tmpMax:=tmp.Max.Day
  else
  begin
    tmpY:=StrToInt(AYear.Text);
    tmpM:=Integer(AMonth.Items.Objects[AMonth.ItemIndex]);

    tmpMax:=DaysInAMonth(tmpY,tmpM);
  end;

  if ADay.ItemIndex=-1 then
     Old:=''
  else
     Old:=ADay.Items[ADay.ItemIndex];

  AddDays(ADay.Items,tmpMin,tmpMax);

  tmpIndex:=ADay.Items.IndexOf(Old);

  if tmpIndex=-1 then
     if First then
        ADay.ItemIndex:=0
     else
        ADay.ItemIndex:=ADay.Items.Count-1
  else
     ADay.ItemIndex:=tmpIndex;
end;

procedure TDateTimeRangeEditor.Modified;
begin
  if not IChanging then
     if Assigned(FOnChanged) then
        FOnChanged(Self);
end;

procedure TDateTimeRangeEditor.ChangedFromTo(Sender: TObject);
begin
  SelectRange(IFromTo.FromValue,IFromTo.ToValue,IFromTo.FromEqual,IFromTo.ToEqual);
  SetCustom;
  Modified;
end;

type
  TNumericFromToAccess=class(TNumericFromTo);

procedure TDateTimeRangeEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabSelected then
     if ISelected=nil then
     begin
       CBPart.Items.Text:=TDateTimePart.AllToText;

       ISelected:=TNumericFromTo.Embedd(Self,TabSelected,ChangedSelected);
       TNumericFromToAccess(ISelected).HideTo;
       ISelected.Refresh(Data);
     end;
end;

procedure TDateTimeRangeEditor.PageRangeChange(Sender: TObject);
begin
  if PageRange.ActivePage=TabFromTo then
     if TabFromTo.ControlCount=0 then
     begin
       IFromTo:=TNumericFromTo.Embedd(Self,TabFromTo,ChangedFromTo);
       IFromTo.Refresh(Data);
     end;
end;

function TDateTimeRangeEditor.Part: TSelectedPart;
begin
  result.Enabled:=(ISelected<>nil) and ISelected.CBFrom.Checked;

  if result.Enabled then
  begin
    result.Part:=SelectedPart;

    if result.Part=TDateTimePart.None then
    begin
      result.Value:=0;
      result.DateTime:=ISelected.FromValue;
    end
    else
      result.Value:=Round(ISelected.FromValue);
  end;
end;

procedure TDateTimeRangeEditor.CalendarFromClick(Sender: TObject);
begin
  SelectRange(CalendarFrom.{$IFDEF FPC}DateTime{$ELSE}Date{$ENDIF},
              CalendarTo.{$IFDEF FPC}DateTime{$ELSE}Date{$ENDIF},
              True, // CBFromCalendar.ItemIndex=0
              True  // CBToCalendar.ItemIndex=0
              );

  SetCustom;
  Modified;
end;

procedure TDateTimeRangeEditor.CBDayChange(Sender: TObject);
begin
  SetCustom;
  Modified;
end;

procedure TDateTimeRangeEditor.CBDayToChange(Sender: TObject);
begin
  SetCustom;
  Modified;
end;

procedure TDateTimeRangeEditor.CBFromEqualChange(Sender: TObject);
begin
  Modified;
end;

procedure TDateTimeRangeEditor.CBMonthChange(Sender: TObject);
begin
  if CBMonth.ItemIndex=-1 then
     CBDay.Clear
  else
     DoAddDays(CBYear,CBMonth,CBDay,True);

  SetCustom;
  Modified;
end;

class procedure TDateTimeRangeEditor.AddMonths(const AItems:TStrings; const AMin,AMax:Word);
var t : Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    for t:=AMin to AMax do
        AItems.AddObject(FormatSettings.LongMonthNames[t],TObject(t));
  finally
    AItems.EndUpdate;
  end;
end;

procedure TDateTimeRangeEditor.DoAddMonths(const AYear,AMonth:TComboBox; First:Boolean);
var tmp : TDateRange;
    tmpMin,
    tmpMax : Word;
    Old : String;
    tmpIndex : Integer;
begin
  tmp.Init(Data);

  if AYear.ItemIndex=0 then
     tmpMin:=tmp.Min.Month
  else
  if tmp.Min.Year=tmp.Max.Year then
     tmpMin:=tmp.Min.Month
  else
     tmpMin:=1;

  if AYear.ItemIndex=AYear.Items.Count-1 then
     tmpMax:=tmp.Max.Month
  else
  begin
    if tmp.Min.Year=tmp.Max.Year then
       tmpMax:=tmp.Max.Month
    else
       tmpMax:=12
  end;

  if AMonth.ItemIndex=-1 then
     Old:=''
  else
     Old:=AMonth.Items[AMonth.ItemIndex];

  AddMonths(AMonth.Items,tmpMin,tmpMax);

  tmpIndex:=AMonth.Items.IndexOf(Old);

  if tmpIndex=-1 then
     if First then
        AMonth.ItemIndex:=0
     else
        AMonth.ItemIndex:=AMonth.Items.Count-1
  else
     AMonth.ItemIndex:=tmpIndex;

  AMonth.OnChange(Self);
end;

procedure TDateTimeRangeEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabRange;
end;

procedure TDateTimeRangeEditor.ChangedSelected(Sender: TObject);
begin
  Modified;
end;

function TDateTimeRangeEditor.FromDate: TDateTime;
begin
  result:=DateFrom(CBYear,CBMonth,CBDay);
end;

function TDateTimeRangeEditor.FromEqual: Boolean;
begin
  result:=CBFromEqual.ItemIndex=0;
end;

procedure TDateTimeRangeEditor.CBMonthToChange(Sender: TObject);
begin
  if CBMonthTo.ItemIndex=-1 then
     CBDayTo.Clear
  else
     DoAddDays(CBYearTo,CBMonthTo,CBDayTo,False);

  SetCustom;
  Modified;
end;

procedure TDateTimeRangeEditor.SelectRange(const AData:TDataItem);
var tmp : TDateRange;
begin
  tmp.Init(AData);
  SelectRange(tmp.Min.Value,tmp.Max.Value,tmp.Min.Equal,tmp.Max.Equal);
end;

function TDateTimeRangeEditor.ToDate: TDateTime;
begin
  result:=DateFrom(CBYearTo,CBMonthTo,CBDayTo);
end;

function TDateTimeRangeEditor.ToEqual: Boolean;
begin
  result:=CBToEqual.ItemIndex=0;
end;

procedure TDateTimeRangeEditor.SelectCombos(const AFrom,ATo:TDateTime;
                                            const AFromEqual,AToEqual:Boolean);

  procedure AddToCombos(const AMin,AMax:TDatePart);
  begin
    CBYearTo.ItemIndex:=CBYearTo.Items.IndexOf(IntToStr(AMax.Year));

    if CBYearTo.ItemIndex=-1 then
    begin
      AddYears(CBYearTo.Items,AMin.Year,AMax.Year);
      CBYearTo.ItemIndex:=CBYearTo.Items.Count-1;
    end;

    CBYearToChange(Self);

    CBMonthTo.ItemIndex:=CBMonthTo.Items.IndexOfObject(TObject(AMax.Month));

    if CBMonthTo.ItemIndex=-1 then
    begin
      AddMonths(CBMonthTo.Items,AMin.Month,AMax.Month);
      CBMonthTo.ItemIndex:=CBMonthTo.Items.Count-1;
    end;

    CBMonthToChange(Self);

    CBDayTo.ItemIndex:=CBDayTo.Items.IndexOf(IntToStr(AMax.Day));

    if CBDayTo.ItemIndex=-1 then
    begin
      AddDays(CBDayTo.Items,AMin.Day,AMax.Day);
      CBDayTo.ItemIndex:=CBDayTo.Items.Count-1;
    end;
  end;

  procedure AddFromCombos(const AMin,AMax:TDatePart);
  begin
    CBYear.ItemIndex:=CBYear.Items.IndexOf(IntToStr(AMin.Year));

    if CBYear.ItemIndex=-1 then
    begin
      AddYears(CBYear.Items,AMin.Year,AMax.Year);
      CBYear.ItemIndex:=0;
    end;

    CBYearChange(Self);

    CBMonth.ItemIndex:=CBMonth.Items.IndexOfObject(TObject(AMin.Month));

    if CBMonth.ItemIndex=-1 then
    begin
      AddMonths(CBMonth.Items,AMin.Month,AMax.Month);
      CBMonth.ItemIndex:=0;
    end;

    CBMonthChange(Self);

    CBDay.ItemIndex:=CBDay.Items.IndexOf(IntToStr(AMin.Day));

    if CBDay.ItemIndex=-1 then
    begin
      AddDays(CBDay.Items,AMin.Day,AMax.Day);
      CBDay.ItemIndex:=0;
    end;
  end;

var tmp : TDateRange;
begin
  tmp.Init(AFrom,ATo,AFromEqual,AToEqual);

  AddFromCombos(tmp.Min,tmp.Max);
  AddToCombos(tmp.Min,tmp.Max);
end;

procedure TDateTimeRangeEditor.SelectRange(const AFrom,ATo:TDateTime;
                                           const AFromEqual,AToEqual:Boolean);
begin
  IChanging:=True;
  try
    SelectCombos(AFrom,ATo,AFromEqual,AToEqual);

    CalendarFrom.{$IFDEF FPC}DateTime{$ELSE}Date{$ENDIF}:=AFrom;
    CalendarTo.{$IFDEF FPC}DateTime{$ELSE}Date{$ENDIF}:=ATo;
  finally
    IChanging:=False;
  end;
end;

function TDateTimeRangeEditor.SelectedPart:TDateTimePart;
begin
  result:=TDateTimePart(CBPart.ItemIndex);
end;

procedure TDateTimeRangeEditor.CBPartChange(Sender: TObject);
var tmpLow,
    tmpHigh : Integer;
begin
  IDatePart:=SelectedPart;
  ISelected.DateTime:=IDatePart=TDateTimePart.None;
  ISelected.Float:=ISelected.DateTime;

  ISelected.CBFrom.Checked:=False;

  if ISelected.DateTime then
     ISelected.Refresh(Data)
  else
  begin
    tmpLow:=IDatePart.Low;
    tmpHigh:=IDatePart.High;

    if tmpLow=0 then
       Dec(tmpHigh);

    ISelected.Refresh(tmpLow,tmpHigh,tmpLow,tmpHigh);
  end;
end;

const
  Period_Last = 6;
  Period_Next = 7;

procedure TDateTimeRangeEditor.CBPeriod2Change(Sender: TObject);

  function GetWeek:TDateRange;
  var tmpDate : TDateTime;
  begin
    tmpDate:=StartOfTheWeek(Now);

    case CBPeriod.ItemIndex of
      Period_Last: tmpDate:=tmpDate-7; // Last
      Period_Next: tmpDate:=tmpDate+7; // Next
    end;

    result.Init(tmpDate,tmpDate+6);
  end;

  function GetMonth:TDateRange;
  var Y,M,D : Word;
  begin
    DecodeDate(Now,Y,M,D);

    case CBPeriod.ItemIndex of
      Period_Last: if M=1 then
         begin
           Dec(Y);
           M:=12;
         end
         else
           Dec(M); // Last

      Period_Next: if M=12 then
         begin
           Inc(Y);
           M:=1;
         end
         else
           Inc(M); // Next
    end;

    result.Init(EncodeDate(Y,M,1),EncodeDate(Y,M,DaysInAMonth(Y,M)));
  end;

  function GetQuarter:TDateRange;
  var Y,M,D : Word;
      FirstMonth,
      LastMonth,
      Quarter : Integer;
  begin
    DecodeDate(Now,Y,M,D);

    Quarter:=M div 3;

    case CBPeriod.ItemIndex of
      Period_Last: if Quarter=0 then
         begin
           Dec(Y);
           Quarter:=3; // Last
         end
         else
           Dec(Quarter); // Last

      Period_Next: if Quarter=3 then
         begin
           Inc(Y);
           Quarter:=0; // Next
         end
         else
           Inc(Quarter); // Next
    end;

    FirstMonth:=1+(3*Quarter);
    LastMonth:=FirstMonth+2;

    result.Init(EncodeDate(Y,FirstMonth,1),EncodeDate(Y,LastMonth,DaysInAMonth(Y,LastMonth)));
  end;

  function GetYear:TDateRange;
  var tmp : Word;
  begin
    tmp:=TBIDateTime.YearOf(Now);

    case CBPeriod.ItemIndex of
      Period_Last: Dec(tmp); // Last
      Period_Next: Inc(tmp); // Next
    end;

    result.Init(EncodeDate(tmp,1,1),EncodeDate(tmp,12,31));
  end;

var tmp : TDateRange;
begin
  case CBPeriod2.ItemIndex of
   -1: Exit;
    0: tmp:=GetWeek;
    1: tmp:=GetMonth;
    2: tmp:=GetQuarter;
    3: tmp:=GetYear;
  end;

  SelectRange(tmp.Min.Value,tmp.Max.Value,tmp.Min.Equal,tmp.Max.Equal);
  Modified;
end;

procedure TDateTimeRangeEditor.SetCustom;
begin
  if not IChanging then
     CBPeriod.ItemIndex:=1;
end;

procedure TDateTimeRangeEditor.CBPeriodChange(Sender: TObject);

  procedure SelectToday(const Delta:Integer);
  var tmp : TDateTime;
  begin
    tmp:=Today+Delta;
    SelectRange(tmp,tmp,FromEqual,ToEqual);
  end;

begin
  CBPeriod2.Enabled:=False;

  case CBPeriod.ItemIndex of
    0: SelectRange(Data); // All time
    1: ;                  // custom
    2: SelectToday(0);    // today
    3: SelectToday(-1);   // yesterday
    4: SelectToday(1);    // tomorrow
  else
    begin
      CBPeriod2.Enabled:=True;
      CBPeriod2Change(Self);
      Exit;
    end;
  end;

  Modified;
end;

procedure TDateTimeRangeEditor.CBYearChange(Sender: TObject);
begin
  if CBYear.ItemIndex=-1 then
  begin
    CBMonth.Clear;
    CBDay.Clear;
  end
  else
    DoAddMonths(CBYear,CBMonth,True);

  SetCustom;
  Modified;
end;

procedure TDateTimeRangeEditor.CBYearToChange(Sender: TObject);
begin
  if CBYearTo.ItemIndex=-1 then
  begin
    CBMonthTo.Clear;
    CBDayTo.Clear;
  end
  else
    DoAddMonths(CBYearTo,CBMonthTo,False);

  SetCustom;
  Modified;
end;

procedure TDateRange.Init(const AFrom, ATo: TDateTime);
begin
  Init(AFrom,ATo,True,True);
end;

end.
