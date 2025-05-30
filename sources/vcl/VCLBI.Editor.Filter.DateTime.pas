{*********************************************}
{  TeeBI Software Library                     }
{  TFilterItem DateTime editor dialog         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Filter.DateTime;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  BI.Arrays, BI.Expression.Filter, VCLBI.Editor.DateTimeRange,
  VCLBI.Editor.ListItems;

(*
   Select a datetime "range" using multiple options.

   1) Predefined ranges:

       "This month", "Last year" etc etc

   2) Custom from/to range

   3) Optional: include/exclude specific months and weekdays

*)

type
  TDateTimeFilterEditor = class(TForm)
    PageControl1: TPageControl;
    TabCommon: TTabSheet;
    TabCustom: TTabSheet;
    LBCommon: TListBox;
    TabIncluded: TTabSheet;
    PanelMonths: TPanel;
    PanelWeeks: TPanel;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure LBCommonClick(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    Item : TFilterItem;

    IMonths,
    IWeeks : TFormListItems;

    IRange : TDateTimeRangeEditor;

    FOnChanged : TNotifyEvent;

    procedure ChangedIncluded(Sender: TObject);
    procedure ChangedRange(Sender: TObject);

    procedure DoChanged;

    function Months: TBooleanArray;
    function WeekDays: TBooleanArray;
  public
    { Public declarations }

    class function Embed(const AOwner:TComponent; const AParent:TWinControl;
                         const AOnChange:TNotifyEvent):TDateTimeFilterEditor; static;

    procedure Refresh(const AItem:TFilterItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Grid, BI.CollectionItem, BI.Expression;

class function TDateTimeFilterEditor.Embed(const AOwner:TComponent;
                         const AParent:TWinControl;
                         const AOnChange:TNotifyEvent):TDateTimeFilterEditor;
begin
  result:=TDateTimeFilterEditor.Create(AOwner);
  TUICommon.AddForm(result,AParent);

  result.FOnChanged:=AOnChange;
end;

procedure AddWeekDays(const AItems:TStrings; const AMin,AMax:Word);
var t : Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    for t:=AMin to AMax do
        AItems.AddObject(FormatSettings.LongDayNames[t],TObject(t));
  finally
    AItems.EndUpdate;
  end;
end;


procedure TDateTimeFilterEditor.FormShow(Sender: TObject);
begin
  LBCommon.ItemIndex:=0;
end;

procedure TDateTimeFilterEditor.LBCommonClick(Sender: TObject);
var tmp : TDateTimeFilter;
begin
  Item.Reset;
  tmp:=Item.DateTime;

  case LBCommon.ItemIndex of
    0: ;// All time
    1: tmp.Style:=TDateTimeFilterStyle.Today;
    2: tmp.Style:=TDateTimeFilterStyle.Yesterday;
    3: begin tmp.Style:=TDateTimeFilterStyle.This; tmp.Period:=TDateTimeSpan.Week; end;
    4: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Day; tmp.Quantity:=7; end;
    5: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Day; tmp.Quantity:=14; end;
    6: begin tmp.Style:=TDateTimeFilterStyle.This; tmp.Period:=TDateTimeSpan.Month; end;
    7: begin tmp.Style:=TDateTimeFilterStyle.This; tmp.Period:=TDateTimeSpan.Quarter; end;
    8: begin tmp.Style:=TDateTimeFilterStyle.This; tmp.Period:=TDateTimeSpan.Year; end;
    9: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Week; end;
   10: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Month; end;
   11: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Quarter; end;
   12: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Year; end;
   13: begin tmp.Style:=TDateTimeFilterStyle.Last; tmp.Period:=TDateTimeSpan.Decade; end;
   14: tmp.Style:=TDateTimeFilterStyle.Tomorrow;
   15: begin tmp.Style:=TDateTimeFilterStyle.Next; tmp.Period:=TDateTimeSpan.Week; end;
   16: begin tmp.Style:=TDateTimeFilterStyle.Next; tmp.Period:=TDateTimeSpan.Month; end;
   17: begin tmp.Style:=TDateTimeFilterStyle.Next; tmp.Period:=TDateTimeSpan.Quarter; end;
   18: begin tmp.Style:=TDateTimeFilterStyle.Next; tmp.Period:=TDateTimeSpan.Year; end;
  end;

  DoChanged;
end;

procedure TDateTimeFilterEditor.Refresh(const AItem: TFilterItem);
begin
  Item:=AItem;

  if Item<>nil then
  begin
    IChanging:=True;
    try
      if IRange<>nil then
      begin
        IRange.Refresh(Item.Data);

        if Item.Numeric.FromValue.Enabled and Item.Numeric.ToValue.Enabled then
           IRange.SelectRange(Item.DateTime.FromDate,Item.DateTime.ToDate,
                              Item.Numeric.FromValue.Equal,
                              Item.Numeric.ToValue.Equal);
      end;
    finally
      IChanging:=False;
    end;
  end;
end;

type
  TFilterItemAccess=class(TFilterItem);
  TMonthsAccess=class(TMonths);
  TWeekDaysAccess=class(TWeekdays);

procedure TDateTimeFilterEditor.ChangedIncluded(Sender: TObject);
var tmp : TFilterItem;
    tmpDate : TDateTimeFilter;
begin
  if not IChanging then
  begin
    tmp:=Item;

    if tmp<>nil then
    begin
      TFilterItemAccess(tmp).BeginUpdate;
      try
        tmpDate:=tmp.DateTime;

        TMonthsAccess(tmpDate.Months).SetMonths(Months);
        TWeekDaysAccess(tmpDate.Weekdays).SetWeekdays(WeekDays);
      finally
        TFilterItemAccess(tmp).EndUpdate;
      end;

      DoChanged;
    end;
  end;
end;

function TDateTimeFilterEditor.Months: TBooleanArray;
var t : Integer;
begin
  result.Resize(12);

  for t:=0 to IMonths.LBItems.Count-1 do
      result[t]:=IMonths.LBItems.Checked[t];
end;

procedure TDateTimeFilterEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabCustom then
  begin
    if IRange=nil then
    begin
      IRange:=TDateTimeRangeEditor.Create(Self);
      TUICommon.AddForm(IRange,TabCustom);

      IRange.Refresh(Item.Data);
      IRange.OnChanged:=ChangedRange;
    end;
  end
  else
  if PageControl1.ActivePage=TabIncluded then
     if IMonths=nil then
     begin
       IMonths:=TFormListItems.Embed(Self,PanelMonths);
       TDateTimeRangeEditor.AddMonths(IMonths.LBItems.Items,1,12);
       IMonths.EnableDrag(False);
       IMonths.CheckAll(True);

       IWeeks:=TFormListItems.Embed(Self,PanelWeeks);
       AddWeekDays(IWeeks.LBItems.Items,1,7);
       IWeeks.EnableDrag(False);
       IWeeks.CheckAll(True);

       IMonths.OnChanged:=ChangedIncluded;
       IMonths.OnChecked:=ChangedIncluded;

       IWeeks.OnChanged:=ChangedIncluded;
       IWeeks.OnChecked:=ChangedIncluded;
     end;
end;

function TDateTimeFilterEditor.WeekDays: TBooleanArray;
var t : Integer;
begin
  result.Resize(7);

  for t:=0 to IWeeks.LBItems.Count-1 do
      result[t]:=IWeeks.LBItems.Checked[t];
end;

procedure TDateTimeFilterEditor.ChangedRange(Sender: TObject);
var tmp : TFilterItem;
    tmpDate : TDateTimeFilter;
    tmpPart : TSelectedPart;
begin
  if not IChanging then
  begin
    tmp:=Item;

    if tmp<>nil then
    begin
      TFilterItemAccess(tmp).BeginUpdate;
      try
        tmpDate:=tmp.DateTime;

        tmpDate.FromDate:=IRange.FromDate;
        tmpDate.FromEqual:=IRange.FromEqual;

        tmpDate.ToDate:=IRange.ToDate;
        tmpDate.ToEqual:=IRange.ToEqual;

        tmpDate.Style:=TDateTimeFilterStyle(IRange.CBPeriod.ItemIndex);

        tmpPart:=IRange.Part;
        tmpDate.Selected.Enabled:=tmpPart.Enabled;
        tmpDate.Selected.Part:=tmpPart.Part;
        tmpDate.Selected.Value:=tmpPart.Value;
        tmpDate.Selected.DateTime:=tmpPart.DateTime;

        if tmpDate.Style=TDateTimeFilterStyle.Custom then
        begin
          tmp.Numeric.FromValue.Enabled:=True;
          tmp.Numeric.ToValue.Enabled:=True;
        end;
      finally
        TFilterItemAccess(tmp).EndUpdate;
      end;

      DoChanged;
    end;
  end;
end;

procedure TDateTimeFilterEditor.DoChanged;
begin
  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

end.
