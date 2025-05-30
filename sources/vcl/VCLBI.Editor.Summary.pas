{*********************************************}
{  TeeBI Software Library                     }
{  TSummary Editor                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Summary;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes,
  Vcl.Graphics, System.Types, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Buttons,
  BI.DataItem, BI.Summary;

type
  TSummaryEditor = class(TForm)
    PageControl1: TPageControl;
    TabMeasure: TTabSheet;
    TabMeasures: TTabSheet;
    TabSheet2: TTabSheet;
    PanelGroups: TPanel;
    LMeasures: TCheckListBox;
    Panel1: TPanel;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    Button1: TButton;
    BRemoveMeasure: TButton;
    GroupBox1: TGroupBox;
    CBRemoveMissing: TCheckBox;
    CBRemoveCols: TCheckBox;
    TabDimensions: TTabSheet;
    Panel2: TPanel;
    UpDim: TSpeedButton;
    DownDim: TSpeedButton;
    Button3: TButton;
    BRemoveDim: TButton;
    LDimensions: TCheckListBox;
    PageMeasures: TPageControl;
    CBAggregate: TComboBox;
    CBMissingAsZero: TCheckBox;
    TabMeasureInfo: TTabSheet;
    Label4: TLabel;
    LabelMeasureKind: TLabel;
    TabCalc: TTabSheet;
    RGRunning: TRadioGroup;
    CBRunningRows: TCheckBox;
    RGPercentage: TRadioGroup;
    TabFilter: TTabSheet;
    EFilter: TEdit;
    LFilter: TLabel;
    EHaving: TEdit;
    LHaving: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    EMeasureExpression: TEdit;
    LMeasureError: TLabel;
    PageGroups: TPageControl;
    TabGroup: TTabSheet;
    RGLayout: TRadioGroup;
    TabHistogram: TTabSheet;
    CBHistogram: TCheckBox;
    Label1: TLabel;
    EBins: TEdit;
    UDBins: TUpDown;
    TabGroupData: TTabSheet;
    Label3: TLabel;
    LGroupError: TLabel;
    EGroupExpression: TEdit;
    CBMinAuto: TCheckBox;
    CBMaxAuto: TCheckBox;
    EHistoMin: TEdit;
    EHistoMax: TEdit;
    Label2: TLabel;
    Label8: TLabel;
    CBAutoBins: TCheckBox;
    Label9: TLabel;
    EHistFormat: TEdit;
    CBDatePart: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    EMeasureName: TEdit;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    Button2: TButton;
    Label12: TLabel;
    EGroupName: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure CBAggregateChange(Sender: TObject);
    procedure CBDatePartChange(Sender: TObject);
    procedure CBHistogramClick(Sender: TObject);
    procedure CBMissingAsZeroClick(Sender: TObject);
    procedure EBinsChange(Sender: TObject);
    procedure LMeasuresClick(Sender: TObject);
    procedure LMeasuresClickCheck(Sender: TObject);
    procedure LMeasuresDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RGLayoutClick(Sender: TObject);
    procedure CBRemoveColsClick(Sender: TObject);
    procedure CBRemoveMissingClick(Sender: TObject);
    procedure LMeasuresDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LDimensionsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LDimensionsClickCheck(Sender: TObject);
    procedure LDimensionsClick(Sender: TObject);
    procedure UpDimClick(Sender: TObject);
    procedure DownDimClick(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure BRemoveDimClick(Sender: TObject);
    procedure BRemoveMeasureClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RGRunningClick(Sender: TObject);
    procedure CBRunningRowsClick(Sender: TObject);
    procedure RGPercentageClick(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
    procedure EHavingChange(Sender: TObject);
    procedure EMeasureExpressionChange(Sender: TObject);
    procedure PageMeasuresResize(Sender: TObject);
    procedure EGroupExpressionChange(Sender: TObject);
    procedure PageGroupsResize(Sender: TObject);
    procedure CBMinAutoClick(Sender: TObject);
    procedure CBMaxAutoClick(Sender: TObject);
    procedure EHistoMinChange(Sender: TObject);
    procedure EHistoMaxChange(Sender: TObject);
    procedure CBAutoBinsClick(Sender: TObject);
    procedure EHistFormatChange(Sender: TObject);
    procedure EMeasureNameChange(Sender: TObject);
    procedure EGroupNameChange(Sender: TObject);
  private
    { Private declarations }

    ISummary : TSummary;
    IChanging : Boolean;

    procedure AddFields;
    procedure AddDimension(const ADim:TGroupBy);
    procedure AddMeasure(const AMeasure:TMeasure);
    procedure ChangedGroup;
    procedure ChangedMeasure;
    procedure Recalculate;
    procedure RecalculateHistogram;
    procedure RefreshMeasureInfo;
    function SelectedGroup:TGroupBy;
    function SelectedMeasure:TMeasure;
    procedure SwapMeasures(const A,B:Integer);
    procedure SwapDimensions(const A,B:Integer);
  public
    { Public declarations }
    OnRecalculate : TNotifyEvent;

    class function Edit(const AOwner:TComponent; const ASummary:TSummary):Boolean; static;

    procedure Refresh(const ASummary:TSummary);
  end;

implementation

{$R *.dfm}

uses
  Math,
  BI.Expression, VCLBI.DataManager, BI.Languages.English, BI.Expressions;

procedure TSummaryEditor.CBAggregateChange(Sender: TObject);
var tmp : TMeasure;
begin
  tmp:=SelectedMeasure;

  tmp.Aggregate:=TAggregate(CBAggregate.ItemIndex);
  CBMissingAsZero.Enabled:=tmp.Aggregate.SupportsAsZero;

  Recalculate;
  ChangedMeasure;
end;

procedure TSummaryEditor.CBAutoBinsClick(Sender: TObject);
begin
  UDBins.Position:=0;
  RecalculateHistogram;
end;

procedure TSummaryEditor.ChangedMeasure;
begin
  RefreshMeasureInfo;
  LMeasures.Items[LMeasures.ItemIndex]:=SelectedMeasure.ToString;
end;

procedure TSummaryEditor.CBRunningRowsClick(Sender: TObject);
begin
  SelectedMeasure.Calculation.RunningByRows:=CBRunningRows.Checked;
  Recalculate;
end;

procedure TSummaryEditor.CBDatePartChange(Sender: TObject);
begin
  SelectedGroup.DatePart:=TDateTimePart(CBDatePart.ItemIndex);

  Recalculate;
  ChangedGroup;
end;

procedure TSummaryEditor.ChangedGroup;
var tmp : Integer;
begin
  tmp:=LDimensions.ItemIndex;
  LDimensions.Items[tmp]:=SelectedGroup.ToString;
end;

procedure TSummaryEditor.CBHistogramClick(Sender: TObject);
begin
  SelectedGroup.Histogram.Active:=CBHistogram.Checked;

  EBins.Enabled:=CBHistogram.Checked;
  UDBins.Enabled:=CBHistogram.Checked;

  Recalculate;
end;

procedure TSummaryEditor.CBMaxAutoClick(Sender: TObject);
begin
  if not IChanging then
  begin
    EHistoMax.Enabled:=not CBMaxAuto.Checked;
    SelectedGroup.Histogram.AutoMaximum:=CBMaxAuto.Checked;
    RecalculateHistogram;
  end;
end;

procedure TSummaryEditor.CBMinAutoClick(Sender: TObject);
begin
  if not IChanging then
  begin
    EHistoMin.Enabled:=not CBMinAuto.Checked;
    SelectedGroup.Histogram.AutoMinimum:=CBMinAuto.Checked;
    RecalculateHistogram;
  end;
end;

procedure TSummaryEditor.CBMissingAsZeroClick(Sender: TObject);
begin
  if not IChanging then
  begin
    SelectedMeasure.Missing.AsZero:=CBMissingAsZero.Checked;
    Recalculate;
  end;
end;

procedure TSummaryEditor.CBRemoveColsClick(Sender: TObject);
begin
  if ISummary<>nil then
  begin
    if ISummary.RemoveMissing.Columns<>CBRemoveCols.Checked then
    begin
      ISummary.RemoveMissing.Columns:=CBRemoveCols.Checked;
      Recalculate;
    end;
  end;
end;

procedure TSummaryEditor.CBRemoveMissingClick(Sender: TObject);
begin
  if ISummary<>nil then
  begin
    if ISummary.RemoveMissing.Rows<>CBRemoveMissing.Checked then
    begin
      ISummary.RemoveMissing.Rows:=CBRemoveMissing.Checked;
      Recalculate;
    end;
  end;
end;

procedure TSummaryEditor.DownDimClick(Sender: TObject);
begin
  SwapDimensions(LDimensions.ItemIndex,LDimensions.ItemIndex+1);
end;

procedure TSummaryEditor.EBinsChange(Sender: TObject);
begin
  if UDBins.Showing then
     if SelectedGroup.Histogram.NumBins<>UDBins.Position then
     begin
       SelectedGroup.Histogram.NumBins:=UDBins.Position;
       RecalculateHistogram;
     end;
end;

class function TSummaryEditor.Edit(const AOwner: TComponent;
  const ASummary: TSummary):Boolean;
begin
  with TSummaryEditor.Create(AOwner) do
  try
    Refresh(ASummary);
    PanelButtons.Show;
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TSummaryEditor.EGroupExpressionChange(Sender: TObject);
var tmp : TExpression;
    Ok : Boolean;
begin
  if not IChanging then
  begin
    LGroupError.Caption:='';

    Ok:=True;

    tmp:=TDataExpression.FromString(ISummary.MainData,EGroupExpression.Text
         {$IFNDEF FPC}
         ,
         function(const APos:Integer; const AMessage:String):Boolean
         begin
           LGroupError.Caption:=Format(BIMsg_ExpressionError,[AMessage,APos]);
           Ok:=False;
           result:=True;
         end
         {$ENDIF}
         );

    if Ok and (tmp<>nil) then
    begin
      SelectedGroup.Expression:=tmp;
      Recalculate;
      ChangedGroup;
    end;
  end;
end;

procedure TSummaryEditor.EGroupNameChange(Sender: TObject);
begin
  if not IChanging then
  begin
    SelectedGroup.Name:=EGroupName.Text;
    Recalculate;
    ChangedGroup;
  end;
end;

procedure TSummaryEditor.EFilterChange(Sender: TObject);

  procedure ChangeFilter(const AFilter:TExpression);
  begin
    ISummary.Filter:=AFilter;
    Recalculate;
  end;

var tmp : String;
    tmpFilter : TExpression;
begin
  LFilter.Caption:='';

  if ISummary.Filter=nil then
     tmp:=''
  else
     tmp:=ISummary.Filter.ToString;

  if tmp<>EFilter.Text then
  begin
    if Trim(EFilter.Text)='' then
       ChangeFilter(nil)
    else
    begin
      tmpFilter:=TDataFilter.FromString(ISummary.MainData,EFilter.Text
                 {$IFNDEF FPC}
                 ,
                 function(const APos:Integer; const AMessage:String):Boolean
                 begin
                   LFilter.Caption:=Format(BIMsg_ExpressionError,[AMessage,APos]);
                   result:=True; // do not raise exception
                 end
                 {$ENDIF});

      if tmpFilter<>nil then
         ChangeFilter(tmpFilter);
    end;
  end;
end;

procedure TSummaryEditor.EHavingChange(Sender: TObject);
var tmp : String;
//    tmpFilter : TExpression;
begin
  LHaving.Caption:='';

  if ISummary.Having=nil then
     tmp:=''
  else
     tmp:=ISummary.Having.ToString;

  if tmp<>EHaving.Text then
  begin
    ISummary.Having.Clear;

    if Trim(EHaving.Text)<>'' then
    begin
      ISummary.Having.Add(EHaving.Text);
      Recalculate;

      {
      Pending: How to validate the expression here?
      ( before the summary is calculated )

      tmpFilter:=TSummaryFilter.FromString(ISummary.Data,EHaving.Text,
                 function(const APos:Integer; const AMessage:String):Boolean
                 begin
                   LHaving.Caption:=IntToStr(APos)+': '+AMessage;
                   result:=True; // do not raise exception
                 end);

      if tmpFilter<>nil then
         ChangeHaving(TDataFilter.VerifyLogical(tmpFilter,EHaving.Text,nil));
      }
    end;
  end;
end;

procedure TSummaryEditor.RecalculateHistogram;
begin
  if SelectedGroup.Histogram.Active then
     Recalculate;
end;

procedure TSummaryEditor.EHistFormatChange(Sender: TObject);
begin
  if not IChanging then
  begin
    SelectedGroup.Histogram.FloatFormat:=EHistFormat.Text;
    RecalculateHistogram;
  end;
end;

procedure TSummaryEditor.EHistoMaxChange(Sender: TObject);
var tmp : Double;
begin
  if not IChanging then
  if TryStrToFloat(EHistoMax.Text,tmp) then
  begin
    SelectedGroup.Histogram.Maximum:=tmp;
    RecalculateHistogram;
  end;
end;

procedure TSummaryEditor.EHistoMinChange(Sender: TObject);
var tmp : Double;
begin
  if not IChanging then
  if TryStrToFloat(EHistoMin.Text,tmp) then
  begin
    SelectedGroup.Histogram.Minimum:=tmp;
    RecalculateHistogram;
  end;
end;

procedure TSummaryEditor.EMeasureExpressionChange(Sender: TObject);
var tmp : TExpression;
    Ok : Boolean;
begin
  if not IChanging then
  begin
    LMeasureError.Caption:='';

    Ok:=True;

    tmp:=TDataExpression.FromString(ISummary.MainData,EMeasureExpression.Text
         {$IFNDEF FPC}
         ,
         function(const APos:Integer; const AMessage:String):Boolean
         begin
           LMeasureError.Caption:=Format(BIMsg_ExpressionError,[AMessage,APos]);
           Ok:=False;
           result:=True;
         end
         {$ENDIF});

    if Ok and (tmp<>nil) then
    begin
      SelectedMeasure.Expression:=tmp;
      Recalculate;
      ChangedMeasure;
    end;
  end;
end;

procedure TSummaryEditor.EMeasureNameChange(Sender: TObject);
begin
  if not IChanging then
  begin
    SelectedMeasure.Name:=EMeasureName.Text;
    Recalculate;
    ChangedMeasure;
  end;
end;

procedure TSummaryEditor.FormCreate(Sender: TObject);
begin
  PageMeasures.Visible:=False;
  PageGroups.Visible:=False;

  PageControl1.ActivePage:=TabMeasures;
  PageMeasures.ActivePage:=TabMeasure;
  PageGroups.ActivePage:=TabGroup;
end;

procedure TSummaryEditor.LDimensionsClick(Sender: TObject);

  procedure SetHistogram(const AHistogram:THistogram);
  begin
    CBHistogram.Checked:=AHistogram.Active;

    EBins.Enabled:=CBHistogram.Checked;
    UDBins.Enabled:=CBHistogram.Checked;

    UDBins.Position:=AHistogram.BinCount;
    CBAutoBins.Checked:=UDBins.Position=0;

    EHistoMin.Text:=FloatToStr(AHistogram.Minimum);
    CBMinAuto.Checked:=AHistogram.AutoMinimum;

    EHistoMax.Text:=FloatToStr(AHistogram.Maximum);
    CBMaxAuto.Checked:=AHistogram.AutoMaximum;

    EHistFormat.Text:=AHistogram.FloatFormat;
  end;

var tmp : Integer;
    tmpBy : TGroupBy;
begin
  IChanging:=True;

  tmp:=LDimensions.ItemIndex;

  PageGroups.Visible:=tmp<>-1;

  BRemoveDim.Enabled:=tmp<>-1;
  UpDim.Enabled:=tmp>0;
  DownDim.Enabled:=tmp<LDimensions.Count-1;

  if PageGroups.Visible then
  begin
    tmpBy:=SelectedGroup;

    RGLayout.ItemIndex:=Ord(tmpBy.Layout);

    if CBDatePart.Items.Count=0 then
       CBDatePart.Items.Text:=TDateTimePart.AllToText;

    CBDatePart.ItemIndex:=Ord(tmpBy.DatePart);

    SetHistogram(tmpBy.Histogram);

    EGroupName.Text:=tmpBy.Name;
    EGroupExpression.Text:=tmpBy.Expression.ToString;

    LGroupError.Caption:='';
  end;

  IChanging:=False;
end;

procedure TSummaryEditor.LDimensionsClickCheck(Sender: TObject);
begin
  SelectedGroup.Active:=LDimensions.Checked[LDimensions.ItemIndex];
  Recalculate;
end;

procedure TSummaryEditor.LDimensionsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp : Integer;
begin
  tmp:=LDimensions.ItemAtPos(Point(X,Y),True);

  if tmp<>-1 then
     SwapDimensions(LDimensions.ItemIndex,tmp);
end;

procedure TSummaryEditor.LMeasuresClick(Sender: TObject);
var tmp : TMeasure;
begin
  IChanging:=True;

  tmp:=SelectedMeasure;

  PageMeasures.Visible:=tmp<>nil;
  BRemoveMeasure.Enabled:=tmp<>nil;

  BUp.Enabled:=LMeasures.ItemIndex>0;
  BDown.Enabled:=LMeasures.ItemIndex<LMeasures.Count-1;

  if tmp<>nil then
  begin
    CBAggregate.ItemIndex:=Ord(tmp.Aggregate);
    CBMissingAsZero.Checked:=tmp.Missing.AsZero;
    CBMissingAsZero.Enabled:=tmp.Aggregate.SupportsAsZero;

    RGPercentage.ItemIndex:=Ord(tmp.Calculation.Percentage);
    CBRunningRows.Checked:=tmp.Calculation.RunningByRows;
    RGRunning.ItemIndex:=Ord(tmp.Calculation.Running);

    EMeasureName.Text:=tmp.Name;

    if tmp.Expression=nil then
       EMeasureExpression.Text:=''
    else
       EMeasureExpression.Text:=tmp.Expression.ToString;

    LMeasureError.Caption:='';
  end;

  RefreshMeasureInfo;

  IChanging:=False;
end;

procedure TSummaryEditor.RefreshMeasureInfo;
var tmp : TMeasure;
begin
  tmp:=SelectedMeasure;

  if tmp=nil then
     LabelMeasureKind.Caption:=TDataKind.dkUnknown.ToString
  else
     LabelMeasureKind.Caption:=tmp.Kind.ToString;
end;

procedure TSummaryEditor.LMeasuresClickCheck(Sender: TObject);
begin
  SelectedMeasure.Active:=LMeasures.Checked[LMeasures.ItemIndex];
  Recalculate;
end;

procedure TSummaryEditor.LMeasuresDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp : Integer;
begin
  tmp:=LMeasures.ItemAtPos(Point(X,Y),True);

  if tmp<>-1 then
     SwapMeasures(tmp,LMeasures.ItemIndex);
end;

procedure TSummaryEditor.LMeasuresDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=Sender;
end;

procedure TSummaryEditor.PageGroupsResize(Sender: TObject);
begin
  EGroupExpression.Width:=Max(40,PageGroups.Width-36);
end;

procedure TSummaryEditor.PageMeasuresResize(Sender: TObject);
begin
  EMeasureExpression.Width:=Max(40,PageMeasures.Width-36);
end;

procedure TSummaryEditor.Recalculate;
begin
  if Assigned(OnRecalculate) then
     OnRecalculate(Self);
end;

procedure TSummaryEditor.Refresh(const ASummary:TSummary);
begin
  IChanging:=True;
  try
    ISummary:=ASummary;

    if ISummary=nil then Exit;

    CBRemoveMissing.Checked:=ISummary.RemoveMissing.Rows;
    CBRemoveCols.Checked:=ISummary.RemoveMissing.Columns;

    AddFields;

    BUp.Enabled:=False;
    BDown.Enabled:=False;

    if ISummary.Filter=nil then
       EFilter.Text:=''
    else
       EFilter.Text:=ISummary.Filter.ToString;

    if ISummary.Having=nil then
       EHaving.Text:=''
    else
       EHaving.Text:=ISummary.Having.ToString;
  finally
    IChanging:=False;
  end;
end;

procedure TSummaryEditor.RGRunningClick(Sender: TObject);
begin
  SelectedMeasure.Calculation.Running:=TCalculationRunning(RGRunning.ItemIndex);
  Recalculate;
end;

procedure TSummaryEditor.RGLayoutClick(Sender: TObject);
begin
  if not IChanging  then
  begin
    SelectedGroup.Layout:=TGroupByLayout(RGLayout.ItemIndex);
    Recalculate;
  end;
end;

procedure TSummaryEditor.RGPercentageClick(Sender: TObject);
begin
  SelectedMeasure.Calculation.Percentage:=TCalculationPercentage(RGPercentage.ItemIndex);
  Recalculate;
end;

procedure TSummaryEditor.SwapMeasures(const A,B:Integer);
begin
  if A<>B then
  begin
    LMeasures.Items.Exchange(A,B);
    ISummary.Measures.Exchange(A,B);

    Recalculate;

    LMeasuresClick(Self);
  end;
end;

function TSummaryEditor.SelectedGroup: TGroupBy;
begin
  if LDimensions.ItemIndex=-1 then
     result:=nil
  else
     result:=ISummary.By[LDimensions.ItemIndex];
end;

function TSummaryEditor.SelectedMeasure: TMeasure;
begin
  if LMeasures.ItemIndex=-1 then
     result:=nil
  else
     result:=ISummary.Measures[LMeasures.ItemIndex];
end;

procedure TSummaryEditor.SwapDimensions(const A,B:Integer);
begin
  if A<>B then
  begin
    LDimensions.Items.Exchange(A,B);
    ISummary.By.Exchange(A,B);

    Recalculate;

    LDimensionsClick(Self);
  end;
end;

procedure TSummaryEditor.UpDimClick(Sender: TObject);
begin
  SwapDimensions(LDimensions.ItemIndex,LDimensions.ItemIndex-1);
end;

procedure TSummaryEditor.AddMeasure(const AMeasure:TMeasure);
var tmp : Integer;
begin
  tmp:=LMeasures.Items.Add(AMeasure.ToString);
  LMeasures.Checked[tmp]:=AMeasure.Active;

  if LMeasures.Count=1 then
  begin
    LMeasures.ItemIndex:=0;
    LMeasuresClick(Self);
  end;
end;

procedure TSummaryEditor.AddDimension(const ADim: TGroupBy);
var tmp : Integer;
begin
  tmp:=LDimensions.Items.Add(ADim.ToString);
  LDimensions.Checked[tmp]:=ADim.Active;

  if LDimensions.Count=1 then
  begin
    LDimensions.ItemIndex:=0;
    LDimensionsClick(Self);
  end;
end;

procedure TSummaryEditor.AddFields;
var t : Integer;
begin
  PageMeasures.Visible:=False;

  LMeasures.Clear;

  for t:=0 to ISummary.Measures.Count-1 do
      AddMeasure(ISummary.Measures[t]);

  PageGroups.Visible:=False;

  LDimensions.Clear;

  for t:=0 to ISummary.By.Count-1 do
      AddDimension(ISummary.By[t]);
end;

procedure TSummaryEditor.BDownClick(Sender: TObject);
begin
  SwapMeasures(LMeasures.ItemIndex,LMeasures.ItemIndex+1);
end;

procedure TSummaryEditor.BUpClick(Sender: TObject);
begin
  SwapMeasures(LMeasures.ItemIndex,LMeasures.ItemIndex-1);
end;

procedure TSummaryEditor.Button1Click(Sender: TObject);
var tmp : TDataItem;
begin
  if (ISummary.Measures.Count>0) and (ISummary.Measures[0].RealData<>nil) then
     tmp:=TDataManager.Choose(Self,ISummary.Measures[0].RealData.Parent,True)
  else
     tmp:=TDataManager.Choose(Self,nil,True);

  if tmp<>nil then
  begin
    AddMeasure(ISummary.AddMeasure(tmp,TAggregate.Sum));
    Recalculate;
  end;
end;

procedure TSummaryEditor.Button3Click(Sender: TObject);
var tmp : TDataItem;
begin
  if (ISummary.By.Count>0) and (ISummary.By[0].RealData<>nil) then
     tmp:=TDataManager.Choose(Self,ISummary.By[0].RealData.Parent,True)
  else
     tmp:=TDataManager.Choose(Self,nil,True);

  if tmp<>nil then
  begin
    AddDimension(ISummary.AddGroupBy(tmp));
    Recalculate;
  end;
end;

procedure TSummaryEditor.BRemoveDimClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LDimensions.ItemIndex;
  LDimensions.Items.Delete(tmp);
  ISummary.By.Delete(tmp);

  LDimensionsClick(Self);
  Recalculate;
end;

procedure TSummaryEditor.BRemoveMeasureClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LMeasures.ItemIndex;
  LMeasures.Items.Delete(tmp);
  ISummary.Measures.Delete(tmp);

  LMeasuresClick(Self);
  Recalculate;
end;

end.
