{*********************************************}
{  TeeBI Software Library                     }
{  Sub TChart plugin used by TBIChart         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Chart.Plugin;
{.$DEFINE FMX}

{$SCOPEDENUMS ON}

interface

uses
  BI.Arrays, BI.DataItem,
  System.Classes,
  Data.DB,

  {$IFDEF FMX}
  System.UITypes, FMX.Controls,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21_0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  FMXTee.Constants, FMXTee.Procs, FMXBI.DataControl, FMXBI.Grid,
  FMXTee.Engine, FMXTee.Chart
  {$ELSE}
  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.TeeProcs, VCLBI.DataControl, VCLBI.Grid,
  VCLTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Chart
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}
  ;

// XE6 dcc32 BUG, workaround not available
{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion>27}
  {$I BI.Chart.Options.inc} // <-- see .inc contents for reason/explanation
 {$ENDIF}
{$ENDIF}

const
  WhiteColor={$IFDEF FMX}TAlphaColors.White{$ELSE}clWhite{$ENDIF};

type
  TBIChartDirection=(Automatic, Rows, Columns);
  TBIChartStacked=(Automatic, No, Yes, Stacked100, Side, SideAll, SelfStack);

  TBITChart=class(TChart)
  private
    {$IFDEF TEEPRO}
    function TryAddUniqueTool(const AOwner:TComponent;
                              const AClass:TTeeCustomToolClass;
                              const AName:String):TTeeCustomTool;
    procedure TryDisableTool(const AClass:TTeeCustomToolClass; const ADisable:Boolean);
    {$ENDIF}

    function InitCountSeries(const ACount:TInteger):TChartSeries;
  protected
    FSeries2D : TChartSeriesClass;
    FSeries3D : TChartSeriesClass;

    DefaultXYSeries : TChartSeriesClass;

    LinePointer : Boolean;

    procedure ReadState(Reader: TReader); override;
  public

    class var
       FourItemSeries,
       Three3DSeries : TChartSeriesClass;

    Constructor Create(AOwner:TComponent); override;

    procedure AddLine(const Axis:TChartAxis);

    procedure ChangeSeries2D(const AClass: TChartSeriesClass;
                             const AHorizontal, ALinePointer: Boolean);

    procedure ClearTitles;

    procedure CreateMulti2D(const X,Y,Z:TDataItem; const ADirection:TBIChartDirection);

    procedure Fill(const AData:TDataArray; const ASeries: TDataItem); overload;
    function Fill(const AData:TInt32Array):TChartSeries; overload;
    function Fill(const AData:TInt64Array):TChartSeries; overload;
    function Fill(const AData:BI.Arrays.TDoubleArray):TChartSeries; overload;
    function Fill(const AData:TDataItem):TChartSeries; overload;
    procedure Fill(const AData:TDataSet; const ValueField:Integer; const TextField:Integer=-1); overload;

    function FillXY(const X,Y:TField):TChartSeries; overload;
    function FillXY(const AData:TDataSet; const X,Y:Integer):TChartSeries; overload;

    procedure FinishViewDimensions;

    Function GetParentComponent: TComponent; override;

    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;

    Function HasParent:Boolean; override;
    procedure Init(const FreeSeries:Boolean);
    function NewSeries(const AClass:TChartSeriesClass):TChartSeries; overload;
    function NewSeries(const Count:Integer):TChartSeries; overload;
    function NewSeries(const X,Y:String):TChartSeries; overload;

    class procedure SetAxesTitles(const ASeries:TChartSeries; const X,Y:String); static;

    procedure SetParentComponent(AParent: TComponent); override;
    procedure ShowHideAxesWalls(const AVisible:Boolean);

    class procedure TrySetName(const AComponent:TComponent; const APrefix:String); static;

    property Series2D:TChartSeriesClass read FSeries2D write FSeries2D;
    property Series3D:TChartSeriesClass read FSeries3D write FSeries3D;
  published
    property Align default TUICommon.AlignClient;
    property BevelOuter default bvNone;
    property Color default WhiteColor;
    property View3D default False;
  end;

  // Converts data from a Chart or one or more Series to a TDataItem
  TChartData=record
  private
    class procedure InitNotMandatory(const ASeries:TChartSeries;
                                     const ACount:Integer); static;

    class function NewSeries(const AOwner:TComponent;
                             const AClass:TChartSeriesClass):TChartSeries; static;
  public
    class procedure AddSeries(const ASeries: TChartSeries; const ADest: TDataItem); static;

    class function From(const AData:TDataArray;
                        const AOwner:TComponent;
                        const AClass:TChartSeriesClass=nil):TChartSeries; overload; static;

    class function From(const AData:TDataItem;
                        const AOwner:TComponent;
                        const AClass:TChartSeriesClass=nil):TChartSeries; overload; static;

    class function From(const ASeries:TChartSeries):TDataItem; overload; static;
    class function From(const AChart:TCustomChart):TDataItem; overload; static;
    class function From(const ASeries:Array of TChartSeries):TDataItem; overload; static;
  end;

implementation

uses
  System.SysUtils,

  {$IFDEF TEEPRO}
  {$IFDEF FMX}
  FMXTee.Tools,
  FMXTee.Series.Bubble, // <-- Bubble for FMX only in "Pro" TeeChart
  {$ELSE}
  VCLTee.TeeTools,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMX}
  FMXTee.Canvas, FMXTee.Series
  {$ELSE}
  VCLTee.TeCanvas, VCLTee.Series, VCLTee.BubbleCh
  {$ENDIF}
  ;

type
  TSeriesAccess=class(TChartSeries);

procedure FixDesignTime(const ASeries:TChartSeries);
begin
  if ASeries<>nil then
//     if csDesigning in ASeries.ComponentState then
     begin
       TSeriesAccess(ASeries).DontSerialize:=True;
       ASeries.ManualData:=True;
     end;
end;

{ TChartData }

procedure AddLabels(const ASeries:TChartSeries; const AData:TDataItem);
var t : Integer;
begin
  for t:=0 to ASeries.Count-1 do
      AData.TextData[t]:=ASeries.Labels[t];
end;

procedure CopyData(const AList:TChartValueList; const AData:TDataItem);
begin
  if AList.DateTime then
     AData.DateTimeData:=TDateTimeArray(AList.Value).Copy
  else
     AData.DoubleData:=BI.Arrays.TDoubleArray(AList.Value).Copy;
end;

type
  TDataAccess=class(TDataItem);

class procedure TChartData.AddSeries(const ASeries: TChartSeries; const ADest: TDataItem);

var t : Integer;
    tmp : TChartValueList;
    tmpContents : TeeFormatFlag;
    tmpPos : Integer;
begin
  // Initialize ADest
  ADest.Clear;

  ADest.AsTable:=True;

  // Set name
  ADest.Name:=ASeries.Title;

  if ADest.Name='' then
     ADest.Name:=ASeries.Name;

  tmpContents:=SeriesGuessContents(ASeries);

  // Add children Items
  for t:=0 to ASeries.ValuesList.Count-1 do
  begin
    tmp:=ASeries.ValuesList[t];

    if (tfNoMandatory in tmpContents) or (tmp<>ASeries.NotMandatoryValueList) then
       if tmp.DateTime then
          ADest.Items.Add(tmp.Name,TDataKind.dkDateTime)
       else
          ADest.Items.Add(tmp.Name,TDataKind.dkDouble);
  end;

  if tfLabel in tmpContents then
     ADest.Items.Add('Text',TDataKind.dkText);

  // Set ADest size
  ADest.Resize(ASeries.Count);

  tmpPos:=0;

  // Fill ADest data from ASeries
  for t:=0 to ASeries.ValuesList.Count-1 do
  begin
    tmp:=ASeries.ValuesList[t];

    if (tfNoMandatory in tmpContents) or (tmp<>ASeries.NotMandatoryValueList) then
    begin
      CopyData(tmp,ADest.Items[tmpPos]);
      Inc(tmpPos);
    end;
  end;

  if tfLabel in tmpContents then
     AddLabels(ASeries,ADest.Items[tmpPos]);
end;

class function TChartData.From(const ASeries: TChartSeries): TDataItem;
begin
  result:=TDataItem.Create(True);
  AddSeries(ASeries,result);
end;

class function TChartData.From(const AChart: TCustomChart): TDataItem;
var tmp : TChartSeries;
begin
  if AChart.SeriesCount=1 then
     result:=From(AChart[0])
  else
  begin
    result:=TDataItem.Create(False);

    result.Name:=AChart.Title.Caption;

    if result.Name='' then
       result.Name:=AChart.Name;

    for tmp in AChart.SeriesList do
        result.Items.Add(TChartData.From(tmp));
  end;
end;

class function TChartData.From(const ASeries: array of TChartSeries): TDataItem;
var tmp : TChartSeries;
begin
  if Length(ASeries)=1 then
     result:=From(ASeries[0])
  else
  begin
    result:=TDataItem.Create(False);

    for tmp in ASeries do
        result.Items.Add(TChartData.From(tmp));
  end;
end;

class procedure TChartData.InitNotMandatory(const ASeries:TChartSeries;
                                           const ACount:Integer);
var tmp : TChartValueList;
begin
  tmp:=ASeries.NotMandatoryValueList;

  tmp.Count:=ACount;
  SetLength(tmp.Value,ACount);
  tmp.FillSequence;
end;

class function TChartData.NewSeries(const AOwner: TComponent;
  const AClass: TChartSeriesClass): TChartSeries;
begin
  result:=AClass.Create(AOwner);
  result.Name:=GetNewSeriesName(AOwner);

  result.ManualData:=True;
  FixDesignTime(result);
end;

type
  TArrayToValues=record
  public
    class function From(const AData:TInt32Array):TChartValues; overload; static;
    class function From(const AData:TInt64Array):TChartValues; overload; static;
    class function From(const AData:TSingleArray):TChartValues; overload; static;
    class function From(const AData:TExtendedArray):TChartValues; overload; static;
    class function From(const AItem:TDataItem):TChartValues; overload; static;
  end;

class function TArrayToValues.From(const AData:TInt32Array):TChartValues;
var t : Integer;
begin
  SetLength(result,AData.Count);

  for t:=0 to AData.Count-1 do
      result[t]:=AData[t];
end;

class function TArrayToValues.From(const AData:TInt64Array):TChartValues;
var t : Integer;
begin
  SetLength(result,AData.Count);

  for t:=0 to AData.Count-1 do
      result[t]:=AData[t];
end;

class function TArrayToValues.From(const AData:TSingleArray):TChartValues;
var t : Integer;
begin
  SetLength(result,AData.Count);

  for t:=0 to AData.Count-1 do
      result[t]:=AData[t];
end;

class function TArrayToValues.From(const AData:TExtendedArray):TChartValues;
var t : Integer;
begin
  SetLength(result,AData.Count);

  for t:=0 to AData.Count-1 do
      result[t]:=AData[t];
end;

class function TArrayToValues.From(const AItem:TDataItem):TChartValues;
begin
  if AItem.Kind=TDataKind.dkDateTime then
     result:=TChartValues(AItem.DateTimeData.Copy)
  else
  if AItem.Kind=TDataKind.dkDouble then
     result:=TChartValues(AItem.DoubleData.Copy)
  else
  begin
    // Do the hard way
    case AItem.Kind of
        dkInt32: result:=From(AItem.Int32Data);
        dkInt64: result:=From(AItem.Int64Data);
       dkSingle: result:=From(AItem.SingleData);
     dkExtended: result:=From(AItem.ExtendedData);
    end;
  end;
end;

type
  TLabelsListAccess=class(TLabelsList);

class function TChartData.From(const AData: TDataArray;
                               const AOwner:TComponent;
                               const AClass:TChartSeriesClass): TChartSeries;

  procedure CopyData(const AList:TChartValueList; const AData:TDataItem);
  begin
    AList.Count:=AData.Count;
    AList.Value:=TArrayToValues.From(AData);
  end;

  function IsNumberOrDateTime(const AData:TDataItem):Boolean;
  begin
    result:=AData.Kind.IsNumeric or (AData.Kind=TDataKind.dkDateTime);
  end;

  function NumericItemsOf(const AData:TDataArray):Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to AData.Count-1 do
        if IsNumberOrDateTime(AData[t]) then
           Inc(result);
  end;

  procedure AddNullsText(const ASeries:TChartSeries; const AText:TTextArray);
  var t : Integer;
  begin
    {$IFDEF SERIESLABELSRESIZE}
    TLabelsListAccess(ASeries.Labels).Resize(AText.Count);

    for t:=0 to AText.Count-1 do
        ASeries.Labels[t]:=AText[t];

    {$ELSE}
    for t:=0 to AText.Count-1 do
        ASeries.AddNull(AText[t]);
    {$ENDIF}
  end;

  // Single numeric data item
  procedure AddXY(const ASeries:TChartSeries; const AData:TDataItem);
  begin
    // Initialize "X"
    InitNotMandatory(ASeries,AData.Count);

    // Copy "Y"
    CopyData(ASeries.MandatoryValueList,AData);
  end;

  // Single data item
  procedure AddSingleItem(const ASeries:TChartSeries; const AData:TDataItem);
  begin
    ASeries.Title:=AData.Name;

    if IsNumberOrDateTime(AData) then
       AddXY(ASeries,AData)
    else
    if AData.Kind=TDataKind.dkText then
       AddNullsText(ASeries,AData.TextData);
  end;

  function DoCreateSeries(const ANum:Integer):TChartSeries;
  begin
    if AClass=nil then
    begin
      if ANum<=2 then
         if AClass=nil then
            result:=TBarSeries.Create(AOwner)
         else
            result:=AClass.Create(AOwner)
      else
      {$IFDEF TEEPRO}
      if ANum=4 then
      begin
        if TBITChart.FourItemSeries=nil then
           result:=nil
        else
           result:=TBITChart.FourItemSeries.Create(AOwner)
      end
      else
      {$ENDIF}
      if ANum=3 then
      begin
        if TBITChart.Three3DSeries=nil then
           {$IFDEF FMX}
           result:=nil // No xyz series in TeeChart FMX "Lite"
           {$ELSE}
           result:=TBubbleSeries.Create(AOwner) // Alternative using Bubble
           {$ENDIF}
        else
           result:=TBITChart.Three3DSeries.Create(AOwner)
      end
      else
         result:=nil;
    end
    else
      result:=AClass.Create(AOwner);
  end;

  procedure AddLabels;
  var t : Integer;
      tmp : TDataItem;
  begin
    for t:=0 to AData.Count-1 do
    begin
      tmp:=AData[t];

      if tmp.Kind=TDataKind.dkText then
      begin
        AddNullsText(result,tmp.TextData);
        break;
      end;
    end;
  end;

  function FindValuesList(const ALists:TChartValueLists; const AName:String):TChartValueList;
  var t : Integer;
  begin
    for t:=0 to ALists.Count-1 do
        if SameText(ALists[t].Name,AName) then
           Exit(ALists[t]);

    result:=nil;
  end;

var
  tmpNum : Integer;

  procedure CopyValues;
  var t,
      tmpPos : Integer;

      tmp : TDataItem;
      tmpList : TChartValueList;
      tmpCount : Integer;
  begin
    if tmpNum<result.ValuesList.Count then
       tmpPos:=1
    else
       tmpPos:=0;  // start at X values

    tmpCount:=0;

    for t:=0 to AData.Count-1 do
    begin
      tmp:=AData[t];

      if IsNumberOrDateTime(tmp) then
      begin
        if tmpNum=1 then
           AddXY(result,tmp)
        else
        begin
          tmpList:=FindValuesList(result.ValuesList,tmp.Name);

          if tmpList=nil then
             tmpList:=result.ValuesList[tmpPos];

          CopyData(tmpList,tmp);

          if tmpList.Count>tmpCount then
             tmpCount:=tmpList.Count;
        end;

        Inc(tmpPos);
      end;
    end;

    if result.NotMandatoryValueList.Count=0 then
    begin
      InitNotMandatory(result,tmpCount);

      result.NotMandatoryValueList.DateTime:=False;
    end;
  end;

{$IFNDEF SERIESLABELSRESIZE}
var t,tt : Integer;
    tmp : TDataItem;
{$ENDIF}
begin
  tmpNum:=NumericItemsOf(AData);

  result:=DoCreateSeries(tmpNum);

  if result=nil then
     Exit; // DoError !

  if AData.Count=1 then
  begin
    AddSingleItem(result,AData[0]);
    result.ColorEachPoint:=True;
  end
  else
  begin
    AddLabels;
    CopyValues;

    {$IFNDEF SERIESLABELSRESIZE}
    for t:=0 to AData.Count-1 do
    begin
      tmp:=AData[t];

      if tmp.Kind=TDataKind.dkText then
      begin
        for tt:=0 to result.Count-1 do
            result.SetNull(tt,False);

        break;
      end;
    end;
    {$ENDIF}
  end
end;

class function TChartData.From(const AData:TDataItem;
                               const AOwner:TComponent;
                               const AClass:TChartSeriesClass=nil):TChartSeries;

  procedure DoError;
  begin
    raise EBIException.Create('Error: Cannot determine Series class from Data'+AData.Name);
  end;

var tmp : TDataArray;
begin
  AData.Load;

  if AData.AsTable then
     tmp:=AData.Items.AsArray
  else
  begin
    tmp:=nil;
    tmp.Add(AData);
  end;

  result:=From(tmp,AOwner,AClass);

  if result=nil then
     DoError;
end;

{ TBITChart }

Constructor TBITChart.Create(AOwner:TComponent);
begin
  inherited;

//  ControlStyle:=ControlStyle+[csNoDesignVisible];
//  ControlStyle:=ControlStyle-[csDesignInteractive,csPannable];

  // Not good: SetSubComponent(True);

  Align:=TUICommon.AlignClient;

  BevelOuter:=bvNone;
  View3D:=False;
  ColorPaletteIndex:=9;

  Gradient.Visible:=False;
  Color:=WhiteColor;

  Legend.Symbol.Shadow.Visible:=False; // Hide;
  Legend.Shadow.Visible:=False; //Hide;

  DefaultXYSeries:=TBarSeries;
end;

procedure TBITChart.AddLine(const Axis: TChartAxis);
{$IFDEF TEEPRO}
var tmpLine : TColorLineTool;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  tmpLine:=TColorLineTool.Create(Self);
  tmpLine.Axis:=Axis;
  tmpLine.Style:=TColorLineStyle.clMinimum;

  // Pending: Draggable axes splitters
  tmpLine.AllowDrag:=False;

  Tools.Add(tmpLine);
  {$ENDIF}
end;

procedure TBITChart.ChangeSeries2D(const AClass: TChartSeriesClass;
  const AHorizontal, ALinePointer: Boolean);
begin
  if AHorizontal then
     DefaultXYSeries:=THorizBarSeries
  else
     DefaultXYSeries:=TBarSeries;

  FSeries2D:=AClass;
  LinePointer:=ALinePointer;
end;

procedure TBITChart.ClearTitles;
var t : Integer;
begin
  for t:=0 to Axes.Count-1 do
  begin
    Axes[t].Title.Caption:='';
    Axes[t].Increment:=0;
  end;

  Title.Caption:='';
  Foot.Caption:='';
  Legend.Title.Caption:='';
end;

procedure TBITChart.Init(const FreeSeries:Boolean);

  {$IFDEF TEEPRO}
  procedure TryAddTips(const AOwner:TComponent);
  var tmp : TTeeCustomTool;
  begin
    tmp:=TryAddUniqueTool(AOwner,TMarksTipTool,'MarkTips');

    if tmp<>nil then
       (tmp as TMarksTipTool).Style:=TSeriesMarksStyle.smsLabelValue;
  end;

  procedure AddMarksTip;
  begin
    if ((Parent<>nil) and (not (csDesigning in Parent.ComponentState)))
       or
       ((Owner=nil) or (not (csLoading in Owner.ComponentState))) then
          if Owner=nil then
             TryAddTips(nil)
          else
             TryAddTips(Owner.Owner);
  end;
  {$ENDIF}

begin
  if FreeSeries then
     FreeAllSeries;

  ClearTitles;

  {$IFDEF TEEPRO}
  AddMarksTip;
  {$ENDIF}
end;

function TBITChart.NewSeries(const AClass:TChartSeriesClass):TChartSeries;
begin
  result:=TChartData.NewSeries(Owner,AClass);
  result.ParentChart:=Self;

  if result.InheritsFrom(TCustomSeries) then
  begin
    TCustomSeries(result).Pointer.Style:=psCircle;

    if result.InheritsFrom(TAreaSeries) then
       TAreaSeries(result).AreaLinesPen.Hide;
  end;

  if LinePointer and result.InheritsFrom(TLineSeries) then
     TCustomSeries(result).Pointer.Visible:=True;
end;

function TBITChart.FillXY(const X, Y: TField):TChartSeries;
begin
  result:=NewSeries(X.FieldName,Y.FieldName);

  result.XValues.ValueSource:=X.FieldName;
  result.YValues.ValueSource:=Y.FieldName;

  result.DataSource:=X.DataSet;
end;

function TBITChart.FillXY(const AData:TDataSet; const X,Y:Integer):TChartSeries;
var tmpX,
    tmpY : TFieldDef;
begin
  tmpX:=AData.FieldDefs[X];
  tmpY:=AData.FieldDefs[Y];

  result:=NewSeries(tmpX.Name,tmpY.Name);

  result.XValues.ValueSource:=tmpX.Name;
  result.YValues.ValueSource:=tmpY.Name;

  result.DataSource:=AData;
end;

procedure TBITChart.FinishViewDimensions;
begin
  if View3D and (not View3DOptions.Orthogonal) then
  begin
    {$IFDEF TEEPRO}
    TryAddUniqueTool(Owner.Owner,TRotateTool,'Rotate');
    {$ENDIF}

    Zoom.Allow:=False;
  end
  else
  begin
    {$IFDEF TEEPRO}
    TryDisableTool(TRotateTool,True);
    {$ENDIF}

    Zoom.Allow:=True;
  end;
end;

class procedure TBITChart.SetAxesTitles(const ASeries:TChartSeries; const X,Y:String);
begin
  if ASeries.YMandatory then
  begin
    ASeries.GetHorizAxis.Title.Caption:=X;
    ASeries.GetVertAxis.Title.Caption:=Y;
  end
  else
  begin
    ASeries.GetHorizAxis.Title.Caption:=Y;
    ASeries.GetVertAxis.Title.Caption:=X;
  end;
end;

function TBITChart.NewSeries(const X,Y:String):TChartSeries;
begin
  if FSeries2D=nil then
     result:=NewSeries(TBarSeries)
  else
     result:=NewSeries(FSeries2D);

  result.XValues.Order:=loNone;

  SetAxesTitles(result,X,Y);
end;

procedure TBITChart.ReadState(Reader: TReader);
begin
  inherited;

  if Reader.Parent is TBIDataControl then
     SetParentComponent(Reader.Parent);
end;

type
  TBIDataControlAccess=class(TBIDataControl);

procedure TBITChart.SetParentComponent(AParent: TComponent);
var tmp : TControl;
begin
  inherited;

  if Assigned(AParent) and (AParent is TBIDataControl) then
     if Parent<>AParent then
     begin
       tmp:=TBIDataControlAccess(AParent).ControlOfClass(TBITChart);

       if tmp<>Self then
          tmp.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};

       Parent:=TBIDataControl(AParent);
     end;
end;

procedure TBITChart.ShowHideAxesWalls(const AVisible:Boolean);
begin
  Axes.Visible:=AVisible;
  Walls.Back.Pen.Visible:=AVisible;
end;

procedure TBITChart.CreateMulti2D(const X,Y,Z:TDataItem; const ADirection:TBIChartDirection);
{
  procedure DoByRows;
  var t,tt : TLoopInteger;
      tmpS : TChartSeries;
  begin
    for t:=0 to Z.DataMap.Count-1 do
    begin
      tmpS:=NewSeries(AMain.Count);
      TSeriesAccess(tmpS).SetDefaultColor;
      tmpS.Title:=Z.DataMap.Items[t];

      tmpS.BeginUpdate;
      try
        for tt:=1 to AMain.Count-1 do
            tmpS.Add(GetValue(Items.Y[tt],t),Items.Y[tt].Name);
      finally
        tmpS.EndUpdate;
      end;
    end;
  end;

  procedure DoByColumns;
  var t,tt : TLoopInteger;
      tmpS : TChartSeries;
  begin
    for t:=1 to AMain.Count-1 do
    begin
      tmpS:=NewSeries(Z.DataMap.Count);
      TSeriesAccess(tmpS).SetDefaultColor;
      tmpS.Title:=Items.Y.DataToString(t);

      tmpS.BeginUpdate;
      try
        for tt:=0 to Z.DataMap.Count-1 do
            tmpS.Add(GetValue(AData[t],tt),Z.DataToString(tt));
      finally
        tmpS.EndUpdate;
      end;
    end;
  end;
end;
}
begin
  ClearTitles;

  {
  if ByRows then
     DoByRows
  else
     DoByColumns;
  }
end;

// Only at design-time. Do not set Name when loading
class procedure TBITChart.TrySetName(const AComponent:TComponent; const APrefix:String);
begin
  if AComponent<>nil then
     if (AComponent.Owner<>nil) and (csDesigning in AComponent.ComponentState) then
        AComponent.Name:=TeeGetUniqueName(AComponent.Owner,APrefix);
end;

{$IFDEF TEEPRO}
procedure TBITChart.TryDisableTool(const AClass:TTeeCustomToolClass; const ADisable:Boolean);
var t : Integer;
begin
  for t:=0 to Tools.Count-1 do
      if Tools[t].ClassType=AClass then
         Tools[t].Active:=not ADisable;
end;

function TBITChart.TryAddUniqueTool(const AOwner:TComponent;
                                     const AClass:TTeeCustomToolClass;
                                     const AName:String):TTeeCustomTool;

  function FoundInTools:TTeeCustomTool;
  var t : Integer;
  begin
    for t:=0 to Tools.Count-1 do
        if Tools[t].ClassType=AClass then
           Exit(Tools[t]);

    result:=nil;
  end;

begin
  result:=FoundInTools;

  if result<>nil then
     TryDisableTool(AClass,False)
  else
  begin
    result:=Tools.Add(AClass.Create(AOwner));
    TrySetName(result,AName);
  end;
end;
{$ENDIF}

function TBITChart.InitCountSeries(const ACount:TInteger):TChartSeries;
begin
  result:=NewSeries(ACount);
  SetAxesTitles(result,'','Count');
end;

function TBITChart.Fill(const AData: TInt32Array):TChartSeries;
var t : TLoopInteger;
begin
  result:=InitCountSeries(AData.Count);

  result.BeginUpdate;
  try
    for t:=0 to AData.Count-1 do
        result.Add(AData[t]);
  finally
    result.EndUpdate;
  end;
end;

function TBITChart.Fill(const AData: TInt64Array):TChartSeries;
var t : TLoopInteger;
begin
  result:=InitCountSeries(AData.Count);

  result.BeginUpdate;
  try
    for t:=0 to AData.Count-1 do
        result.Add(AData[t]);
  finally
    result.EndUpdate;
  end;
end;

function TBITChart.Fill(const AData: BI.Arrays.TDoubleArray):TChartSeries;
begin
  result:=InitCountSeries(AData.Count);

  TChartData.InitNotMandatory(result,AData.Count);
  result.MandatoryValueList.Value:=TChartValues(AData);
end;

function TBITChart.Fill(const AData: TDataItem):TChartSeries;
begin
  result:=TChartData.From(AData,Self,DefaultXYSeries);

  if result<>nil then
  begin
    AddSeries(result);

    if result.Title<>'' then
       Title.Caption:=result.Title;
  end;
end;

procedure TBITChart.Fill(const AData:TDataSet; const ValueField:Integer; const TextField:Integer=-1);
var tmpS : TChartSeries;
    tmpValue : TFieldDef;
    tmpX,
    tmpY : String;
begin
  if AData.Active then
  begin
    tmpS:=NewSeries(AData.RecordCount);

    tmpValue:=AData.FieldDefs[ValueField];

    tmpS.MandatoryValueList.ValueSource:=tmpValue.Name;
    tmpS.MandatoryValueList.DateTime:=tmpValue.DataType=ftDateTime;

    tmpY:=tmpValue.Name;

    if TextField<>-1 then
    begin
      tmpS.XLabelsSource:=AData.FieldDefs[TextField].Name;
      tmpX:=tmpS.XLabelsSource;
    end
    else
      tmpX:='';

    SetAxesTitles(tmpS,tmpX,tmpY);

    tmpS.DataSource:=AData;
  end;
end;

function TBITChart.NewSeries(const Count:Integer):TChartSeries;
begin
  if FSeries2D=nil then
  begin
    if Count<100 then
    begin
      result:=NewSeries(DefaultXYSeries);
      result.Marks.Style:=smsValue;
    end
    else
      result:=NewSeries(TFastLineSeries);
  end
  else
    result:=NewSeries(FSeries2D);
end;

procedure TBITChart.Fill(const AData:TDataArray; const ASeries: TDataItem);

  function MapToString(const AIndex:TLoopInteger):String;
  begin
    if ASeries.Kind=TDataKind.dkUnknown then
       result:=''
    else
       result:=ASeries.DataMap.AsString(AIndex);
  end;

var t,
    Dim : Integer;
    S : TChartSeries;
    tmp : TNativeInteger;
begin
  // Pending:
  // Check ASeries<>nil, AData.Count>0
  // What to do with 3 or more AData items? (3D ?)

  ASeries.Stats;

  for t:=0 to ASeries.DataMap.Count-1 do
  begin
    S:=NewSeries(TPointSeries);
    S.Title:=MapToString(t);
    (S as TPointSeries).Pointer.Style:=psCircle;
  end;

  Legend.Title.Caption:=ASeries.Name;

  Dim:=AData.Count;

  for t:=0 to AData[0].Count-1 do
      if ASeries.FindInMap(t,tmp) then
         if Dim=1 then
            Series[tmp].Add(GetValue(AData[0],t))
         else
            Series[tmp].AddXY(GetValue(AData[0],t),GetValue(AData[1],t));

  if Dim=1 then
  begin
    Axes.Bottom.Title.Caption:='';
    Axes.Left.Title.Caption:=AData[0].Name;
  end
  else
  begin
    Axes.Bottom.Title.Caption:=AData[0].Name;
    Axes.Left.Title.Caption:=AData[1].Name;
  end;

  if ASeries.Parent=nil then
     Title.Caption:=''
  else
     Title.Caption:=ASeries.Parent.Name;
end;

function TBITChart.GetParentComponent: TComponent;
begin
  result:=Parent;
end;

class function TBITChart.GetValue(const AData:TDataItem; const Index:TInteger):TChartValue;
begin
  case AData.Kind of
       dkInt32: result:=AData.Int32Data[Index];
       dkInt64: result:=AData.Int64Data[Index];
      dkSingle: result:=AData.SingleData[Index];
      dkDouble: result:=AData.DoubleData[Index];
    dkExtended: result:=AData.ExtendedData[Index];
    dkDateTime: result:=AData.DateTimeData[Index];
  else
    result:=0;
  end;
end;

function TBITChart.HasParent: Boolean;
begin
  result:=True;
end;

initialization
  TeeActivateGroup;
  RegisterClass(TBITChart);
  RegisterTeeStandardSeries;
finalization
  TeeActivateGroup;
  UnRegisterClass(TBITChart);
end.
