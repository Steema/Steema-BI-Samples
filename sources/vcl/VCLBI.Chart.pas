{*********************************************}
{  TeeBI Software Library                     }
{  TChart output                              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Chart;
{.$DEFINE FMX}

{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.SysUtils, Data.DB,

  {$IFDEF FMX}
  FMX.Controls,
  FMXTee.Constants, FMXTee.Procs, FMXBI.DataControl, FMXBI.Grid,
  {$ELSE}
  Winapi.Messages,
  VCLTee.TeeConst, VCLTee.TeeProcs, VCLBI.DataControl, VCLBI.Grid,
  VCLTee.TeeGDIPlus,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMX}
  System.UITypes, FMXTee.Chart, FMXTee.Engine, FMXTee.Series,
  FMXBI.Chart.Plugin,

  {$IFDEF TEEPRO}
  FMXBI.Chart.Geo,
  {$ENDIF}

  {$ELSE}

  VCL.Graphics, VCL.Controls, VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series,
  VCLBI.Chart.Plugin,

  {$IFDEF TEEPRO}
  VCLBI.Chart.Geo,
  {$ENDIF}

  {$ENDIF}

  BI.DataItem, BI.Arrays, BI.Summary, BI.DataSource, BI.Info;

type
  TBIChartItems=class(TPersistent)
  private
    IX,
    IZ,
    IText,
    IGroup,
    IColors : String;

    IY : TTextArray;

    {$IFDEF TEEPRO}
    FGeoContext : TGeoContext;
    {$ENDIF}

    procedure ExchangeXY;
    procedure ExchangeXZ;
    procedure ExchangeYZ;
    procedure Guess(const AData:TDataArray);
    procedure GetXYZ(out AX,AY,AZ:TDataItem);
    procedure MoveXToY;

    procedure ReadColors(Reader: TReader);
    procedure ReadGroup(Reader: TReader);
    procedure ReadText(Reader: TReader);
    procedure ReadX(Reader: TReader);
    procedure ReadY(Reader: TReader);
    procedure ReadZ(Reader: TReader);
    procedure WriteColors(Writer: TWriter);
    procedure WriteGroup(Writer: TWriter);
    procedure WriteText(Writer: TWriter);
    procedure WriteX(Writer: TWriter);
    procedure WriteY(Writer: TWriter);
    procedure WriteZ(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded(const AData:TDataArray);
  public
    X,
    Z,
    Text,
    Colors,
    Group : TDataItem;

    Y : TDataArray;

    procedure Clear;

    function CanFinancial:Boolean;
    function CanGeoGraphic:Boolean;
    function CanThreeD:Boolean;
    function CanXYZ:Boolean;
  end;

  {$IFNDEF FPC}
  TGetText=TFunc<Integer,String>;
  {$ENDIF}

  TBIChartMode=(Automatic, XY, ThreeD, Financial, Geographic);
  TBIChart3DMode=(Automatic, Table, Grid, XYZ);
  TBIChartDimensions=(Automatic, View2D, Orthogonal, View3D);
  TBIChartLegend=(Automatic, Show, Hide);
  TBIChartMarks=(Automatic, Show, Hide);
  TBISeriesDirection=(Automatic, Horizontal, Vertical);

  TBIChart=class;

  TBIChartOptions=class(TPersistent)
  private
    FDimensions: TBIChartDimensions;
    FDirection : TBIChartDirection;
    FItems : TBIChartItems;
    FLegend: TBIChartLegend;
    FMarks: TBIChartMarks;
    FMode: TBIChartMode;
    FSeriesDirection: TBISeriesDirection;
    FStacked: TBIChartStacked;
    FXYZMode: TBIChart3DMode;

    IChart : TBIChart;

    procedure Finish;
    procedure FinishViewDimensions;
    function GetSeries2D: TChartSeriesClass;
    function GetSeries3D: TChartSeriesClass;
    procedure SetDimensions(const Value: TBIChartDimensions);
    procedure SetDirection(const Value: TBIChartDirection);
    procedure SetItems(const Value: TBIChartItems);
    procedure SetLegend(const Value: TBIChartLegend);
    procedure SetMarks(const Value: TBIChartMarks);
    procedure SetMode(const Value: TBIChartMode);
    procedure SetSeries2D(const Value: TChartSeriesClass);
    procedure SetSeries3D(const Value: TChartSeriesClass);
    procedure SetStacked(const Value: TBIChartStacked);
    procedure SetXYZMode(const Value: TBIChart3DMode);
    procedure SetSeriesDirection(const Value: TBISeriesDirection);
  public
    Constructor Create(const AChart:TBIChart);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure ChangeSeries2D(const AClass:TChartSeriesClass;
                             const AHorizontal,ALinePointer:Boolean);

    property Series2D:TChartSeriesClass read GetSeries2D write SetSeries2D;
    property Series3D:TChartSeriesClass read GetSeries3D write SetSeries3D;
  published
    property Dimensions:TBIChartDimensions read FDimensions write SetDimensions
                        default TBIChartDimensions.Automatic;

    property Direction:TBIChartDirection read FDirection write SetDirection default TBIChartDirection.Automatic;
    property Items:TBIChartItems read FItems write SetItems;
    property Legend:TBIChartLegend read FLegend write SetLegend
                        default TBIChartLegend.Automatic;
    property Marks:TBIChartMarks read FMarks write SetMarks
                        default TBIChartMarks.Automatic;
    property Mode:TBIChartMode read FMode write SetMode default TBIChartMode.Automatic;
    property SeriesDirection: TBISeriesDirection read FSeriesDirection write SetSeriesDirection
                          default TBISeriesDirection.Automatic;
    property Stacked:TBIChartStacked read FStacked write SetStacked
                        default TBIChartStacked.Automatic;
    property XYZMode:TBIChart3DMode read FXYZMode write SetXYZMode default TBIChart3DMode.Automatic;
  end;

  {$IF Defined(CompilerVersion)}
   {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
   {$ENDIF}
  {$ENDIF}
  TBIChart=class(TBIDataControl)
  private
    FOptions : TBIChartOptions;

    IDataInfo : TDataInfo;
    IDirtyData : TDataItem;

    procedure AddXY(const AItems:TBIChartItems);

    procedure ApplyData(const AData:TDataItem);
    procedure CreateChart;
    function CreateSeries(const X,Y:TDataItem):TChartSeries;
    function DirectChart:TBITChart;
    procedure FillSeries(const ASeries:TChartSeries; const X,Y,AText:TDataItem);
    function DoFreeSeries:Boolean;
    function GetChart:TBITChart;
    function GuessRealData(const AData:TDataArray):TDataArray;
    function IsDesigning:Boolean;
    procedure SetOptions(const Value: TBIChartOptions);
  protected
    Index : TCursorIndex;

    RealMode : TBIChartMode;
    RealMode3D : TBIChart3DMode;

    procedure ExchangeXY;
    procedure ExchangeYZ;
    procedure ExchangeXZ;

    procedure DirectRefresh;
    procedure Loaded; override;

    {$IFDEF FMX}
    procedure Paint; override;
    {$ELSE}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}

    procedure ReadState(Reader: TReader); override;
    procedure ResetTheme;
    procedure SetDataDirect(const Value: TDataItem); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Clear;

    {$IFNDEF FPC}
    procedure Fill(const Map:TDataMap; const Text:TGetText=nil); overload;
    {$ENDIF}

    procedure Fill(const AData:TDataArray); overload;
    procedure Fill(const AItems:TDataItems); overload;
    procedure Fill(const AHistogram:THistogram; const ASource:TDataItem); overload;
    procedure Fill(const ASummary:TSummary); overload;
    procedure Fill(const ACursor:TDataCursor; const AItems:TDataArray=nil); overload;

    function GetChildOwner: TComponent; override;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;

    procedure SetTwoAxes;
    procedure SplitAxes(const Vertical:Boolean);
  published
    property Chart:TBITChart read GetChart;

    {$IFNDEF FMX}
    property Height default 250;
    property Width default 400;
    {$ENDIF}

    property Options:TBIChartOptions read FOptions write SetOptions;
  end;

implementation

uses
  {$IFDEF FMX}
  FMXTee.Canvas,

  {$IFDEF TEEPRO}
  FMXBI.Chart.Financial, FMXBI.Chart.ThreeD,
  {$ENDIF}

  {$ELSE}
  VCLTee.TeCanvas,

  {$IFDEF TEEPRO}
  VCLBI.Chart.Financial, VCLBI.Chart.ThreeD,
  {$ENDIF}

  {$ENDIF}

  BI.Persist, BI.Store.Component, BI.Geographic;

function MapCount(const AData:TDataItem):TInteger;
begin
  if AData=nil then
     result:=0
  else
  begin
    AData.Load;
    AData.Stats;

    if AData.DataMap=nil then
       result:=0
    else
       result:=AData.DataMap.Count;
  end;
end;

type
  TDataItemAccess=class(TDataItem);

function CanBeMeasure(const ANumeric:TDataItem):Boolean;
begin
  result:=(not ANumeric.Primary) and
         //(not TDataItemAccess(ANumeric).HasMaster) and
         (not TDataItemAccess(ANumeric).IHasDate) and
         (not ANumeric.Missing.All);

//  if result then
//     result:=(ANumeric.Count=1) or (MapCount(ANumeric)>1);
end;

function IsNumericOrDateTime(const AData:TDataItem):Boolean;
begin
  if AData=nil then
     result:=False
  else
  if AData.AsTable and (AData.Items.Count>0) then
     result:=IsNumericOrDateTime(AData.Items[0])
  else
     result:=AData.Kind.IsNumeric or (AData.Kind=TDataKind.dkDateTime);
end;

function IsMeasure(const AData:TDataItem):Boolean;
begin
  result:=IsNumericOrDateTime(AData) and (AData.AsTable or CanBeMeasure(AData));
end;

function LastUniqueTable(const AData:TDataItem):TDataItem;
begin
  result:=AData;

  while (result.Items.Count=1) and result.Items[0].AsTable do
        result:=result.Items[0];
end;

function HasMeasures(const AData:TDataItem):Boolean;
begin
  result:=IsMeasure(LastUniqueTable(AData));
end;

{ TBIChartItems }

procedure TBIChartItems.GetXYZ(out AX, AY, AZ: TDataItem);
begin
  if X=nil then
  begin
    if Z=nil then
    begin
      AX:=Y[0];
      AY:=Y[1];
      AZ:=Y[2];
    end
    else
    begin
      AX:=Y[0];
      AY:=Y[1];
      AZ:=Z;
    end;
  end
  else
  if Z=nil then
  begin
    AX:=X;
    AY:=Y[1];
    AZ:=Y[2];
  end
  else
  begin
    AX:=X;
    AY:=Y[0];
    AZ:=Z;
  end;
end;

procedure TBIChartItems.Guess(const AData:TDataArray);

  function AnyOfKind(const AData:TDataArray; const AKind:TDataKind):TDataItem;
  var tmp : TDataItem;
  begin
    for tmp in AData do
        if tmp.AsTable then
        begin
          result:=AnyOfKind(tmp.Items.AsArray,AKind);

          if result<>nil then
             Exit;
        end
        else
        if tmp.Kind=AKind then
           Exit(tmp);

    result:=nil;
  end;

  procedure SwapItems(var A,B:TDataItem);
  var tmp : TDataItem;
  begin
    tmp:=A;
    A:=B;
    B:=tmp;
  end;

  procedure GetXY(out X,Y:TDataItem);

    function ExistsBetterY(const AFrom:Integer):Boolean;
    var t : Integer;
        tmp : TDataItem;
    begin
      for t:=AFrom to High(AData) do
      begin
        tmp:=AData[t];

        if IsMeasure(tmp) then
           if tmp.AsTable or (MapCount(tmp)>MapCount(Y)) then
              Exit(True);
      end;

      result:=False;
    end;

  var t : Integer;
      tmp : TDataItem;
      tmpLinked : TDataItem;
  begin
    X:=nil;
    Y:=nil;

    for t:=0 to High(AData) do
    begin
      tmp:=AData[t];

      tmpLinked:=TGeo.LinkedTo(tmp);

      if tmpLinked<>nil then
      begin
        X:=tmp;
      end
      else
      if IsNumericOrDateTime(tmp) then
      begin
        if (Y=nil) and CanBeMeasure(tmp) then
           Y:=tmp
        else
        if X=nil then
        begin
          if MapCount(tmp)>1 then
          begin
            if (Y<>nil) and TDataItemAccess(Y).HasMaster then
            begin
              X:=Y;
              Y:=tmp;
            end
            {
            // Do not set X here, (too early)
            else
               X:=tmp};
          end;
        end;

        if (X<>nil) and (Y<>nil) then
        begin
          if X.AsTable and (not Y.AsTable) then
             SwapItems(X,Y)
          else
          begin
            X.Stats;

            if (X.DataMap<>nil) and (X.DataMap.Sorted=TDataOrder.None) then
            begin
              Y.Stats;

              if (Y.DataMap<>nil) and (Y.DataMap.Sorted<>TDataOrder.None) then
                 SwapItems(X,Y);
            end;
          end;

          if ExistsBetterY(t+1) then
             Y:=nil
          else
             break; // Z ??
        end;
      end;
    end;

    (* Breaks Geo guess !
    // Hardcoded minimum of 10 points to use XY
    if X<>nil then
       if X.Count<10 then
          X:=nil;
    *)
  end;

  function TryGetAnyNot(const Y:TDataArray):TDataItem;
  var tmp : TDataItem;
  begin
    for tmp in AData do
        if tmp.Kind<>TDataKind.dkUnknown then
           if not Y.Exists(tmp) then
              if tmp.Count>1 then // <-- at least 2 rows
                 Exit(tmp);

    result:=nil;
  end;

  procedure TryAddOtherY;
  var tmp : TDataItem;
  begin
    for tmp in AData do
      if (tmp<>X) and (tmp<>Text) then
         if not Y.Exists(tmp) then
            if HasMeasures(tmp) then
            begin
              if tmp.AsTable then
                 if (Text=nil) and (Y.Count=1) then
                 begin
                   Text:=Y[0];
                   Y:=nil;
                 end;

              Y.Add(tmp);
            end;
  end;

  procedure TryFindX;
  var tmp : TDataItem;
  begin
    if Y.Count=2 then
    begin
      if SameText(Y[0].Name,'X') and
         SameText(Y[1].Name,'Y') then
      begin
        X:=Y[0];
        Y.Delete(0);
      end
      else
      if SameText(Y[1].Name,'X') and
         SameText(Y[0].Name,'Y') then
      begin
        X:=Y[1];
        Y.Delete(1);
      end;
    end
    else
    if Y.Count=1 then
    begin
      tmp:=TryGetAnyNot(Y);

      if tmp<>nil then
      begin
        if SameText(Y[0].Name,'X') and
           SameText(tmp.Name,'Y') then
        begin
          X:=Y[0];
          Y[0]:=tmp;
        end
        else
        if SameText(Y[0].Name,'Y') and
           SameText(tmp.Name,'X') then
             X:=tmp;
      end;
    end;
  end;

var tmpY : TDataItem;
begin
  GetXY(X,tmpY);

  if tmpY<>nil then
     Y.Add(tmpY);

  if X=nil then
     TryFindX;

  Text:=AnyOfKind(AData,dkText);

  if (Text=nil) and (X=nil) then
     Text:=TryGetAnyNot(Y);

  if Text=nil then
     Text:=AnyOfKind(AData,dkBoolean);

  TryAddOtherY;

  if X<>nil then
     if Y.Count>1 then
        Z:=Y[1];
end;

procedure TBIChartItems.Loaded(const AData:TDataArray);

  // Naive search
  function FindData(const AName:String):TDataItem;
  var t : Integer;
  begin
    if AName<>'' then
       for t:=0 to AData.Count-1 do
           if SameText(AData[t].Name,AName) then
              Exit(AData[t]);

    result:=nil;
  end;

  procedure TryAddY(const AData:TDataItem);
  var L : Integer;
  begin
    if AData=nil then
       // raise !
    else
    begin
      L:=Length(Y);
      SetLength(Y,L+1);
      Y[L]:=AData;
    end;
  end;

var t,
    L : Integer;
begin
  X:=FindData(IX);
  Z:=FindData(IZ);
  Text:=FindData(IText);
  Group:=FindData(IGroup);
  Colors:=FindData(IColors);

  if IY<>nil then
  begin
    Y:=nil;

    L:=Length(IY);

    for t:=0 to L-1 do
        TryAddY(FindData(IY[t]));
  end;
end;

// Move back X to Y
procedure TBIChartItems.MoveXToY;
begin
  if X<>nil then
  begin
    Y.Insert(X,1);
    X:=nil;
  end;
end;

{ TBIChart }

function TBIChart.DoFreeSeries:Boolean;
begin
  case RealMode of
    TBIChartMode.XY: result:=True;

  {$IFDEF TEEPRO}
       TBIChartMode.ThreeD: result:=not TThreeDChart.CanReuse(Chart);
    TBIChartMode.Financial: result:=not TFinancialChart.CanReuse(Chart);
   TBIChartMode.Geographic: result:=not TGeoChart.CanReuse(Chart,Options.Items.FGeoContext);
  {$ENDIF}
  else
    result:=True;
  end;
end;

procedure TBIChart.Clear;
begin
  Chart.Init(DoFreeSeries);

  Chart.Legend.Visible:=Options.Legend<>TBIChartLegend.Hide;
end;

Constructor TBIChart.Create(AOwner: TComponent);
begin
  inherited;

  FOptions:=TBIChartOptions.Create(Self);

  Height:=250;
  Width:=400;

  {$IFNDEF FMX}
  {$IFNDEF FPC}
  BevelOuter:=bvNone;
  {$ENDIF}
  {$ENDIF}

  if IsDesigning then
  begin
    {$IFNDEF FPC}
    Padding.Top:=24; // <-- temporary workaround to enable mouse drag of TBIChart at design-time
    {$ENDIF}

    if (not Assigned(Owner)) or (not (csLoading in Owner.ComponentState)) then
       CreateChart;
  end;
end;

Destructor TBIChart.Destroy;
begin
  IDataInfo.Free;
  FOptions.Free;
  inherited;
end;

function TBIChart.IsDesigning:Boolean;
begin
  result:=csDesigning in ComponentState;
end;

const
  LightSilver={$IFDEF FMX}$FFE6E6E6{$ELSE}$E6E6E6{$ENDIF};

procedure TBIChart.ResetTheme;
var t : Integer;
begin
  Chart.Walls.Hide;

  Chart.Legend.Transparent:=True;

  for t:=0 to Chart.Axes.Count-1 do
      Chart.Axes[t].Grid.Color:=LightSilver;
end;

{ Not a good solution to avoid design-time dragging of Chart inside BIChart
type
  TComponentAccess=class(TComponent);

procedure TBIChart.FixDesigntime(const AComponent:TComponent);
begin
  TComponentAccess(AComponent).SetDesigning(False);
end;
}

procedure TBIChart.CreateChart;
var tmp : TBITChart;
begin
  tmp:=TBITChart.Create(Owner);
  tmp.Parent:=Self;

  TBITChart.TrySetName(tmp,'BITChart');

  tmp.Init(True);
end;

function TBIChart.GetChildOwner: TComponent;
begin
  result:=Owner;
end;

function TBIChart.DirectChart:TBITChart;
var tmp : TControl;
begin
  tmp:=ControlOfClass(TBITChart);

  if tmp=nil then
     result:=nil
  else
     result:=tmp as TBITChart;
end;

Procedure TBIChart.GetChildren(Proc:TGetChildProc; Root:TComponent);
begin
  inherited;

  if DirectChart<>nil then
     if (Root=Self) and ((DirectChart.Owner<>Root) or (DirectChart.Owner=nil)) then
        Proc(DirectChart);
end;

procedure TBIChart.ApplyData(const AData:TDataItem);
var tmp : TDataArray;
begin
  if AData=nil then
     Options.FItems.Clear
  else
  begin
    AData.Load;

    SetLength(tmp,1);
    tmp[0]:=AData;
    Fill(tmp);

    Chart.Title.Caption:=AData.Name;
  end;
end;

procedure TBIChart.Assign(Source: TPersistent);
begin
  if Source is TBIChart then
     Options.Assign(TBIChart(Source).Options);

  inherited;
end;

{$IFDEF FMX}
procedure TBIChart.Paint;
{$ELSE}
procedure TBIChart.WMPaint(var Message: TWMPaint);
{$ENDIF}
begin
  if IDirtyData<>nil then
  begin
    Chart.Init(DoFreeSeries);

    if not (csLoading in ComponentState) then
       ApplyData(IDirtyData);

    IDirtyData:=nil;
  end;

  inherited;
end;

procedure TBIChart.SetDataDirect(const Value: TDataItem);
begin
  IDirtyData:=Value;

  {$IFDEF FMX}
  Repaint;
  {$ELSE}
  Invalidate;
  {$ENDIF}
end;

procedure TBIChart.SetOptions(const Value: TBIChartOptions);
begin
  FOptions.Assign(Value);
end;

function TBIChart.GetChart:TBITChart;
begin
  if DirectChart=nil then
     CreateChart;

  result:=DirectChart;
end;

procedure TBIChart.Loaded;
var tmp : TDataArray;
begin
  if Data<>nil then
  begin
    SetLength(tmp,1);
    tmp[0]:=Data;

    Options.Items.Loaded(GuessRealData(tmp));
  end;

  inherited;

  SetDataDirect(Data);

  // Pending: Try to re-link existing Series with Items properties,
  // to reuse them instead of destroying and recreating the series

  if not IsDesigning then
     {$IFNDEF FPC}
     Padding.Top:=0; // <-- temporary workaround to enable mouse drag of TBIChart at design-time
     {$ENDIF}
end;

procedure TBIChart.ReadState(Reader: TReader);
var tmpChart : TChart;
begin
  tmpChart:=DirectChart;

  if Assigned(tmpChart) and
     (not (csLoading in tmpChart.ComponentState))
     and (not (csAncestor in tmpChart.ComponentState)) then
       tmpChart.Free;

  inherited;

  Chart;

  {
  if Chart<>nil then
     FixDesigntime(Chart);
  }
end;

procedure AssociateSeries(const ASeries:TChartSeries; const AData:TDataItem);
begin
  {$IFDEF HASTAGOBJECT}
  ASeries.TagObject:=AData;
  {$ELSE}
  ASeries.Tag:=ObjectToTag(AData);
  {$ENDIF}
end;

function TBIChart.CreateSeries(const X,Y:TDataItem):TChartSeries;
begin
  result:=nil;

  if X=nil then
  begin
    if Y=nil then
       Exit
    else
    begin
      result:=Chart.NewSeries(Y.Count);
      result.Title:=Y.Name;

      AssociateSeries(result,Y);
    end;

    {$IFDEF TEEPRO}
    result.GetHorizAxis.Texts.Style:=talAuto;
    {$ELSE}
    result.GetHorizAxis.LabelStyle:=talAuto;
    {$ENDIF}
  end
  else
  if Y<>nil then
  begin
    result:=Chart.NewSeries(X.Name,Y.Name);
    result.Title:=Y.Name;

    result.NotMandatoryValueList.DateTime:=X.Kind=dkDateTime;

    AssociateSeries(result,Y);

    {$IFDEF TEEPRO}
    result.GetHorizAxis.Texts.Style:=talValue;
    {$ELSE}
    result.GetHorizAxis.LabelStyle:=talValue;
    {$ENDIF}
  end;
end;

//type
//  TDataItemAccess=class(TDataItem);

procedure TBIChart.DirectRefresh;

  {$IFDEF TEEPRO}
  (*
  procedure AddCursorTool;
  var tmp : TCursorTool;
  begin
    tmp:=TCursorTool.Create(Owner);
    tmp.Style:=TCursorToolStyle.cssVertical;
    tmp.Pen.Color:=OperaPalette[4];
    tmp.Pen.Width:=2;
    tmp.FollowMouse:=True;

    Tools.Add(tmp);
  end;
  *)
  {$ENDIF}

  // We only have text data. Nothing numeric.
  // So, chart an histogram (distribution frequency)
  procedure AddText(const AText:TDataItem);
  var tmpS: TSummary;
      tmpBy : TGroupBy;
      tmpD : TDataItem;
  begin
    tmpS:=TSummary.Create(nil);
    try
      tmpS.Measures.Add(AText,TAggregate.Count);
      tmpBy:=tmpS.By.Add(AText);

      tmpBy.Histogram.NumBins:=9;  // ABC DEF GHI JKL MNO PQR STU VWX YZ
      tmpBy.Histogram.Active:=True;

      tmpD:=tmpS.Calculate;

      Options.Items.Guess(tmpD.Items.AsArray);

      if Options.Items.Y<>nil then
         AddXY(Options.Items);

      Options.Items.Clear;
    finally
      tmpS.Free;
    end;
  end;

  function MeasuresOf(const AItems:TDataArray):Integer;
  var tmp : TDataItem;
  begin
    result:=0;

    for tmp in AItems do
        if IsMeasure(tmp) then
             Inc(result);
  end;

  function LastSubItemsOf(const AData:TDataItem):TDataArray;
  var tmp : TDataItem;
  begin
    tmp:=AData;

    while tmp.AsTable do
       if (tmp.Items.Count=1) and tmp.Items[0].AsTable then
          tmp:=tmp.Items[0]
       else
          break;

    result:=tmp.Items.AsArray;
  end;

  function GuessRealMode:TBIChartMode;
  begin
    {$IFDEF TEEPRO}
    if Options.Items.CanGeographic then // <-- Try first Geographic, then others
       result:=TBIChartMode.Geographic
    else
    if Options.Items.CanFinancial then
       result:=TBIChartMode.Financial
    else
    if Options.Items.CanThreeD and (Data<>nil) and (Data.Count>32) then // min rows to consider XYZ
       result:=TBIChartMode.ThreeD
    else
    {$ENDIF}
       result:=TBIChartMode.XY;
  end;

  {$IFDEF TEEPRO}
  procedure TryGrid3D;
  var tmpX,
      tmpY,
      tmpZ : TDataItem;
  begin
    if Options.Items.Y.Count>3 then
       TThreeDChart.CreateGridTable(Chart,Options.Items.Y,Options.FDirection)
    else
    begin
      tmpX:=Options.Items.X;

      if tmpX=nil then
         tmpX:=Options.Items.Text;

      if Options.Items.Y<>nil then
         tmpY:=Options.Items.Y[0]
      else
         tmpY:=nil;

      tmpZ:=Options.Items.Z;

      if tmpZ=nil then
         if Options.Items.Y.Count>1 then
            tmpZ:=Options.Items.Y[1];

      if (tmpX<>nil) and (tmpY<>nil) and (tmpZ<>nil) then
         if Options.Items.CanXYZ then
            TThreeDChart.CreateGrid3D(Chart,tmpX,tmpY,tmpZ,Options.Direction)
         else
            Chart.CreateMulti2D(tmpX,tmpY,tmpZ,Options.Direction)
    end;
  end;

  procedure TryXYZ;
  var X,Y,Z : TDataItem;
  begin
    Options.Items.GetXYZ(X,Y,Z);
    TThreeDChart.CreateXYZ(Chart,X,Y,Z);
  end;

  procedure TryGeo;
  var tmpY,
      tmpText : TDataItem;
  begin
    if Options.Items.Y.Count>0 then
       tmpY:=Options.Items.Y[0]  // multiple Geo values on multiple series???
    else
       tmpY:=nil;

    tmpText:=Options.Items.Text;

    if tmpText=nil then
       tmpText:=Options.Items.X
    else
    if tmpY=nil then
       tmpY:=Options.Items.X;

    TGeoChart.Fill(Chart,tmpY,tmpText,Options.Items.FGeoContext);
  end;
  {$ENDIF}

  procedure CalcRealMode3D;
  begin
    if Options.XYZMode=TBIChart3DMode.Automatic then
    begin
      if Options.Items.CanXYZ then
         if Options.Items.Y.Count>3 then
            RealMode3D:=TBIChart3DMode.Table // <-- priority is Table3D
         else
         if Options.Items.Y[0].Count<3 then
            RealMode3D:=TBIChart3DMode.Grid // <-- priority is Grid3D
         else
            RealMode3D:=TBIChart3DMode.XYZ
      else
         RealMode3D:=TBIChart3DMode.Grid;

      if RealMode3D=TBIChart3DMode.Grid then
         Options.Items.MoveXToY;
    end
    else
      RealMode3D:=Options.XYZMode;
  end;

  procedure CalcRealMode;
  begin
    RealMode:=TBIChartMode.Automatic;

    case Options.Mode of
     TBIChartMode.Automatic: RealMode:=GuessRealMode;

     TBIChartMode.Financial: if Options.Items.CanFinancial then
                                RealMode:=TBIChartMode.Financial;

        TBIChartMode.ThreeD: if Options.Items.CanThreeD then
                                RealMode:=TBIChartMode.ThreeD;

    TBIChartMode.Geographic: if Options.Items.CanGeographic then
                                RealMode:=TBIChartMode.Geographic;
    else
      RealMode:=Options.Mode;
    end;
  end;

var Old : String;
begin
  if csLoading in ComponentState then
     Exit;

  CalcRealMode;

  Old:=Chart.Title.Caption;
  Clear;
  Chart.Title.Caption:=Old;

  Chart.View3DOptions.Orthogonal:=True;

  Chart.ShowHideAxesWalls(True);

  {$IFDEF TEEPRO}
  if RealMode=TBIChartMode.Financial then
  begin
    Chart.ClearTitles;
    TFinancialChart.Fill(Chart,Options.Items.Y,Options.Items.X);

    if Options.Legend=TBIChartLegend.Automatic then
       Chart.Legend.Hide;
  end
  else
  if RealMode=TBIChartMode.ThreeD then
  begin
    CalcRealMode3D;

    if Options.Items.CanXYZ and (RealMode3D=TBIChart3DMode.XYZ) then
       TryXYZ
    else
       TryGrid3D;
  end
  else
  if RealMode=TBIChartMode.Geographic then
     TryGeo
  else
  {$ENDIF}
  if Options.Items.Y=nil then
  begin
    if Options.Items.Text<>nil then
       AddText(Options.Items.Text);
  end
  else
    AddXY(Options.Items);

  Options.Finish;
end;

procedure TBIChart.ExchangeXY;
begin
  Options.Items.ExchangeXY;
  DirectRefresh;
end;

procedure TBIChart.ExchangeYZ;
begin
  Options.Items.ExchangeYZ;
  DirectRefresh;
end;

procedure TBIChart.ExchangeXZ;
begin
  Options.Items.ExchangeXZ;
  DirectRefresh;
end;

procedure TBIChart.FillSeries(const ASeries:TChartSeries; const X,Y,AText:TDataItem);

  function RealKind(const AData:TDataItem):TDataKind;
  begin
    if AData.AsTable and (AData.Items.Count>0) then
       result:=RealKind(AData.Items[0])
    else
       result:=AData.Kind;
  end;

  function RealTable(const AData:TDataItem):TDataItem;
  begin
    if AData.AsTable and (AData.Items.Count>0) then
       if AData.Items[0].AsTable then
          result:=RealTable(AData.Items[0])
       else
          result:=AData
    else
       result:=AData;
  end;

  procedure FillOneSeries(const ASeries:TChartSeries; const AData:TDataItem);

    procedure AddValue(const APosition:TInteger);
    var tmpYVal : TChartValue;
        tmpLabel : String;
    begin
      tmpYVal:=TBITChart.GetValue(AData,APosition);

      if AText<>nil then
         tmpLabel:=AText.DataToString(APosition);

      if X=nil then
         ASeries.Add(tmpYVal,tmpLabel)
      else
         ASeries.AddXY(TBITChart.GetValue(X,APosition),tmpYVal,tmpLabel);
    end;

  var t : TLoopInteger;
  begin
    if Index=nil then
       if ASeries.YMandatory then
          for t:=0 to AData.Count-1 do
              AddValue(t)
       else
          for t:=AData.Count-1 downto 0 do
              AddValue(t)
    else
       if ASeries.YMandatory then
          for t:=Low(Index) to High(Index) do
              AddValue(Index[t])
       else
          for t:=High(Index) downto Low(Index) do
              AddValue(Index[t])
  end;

  procedure FillTable(const Y:TDataItem);
  var t : TLoopInteger;
      tmpS2 : TChartSeries;
  begin
    FillOneSeries(ASeries,Y.Items[0]);
    ASeries.Title:=Y.Items[0].Name;
    ASeries.ColorEachPoint:=False;

    for t:=1 to Y.Items.Count-1 do
    begin
      tmpS2:=Chart.NewSeries(TChartSeriesClass(ASeries.ClassType));

      tmpS2.ColorEachPoint:=False;
      tmpS2.Marks.Style:=ASeries.Marks.Style;

      FillOneSeries(tmpS2,Y.Items[t]);
      tmpS2.Title:=Y.Items[t].Name;
    end;

    Chart.Legend.Title.Caption:=Y.Name;
  end;

var tmpLabel : String;
    t : TLoopInteger;
    tmpYVal : TChartValue;
    tmpY : TDataItem;
    tmpMap : TBooleanMap;
begin
  if Y<>nil then
     ASeries.MandatoryValueList.DateTime:=RealKind(Y)=dkDateTime;

  tmpLabel:='';

  ASeries.BeginUpdate;
  try
    if Y=nil then
    begin
      if AText<>nil then
      begin
        AText.Load;

        if AText.Kind=TDataKind.dkBoolean then
        begin
          tmpMap:=TBooleanMap(AText.DataMap);

          ASeries.Add(tmpMap.Map[1],BoolToStr(True,True));
          ASeries.Add(tmpMap.Map[0],BoolToStr(False,True));
        end
        else
        if AText.Kind=TDataKind.dkText then // <-- Histogram?
           for t:=0 to AText.DataMap.Count-1 do
           begin
             tmpLabel:=TTextMap(AText.DataMap)[t];
             tmpYVal:=AText.DataMap.Map[t];
             ASeries.Add(tmpYVal,tmpLabel);
           end;
      end;
    end
    else
    begin
      Y.Load;

      if AText<>nil then
         AText.Load;

      if X<>nil then
         X.Load;

      tmpY:=RealTable(Y);

      if tmpY.AsTable then
         FillTable(tmpY)
      else
         FillOneSeries(ASeries,tmpY);
    end;
  finally
    ASeries.EndUpdate;
  end;
end;

type
  TSeriesAccess=class(TChartSeries);

procedure TBIChart.AddXY(const AItems:TBIChartItems);

  procedure AddByRows;
  var tmpS : TChartSeries;
      t    : Integer;
      tmpX : String;
      tmpY : TDataItem;
  begin
    tmpS:=CreateSeries(AItems.X,AItems.Y[0]);

    tmpX:='';

    if AItems.Text<>nil then
       tmpX:=AItems.Text.Name;

    if tmpX='' then
       if AItems.X<>nil then
          tmpX:=AItems.X.Name;

    Chart.SetAxesTitles(tmpS,tmpX,tmpS.Title);

    FillSeries(tmpS,AItems.X,AItems.Y[0],AItems.Text);

    if Length(AItems.Y)>1 then
    begin
      TSeriesAccess(tmpS).SetDefaultColor;

      for t:=Low(AItems.Y)+1 to High(AItems.Y) do
      begin
        tmpY:=AItems.Y[t];

        if tmpY.Kind.IsNumeric then
        begin
          tmpS:=CreateSeries(AItems.X,tmpY);
          TSeriesAccess(tmpS).SetDefaultColor;

          FillSeries(tmpS,AItems.X,tmpY,AItems.Text);
        end;
      end;

      // Clear Title because there are more than one series

      if tmpS.YMandatory then
         tmpS.GetVertAxis.Title.Caption:=''
      else
         tmpS.GetHorizAxis.Title.Caption:='';
    end
    else
    if (Chart.SeriesCount=1) and
       (not tmpS.HasZValues) and
       (not tmpS.DrawBetweenPoints) then // <-- skip Line and Area
          tmpS.ColorEachPoint:=True
  end;

  procedure AddByColumns(const AMain:TDataItem);
  var tmpS : TChartSeries;
      t,
      tt : Integer;
      tmp,
      tmpX : TChartValue;
      tmpY : TDataItem;
  begin
    tmpX:=0;

    for t:=0 to AMain.Count-1 do
    begin
      tmpS:=Chart.NewSeries(AItems.Y.Count);

      if AItems.Text<>nil then
         tmpS.Title:=AItems.Text.DataToString(t);

      if AItems.X<>nil then
         tmpX:=TBITChart.GetValue(AItems.X,t);

      tmpS.BeginUpdate;
      try
        for tt:=0 to AItems.Y.Count-1 do
        begin
          tmpY:=AItems.Y[tt];

          tmp:=TBITChart.GetValue(tmpY,t);

          if AItems.X=nil then
             tmpS.Add(tmp,tmpY.Name)
          else
             tmpS.AddXY(tmpX,tmp,tmpY.Name)
        end;
      finally
        tmpS.EndUpdate;
      end;
    end;
  end;

var tmp : TBIChartDirection;
    tmpData : TDataItem;
begin
  tmpData:=AItems.Y[0].Parent;

  tmp:=Options.Direction;

  if tmp=TBIChartDirection.Automatic then
     if tmpData.Count>AItems.Y.Count then
        tmp:=TBIChartDirection.Rows
     else
        tmp:=TBIChartDirection.Columns;

  if tmp=TBIChartDirection.Rows then
     AddByRows
  else
     AddByColumns(tmpData);
end;

function TBIChart.GuessRealData(const AData:TDataArray):TDataArray;
var tmp : TDataItem;
begin
  if AData.Count=1 then
  begin
    tmp:=LastUniqueTable(AData[0]);

    if tmp.AsTable then
       result:=tmp.Items.AsArray
    else
    begin
      if (tmp.Kind=TDataKind.dkUnknown) and (tmp.Items.Count>0) then
      begin
        if IDataInfo=nil then
           IDataInfo:=TDataInfo.Create(tmp)
        else
           IDataInfo.Data:=tmp;

        result:=IDataInfo.Items.AsArray;
      end
      else
      begin
        SetLength(result,1);
        result[0]:=tmp;
      end;
    end
  end
  else
    result:=AData;
end;

procedure TBIChart.Fill(const AData:TDataArray);
begin
  Options.Items.Clear;

  if AData.Count>0 then
  begin
    Options.Items.Guess(GuessRealData(AData));
    DirectRefresh;
  end
end;

{$IFNDEF FPC}
procedure TBIChart.Fill(const Map:TDataMap; const Text:TGetText=nil);
var tmpS : TChartSeries;
       t : TLoopInteger;
begin
  tmpS:=Chart.NewSeries(Map.Count);
  Chart.SetAxesTitles(tmpS,'','Count');

  tmpS.BeginUpdate;
  try
    if Assigned(Text) then
       for t:=0 to Map.Count-1 do
           tmpS.Add(Map.Map[t],Text(t))
    else
       for t:=0 to Map.Count-1 do
           tmpS.Add(Map.Map[t]);

  finally
    tmpS.EndUpdate;
  end;
end;
{$ENDIF}

procedure TBIChart.Fill(const AHistogram: THistogram; const ASource:TDataItem);
var tmpData : TDataItem;
begin
  tmpData:=AHistogram.Calculate(ASource);
  try
    Fill(tmpData.Items);
  finally
    tmpData.Free;
  end;
end;

procedure TBIChart.Fill(const ACursor:TDataCursor; const AItems:TDataArray=nil);
begin
  Index:=ACursor.Index;

  if ACursor.Count>0 then
     if AItems=nil then
        if ACursor.DataItems=nil then
           Fill(ACursor.Data.Items.AsArray)
        else
           Fill(ACursor.DataItems)
     else
        Fill(AItems);
end;

procedure TBIChart.Fill(const ASummary: TSummary);
var tmpData : TDataItem;
begin
  tmpData:=ASummary.Calculate;
  try
    Fill(tmpData.Items);
  finally
    tmpData.Free;
  end;
end;

procedure TBIChart.Fill(const AItems: TDataItems);
begin
  Fill(AItems.AsArray);
end;

procedure TBIChart.SplitAxes(const Vertical:Boolean);
var t : Integer;
    tmp : TChartAxis;
    tmpC : Integer;
    tmpSize : Single;
begin
  tmpC:=Chart.SeriesCount;

  tmpSize:=0.9*(100/tmpC);

  for t:=1 to tmpC-1 do
  begin
    if Vertical then
       Chart[t].VertAxis:=TVertAxis.aCustomVertAxis
    else
       Chart[t].HorizAxis:=THorizAxis.aCustomHorizAxis;

    tmp:=Chart.CustomAxes.Add {$IFNDEF TEEPRO}as TChartAxis{$ENDIF};

    tmp.Horizontal:=not Vertical;
    tmp.StartPosition:=t*(100/tmpC);

    if t=tmpC-1 then
       tmp.EndPosition:=100
    else
       tmp.EndPosition:=tmp.StartPosition+tmpSize;

    tmp.Title.Caption:=Chart[t].Title;
    tmp.Title.Font.Style:=[TFontStyle.fsBold];

    if Vertical then
       Chart[t].CustomVertAxis:=tmp
    else
       Chart[t].CustomHorizAxis:=tmp;

    if t<tmpC-1 then
       Chart.AddLine(tmp);
  end;

  if Vertical then
     tmp:=Chart[0].GetVertAxis
  else
     tmp:=Chart[0].GetHorizAxis;

  tmp.EndPosition:=tmpSize;
  tmp.Title.Caption:=Chart[0].Title;

  Chart.AddLine(tmp);
end;

procedure TBIChart.SetTwoAxes;
var t,
    tmpMid : Integer;
    tmpY : Boolean;
begin
  tmpMid:=Chart.SeriesCount div 2;

  tmpY:=Chart[0].YMandatory;

  for t:=0 to tmpMid-1 do
      if tmpY then
         Chart[t].VertAxis:=TVertAxis.aLeftAxis
      else
         Chart[t].HorizAxis:=THorizAxis.aBottomAxis;

  for t:=tmpMid to Chart.SeriesCount-1 do
      if tmpY then
         Chart[t].VertAxis:=TVertAxis.aRightAxis
      else
         Chart[t].HorizAxis:=THorizAxis.aTopAxis;

  if tmpY then
     Chart.Axes.Right.Grid.Hide
  else
     Chart.Axes.Top.Grid.Hide;

  if Chart.SeriesCount=2 then
  begin
    if tmpY then
       Chart.Axes.Right.Title.Caption:=Chart[1].Title
    else
       Chart.Axes.Top.Title.Caption:=Chart[1].Title;
  end;
end;

{ TBIChartItems }

procedure TBIChartItems.Clear;
begin
  X:=nil;
  Y:=nil;
  Z:=nil;
  Colors:=nil;
  Text:=nil;
  Group:=nil;
end;

procedure TBIChartItems.ReadColors(Reader: TReader);
begin
  IColors:=Reader.ReadString;
end;

procedure TBIChartItems.ReadGroup(Reader: TReader);
begin
  IGroup:=Reader.ReadString;
end;

procedure TBIChartItems.ReadText(Reader: TReader);
begin
  IText:=Reader.ReadString;
end;

procedure TBIChartItems.ReadX(Reader: TReader);
begin
  IX:=Reader.ReadString;
end;

procedure TBIChartItems.ReadY(Reader: TReader);
var L : Integer;
begin
  IY:=nil;
  L:=0;

  Reader.ReadListBegin;

  while not Reader.EndOfList do
  begin
    SetLength(IY,L+1);
    IY[L]:=Reader.ReadString;
    Inc(L);
  end;

  Reader.ReadListEnd;
end;

procedure TBIChartItems.ReadZ(Reader: TReader);
begin
  IZ:=Reader.ReadString;
end;

procedure TBIChartItems.WriteColors(Writer: TWriter);
begin
  Writer.WriteString(Colors.Name);
end;

procedure TBIChartItems.WriteGroup(Writer: TWriter);
begin
  Writer.WriteString(Group.Name);
end;

procedure TBIChartItems.WriteText(Writer: TWriter);
begin
  Writer.WriteString(Text.Name);
end;

procedure TBIChartItems.WriteX(Writer: TWriter);
begin
  Writer.WriteString(X.Name);
end;

procedure TBIChartItems.WriteY(Writer: TWriter);
var t : Integer;
begin
  Writer.WriteListBegin;

  for t:=0 to Y.Count-1 do
      Writer.WriteString(Y[t].Name);

  Writer.WriteListEnd;
end;

procedure TBIChartItems.WriteZ(Writer: TWriter);
begin
  Writer.WriteString(Z.Name);
end;

procedure TBIChartItems.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('X', ReadX, WriteX, X<>nil);
  Filer.DefineProperty('Z', ReadZ, WriteZ, Z<>nil);
  Filer.DefineProperty('Text', ReadText, WriteText, Text<>nil);
  Filer.DefineProperty('Group', ReadGroup, WriteGroup, Group<>nil);
  Filer.DefineProperty('Colors', ReadColors, WriteColors, Colors<>nil);
  Filer.DefineProperty('Y', ReadY, WriteY, Y<>nil);
end;

procedure TBIChartItems.ExchangeXY;
var tmp : TDataItem;
begin
  tmp:=X;

  if Y<>nil then
     X:=Y[0]
  else
     X:=nil;

  if tmp=nil then
     Y:=nil
  else
  if Y=nil then
     Y.Add(tmp)
  else
     Y[0]:=tmp;
end;

procedure TBIChartItems.ExchangeXZ;
var tmp : TDataItem;
begin
  tmp:=X;
  X:=Z;
  Z:=tmp;
end;

procedure TBIChartItems.ExchangeYZ;
var tmp : TDataItem;
begin
  tmp:=Z;

  if Y<>nil then
     Z:=Y[0]
  else
     Z:=nil;

  if tmp=nil then
     Y:=nil
  else
  if Y=nil then
     Y.Add(tmp)
  else
     Y[0]:=tmp;
end;

function TBIChartItems.CanFinancial:Boolean;
begin
  {$IFDEF TEEPRO}
  result:=TFinancialChart.Guess(Y);
  {$ELSE}
  result:=False;
  {$ENDIF}
end;

function TBIChartItems.CanGeoGraphic:Boolean;
begin
  {$IFDEF TEEPRO}
  if (X=nil) and (Y.Count>0) then
     result:=TGeoChart.Guess(Y[0],Text,FGeoContext)
  else
     result:=TGeoChart.Guess(X,Text,FGeoContext);
  {$ELSE}
  result:=False;
  {$ENDIF}
end;

function TBIChartItems.CanThreeD:Boolean;

  {$IFDEF TEEPRO}
  function CountThree3D:Integer;
  begin
    result:=0;

    if MapCount(X)>1 then
       Inc(result);

    if MapCount(Text)>1 then
       Inc(result);

    if MapCount(Group)>1 then
       Inc(result);

    if (Y<>nil) and (Y[0].Count>1) then
       Inc(result); // <-- just 1
  end;
  {$ENDIF}

begin
  {$IFDEF TEEPRO}
  result:=CanXYZ or (CountThree3D>2);
  {$ELSE}
  result:=False;
  {$ENDIF}
end;

function TBIChartItems.CanXYZ:Boolean;
begin
  result:=IsMeasure(X) and
          (Y<>nil) and
          IsMeasure(Z);

  if not result then
  begin
    result:=(Y.Count>=3); // and (Y[0].Count>1); // at least 3 cols, 2 rows

    if not result then
       result:=IsMeasure(X) and
               (Y.Count>=2);
  end;
end;

{ TBIChartOptions }

Constructor TBIChartOptions.Create(const AChart:TBIChart);
begin
  inherited Create;

  FItems:=TBIChartItems.Create;
  IChart:=AChart;
end;

Destructor TBIChartOptions.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TBIChartOptions.ChangeSeries2D(const AClass: TChartSeriesClass;
  const AHorizontal, ALinePointer: Boolean);
begin
  IChart.Chart.ChangeSeries2D(AClass,AHorizontal,ALinePointer);
end;

procedure TBIChartOptions.Assign(Source: TPersistent);
var tmp : TBIChartOptions;
begin
  if Source is TBIChartOptions then
  begin
    tmp:=TBIChartOptions(Source);

    FDimensions:=tmp.FDimensions;
    FLegend:=tmp.FLegend;
    FMarks:=tmp.FMarks;
    FStacked:=tmp.FStacked;

    FDirection:=tmp.FDirection;
    FMode:=tmp.FMode;
    FItems.Assign(tmp.FItems);
    FXYZMode:=tmp.FXYZMode;

    // Always force refresh
    IChart.DirectRefresh;
  end
  else
    inherited;
end;

type
  TCustomStackSeriesAccess=class(TCustomStackSeries);

procedure TBIChartOptions.Finish;

  function CustomSeriesStack:TCustomSeriesStack;
  begin
    case FStacked of
       TBIChartStacked.Yes: result:=TCustomSeriesStack.cssStack;
TBIChartStacked.Stacked100: result:=TCustomSeriesStack.cssStack100;
     TBIChartStacked.Side,
   TBIChartStacked.SideAll: result:=TCustomSeriesStack.cssOverlap;
    else
      {TBIChartStacked.No:}
      result:=TCustomSeriesStack.cssNone;
    end;
  end;

  function CustomBarStack:TMultiBar;
  begin
    case FStacked of
        TBIChartStacked.No: result:=TMultiBar.mbNone;
       TBIChartStacked.Yes: result:=TMultiBar.mbStacked;
TBIChartStacked.Stacked100: result:=TMultiBar.mbStacked100;
      TBIChartStacked.Side: result:=TMultiBar.mbSide;
   TBIChartStacked.SideAll: result:=TMultiBar.mbSideAll;
 TBIChartStacked.SelfStack: result:=TMultiBar.mbSelfStack;
    else
      result:=TMultiBar.mbSide;
    end;
  end;

  function PieStack:TMultiPie;
  begin
    case FStacked of
        TBIChartStacked.No: result:=TMultiPie.mpDisabled;

       {$IFDEF TEEPRO}
       TBIChartStacked.Yes: result:=TMultiPie.mpConcentric;
       {$ENDIF}
    else
       result:=TMultiPie.mpAutomatic;
    end;
  end;

var tmp : TChartSeries;
begin
  if FMarks<>TBIChartMarks.Automatic then
     for tmp in IChart.Chart.SeriesList do
         tmp.Marks.Visible:=FMarks=TBIChartMarks.Show;

  for tmp in IChart.Chart.SeriesList do
      if tmp is TCustomSeries then
      begin
        TCustomStackSeriesAccess(tmp).Stacked:=CustomSeriesStack;

        {$IFDEF TEEPRO}
        if (tmp is TAreaSeries) and
           (TCustomStackSeriesAccess(tmp).Stacked<>cssStack) and
           (TCustomStackSeriesAccess(tmp).Stacked<>cssStack100) then
             tmp.Transparency:=20;
        {$ENDIF}

        // Lots of points? Make them single or small "dots"
        if tmp.Count>(IChart.Chart.Width {$IFDEF FMX}*0.5{$ELSE}div 2{$ENDIF}) then
           TCustomSeries(tmp).Pointer.Style:=TSeriesPointerStyle.psSmallDot;
      end
      else
      if tmp is TCustomBarSeries then
         TCustomBarSeries(tmp).MultiBar:=CustomBarStack
      else
      if tmp is TPieSeries then
         TPieSeries(tmp).MultiPie:=PieStack
      {$IFDEF TEEPRO}
      else
        TThreeDChart.FinishSeries(IChart.Chart,tmp,FStacked)
      {$ENDIF}
      ;

  FinishViewDimensions;
end;

procedure TBIChartOptions.FinishViewDimensions;
begin
  IChart.Chart.FinishViewDimensions;
end;

function TBIChartOptions.GetSeries2D: TChartSeriesClass;
begin
  result:=IChart.Chart.Series2D;
end;

function TBIChartOptions.GetSeries3D: TChartSeriesClass;
begin
  result:=IChart.Chart.Series3D;
end;

procedure TBIChartOptions.SetDimensions(const Value: TBIChartDimensions);
begin
  if FDimensions<>Value then
  begin
    FDimensions:=Value;

    if FDimensions=TBIChartDimensions.View2D then
    begin
      IChart.Chart.View3D:=False;
      FinishViewDimensions;
    end
    else
    if FDimensions<>TBIChartDimensions.Automatic then
    begin
      IChart.Chart.View3D:=True;
      IChart.Chart.View3DOptions.Orthogonal:=FDimensions=TBIChartDimensions.Orthogonal;
      FinishViewDimensions;
    end
    else
      IChart.DirectRefresh;
  end;
end;

procedure TBIChartOptions.SetDirection(const Value: TBIChartDirection);
begin
  if FDirection<>Value then
  begin
    FDirection:=Value;
    IChart.DirectRefresh;
  end;
end;

procedure TBIChartOptions.SetItems(const Value: TBIChartItems);
begin
  Items.Assign(Value);
end;

procedure TBIChartOptions.SetLegend(const Value: TBIChartLegend);
begin
  if FLegend<>Value then
  begin
    FLegend:=Value;

    if FLegend=TBIChartLegend.Automatic then
       IChart.DirectRefresh
    else
       IChart.Chart.Legend.Visible:=FLegend=TBIChartLegend.Show;
  end;
end;

procedure TBIChartOptions.SetMarks(const Value: TBIChartMarks);
var tmp : TChartSeries;
begin
  if FMarks<>Value then
  begin
    FMarks:=Value;

    if FMarks=TBIChartMarks.Automatic then
       IChart.DirectRefresh
    else
    for tmp in IChart.Chart.SeriesList do
        tmp.Marks.Visible:=FMarks=TBIChartMarks.Show;
  end;
end;

procedure TBIChartOptions.SetMode(const Value: TBIChartMode);
begin
  if FMode<>Value then
  begin
    FMode:=Value;
    IChart.DirectRefresh;
  end;
end;

procedure TBIChartOptions.SetSeries2D(const Value: TChartSeriesClass);
begin
  if IChart.Chart.Series2D<>Value then
  begin
    IChart.Chart.Series2D:=Value;
    IChart.DirectRefresh;
  end;
end;

procedure TBIChartOptions.SetSeries3D(const Value: TChartSeriesClass);
begin
  {$IFDEF TEEPRO}
  if TThreeDChart.SetSeries3D(IChart.Chart,Value) then
     IChart.DirectRefresh;
  {$ELSE}
  IChart.Chart.Series3D:=Value;
  {$ENDIF}
end;

procedure TBIChartOptions.SetSeriesDirection(const Value: TBISeriesDirection);
begin
  if FSeriesDirection<>Value then
  begin
    FSeriesDirection:=Value;
    IChart.DirectRefresh;
  end;
end;

procedure TBIChartOptions.SetStacked(const Value: TBIChartStacked);
begin
  if FStacked<>Value then
  begin
    FStacked:=Value;

    // Pending: try to change to new stacked if <> automatic, without
    // forcing call to DirectRefresh
    IChart.DirectRefresh;
  end;
end;

procedure TBIChartOptions.SetXYZMode(const Value: TBIChart3DMode);
begin
  if FXYZMode<>Value then
  begin
    FXYZMode:=Value;
    IChart.DirectRefresh;
  end;
end;

end.
