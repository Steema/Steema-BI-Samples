{*********************************************}
{  TeeBI Software Library                     }
{  TChart 3D Charts                           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Chart.ThreeD;
{$DEFINE FMX}

interface

uses
  BI.DataItem,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Procs, FMXBI.DataControl, FMXBI.Grid,
  FMXTee.Engine, FMXTee.Chart,
  FMXBI.Chart.Plugin,
  {$ELSE}
  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.TeeProcs, VCLBI.DataControl, VCLBI.Grid,
  VCLTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Chart,
  VCLBI.Chart.Plugin,
  {$ENDIF}

  {$IFDEF FMX}
  FMXTee.Series.Surface
  {$ELSE}
  VCLTee.TeeSurfa
  {$ENDIF}
  ;

type
  TCustom3DSeriesClass=class of TCustom3DSeries;

  TThreeDChart=record
  public
    class function CanReuse(const AChart:TBITChart):Boolean; static;

    class procedure CreateGrid3D(const AChart:TBITChart;
                                 const X,Y,Z:TDataItem;
                                 const ADirection:TBIChartDirection); static;

    class procedure CreateGridTable(const AChart:TBITChart;
                                    const AData:TDataArray;
                                    const ADirection:TBIChartDirection); static;

    class procedure CreateXYZ(const AChart:TBITChart;
                              const X,Y,Z:TDataItem); static;

    class function Create3DSeries(const AChart:TBITChart):TCustom3DSeries; static;

    class procedure FinishSeries(const AChart:TChart;
                                 const ASeries:TChartSeries;
                                 const AStacked:TBIChartStacked); static;

    class procedure FinishXYZ(const AChart:TChart); static;

    class function SetSeries3D(const AChart:TBITChart;
                               const AClass:TChartSeriesClass):Boolean; static;

    class procedure TryPaletteLegend(const AChart:TBITChart; const ASeries:TChartSeries); static;
  end;

implementation

uses
  BI.Arrays,

  {$IFDEF FMX}
  FMXTee.Series.Point3D
  {$ELSE}
  VCLTee.TeePoin3
  {$ENDIF}
  ;

class function TThreeDChart.CanReuse(const AChart:TBITChart):Boolean;
begin
  // Pending: Try to investigate AChart, to avoid destroying all series in it
  result:=False;
end;

class function TThreeDChart.Create3DSeries(const AChart:TBITChart):TCustom3DSeries;
begin
  if (AChart.Series3D<>nil) and AChart.Series3D.InheritsFrom(TCustom3DSeries) then
     result:=AChart.NewSeries(AChart.Series3D) as TCustom3DSeries
  else
     result:=AChart.NewSeries(TColorGridSeries) as TCustom3DSeries;
end;

class procedure TThreeDChart.CreateGridTable(const AChart:TBITChart;
                     const AData:TDataArray;
                     const ADirection:TBIChartDirection);
var
  tmpParent : TDataItem;

  procedure DoXYZByRows(const ASeries:TCustom3DSeries);
  var t,tt : TLoopInteger;
  begin
    for t:=0 to AData.Count-1 do
        for tt:=0 to tmpParent.Count-1 do
            ASeries.AddXYZ(t,TBITChart.GetValue(AData[tt],t),tt);
  end;

  procedure DoXYZByColumns(const ASeries:TCustom3DSeries);
  var t,tt : TLoopInteger;
  begin
    for t:=0 to AData.Count-1 do
        for tt:=0 to tmpParent.Count-1 do
            ASeries.AddXYZ(tt,TBITChart.GetValue(AData[tt],t),t);
  end;

  procedure DoGridByRows(const ASeries:TCustom3DGridSeries);
  var t,tt : TLoopInteger;
  begin
    ASeries.NumXValues:=AData.Count;
    ASeries.NumZValues:=tmpParent.Count;

    for t:=0 to ASeries.NumZValues-1 do
        for tt:=0 to ASeries.NumXValues-1 do
            ASeries.Value[t,tt]:=TBITChart.GetValue(AData[tt],t);

    // X Labels ?
    //ASeries.XLabel[t]:=AData.Items[t].Name;

    ASeries.GetVertAxis.Increment:=1;
  end;

  procedure DoGridByColumns(const ASeries:TCustom3DGridSeries);
  var t,tt : TLoopInteger;
  begin
    ASeries.NumXValues:=tmpParent.Count;
    ASeries.NumZValues:=AData.Count;

    for t:=0 to ASeries.NumZValues-1 do
        for tt:=0 to ASeries.NumXValues-1 do
            ASeries.Value[t,tt]:=TBITChart.GetValue(AData[t],tt);

    // Z Labels ?
    //ASeries.ZLabel[t]:=AData.Items[t].Name;

    ASeries.GetHorizAxis.Increment:=1;
  end;

  function RealDirection:TBIChartDirection;
  begin
    result:=ADirection;

    if result=TBIChartDirection.Automatic then
       if AData.Count>tmpParent.Count then
          result:=TBIChartDirection.Columns
       else
          result:=TBIChartDirection.Rows;
  end;

var tmp : TCustom3DSeries;
begin
  tmpParent:=AData[0].Parent;
  tmpParent.Load;

  AChart.ClearTitles;

  tmp:=Create3DSeries(AChart);

  tmp.BeginUpdate;
  try
    if tmp is TCustom3DGridSeries then
       if RealDirection=TBIChartDirection.Rows then
          DoGridByRows(TCustom3DGridSeries(tmp))
       else
          DoGridByColumns(TCustom3DGridSeries(tmp))
    else
       if RealDirection=TBIChartDirection.Rows then
          DoXYZByRows(tmp)
       else
          DoXYZByColumns(tmp);
  finally
    tmp.EndUpdate;
  end;
end;

class procedure TThreeDChart.CreateGrid3D(const AChart:TBITChart;
                     const X,Y,Z:TDataItem; const ADirection:TBIChartDirection);
var
  Main : TDataItem;

  procedure DoXYZByRows(const ASeries:TCustom3DSeries);
  var t,tt : TLoopInteger;
  begin
    for t:=0 to Z.DataMap.Count-1 do
        for tt:=1 to Main.Count-1 do
            ASeries.AddXYZ(t,TBITChart.GetValue(Y,t),tt);
  end;

  procedure DoXYZByColumns(const ASeries:TCustom3DSeries);
  var t,tt : TLoopInteger;
  begin
    for t:=0 to Z.DataMap.Count-1 do
        for tt:=1 to Main.Count-1 do
            ASeries.AddXYZ(tt,TBITChart.GetValue(Y,t),t);
  end;

  procedure DoByRows(const ASeries:TCustom3DGridSeries);
  var t,tt : TLoopInteger;
  begin
    ASeries.NumXValues:=Main.Count-1;
    ASeries.NumZValues:=Z.DataMap.Count-1;

    for t:=0 to Z.DataMap.Count-1 do
        for tt:=1 to Main.Count-1 do
            ASeries.Value[t,tt]:=TBITChart.GetValue(Y,t);

    for t:=1 to Main.Count-1 do
        ASeries.XLabel[t]:=X.DataToString(t);

    // Z Labels ?
  end;

  procedure DoByColumns(const ASeries:TCustom3DGridSeries);
  var t,tt : TLoopInteger;
  begin
    ASeries.NumXValues:=Z.DataMap.Count-1;
    ASeries.NumZValues:=Main.Count-1;

    for t:=0 to Z.DataMap.Count-1 do
        for tt:=1 to Main.Count-1 do
            ASeries.Value[t,tt]:=TBITChart.GetValue(Y,t);

    for t:=1 to Main.Count-1 do
        ASeries.XLabel[t]:=Z.DataToString(t);

    // X Labels ?
  end;

  function CalcDirection:TBIChartDirection;
  begin
    result:=ADirection;

    if result=TBIChartDirection.Automatic then
       if Main.Count>Z.DataMap.Count then
          result:=TBIChartDirection.Rows
       else
          result:=TBIChartDirection.Columns;
  end;

var tmpDir : TBIChartDirection;
    tmp : TCustom3DSeries;
begin
  Z.Stats;

  Main:=Y.Parent; // <-- support for hops?

  AChart.ClearTitles;

  tmpDir:=CalcDirection;

  tmp:=Create3DSeries(AChart);

  tmp.BeginUpdate;
  try
    if tmp is TCustom3DGridSeries then
       if tmpDir=TBIChartDirection.Rows then
          DoByRows(TCustom3DGridSeries(tmp))
       else
          DoByColumns(TCustom3DGridSeries(tmp))
    else
       if tmpDir=TBIChartDirection.Rows then
          DoXYZByRows(tmp)
       else
          DoXYZByColumns(tmp);
  finally
    tmp.EndUpdate;
  end;
end;

class procedure TThreeDChart.FinishXYZ(const AChart:TChart);
begin
  AChart.Axes.Depth.Visible:=True;

  AChart.View3D:=True;
  AChart.View3DOptions.Orthogonal:=False;
  AChart.Panning.MouseWheel:=pmwNone;
  AChart.ZoomWheel:=pmwNormal;
  AChart.View3DOptions.Perspective:=100;
  AChart.Chart3DPercent:=100;
  AChart.View3DOptions.Zoom:=75;
end;

class procedure TThreeDChart.CreateXYZ(const AChart:TBITChart; const X,Y,Z:TDataItem);
var tmpClass : TChartSeriesClass;
    tmp : TCustom3DSeries;
    t : Integer;
begin
  AChart.ClearTitles;

  if (AChart.Series3D<>nil) and AChart.Series3D.InheritsFrom(TCustom3DSeries) then
     tmpClass:=AChart.Series3D
  else
     tmpClass:=TPoint3DSeries;

  tmp:=AChart.NewSeries(tmpClass) as TCustom3DSeries;

  tmp.BeginUpdate;
  try
    if tmp is TPoint3DSeries then
    begin
      TPoint3DSeries(tmp).LinePen.Hide;
      TPoint3DSeries(tmp).Pointer.Style:=psCircle;
      TPoint3DSeries(tmp).Pointer.Pen.Hide;
    end;

    for t:=0 to X.Parent.Count-1 do
        tmp.AddXYZ(TBITChart.GetValue(X,t),
                   TBITChart.GetValue(Y,t),
                   TBITChart.GetValue(Z,t));

    AChart.SetAxesTitles(tmp,X.Name,Y.Name);

    tmp.ParentChart.Axes.Depth.Title.Caption:=Z.Name;

    FinishXYZ(AChart);
  finally
    tmp.EndUpdate;
  end;
end;

function TowerStack(const AStacked:TBIChartStacked):TTowerStacked;
begin
  case AStacked of
     TBIChartStacked.Yes: result:=TTowerStacked.tsStacked;
TBIChartStacked.Stacked100: result:=TTowerStacked.tsStacked100;
  else
    {TBIChartStacked.No:}
    result:=TTowerStacked.tsNone;
  end;
end;

class procedure TThreeDChart.FinishSeries(const AChart:TChart;
                                          const ASeries:TChartSeries;
                                          const AStacked:TBIChartStacked);
var tmp : TColorGridSeries;
begin
  if ASeries is TTowerSeries then
     TTowerSeries(ASeries).Stacked:=TowerStack(AStacked)
  else
  if ASeries is TColorGridSeries then
  begin
    tmp:=TColorGridSeries(ASeries);

    // Lots of cells? Hide Grid pen
    if (tmp.NumXValues>(AChart.Width {$IFDEF FMX}*0.25{$ELSE}div 4{$ENDIF})) or
       (tmp.NumZValues>(AChart.Height {$IFDEF FMX}*0.25{$ELSE}div 4{$ENDIF})) then
          tmp.Pen.Hide;
  end;
end;

class function TThreeDChart.SetSeries3D(const AChart:TBITChart;
                                        const AClass:TChartSeriesClass):Boolean;
begin
  result:=False;

  if AChart.Series3D<>AClass then
     if AClass.InheritsFrom(TCustom3DSeries) then
     begin
       AChart.Series3D:=TCustom3DSeriesClass(AClass);
       result:=True;
     end
     else
       raise EBIException.Create('Error: Series3D class must inherit from TCustom3DSeries');
end;

class procedure TThreeDChart.TryPaletteLegend(const AChart:TBITChart; const ASeries: TChartSeries);
begin
  if ASeries is TCustom3DPaletteSeries then
  begin
    AChart.Legend.Hide;
    AChart.ShowPaletteLegend(ASeries);
  end;
end;

initialization
  TBITChart.Three3DSeries:=TCustom3DSeries;
finalization
  TBITChart.Three3DSeries:=nil;
end.
