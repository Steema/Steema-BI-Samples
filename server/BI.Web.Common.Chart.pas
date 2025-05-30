{*********************************************}
{  TeeBI Software Library                     }
{  Web Server TeeChart (VCL and Firemonkey)   }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Common.Chart;

interface

uses
  System.Classes,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Canvas, FMXTee.Procs, FMXTee.Chart,

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO}
  {$ENDIF}

  {$IFDEF TEEPRO}
  FMXTee.Themes, FMXBI.PDF, FMXTee.Canvas.Javascript,
  {$ENDIF}

  FMXTee.Series, FMXBI.Chart,

  {$ELSE}

  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.Chart, VCLTee.TeeExport, VCLTee.TeeBmpOptions,
  VCLTee.Series, VCLTee.TeEngine, VCLBI.Chart,

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO}
  {$ENDIF}

  {$IFDEF TEEPRO}
  VCLTee.TeeJPEG, VCLTee.TeePNG, VCLTee.TeeThemes, VCLBI.PDF,
  VCLTee.TeeJavascript,
  {$ENDIF}

  {$ENDIF}

  BI.DataSource, BI.Web.Common, BI.Web.Context,
  System.UITypes;

type
  TBIWebCommonChart=record
  private
    Context : TWebContext;
    Cursor  : TDataCursor;
    Params  : TStrings;

    class function ChartFromCursor(var ACursor:TDataCursor;
                           const Params: TStrings;
                           const AWidth,AHeight:Integer;
                           const AUseTheme:Boolean=True):TBIChart; static;

    class function ChartToStream(var ACursor:TDataCursor; const Params:TStrings;
                           const AFormat:String;
                           const AWidth,AHeight:Integer):TStream; static;

    procedure DoExport(const AExtension,AContent:String);
    function DoExportChart(const AFormat:String):Boolean;

    {$IFDEF TEEPRO}
    procedure ExportChartHTML5;
    {$ENDIF}

    class procedure GetWidthHeight(const Params:TStrings; out AWidth,AHeight:Integer); static;
  public
    class function ExportChart(const AContext:TWebContext;
                               const ACursor:TDataCursor;
                               const AParams:TStrings;
                               const AFormat:String):Boolean; static;
  end;

implementation

uses
  {$IFDEF FMX}
  FMX.Graphics, FMX.Surfaces,
  {$ENDIF}

  System.SysUtils;

class function TBIWebCommonChart.ChartFromCursor(var ACursor: TDataCursor;
                    const Params : TStrings;
                    const AWidth, AHeight: Integer;
                    const AUseTheme:Boolean=True): TBIChart;

  function GetBoolean(const ATag:String; const ADefault:Boolean):Boolean;
  var tmp : String;
  begin
    tmp:=Params.Values[ATag];

    if tmp='' then
       result:=ADefault
    else
       result:=StrToBoolDef(tmp,ADefault);
  end;

  function Get3D:Boolean;
  begin
    result:=GetBoolean('view3d',False);
  end;

  {$IFDEF TEEPRO}
  function FindThemeClass(const S:String):TChartThemeClass;
  var t : Integer;
      tmp : TChartTheme;
  begin
    result:=TLookoutTheme;

    if S<>'' then
    begin
      for t:=0 to ChartThemes.Count-1 do
      begin
        tmp:=ChartThemes[t].Create(nil);
        try
          if SameText(tmp.Description,S) then
             Exit(ChartThemes[t]);
        finally
          tmp.Free;
        end;
      end;
    end;
  end;
  {$ENDIF}

begin
  result:=TBIChart.Create(nil);

  {$IFDEF TEEPRO}
  if AUseTheme then
  begin
    ApplyChartTheme(FindThemeClass(Params.Values['theme']),result.Chart);

    // Force disable bevels after applying theme
    result.Chart.BevelOuter:=bvNone;
  end;
  {$ENDIF}

  result.Width:=AWidth;
  result.Height:=AHeight;

  // Pending: change size internally at TBIChart
  result.Chart.Width:=AWidth;
  result.Chart.Height:=AHeight;

  result.Fill(ACursor,nil);
  result.Chart.View3D:=Get3D;
end;

{$IFDEF FMX}
function ChartToImage(const AChart:TChart; const AFormat:String):TStream;
var tmpB : TBitmap;
    tmpSurface : TBitmapSurface;
begin
  tmpB:=AChart.TeeCreateBitmap(TAlphaColors.Null,AChart.GetRectangle);
  try
    tmpSurface:=TBitmapSurface.Create;
    try
      tmpSurface.Assign(tmpB);

      result:=TMemoryStream.Create;

      //XE6: AV if png is used for first time (jpg is ok, then png is ok too)
      //possible workaround: if not TBitmapCodecManager.CodecExists()...

      TBitmapCodecManager.SaveToStream(result, tmpSurface, AFormat, nil {SaveParams});
    finally
      tmpSurface.Free;
    end;
  finally
    tmpB.Free;
  end;
end;

{$ELSE}

function ChartToImage(const AChart:TChart; const AFormat:String):TStream;
var tmp : TTeeExportFormat;
begin
  {$IFDEF TEEPRO}
  if SameText(AFormat,'.JPG') or SameText(AFormat,'.JPEG') then
     tmp:=TJPEGExportFormat.Create
  else
     tmp:=TPNGExportFormat.Create;
  {$ELSE}
     tmp:=TBMPExportFormat.Create;
  {$ENDIF}

  try
    tmp.Panel:=AChart;
    // Format.CheckSize(AWidth, AHeight);

    result:=TMemoryStream.Create;
    tmp.SaveToStream(result); // ,Options
  finally
    tmp.Free;
  end;
end;
{$ENDIF}

class function TBIWebCommonChart.ChartToStream(var ACursor:TDataCursor;
                                    const Params:TStrings;
                                    const AFormat:String;
                                    const AWidth,AHeight:Integer):TStream;
var tmp : TBIChart;
begin
  tmp:=ChartFromCursor(ACursor,Params,AWidth,AHeight);
  try
    result:=ChartToImage(tmp.Chart,AFormat);
    result.Position:=0;
  finally
    tmp.Free;
  end;
end;

class procedure TBIWebCommonChart.GetWidthHeight(const Params:TStrings; out AWidth,AHeight:Integer);

  function TryTag(const ATag:String; const ADefault:Integer):Integer;
  var tmp : String;
  begin
    tmp:=Params.Values[ATag];

    if tmp='' then
       result:=ADefault
    else
       result:=StrToIntDef(tmp,ADefault);
  end;

begin
  AWidth:=TryTag('width',800);
  AHeight:=TryTag('height',600);
end;

procedure TBIWebCommonChart.DoExport(const AExtension,AContent:String);
var w,h : Integer;
begin
  GetWidthHeight(Params,w,h);
  Context.ResponseStream:=ChartToStream(Cursor,Params,AExtension,w,h);

  if Context.ResponseStream<>nil then
     Context.ContentType:=AContent;
end;

{$IFDEF TEEPRO}
procedure TBIWebCommonChart.ExportChartHTML5;
var w,h : Integer;
    tmp : TBIChart;
    tmpJS : TJavascriptExportFormat;
begin
  GetWidthHeight(Params,w,h);

  tmp:=ChartFromCursor(Cursor,Params,w,h,False);
  try
    tmp.Chart.Title.Caption:=Params.Values['title'];

    // Avoid "TeeChart" title in html5 javascript chart
    if tmp.Chart.Title.Caption='' then
       tmp.Chart.Title.Caption:=' ';

    tmp.Chart.Color:=clNone; //{$IFDEF FMX}TAlphaColors.White{$ELSE}clWhite{$ENDIF};

    tmpJS:=TJavascriptExportFormat.Create;
    try
      // DEBUG using local js:
      //tmpJS.SourceScriptPath:='file:///C:\Root\Teechartjscript\src';

      tmp.Chart.View3DOptions.FontZoom:=StrToIntDef(Params.Values['fontzoom'],100); // %

      tmpJS.Minify:=True;
      tmpJS.Panel:=tmp.Chart;

      Context.ContentText:=tmpJS.JScript.Text;
    finally
      tmpJS.Free;
    end;
  finally
    tmp.Free;
  end;
end;
{$ENDIF}

function TBIWebCommonChart.DoExportChart(const AFormat:String):Boolean;
begin
  result:=True;

  if SameText(AFormat,'.PNG') then
     DoExport('.png','image/png')
  else
  if SameText(AFormat,'.JPG') or SameText(AFormat,'.JPEG') then
     DoExport('.jpg','image/jpeg')
  else
  {$IFDEF TEEPRO}
  if SameText(AFormat,'.HTML5') then
     ExportChartHTML5
  else
  {$ENDIF}
     result:=False;
end;

class function TBIWebCommonChart.ExportChart(const AContext:TWebContext;
                               const ACursor:TDataCursor;
                               const AParams:TStrings;
                               const AFormat:String):Boolean;
var tmp : TBIWebCommonChart;
begin
  tmp.Context:=AContext;
  tmp.Cursor:=ACursor;
  tmp.Params:=AParams;

  result:=tmp.DoExportChart(AFormat);
end;

initialization
  RegisterTeeStandardSeries;
end.
