{*********************************************}
{  TeeBI Software Library                     }
{  TChart Geographic Maps                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Chart.Geo;
{.$DEFINE FMX}

interface

uses
  {$IFDEF FMX}
  FMXBI.Chart.Plugin, FMXTee.Series.World,
  {$ELSE}
  VCLBI.Chart.Plugin, VCLTee.TeeWorldSeries,
  {$ENDIF}

  BI.DataItem;

type
  TGeoContext=record
  public
    Map : TWorldMap;
    ByCode,
    IsMulti : Boolean;
  end;

  TGeoChart=record
  private
    class function AreEntities(const Text:TDataItem; out AContext:TGeoContext):Boolean; static;
  public
    class function CanReuse(const AChart:TBITChart;
                            const AContext:TGeoContext):Boolean; static;

    class procedure Fill(const AChart:TBITChart;
                         const AValues,AText:TDataItem;
                         const AContext:TGeoContext); static;

    class function Guess(const X,Text:TDataItem;
                         out AContext:TGeoContext):Boolean; static;
  end;

implementation

uses
  BI.Arrays, BI.Persist, BI.Geographic, BI.Compare, BI.Expressions,
  System.SysUtils,

  {$IFDEF FMX}
  System.UITypes,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21_0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  FMXTee.Constants, FMXTee.Engine, FMXTee.Chart, FMXTee.Canvas,
  FMXTee.Series.Map, FMXTee.Editor.Series.WorldMap
  {$ELSE}

  VCL.Graphics,

  VCLTee.TeeConst, VCLTee.TeEngine, VCLTee.Chart,
  VCLTee.TeeMapSeries, VCLTee.TeeWorldSeriesEditor
  {$ENDIF}
  ;

{$IF (TeeVCLBuildVersionInteger >= 161005)}
{$DEFINE NEWWORLD}
{$ENDIF}

type
  TChartSeriesAccess=class(TChartSeries);
  TDataAccess=class(TDataItem);

procedure FillPolygons(const ASeries:TWorldSeries; const AData,AEntity:TDataItem; const ByCode,IsMulti:Boolean);

  // Note: For "AValues.AsTable", first use a THop to investigate if AValues
  // is a "Master" of row data. If so, use "FirstNonNull" (it can only contain one value).
  // If not, then we must create multiple maps, one for each numeric Item in AValues
  function GetFirstNonNull(const AItems:TDataArray; const ARow:TInteger; out AValue:TChartValue):Boolean;
  var t : Integer;
  begin
    for t:=0 to AItems.Count-1 do
        if not AItems[t].Missing[ARow] then
        begin
          AValue:=TBITChart.GetValue(AItems[t],ARow);
          Exit(True);
        end;

    result:=False;
  end;

  procedure SetValue(const APoly:TTeePolygon; const AValue:TChartValue);
  var tmp : Integer;
  begin
    tmp:=APoly.Index;

    ASeries.ZValues.Value[tmp]:=AValue; // slower: APoly.Z:=AValue;

    {$IFNDEF FPC}
    // Speed trick
    TChartSeriesAccess(ASeries).FColors[tmp]:=clTeeColor;
    {$ELSE}
    // Slower
    ASeries.SetNull(APoly.Index,False);
    {$ENDIF}
  end;

  procedure SetPolyValue(const APoly:TTeePolygon; const AIndex:Integer);

    procedure DoSetValue(const AValue:TChartValue);
    {$IFDEF NEWWORLD}
    var tmpPolys : TTeePolygons;
        p : Integer;
    {$ENDIF}
    begin
      SetValue(APoly,AValue);

      {$IFDEF NEWWORLD}
      tmpPolys:=APoly.Siblings;

      for p:=Low(tmpPolys) to High(tmpPolys) do
          SetValue(tmpPolys[p],AValue);
      {$ENDIF}
    end;

  var tmpValue : TChartValue;
  begin
    if AData.AsTable then
    begin
      if GetFirstNonNull(AData.Items.AsArray,AIndex,tmpValue) then
         DoSetValue(tmpValue)
      else
         ASeries.SetNull(AIndex);
    end
    else
      DoSetValue(TBITChart.GetValue(AData,AIndex));
  end;

  function FindByName(const AName:String):TTeePolygon;
  var tmp : String;
  begin
    result:=ASeries.Shapes.ByName[AName];

    if result=nil then
    begin
      tmp:=TGeo.FindSynonym(AName);

      if tmp<>'' then
         result:=ASeries.Shapes.ByName[tmp];
    end;
  end;

  function FindString(const S:String):TTeePolygon;
  begin
    if ByCode then
       result:=ASeries.Shapes.ByCode[S]
    else
       result:=FindByName(S);
  end;

var
  Entity : TEntity;
  HasEntity : Boolean;

  function FindPolygon(const AShapes:TTeePolygonList; const AData:TDataItem; const AIndex:TInteger):TTeePolygon;
  var tmp : String;
      tmpIndex : TInteger;
  begin
    if HasEntity then
    begin
      if ByCode or (AData.Master=Entity.Name) then
         tmp:=Entity.CodeToString(AData,AIndex)
      else
      begin
        // Use THops ?
        case AData.Master.Kind of
          dkInt32: tmpIndex:=AData.Master.Int32Data.IndexOf(AData.Int32Data[AIndex]);
          dkInt64: tmpIndex:=AData.Master.Int64Data.IndexOf(AData.Int64Data[AIndex]);
           dkText: tmpIndex:=AData.Master.TextData.IndexOf(AData.TextData[AIndex]);
        else
          tmpIndex:=-1;
        end;

        if tmpIndex=-1 then
           tmp:=''
        else
           tmp:=Entity.CodeToString(Entity.Name,tmpIndex);
      end;
    end
    else
       tmp:=AData.DataToString(AIndex);

    if tmp='' then
       result:=nil
    else
       result:=FindString(tmp);
  end;

  procedure FillSingle(const AIndex:Integer);
  var tmpPoly : TTeePolygon;
  begin
    tmpPoly:=FindPolygon(ASeries.Shapes,AEntity,AIndex);

    if tmpPoly=nil then
    begin
      // Pending: Add an extra "Unknown" Series value?

      // Set all to Null?
      //raise EBIException.Create('Error Geo entity not found: '+AEntity.DataToString(t));
    end
    else
      SetPolyValue(tmpPoly,AIndex);
  end;

var
  tmpMultiData : TMasterDetail;

  function FindMaster(const AData:TDataItem; const S:String):TInteger;
  var t: TLoopInteger;
  begin
    for t:=0 to AData.Count-1 do
        if AData.DataToString(t)=S then
           Exit(t);

    result:=-1;
  end;

  function FindDetails(const S:String; out ACode:String):TNativeIntArray;
  var t,
      tmp  : Integer;
      tmpIndex : TNativeIntArray;
      tmpMaster : TDataItem;
  begin
    result:=nil;

    if ByCode then
       tmp:=FindMaster(tmpMultiData.Master.ID,S)
    else
       tmp:=FindMaster(tmpMultiData.Master.Name,S);

    if tmp=-1 then
       ACode:=''
    else
    begin
      if ByCode then
         ACode:=S
      else
         ACode:=tmpMultiData.Master.CodeToString(tmpMultiData.Master.ID,tmp);

      tmpMaster:=tmpMultiData.DetailToMaster;

      if TDataAccess(tmpMaster).IMaster.Index=nil then
         tmpMaster.CreateMasterIndex;

      tmpIndex:=TDataAccess(tmpMaster).IMaster.Index;

      for t:=0 to High(tmpIndex) do
          if tmpIndex[t]=tmp then
             result.Append(t);
    end;
  end;

  procedure FillMulti(const AIndex:Integer);

    function GetDetailShapes(const Detail:TNativeIntArray):TTeePolygons;
    var t,
        L : Integer;
        tmpS : String;
        tmp : TTeePolygon;
    begin
      L:=0;

      for t:=0 to High(Detail) do
      begin
        tmpS:=tmpMultiData.Detail.CodeToString(tmpMultiData.Detail.ID,Detail[t]);

        tmp:=ASeries.Shapes.ByCode[tmpS];

        if tmp<>nil then
        begin
          SetLength(result,L+1);
          result[L]:=tmp;
          Inc(L);
        end;
      end;
    end;

  var t : Integer;
      tmpCode,
      tmpS : String;
      tmpShapes : TTeePolygons;
  begin
    tmpS:=AEntity.DataToString(AIndex);

    tmpShapes:=GetDetailShapes(FindDetails(tmpS,tmpCode));

    {$IFDEF NEWWORLD}
    ASeries.Groups.Add(tmpS,tmpCode,tmpShapes);
    {$ENDIF}

    for t:=0 to High(tmpShapes) do
        SetPolyValue(tmpShapes[t],AIndex);
  end;

var t : Integer;
    tmp : TMasterDetail;
    tmpLink : TDataItem;
begin
  if IsMulti then
     TGeo.IsMultiEntity(AEntity,tmpMultiData);

  tmpLink:=TGeo.LinkedTo(AEntity);

  HasEntity:=TMasterDetail.FindDetail(tmpLink.Parent,Entity);

  if not HasEntity then
  begin
    HasEntity:=TMasterDetail.FindMaster(tmpLink.Parent,tmp);

    if HasEntity then
       Entity:=tmp.Master;
  end;

  for t:=0 to AData.Count-1 do
    if not AData.Missing[t] then
       if IsMulti then
          FillMulti(t)
       else
          FillSingle(t);
end;

procedure SetAllNull(const ASeries:TChartSeries);
var t : Integer;
begin
  ASeries.SetNull(0);

  {$IFNDEF FPC}
  // Speed trick
  for t:=1 to ASeries.Count-1 do
      TChartSeriesAccess(ASeries).FColors[t]:=clNone;
  {$ELSE}
      // Slower
      ASeries.SetNull(t);
  {$ENDIF}
end;

class function TGeoChart.CanReuse(const AChart: TBITChart;
                                  const AContext:TGeoContext): Boolean;
begin
  result:=(AChart.SeriesCount=1) and
          (AChart[0] is TWorldSeries) and
          (TWorldSeries(AChart[0]).Map=AContext.Map);
end;

class procedure TGeoChart.Fill(const AChart:TBITChart;
                               const AValues,AText:TDataItem;
                               const AContext:TGeoContext);

  function TrySplitLong(const S:String):String;
  var tmp,
      L : Integer;
  begin
    L:=Length(S);

    if L>40 then
    begin
      tmp:=L div 2;
      result:=Copy(S,1,tmp)+#13#10+Copy(S,tmp+1,Length(S));
    end
    else
       result:=S;
  end;

  function IsCountiesMap(const AMap:TWorldMap):Boolean;
  begin
    result:=(AMap=TWorldMap.wmUSACounties) or (AMap=TWorldMap.wmUSAAlaskaCounties);
  end;

  function FirstNonSubTable(const AData:TDataItem):TDataItem;
  begin
    result:=AData;

    while result.AsTable and
          (result.Items.Count=1) and
          result.Items[0].AsTable do
            result:=result.Items[0];
  end;

  procedure InitValues(var Values:TChartValues);
  var t : Integer;
  begin
    for t:=0 to High(Values) do
        Values[t]:=0;
  end;

  procedure SetDegreesFormat(const ASeries:TChartSeries);
  const DegreesFormat='#,##º';
  begin
    ASeries.GetHorizAxis.AxisValuesFormat:=DegreesFormat;
    ASeries.GetVertAxis.AxisValuesFormat:=DegreesFormat;
  end;

var tmpS : TWorldSeries;
begin
  TGeo.Check;

  // Optimization, call CreateMap constructor to pass Map
  // (this is much faster than normal Create and setting tmpS.Map:=...)
  if CanReuse(AChart,AContext) then
  begin
    tmpS:=TWorldSeries(AChart[0]);
    InitValues(tmpS.MandatoryValueList.Value);
    tmpS.MandatoryValueList.Modified:=True;
  end
  else
  begin
    tmpS:=TWorldSeries.CreateMap(AChart.Owner,AContext.Map);
    tmpS.ParentChart:=AChart;
  end;

  SetAllNull(tmpS);

  {$IFDEF NEWWORLD}
  tmpS.Groups.Clear;

  if AContext.IsMulti then
     tmpS.MapLegend.Style:=TMapLegendStyle.lsGroups
  else
  begin
    if IsCountiesMap(AContext.Map) then // <-- too many polygons
       tmpS.MapLegend.Style:=TMapLegendStyle.lsPalette // <-- show palette
    else
       tmpS.MapLegend.Style:=TMapLegendStyle.lsTexts;
  end;
  {$ENDIF}

  tmpS.Pen.Color:={$IFDEF FMX}TAlphaColors.Silver{$ELSE}clSilver{$ENDIF};

  AChart.ShowHideAxesWalls(False);

  if (AValues<>nil) and (AText<>nil) then
  begin
    FillPolygons(tmpS,FirstNonSubTable(AValues),AText,AContext.ByCode,AContext.IsMulti);

    AChart.Legend.Title.Caption:=TrySplitLong(AValues.Name);
    AChart.Legend.TextStyle:=TLegendTextStyle.ltsLeftValue;

    if tmpS.Map=TWorldMap.wmWorld then
       tmpS.Entities.Antarctica.Visible:=False;
  end;

  // Degrees
  SetDegreesFormat(tmpS);
end;

function ContainsAlaskaHawaii(const Text:TDataItem; const ByCode:Boolean):Boolean;

  function TextContains(const AItems:Array of String):Boolean;
  var tmp : TTextMap;
      t : Integer;
      tmpExists : Boolean;
  begin
    tmp:=TGeo.TextMapOf(Text);

    for t:=Low(AItems) to High(AItems) do
        if tmp.Find(AItems[t],tmpExists)<>-1 then
           Exit(True);

    result:=False;
  end;

begin
  if ByCode then
     result:=TextContains(['AK','HI'])
  else
     result:=TextContains(['Alaska','Hawaii']);
end;

type
  TEntityMap=record
  public
    IsMulti,
    Synonyms : Boolean;
    Map : TWorldMap;
    Entity : TEntity;
  end;

  TEntityMaps=Array of TEntityMap;

  TEntityMapsHelper=record helper for TEntityMaps
  private
    class procedure Check; static;
    function FindMapIn(const AData:TDataItem; out AContext:TGeoContext):Boolean;
  public
    procedure Add(const AMap:TWorldMap;
                  const AEntity:TEntity;
                  const UseSynonyms:Boolean=False);

    procedure AddMulti(const AMap:TWorldMap;
                       const AEntity:TEntity;
                       const UseSynonyms:Boolean=False);

    function Count:Integer; inline;
  end;

var
  IEntityMaps:TEntityMaps=nil;

function TEntityMapsHelper.Count:Integer;
begin
  result:=Length(Self);
end;

class procedure TEntityMapsHelper.Check;
begin
  if IEntityMaps=nil then
  begin
    TGeo.Check;

    IEntityMaps.Add(TWorldMap.wmWorld,TGeo.Entities.Country,True);

    IEntityMaps.AddMulti(TWorldMap.wmWorld,TGeo.Continents);

    {$IFDEF NEWWORLD}
    IEntityMaps.Add(TWorldMap.wmAustraliaStates,TGeo.Entities.Australia.Lands);
    IEntityMaps.Add(TWorldMap.wmAustraliaCounties,TGeo.Entities.Australia.Counties);
    {$ENDIF}

    IEntityMaps.Add(TWorldMap.wmChina,TGeo.Entities.China.Provinces);

    {$IFDEF NEWWORLD}
    IEntityMaps.Add(TWorldMap.wmChinaProvinces,TGeo.Entities.China.Prefectures);
    {$ENDIF}

    IEntityMaps.AddMulti(TWorldMap.wmFrance,TGeo.Entities.France.Regions);
    IEntityMaps.Add(TWorldMap.wmFrance,TGeo.Entities.France.Departements);

    IEntityMaps.AddMulti(TWorldMap.wmGermany,TGeo.Entities.Germany.States);
    IEntityMaps.Add(TWorldMap.wmGermany,TGeo.Entities.Germany.Districts);

    IEntityMaps.AddMulti(TWorldMap.wmJapan,TGeo.Entities.Japan.Regions);
    IEntityMaps.Add(TWorldMap.wmJapan,TGeo.Entities.Japan.Prefectures);

    IEntityMaps.AddMulti(TWorldMap.wmSpain,TGeo.Entities.Spain.Regions);
    IEntityMaps.Add(TWorldMap.wmSpain,TGeo.Entities.Spain.Provinces);

    IEntityMaps.AddMulti(TWorldMap.wmUK,TGeo.Entities.UK.NUTS);
    IEntityMaps.Add(TWorldMap.wmUK,TGeo.Entities.UK.Counties);

    IEntityMaps.Add(TWorldMap.wmUSA,TGeo.Entities.USA.States);
    IEntityMaps.Add(TWorldMap.wmUSACounties,TGeo.Entities.USA.Counties);
  end;
end;

function FindMapEntities(const Text:TDataItem; out AContext:TGeoContext):Boolean;
var t : Integer;
begin
  for t:=0 to High(IEntityMaps) do
      if TGeo.AllFoundIn(Text,IEntityMaps[t].Entity,IEntityMaps[t].Synonyms) then
      begin
        AContext.Map:=IEntityMaps[t].Map;

        if AContext.Map=TWorldMap.wmUSA then
        begin
          if ContainsAlaskaHawaii(Text,AContext.ByCode) then
             AContext.Map:=TWorldMap.wmUSAHawaiiAlaska;
        end
        else
        if AContext.Map=TWorldMap.wmUSACounties then
        begin
          if ContainsAlaskaHawaii(Text,AContext.ByCode) then
             AContext.Map:=TWorldMap.wmUSAAlaskaCounties;
        end;

        Exit(True);
      end;

  result:=False;
end;

class function TGeoChart.AreEntities(const Text:TDataItem; out AContext:TGeoContext):Boolean;

  function IsNumeric(const Text:TDataItem):Boolean;
  var t : Integer;
      tmp : Int64;
  begin
    for t:=0 to Text.Count-1 do
        if TryStrToInt64(Text.TextData[t],tmp) then
           Exit(True);

    result:=False;
  end;

  function FindBestWorldMap:TWorldMap;
  begin
    // Pending: Try to reduce to the smaller map that includes all Text.TextData countries
    result:=TWorldMap.wmWorld;
  end;

begin
  if (Text.Kind=TDataKind.dkText) and (Text.Count>0) and (not IsNumeric(Text)) then
  begin
    AContext.ByCode:=False;

    result:=FindMapEntities(Text,AContext);

    if result then
       if AContext.Map=TWorldMap.wmWorld then
          AContext.Map:=FindBestWorldMap;
  end
  else
    result:=False;
end;

class function TGeoChart.Guess(const X,Text:TDataItem;
                               out AContext:TGeoContext):Boolean;

  function LinkedToGeo(const AData:TDataItem):Boolean;
  var tmp : TDataItem;
  begin
    tmp:=TGeo.LinkedTo(AData);
    result:=(tmp<>nil) and IEntityMaps.FindMapIn(tmp,AContext);
  end;

begin
  TGeo.Check;

  result:=LinkedToGeo(Text) or
          LinkedToGeo(X) or
          ((Text<>nil) and AreEntities(Text,AContext)); // <-- Investigate !
end;

{ TEntityMapsHelper }

procedure TEntityMapsHelper.Add(const AMap: TWorldMap;
                                const AEntity:TEntity;
                                const UseSynonyms: Boolean);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);

  Self[L].Synonyms:=UseSynonyms;
  Self[L].Map:=AMap;
  Self[L].Entity:=AEntity;
end;

procedure TEntityMapsHelper.AddMulti(const AMap:TWorldMap;
                                     const AEntity:TEntity;
                                     const UseSynonyms:Boolean=False);
begin
  Add(AMap,AEntity,UseSynonyms);
  Self[Count-1].IsMulti:=True;
end;

function TEntityMapsHelper.FindMapIn(const AData: TDataItem;
  out AContext: TGeoContext):Boolean;
var t : Integer;
begin
  Check;

  AContext.ByCode:=False;
  AContext.Map:=TWorldMap.wmWorld;

  for t:=0 to High(Self) do
      if Self[t].Entity.Data=AData.Parent then
      begin
        AContext.Map:=Self[t].Map;
        AContext.ByCode:=AData=Self[t].Entity.ID;
        AContext.IsMulti:=Self[t].IsMulti;

        Exit(True);
      end;

  result:=False;
end;

end.
