{*********************************************}
{  TeeBI Software Library                     }
{  TTeeBISource class                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Chart.Source;
{.$DEFINE FMX}

interface

uses
  System.Classes,
  {$IFDEF FMX}
  FMXTee.Engine,
  {$ELSE}
  VCLTee.TeEngine,
  {$ENDIF}
  BI.DataItem, BI.CollectionItem;

type
  TTeeBISource=class(TTeeSeriesSource)
  private
    FItem : TDataCollectionItem;

    function GetData: TDataItem;
    function GetProvider: TComponent;
    procedure SetData(const Value: TDataItem);
    procedure SetProvider(const Value: TComponent);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    class Function Description:String; override;
    class Function Editor:TComponentClass; override;

    Procedure Load; override;
  published
    property Active;
    property Data:TDataItem read GetData write SetData;
    property Provider:TComponent read GetProvider write SetProvider;
    property Series;
  end;

implementation

uses
  System.Types,

  {$IFDEF FMX}
  FMXTee.Canvas,
  FMXBI.Chart.Plugin,
  FMXBI.Editor.Chart.Source,

  {$ELSE}
  VCLTee.TeCanvas,
  VCLBI.Chart.Plugin,
  VCLBI.Editor.Chart.Source,
  {$ENDIF}

  BI.Arrays;

const
  BITeeMsg_DataItem='BI Data';

{ TTeeBISource }

Destructor TTeeBISource.Destroy;
begin
  FItem.Free;
  inherited;
end;

type
  TDataCollectionItemAccess=class(TDataCollectionItem);

constructor TTeeBISource.Create(AOwner: TComponent);
begin
  inherited;
  FItem:=TDataCollectionItem.Create(nil);
end;

procedure TTeeBISource.DefineProperties(Filer: TFiler);
begin
  inherited;
  TDataCollectionItemAccess(FItem).DefineProperties(Filer);
end;

class function TTeeBISource.Description: String;
begin
  result:=BITeeMsg_DataItem;
end;

class function TTeeBISource.Editor: TComponentClass;
begin
  result:=TBISourceEditor;
end;

function TTeeBISource.GetData: TDataItem;
begin
  result:=FItem.Data;
end;

function TTeeBISource.GetProvider: TComponent;
begin
  result:=FItem.Provider;
end;

procedure TTeeBISource.Load;
var
  tmpList : Array of TChartValueList;
  tmpData : TDataArray;

  // Get all (optional) data items for all series ValueLists
  procedure PrepareLists(const AData:TDataItem);
  var t,
      L : Integer;
      tmp : TChartValueList;
  begin
    tmpData:=nil;
    tmpList:=nil;

    for t:=0 to Series.ValuesList.Count-1 do
    begin
      tmp:=Series.ValuesList[t];

      if (tmp<>Series.MandatoryValueList) and (tmp.ValueSource<>'') then
      begin
        tmpData.Add(AData[tmp.ValueSource]);

        L:=Length(tmpList);
        SetLength(tmpList,L+1);
        tmpList[L]:=tmp;
      end;
    end;
  end;

  // Add mandatory value and (optional) text
  procedure AddMandatory(const AData:TDataItem);
  var tmpText,
      tmpItem : TDataItem;
      t : TLoopInteger;
      tmpLabel : String;
  begin
    if Series.XLabelsSource='' then
       tmpText:=nil
    else
       tmpText:=AData[Series.XLabelsSource];

    tmpItem:=AData[Series.MandatoryValueList.ValueSource];

    tmpLabel:='';

    for t:=0 to AData.Count-1 do
    begin
      if tmpText<>nil then
         tmpLabel:=tmpText.DataToString(t);

      Series.Add(TBITChart.GetValue(tmpItem,t),tmpLabel);
    end;
  end;

  // Add (optional) rest of value lists
  procedure AddRest;
  var t : TLoopInteger;
      tt : Integer;
      tmpItem : TDataItem;
      tmpValue : TChartValues;
  begin
    for t:=0 to tmpData.Count-1 do
    begin
      tmpValue:=tmpList[t].Value;
      tmpItem:=tmpData[t];

      for tt:=0 to tmpItem.Count-1 do
          tmpValue[tt]:=TBITChart.GetValue(tmpItem,tt);
    end;
  end;

  procedure DoErrorMandatory;
  begin
    raise EBIException.Create('Error: Missing ValueSource in Series ValueList: '+Series.MandatoryValueList.Name);
  end;

var tmp : TDataItem;
begin
  inherited;

  if Series<>nil then
  begin
    if Series.MandatoryValueList.ValueSource='' then
       DoErrorMandatory;

    tmp:=Data;

    if tmp<>nil then
    begin
      tmp.Load;

      PrepareLists(tmp);

      Series.Clear;
      AddMandatory(tmp);
      AddRest;
    end;
  end;
end;

procedure TTeeBISource.Loaded;
begin
  TDataCollectionItemAccess(FItem).Loaded;
  inherited;
end;

procedure TTeeBISource.SetData(const Value: TDataItem);
begin
  FItem.Data:=Value;
end;

procedure TTeeBISource.SetProvider(const Value: TComponent);
begin
  FItem.Provider:=Value;
end;

initialization
  TeeActivateGroup;
  RegisterClass(TTeeBISource);
  TeeSources.Add(TTeeBISource);
finalization
  TeeSources.Remove(TTeeBISource);
end.
