{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL and Firemonkey)            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.AllData;

interface

uses
  System.Classes, System.Types, Data.DB,
  BI.Arrays, BI.Data, BI.DataSource;

const
  CRLF=#13#10;

type
  TAllData=class
  private
    FStore : String;
  public
    Constructor Create(const AStore:String='');

    function AllData:TDataItem;

    function Find(const AData:String):TDataItem;

    function GetDataOrigins(const AItems: TDataItems): TStringDynArray;
    function GetData(const AItems:TDataItems):TStringDynArray; overload;

    function GetData:String; overload;
    function GetDataArray: TStringDynArray;

    function GetDefinition(const AName:String): String;

    function DataStream(const ACursor:TDataCursor; const Zip:Boolean=False):TStream; overload;
    function MetaStream(const AData:String; const Zip:Boolean=False):TStream; overload;

    property Store:String read FStore;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, BI.Persist, BI.Summary;

{ TAllData }

Constructor TAllData.Create(const AStore: String);
begin
  inherited Create;

  if AStore='' then
     FStore:=TStore.DefaultName
  else
     FStore:=AStore;
end;

function TAllData.AllData: TDataItem;
var
  tmpRemote: Boolean;
  tmpSizeItem,
  tmpStatus: TDataItem;

  procedure ProcessData(const AName:String; const AIndex:Integer);
  var tmp,
      tmpPath : String;

      tmpSize : Int64;

      Data : TDataItem;
  begin
    tmp:=TPath.GetFileNameWithoutExtension(AName);

    result.Items[0].TextData[AIndex]:=tmp;

    if tmpRemote then
       tmpSizeItem.Missing[AIndex]:=True
    else
    begin
      tmpPath:=TStore.FullPath(FStore,tmp);

      if TFile.Exists(tmpPath+TPersistence.Extension) then
      begin
        result.Items[5].DateTimeData[AIndex]:=TFile.GetLastWriteTime(tmpPath+TPersistence.Extension);

        tmpSize:=TBIFileSource.GetFileSize(tmpPath+TPersistence.Extension);

        if TFile.Exists(tmpPath+TDataPersistence.Extension) then
           Inc(tmpSize,TBIFileSource.GetFileSize(tmpPath+TDataPersistence.Extension));

        tmpSizeItem.Int64Data[AIndex]:=tmpSize;
      end
      else
        tmpSizeItem.Missing[AIndex]:=True;
    end;

    if tmpSizeItem.Missing[AIndex] then
       result.Items[5].Missing[AIndex]:=True;

    Data:=TStore.Load(FStore,tmp,function(const Sender:TObject; const Text:String):Boolean
      begin
        tmpStatus.TextData[AIndex]:=Text;
        result:=True;
      end);

    if Data<>nil then
    begin
      result.Items[1].Int32Data[AIndex]:=Data.Items.Count;
      result.Items[2].Int32Data[AIndex]:=Data.TotalColumns;
      result.Items[3].Int64Data[AIndex]:=Data.TotalRows;
      tmpStatus.TextData[AIndex]:='OK';
    end;
  end;

var H,t : Integer;
    Data : TStringDynArray;
begin
  result:=TDataItem.Create(True);
  result.Name:=FStore;

  result.Items.Add('Name',dkText);
  result.Items.Add('Data',dkInt32);
  result.Items.Add('Items',dkInt32);
  result.Items.Add('Rows',dkInt64);
  tmpSizeItem:=result.Items.Add('Size',dkInt64);
  result.Items.Add('Modified',dkDateTime);
  tmpStatus:=result.Items.Add('Status',dkText);

  Data:=GetDataArray;

  H:=High(Data);

  result.Resize(H+1);

  tmpRemote:=TStore.IsRemote(FStore);

  for t:=Low(Data) to H do
      ProcessData(Data[t],t);
end;

function TAllData.GetDataArray: TStringDynArray;
begin
  result:=TStore.AllData(FStore);
end;

type
  TDataDefAccess=class(TDataDefinition);

function TAllData.GetDefinition(const AName:String): String;
var tmp : TDataDefinition;
begin
  tmp:=TStore.GetDefinition(FStore,AName);
  try
    result:=TDataDefAccess(tmp).Strings.Text;
  finally
    tmp.Free;
  end;
end;

function TAllData.GetData: String;
var H,t : Integer;
    Data : TStringDynArray;
begin
  result:='';

  Data:=GetDataArray;

  H:=High(Data);

  for t:=Low(Data) to H do
  begin
    result:=result+TPath.GetFileNameWithoutExtension(Data[t]);

    if t<H then
       result:=result+CRLF;
  end;
end;

function TAllData.GetData(const AItems: TDataItems): TStringDynArray;
var t : Integer;
begin
  SetLength(result,AItems.Count);

  for t:=0 to AItems.Count-1 do
      result[t]:=AItems[t].Name;
end;

function TAllData.GetDataOrigins(const AItems: TDataItems): TStringDynArray;
var t : Integer;
begin
  SetLength(result,AItems.Count);

  for t:=0 to AItems.Count-1 do
      result[t]:=TStore.OriginOf(AItems[t],FStore);
end;

type
  TProviderAccess=class(TDataProvider);

// Return a binary stream with ACursor data contents
function TAllData.DataStream(const ACursor:TDataCursor; const Zip:Boolean=False):TStream;

  // If we need compression, do zip AStream, return a compressed stream
  function TryZip(const AStream:TStream):TStream;
  begin
    if Zip then
       result:=TStore.ZipStream(AStream)
    else
       result:=AStream;
  end;

  // Return the data item that has a valid "delay handler".
  // Go up to data Parent in the hierarchy until handler is found.
  function DelayHandlerOf(const AData:TDataItem):TDataItem;
  begin
    result:=AData;

    while result.Provider=nil do
    begin
      result:=result.Parent;

      if result=nil then
         break;
    end;
  end;

  // Return the next sibling data of AData in AHandler parent items array
  function NextSiblingOf(const AData,AHandler:TDataItem):TDataItem;
  var tmpData : Integer;
      tmpCount : TInteger;
  begin
    if AData=AHandler then
       result:=nil
    else
    begin
      tmpData:=AHandler.Items.AsArray.IndexOf(AData);

      if tmpData=-1 then
         result:=nil // error??? Exit(TryZip(AHandler.DelayHandler.GetStream([Data])))
      else
      begin
        if AHandler.AsTable then
           tmpCount:=AHandler.Count
        else
           tmpCount:=AHandler.Items.Count;

        if tmpData=tmpCount-1 then
           result:=nil
        else
           result:=AHandler.Items[tmpData+1];
      end;
    end;
  end;

var tmpD,
    tmpNext : TDataItem;
begin
  result:=nil;

  if ACursor.Data<>nil then
  begin
    tmpD:=DelayHandlerOf(ACursor.Data);

    if tmpD<>nil then
    begin
      // Pending: Pass ACursor Start and Max params to GetStream,
      // to limit the number of rows in the returned Stream

      if ACursor.Items=nil then
      begin
        // Try to search the "next" sibling data
        tmpNext:=NextSiblingOf(ACursor.Data,tmpD);

        result:=TryZip(TProviderAccess(tmpD.Provider).GetStream(ACursor.Data,tmpNext));
      end
      else
        result:=TryZip(TProviderAccess(tmpD.Provider).GetStream(ACursor.DataItems));
    end;
  end;
end;

function TAllData.Find(const AData:String):TDataItem;
begin
  result:=TStore.OriginToData(nil,FStore,AData);
end;

function TAllData.MetaStream(const AData: String; const Zip:Boolean=False): TStream;
begin
  result:=TStore.DataToStream(FStore,AData,Zip);
end;

end.
