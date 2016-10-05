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
  BI.Arrays, BI.Arrays.Strings, BI.Data, BI.DataSource;

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

    function GetDataOrigins(const AItems: TDataItems): TStringArray;
    function GetData(const AItems:TDataItems):TStringArray; overload;

    function GetData:String; overload;
    function GetDataArray: TStringArray;

    function GetDefinition(const AName:String): String;

    function DataStream(const ACursor:TDataCursor; const Zip:Boolean=False):TStream; overload;
    function MetaStream(const AData:TDataItem; const Zip:Boolean=False):TStream; overload;

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

function GetFileSizeAndDate(const AFile1,AFile2:String; out ADate:TDateTime; out ASize:Int64):Boolean;
begin
  if TFile.Exists(AFile1) then
  begin
    ADate:=TFile.GetLastWriteTime(AFile1);
    ASize:=TBIFileSource.GetFileSize(AFile1);

    if TFile.Exists(AFile2) then
       Inc(ASize,TBIFileSource.GetFileSize(AFile2));

    result:=True;
  end
  else
    result:=False;
end;

function TAllData.AllData: TDataItem;
var
  tmpRemote: Boolean;
  tmpSizeItem,
  tmpStatus: TDataItem;

  procedure ProcessData(const AName:String; const AIndex:Integer);
  var tmp,
      tmpPath : String;

      tmpDate : TDateTime;
      tmpSize : Int64;

      Data : TDataItem;
  begin
    tmp:=TPath.GetFileNameWithoutExtension(AName);

    // Data Name
    result.Items[0].TextData[AIndex]:=tmp;

    if tmpRemote then
       tmpSizeItem.Missing[AIndex]:=True
    else
    begin
      // Data Last DateTime modified, and Data Size
      tmpPath:=TStore.FullPath(FStore,tmp);

      if GetFileSizeAndDate(tmpPath+TPersistence.Extension,
                            tmpPath+TDataPersistence.Extension,
                            tmpDate,tmpSize) then
      begin
        result.Items[5].DateTimeData[AIndex]:=tmpDate;
        tmpSizeItem.Int64Data[AIndex]:=tmpSize;
      end
      else
        tmpSizeItem.Missing[AIndex]:=True;
    end;

    if tmpSizeItem.Missing[AIndex] then
       result.Items[5].Missing[AIndex]:=True;

    // Load Data structure info
    Data:=TStore.Load(FStore,tmp,function(const Sender:TObject; const Text:String):Boolean
      begin
        tmpStatus.TextData[AIndex]:=Text;
        result:=True;
      end);

    if Data<>nil then
    begin
      // Data metrics
      result.Items[1].Int32Data[AIndex]:=Data.Items.Count;
      result.Items[2].Int32Data[AIndex]:=Data.TotalColumns;
      result.Items[3].Int64Data[AIndex]:=Data.TotalRows;

      tmpStatus.TextData[AIndex]:='OK';
    end;
  end;

var t : Integer;
    Data : TStringArray;
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

  result.Resize(Data.Count);

  tmpRemote:=TStore.IsRemote(FStore);

  for t:=0 to High(Data) do
      ProcessData(Data[t],t);
end;

function TAllData.GetDataArray: TStringArray;
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
    Data : TStringArray;
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

function TAllData.GetData(const AItems: TDataItems): TStringArray;
var t : Integer;
begin
  SetLength(result,AItems.Count);

  for t:=0 to AItems.Count-1 do
      result[t]:=AItems[t].Name;
end;

function TAllData.GetDataOrigins(const AItems: TDataItems): TStringArray;
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

function TAllData.MetaStream(const AData: TDataItem; const Zip:Boolean=False): TStream;
begin
  result:=TStore.DataToStream(AData,Zip);
end;

end.
