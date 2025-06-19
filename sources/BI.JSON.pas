{*********************************************}
{  TeeBI Software Library                     }
{  JSON data import                           }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.JSON;

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, BI.DataSource, BI.Persist;

type
  EBIJSON=class(EBIException);

  TJSONEngine=class abstract
  protected
    procedure ArrayChild(const Index:Integer); virtual; abstract;
    function AsBoolean:Boolean; virtual; abstract;
    function AsDouble:Double; virtual; abstract;
    function AsString:String; virtual; abstract;
    function EnterArray:Integer; virtual; abstract;
    function EnterObject:Integer; virtual; abstract;
    function IsArray:Boolean; virtual; abstract;
    function IsBoolean:Boolean; virtual; abstract;
    function IsNull:Boolean; virtual; abstract;
    function IsNumber:Boolean; virtual; abstract;
    function IsObject:Boolean; virtual; abstract;
    function IsString:Boolean; virtual; abstract;
    function ObjectChild(const Index:Integer):String; virtual; abstract;
    procedure Parse(const Text:String); virtual; abstract;
    procedure Pop; virtual; abstract;
  end;

  TJSONEngineClass=class of TJSONEngine;

  TBIJSONFormat=(&Normal, &Array);

  TBIJSON=class(TBIHierarchicalSource)
  private
    JSON : TJSONEngine;

    procedure AppendArray(const AIndex:TInteger; const AData:TDataItem);
    procedure AppendTo(const AData:TDataItem);
    procedure CheckEngine;
    procedure DoAppend(const Index:TInteger; const Data:TDataItem);
  public
    class var
      EngineClass : TJSONEngineClass;

    Format : TBIJSONFormat;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    Constructor CreateEngine(const AEngine:TJSONEngine);
    Destructor Destroy; override;

    class function FileFilter:TFileFilters; override;

    class function FromFile(const AFileName:String; const AFormat:TBIJSONFormat):TDataItem; overload;

    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; overload; override;
    function ImportFile(const FileName:String):TDataArray; override;

    function ImportText(const Text:String): TDataItem;

    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIJSONExport=class(TBITextExport)
  private
    const
      Tab=#9;

    var
      IsFirst : Boolean;
      IItems : TStrings;

    procedure EmitDetail(const Data:TDataItem; const AIndex:TCursorIndex; const AItems:TStrings; const Indent:String);
    procedure EmitRow(const AIndex:TInteger; const Items:TDataArray; const AItems:TStrings; const Indent:String);
    procedure EmitRows(const AIndex:TInteger);
    function Escape(const S:String):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Header : Boolean;

    class function AsString(const AData: TDataItem; const Header:Boolean): String; overload; static;
    class function FileFilter: TFileFilters; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

implementation

uses
  System.SysUtils,
  {$IFDEF FPC}
  BI.FPC, BI.JSON.FPC,
  {$ELSE}
  System.Generics.Collections,
  System.IOUtils,
  BI.JSON.Standard,
  {$ENDIF}
  BI.Expression, BI.Languages.English;

{ TBIJSON }

Constructor TBIJSON.Create(const Definition: TDataDefinition; const MultiThread: Boolean);
begin
  inherited;

  if Definition<>nil then
     if SameText(Definition['FORMAT'],'ARRAY') then
        Format:=TBIJSONFormat.&Array;
end;

Constructor TBIJSON.CreateEngine(const AEngine:TJSONEngine);
begin
  Create;
  JSON:=AEngine;
end;

Destructor TBIJSON.Destroy;
begin
  JSON.Free;
  inherited;
end;

function TBIJSON.Import(const Folder: String; Recursive: Boolean): TDataArray;
begin
  result:=Import(Folder,'*.json',ExcludePattern,Recursive);
end;

type
  TDataAccess=class(TDataItem);

procedure TBIJSON.DoAppend(const Index:TInteger; const Data:TDataItem);

  procedure SetAsNumber(const Col:TDataItem);

    procedure ChangeToInt64;
    var t : TLoopInteger;
    begin
      TDataAccess(Col).FKind:=dkInt64;
      Col.Resize(Col.Count);

      for t:=0 to Col.Count-2 do
          Col.Int64Data[t]:=Col.Int32Data[t];

      Col.Int32Data:=nil;
    end;

    procedure ChangeToDouble;
    var t : TLoopInteger;
        tmp : TDataKind;
    begin
      tmp:=Col.Kind;

      TDataAccess(Col).FKind:=dkDouble;
      Col.Resize(Col.Count);

      if tmp=TDataKind.dkInt32 then
      begin
        for t:=0 to Col.Count-2 do
            Col.DoubleData[t]:=Col.Int32Data[t];

        Col.Int32Data:=nil;
      end
      else
      begin
        for t:=0 to Col.Count-2 do
            Col.DoubleData[t]:=Col.Int64Data[t];

        Col.Int64Data:=nil;
      end;
    end;

  var
    tmpS : String;

    procedure TryAsInt64;
    var tmpInt64 : Int64;
    begin
      if TryStrToInt64(tmpS,tmpInt64) then
      begin
        if Col.Kind<>TDataKind.dkInt64 then
           ChangeToInt64;

        Col.Int64Data[Index]:=tmpInt64;
      end
      else
      begin
        if Col.Kind<>TDataKind.dkDouble then
           ChangeToDouble;

        Col.DoubleData[Index]:=JSON.AsDouble;
      end;
    end;

  var tmpInt32 : Int32;
  begin
    tmpS:=JSON.AsString;

    if Col.Kind=TDataKind.dkInt32 then
    begin
      if TryStrToInt(tmpS,tmpInt32) then
         Col.Int32Data[Index]:=tmpInt32
      else
         TryAsInt64;
    end
    else
    if Col.Kind=TDataKind.dkInt64 then
       TryAsInt64
    else
    if Col.Kind=TDataKind.dkDouble then
       Col.DoubleData[Index]:=JSON.AsDouble
    else
    begin
      if Col.Kind<>TDataKind.dkText then
         ChangeToText(Col,Index,Col.Count);

      Col.TextData[Index]:=tmpS;
    end;
  end;

  function NewDataItems(const AName:String):TDataItem;
  begin
    result:=TDataItem.Create(not Hierarchical);
    result.Name:=AName;
    Data.Items.Add(result);
  end;

  function NewArray(const AKey:String):TDataItem;
  begin
    result:=NewDataItems(AKey);

    if not Hierarchical then
       result.Master:=result.Parent;
  end;

  function GuessItemOfKind(const AKind:TDataKind):TDataItem;
  var t : Integer;
  begin
    result:=nil;

    for t:=0 to Data.Items.Count-1 do
        if Data.Items[t].Kind=AKind then
        begin
          result:=Data.Items[t];
          break;
        end;

    if result=nil then
    begin
      result:=Data.Items.Add(AKind.ToString,AKind);
      result.Resize(Data.Count);
    end;

    if Index>=result.Count then
       result.Resize(Index+1);
  end;

var Col : TDataItem;
    Key : String;
    t,
    tt,
    tmpCount : Integer;
    tmp : String;
    tmpItemsCount : Integer;
begin
  if JSON.IsObject then
  begin
    tmpCount:=JSON.EnterObject;

    if tmpCount>0 then
    begin
      tmpItemsCount:=Data.Items.Count;

      if tmpCount<>tmpItemsCount then
         for t:=0 to tmpItemsCount-1 do
             Data.Items[t].Missing[Index]:=True;

      for t:=0 to tmpCount-1 do
      begin
        Key:=JSON.ObjectChild(t);

        if Hierarchical then
           Col:=nil
        else
           Col:=Data.Items.Find(Key);

        if Col=nil then
        begin
          if JSON.IsObject then
             Col:=NewDataItems(Key)
          else
          if JSON.IsArray then
             Col:=NewArray(Key)
          else
          if JSON.IsNumber then
             Col:=Data.Items.Add(Key,TDataKind.dkInt32) // Start as minsize Int32
          else
          if JSON.IsBoolean then
             Col:=Data.Items.Add(Key,TDataKind.dkBoolean)
          else
             Col:=Data.Items.Add(Key,TDataKind.dkText);

          if not JSON.IsArray then
          begin
            if Hierarchical and (not JSON.IsObject) then
               Col.Resize(1)
            else
               Col.Resize(Data.Count);

            if Index>0 then
               for tt:=0 to Index-1 do
                   Col.Missing[tt]:=True;
          end;
        end;

        if JSON.IsNull then
           Col.Missing[Index]:=True
        else
        begin
          if tmpCount<>tmpItemsCount then
             Col.Missing[Index]:=False;

          if JSON.IsObject then
             DoAppend(Index,Col)
          else
          if JSON.IsArray then
             AppendArray(Index,Col)
          else
          if JSON.IsNumber then
             SetAsNumber(Col)
          else
          if JSON.IsBoolean then
             // Pending:
             //
             // if Col.Kind<>TDataKind.dkBoolean then
             //  ChangeToBoolean(Col,Index,Col.Count);

             Col.BooleanData[Index]:=JSON.AsBoolean
          else
          begin
            if Col.Kind<>TDataKind.dkText then
               ChangeToText(Col,Index,Col.Count);

             Col.TextData[Index]:=JSON.AsString;
          end;
        end;

        JSON.Pop;
      end;
    end;
  end
  else
  if JSON.IsNumber then
  begin
    Col:=GuessItemOfKind(TDataKind.dkDouble);
    Col.DoubleData[Index]:=JSON.AsDouble;
  end
  else
  if JSON.IsBoolean then
  begin
    Col:=GuessItemOfKind(TDataKind.dkBoolean);
    Col.BooleanData[Index]:=JSON.AsBoolean;
  end
  else
  if JSON.IsString then
  begin
    Col:=GuessItemOfKind(TDataKind.dkText);
    Col.TextData[Index]:=JSON.AsString;
  end
  else
  if JSON.IsNull then
  begin
    Col:=GuessItemOfKind(TDataKind.dkText);
    Col.Missing[Index]:=True;
  end
  else
  if JSON.IsArray then
     AppendArray(Index,NewArray(''))
  else
  begin
    tmp:={$IFNDEF FPC}System.{$ENDIF}SysUtils.Format(BIMsg_JSON_WrongClass,[JSON.ClassName]);

    if not CallOnError(tmp) then
       raise EBIJSON.Create(tmp);
  end;
end;

class function TBIJSON.FromFile(const AFileName: String;
  const AFormat: TBIJSONFormat): TDataItem;
var tmp : TBIJSON;
begin
  if AFormat=TBIJSONFormat.Normal then
     result:=TBIJSON.FromFile(AFileName)
  else
  begin
    tmp:=TBIJSON.Create;
    try
      tmp.Format:=AFormat;
      result:=TBISource.FromData(tmp.ImportFile(AFileName));
    finally
      tmp.Free;
    end;
  end;
end;

procedure TBIJSON.AppendArray(const AIndex:TInteger; const AData:TDataItem);

  procedure ResizeIndex(const ACount:TInteger);
  var tmp : TInteger;
      t : Integer;
  begin
    // Fix sub-sub... nested data items
    tmp:=TDataAccess(AData).IMaster.Index.Count;

    if tmp<AIndex then
    begin
      TDataAccess(AData).IMaster.Index.Resize(AIndex);

      if tmp>0 then
         for t:=tmp to AIndex-1 do
             if t=0 then
                TDataAccess(AData).IMaster.Index[t]:=0
             else
                TDataAccess(AData).IMaster.Index[t]:=TDataAccess(AData).IMaster.Index[t-1]
    end;

    TDataAccess(AData).IMaster.AppendIndex(ACount);
  end;

var t : TLoopInteger;
    tmp : TInteger;
    tmpCount : Integer;
    tmpData : TDataItem;
begin
  tmpCount:=JSON.EnterArray;

  if not Hierarchical then
     ResizeIndex(tmpCount);

  if tmpCount>0 then
  begin
    // Resize and append all array items

    if Hierarchical then
    begin
      for t:=0 to tmpCount-1 do
      begin
        JSON.ArrayChild(t);

        tmpData:=TDataItem.Create;
        AData.Items.Add(tmpData);
        //tmpData.Resize(1);

        DoAppend(0,tmpData);

        JSON.Pop;
      end;
    end
    else
    begin
      tmp:=AData.Count;

      AData.Resize(tmp+tmpCount);

      for t:=0 to tmpCount-1 do
      begin
        JSON.ArrayChild(t);
        DoAppend(tmp+t,AData);
        JSON.Pop;
      end;
    end;
  end;
end;

procedure TBIJSON.AppendTo(const AData:TDataItem);
var tmp : TInteger;
begin
  if JSON.IsArray then
     AppendArray(AData.Count,AData)
  else
  begin
    if Hierarchical then
       tmp:=0
    else
    begin
      tmp:=AData.Count;
      AData.Resize(tmp+1);
    end;

    DoAppend(tmp,AData);
  end;
end;

procedure TBIJSON.CheckEngine;
begin
  if JSON=nil then
     if EngineClass=nil then
        JSON:={$IFDEF FPC}TFPCJSON{$ELSE}TStandardJSON{$ENDIF}.Create
     else
        JSON:=EngineClass.Create;
end;

function TBIJSON.Import(const Strings: TStrings): TDataArray;
var t : Integer;
    Data : TDataItem;
    tmpCancel : Boolean;
begin
  if Format=TBIJSONFormat.Normal then
     Data:=ImportText(Strings.Text)
  else
  begin
    CheckEngine;

    tmpCancel:=False;
    DoProgress(0,tmpCancel);

    Data:=TDataItem.Create(True);
    try
      // ie. MongoDB restaurants.json format (multiple objects, one per line):
      for t:=0 to Strings.Count-1 do
      begin
        JSON.Parse(Strings[t]);
        AppendTo(Data);

        if Assigned(OnProgress) then
        begin
          if Assigned(OnProgress) then
             DoProgress(t*100/Strings.Count,tmpCancel);

          if tmpCancel then
          begin
            Data.Free;
            Data:=nil;
            break;
          end;
        end;
      end;

    except
      on Exception do
      begin
        Data.Free;
        raise;
      end;
    end;
  end;

  SetLength(result,1);
  result[0]:=Data;
end;

function TBIJSON.ImportFile(const FileName: String): TDataArray;
begin
  if Supports(TPath.GetExtension(FileName)) then
     result:=DoImportFile(FileName)
  else
     result:=inherited;
end;

function TBIJSON.ImportText(const Text:String): TDataItem;
begin
  CheckEngine;

  JSON.Parse(Text);

  result:=TDataItem.Create(not Hierarchical);
  try
    AppendTo(result);
  except
    on Exception do
    begin
      result.Free; // <-- Avoid memory leak
      raise;
    end;
  end;
end;

class function TBIJSON.FileFilter:TFileFilters;
begin
  result:=nil;
  result.Add('JSON files','*.json');
end;

class function TBIJSON.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.JSON');
end;

{ TBIJSONExport }

// http://www.ietf.org/rfc/rfc4627.txt  Section: 2.5 (invalid characters)
function TBIJSONExport.Escape(const S:String):String;
var t : Integer;
begin
  result:=S;

  for t:=1 to Length(result) do
      if Ord(result[t])<$1F then
         result[t]:=' ';
end;

class function TBIJSONExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('JSON','*.json');
end;

class function TBIJSONExport.Supports(const Extension: String): Boolean;
begin
  result:=TBIJSON.Supports(Extension);
end;

procedure TBIJSONExport.EmitRow(const AIndex:TInteger; const Items:TDataArray; const AItems:TStrings; const Indent:String);

  function AsBoolean(const AItem:TDataItem):String;
  begin
    if AItem.BooleanData[AIndex] then
       result:='"TRUE"'
    else
       result:='"FALSE"';
  end;

  function AsString(const AItem:TDataItem):String;
  begin
    if AItem.Kind=dkBoolean then
       result:=AsBoolean(AItem)
    else
    begin
      result:=DataToString(AItem,AIndex);

      if AItem.Kind=TDataKind.dkText then
         result:=Escape(result);

      case AItem.Kind of
        dkText,
        dkDateTime : result:='"'+result+'"';
      end;
    end;
  end;

  procedure EmitItems(const AItem:TDataItem);
  begin
    EmitRow(0,AItem.Items.AsArray,AItems,Indent+Tab);
  end;

  procedure EmitTable(const AItem:TDataItem);
  var tmpData : TDataArray;
  begin
    if AItem is TDetailData then
    begin
      tmpData:=nil;
      tmpData.Add(Data);

      EmitDetail(TDetailData(AItem).Detail,TDataCursor.MasterDetailIndex(TDetailData(AItem).Detail,tmpData,AIndex),AItems,Indent+Tab);
    end
    else
    if TDataAccess(AItem).HasMaster then
       EmitDetail(AItem,TDataAccess(AItem).IMaster.GetIndex(AItem,AIndex),AItems,Indent+Tab)
    else
       EmitRow(AIndex,AItem.Items.AsArray,AItems,Indent+Tab);
  end;

var H, t : Integer;
    Col : TDataItem;
    tmp : String;
    tmpName : String;
begin
  AItems.Add(Indent+'{');

  H:=High(Items);

  for t:=0 to H do
  begin
    Col:=Items[t];

    tmpName:=Indent+Tab+'"'+Escape(Col.Name)+'":';

    if Col.AsTable then
    begin
      AItems.Add(tmpName);

      EmitTable(Col);

      if t<H then
         AItems.Add(Indent+Tab+',');
    end
    else
    if Col.Kind=TDataKind.dkUnknown then
    begin
      //AItems.Add(tmpName+' [');

      EmitItems(Col);

      //AItems.Add(Indent+Tab+'],');
    end
    else
    begin
      if Col.Missing[AIndex] then
         tmp:=tmpName+' "NULL"'
      else
         tmp:=tmpName+' '+AsString(Col);

      if t<H then
         AItems.Add(tmp+',')
      else
         AItems.Add(tmp);
    end;
  end;

  AItems.Add(Indent+'}');
end;

procedure TBIJSONExport.EmitDetail(const Data: TDataItem; const AIndex:TCursorIndex; const AItems:TStrings; const Indent:String);
var H : TInteger;
    t : TLoopInteger;
begin
  AItems.Add(Indent+'[');

  H:=High(AIndex);

  for t:=0 to H do
  begin
    EmitRow(AIndex[t],Data.Items.AsArray,AItems,Indent+Tab);

    if t<H then
       AItems.Add(Indent+Tab+',');
  end;

  AItems.Add(Indent+']');
end;

procedure TBIJSONExport.EmitRows(const AIndex:TInteger);
var tmpLast : String;
begin
  if IsFirst then
     IsFirst:=False
  else
  begin
    tmpLast:=IItems[IItems.Count-1];
    IItems[IItems.Count-1]:=tmpLast+',';
  end;

  EmitRow(AIndex,Items,IItems,Tab);
end;

class function TBIJSONExport.AsString(const AData: TDataItem;
  const Header: Boolean): String;
var tmp : TBIJSONExport;
begin
  tmp:=TBIJSONExport.Create;
  try
    tmp.Header:=Header;
    tmp.Data:=AData;
    result:=tmp.AsString;
  finally
    tmp.Free;
  end;
end;

procedure TBIJSONExport.DoEmit(const AItems: TStrings);

  procedure AddHeader(const AItems:TStrings);
  var t, H : Integer;
      tmp : String;
  begin
    H:=High(Items);

    for t:=0 to H do
    begin
      tmp:='{ "name": "'+Escape(Items[t].Name)+'" ';

      tmp:=tmp+'}';

      if t<H then
         tmp:=tmp+',';

      AItems.Add(Tab+Tab+tmp);
    end;
  end;

  procedure AddHeaders;
  begin
    AItems.Add('{');

    AItems.Add(Tab+'"data": { "name": "'+Escape(Cursor.Data.Name)+'", "Items": '+IntToStr(Items.Count)+'},');

    AItems.Add(Tab+'"Items" : [');
    AddHeader(AItems);
    AItems.Add(Tab+'],');

    AItems.Add(Tab+'"rows" : [');
  end;

var OldDecimal : Char;
begin
  OldDecimal:=FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator:='.';

    if SchemaOnly then
       AddHeaders
    else
    begin
      if Header then
         AddHeaders
      else
         AItems.Add('[');

      IsFirst:=True;
      IItems:=AItems;

      Cursor.Loop(EmitRows);

      if Header then
      begin
        AItems.Add(Tab+']');
        AItems.Add('}');
      end
      else
        AItems.Add(']');
    end;

  finally
    FormatSettings.DecimalSeparator:=OldDecimal;
  end;
end;

initialization
  TBIJSON.EngineClass:=nil;

  TBIFileImporters.RegisterClass(TBIJSON);
  TBIExporters.RegisterClass(TBIJSONExport);
finalization
  TBIExporters.UnRegisterClass(TBIJSONExport);
  TBIFileImporters.UnRegisterClass(TBIJSON);
end.
