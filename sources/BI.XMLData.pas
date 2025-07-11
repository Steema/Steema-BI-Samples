{*********************************************}
{  TeeBI Software Library                     }
{  XML data import                            }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.XMLData;

interface

{

  This unit contains an ABSTRACT class to import and export text data in
  XML format into TeeBI TDataItem structures.

  A new XML engine can be supported writing a new derived class implementing
  the virtual methods.

  See the different BI.XMLData.* units for actual implementations of this
  abstraction supporting the following XML engines:

  Unit                   Description
  ------------------     -----------------------------------------------------
  BI.XMLData.MSXML       The standard XML that comes with Microsoft Windows
  BI.XMLData.OXml        https://code.google.com/p/omnixml
  BI.XMLData.Omni        http://www.kluug.net/omnixml.php
  BI.XMLData.FPC         XML support in FreePascal (Lazarus rtl)

}


uses
  System.Classes,
  BI.Arrays, BI.DataItem, BI.DataSource;

type
  EBIXML=class(EBIException);

  TXmlEngine=class abstract
  public
    Constructor Create(const AOwner:TComponent); virtual; abstract;

    function Attribute(const Index:Integer):String; virtual; abstract;
    function AttributeCount:Integer; virtual; abstract;
    function AttributeName(const Index: Integer):String; virtual; abstract;
    function Count:Integer; virtual; abstract;
    procedure FromString(const Text:String); virtual; abstract;
    function GetAttribute(const AName:String):String;
    function HasParent:Boolean; virtual; abstract;
    function HasText:Boolean; virtual; abstract;
    function IsValid:Boolean; virtual; abstract;
    procedure Item(const Index: Integer); virtual; abstract;
    procedure LoadFromFile(const Filename: String); virtual; abstract;
    function Name:String; virtual; abstract;
    procedure Parent; virtual; abstract;
    function Parse(const Text:String):Boolean; virtual;
    procedure Root; virtual; abstract;
    function Text:String; virtual; abstract;
  end;

  TXmlEngineClass=class of TXmlEngine;

  TBIXML=class(TBIHierarchicalSource)
  private
    FExclude : TTextArray;

    procedure DoAppend(const Data:TDataItem; const XML:TXmlEngine; const CallProgress:Boolean);
  protected
    XML : TXmlEngine;
  public
    class var
      EngineClass : TXmlEngineClass;

    Constructor CreateEngine(const AEngine:TXmlEngine);
    Destructor Destroy; override;

    class function CreateParser:TXmlEngine; static;

    class function FileFilter: TFileFilters; override;
    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; overload; override;

    function ImportText(const Text:String): TDataItem;

    function Parse(const Text:String): Boolean;
    class function Supports(const Extension:String):Boolean; override;

    property ExcludeNodes:TTextArray read FExclude;
  end;

  TBIXMLEmit=(Header,Simple);

  TBIXMLExport=class(TBITextExport)
  private
    const
       Tab=#9;

    var
      IItems : TStrings;

    procedure EmitDetail(const Data:TDataItem; const AIndex:TCursorIndex; const Items:TStrings; const Indent:String);
    procedure EmitRow(const AIndex:TInteger; const AData:TDataArray; const Items:TStrings; const Indent:String);
    procedure EmitRows(const AIndex:TInteger);
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Emit : TBIXMLEmit;

    class function FileFilter: TFileFilters; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

implementation

uses
  System.SysUtils,
  {$IFDEF FPC}
  BI.XMLData.FPC,
  {$ELSE}
  BI.XMLData.MSXML,
  System.Character,
  {$ENDIF}
  BI.Languages.English;

{ TXmlEngine }

function TXmlEngine.Parse(const Text:String):Boolean;
begin
  FromString(Text);
  result:=IsValid;
end;

{ TBIXML }

Constructor TBIXML.CreateEngine(const AEngine: TXmlEngine);
begin
  Create;
  Xml:=AEngine;
end;

Destructor TBIXML.Destroy;
begin
  Xml.Free;
  inherited;
end;

function TBIXML.Import(const Folder:String; Recursive: Boolean): TDataArray;
begin
  result:=Import(Folder,'*.xml',ExcludePattern,Recursive);
end;

function TBIXML.Import(const Strings: TStrings): TDataArray;
var tmp : TDataItem;
begin
  tmp:=ImportText(Strings.Text);

  if tmp=nil then
     result:=nil
  else
  begin
    SetLength(result,1);
    result[0]:=tmp;
  end;
end;

type
  TDataAccess=class(TDataItem);

procedure TBIXML.DoAppend(const Data:TDataItem; const Xml:TXmlEngine; const CallProgress:Boolean);

  // Returns the non-detail top Parent of AData
  function TopParent(const AData:TDataItem):TDataItem;
  begin
    result:=AData.Parent;

    while result<>nil do
       if (result.Parent=nil) or (result.Master<>nil) then
          Exit
       else
          result:=result.Parent;
  end;

  function ResizeItem(const AData:TDataItem):TInteger;
  var tmp : TDataItem;
  begin
    tmp:=TopParent(AData);

    if tmp.Master=nil then
    begin
      if tmp.Count=0 then
         result:=0
      else
         result:=tmp.Count-1;

      if AData.Count<=result then
      begin
        AData.Resize(result+1);

        if tmp.Count<>AData.Count then
           tmp.Resize(AData.Count);
      end;
    end
    else
    if AData.Parent.Items.AsArray.IndexOf(AData)=0 then
    begin
      result:=tmp.Count;
      tmp.Resize(result+1);
      AData.Resize(result+1); // <-- pending to fix: "AData=tmp" but "tmp.Kind<>Unknown"
    end
    else
    begin
      if tmp.Count=0 then
         tmp.Resize(1);

      result:=tmp.Count-1;

      if AData.Count<=result then
         AData.Resize(result+1);
    end;
  end;

  procedure AddAttribute(const AData:TDataItem; const Key,Text:String);
  var Item : TDataItem;
      //tmpItem : TDataItem;
      tmp : TInteger;
  begin
    Item:=AData.Items.Find(Key);

    if Item=nil then
       Item:=AData.Items.Add(Key,GuessKind(Text));

    tmp:=ResizeItem(Item);

    {
    if Item=nil then
    begin
      Item:=AData.Items.Add(Key,GuessKind(Text));
      tmp:=ResizeItem(Item);
    end
    else
    begin
      tmpItem:=TopParent(Item);
      tmp:=Item.Count;
      tmpItem.Resize(tmp+1);
      Item.Parent.AsTable:=True;
      Item.Parent.Master:=Item.Parent.Parent;
    end;
    }

    SetColumn(Item,tmp,Text);
  end;

  procedure AddChildren(const AData:TDataItem; const Count:Integer);
  var t : Integer;

      tmp,
      tmpOld : Integer;

      tmpProgress,
      tmpCancel : Boolean;
  begin
    tmpProgress:=Assigned(OnProgress) and CallProgress;

    tmpCancel:=False;
    tmpOld:=0;

    if tmpProgress then
       DoProgress(0,tmpCancel);

    try
      if not tmpCancel then
      begin
        if AData.Master<>nil then
           TDataAccess(AData).IMaster.AppendIndex(Count);

        for t:=0 to Count-1 do
        begin
          Xml.Item(t);

          DoAppend(AData,Xml,False);

          if tmpProgress then
          begin
            tmp:=Round(100*t/Count);

            if tmp<>tmpOld then
            begin
              DoProgress(tmp,tmpCancel);
              tmpOld:=tmp;
            end;

            if tmpCancel then
               break;
          end;

          Xml.Parent;
        end;
      end;

    finally
      if tmpProgress then
         DoProgress(100,tmpCancel);
    end;
  end;

  procedure ResetParentCount(ACol:TDataItem);
  var tmp : TInteger;
  begin
    // Reset Count up to all Parents for Master->Detail xml data
    repeat
      ACol:=ACol.Parent;

      if (ACol<>nil) and ACol.AsTable {and (Col.Count=0)} and (ACol.Items.Count>0) then
      begin
        tmp:=ACol.Items[0].Count;

        if ACol.Count<>tmp then
           ACol.Resize(tmp);

        if ACol.Master<>nil then
           break;
      end
      else
         break;

    until ACol=nil;
  end;

  procedure AppendTopParent(const ACol:TDataItem);
  var tmp : TDataItem;
  begin
    tmp:=TopParent(ACol);
    tmp.Resize(tmp.Count+1);
  end;

var Col : TDataItem;
    Key : String;
    t : Integer;
    tmpAttrCount,
    tmpCount : Integer;
    tmpText : String;
    tmpPos : TInteger;
begin
  Key:=Xml.Name;

  if ExcludeNodes.IndexOf(Key)<>-1 then
     Exit;

  Col:=Data.Items.Find(Key);

  tmpCount:=Xml.Count;
  tmpAttrCount:=Xml.AttributeCount;

  if Col=nil then
  begin
    if (tmpCount>1) or (tmpAttrCount>0) then
    begin
      Col:=TDataItem.Create(True);
      Col.Name:=Key;
      Data.Items.Add(Col);
    end
    else
      Col:=Data.Items.Add(Key,TDataKind.dkUnknown);
  end
  else
  begin
    if (tmpCount>1) {or (tmpAttrCount>0)} then
  //  if tmpCount>1 then
    begin
      // Try to convert Data into a detail of its Parent
      if Data.Parent<>nil then
         if (Data.Parent.Items.Count>1) and (Data.Master=nil) then
         begin
           Data.Master:=Data.Parent;
           TDataAccess(Data).IMaster.AppendIndex(tmpCount);
         end;

      if (Data.Master=nil) and (Col.Master=nil) then
         AppendTopParent(Col);
    end
    else
    if (Col.Master=nil) {and Col.Parent.AsTable} and (Col.Parent.Master=nil)
        and (Col.Items.Count>1) then
       Col.Master:=Col.Parent;
  end;

  if tmpAttrCount>0 then
  begin
    for t:=0 to tmpAttrCount-1 do
        AddAttribute(Col,Xml.AttributeName(t),Xml.Attribute(t));

    if tmpCount>0 then
       AddChildren(Col,tmpCount);
  end
  else
  if Xml.HasText then
  begin
    tmpText:=Xml.Text;

    if Col.Kind=TDataKind.dkUnknown then
       TDataAccess(Col).FKind:=GuessKind(tmpText);

    tmpPos:=ResizeItem(Col);
    SetColumn(Col,tmpPos,tmpText);
  end
  else
  if tmpCount=0 then
     Col.Missing[ResizeItem(Col)]:=True
  else
  if tmpCount>0 then
  begin
//    if tmpCount=77 then
//       tmpCount:=3;

    AddChildren(Col,tmpCount);

    (*
    // Bad, temporary fix to resize last extra row:
    if Col.Master<>nil then
       if Col.Master.Parent=nil then
       begin
         Col.Resize(tmpCount);

         if Col.Items.Count=1 then
            if Col.Items[0].AsTable then
               Col.Items[0].Resize(tmpCount);
       end;
    *)
  end;

  ResetParentCount(Col);
end;

class function TBIXML.CreateParser:TXmlEngine;
begin
  if EngineClass=nil then
     result:={$IFDEF FPC}TFPCXML{$ELSE}TMsXML{$ENDIF}.Create(nil)
  else
     result:=EngineClass.Create(nil);
end;

function TBIXML.Parse(const Text:String): Boolean;
begin
  if Xml=nil then
     Xml:=CreateParser;

 result:=Xml.Parse(Text);
end;

function TBIXML.ImportText(const Text:String): TDataItem;
begin
  result:=nil;

  if Parse(Text) then
  begin
    QuotedBooleans:=True;

    result:=TDataItem.Create(True);
    DoAppend(result,Xml,True);
  end
  else
  if not CallOnError(BIMsg_XML_WrongContent) then
     raise EBIXML.Create(BIMsg_XML_WrongContent);
end;

class function TBIXML.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('XML files','*.xml');
end;

class function TBIXML.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.XML');
end;

{ TBIXMLExport }

// Validate tag
function XMLTag(const S:String):String;
begin
  {$IFDEF FPC}
  result:=TFPCXML.Normalize(S);
  {$ELSE}

  if S='' then
     result:='_'
  else
  begin
    result:=S.Replace(' ','_');

    if result.StartsWith('xml',True) then
       result:='_'+result;

    if (result.Chars[1]<>'_') and (not result.Chars[1].IsLetter) then
       result:='_'+result;
  end;
  {$ENDIF}
end;

function ToBoolean(const Value:Boolean):String;
begin
  if Value then
     result:='true'
  else
     result:='false';
end;

procedure TBIXMLExport.EmitRow(const AIndex:TInteger; const AData:TDataArray; const Items:TStrings; const Indent:String);

  // Pending: Optimize speed:
  function EscapeXML(const S:String):String;
  begin
    // First replacement should be: '&'
    result:=StringReplace(S,'&','&amp;',[rfReplaceAll]);

    result:=StringReplace(result,'"','&quot;',[rfReplaceAll]);
    result:=StringReplace(result,'''','&apos;',[rfReplaceAll]);
    result:=StringReplace(result,'<','&lt;',[rfReplaceAll]);
    result:=StringReplace(result,'>','&gt;',[rfReplaceAll]);
  end;

var tmp,
    tmpTag,
    tmpC : String;
    t : Integer;
    Col : TDataItem;
    tmpData : TDataArray;
begin
  tmpData:=nil;
  tmpData.Add(Data);

  for t:=0 to High(AData) do
  begin
    Col:=AData[t];

    if not Col.Missing[AIndex] then
    begin
      tmpTag:=XMLTag(Col.Name);

      if Col.AsTable then
      begin
        Items.Add(Indent+'<'+tmpTag+'>');

        if Col is TDetailData then
           EmitDetail(TDetailData(Col).Detail,TDataCursor.MasterDetailIndex(TDetailData(Col).Detail,tmpData,AIndex),Items,Indent+Tab)
        else
        if Col.Master<>nil then
           EmitDetail(Col,TDataAccess(Col).IMaster.GetIndex(Col,AIndex),Items,Indent+Tab)
        else
           EmitRow(AIndex,Col.Items.AsArray,Items,Indent+Tab);

        Items.Add(Indent+'</'+tmpTag+'>');
      end
      else
      begin
        tmp:=Indent+'<'+tmpTag+'>';

        if Col.Kind=dkBoolean then
           tmp:=tmp+ToBoolean(Col.BooleanData[AIndex])
        else
        begin
          tmpC:=DataToString(Col,AIndex);

          case Col.Kind of
            dkText     : tmpC:='"'+EscapeXML(tmpC)+'"';
            dkDateTime : tmpC:='"'+tmpC+'"';
          end;

          tmp:=tmp+tmpC;
        end;

        Items.Add(tmp+'</'+tmpTag+'>');
      end;
    end;
  end;
end;

procedure TBIXMLExport.EmitRows(const AIndex:TInteger);
begin
  IItems.Add('<item>');
  EmitRow(AIndex,Items,IItems,Tab+Tab);
  IItems.Add('</item>');
end;

class function TBIXMLExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('XML','*.xml');
end;

class function TBIXMLExport.Supports(const Extension: String): Boolean;
begin
  result:=TBIXML.Supports(Extension);
end;

procedure TBIXMLExport.EmitDetail(const Data:TDataItem; const AIndex:TCursorIndex; const Items:TStrings; const Indent:String);
var t : TLoopInteger;
begin
  if Data.Items.Count>1 then
     for t:=0 to High(AIndex) do
     begin
       Items.Add(Indent+'<item>');
       EmitRow(AIndex[t],Data.Items.AsArray,Items,Indent+Tab);
       Items.Add(Indent+'</item>');
     end
  else
     for t:=0 to High(AIndex) do
         EmitRow(AIndex[t],Data.Items.AsArray,Items,Indent);
end;

procedure TBIXMLExport.DoEmit(const AItems: TStrings);

  procedure AddHeader(const Ident:String; AData:TDataItem);
  var Item : TDataItem;
  begin
    AItems.Add(Ident+'<data name="'+AData.Name+'">');
      AItems.Add(Ident+Tab+'<kind>'+AData.Kind.ToString+'</kind>');
      AItems.Add(Ident+Tab+'<count>'+IntToStr(AData.Count)+'</count>');
      AItems.Add(Ident+Tab+'<table>'+ToBoolean(AData.AsTable)+'</table>');

    if AData.Items.Count>0 then
    begin
      AItems.Add(Ident+Tab+'<items>');

      for Item in Items do
          AddHeader(Ident+Tab,Item);

      AItems.Add(Ident+Tab+'</items>');
    end;

    AItems.Add(Ident+'</data>');
  end;

var OldDecimal : Char;
    tmpTag : String;
begin
  OldDecimal:=FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator:='.';

    if Emit=TBIXMLEmit.Header then
       AItems.Add('<?xml version="1.0" encoding="UTF-8" standalone="no" ?>');

    if SchemaOnly then
       AddHeader(Tab,Cursor.Data)
    else
    begin
      tmpTag:=XMLTag(Data.Name);

      AItems.Add('<'+tmpTag+'>');

      IItems:=AItems;
      Cursor.Loop(EmitRows);

      AItems.Add('</'+tmpTag+'>');
    end;

  finally
    FormatSettings.DecimalSeparator:=OldDecimal;
  end;
end;

{ TXmlEngine }

function TXmlEngine.GetAttribute(const AName: String): String;
var t : Integer;
begin
  for t:=0 to AttributeCount-1 do
      if SameText(AttributeName(t),AName) then
         Exit(Attribute(t));

  result:='';
end;

initialization
  TBIXML.EngineClass:=nil;

  TBIFileImporters.RegisterClass(TBIXML);
  TBIExporters.RegisterClass(TBIXMLExport);
finalization
  TBIExporters.UnRegisterClass(TBIXMLExport);
  TBIFileImporters.UnRegisterClass(TBIXML);
end.
