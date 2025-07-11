{*********************************************}
{  TeeBI Software Library                     }
{  HTML data import and export                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Html;

(*
  Classes to import / export TDataItem from / to HTML <table>

   TBIHtml.Import : From HTML one or more <table> content into a TDataItem

   TBIHtmlExport.AsString : From TDataItem into HTML <table>

  TBIHtmlHelper : Methods to generate HTML content easier, for example:

     TBIHtmlHelper.Link('SteeBI', 'http://www.steebi.com')

     returns:  '<a href="http://www.steebi.com">SteeBI</a>'

*)

interface

uses
  System.Classes, System.Types,
  {$IFDEF FPC}
  BI.FPC, Graphics,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  Data.DB, BI.DataItem, BI.DataSource, BI.Arrays, BI.Arrays.Strings, BI.UI,
  BI.XMLData;

type
  THTMLEngine=class(TXmlEngine)
  public
    Constructor CreateParser; virtual; abstract;
  end;

  THTMLEngineClass=class of THTMLEngine;

  TBIHTML=class(TBITextSource)
  private
    function GetTable(const xml:TXmlEngine):TDataItem;
    class function GetTables(const xml:TXmlEngine):TDataArray; static;
    class function TryFindID(const xml:TXmlEngine):String; static;
  public
    class var
      EngineClass : THTMLEngineClass;

    class function FileFilter: TFileFilters; override;
    function Import(const Strings:TStrings):TDataArray; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIHTMLExport=class(TBITextExport)
  private
    AddedRows : TInteger;
    IItems : TStrings;

    procedure AddRow(const AIndex:TInteger);
    function AddRows(const AItems: TStrings):TInteger;
    function CellAlign(const AData:TDataItem; const AddLeft:Boolean):String;
    function CellValue(const AItem:TDataItem; const AIndex:TInteger):String;
    function Headers(const AddSortIcons:Boolean):String;
    function MaxDepth(const AData:TDataArray):Integer;
    function Row(const AIndex:TInteger; const AData:TDataArray):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    var
      Borders : Boolean;
      Colorizers : TDataColorizers;
      SortIcons : Boolean;

    Constructor Create; override;

    class function FileFilter: TFileFilters; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIHtmlHelper=class
  public
  const
    Return='</br>';
  class var
    TableClass:String;

    class function Color(const AColor:TColor):String; static;
    class function Combo(const AName:String; const Texts,Values:TStringArray): String; static;
    class function Escape(const S:String):String; static;
    class function FinishForm(const AName:String):String; static;
    class function Hidden(const AName:String; const AValue:Integer):String; overload; static;
    class function Hidden(const AName,AValue:String):String; overload; static;
    class function InitForm:String; static;
    class function Link(const Text,URL:String):String; static;
    class function Navigator(const ACursor:TDataCursor; const URL,Button,ButtonDisabled:String):String; static;
    class function ToTable(const ADataSet:TDataSet):String; static;
  end;

implementation

{$IFNDEF FPC}
{$IF CompilerVersion>27}
{$DEFINE HAS_NETENCODING} // Solution alternative for XE4,5,6?
{$ENDIF}
{$ENDIF}

uses
  System.SysUtils,
  {$IFDEF HAS_NETENCODING}
  System.NetEncoding,
  {$ENDIF}
  BI.Languages.English, BI.UI.Colors;

{ TBIHTML }

class function TBIHTML.TryFindID(const xml:TXmlEngine):String;
begin
  result:=xml.GetAttribute('id');

  if result='' then
  begin
    result:=xml.GetAttribute('name');

    if result='' then
       result:=xml.Name;
  end;
end;

type
  // Helper record to handle:  "<td rowspan='xxx'>"
  TRowSpan=record
  private
    function TryCopy(const ARow:TInteger):Boolean;
  public
    Data : TDataItem;
    Count : Integer;
  end;

function TRowSpan.TryCopy(const ARow:TInteger):Boolean;
begin
  if Count>0 then
  begin
    Data.CopyFrom(ARow,Data,ARow-1);
    Dec(Count);

    result:=True;
  end
  else
    result:=False;
end;

function HTMLTrim(const S:String):String;
const
  NBSP=#160;
  NBSP2='&nbsp;';
begin
  result:=Trim(S).Replace(NBSP2,'');

  while Copy(result,1,1)=NBSP do
      Delete(result,1,1);

  while Copy(result,Length(result),1)=NBSP do
      Delete(result,Length(result),1);
end;

type
  TDataItemAccess=class(TDataItem);

function TBIHTML.GetTable(const xml:TXmlEngine):TDataItem;
{$IFDEF HAS_NETENCODING}
var
  Encoding : TNetEncoding;
{$ENDIF}

  function IsHiddenSpan:Boolean;
  var tmp : String;
  begin
    if SameText(xml.Name,'span') then
    begin
      tmp:=xml.GetAttribute('style');

      result:=SameText(tmp,'display:none');
    end
    else
      result:=False;
  end;

  function TryFindText:String;
  var t : Integer;
  begin
    if xml.HasText then
       result:=xml.Text
    else
    begin
      result:='';

      for t:=0 to xml.Count-1 do
      begin
        xml.Item(t);

        if not IsHiddenSpan then
           result:=TryFindText;

        xml.Parent;

        if result<>'' then
           break;
      end;
    end;
  end;

  function TryFindRowSpan:Integer;
  var tmp : String;
  begin
    tmp:=xml.GetAttribute('rowspan');

    if tmp='' then
       result:=0
    else
       result:=StrToIntDef(tmp,1)-1;
  end;

var
  RowSpans : Array of TRowSpan;

  procedure AddNewRowSpan(const AItem:TDataItem);
  var tmp : Integer;
  begin
    tmp:=AItem.Parent.Items.Count-1;

    SetLength(RowSpans,tmp+1);

    RowSpans[tmp].Data:=AItem;
    RowSpans[tmp].Count:=0;
  end;

  procedure AddRow(const AData:TDataItem);

    function AddNewColumn(const AName:String):TDataItem;
    begin
      // Pending: Use "AName" as Item Name only if xml.Name is "th" (table header) ?
      result:=AData.Items.Add(AData.Items.UniqueName(AName),TDataKind.dkUnknown);
      result.Resize(AData.Count);

      AddNewRowSpan(result);
    end;

    procedure SetCell(const AItem:TDataItem; const AIndex:TInteger; const AValue:String);
    begin
      if AItem.Kind=TDataKind.dkUnknown then
      begin
        TDataItemAccess(AItem).FKind:=GuessKind(AValue);
        AItem.Resize(AData.Count);
      end;

      SetColumn(AItem,AIndex,AValue);
    end;

  var t : Integer;
      tmp : Integer;
      tmpIndex : Integer;
      tmpItem : TDataItem;
      tmpS : String;
  begin
    tmpIndex:=AData.Count;

    if AData.Items.Count>0 then
       AData.Resize(tmpIndex+1);

    tmp:=0;

    // Add previous-row "spans", if any
    if AData.Count>0 then
       for t:=0 to High(RowSpans) do
           if RowSpans[t].TryCopy(tmpIndex) then
              Inc(tmp)
           else
              break;

    // Loop all <td> or <th> tags

    for t:=0 to xml.Count-1 do
    begin
      xml.Item(t);

      if SameText(xml.Name,'td') or SameText(xml.Name,'th') then
      begin
        tmpS:=HTMLTrim(TryFindText);

        if AData.Items.Count<=tmp then
           AddNewColumn(tmpS);

        RowSpans[tmp].Count:=TryFindRowSpan;

        if tmpS<>'' then
        begin
          {$IFDEF HAS_NETENCODING}
          if Encoding<>nil then
             tmpS:=Encoding.Decode(tmpS);
          {$ENDIF}

          {if AData.Items.Count<=tmp then
             tmpItem:=AddNewColumn(tmpS)
          else}
             tmpItem:=AData.Items[tmp];

          if AData.Count>0 then
             SetCell(tmpItem,tmpIndex,tmpS);
        end;

        Inc(tmp);

        // Try process in-middle row spans
        while (Length(RowSpans)>tmp) and RowSpans[tmp].TryCopy(tmpIndex) do
              Inc(tmp);
      end;

      xml.Parent;
    end;
  end;

  // Loop all <tr> tags
  procedure AddRows(const AData:TDataItem);
  var t : Integer;
  begin
    for t:=0 to xml.Count-1 do
    begin
      xml.Item(t);

      if SameText(xml.Name,'tr') then
         AddRow(AData);

      xml.Parent;
    end;
  end;

var t : Integer;
begin
  result:=TDataItem.Create(True);
  result.Name:=TryFindID(xml);

  {$IFDEF HAS_NETENCODING}
  // Seattle/Berlin error ! TNetEncoding.HTML;
  // It is not reliable to use Encoding.Decode, as we don't know if the string
  // contains encoded characters or not. Eg: "a & b" fails
  Encoding:=nil;
  {$ENDIF}

  for t:=0 to xml.Count-1 do
  begin
    xml.Item(t);

    if SameText(xml.Name,'thead') then
       AddRows(result)
    else
    if SameText(xml.Name,'tbody') then
       AddRows(result)
    else
    if SameText(xml.Name,'tr') then
       AddRow(result);

    xml.Parent;
  end;
end;

class function TBIHTML.GetTables(const xml:TXmlEngine):TDataArray;
var
  Tables : TDataArray;
  HTML : TBIHTML;

  procedure TryAddTable;

    procedure DoExtract;
    var t : Integer;
    begin
      for t:=0 to xml.Count-1 do
      begin
        xml.Item(t);
        TryAddTable;
        xml.Parent;
      end;
    end;

  begin
    if SameText(xml.Name,'table') then
       Tables.Add(HTML.GetTable(xml))
    else
       DoExtract;
  end;

begin
  HTML:=TBIHTML.Create;
  try
    xml.Root;
    TryAddTable;

    result:=Tables;
  finally
    HTML.Free;
  end;
end;

{type
  TBIXMLAccess=class(TBIXML);}

function TBIHTML.Import(const Strings: TStrings): TDataArray;

  // XMLDom error: "DTD is prohibited"
  procedure FixDocType;
  const
    DocType='<!doctype html>';
    HTMLTag='<html';

  var t : Integer;
      tmp : String;
  begin
    t:=0;

    while t<Strings.Count-1 do
    begin
      tmp:=Trim(Strings[t]);

      if SameText(Copy(tmp,1,Length(DocType)),DocType) then
      begin
        Strings.Delete(t);
        break;
      end
      else
      if SameText(Copy(tmp,1,Length(HTMLTag)),HTMLTag) then
         break
      else
         Inc(t);
    end;
  end;

var tmp : TXmlEngine;
begin
  result:=nil;

  if Strings.Count>0 then
  begin
    if EngineClass=nil then
    begin
      FixDocType;

      tmp:=TBIXML.CreateParser;
    end
    else
      tmp:=EngineClass.CreateParser;

    try
      if tmp.Parse(Strings.Text) then
         result:=GetTables(tmp);
    finally
      tmp.Free;
    end;
  end;
end;

class function TBIHTML.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('HTML files','*.htm;*.html');
end;

class function TBIHTML.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.html') or SameText(Extension,'.htm');
end;

{ TBIHTMLExport }

Constructor TBIHTMLExport.Create;
begin
  inherited;
  FloatFormat:='0.##';
  SortIcons:=True;
end;

// Returns the max number of levels (child-child... items)
function TBIHTMLExport.MaxDepth(const AData:TDataArray):Integer;

  function DepthOf(const AData:TDataItem):Integer;
  begin
    if AData is TDetailData then
       result:=1
    else
    if AData.AsTable and (AData.Master=nil) then
       result:=1+MaxDepth(AData.Items.AsArray)
    else
       result:=1;
  end;

var tmp : Integer;
    Item : TDataItem;
begin
  result:=1;

  for Item in AData do
  begin
    tmp:=DepthOf(Item);

    if tmp>result then
       result:=tmp;
  end;
end;

type
  TDataAccess=class(TDataItem);

function TBIHTMLExport.CellAlign(const AData:TDataItem; const AddLeft:Boolean):String;
begin
  case AData.Kind of
      dkInt32 : if TDataAccess(AData).IHasDate then
                   if AddLeft then
                      result:=' align="left"'
                   else
                      result:=''
                else
                   result:=' align="right"';

      dkInt64,
     dkSingle,
     dkDouble,
   dkExtended: result:=' align="right"';
    dkBoolean: result:=' align="center"';
  else
    if AddLeft then
       result:=' align="left"'
    else
       result:='';
  end;
end;

function TBIHTMLExport.CellValue(const AItem:TDataItem; const AIndex:TInteger):String;
begin
  if AItem.Kind=TDataKind.dkBoolean then
  begin
    if AItem.Missing[AIndex] then
       result:=''
    else
    begin
      result:='<input type="checkbox"';

      if AItem.BooleanData[AIndex] then
         result:=result+' checked="checked"';

      result:=result+' disabled/>';
    end;
  end
  else
    result:=DataToString(AItem,AIndex);
end;

function TBIHTMLExport.Row(const AIndex:TInteger; const AData:TDataArray):String;
var
  tmpFontColor : String;

  function CellOf(const AData:TDataItem; const AText:String):String;
  begin
    result:=AText;

    if Assigned(FOnGetText) then
       FOnGetText(Self,AData,AIndex,result);

   if tmpFontColor<>'' then
      result:='<span style="color:'+tmpFontColor+'">'+result+'</span>';
  end;

  function BackgroundColor(const AData:TDataItem):String;
  var tmpPercent : Double;
      tmpColor : Integer;
      NewColor : TColor;
  begin
    if Colorizers=nil then
       result:=''
    else
    if Colorizers.TryColorize(AData,AIndex,tmpPercent,tmpColor) then
    begin
      NewColor:=Colorizers[tmpColor].ColorOf(tmpPercent);

      result:=' style="background-color:'+TBIHTMLHelper.Color(NewColor)+'"';

      // Not in HTML5: result:=' bgcolor="'+TBIHTMLHelper.Color(NewColor)+'"';

      if Colorizers[tmpColor].TextColor=TColorizeTextColor.Automatic then
         if TColorFunction.ToRGB(NewColor)>$7FFFFF then
            tmpFontColor:='#0'
         else
            tmpFontColor:='#FFFFFF';
    end;
  end;

var t : Integer;
    Item : TDataItem;
    tmpS : String;
begin
  result:='';

  for t:=0 to High(AData) do
  begin
    Item:=AData[t];

    tmpFontColor:='';

    if Item is TDetailData then
       result:=result+'<td>'+CellOf(Item,'(detail)')+'</td>'
    else
    if Item.AsTable then
       if Item.Master=nil then
          result:=result+Row(AIndex,Item.Items.AsArray)
       else
          result:=result+'<td>'+CellOf(Item,'(dataset)')+'</td>'
    else
    if Item.Kind=dkUnknown then
    begin
      if Item.Items.Count>AIndex then
         result:=result+'<td>'+CellOf(Item,Item.Items[AIndex].Name)+'</td>'
      else
         result:=result+'<td>'+CellOf(Item,'')+'</td>';
    end
    else
    begin
      result:=result+'<td'+CellAlign(Item,False)+BackgroundColor(Item)+'>';

      tmpS:=CellValue(Item,AIndex);

      result:=result+CellOf(Item,tmpS)+'</td>';
    end;
  end;
end;

class function TBIHTMLExport.Supports(const Extension: String): Boolean;
begin
  result:=TBIHTML.Supports(Extension);
end;

procedure TBIHtmlExport.AddRow(const AIndex:TInteger);
begin
  Inc(AddedRows);
  IItems.Add('<tr>'+Row(AIndex,Items)+'</tr>');
end;

function TBIHTMLExport.AddRows(const AItems: TStrings):TInteger;
begin
  AddedRows:=0;
  IItems:=AItems;
  Cursor.Loop(AddRow);
  result:=AddedRows;
end;

function TBIHTMLExport.Headers(const AddSortIcons:Boolean):String;

  function ColSpan(const Num:Integer):String;
  begin
    if Num>1 then
       result:=' colspan="'+IntToStr(Num)+'"'
    else
       result:='';
  end;

  function SpanCount(const AData:TDataItem):Integer;
  var Item : TDataItem;
  begin
    if AData is TDetailData then
       result:=1
    else
    if AData.AsTable and (AData.Master=nil) then
    begin
      result:=0;

      for Item in AData.Items.AsArray do
          Inc(result,SpanCount(Item));
    end
    else
       result:=1;
  end;

  function HeaderOf(const Depth:Integer; const AData:TDataArray):String;
  var
    AddedSortIcons : Boolean;

    function HeaderAtDepth(const Depth:Integer; AData:TDataItem):String;

      function SortIcon(const Descending:Boolean):String;
      begin
        if Descending then
           result:='&#x25B2;'
        else
           result:='&#x25BC;';
      end;

      function IsDataOrdered:String;
      begin
        AData.Stats; // <-- force creating DataMap

        if (AData.DataMap=nil) or (AData.DataMap.Sorted=TDataOrder.None) then
           result:=''
        else
           result:=SortIcon(AData.DataMap.Sorted=TDataOrder.Descending);
      end;

      function SortingIcons:String;
      var tmp : Integer;
      begin
        if Cursor=nil then
           result:=IsDataOrdered
        else
        begin
          tmp:=Cursor.SortBy.IndexOf(AData);

          if tmp=-1 then
             result:=IsDataOrdered
          else
             result:=SortIcon(Cursor.SortBy.Items[tmp].Descending);
        end;
      end;

    var t : Integer;
        tmpSort : String;
    begin
      for t:=0 to Depth-1 do
      begin
        if AData is TDetailData then
           Exit('<th></th>')
        else
        if AData.AsTable and (AData.Master=nil) then
           Exit(HeaderOf(Depth-1,AData.Items.AsArray))
        else
           Exit('<th></th>')
      end;

      if AddedSortIcons or (not AddSortIcons) then
         tmpSort:=''
      else
      begin
        tmpSort:=SortingIcons;
        AddedSortIcons:=tmpSort<>'';
      end;

      result:='<th'+ColSpan(SpanCount(AData))+CellAlign(AData,True)+'>'+AData.Name+tmpSort+'</th>';
    end;

  var Item : TDataItem;
  begin
    result:='';

    AddedSortIcons:=False;

    for Item in AData do
        result:=result+HeaderAtDepth(Depth,Item);
  end;

var t : Integer;
begin
  result:='';

  for t:=0 to MaxDepth(Items)-1 do
      result:=result+'<tr>'+HeaderOf(t,Items)+'</tr>'+TCommonUI.CRLF;
end;

procedure TBIHTMLExport.DoEmit(const AItems: TStrings);

  function Border:String;
  begin
    if Borders then
       result:='border="1" '
    else
       result:='';
  end;

var NumRows : TInteger;
begin
  AItems.Add('<table '+Border+'class="'+TBIHtmlHelper.TableClass+'">'+TCommonUI.CRLF);

  if Totals<>nil then
     AItems.Add('<tfoot><tr>'+Row(0,Totals.Items.AsArray)+'</tr></foot>');

  AItems.Add('<tbody>'+TCommonUI.CRLF);

  if SchemaOnly then
     NumRows:=0
  else
     NumRows:=AddRows(AItems);

  AItems.Add('</tbody>'+TCommonUI.CRLF+'</table>');

  AItems.Insert(1,'<thead>'+Headers(SortIcons and (NumRows>1))+'</thead>'+TCommonUI.CRLF);
end;

class function TBIHTMLExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('HTML','*.htm;*.html');
end;

{ TBIHtmlHelper }

class function TBIHTMLHelper.ToTable(const ADataSet: TDataSet): String;
const
  CRLF=#13#10;

  function FieldNames(const AFields:TFields):String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to AFields.Count-1 do
        {$IFNDEF FPC}
        if AFields[t] is TObjectField then
           result:=result+FieldNames(TObjectField(AFields[t]).Fields)
        else
        {$ENDIF}
           result:=result+'<th>'+AFields[t].DisplayName+'</th>';
  end;

  function Header:String;
  begin
    result:='<tr>'+FieldNames(ADataSet.Fields)+'</tr>';
  end;

  function Row:String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to ADataSet.Fields.Count-1 do
        result:=result+'<td>'+ADataSet.Fields[t].AsString+'</td>';
  end;

begin
  if ADataSet=nil then
     raise EBIException.Create('Error: Missing DataSet');

  result:='<table class="'+TBIHtmlHelper.TableClass+'">'+CRLF+'<thead>'+Header+'</thead>'+CRLF;

  ADataSet.DisableControls;
  try
    result:=result+'<tbody>'+CRLF;

    ADataSet.First;

    while not ADataSet.Eof do
    begin
      result:=result+'<tr>'+Row+'</tr>'+CRLF;
      ADataSet.Next;
    end;

    result:=result+'</tbody>'+CRLF;
  finally
    ADataSet.EnableControls;
  end;

  result:=result+'</table>'+CRLF;
end;

class function TBIHtmlHelper.Escape(const S:String):String;
begin
  result:=StringReplace(S,'"','&quot;',[rfReplaceAll]);
  result:=StringReplace(result,'+','%2B',[rfReplaceAll]);
end;

class function TBIHtmlHelper.Color(const AColor: TColor): String;

  function SwapColor(const AColor:TColor):TColor;
  begin
    // Swap R <--> B
    TColorRec(Result).R := TColorRec(AColor).B;
    TColorRec(Result).G := TColorRec(AColor).G;
    TColorRec(Result).B := TColorRec(AColor).R;
    TColorRec(Result).A := 0;
  end;

begin
  result:='#'+IntToHex(SwapColor(AColor),6);
end;

class function TBIHtmlHelper.Combo(const AName:String; const Texts,Values:TArrayOfStrings{TStringArray}): String;
var t : Integer;
    tmpValue : String;
begin
  result:='<select name="'+AName+'">';

  for t:=0 to High(Texts) do
  begin
    if Values=nil then
       tmpValue:=Texts[t]
    else
       tmpValue:=Values[t];

    result:=result+'<option value="'+Escape(tmpValue)+'">'+Texts[t]+'</option>';
  end;

  result:=result+'</select>';
end;

class function TBIHtmlHelper.InitForm:String;
begin
  result:='<form action="" method="post">';
end;

class function TBIHtmlHelper.Link(const Text, URL: String): String;
begin
  result:='<a href="'+URL+'">'+Text+'</a>';
end;

class function TBIHtmlHelper.Navigator(const ACursor:TDataCursor; const URL,Button,ButtonDisabled:String): String;

  function MakeURL(const AStart:TInteger):String;
  begin
    result:=URL+'&start='+IntToStr(AStart);
  end;

var Start,
    Finish : TInteger;

  function Link(AStart:TInteger; const AText:String):String;
  begin
    if AStart<0 then
       AStart:=0
    else
    if AStart>ACursor.Count-1 then
       AStart:=ACursor.Count-1;

    {
    // using <a> tag instead of <input button>:
    if (AStart<=Start) or (AStart>=Finish) then
       result:='<a href="'+MakeURL(AStart)+'">'+AText+'</a>'
    else
       result:=AText;
    }

    // using <button> tag:
    result:='<input type="button" onclick="location.href='''+MakeURL(AStart)+''';" value="'+AText+'"';

    if (AStart>=Start) and (AStart<=Finish) then
    begin
       result:=result+' disabled';

       if ButtonDisabled<>'' then
          result:=result+' class="'+ButtonDisabled+'"';
    end
    else
    if Button<>'' then
       result:=result+' class="'+Button+'"';

    result:=result+'/>';
  end;

var
  Max : TInteger;

  function CurrentPosition:String;
  var tmp,
      tmpLast : TInteger;

      CurrentPage,
      TotalPages : Integer;
  begin
    tmp:=1+ACursor.Start;

    if ACursor.Max=0 then
    begin
      TotalPages:=1;
      CurrentPage:=1;
    end
    else
    begin
      TotalPages:=(1+Max) div ACursor.Max;

      if TotalPages*ACursor.Max<1+Max then
         Inc(TotalPages);

      if tmp+ACursor.Max>Max then
         CurrentPage:=TotalPages
      else
         CurrentPage:=1+(tmp div ACursor.Max);
    end;

    if ACursor.Max=1 then
       result:=Format(BIMsg_NavigatorSingleItem,[tmp,Max])
    else
    begin
      tmpLast:=tmp+ACursor.Max-1;

      if tmpLast>1+Max then
         tmpLast:=1+Max;

      result:=Format(BIMsg_NavigatorItem,[tmp,tmpLast,1+Max])+' '+
              Format(BIMsg_NavigatorPage,[CurrentPage,TotalPages]);
    end;
  end;

begin
  Max:=ACursor.Count-1;

  if ACursor.Max<1 then
     Finish:=Max
  else
  begin
    Finish:=ACursor.Start+ACursor.Max-1;

    if Finish>Max then
       Finish:=Max;
  end;

  if ACursor.Start<0 then
     Start:=0
  else
  begin
    Start:=ACursor.Start;

    if Start>Max then
       Start:=Max;
  end;

  if (Start<=0) and ((Finish=Max) or (Finish<0)) then
     result:=''
  else
     result:=Link(0,'&lt&lt')+'&nbsp'+
             Link(Start-ACursor.Max,'&lt')+'&nbsp'+
             CurrentPosition+'&nbsp'+
             Link(Start+ACursor.Max,'&gt')+'&nbsp'+
             Link(1+Max-ACursor.Max,'&gt&gt');
end;

class function TBIHtmlHelper.FinishForm(const AName:String):String;
begin
  result:='<input type="submit" name="'+AName+'" value="Get" style="border:0; height:34px; margin:0;"></input></form>';
end;

class function TBIHtmlHelper.Hidden(const AName:String; const AValue:Integer):String;
begin
  result:=Hidden(AName,IntToStr(AValue));
end;

class function TBIHtmlHelper.Hidden(const AName,AValue:String):String;
begin
  result:='<input type="hidden" name="'+AName+'" value="'+AValue+'"/>';
end;

initialization
  TBIHTML.EngineClass:=nil;

  TBIFileImporters.RegisterClass(TBIHTML);
  TBIExporters.RegisterClass(TBIHTMLExport);
finalization
  TBIExporters.UnRegisterClass(TBIHTMLExport);
  TBIFileImporters.UnRegisterClass(TBIHTML);
end.
