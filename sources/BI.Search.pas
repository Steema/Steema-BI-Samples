{*********************************************}
{  TeeBI Software Library                     }
{  Search Data component                      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Search;

interface

{$IFNDEF FPC}
{$IF CompilerVersion>27}
{$DEFINE THREADING}
{$ENDIF}
{$ENDIF}

uses
  System.Classes,

  {$IFDEF FPC}
  // MTProcs, <-- DoParallel
  {$ELSE}
  {$IFDEF THREADING}
  System.Threading,
  {$ENDIF}
  {$ENDIF}

  BI.DataItem, BI.Arrays, BI.DataSource;

// TDataSearch returns a copy of the Source data with rows that match
// content as text of one field or all fields.

type
  TSearchFinished=procedure(const AIndex:TCursorIndex) of object;
  TSearchProgress=procedure(var Stop:Boolean) of object;

  TDataSearchPart=(Anywhere, AtStart, AtEnd, Exact);

  TSearchHits=record
  public
    Enabled : Boolean;
    Items : Array of TDataArray;

    procedure Append(const AItem:TDataItem; const ARow:TInteger); inline;
    function Count:TInteger;
    function Exists(const ARow:TInteger; const AData:TDataItem):Boolean;
    procedure Find(const AIndex:TInteger; out ARow:TInteger; out AData:TDataItem);
    function IndexOf(const ARow:TInteger; const AData:TDataItem):TInteger;
  end;

  TDataSearch=record
  private
    FStop : Boolean;

    FOnFinished : TSearchFinished;
    FOnProgress : TSearchProgress;

    {$IFDEF THREADING}
    Task : ITask;
    {$ENDIF}

    function AsDateTime(const AItem:TDataItem; const AIndex:TInteger):String;

    function AsFloat(const Value:Single):String; overload;
    function AsFloat(const Value:Double):String; overload;

    {$IFDEF CPUX86}
    function AsFloat(const Value:Extended):String; overload;
    {$ENDIF}

    function AsString(const AItem:TDataItem; const AIndex:TInteger):String;
    function DoFind(const AText:String):TCursorIndex;
    procedure Finish(const AIndex:TCursorIndex);
  public
    CaseSensitive : Boolean;  // Default: False (ignore upper or lower case)

    DateTimeFormat,
    FloatFormat : String;   // For datetime and float data items

    TextPart : TDataSearchPart;  // Match search at start, end, anywhere or exact

    // Stores all "hits" after doing a search
    Hits : TSearchHits;

    Items  : TDataItem;

    // Data to search
    Source : TDataItem;

    // Indices to search (default is nil = all Source data)
    Index : TCursorIndex;

    // Do search using a background thread
    procedure BackgroundFind(const AText:String);

    // Search and return the array of row indices that have matches
    function Find(const AText:String):TCursorIndex;

    // Cancel and reset search if background thread is running
    procedure Stop;

    property OnFinished:TSearchFinished read FOnFinished write FOnFinished;
    property OnProgress:TSearchProgress read FOnProgress write FOnProgress;
  end;

implementation

uses
  System.SysUtils;

{ TDataSearch }

// Return a data item DateTime value as string, using DateTimeFormat specifier
function TDataSearch.AsDateTime(const AItem:TDataItem; const AIndex:TInteger):String;
begin
  if DateTimeFormat='' then
     result:=AItem.DataToString(AIndex)
  else
     result:=FormatDateTime(DateTimeFormat,AItem.DateTimeData[AIndex])
end;

function TDataSearch.AsFloat(const Value:Single):String;
begin
  result:=FormatFloat(FloatFormat,Value);
end;

function TDataSearch.AsFloat(const Value: Double): String;
begin
  result:=FormatFloat(FloatFormat,Value);
end;

{$IFDEF CPUX86}
function TDataSearch.AsFloat(const Value: Extended): String;
begin
  result:=FormatFloat(FloatFormat,Value);
end;
{$ENDIF}

// Return a data item value as string, using formatting specifiers.
function TDataSearch.AsString(const AItem:TDataItem; const AIndex:TInteger):String;
begin
  if AItem.Kind=TDataKind.dkDateTime then
     result:=AsDateTime(AItem,AIndex)
  else
  if FloatFormat='' then
     result:=AItem.DataToString(AIndex)
  else
  case AItem.Kind of
      dkSingle: result:=AsFloat(AItem.SingleData[AIndex]);
      dkDouble: result:=AsFloat(AItem.DoubleData[AIndex]);
    dkExtended: result:=AsFloat(AItem.ExtendedData[AIndex]);
  else
    result:=AItem.DataToString(AIndex);
  end;
end;

type
  TMatchFunction=function(const AText:String):Boolean of object;

  TMatchText=class
  private
    function MatchPart(const AText:String):Boolean;

    function CaseSensitive(const AText:String):Boolean; inline;
    function CaseInsensitive(const AText:String):Boolean; inline;
    function ExactSensitive(const AText:String):Boolean; inline;
    function ExactInsensitive(const AText:String):Boolean; inline;
  public
    Part : TDataSearchPart;
    Text : String;
    TextLength : Integer;

    Match : TMatchFunction;

    Constructor Create(const ASearch:TDataSearch; const AText:String);
  end;

{ TMatchText }

Constructor TMatchText.Create(const ASearch:TDataSearch; const AText:String);
begin
  inherited Create;

  Part:=ASearch.TextPart;

  if ASearch.CaseSensitive then
  begin
    if Part=TDataSearchPart.Exact then
       Match:=ExactSensitive
    else
       Match:=CaseSensitive;

    Text:=AText;
  end
  else
  begin
    if Part=TDataSearchPart.Exact then
       Match:=ExactInsensitive
    else
       Match:=CaseInsensitive;

    Text:=UpperCase(AText);
  end;

  if Part=TDataSearchPart.AtEnd then
     TextLength:=Length(Text)
  else
     TextLength:=0;
end;

function TMatchText.MatchPart(const AText:String):Boolean;
var tmpPos : Integer;
begin
  tmpPos:=Pos(Text,AText);

  case Part of
   Anywhere: result:=tmpPos>0;
    AtStart: result:=tmpPos=1;
      AtEnd: result:=(tmpPos>0) and (tmpPos=1+Length(AText)-TextLength);
  else
    result:=False
  end;
end;

// Returns True when Text matches AText string
function TMatchText.CaseSensitive(const AText:String):Boolean;
begin
  result:=MatchPart(AText);
end;

// Returns True when Text is exact to AText string
function TMatchText.ExactSensitive(const AText:String):Boolean;
begin
  result:=(Text=AText)
end;

// Returns True when Text matches AText string
function TMatchText.CaseInsensitive(const AText:String):Boolean;
begin
  result:=MatchPart(UpperCase(AText));
end;

// Returns True when Text is exact to AText string (case-insensitive)
function TMatchText.ExactInsensitive(const AText:String):Boolean;
begin
  result:=(Text=UpperCase(AText));
end;

function TDataSearch.DoFind(const AText:String):TCursorIndex;
var
  tmpMatch : TMatchText;

  // Tests matching in all items, returns True if at least one item is matched.
  function FindHitsInItem(const AItems:TDataArray; const AIndex:TInteger):Boolean;
  var t : Integer;
  begin
    result:=False;

    for t:=0 to AItems.Count-1 do
        if AItems[t].AsTable then
        begin
          if FindHitsInItem(AItems[t].Items.AsArray,AIndex) then
             result:=True;
        end
        else
        if tmpMatch.Match(AsString(AItems[t],AIndex)) then
        begin
          result:=True;

          // Add a new hit for the AIndex row
          if Hits.Enabled then
             Hits.Append(AItems[t],AIndex)
          else
             break;
        end;
  end;

  // For all rows, do search on all data items
  function FindInItems(const AItems:TDataArray):TCursorIndex;
  var tmpFilter : TCursorIndex;

    function StopFindAt(const AIndex:TInteger):Boolean;
    begin
      if FindHitsInItem(AItems,AIndex) then
         tmpFilter.Append(AIndex);

      if Assigned(OnProgress) then
      begin
        OnProgress(FStop);
        result:=FStop;
      end
      else
        result:=False;
    end;

  var t : TLoopInteger;
  begin
    tmpFilter:=nil;

    if Index=nil then
    begin
      for t:=0 to Source.Count-1 do
          if StopFindAt(t) then
             break;
    end
    else
    begin
      for t:=Low(Index) to High(Index) do
          if StopFindAt(Index[t]) then
             break;
    end;

    result:=tmpFilter;
  end;

  // For all rows, do search on a single data item
  function FindInItem(const AItem:TDataItem):TCursorIndex;
  var tmpFilter : TCursorIndex;

    function StopFindAt(const AIndex:TInteger):Boolean;
    begin
      if tmpMatch.Match(AsString(AItem,AIndex)) then
      begin
        tmpFilter.Append(AIndex);

        if Hits.Enabled then
           Hits.Append(AItem,AIndex);
      end;

      if Assigned(OnProgress) then
      begin
        OnProgress(FStop);
        result:=FStop;
      end
      else
        result:=False;
    end;

  var t : TLoopInteger;
  begin
    tmpFilter:=nil;

    if Index=nil then
    begin
      for t:=0 to Source.Count-1 do
          if StopFindAt(t) then
             break;
    end
    else
    begin
      for t:=Low(Index) to High(Index) do
          if StopFindAt(Index[t]) then
             break;
    end;

    result:=tmpFilter;
  end;

  // For all Items, do search on a single list item
  function FindInList(const AItem:TDataItem):TCursorIndex;
  var t : TLoopInteger;
      tmp : TDataArray;
  begin
    result:=nil;

    tmp:=AItem.Items.AsArray;

    for t:=0 to tmp.Count-1 do
    begin
      if tmpMatch.Match(tmp[t].Name) then
      begin
        result.Append(t);

        if Hits.Enabled then
           Hits.Append(AItem,t);
      end;

      if Assigned(OnProgress) then
      begin
        OnProgress(FStop);

        if FStop then
           break;
      end;
    end;
  end;

var tmpItem : TDataItem;
begin
  result:=nil;

  FStop:=False;

  // Load data
  Source.Load;

  // Prepare search options
  tmpMatch:=TMatchText.Create(Self,AText);
  try
    if AText='' then
       Hits.Items:=nil
    else
    begin
      if Items=nil then
         tmpItem:=Source
      else
         tmpItem:=Items;

      // Initialize hit array
      if Hits.Enabled then
         if (tmpItem.Kind<>TDataKind.dkUnknown) or tmpItem.AsTable then
            SetLength(Hits.Items,Source.Count)
         else
            SetLength(Hits.Items,tmpItem.Items.Count);

      if tmpItem.AsTable then
         result:=FindInItems(tmpItem.Items.AsArray)
      else
      if tmpItem.Kind=TDataKind.dkUnknown then
         result:=FindInList(tmpItem)
      else
         result:=FindInItem(tmpItem);
    end;
  finally
    tmpMatch.Free;
  end;
end;

// Cancel search. Stop background thread if it was running.
procedure TDataSearch.Stop;
begin
  FStop:=True;

  {$IFDEF THREADING}
  if Task<>nil then
     Task.Cancel;

  Task:=nil;
  {$ENDIF}
  
  Hits.Items:=nil;
end;

{$IFDEF THREADING}
type
  PDataSearch=^TDataSearch;
{$ENDIF}

// Calculate array of row indices that match AText in Source items,
// using a background thread.
// The resulting array is passed to OnFinished event when the thread finishes.

procedure TDataSearch.BackgroundFind(const AText:String);
{$IFDEF THREADING}
var tmp : PDataSearch;
{$ENDIF}
begin
  {$IFDEF THREADING}
  Stop;

  tmp:=@Self;

  if Source<>nil then
     Task:=TTask.Run(procedure
     begin
       tmp.Finish(tmp.DoFind(AText));
     end);
  {$ELSE}
  Finish(Find(AText));
  {$ENDIF}
end;

// Return array of row indices that match AText in Source items
function TDataSearch.Find(const AText:String):TCursorIndex;
begin
  Stop;

  if Source=nil then
     result:=nil
  else
     result:=DoFind(AText);

  Finish(result);
end;

procedure TDataSearch.Finish(const AIndex: TCursorIndex);
begin
  if Assigned(FOnFinished) then
     FOnFinished(AIndex);
end;

{ TSearchHits }

// Adds a new search hit for a given row
procedure TSearchHits.Append(const AItem: TDataItem; const ARow: TInteger);
begin
  Items[ARow].Add(AItem);
end;

// Returns the total number of search hits
function TSearchHits.Count:TInteger;
var t : Integer;
begin
  result:=0;

  if Enabled then
     for t:=0 to High(Items) do
         Inc(result,Items[t].Count);
end;

// Returns True when there exists a hit for AData in ARow
function TSearchHits.Exists(const ARow: TInteger; const AData: TDataItem): Boolean;
begin
  if Length(Items)>ARow then
     result:=Items[ARow].Exists(AData)
  else
     result:=False;
end;

// Speed semi-bottleneck. Sequential search.
function TSearchHits.IndexOf(const ARow:TInteger; const AData:TDataItem):TInteger;
var t : Integer;
begin
  result:=0;

  for t:=0 to ARow-1 do
      Inc(result,Items[t].Count);

  for t:=0 to High(Items[ARow]) do
      if Items[ARow][t]=AData then
         break
      else
         Inc(result);
end;

// Speed Bottleneck. Sequential search.
// Returns the row and data for a given "Index" search hit
procedure TSearchHits.Find(const AIndex:TInteger; out ARow:TInteger; out AData:TDataItem);
var L,
    tmp : TInteger;
    tmpRowItems : Integer;
begin
  ARow:=-1;
  tmp:=-1;
  AData:=nil;

  L:=Length(Items);

  if L>0 then
  while tmp<=AIndex do
  begin
    Inc(ARow);

    tmpRowItems:=Items[ARow].Count;

    if (tmp+tmpRowItems)>=AIndex then
    begin
      AData:=Items[ARow][AIndex-tmp-1];
      Exit;
    end;

    Inc(tmp,tmpRowItems);

    if ARow>=L then
    begin
      ARow:=-1;
      Exit;
    end;
  end;
end;

end.


