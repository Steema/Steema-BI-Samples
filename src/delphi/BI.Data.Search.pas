{*********************************************}
{  TeeBI Software Library                     }
{  Search Data                                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Search;

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

  BI.Data, BI.Arrays, BI.DataSource;

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
