{*********************************************}
{  TeeBI Software Library                     }
{  SQL Language support                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.SQL;

interface

uses
  BI.Arrays, BI.Data, BI.DataSource, BI.Summary, BI.Expression,
  BI.Persist;

type
  ESQLParser=class(EBIException);

  // Converts an SQL string to its equivalent query (TDataSelect) or
  // summary (TSummary) object instance
  TSQLParser=class
  private
    FData : TDataItem;
    FError : TBIErrorProc;
    FText : String;

    ILength,
    IPos : Integer;

    procedure DoError(const AError:String);
    function EndOfText:Boolean;
    function GetExpression: String;
    function GetExpressions:TTextArray;
    function NextIdent:String;
    function Optional(const AText:String):Boolean;
  public
    Constructor Create(const AData:TDataItem; const AText:String);

    function Calculate:TDataItem;

    class function FindAggregate(var S:String; out Agg:TAggregate):Boolean; static;
    class function FindGroupByPart(var S:String; out APart:TDateTimePart):Boolean; static;
    class function GetFunction(var S:String; out AFunc:String):Boolean; static;

    function Parse(const ErrorProc:TBIErrorProc=nil):TObject;

    class procedure ParseSort(const AData:TDataItem; var ASort:TSortItems; const AOrder:TTextArray;
                              const SQL:Boolean=False;
                              const Error:TBIErrorProc=nil); static;
    class function StringToData(const AData:TDataItem; const S:String; const ErrorProc:TBIErrorProc=nil):TDataItem; static;
  end;

  // Converts a query (TDataSelect) or summary (TSummary) object instance into
  // its equivalent SQL string
  TBISQL=record
  private
    class function FilterOf(const AFilter:TExpression; const AWhereCount:Integer):String; static;

    // Return the "equivalent" SQL script:
    class function From(const ASelect:TDataSelect):String; overload; static;

    // Return the "equivalent" SQL script:
    class function From(const ASummary:TSummary):String; overload; static;

    // Return SQL from AData Provider:
    class function From(const AData:TDataItem):String; overload; static;

    class function FromAndWhere(const AMain:TDataItem; const ADatas:TDataArray):String; static;
  public
    // Return the "equivalent" SQL script:
    class function From(const AProvider:TDataProvider):String; overload; static;

    // Parse SQL syntax, execute and return result
    class function From(const AData:TDataItem; const SQL:String):TDataItem; overload; static;
  end;

implementation
