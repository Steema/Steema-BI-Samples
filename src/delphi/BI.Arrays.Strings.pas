{*********************************************}
{  TeeBI Software Library                     }
{  TStringArray and helper type               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Arrays.Strings;

interface

// TStringArray is a lightweight "array of string",
// similar to BI.Arrays TTextArray type.

// Main purpose of TStringArray is to implement a "Split" function syntax
// compatible with all versions of both FreePascal and Delphi (VCL and FMX)

type
  TStringArray=Array of String;

  TStringArrayHelper=record helper for TStringArray
  public
    procedure Add(const S:String);
    function Count:Integer; inline;

    class function Split(const S,Delimiter:String):TStringArray; overload; static;
    class function Split(const S:String; const Delimiter:Char):TStringArray; overload; static;
    class function Split(const S:String; const Delimiter,Quote:Char):TStringArray; overload; static;
  end;

implementation
