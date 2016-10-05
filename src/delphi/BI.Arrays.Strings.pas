unit BI.Arrays.Strings;

interface

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
