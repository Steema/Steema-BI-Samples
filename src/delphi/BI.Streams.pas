{*********************************************}
{  TeeBI Software Library                     }
{  Binary Reader and Writer classes           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Streams;

interface

(*
 TBIReader and TBIWriter classes are a blend of two different
 formats:

 A) TBinaryReader / TBinaryWriter  (raw binary)
 B) TReader / TWriter  (prefixed binary)

 The first byte in the Stream determines which mode is used to
 read or write.
 A byte 0 means to use "B", anything else means "A".

 Pros of A:

  -Small source code, easy to port to other languages (ie: C#)
  -No dependency on RTL.
  -More Speed: aprox 30% faster reading, 10% faster saving.

 Pros of B:

  -Aprox 10% decrease in size.
  -RTL compatible.
  -Provides "data integrity" checks (detects corruption).

*)

uses
  System.Classes, System.SysUtils;

{$IF SizeOf(Extended) = 10}
  {$DEFINE EXTENDEDIS10BYTES}  // 32bit cpu x86
{$ENDIF}

type
  TBIStreamer=class
  private
    FStream : TStream;

    IRaw : Boolean;
    IEncoding : TEncoding;

    // Buffering
    FBuffer: TBytes;
    FBufSize: Integer;
    FBufPos,
    FBufEnd: {$IFDEF FPC}Int64{$ELSE}Integer{$ENDIF};

    procedure InitBuffer(const BufferSize:Integer);
  public
    const
      DefaultBuffer=2048;

    Constructor Create(const AStream:TStream);

    property Raw:Boolean read IRaw;
  end;

  TBIReader=class(TBIStreamer)
  private
    IReader : TReader;

    function GetPosition: Int64;
    procedure ReadBuffer;
    procedure SetPosition(const Value: Int64);
  public
    Version : Integer;

    Constructor Create(const AStream:TStream;
                       const BufferSize:Integer=TBIStreamer.DefaultBuffer);

    Destructor Destroy; override;

    procedure Read(var Value; Size:Integer);
    function ReadBoolean:Boolean; inline;
    procedure ReadComponent(const Value:TComponent); inline;
    function ReadDate: TDateTime; inline;
    function ReadDouble: Double; {$IFNDEF FPC}inline;{$ENDIF}
    function ReadFloat: Extended; inline;
    function ReadInteger: Integer; inline;
    function ReadInt64: Int64; inline;
    function ReadSingle: Single; inline;
    function ReadString:String;

    property Position:Int64 read GetPosition write SetPosition;
  end;

  TBIWriter=class(TBIStreamer)
  private
    IWriter : TWriter;

    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
    procedure WriteBuffer;
  public
    Constructor Create(const AStream:TStream;
                       const BufferSize:Integer=TBIStreamer.DefaultBuffer;
                       const Raw:Boolean=False);
    Destructor Destroy; override;

    procedure FlushBuffer;

    procedure Write(const Value; Size:Integer);
    procedure WriteBoolean(const Value: Boolean); inline;
    procedure WriteComponent(const Value:TComponent); inline;
    procedure WriteDate(const Value: TDateTime); inline;
    procedure WriteDouble(const Value: Double); inline;
    procedure WriteFloat(const Value: Extended); inline;
    procedure WriteInteger(const Value: Integer); overload; inline;
    procedure WriteInteger(const Value: Int64); overload; inline;
    procedure WriteInt64(const Value: Int64); inline;
    procedure WriteSingle(const Value: Single); inline;
    procedure WriteString(const Value: String);

    property Position:Int64 read GetPosition write SetPosition;
  end;

implementation
