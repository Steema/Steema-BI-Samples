{*********************************************}
{  TeeBI Software Library                     }
{  Binary Reader and Writer classes           }
{  Copyright (c) 2015-2025 by Steema Software }
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
  {System.}Classes, {System.}SysUtils;

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

{ TBIStreamer }

Constructor TBIStreamer.Create(const AStream: TStream);
begin
  inherited Create;
  FStream:=AStream;
end;

procedure TBIStreamer.InitBuffer(const BufferSize:Integer);
begin
  FBufSize:=BufferSize;
  SetLength(FBuffer,BufferSize);
end;

{ TBIReader }

Constructor TBIReader.Create(const AStream: TStream; const BufferSize: Integer);
begin
  inherited Create(AStream);

  {$IFDEF FPC}
  AStream.Read(IRaw,SizeOf(IRaw));
  {$ELSE}
  AStream.ReadData(IRaw);
  {$ENDIF}

  if IRaw then
  begin
    InitBuffer(BufferSize);
    IEncoding:=TEncoding.UTF8;
  end
  else
    IReader:=TReader.Create(AStream,BufferSize);
end;

Destructor TBIReader.Destroy;
begin
  if IRaw then
     FStream.Seek(FBufPos - FBufEnd, TSeekOrigin.soCurrent)
  else
     IReader.Free;

  inherited;
end;

procedure TBIReader.ReadBuffer;
begin
  FBufEnd:=FStream.Read(FBuffer, {$IFNDEF FPC}0,{$ENDIF} Length(FBuffer));

  if FBufEnd = 0 then
     raise EReadError.Create('Stream Read Error');

  FBufPos:=0;
end;

procedure TBIReader.ReadComponent(const Value:TComponent);
begin
  IReader.ReadRootComponent(Value);
end;

// Copied from RTL and FPC Classes.pas

{$IFDEF FPC}
type
  TBinaryObjectReaderAccess=class(TBinaryObjectReader);
{$ENDIF}

function TBIReader.GetPosition: Int64;
begin
  if IRaw then
     result:=FStream.Position - (FBufEnd - FBufPos)
  else
     result:={$IFDEF FPC}
             FStream.Position - (TBinaryObjectReaderAccess(IReader.Driver).FBufEnd - TBinaryObjectReaderAccess(IReader.Driver).FBufPos)
             {$ELSE}
             IReader.Position
             {$ENDIF};
end;

procedure TBIReader.SetPosition(const Value: Int64);
begin
  if IRaw then
  begin
    FStream.Position:=Value;
    FBufPos:=0;
    FBufEnd:=0;
  end
  else
    {$IFDEF FPC}
    FStream.Position:=Value;
    TBinaryObjectReaderAccess(IReader.Driver).FBufPos:=0;
    TBinaryObjectReaderAccess(IReader.Driver).FBufEnd:=0;

    {$ELSE}
    IReader.Position:=Value;
   {$ENDIF}
end;

procedure TBIReader.Read(var Value; Size: Integer);
var
  LShouldRead: Longint;
  BufOffset: LongInt;
begin
  if IRaw then
  begin
    BufOffset := 0;

    while Size>0 do
    begin
      LShouldRead := FBufEnd - FBufPos;

      if LShouldRead = 0 then
      begin
        ReadBuffer;
        LShouldRead := FBufEnd;
      end;

      if Size <= LShouldRead then
         LShouldRead := Size;

      Move(Pointer(PByte(FBuffer) + FBufPos)^, Pointer(PByte(@Value) + BufOffset)^, LShouldRead);
      Inc(FBufPos, LShouldRead);
      Inc(BufOffset, LShouldRead);

      Dec(Size, LShouldRead);
    end;
  end
  else
    IReader.Read(Value,Size);
end;

function TBIReader.ReadBoolean: Boolean;
begin
  if IRaw then
     Read(result,SizeOf(Boolean))
  else
     result:=IReader.ReadBoolean;
end;

function TBIReader.ReadDate: TDateTime;
begin
  if IRaw then
     Read(result,SizeOf(TDateTime))
  else
     result:=IReader.ReadDate;
end;

function TBIReader.ReadDouble: Double;
begin
  if IRaw then
     Read(result,SizeOf(Double))
  else
     {$IFDEF FPC}
     if IReader.NextValue=TValueType(Ord(vaUTF8String)+1) then
     begin
       IReader.ReadValue;
       IReader.Read(Result,SizeOf(Double));
     end
     else
        result:=IReader.ReadFloat; // ?? should not pass here

     {$ELSE}
     result:=IReader.ReadDouble;
     {$ENDIF}
end;

{$IFDEF FPC}
{$IFDEF CPUX64}
type
  TExtended80Rec=record
    Bytes : array[0..9] of Byte;
  end;
{$ENDIF}
{$ENDIF}

function TBIReader.ReadFloat: Extended;
{$IFNDEF EXTENDEDIS10BYTES}
var E: TExtended80Rec;
{$ENDIF}
begin
  if IRaw then
  begin
    {$IFDEF EXTENDEDIS10BYTES}
    Read(result,SizeOf(Extended));
    {$ELSE}
    Read(E, SizeOf(E));

    {$IFDEF FPC}
    System.Move(E,result,SizeOf(E));
    {$ELSE}
    result:=Extended(E);
    {$ENDIF}

    {$ENDIF}
  end
  else
     result:=IReader.ReadFloat;
end;

function TBIReader.ReadInt64: Int64;
begin
  if IRaw then
     Read(result,SizeOf(Int64))
  else
     result:=IReader.ReadInt64;
end;

function TBIReader.ReadInteger: Integer;
begin
  if IRaw then
     Read(result,SizeOf(Integer))
  else
     result:=IReader.ReadInteger;
end;

function TBIReader.ReadSingle: Single;
begin
  if IRaw then
     Read(result,SizeOf(Single))
  else
     result:=IReader.ReadSingle;
end;

// Big speed optimization when reading strings.
// Aprox 3x when the strings are simple short strings or ansi strings
// (ie: no Unicode, no UTF8)

{.$DEFINE BISHORTSTRING}

function TBIReader.ReadString: String;

  {$IFDEF BISHORTSTRING}

  // Bypass RTL TReader using TEncoding to read plain simple strings
  function DoReadString:ShortString;
  var L: Byte;
  begin
    IReader.ReadValue;
    IReader.Read(L, SizeOf(Byte));
    SetLength(result, L);
    IReader.Read(result[1], L);
  end;

  // Bypass RTL TReader using TEncoding to read plain simple Ansi strings
  function DoReadAnsiString:AnsiString;
  var L: Integer;
  begin
    IReader.ReadValue;
    IReader.Read(L, SizeOf(Integer));
    SetLength(result, L);
    IReader.Read(result[1], L);
  end;
  {$ENDIF}

  // This code is here inside a function, to avoid DynArrayClear at the end
  // of TBIReader.ReadString
  function ReadEncoding(const ALength:Integer):String;
  var S : TBytes;
  begin
    SetLength(S,ALength);
    Read(S[0],ALength);
    result:=IEncoding.GetString(S,0,ALength);
  end;

var n : Integer;

    {$IFDEF BISHORTSTRING}
    tmp : TValueType;
    {$ENDIF}
begin
  if IRaw then
  begin
    n:=ReadInteger;

    if n=0 then
       result:=''
    else
       result:=ReadEncoding(n);
  end
  else
  begin
    {$IFDEF BISHORTSTRING}
    tmp:=IReader.NextValue;

    if tmp=vaString then
       result:=String(DoReadString)
    else
    if tmp=vaLString then
       result:=String(DoReadAnsiString)
    else
    {$ENDIF}
       result:=IReader.ReadString;
  end;
end;

{ TBIWriter }

Constructor TBIWriter.Create(const AStream: TStream; const BufferSize: Integer;
                             const Raw:Boolean);
begin
  inherited Create(AStream);

  IRaw:=Raw;

  {$IFDEF FPC}
  AStream.Write(IRaw,SizeOf(IRaw));
  {$ELSE}
  AStream.WriteData(IRaw);
  {$ENDIF}

  if IRaw then
  begin
    InitBuffer(BufferSize);
    IEncoding:=TEncoding.UTF8;
  end
  else
    IWriter:=TWriter.Create(AStream,BufferSize);
end;

Destructor TBIWriter.Destroy;
begin
  if IRaw then
     WriteBuffer
  else
     IWriter.Free;

  inherited;
end;

{$IFDEF FPC}
type
  TBinaryObjectWriterAccess=class(TBinaryObjectWriter);
{$ENDIF}

procedure TBIWriter.FlushBuffer;
begin
  if IRaw then
     WriteBuffer
  else
     {$IFDEF FPC}
     TBinaryObjectWriterAccess(IWriter.Driver).FlushBuffer;
     {$ELSE}
     IWriter.FlushBuffer
     {$ENDIF};
end;

function TBIWriter.GetPosition: Int64;
begin
  if IRaw then
     result:=FStream.Position - (FBufEnd - FBufPos)
  else
     result:={$IFDEF FPC}
             FStream.Position - (TBinaryObjectWriterAccess(IWriter.Driver).FBufEnd - TBinaryObjectWriterAccess(IWriter.Driver).FBufPos)
             {$ELSE}
             IWriter.Position
             {$ENDIF};
end;

procedure TBIWriter.SetPosition(const Value: Int64);
begin
  if IRaw then
  begin
    FStream.Position:=Value;
    FBufPos:=0;
    FBufEnd:=0;
  end
  else
    {$IFDEF FPC}
    FStream.Position:=Value;
    TBinaryObjectWriterAccess(IWriter.Driver).FBufPos:=0;
    TBinaryObjectWriterAccess(IWriter.Driver).FBufEnd:=0;

    {$ELSE}
    IWriter.Position:=Value;
    {$ENDIF}
end;

procedure TBIWriter.WriteBuffer;
begin
  FStream.WriteBuffer(FBuffer, FBufPos);
  FBufPos:=0;
end;

procedure TBIWriter.WriteComponent(const Value: TComponent);
begin
  IWriter.WriteRootComponent(Value);
end;

procedure TBIWriter.Write(const Value; Size: Integer);
var
  LShouldWrite: Longint;
  BufOffset: LongInt;
begin
  if IRaw then
  begin
    BufOffset := 0;

    while Size > 0 do
    begin
      LShouldWrite := FBufSize - FBufPos;

      if LShouldWrite = 0 then
      begin
        WriteBuffer;
        LShouldWrite := FBufSize;
      end;

      if LShouldWrite >= Size then
         LShouldWrite := Size;

      Move(Pointer(PByte(@Value) + BufOffset)^, Pointer(PByte(FBuffer) + FBufPos)^, LShouldWrite);
      Inc(FBufPos, LShouldWrite);
      Inc(BufOffset, LShouldWrite);

      Dec(Size, LShouldWrite);
    end;
  end
  else
    IWriter.Write(Value,Size);
end;

procedure TBIWriter.WriteBoolean(const Value: Boolean);
begin
  if IRaw then
     Write(Value,SizeOf(Boolean))
  else
     IWriter.WriteBoolean(Value);
end;

procedure TBIWriter.WriteDate(const Value: TDateTime);
begin
  if IRaw then
     Write(Value,SizeOf(TDateTime))
  else
     IWriter.WriteDate(Value);
end;

procedure TBIWriter.WriteDouble(const Value: Double);
begin
  if IRaw then
     Write(Value,SizeOf(Double))
  else
     IWriter.{$IFDEF FPC}WriteFloat{$ELSE}WriteDouble{$ENDIF}(Value);
end;

procedure TBIWriter.WriteFloat(const Value: Extended);
{$IFNDEF EXTENDEDIS10BYTES}
var E : TExtended80Rec;
{$ENDIF}
begin
  if IRaw then
  begin
    {$IFDEF EXTENDEDIS10BYTES}
    Write(Value,SizeOf(Extended));
    {$ELSE}

    {$IFDEF FPC}
    System.Move(Value,E,SizeOf(E));
    {$ELSE}
    E:=TExtended80Rec(Value);
    {$ENDIF}

    Write(E,SizeOf(E));
    {$ENDIF}
  end
  else
     IWriter.WriteFloat(Value);
end;

procedure TBIWriter.WriteInt64(const Value: Int64);
begin
  if IRaw then
     Write(Value,SizeOf(Int64))
  else
     IWriter.WriteInteger(Value);
end;

procedure TBIWriter.WriteInteger(const Value: Int64);
begin
  WriteInt64(Value);
end;

procedure TBIWriter.WriteInteger(const Value: Integer);
begin
  if IRaw then
     Write(Value,SizeOf(Integer))
  else
     IWriter.WriteInteger(Value);
end;

procedure TBIWriter.WriteSingle(const Value: Single);
begin
  if IRaw then
     Write(Value,SizeOf(Single))
  else
     IWriter.WriteSingle(Value);
end;

procedure TBIWriter.WriteString(const Value: String);
var B : TBytes;
begin
  if IRaw then
  begin
    if Value='' then
       WriteInteger(0)
    else
    begin
      B:=IEncoding.GetBytes(Value);
      WriteInteger(Length(B));
      Write(B[0],Length(B));
    end;
  end
  else
    IWriter.WriteString(Value);
end;

end.
