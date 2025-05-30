{*********************************************}
{  TeeBI Software Library                     }
{  Enhanced TTextReader (speed optimized)     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Streams.Reader;

interface

uses
  System.Classes, System.SysUtils;

type
  TBIStreamReader = class(TTextReader)
  private
    FBufferedData: TStringBuilder;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FEncoding: TEncoding;
    FNoDataInStream: Boolean;
    FOwnsStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TStream;
    function DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding);
    function GetEndOfStream: Boolean;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; DetectBOM: Boolean); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding;
      DetectBOM: Boolean = False; BufferSize: Integer = 4096); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string; Encoding: TEncoding;
      DetectBOM: Boolean = False; BufferSize: Integer = 4096); overload;
    destructor Destroy; override;
    procedure Close; override;
//    procedure DiscardBufferedData;
    procedure OwnStream; inline;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(var Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(var Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    property BaseStream: TStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
  end;

implementation

{ TBIStreamReader }

constructor TBIStreamReader.Create(Stream: TStream);
begin
  Create(Stream, TEncoding.UTF8, True);
end;

constructor TBIStreamReader.Create(Stream: TStream; DetectBOM: Boolean);
begin
  Create(Stream, TEncoding.UTF8, DetectBOM);
end;

constructor TBIStreamReader.Create(Stream: TStream; Encoding: TEncoding;
  DetectBOM: Boolean; BufferSize: Integer);
begin
  inherited Create;

  {
  if not Assigned(Stream) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Stream']); // DO NOT LOCALIZE
  if not Assigned(Encoding) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE
  }

  FBufferedData := TStringBuilder.Create;
  FEncoding := Encoding;
  FBufferSize := BufferSize;
  if FBufferSize < 128 then
    FBufferSize := 128;
  FNoDataInStream := False;
  FStream := Stream;
  FOwnsStream := False;
  FDetectBOM := DetectBOM;
  FSkipPreamble := not FDetectBOM;
end;

constructor TBIStreamReader.Create(const Filename: string; Encoding: TEncoding;
  DetectBOM: Boolean; BufferSize: Integer);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), Encoding, DetectBOM, BufferSize);
  FOwnsStream := True;
end;

constructor TBIStreamReader.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), DetectBOM);
  FOwnsStream := True;
end;

constructor TBIStreamReader.Create(const Filename: string);
begin
  Create(TFileStream.Create(Filename, fmOpenRead));
  FOwnsStream := True;
end;

destructor TBIStreamReader.Destroy;
begin
  Close;
  inherited;
end;

procedure TBIStreamReader.Close;
begin
  if (FStream <> nil) and FOwnsStream then
  begin
    FStream.Free;
    FStream := nil;
  end;

  if FBufferedData <> nil then
  begin
    FBufferedData.Free;
    FBufferedData := nil;
  end;
end;

{
procedure TBIStreamReader.DiscardBufferedData;
begin
  if FBufferedData <> nil then
  begin
    FBufferedData.Remove(0, FBufferedData.Length);
    FNoDataInStream := False;
  end;
end;
}

function TBIStreamReader.DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
var
  LEncoding: TEncoding;
begin
  // try to automatically detect the buffer encoding
  LEncoding := nil;
  Result := TEncoding.GetBufferEncoding(Buffer, LEncoding);
  // detected encoding points to Default and param Encoding requests some other
  // type of Encoding; set the Encoding param to UTF8 (Default)
  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
    Encoding := TEncoding.UTF8
  else
    Encoding := LEncoding;

  FDetectBOM := False;
end;

procedure TBIStreamReader.FillBuffer(var Encoding: TEncoding);
const
  BufferPadding = 4;
var
  LString: string;
  LBuffer: TBytes;
  BytesRead: Integer;
  StartIndex: Integer;
  ByteCount: Integer;
  ByteBufLen: Integer;
  ExtraByteCount: Integer;

  procedure AdjustEndOfBuffer(const LBuffer: TBytes; Offset: Integer);
  var
    Pos, Size: Integer;
    Rewind: Integer;
  begin
    Dec(Offset);
    for Pos := Offset downto 0 do
    begin
      for Size := Offset - Pos + 1 downto 1 do
      begin
        if Encoding.GetCharCount(LBuffer, Pos, Size) > 0 then
        begin
          Rewind := Offset - (Pos + Size - 1);
          FStream.Position := FStream.Position - Rewind;
          BytesRead := BytesRead - Rewind;
          Exit;
        end;
      end;
    end;
  end;

begin
  SetLength(LBuffer, FBufferSize + BufferPadding);

  // Read data from stream
  BytesRead := FStream.Read(LBuffer[0], FBufferSize);
  FNoDataInStream := BytesRead < FBufferSize;

  // Adjust the end of the buffer to be sure we have a valid encoding
  if not FNoDataInStream then
    AdjustEndOfBuffer(LBuffer, BytesRead);

  // Check for byte order mark and calc start index for character data
  if FDetectBOM then
    StartIndex := DetectBOM(Encoding, LBuffer)
  else if FSkipPreamble then
    StartIndex := SkipPreamble(Encoding, LBuffer)
  else
    StartIndex := 0;

  // Convert to string and calc byte count for the string
  ByteBufLen := BytesRead - StartIndex;
  LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
  ByteCount := FEncoding.GetByteCount(LString);

  // If byte count <> number of bytes read from the stream
  // the buffer boundary is mid-character and additional bytes
  // need to be read from the stream to complete the character
  ExtraByteCount := 0;
  while (ByteCount <> ByteBufLen) and (ExtraByteCount < FEncoding.GetMaxByteCount(1)) do
  begin
    // Expand buffer if padding is used
    if (StartIndex + ByteBufLen) = Length(LBuffer) then
      SetLength(LBuffer, Length(LBuffer) + BufferPadding);

    // Read one more byte from the stream into the
    // buffer padding and convert to string again
    BytesRead := FStream.Read(LBuffer[StartIndex + ByteBufLen], 1);
    if BytesRead = 0 then
      // End of stream, append what's been read and discard remaining bytes
      Break;

    Inc(ExtraByteCount);

    Inc(ByteBufLen);
    LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
    ByteCount := FEncoding.GetByteCount(LString);
  end;

  // Add string to character data buffer
  FBufferedData.Append(LString);
end;

function TBIStreamReader.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (FBufferedData <> nil) and (FBufferedData.Length < 1) then
    FillBuffer(FEncoding);
  Result := FNoDataInStream and ((FBufferedData = nil) or (FBufferedData.Length = 0));
end;

procedure TBIStreamReader.OwnStream;
begin
  FOwnsStream := True;
end;

function TBIStreamReader.Peek: Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    if FBufferedData.Length < 1 then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData.Chars[0]);
  end;
end;

function TBIStreamReader.Read(var Buffer: TCharArray; Index,
  Count: Integer): Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    while (FBufferedData.Length < Count) and (not EndOfStream) and (not FNoDataInStream) do
      FillBuffer(FEncoding);

    if FBufferedData.Length > Count then
      Result := Count
    else
      Result := FBufferedData.Length;

    FBufferedData.CopyTo(0, Buffer, Index, Result);
    FBufferedData.Remove(0, Result);
  end;
end;

function TBIStreamReader.ReadBlock(var Buffer: TCharArray; Index,
  Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TBIStreamReader.Read: Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    if FBufferedData.Length < 1 then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData.Chars[0]);
    FBufferedData.Remove(0, 1);
  end;
end;

function TBIStreamReader.ReadLine: string;
var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;
  LChar : Char; // <-- this small change improves reading speed 80%
begin
  Result := '';
  if FBufferedData = nil then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;

  while True do
  begin
    if (NewLineIndex + 2 > FBufferedData.Length) and (not FNoDataInStream) then
      FillBuffer(FEncoding);

    if NewLineIndex >= FBufferedData.Length then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        FillBuffer(FEncoding);
        if FBufferedData.Length = 0 then
          Break;
      end;
    end;

    LChar:=FBufferedData[NewLineIndex];

    if LChar = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end
    else
    if (LChar = #13) and (NewLineIndex + 1 < FBufferedData.Length) and (FBufferedData[NewLineIndex + 1] = #10) then
    begin
      PostNewLineIndex := NewLineIndex + 2;
      Break;
    end
    else
    if LChar = #13 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end;

    Inc(NewLineIndex);
  end;

  Result := FBufferedData.ToString;
  SetLength(Result, NewLineIndex);
  FBufferedData.Remove(0, PostNewLineIndex);
end;

function TBIStreamReader.ReadToEnd: string;
begin
  Result := '';
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    repeat
      FillBuffer(FEncoding);
    until FNoDataInStream;
    Result := FBufferedData.ToString;
    FBufferedData.Remove(0, FBufferedData.Length);
  end;
end;

function TBIStreamReader.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
var
  I: Integer;
  LPreamble: TBytes;
  BOMPresent: Boolean;
begin
  Result := 0;
  LPreamble := Encoding.GetPreamble;
  if (Length(LPreamble) > 0) then
  begin
    if Length(Buffer) >= Length(LPreamble) then
    begin
      BOMPresent := True;
      for I := 0 to Length(LPreamble) - 1 do
        if LPreamble[I] <> Buffer[I] then
        begin
          BOMPresent := False;
          Break;
        end;
      if BOMPresent then
        Result := Length(LPreamble);
    end;
  end;
  FSkipPreamble := False;
end;

end.
