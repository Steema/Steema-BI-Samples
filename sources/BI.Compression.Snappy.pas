{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression plugin for Google Snappy   }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Compression.Snappy;

interface

{$IFNDEF CPUX64}
{$MESSAGE FATAL 'Snappy works only with x64 bits CPU'}
{$ENDIF}

uses
  System.Classes, BI.Arrays.Strings, BI.Compression,
  Snappy;

type
  TSnappyCompression=class(TCompressionPlugin)
  private
    class var
      Env: psnappy_env;

    class procedure InitEnv; static;
    class procedure FreeEnv; static;
  public
    function Compress(const AStream:TStream; const AName:String):TMemoryStream; override;
    function DeCompress(const AStream:TStream; const AName:String):TMemoryStream; override;

    function DeCompress(const AFileName:String;
                        const APassword:String='';
                        const AError:TCompressionError=nil):TStringArray; override;
  end;

implementation

uses
  System.SysUtils;

{ TSnappyCompression }

function TSnappyCompression.Compress(const AStream: TStream;
  const AName: String): TMemoryStream;

  procedure DoError;
  begin
    raise EBIException.Create('Error compressing stream using Snappy');
  end;

var
  tmpSize : NativeUInt;

  procedure DoCompress(const AStream:TMemoryStream; var Dest:TMemoryStream);
  var outlen: Int64;
  begin
    if snappy_compress(Env, AStream.Memory, tmpSize, result.Memory, @outlen) = 0 then
       result.Size:=outlen
    else
       DoError;
  end;

var tmp : TMemoryStream;
begin
  InitEnv;

  result:=TMemoryStream.Create;

  tmpSize:=AStream.Size;

  result.Size:=snappy_max_compressed_length(tmpSize);

  if AStream is TMemoryStream then
     DoCompress(TMemoryStream(AStream),result)
  else
  begin
    tmp:=TMemoryStream.Create;
    try
      tmp.CopyFrom(AStream,AStream.Size);
      DoCompress(tmp,result);
    finally
      tmp.Free;
    end;
  end;
end;

function TSnappyCompression.DeCompress(const AStream: TStream;
  const AName: String): TMemoryStream;

  procedure DoError;
  begin
    raise EBIException.Create('Error compressing stream using Snappy');
  end;

  procedure DoUnCompress(const AStream:TMemoryStream; var Dest:TMemoryStream);
  var outlen: Int64;
      tmpSize : Int64;
  begin
    tmpSize:=AStream.Size;

    if snappy_uncompressed_length(AStream.Memory, tmpSize, @outlen) then
    begin
      result.Size:=outlen;

      snappy_uncompress(AStream.Memory, tmpSize, Dest.Memory);
    end
    else
       DoError;
  end;

var tmp : TMemoryStream;
begin
  InitEnv;

  result:=TMemoryStream.Create;

  if AStream is TMemoryStream then
     DoUnCompress(TMemoryStream(AStream),result)
  else
  begin
    tmp:=TMemoryStream.Create;
    try
      tmp.CopyFrom(AStream,AStream.Size);
      DoUnCompress(tmp,result);
    finally
      tmp.Free;
    end;
  end;
end;

class procedure TSnappyCompression.InitEnv;
var tmp : Integer;
begin
  if Env=nil then
  begin
    New(Env);

    tmp:=snappy_init_env(Env);

    if tmp<>0 then
       raise EBIException.Create('Error '+IntToStr(tmp)+' initializing Snappy');
  end;
end;

class procedure TSnappyCompression.FreeEnv;
begin
  snappy_free_env(Env);
  Dispose(Env);
end;

function TSnappyCompression.DeCompress(const AFileName, APassword: String;
  const AError: TCompressionError): TStringArray;
var tmp : TSystemCompression;
begin
  // *.zip files are not supported by Snappy, so use default

  tmp:=TSystemCompression.Create;
  try
    result:=tmp.DeCompress(AFileName,APassword,AError);
  finally
    tmp.Free;
  end;
end;

initialization
finalization
  if TSnappyCompression.Env<>nil then
     TSnappyCompression.FreeEnv;
end.
