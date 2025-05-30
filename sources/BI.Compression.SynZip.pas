{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression plugin for mORMot SynZip   }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Compression.SynZip;

// http://synopse.info/fossil/wiki?name=Downloads

interface

(*
  This unit defines two different Compression "plugin" classes that can be used
  with TeeBI TCompression class to zip streams faster than using the default
  System.Zip compression code.

  To activate SynZip or SynLZ:

  uses
    BI.Compression, BI.Compression.SynZip;

  TCompression.Plugin:=TSynLZCompression;


  To deactivate and use the standard System.Zip again:

  TCompression.Plugin:=TSystemCompression;


  Note:
    Compression is used at several places, for example at BIWeb server to
    transfer compressed TDataItem streams.

    Both sides of the communication (client and server) must use the same
    compression plugin.

*)

uses
  System.Classes, BI.Arrays.Strings, BI.Compression;

type
  TSynZipCompression=class(TCompressionPlugin)
  public
    function Compress(const AStream:TStream; const AName:String=''):TMemoryStream; override;
    function DeCompress(const AStream:TStream; const AName:String=''):TMemoryStream; override;

    function DeCompress(const AFileName:String;
                        const APassword:String='';
                        const AError:TCompressionError=nil):TStringArray; override;
  end;

  TSynLZCompression=class(TCompressionPlugin)
  public
    function Compress(const AStream:TStream; const AName:String=''):TMemoryStream; override;
    function DeCompress(const AStream:TStream; const AName:String=''):TMemoryStream; override;

    function DeCompress(const AFileName:String;
                        const APassword:String='';
                        const AError:TCompressionError=nil):TStringArray; override;
  end;

implementation

uses
  System.IOUtils,
  SynZip, SynLZ; // <-- mORMot compression units

{ TSynZipCompression }

function TSynZipCompression.Compress(const AStream: TStream;
  const AName: String): TMemoryStream;

  procedure DoError;
  begin
    raise EBIException.Create('Error: Empty SynZip compressed stream');
  end;

var tmpSize: Integer;
    tmp: Pointer;
begin
  result:=TMemoryStream.Create;

  tmpSize:=AStream.Size;

  {
  tmp:=TSynZipCompressor.Create(result,1);
  try
    tmp.Write(
  finally
    tmp.Free;
  end;
  }

  if AStream is TMemoryStream then
     CompressStream(TMemoryStream(AStream).Memory,tmpSize,result,1)
  else
  begin
    GetMem(tmp,tmpSize);
    try
      AStream.Read(tmp^, tmpSize);

      if CompressStream(tmp,tmpSize,result,1)=0 then
         DoError;
    finally
      FreeMem(tmp);
    end;
  end;
end;

function TSynZipCompression.DeCompress(const AStream: TStream;
  const AName: String): TMemoryStream;
var tmpSize: Integer;
    tmp: Pointer;
begin
  result:=TMemoryStream.Create;

  tmpSize:=AStream.Size;

  if AStream is TMemoryStream then
     UnCompressStream(TMemoryStream(AStream).Memory,tmpSize,result,nil)
  else
  begin
    GetMem(tmp,tmpSize);
    try
      AStream.Read(tmp^, tmpSize);

      UnCompressStream(tmp,tmpSize,result,nil);
    finally
      FreeMem(tmp);
    end;
  end;
end;

function TSynZipCompression.DeCompress(const AFileName, APassword: String;
  const AError: TCompressionError): TStringArray;
var tmp : TZipRead;
    t : Integer;
    tmpTemp,
    tmpFile : String;
begin
  tmp:=TZipRead.Create(AFileName);
  try
    SetLength(result,tmp.Count);

    tmpTemp:=TPath.GetTempPath;

    for t:=0 to result.Count-1 do
    begin
      tmpFile:=TPath.Combine(tmpTemp,tmp.Entry[t].zipName);

      tmp.UnZip(t,tmpTemp);

      result.Add(tmpFile);
    end;

  finally
    tmp.Free;
  end;
end;

{ TSynLZCompression }

function TSynLZCompression.Compress(const AStream: TStream;
  const AName: String): TMemoryStream;
var
  tmpSize : Int64;

  procedure DoCompress(const AStream:TMemoryStream; var Dest:TMemoryStream);
  var tmp : Integer;
  begin
    tmp:=SynLZcompress1(AStream.Memory,tmpSize,Dest.Memory);
    result.Size:=tmp;
  end;

var tmp : TMemoryStream;
begin
  tmpSize:=AStream.Size;

  result:=TMemoryStream.Create;
  result.Size:=SynLZcompressdestlen(tmpSize);

  if AStream is TMemoryStream then
     DoCompress(TMemoryStream(AStream),result)
  else
  begin
    tmp:=TMemoryStream.Create;
    try
      tmp.CopyFrom(AStream,tmpSize);
      DoCompress(tmp,result);
    finally
      tmp.Free;
    end;
  end;
end;

function TSynLZCompression.DeCompress(const AStream: TStream;
  const AName: String): TMemoryStream;
var
  tmpSize : Int64;

  procedure DoUnCompress(const AStream:TMemoryStream; var Dest:TMemoryStream);
  var tmp : Integer;
  begin
    tmp:=SynLZDecompress1(AStream.Memory,tmpSize,Dest.Memory);
    result.Size:=tmp;
  end;

var tmp : TMemoryStream;
begin
  tmpSize:=AStream.Size;

  result:=TMemoryStream.Create;
  result.Size:=SynLZcompressdestlen(tmpSize);

  if AStream is TMemoryStream then
     DoUnCompress(TMemoryStream(AStream),result)
  else
  begin
    tmp:=TMemoryStream.Create;
    try
      tmp.CopyFrom(AStream,tmpSize);
      DoUnCompress(tmp,result);
    finally
      tmp.Free;
    end;
  end;
end;

function TSynLZCompression.DeCompress(const AFileName, APassword: String;
  const AError: TCompressionError): TStringArray;
var tmp : TSynZipCompression;
begin
  tmp:=TSynZipCompression.Create;
  try
    result:=tmp.DeCompress(AFileName,APassword,AError);
  finally
    tmp.Free;
  end;
end;

end.
