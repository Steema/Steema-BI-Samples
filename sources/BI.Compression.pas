{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression abstract class             }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Compression;

interface

{
 TCompression implements zip / unzip compression of a TStream or *.zip file

 The TCompression.Plugin property determines which "engine" is used:

 Default: "System.Zip" Delphi RTL

 Alternative:

   mORMot SynLZ:

    uses BI.Compression, BI.Compression.SynZip;
    TCompression.Plugin:=TSynZipCompression;

   To reset back to default:

    TCompression.Plugin:=TSystemCompression;

 Notes:

   TCompression.Compress method does not alter the Stream Position, it compresses
   starting from the current position.
}

uses
  {System.}Classes,
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.Zip,
  {$ENDIF}

  BI.Arrays.Strings;

type
  TCompressionError=function(const Text:String):Boolean of object;

  TCompressionPlugin=class abstract
  public
    function Compress(const AStream:TStream; const AName:String):TMemoryStream; virtual; abstract;
    function DeCompress(const AStream:TStream; const AName:String):TMemoryStream; overload; virtual; abstract;
    function DeCompress(const AFileName:String;
                        const APassword:String='';
                        const AError:TCompressionError=nil):TStringArray; overload; virtual; abstract;
  end;

  TCompressionPluginClass=class of TCompressionPlugin;

  TCompression=record
  private
    class var
      IPlugin : TCompressionPlugin;

    class function GetPlugin:TCompressionPluginClass; static;
    class procedure SetPlugin(const Value:TCompressionPluginClass); static;
  public
    class function Compress(const AStream:TStream; const AName:String):TMemoryStream; static; inline;
    class function DeCompress(const AStream:TStream; const AName:String):TMemoryStream; overload; static; inline;
    class function DeCompress(const AFileName:String;
                              const APassword:String='';
                              const AError:TCompressionError=nil):TStringArray; overload; static; inline;

    class property Plugin : TCompressionPluginClass read GetPlugin write SetPlugin;
  end;

{$IFNDEF FPC}
{$IF CompilerVersion>=31}
{$DEFINE ENCRYPTEDZIP}
{$ENDIF}
{$ENDIF}

type
  TSystemCompression=class(TCompressionPlugin)
  {$IFDEF ENCRYPTEDZIP}
  private
    function DecryptZipStream(const InStream: TStream;
                              const ZipFile: TZipFile;
                              const Item: TZipHeader;
                              IsEncrypted: Boolean): TStream;
  {$ENDIF}

  public
    function Compress(const AStream:TStream; const AName:String):TMemoryStream; override;
    function DeCompress(const AStream:TStream; const AName:String):TMemoryStream; override;

    function DeCompress(const AFileName:String;
                        const APassword:String='';
                        const AError:TCompressionError=nil):TStringArray; override;
  end;

implementation

uses
  {System.}SysUtils,

  {$IFNDEF FPC}
  System.IOUtils,
  {$ENDIF}

  BI.Languages.English, BI.UI, BI.Arrays;

{ TSystemCompression }

{$IFDEF ENCRYPTEDZIP}
function TSystemCompression.DecryptZipStream(const InStream: TStream;
                                             const ZipFile: TZipFile;
                                             const Item: TZipHeader;
                                             IsEncrypted: Boolean): TStream;
begin
  if IsEncrypted then
     result:=TCrypto.Decrypt(InStream) // <-- Pending !
  else
     result:=InStream;
end;
{$ENDIF}

function TSystemCompression.Compress(const AStream:TStream; const AName:String):TMemoryStream;
var Z : TZipFile;
begin
  Z:=TZipFile.Create;
  try
    result:=TMemoryStream.Create;
    Z.Open(result,TZipMode.zmWrite);
    Z.Add(AStream,AName);
    Z.Close;
  finally
    Z.Free;
  end;
end;

function TSystemCompression.DeCompress(const AStream:TStream; const AName:String):TMemoryStream;

  procedure DoError;
  begin
    raise EBIException.Create(BIMsg_Web_ErrorUnzip);
  end;

var zip : TZipFile;
    tmpOut : TBytes;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIF}

  zip:=TZipFile.Create;
  try
    zip.Open(AStream,TZipMode.zmRead);
    zip.Read(AName,tmpOut);

    if Length(tmpOut)=0 then
       DoError;

    result:=TBytesStream.Create(tmpOut);
  finally
    zip.Free;
  end;
end;

function TSystemCompression.DeCompress(const AFileName:String;
                                       const APassword:String;
                                       const AError:TCompressionError):TArrayOfStrings{TStringArray};
  procedure DoError;
  var tmp : String;
  begin
    tmp:=Format(BIMsg_ZIP_FileNotValid,[AFileName]);

    if (not Assigned(AError)) or (not AError(tmp)) then
       raise EBIException.Create(tmp);
  end;

var z : TZipFile;
    tmpFile,
    tmpTemp : String;
    t : Integer;
begin
  result:=nil;

  if TZipFile.IsValid(AFileName) then
  begin
    z:=TZipFile.Create;
    try
      // Pending: CompressionLevel = Default, Deflate...
      // TZipFile.RegisterCompressionHandler(...

      {$IFDEF ENCRYPTEDZIP}
      if APassword<>'' then
         z.OnCreateDecompressStream:=DecryptZipStream;
      {$ENDIF}

      z.Open(AFileName, zmRead);
      try
        tmpTemp:=TPath.GetTempPath;

        for t:=0 to z.FileCount-1 do
        begin
          tmpFile:=TPath.Combine(tmpTemp,z.FileName[t]);

          z.Extract(t,tmpTemp,False);
          result.Add(tmpFile);
        end;
      finally
        z.Close;
      end;
    finally
      z.Free;
    end;
  end
  else
    DoError;
end;

{ TCompression }

class function TCompression.Compress(const AStream: TStream; const AName:String): TMemoryStream;
begin
  result:=IPlugin.Compress(AStream,AName);
end;

class function TCompression.DeCompress(const AStream:TStream; const AName:String):TMemoryStream;
begin
  result:=IPlugin.DeCompress(AStream,AName);
end;

class function TCompression.GetPlugin: TCompressionPluginClass;
begin
  if IPlugin=nil then
     result:=nil
  else
     result:=TCompressionPluginClass(IPlugin.ClassType);
end;

class procedure TCompression.SetPlugin(const Value: TCompressionPluginClass);
begin
  IPlugin.Free;
  IPlugin:=Value.Create;
end;

class function TCompression.DeCompress(const AFileName:String;
                                       const APassword:String='';
                                       const AError:TCompressionError=nil):TArrayOfStrings{TStringArray};
begin
  result:=IPlugin.DeCompress(AFileName,APassword,AError);
end;

{$IFNDEF FPC}
initialization
  TCompression.Plugin:=TSystemCompression;
finalization
  TCompression.IPlugin.Free;
{$ENDIF}
end.
