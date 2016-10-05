{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression abstract class             }
{  Copyright (c) 2016 by Steema Software      }
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
  System.Classes,
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
