{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression plugin for mORMot SynZip   }
{  Copyright (c) 2016 by Steema Software      }
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
