{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression plugin for Google Snappy   }
{  Copyright (c) 2016 by Steema Software      }
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
