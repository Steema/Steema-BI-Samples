{*********************************************}
{  TeeBI Software Library                     }
{  Zip Compression plugin for d7zip API       }
{  https://github.com/geoffsmith82/d7zip      }
{                                             }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Compression.d7zip;

// https://github.com/geoffsmith82/d7zip

{

 Author: Henri Gourvest hgourvest@progdigy.com
 Licence: MPL1.1
 Date: 2011-11-29
 Version: 1.2
 Original source at Google Code
 Extended version by Daniel Marschall 2024-05-15 with a lot of changes (see sevenzip.pas header for changelog)
 Example usage Demo added by Geoffrey Smith
}

interface

(*
  This unit defines a Compression "plugin" class that can be used
  with TeeBI TCompression class to zip streams faster than using the default
  System.Zip compression code.

  Also to import 7z supported files, into TDataItem objects.

  To activate d7zip:

  uses
    BI.Compression, BI.Compression.d7zip;

  TCompression.Plugin:=Td7zipCompression;


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
  Td7zipCompression=class(TCompressionPlugin)
  public
    function Compress(const AStream:TStream; const AName:String=''):TMemoryStream; override;
    function DeCompress(const AStream:TStream; const AName:String=''):TMemoryStream; override;

    function DeCompress(const AFileName:String;
                        const APassword:String='';
                        const AError:TCompressionError=nil):TStringArray; override;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.IOUtils, BI.Arrays,
  SevenZip; // <-- d7zip compression unit


{ Td7zipCompression }

function Td7ZipCompression.Compress(const AStream: TStream;
  const AName: String): TMemoryStream;
var Arch : I7zOutArchive;
    tmpTime : TFileTime;
begin
  Arch:=CreateOutArchive(CLSID_CFormat7z);

  tmpTime:=CurrentFileTime;

  Arch.AddStream(AStream, soReference, 4, //Ord(TFileAttribute.faArchive),
     tmpTime, tmpTime, AName, False, False);

//  SetCompressionLevel(Arch, 5);
//  SevenZipSetCompressionMethod(Arch, m7BZip2);
//  Arch.SetPassword('password');
//  Arch.SaveToFile('c:\test.zip');

  result:=TMemoryStream.Create;
  Arch.SaveToStream(result);
end;

function Td7ZipCompression.DeCompress(const AStream: TStream;
  const AName: String): TMemoryStream;
var Arch : I7zInArchive;
    strm : IInStream;
    t : Integer;
begin
  Arch:=CreateInArchive(CLSID_CFormat7z);
  strm:=T7zStream.Create(AStream, soReference);
  Arch.OpenStream(strm);

  for t:=0 to Arch.NumberOfItems-1 do
      if not Arch.ItemIsFolder[t] then
         if SameText(Arch.ItemPath[t],AName) then
         begin
           result:=TMemoryStream.Create;
           Arch.ExtractItem(t,result,False);

           Exit;
         end;

  raise EBIException.Create('Error: zip file does not contain data named: '+AName);
end;

function Td7ZipCompression.DeCompress(const AFileName, APassword: String;
  const AError: TCompressionError): TStringArray;
var Arch : I7zInArchive;
    t : Integer;
    tmpTemp,
    tmpFile : String;
    tmpStream : TStream;
begin
  Arch:=CreateInArchive(CLSID_CFormat7z);
  Arch.SetPassword(APassword);
  Arch.OpenFile(AFileName);

  tmpTemp:=TPath.GetTempPath;

  SetLength(result,Arch.NumberOfItems);

  for t:=0 to result.Count-1 do
  begin
    tmpFile:=TPath.Combine(tmpTemp,Arch.ItemPath[t]);

    tmpStream:=TFileStream.Create(tmpFile,fmOpenWrite);
    try
      Arch.ExtractItem(t,tmpStream,False);
    finally
      tmpStream.Free;
    end;

    result.Add(tmpFile);
  end;
end;

end.
