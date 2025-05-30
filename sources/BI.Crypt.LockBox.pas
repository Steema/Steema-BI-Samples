{*********************************************}
{  TeeBI Software Library                     }
{  TCrypto plugin for TurboPower LockBox      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Crypt.LockBox;

interface

uses
  System.Classes, BI.UI, LbClass;

type
  TBICipher=(Blowfish, DES, DES3, Rijndael);

  TBILockBox=class(TCrypto)
  private
    class function GetCipher: TBICipher; static;
    class procedure SetCipher(const Value: TBICipher); static;
  protected
    class var FCipher : TLbSymmetricCipher;
  public
    class function Decrypt(const Text:String):String; override;
    class function Decrypt(const Stream:TStream):TStream; override;

    class function Encrypt(const Text:String):String; override;
    class function Encrypt(const Stream:TStream):TStream; override;

    class procedure SetKey(const AKey:String); static;

    class property Cipher:TBICipher read GetCipher write SetCipher
                 default TBICipher.Blowfish;
  end;

implementation

{ TBILockBox }

class function TBILockBox.Decrypt(const Text: String): String;
begin
  result:=FCipher.DecryptString(Text);
end;

class function TBILockBox.Encrypt(const Text: String): String;
begin
  result:=FCipher.EncryptString(Text);
end;

class function TBILockBox.Encrypt(const Stream: TStream): TStream;
begin
  result:=TMemoryStream.Create;
  FCipher.EncryptStream(Stream,result);
end;

class function TBILockBox.GetCipher: TBICipher;
begin
  if FCipher is TLbBlowfish then
     result:=TBICipher.Blowfish
  else
  if FCipher is TLbDES then
     result:=TBICipher.DES
  else
  if FCipher is TLb3DES then
     result:=TBICipher.DES3
  else
     result:=TBICipher.Rijndael;
end;

class procedure TBILockBox.SetCipher(const Value: TBICipher);
begin
  FCipher.Free;

  case Value of
    Blowfish: FCipher:=TLbBlowfish.Create(nil);
         DES: FCipher:=TLbDES.Create(nil);
        DES3: FCipher:=TLb3DES.Create(nil);
    Rijndael: FCipher:=TLbRijndael.Create(nil);
  end;

  FCipher.GenerateRandomKey;
end;

class procedure TBILockBox.SetKey(const AKey: String);
begin
  FCipher.GenerateKey(AKey);
end;

class function TBILockBox.Decrypt(const Stream: TStream): TStream;
begin
  result:=TMemoryStream.Create;
  FCipher.DecryptStream(Stream,result);
end;

initialization
  TBILockBox.Cipher:=TBICipher.Blowfish;
  TCrypto.Engine:=TBILockBox;
finalization
  TBILockBox.FCipher.Free;
  TCrypto.Engine:=nil;
end.
