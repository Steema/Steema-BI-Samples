{*********************************************}
{  TeeBI Software Library                     }
{  TCrypto plugin for TurboPower LockBox      }
{  Copyright (c) 2015-2017 by Steema Software }
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
