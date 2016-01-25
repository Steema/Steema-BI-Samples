{*********************************************}
{  TeeBI Software Library                     }
{  Common VCL and FMX UI helper methods       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.UI;

interface

uses
  System.Classes,
  {$IFDEF FPC}
  BI_FPC,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  BI.Data, BI.UI.Colors;

type
  TDataItemsInfo=record
  private
    class procedure AddItems(const AItems:TDataItems); static;
    class procedure FillItemsOf(const AData,ADest:TDataItem); static;
  public
    class function ItemsOf(const AData:TDataItem):TDataItem; overload; static;
  end;

  TShowMessageProc={$IFNDEF FPC}reference to{$ENDIF} procedure(const AText:String);

  TCommonUI=record
  public
    class var
      ShowMessage:TShowMessageProc;

    class procedure AddItems(const AData:TDataItem; const AItems:TStrings); static;
    class procedure AddInfo(const AData:TDataItem; const AItems:TStrings); static;
    class function BytesToString(const Bytes: Int64): String; static;
    class function ToBooleanString(const Bool:Boolean):String; static;
  end;

  TCryptoClass=class of TCrypto;

  TCrypto=class
  public
    class var Engine : TCryptoClass; // default is "nil" (no encryption)

    class function Decrypt(const Text:String):String; overload; virtual;
    class function Decrypt(const Stream:TStream):TStream; overload; virtual;

    class function Encrypt(const Text:String):String; overload; virtual;
    class function Encrypt(const Stream:TStream):TStream; overload; virtual;
  end;

  // Returns a TDataItem with the Map values of a given AData
  TDataMapAsData=class
  public
    class function FromData(const AData:TDataItem):TDataItem; static;
  end;

  // Colorizer used by TBIGrid (both VCL and FMX)

  TColorizeMode=(Full,Left); // Full = full cell background

  TColorizeTextColor=(Automatic,Fixed); // Automatic = best Font color

  TDataColorizer=record
  private
    IValid : Boolean;

    IRange,
    IMin : Double;
  public
    Colors : TColorFunction;
    Data : TDataItem;
    Mode : TColorizeMode;
    TextColor : TColorizeTextColor;

    function ColorOf(const Value:Double):TColor;
    function Normalized(const Value:Double):Double;
    function ValidRange:Boolean;
  end;

  TDataColorizers=Array of TDataColorizer;

implementation
