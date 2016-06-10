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
  TShowMessageProc=procedure(const AText:String);

  TCommonUI=record
  public
    class var
      ShowMessage:TShowMessageProc;

    class procedure AddItems(const AData:TDataItem; const AItems:TStrings); static;
    class procedure AddInfo(const AData:TDataItem; const AItems:TStrings); static;
    class procedure AddKinds(const AItems:TStrings); static;
    class function BytesToString(const Bytes: Int64): String; static;
    class function ToBooleanString(const Bool:Boolean):String; static;
    class function UniqueName(const AComponent:TComponent):String; static;
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

  // Returns the list of distinct items in AData, optionally ordered by their
  // frequency (count appearance on the AData array)
  TDataMapAsData=class
  public
  type
    TDataMapOrder=(None,Item,Count);

    class function FromData(const AData:TDataItem;
                            const AddCounts:Boolean=True;
                            const Order:TDataMapOrder=TDataMapOrder.Count;
                            const Ascending:Boolean=False):TDataItem; static;
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

    function AlphaColorOf(const Value: Double): TAlphaColor;
    function ColorOf(const Value:Double):TColor;
    function Normalized(const Value:Double):Double;
    function ValidRange:Boolean;
  end;

  TDataColorizers=Array of TDataColorizer;

  TDataColorizersHelper=record helper for TDataColorizers
  public
    procedure Add(const AItem:TDataItem);
    function TryColorize(const AItem:TDataItem; const AIndex:Integer;
                         out APercent:Double; out AColorIndex:Integer):Boolean;
  end;

  TAlternateColor=class(TPersistent)
  private
    FColor : TColor;
    FEnabled : Boolean;

    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  protected
    FOnChange : TNotifyEvent;
  public
  const
    DefaultColor=$F2F2F2;

    Constructor Create;
    procedure Assign(Source:TPersistent); override;
  published
    property Color:TColor read FColor write SetColor default DefaultColor;
    property Enabled:Boolean read FEnabled write SetEnabled default False;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TDataKindConvert=record
  public
    class function Convert(const AData:TDataItem; const AKind:TDataKind):Boolean; static;
  end;

implementation
