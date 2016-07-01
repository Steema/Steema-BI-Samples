{*********************************************}
{  TeeBI Software Library                     }
{  Color selection from values                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.UI.Colors;

interface

uses
  {$IFDEF FPC}
  BI.FPC
  {$ELSE}
  System.UITypes
  {$ENDIF}
  ;

type
  TColorItem=record
  public
    Color : TAlphaColor;
    Value : Single;
  end;

  // Given a Palette of colors and linear position of each color (from 0 to 1),
  // the FromValue function returns the interpolated RGBA Color that corresponds
  // to AValue (from 0 to 1).
  TColorFunction=record
  private
    function CalcValue(const AValue: Single): TAlphaColor;
    procedure DefaultPalette;
    function FindPalette(const AValue: Single): Integer;
  public
    // Interpolated : Boolean;
    Inverted : Boolean;
    Palette : Array of TColorItem;

    // Add a new Color to palette
    procedure Add(const AValue:Single; const AColor:TAlphaColor);

    // Remove all colors from palette
    procedure Clear;

    // Returns the color that corresponds to AValue
    function FromValue(const AValue:Single):TAlphaColor;

    // Alpha Color to Integer RGB color
    class function ToRGB(const AColor:TAlphaColor):Integer; static;
  end;

implementation
