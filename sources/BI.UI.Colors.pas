{*********************************************}
{  TeeBI Software Library                     }
{  Color selection from values                }
{  Copyright (c) 2015-2025 by Steema Software }
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

  TColorItems=Array of TColorItem;

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
    Palette : TColorItems;

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

{ TColorFunction }

procedure TColorFunction.Add(const AValue: Single; const AColor: TAlphaColor);
var L : Integer;
begin
  L:=Length(Palette);
  SetLength(Palette,L+1);

  Palette[L].Color:=AColor;
  Palette[L].Value:=AValue;
end;

procedure TColorFunction.Clear;
begin
  Palette:=nil;
end;

// Default colors: Green -> Yellow -> Red
procedure TColorFunction.DefaultPalette;
begin
  Clear;

//  Add(0,TAlphaColors.Green);
//  Add(1,TAlphaColors.Red);

  Add(0,TAlphaColors.Green);
  Add(0.5,TAlphaColors.Yellow);
  Add(1,TAlphaColors.Red);
end;

// Returns the index of the nearest color in the palette that
// belongs to AValue (from 0 to 1)
function TColorFunction.FindPalette(const AValue: Single): Integer;
var Min,
    Mid,
    Max : Integer;
begin
  Max:=High(Palette);
  Min:=0;

  while Min<Max do
  begin
    Mid:=(Min+Max) div 2;

    if Palette[Mid].Value<AValue then
       Min:=Succ(Mid)
    else
       Max:=Mid;
  end;

  if Max=Min then
     result:=Min
  else
     result:=High(Palette);
end;

// Returns the index of the nearest color in the palette that
// belongs to AValue (from 0 to 1, or 1 to 0 depending on the Inverted property)
function TColorFunction.FromValue(const AValue: Single): TAlphaColor;
begin
  if Inverted then
     result:=CalcValue(1-AValue)
  else
     result:=CalcValue(AValue);
end;

// Returns the interpolated color that belongs to AValue
// (from Min to Max, or Max to Min depending on the Inverted property)
function TColorFunction.CalcValue(const AValue: Single): TAlphaColor;
type
  TRGBAInteger=record
    R,G,B,A : Integer;
  end;

var tmp : Integer;

    Percent,
    tmpPrevious,
    tmpRange : Single;

    IStart,
    IEnd : TAlphaColorRec;

    IRange : TRGBAInteger;
begin
  if Palette=nil then
     DefaultPalette;

  tmp:=FindPalette(AValue);

  if tmp=0 then
     result:=Palette[0].Color
  else
  begin
    tmpPrevious:=Palette[tmp-1].Value;

    tmpRange:=Palette[tmp].Value-tmpPrevious;

    if (tmpRange=0) then // or (not Interpolated) then
       result:=Palette[tmp].Color
    else
    begin
      // interpolate color

      Percent:=(AValue-tmpPrevious)/tmpRange;

      IEnd:=TAlphaColorRec(Palette[tmp].Color);
      IStart:=TAlphaColorRec(Palette[tmp-1].Color);

      {$IFNDEF FPC}
      IRange.A:=IEnd.A-IStart.A;
      {$ENDIF}

      IRange.R:=IEnd.R-IStart.R;
      IRange.G:=IEnd.G-IStart.G;
      IRange.B:=IEnd.B-IStart.B;

      {$IFDEF FPC}
      TAlphaColorRec(result).A:=0;
      {$ELSE}
      TAlphaColorRec(result).A:=IStart.A+Round(Percent*IRange.A);
      {$ENDIF}

      TAlphaColorRec(result).R:=IStart.R+Round(Percent*IRange.R);
      TAlphaColorRec(result).G:=IStart.G+Round(Percent*IRange.G);
      TAlphaColorRec(result).B:=IStart.B+Round(Percent*IRange.B);
    end;
  end;
end;

// Returns the RGB parts of AColor as an Integer. Removes the Alpha part.
class function TColorFunction.ToRGB(const AColor: TAlphaColor): Integer;
begin
  TColorRec(result).A:=0;
  TColorRec(result).R:=TAlphaColorRec(AColor).R;
  TColorRec(result).G:=TAlphaColorRec(AColor).G;
  TColorRec(result).B:=TAlphaColorRec(AColor).B;
end;

end.
