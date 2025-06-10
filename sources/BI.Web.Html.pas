{*********************************************}
{  TeeBI Software Library                     }
{  HTML Helper methods                        }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Html;

interface

uses
  System.UITypes;

type
  THTMLHelper=class
  public
    const
      PureCSS='<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css" integrity="sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls" crossorigin="anonymous">';

    class function AlphaColorToColor(const AColor:TAlphaColor):TColor; static;
  end;

implementation

class function THTMLHelper.AlphaColorToColor(const AColor:TAlphaColor):TColor;
begin
  // Swap R <--> B
  TColorRec(Result).R := TAlphaColorRec(AColor).B;
  TColorRec(Result).G := TAlphaColorRec(AColor).G;
  TColorRec(Result).B := TAlphaColorRec(AColor).R;

  // Alpha 0
  TColorRec(Result).A := 0;
end;

end.
