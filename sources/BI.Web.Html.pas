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
      PureCSS='<link rel="stylesheet" href="http://yui.yahooapis.com/pure/0.6.0/pure-min.css">';

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
