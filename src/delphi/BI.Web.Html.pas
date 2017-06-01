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
