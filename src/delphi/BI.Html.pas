{*********************************************}
{  TeeBI Software Library                     }
{  HTML data import and export                }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Html;

(*
  Classes to import / export TDataItem from / to HTML <table>

   TBIHtml.Import : From HTML one or more <table> content into a TDataItem

   TBIHtmlExport.AsString : From TDataItem into HTML <table>

  TBIHtmlHelper : Methods to generate HTML content easier, for example:

     TBIHtmlHelper.Link('SteeBI', 'http://www.steebi.com')

     returns:  '<a href="http://www.steebi.com">SteeBI</a>'

*)

interface

uses
  System.Classes, System.Types,
  {$IFDEF FPC}
  BI.FPC, Graphics,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  Data.DB, BI.DataItem, BI.DataSource, BI.Arrays, BI.Arrays.Strings, BI.UI,
  BI.XMLData;

type
  TBIHTML=class(TBITextSource)
  private
    function GetTable(const Xml:TXmlEngine):TDataItem;
    class function GetTables(const Xml:TXmlEngine):TDataArray; static;
    class function TryFindID(const Xml:TXmlEngine):String; static;
  public
    class function FileFilter: TFileFilters; override;
    function Import(const Strings:TStrings):TDataArray; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIHTMLExport=class(TBITextExport)
  private
    AddedRows : TInteger;
    IItems : TStrings;

    procedure AddRow(const AIndex:TInteger);
    function AddRows(const AItems: TStrings):TInteger;
    function CellAlign(const AData:TDataItem; const AddLeft:Boolean):String;
    function CellValue(const AItem:TDataItem; const AIndex:TInteger):String;
    function Headers(const AddSortIcons:Boolean):String;
    function MaxDepth(const AData:TDataArray):Integer;
    function Row(const AIndex:TInteger; const AData:TDataArray):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    var
      Borders : Boolean;
      Colorizers : TDataColorizers;
      SortIcons : Boolean;

    Constructor Create; override;

    class function FileFilter: TFileFilters; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIHtmlHelper=class
  public
  const
    Return='</br>';
  class var
    TableClass:String;

    class function Color(const AColor:TColor):String; static;
    class function Combo(const AName:String; const Texts,Values:TStringArray): String; static;
    class function Escape(const S:String):String; static;
    class function FinishForm(const AName:String):String; static;
    class function Hidden(const AName:String; const AValue:Integer):String; overload; static;
    class function Hidden(const AName,AValue:String):String; overload; static;
    class function InitForm:String; static;
    class function Link(const Text,URL:String):String; static;
    class function Navigator(const ACursor:TDataCursor; const URL,Button,ButtonDisabled:String):String; static;
    class function ToTable(const ADataSet:TDataSet):String; static;
  end;

implementation
