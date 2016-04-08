{*********************************************}
{  TeeBI Software Library                     }
{  HTML data import and export                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Html;

interface

uses
  System.Classes, System.Types,
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  Data.DB, BI.Data, BI.DataSource, BI.Arrays, BI.UI;

type
  TBIHTML=class(TBITextSource)
  public
    class function ExportFormat:TBIExport; override;

    function Import(const Strings:TStrings):TDataArray; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIHTMLExport=class(TBIExport)
  private
    AddedRows : TInteger;
    IItems : TStrings;

    procedure AddRow(const AIndex:TInteger);
    function AddRows(const AItems: TStrings):TInteger;
    function CellAlign(const AData:TDataItem; const AddLeft:Boolean):String;
    function Headers(const AddSortIcons:Boolean):String;
    function MaxDepth(const ADatas:TDataArray):Integer;
    function Row(const AIndex:TInteger; const ADatas:TDataArray):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    const
      CRLF=#13#10;

    var
      Borders : Boolean;
      Colorizers : TDataColorizers;
      FloatFormat : String;
      SortIcons : Boolean;

    Constructor Create; override;
  end;

  TBIHtmlHelper=class
  public
  const
    Return='</br>';
  class var
    TableClass:String;

    class function Color(const AColor:TColor):String; static;
    class function Combo(const AName:String; const Texts,Values:TStringDynArray): String; static;
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
