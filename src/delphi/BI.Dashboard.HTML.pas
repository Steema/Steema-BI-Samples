{*********************************************}
{  TeeBI Software Library                     }
{  HTML Web Dashboard                         }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Dashboard.HTML;

interface

uses
  System.Classes,

  {$IFNDEF FPC}
  System.Generics.Collections,
  {$ENDIF}

  BI.Dashboard;

type
  TDiv=record
  public
    Item : TDashboardItem;
    ID : Integer;
    Style : String;
    HTML : String;
    Position : String;
  end;

  TDivs=class(TList<TDiv>)
  public
    function IndexOfPosition(const APosition:String):Integer;
  end;

  THTMLRender=class;

  THTMLChartProc=function(const ARender:THTMLRender; const AItem:TDashboardItem; const AFormat:String):String;

  THTMLRender=class(TRender)
  private
  const
    ItemSeparator='%----%';

  var
    Divs : TDivs;
    FHTML : TStringList;
    FMinify : Boolean;
    FTitle : String;

    IBody : String;
    IRadioCount,
    IDivCount : Integer;
    IAnyList : Boolean;
    IUpdate : Boolean;

    {$IFDEF AUTOREFCOUNT}
    [Weak]
    {$ENDIF}
    IDashboard : TDashboard;

    procedure Add(const S:String); inline;
    procedure AddAllChartsFunctions;
    procedure AddButtonsCSS;
    procedure AddCSS;
    procedure AddListChange;
    procedure AddPartialHead;
    procedure AddTeeScript(const AScript:String);
    procedure AddUpdateScript;
    procedure EmitDivs;

    function HTMLGrid(const AItem:TDashboardItem):String;

    procedure PrepareVariables(const AParams:TStrings);
  protected
  const
    ScriptBegin='<script type="text/javascript">';
    ScriptEnd='</script>';
    CSSBegin='<style type="text/css">';
    CSSEnd='</style>';

    procedure AddItemSeparator(const AIndex:Integer=-1; const AKind:TPanelKind=TPanelKind.Automatic); override;
    procedure AddListener(const AName:String; const ASource:TObject); override;
    function FreeChartName:String;
  public
    class var
      HTMLChart : THTMLChartProc;

    var
      CSS: String;
      SteemaSources:String;
      Charts : TStringList;

      IAnyGauge : Boolean;
      IAnyMap   : Boolean;

    Constructor Create;
    Destructor Destroy; override;

    class function ClassOf(const AItem:TDashboardItem):String; static;
    procedure Clear; override;

    procedure Emit(const ADashboard:TDashboard; const AItem:Integer; const APosition:String); override;
    procedure Init(const ADashboard:TDashboard; const ALayout:String=''; const AParams:TStrings=nil); override;

    procedure Finish; override;

    property HTML:TStringList read FHTML;
    property MinifyScripts:Boolean read FMinify write FMinify default True;
    property Title:String read FTitle write FTitle;
  end;

implementation
