unit BI.Dashboard.HTML;

interface

uses
  System.Classes, System.Generics.Collections, BI.Dashboard;

type
  TDiv=record
  public
    Panel : TBIPanel;
    ID : String;
    Title : String;
    HTML : String;
    Position : String;
  end;

  TDivs=class(TList<TDiv>)
  public
    function IndexOfPosition(const APosition:String):Integer;
  end;

  THTMLRender=class(TRender)
  private
  const
    ItemSeparator='%----%';

  var
    Charts : TStringList;
    Divs : TDivs;
    FVariables : TVariables;

    IDivCount : Integer;
    IAnyList : Boolean;

    procedure Add(const S:String); inline;
    procedure AddAllChartsFunctions;
    procedure AddCSS;
    procedure AddListChange;
    procedure AddPartialHead;
    procedure AddTeeScript(const AScript:String);
    procedure EmitDivs;
    function FreeChartName:String;
    procedure PrepareVariables(const AParams:TStrings);
  protected
    procedure AddItemSeparator(const AIndex:Integer=-1); override;
    procedure AddListener(const AName:String; const ADataIndex:Integer); override;
    function Variables:TVariables; override;
  public
    const
      PureCSS='<link rel="stylesheet" href="http://yui.yahooapis.com/pure/0.6.0/pure-min.css">';

    var
      HTML : TStrings;

    Constructor Create; override;
    Destructor Destroy; override;

    procedure Emit(const AItem:TDashboardItem; const AKind:String; const APosition:Integer); override;
    procedure Init(const ADashboard:TDashboard; const ALayout:String='';
                   const AParams:TStrings=nil); override;
    procedure Finish; override;
  end;

implementation
