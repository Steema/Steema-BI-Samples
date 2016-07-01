unit BI.Dashboard.Layouts;

{$SCOPEDENUMS ON}

interface

uses
  System.Classes,
  BI.Data;

type
  TUnits=(Pixels,Percent);

  TBICoordinate=class(TPersistent)
  private
    FValue: Single;
    FUnits: TUnits;

    procedure SetUnits(const Value: TUnits);
    procedure SetValue(const AValue: Single);
  public
    procedure Assign(Source:TPersistent); override;

    procedure FromString(const S:String);
  published
    property Units:TUnits read FUnits write SetUnits default TUnits.Pixels;
    property Value:Single read FValue write SetValue;
  end;

  TLayouts=class;

  TLayoutStyle=(Automatic,Panels,Tabs,List,Combo);

  TLayoutItem=class(TCollectionItem)
  private
    FAlign : String;
    FHeight : TBICoordinate;
    FItems : TLayouts; // recursive
    FName : String;
    FNested : String;
    FStyle : TLayoutStyle;
    FWidth : TBICoordinate;

    function Get(const AIndex: Integer): TLayoutItem;
    procedure Put(const AIndex: Integer; const Value: TLayoutItem);

    procedure SetItems(const Value:TLayouts);
    procedure SetName(const Value:String);
  public
    Constructor Create(ACollection:TCollection); override;
    Destructor Destroy; override;

    function Find(const AName:String):TLayoutItem; // Recursive
    function IsHorizontal:Boolean;
    function IsPanels:Boolean;

    property Item[const AIndex:Integer]:TLayoutItem read Get write Put; default;
  published
    property Align:String read FAlign write FAlign;
    property Height:TBICoordinate read FHeight write FHeight;
    property Items:TLayouts read FItems write SetItems;
    property Name:String read FName write SetName;
    property Nested:String read FNested write FNested;
    property Style:TLayoutStyle read FStyle write FStyle default TLayoutStyle.Automatic;
    property Width:TBICoordinate read FWidth write FWidth;
  end;

  TLayouts=class(TOwnedCollection)
  private
    function Get(const AIndex: Integer): TLayoutItem;
    procedure Put(const AIndex: Integer; const Value: TLayoutItem);
  public
    class var
       Predefined : TLayouts;

    Destructor Destroy; override;

    procedure AddTo(const AItems: TStrings);
    function Best(const ACount:Integer):String;
    function Find(const AName:String):TLayoutItem;
    function IndexOf(const AItem:TLayoutItem):Integer;

    property Item[const AIndex:Integer]:TLayoutItem read Get write Put; default;
  end;

implementation
