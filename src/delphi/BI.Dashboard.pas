{*********************************************}
{  TeeBI Software Library                     }
{  Base TDashboard and TBITemplate classes    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Dashboard;

interface

uses
  System.UITypes, System.Classes, System.Generics.Collections,
  BI.Data, BI.Arrays;

type
  TBase=class
  end;

  TBase<T>=class(TBase)
  private
    class var Registered:TDictionary<String,T>;
  public
    class Constructor Create;
    class Destructor Destroy;

    class function Find(const AName:String):T; static;
    class procedure Register(const AName:String; const AClass:T); static;
  end;

  TBITemplate=class;

  TRenderClass=class of TRender;

  TChangeListener=class;

  TBaseItem=class
  private
    IList : TDataItem;
    Index : Integer;

    IUsedVariables : TTextArray;
    IVisual : TBITemplate;

    procedure AddUsedVariable(const AName:String);
    function GetItem(const AName: String): String; inline;
  protected
    function GetBoolean(const AName:String; const ADefault:Boolean):Boolean; overload; inline;
    function GetBoolean(const AList:TDataItem; const AName:String; const ADefault:Boolean):Boolean; overload;
    function GetColor(const AName:String):TAlphaColor; overload; inline;
    function GetColor(const AList:TDataItem; const AName:String):TAlphaColor; overload;
    function GetString(const AList:TDataItem; const AName:String; const AIndex:Integer=0):String;

    function TryReplaceVariable(const S:String):String;
  public
    Constructor Create(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); virtual;

    function UsesVariable(const Sender:TChangeListener):Boolean; virtual;

    property Item[const AName:String]:String read GetItem; default;
  end;

  TBIFont=class(TBaseItem)
  public
    Color : TAlphaColor;

    Constructor Create(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); override;
  end;

  TBIPanel=class(TBaseItem)
  protected
    IData : Integer;
  public
    Back : TAlphaColor;
    Name : String;
    Kind : String;
    Source : String;
    Target : String;
    Font : TBIFont;
    Format : String;

    Constructor Create(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); override;
    Destructor Destroy; override;

    function TitleOrName:String;
    function UsesVariable(const Sender:TChangeListener):Boolean; override;
  end;

  TDashboardItem=class(TBaseItem)
  private
    IPosition : Integer;
  public
    Panel : TBIPanel;
    Position : Integer;
    Selected : String;

    function CalcKind:String;
    function UsesVariable(const Sender:TChangeListener):Boolean; override;
  end;

  TDashboardItems=Array of TDashboardItem;

  TDashboardItemsHelper=record helper for TDashboardItems
  public
    procedure Add(const AItem:TDashboardItem);
    function Count:Integer; inline;
  end;

  TChangeEvent=procedure(const Sender:TChangeListener; const AValue:String) of object;

  TChangeListener=class
  private
    FIndex : Integer;
    FName : String;
    FOnChange : TChangeEvent;
  public
    procedure Changed(const AValue:String);

    property Index:Integer read FIndex;
    property Name:String read FName;
  end;

  TListeners=class(TList<TChangeListener>)
  private
    procedure Add(const AName:String; const AIndex:Integer; const AOnChange:TChangeEvent);
  public
    Destructor Destroy; override;

    procedure Changed(const AName,AValue:String);
  end;

  TVariable=record
  public
    Name : String;
    Value : String;
    Listeners : TListeners;
  end;

  TVariables=class(TList<TVariable>)
  public
    Destructor Destroy; override;

    procedure Add(const AName,AValue:String);
    procedure AddListener(const AName:String; const AIndex:Integer; const AOnChange:TChangeEvent);
    procedure Change(const AName:String; const AValue:Boolean); overload;
    procedure Change(const AName,AValue:String); overload;
    function IndexOf(const AName:String):Integer;
    procedure TryAdd(const AName:String);
  end;

  TDashboard=class(TBIPanel)
  public
    Layout : String;
    Items : TDashboardItems;

    Destructor Destroy; override;

    function IndexOfPanel(const AName:String):Integer;
  end;

  TRender=class(TBase<TRenderClass>)
  private
    class function GetFirstData(const AData:TDataItem):TDataItem; static;
    procedure PrepareVariables(const ADashboard:TDashboard);
  protected
    FVisual : TBITemplate;

    ILayout : String;
    IParent : TRender;

    class procedure AddItems(const AItems: TStrings; const AData:TDataItem); static;
    procedure AddItemSeparator(const AIndex:Integer=-1); virtual;
    procedure AddListener(const AName:String; const ADataIndex:Integer); virtual; abstract;
    function BestLayout(const ADashboard:TDashboard):String;
    procedure CountAndSelected(const AItem:TDashboardItem; out AMax,ASelected:Integer);
    function CreateVariables:TVariables;
    class function DataAsString(const AData:TDataItem):String; static;
    function NumLayout:Integer;
    function PanelData(const APanel:TBIPanel):TDataItem;
    function TopRender:TRender;
    function Variables:TVariables; virtual; abstract;
  public
    Constructor Create; virtual; abstract;

    procedure Emit(const AItem:TDashboardItem; const AKind:String; const APosition:Integer); virtual;
    procedure Init(const ADashboard:TDashboard; const ALayout:String='';
                   const AParams:TStrings=nil); virtual;
    procedure Finish; virtual;

    class procedure UnRegister(const AClass:TRenderClass); static;
  end;

  TPanels=Array of TBIPanel;

  TPanelsHelper=record helper for TPanels
  public
    procedure Add(const APanel:TBIPanel);
    function Count:Integer;
    function IndexOf(const AName:String):Integer;
  end;

  TDashboards=Array of TDashboard;

  TDashboardHelper=record helper for TDashboards
  public
    procedure Add(const ADashboard:TDashboard);
    function Count:Integer;
    function IndexOf(const AName:String):Integer;
  end;

  TVisualData=class(TBaseItem)
  private
    IDataOrigin: String;
    IData : TDataItem;

    DataIndex : Integer;

    FTemplate : TBITemplate;
  public
    Kind,
    Name,
    Store : String;

    Constructor Create(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); override;

    procedure Clear;
    function Data:TDataItem;
    procedure Reset;
  end;

  TVisualDatas=Array of TVisualData;

  TVisualDatasHelper=record helper for TVisualDatas
  private
    procedure AddVariables(const ARender:TRender);
  public
    procedure Change(const AName,AValue:String);
    procedure Clear;
    function Count:Integer; inline;
    function IndexOf(const AName:String):Integer;
  end;

  TBITemplate=class(TBaseItem)
  private
    IDataOrigin : String;
    IData : TDataItem;

    IRender : TRender;

    function CreateDashboard(const AList:TDataItem;
                             const AIndex:Integer):TDashboard;
    function CreatePanel(const AList:TDataItem;
                         const AIndex:Integer):TBIPanel;

    function GetData: TDataItem;
    function ParseSQL(const SQL:String; const AIndex:Integer):TDataItem;
  protected
    Owner : TObject;

    function GetItem(const AName:String):TDataItem;
    function GetPanelData(const APanel:TBIPanel):TDataItem;
  public
    Datas : TVisualDatas;
    Panels : TPanels;
    Dashboards : TDashboards;

    Destructor Destroy; override;

    class function FromData(const AData:TDataItem):TBITemplate; static;
    class function FromJSON(const AJSON:String):TBITemplate; static;

    procedure Generate(const ARender:TRender; const ADashboard:TDashboard;
                       const ALayout:String='';
                       const AParams:String='');
  end;

implementation
