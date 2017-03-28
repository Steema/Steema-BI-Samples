{*********************************************}
{  TeeBI Software Library                     }
{  Base TDashboard and TBITemplate classes    }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Dashboard;

interface

uses
  System.Classes,

  {$IFDEF FPC}
  BI.FPC, FGL,
  {$ELSE}
  System.UITypes,
  System.Generics.Collections,
  {$ENDIF}

  BI.Data, BI.Arrays, BI.UI, BI.Expression, BI.DataSource,
  BI.Data.CollectionItem, BI.Dashboard.Layouts, BI.Query;

type
  TChangeListener=class;

  TChangeEvent=procedure(const Sender:TChangeListener; const AValue:TExpression) of object;

  TChangeListener=class
  private
    FName : String;
    FOnChange : TChangeEvent;
    FSource: TObject;
  public
    procedure Changed(const AValue:TExpression);

    property Name:String read FName;
    property Source:TObject read FSource;
  end;

  TListeners={$IFDEF FPC}class(TFPGList<TChangeListener>){$ELSE}class(TList<TChangeListener>){$ENDIF}
  private
    procedure Add(const AName:String; const ASource:TObject; const AOnChange:TChangeEvent);
  public
    Destructor Destroy; override;

    procedure Changed(const AName:String; const AValue:TExpression);
  end;

  TVariable=class
  private
    FValue : TExpression;

    procedure Clear;
    procedure SetValue(const AValue: TExpression);
  public
    Name : String;
    Listeners : TListeners;

    property Value:TExpression read FValue write SetValue;
  end;

  TVariableArray=Array of TVariable;

  TVariables={$IFDEF FPC}class(TFPGList<TVariable>){$ELSE}class(TObjectList<TVariable>){$ENDIF}
  protected
    IMain : TDataItem;

    {$IFNDEF FPC}
    procedure Notify(const Item: TVariable; Action: TCollectionNotification); override;
    {$ENDIF}
  public
    procedure Add(const AName:String; const AValue:TExpression);
    procedure AddListener(const AName:String; const ASource:TObject; const AOnChange:TChangeEvent);
    procedure Change(const AName:String; const AValue:Boolean); overload;
    procedure Change(const AName:String; const AValue:TExpression); overload;
    function IndexOf(const AName:String):Integer;
    procedure TryAdd(const AName:String);
  end;

  TVisualDataKind=(Data,Query,SingleRecord,Variable,FileURL,Custom);

  TVisualData=class(TDataCollectionItem)
  private
    FExpression : TExpression;
    FName : String;
    IUsedVariables : TTextArray;

    procedure AddVariable(const AName:String);
    function AsExpression:TExpression;
    procedure Change(const Value:TExpression);
    procedure Clear;
    procedure SetName(const Value:String);
    function Variables:TVariables;
  protected
    property Expression:TExpression read FExpression write FExpression;
  published
    property Name:String read FName write SetName;
  end;

  TRender=class;

  TVisualDataItems=class(TOwnedCollection)
  private
    procedure AddVariables(const ARender:TRender);

    function Get(const Index: Integer): TVisualData;
    procedure Put(const Index: Integer; const Value: TVisualData);
    function UniqueName:String;
  public
    function Add(const AData:TDataItem):TVisualData;
    procedure Change(const AName:String; const AValue:TExpression);
    function Find(const AName:String):TVisualData;
    function IndexOf(const AName:String):Integer; overload;
    function ValueOf(const AName:String):TExpression;

    property Item[const Index:Integer]:TVisualData read Get write Put; default;
  end;

  TBITemplate=class;

  TBaseItem=class(TCollectionItem)
  private
    FName : String;
    IUsedVariables : TTextArray;

    procedure AddUsedVariable(const AName:String);
    procedure DoChanged;
    function IsLoading:Boolean;
    procedure SetName(const Value:String);
  protected
    function ReplaceString(const S:String):String;
    function Template: TBITemplate;
    function Variables:TVariables;
  public
    function UsesVariable(const Sender:TChangeListener):Boolean; virtual;
  published
    property Name:String read FName write SetName;
  end;

  TBIFont=class(TPersistent)
  private
    FBold : Boolean;
    FColor : TAlphaColor;
    FFamily : String;
    FItalic : Boolean;
    FSize : Single;

    procedure SetColor(const Value: TAlphaColor);
  published
    property Bold:Boolean read FBold write FBold default False;
    property Color:TAlphaColor read FColor write SetColor default TAlphaColors.Null;
    property Family:String read FFamily write FFamily;
    property Italic:Boolean read FItalic write FItalic default False;
    property Size:Single read FSize write FSize;
  end;

  TPanelKind=(Automatic,Grid,List,Text,CheckList,Combo,Buttons,Image,Slider,
              Check,Navigator,Tree,Radio,Chart);

  TPanelKindHelper=record helper for TPanelKind
  public
    class function From(const S:String):TPanelKind; static;
    function ToString:String;
  end;

  TCustomBIPanel=class(TBaseItem)
  private
    FBack : TAlphaColor;
    FDisplay : String;
    FFont : TBIFont;
    FKey : String;
    FKind : TPanelKind;
    FSource,
    FTarget,
    FTitle,
    FURL  : String;

    FHeight,
    FWidth : TBICoordinate;

    IDataLoad : String;

    IData : TVisualData;
    ISelect : TDataItem;

    function GetData:String;
    procedure LinkData(const AData: String);
    procedure SetData(const Value: String);
    procedure SetVisualData(const Value: TVisualData);
    procedure SetBack(const Value: TAlphaColor);

    procedure SetFont(const Value: TBIFont);
    procedure SetHeight(const Value: TBICoordinate);
    procedure SetKind(const Value: TPanelKind);
    procedure SetTarget(const Value: String);
    procedure SetWidth(const Value: TBICoordinate);
  public
    Constructor Create(Collection:TCollection); override;
    Destructor Destroy; override;

    function UsesVariable(const Sender:TChangeListener):Boolean; override;
    property VisualData:TVisualData read IData write SetVisualData;

  { To be published }
    property Back:TAlphaColor read FBack write SetBack default TAlphaColors.Null;
    property Data:String read GetData write SetData;
    property Display:String read FDisplay write FDisplay;
    property Font:TBIFont read FFont write SetFont;
    property Height:TBICoordinate read FHeight write SetHeight;
    property Key:String read FKey write FKey;
    property Kind:TPanelKind read FKind write SetKind default TPanelKind.Automatic;
    property Source:String read FSource write FSource;
    property Target:String read FTarget write SetTarget;
    property Title:String read FTitle write FTitle;
    property URL:String read FURL write FURL;
    property Width:TBICoordinate read FWidth write SetWidth;
  end;

  TBIPanel=class(TCustomBIPanel)
  published
    property Back;
    property Data;
    property Font;
    property Height;
    property Kind;
    property Source;
    property Target;
    property URL;
    property Width;
  end;

  TDashboard=class;

  TDashboardItem=class(TCustomBIPanel)
  private
    FCSSClass,
    FSelected,
    FText : String;

    FColorize : Boolean;
    FHorizontal : Boolean;
    FNavigator : Boolean;

    FMin,
    FMax,
    FPadding : Single;

    FPosition : String;
    FPanel : TCustomBIPanel;

    FVisible: Boolean;

    // DashboardTree
    FMaster,
    FDetail : TDataItem;

    IPanelLoad,
    IPosition : String;

    procedure ClearVariables;
    function DoGetSelected(const AData:TDataItem; const AIndex:Integer): String;
    function GetDashboard: TDashboard;
    function GetDisplayData(const AData:TDataItem):TDataItem;
    function GetKeyData(const AData:TDataItem):TDataItem;
    function GetPanel:String;
    function GetText:String;
    procedure LinkPanel(const APanel: String);
    procedure Loaded;
    procedure SetPanel(const APanel:String);
    procedure SetVisible(const Value: Boolean);
  protected
    Format : String;
    Minify : Boolean; // HTML JS
  public
    Constructor Create(Collection:TCollection); override;

    function CalcDisplay:String;
    function CalcKind:TPanelKind;
    function CountOfData:TInteger;
    function GetSelected(const AIndex:Integer):String;
    function GetSelectedDisplay(const AIndex:Integer): String;
    function SelectedIndex:Integer;
    function UsesVariable(const Sender:TChangeListener):Boolean; override;

    property Dashboard:TDashboard read GetDashboard;
    property Detail:TDataItem read FDetail;
    property Master:TDataItem read FMaster;
    property PanelItem:TCustomBIPanel read FPanel write FPanel;
    property RealPosition:String read IPosition;
  published
    property CSSClass:String read FCSSClass write FCSSClass;
    property Colorize:Boolean read FColorize write FColorize default False;
    property Height;
    property Horizontal:Boolean read FHorizontal write FHorizontal default False;
    property Kind;
    property Max:Single read FMax write FMax;
    property Min:Single read FMin write FMin;
    property Navigator:Boolean read FNavigator write FNavigator default False;
    property Padding:Single read FPadding write FPadding;
    property Panel:String read GetPanel write SetPanel;
    property Position:String read FPosition write FPosition;
    property Selected:String read FSelected write FSelected;
    property Target;
    property Text:String read GetText write FText;
    property Title;
    property URL;
    property Visible:Boolean read FVisible write SetVisible default True;
    property Width;
  end;

  TDashboardItems=class(TOwnedCollection)
  private
    procedure ClearPositions;
    function Get(const Index: Integer): TDashboardItem;
    procedure Loaded;
    procedure Put(const Index: Integer; const Value: TDashboardItem);
    function UniqueName:String;
    function VisibleCount:Integer;
  public
    function Add(const APanel:TBIPanel):TDashboardItem; overload;
    function IndexOf(const AName:String):Integer;

    property Item[const Index:Integer]:TDashboardItem read Get write Put; default;
  end;

  TDashboard=class(TCustomBIPanel)
  private
    FItems : TDashboardItems;
    FLayout : String;
    FPadding : Single;
    FSplitters : Boolean;
    FTitles : Boolean;

    function Get(const AIndex: Integer): TDashboardItem;
    function IsFreePosition(const AName:String):Boolean;
    procedure Loaded;
    procedure Put(const AIndex: Integer; const Value: TDashboardItem);
    procedure SetLayout(const Value: String);
    procedure SetItems(const Value: TDashboardItems);
  protected
    FLayoutItem : TLayoutItem;
  public
    Constructor Create(ACollection:TCollection); override;
    Destructor Destroy; override;

    function Find(const APanel:TBIPanel):TDashboardItem;
    function IndexOfPanel(const AName:String):Integer;

    property Item[const AIndex:Integer]:TDashboardItem read Get write Put; default;
    property LayoutItem:TLayoutItem read FLayoutItem;
  published
    property Back;
    property Height;
    property Items:TDashboardItems read FItems write SetItems;
    property Layout:String read FLayout write SetLayout;
    property Padding:Single read FPadding write FPadding;
    property Splitters:Boolean read FSplitters write FSplitters default True;
    property Titles:Boolean read FTitles write FTitles default True;
    property Width;
  end;

  TRender=class
  private
    procedure PrepareVariables(const ADashboard:TDashboard);
  protected
    FVisual : TBITemplate;
    FVariables : TVariables;

    ILayout : TLayoutItem;
    ILayoutName : String;

    IParent : TRender;

    class procedure AddItems(const AItems: TStrings; const AData:TDataItem;
                const ADisplay:String=''); static;
    procedure AddItemSeparator(const AIndex:Integer=-1; const AKind:TPanelKind=TPanelKind.Automatic); virtual;
    procedure AddListener(const AName:String; const ASource:TObject); virtual; abstract;
    procedure ChangeSelected(const AItem:TDashboardItem; const AIndex:Integer);
    class function DataAsString(const AData:TDataItem):String; static;
    function FindRootLayout(const AName:String):TLayoutItem;
    function GetColorizers(const AItem:TDashboardItem):TDataColorizers;
    function TopRender:TRender;
  public
    procedure Clear; virtual; abstract;
    procedure Emit(const ADashboard:TDashboard; const AItem:Integer; const APosition:String); overload; virtual;

    class function GetDisplayData(const AData:TDataItem; const AName:String=''):TDataItem; static;
    procedure Init(const ADashboard:TDashboard; const ALayout:String='';
                   const AParams:TStrings=nil); virtual;
    procedure Finish; virtual;
  end;

  TRenderClass=class of TRender;

  TPanels=class(TOwnedCollection)
  private
    function Get(const Index: Integer): TBIPanel;
    procedure Loaded;
    procedure Put(const Index: Integer; const Value: TBIPanel);
    function UniqueName:String;
  public
    function Add(const AData:TVisualData):TBIPanel;
    function Find(const AName:String):TBIPanel;
    function IndexOf(const AName:String):Integer;

    property Item[const Index:Integer]:TBIPanel read Get write Put; default;
  end;

  TDashboards=class(TOwnedCollection)
  private
    function Get(const Index: Integer): TDashboard;
    procedure Loaded;
    procedure Put(const Index: Integer; const Value: TDashboard);
    function UniqueName:String;
  public
    function Add:TDashboard;
    function Find(const AName:String):TDashboard;
    function IndexOf(const AName:String):Integer;

    property Item[const Index:Integer]:TDashboard read Get write Put; default;
  end;

  TBITemplate=class(TComponent)
  private
    FDashboards : TDashboards;
    FData : TVisualDataItems;
    FLayouts : TLayouts;
    FPanels : TPanels;

    IList : TDataItem;
    IMainData : TDataItem;
    IRender : TRender;

    procedure DataResolver(const AName:String; out AData:TDataItem);
    function ProviderFromSQL(const ADest:TVisualData; const SQL: String): TDataProvider;
    procedure SetDashboards(const Value: TDashboards);
    procedure SetData(const Value: TVisualDataItems);
    procedure SetLayouts(const Value: TLayouts);
    procedure SetPanels(const Value: TPanels);
  protected
    function FindData(const AStore,AName:String):TDataItem;
    function GetItem(const AName:String):TDataItem;
    property List:TDataItem read IList write IList;
    procedure Loaded; override;
    function NewOwner:TComponent;
    function PanelData(const APanel:TBIPanel):TDataItem;
  public
    class
      var Root : String;

    Constructor Create(Owner:TComponent); override;
    Destructor Destroy; override;

    procedure Clear;
    function FindLayout(const ALayout:String):TLayoutItem;

    procedure Generate(const ARender:TRender; const ADashboard:TDashboard;
                       const ALayout:String='';
                       const AParams:String='');

    class function GetMasterDetail(const AData:TDataItem; const AIndex:TInteger):TCursorIndex;
    function NewQuery(const ADest:TVisualData; const SQL:String):TBIQuery;

    property MainData:TDataItem read IMainData write IMainData;
  published
    property Dashboards:TDashboards read FDashboards write SetDashboards;
    property Data:TVisualDataItems read FData write SetData;
    property Layouts:TLayouts read FLayouts write SetLayouts;
    property Panels:TPanels read FPanels write SetPanels;
  end;

implementation
