{*********************************************}
{  TeeBI Software Library                     }
{  Base TDashboard and TBITemplate classes    }
{  Copyright (c) 2015-2016 by Steema Software }
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

  BI.Data, BI.Arrays, BI.UI, BI.Expression, BI.DataSource;

type
  TChangeListener=class;

  TChangeEvent=procedure(const Sender:TChangeListener; const AValue:TExpression) of object;

  TChangeListener=class
  private
    FIndex : Integer;
    FName : String;
    FOnChange : TChangeEvent;
  public
    procedure Changed(const AValue:TExpression);

    property Index:Integer read FIndex;
    property Name:String read FName;
  end;

  TListeners={$IFDEF FPC}class(TFPGList<TChangeListener>){$ELSE}class(TList<TChangeListener>){$ENDIF}
  private
    procedure Add(const AName:String; const AIndex:Integer; const AOnChange:TChangeEvent);
  public
    Destructor Destroy; override;

    procedure Changed(const AName:String; const AValue:TExpression);
  end;

  TVariable=record
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

  TVariables={$IFDEF FPC}class(TFPGList<TVariable>){$ELSE}class(TList<TVariable>){$ENDIF}
  protected
    IMain : TDataItem;

    {$IFNDEF FPC}
    procedure Notify(const Item: TVariable; Action: TCollectionNotification); override;
    {$ENDIF}
  public
    procedure Add(const AName:String; const AValue:TExpression);
    procedure AddListener(const AName:String; const AIndex:Integer; const AOnChange:TChangeEvent);
    procedure Change(const AName:String; const AValue:Boolean); overload;
    procedure Change(const AName:String; const AValue:TExpression); overload;
    function IndexOf(const AName:String):Integer;
    procedure TryAdd(const AName:String);
  end;

  TBITemplate=class;

  TBaseItem=class(TComponent)
  private
    Index : Integer;

    IUsedVariables : TTextArray;
    IVisual : TBITemplate;

    IOwnsList : Boolean;

    procedure AddUsedVariable(const AName:String);
    function GetItem(const AName: String): String; inline;
    procedure SetItem(const AName,AValue: String);
    function Variables:TVariables;
  protected
    IList : TDataItem;

    function GetBoolean(const AName:String; const ADefault:Boolean):Boolean; overload; inline;
    function GetBoolean(const AList:TDataItem; const AName:String; const ADefault:Boolean):Boolean; overload;
    function GetColor(const AName:String):TAlphaColor; overload; inline;
    function GetColor(const AList:TDataItem; const AName:String):TAlphaColor; overload;
    function GetString(const AList:TDataItem; const AName:String; const AIndex:Integer):String; overload;
    function GetString(const AList:TDataItem; const AName:String):String; overload; inline;

    function ReplaceString(const S:String):String;

//    function TryReplaceVariable(const S:String):String;
  public
    Name : String;

    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem=nil; const AIndex:Integer=0); virtual;
    Destructor Destroy; override;

    function FindItem(const AName:String):TDataItem;
    function FindSubItem(const AName,ASub:String; out AValue:String):Boolean;

    function UsesVariable(const Sender:TChangeListener):Boolean; virtual;

    property Item[const AName:String]:String read GetItem write SetItem; default;
  end;

  TBIFont=class(TBaseItem)
  public
    Color : TAlphaColor;

    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); override;
  end;

  TBIPanel=class(TBaseItem)
  private
    FData : TDataItem;
    FHeight,
    FWidth : String;

    ISelect : TDataItem;

    function GetData: TDataItem;
    procedure SetData(const Value: TDataItem);
    function TrySelect(const AData:TDataItem):TDataItem;
  protected
    IData : Integer;
  public
    Back : TAlphaColor;
    Kind : String;
    Target : String;
    Font : TBIFont;

    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem=nil; const AIndex:Integer=0); override;
    Destructor Destroy; override;

    function UsesVariable(const Sender:TChangeListener):Boolean; override;
  published
    property Data:TDataItem read GetData write SetData;
    property Height : String read FHeight write FHeight;
    property Width : String read FWidth write FWidth;
  end;

  TDashboardItem=class(TBaseItem)
  private
    FHeight,
    FPosition,
    FWidth : String;

    FPanel : TBIPanel;

    IPosition : String;

    function DoGetSelected(const AData:TDataItem; const AIndex:Integer): String;
    function GetDisplayData(const AData:TDataItem):TDataItem;
    function GetKeyData(const AData:TDataItem):TDataItem;
    procedure SetPanel(const APanel:TBIPanel);
  protected
    Dashboard : TBIPanel;

    function TryGetSub(const AName,ASubName:String; out AValue:String):Boolean;
  public
    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem=nil; const AIndex:Integer=0); override;

    function CalcKind:String;
    function CountOfData:TInteger;
    function GetData(const AName:String):String;
    function GetSelected(const AIndex:Integer):String;
    function GetSelectedDisplay(const AIndex:Integer): String;
    function SelectedIndex:Integer;
    function Text:String;
    function UsesVariable(const Sender:TChangeListener):Boolean; override;
  published
    property Height : String read FHeight write FHeight;
    property Panel : TBIPanel read FPanel write SetPanel;
    property Position:String read FPosition write FPosition;
    property Width : String read FWidth write FWidth;
  end;

  TDashboardItems={$IFDEF FPC}class(TFPGList<TDashboardItem>){$ELSE}class(TList<TDashboardItem>){$ENDIF}
  public
    function Add(const APanel:TBIPanel):TDashboardItem; overload;
  end;

  TLayouts=class;

  TLayoutItem=class(TBaseItem)
  private
  public
    Align : String;
    Height : String;
    Items : TLayouts;
    Nested : String;
    Style : String;
    Width : String;

    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem=nil; const AIndex:Integer=0); override;
    Destructor Destroy; override;

    function Find(const AName:String):TLayoutItem; // Recursive
    function IsHorizontal:Boolean;
  end;

  TLayouts={$IFDEF FPC}class(TFPGList<TLayoutItem>){$ELSE}class(TList<TLayoutItem>){$ENDIF}
  private
    class var
       FPredefined : TLayouts;

    procedure ImportFrom(const AVisual:TBITemplate; const AData:TDataItem);
  public
    Destructor Destroy; override;

    procedure AddTo(const AItems: TStrings);
    function Find(const AName:String):TLayoutItem;

    class function Predefined:TLayouts; static;
  end;

  TDashboard=class(TBIPanel)
  private
    FItems : TDashboardItems;
    FLayout : String;
    FSplitters : Boolean;
    FTitles : Boolean;

    function GetItems:TDashboardItems;
    function IsFreePosition(const AName:String):Boolean;
  public
    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem=nil; const AIndex:Integer=0); override;
    Destructor Destroy; override;

    function Find(const APanel:TBIPanel):TDashboardItem;
    function IndexOfPanel(const AName:String):Integer;

    property Items:TDashboardItems read GetItems;
  published
    property Layout : String read FLayout write FLayout;
    property Splitters : Boolean read FSplitters write FSplitters default True;
    property Titles : Boolean read FTitles write FTitles default False;
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
    procedure AddItemSeparator(const AIndex:Integer=-1; const AKind:String=''); virtual;
    procedure AddListener(const AName:String; const ADataIndex:Integer); virtual; abstract;
    function BestLayout(const ADashboard:TDashboard):String;
    procedure ChangeSelected(const AItem:TDashboardItem; const AIndex:Integer);
    class function DataAsString(const AData:TDataItem):String; static;
    function FindRootLayout(const AName:String):TLayoutItem;
    function GetColorizers(const AItem:TDashboardItem):TDataColorizers;
    function TopRender:TRender;
  public
    procedure Emit(const ADashboard:TDashboard; const AItem:Integer; const APosition:String); overload; virtual;

    class function GetDisplayData(const AData:TDataItem; const AName:String=''):TDataItem; static;
    procedure Init(const ADashboard:TDashboard; const ALayout:String='';
                   const AParams:TStrings=nil); virtual;
    procedure Finish; virtual;
  end;

  TRenderClass=class of TRender;

  TPanels={$IFDEF FPC}class(TFPGList<TBIPanel>){$ELSE}class(TList<TBIPanel>){$ENDIF}
  public
    function IndexOf(const AName:String):Integer;
  end;

  TDashboards={$IFDEF FPC}class(TFPGList<TDashboard>){$ELSE}class(TList<TDashboard>){$ENDIF}
  public
    function IndexOf(const AName:String):Integer;
  end;

  TVisualDataKind=(Data,Query,SingleRecord,Variable,FileURL,Custom);

  TVisualData=class(TBaseItem)
  private
    FKind : TVisualDataKind;
    FStore : String;

    IDataOrigin: String;
    IData : TDataItem;
    IExpression : TExpression;
    IMain : TDataItem;
    IValue : TExpression;

    DataIndex : Integer;

    procedure Change(const AValue:TExpression);
    function ExpressionValue:TExpression;
    function SingleRecordFrom(const AData: TDataItem):TDataItem;
  public
    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); override;
    Destructor Destroy; override;

    procedure Clear;
    function Data:TDataItem;
    procedure Reset;
    function Value:TExpression;
  published
    property Kind : TVisualDataKind read FKind write FKind;
    property Store : String read FStore write FStore;
  end;

  TVisualDataArray=Array of TVisualData;

  TVisualDataArrayHelper=record helper for TVisualDataArray
  private
    procedure AddVariables(const ARender:TRender);
  public
    procedure Change(const AName:String; const AValue:TExpression);
    procedure Clear;
    function Count:Integer; inline;
    function IndexOf(const AName:String):Integer;
    function ValueOf(const AName:String):TExpression;
  end;

  TBITemplate=class(TBaseItem)
  private
    IRender : TRender;

    IMainData : TDataItem;

    function CreateDashboard(const AList:TDataItem;
                             const AIndex:Integer):TDashboard;
    function CreatePanel(const AList:TDataItem;
                         const AIndex:Integer):TBIPanel;

    function ParseSQL(const AMain:TDataItem; const SQL:String; const AIndex:Integer):TDataItem;
  protected
    Owner : TObject;

    function FindData(const AStore,AName:String):TDataItem;
    function GetItem(const AName:String):TDataItem;
    function PanelData(const APanel:TBIPanel):TDataItem;
  public
    Dashboards : TDashboards;
    Data : TVisualDataArray;
    Layouts : TLayouts;
    Panels : TPanels;

    class
      var Root : String;

    Constructor CreateData(const AVisual:TBITemplate; const AList:TDataItem; const AIndex:Integer=0); override;
    Destructor Destroy; override;

    function FindLayout(const ALayout:String):TLayoutItem;

    class function FromData(const AData:TDataItem):TBITemplate; static;
    class function FromJSON(const AJSON:String):TBITemplate; static;
    class function FromJSONFile(const AJSONFile:String):TBITemplate; static;

    procedure Generate(const ARender:TRender; const ADashboard:TDashboard;
                       const ALayout:String='';
                       const AParams:String='');

    class function GetMasterDetail(const AData:TDataItem; const AIndex:TInteger):TCursorIndex;

    class function ImportJSON(const AText:String):TDataItem;

    property List:TDataItem read IList; // deprecated ?
  end;

implementation
