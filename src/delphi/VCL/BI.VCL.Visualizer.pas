{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Control                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Visualizer;
{.$DEFINE FMX}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, Data.DB,
  BI.Data, BI.Arrays, BI.DataSource,

  {$IFDEF FMX}
  System.Types,
  Fmx.Types, Fmx.Controls, Fmx.StdCtrls, Fmx.ListBox, Fmx.TabControl,
  Fmx.Layouts, Fmx.Objects, Fmx.TreeView, BI.FMX.DataControl, BI.FMX.Grid
  {$ELSE}

  Vcl.Controls, VCL.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Forms,
  BI.VCL.DataControl, BI.VCL.Grid
  {$ENDIF}
  ;

type
  TGroup=class;

  TGroupClass=class of TGroup;

  TVisualizerItems=class;

  TVisualizerItem=class(TCollectionItem)
  private
    FEnabled : Boolean;
    FItems : TVisualizerItems;

    procedure ReCalculate;
    procedure SetEnabled(const Value:Boolean);
    function GetSelected(const Index: TInteger): Boolean;
    procedure SetSelected(const Index: TInteger; const Value: Boolean);
  public
    Current : TGroup;
    Data : TDataItem;
    GroupBy : TDataItem;
    GroupClass : TGroupClass;

    Constructor Create(Collection:TCollection); override;
    Destructor Destroy; override;

    function AsString(const AIndex:TInteger):String;
    function Count:TInteger;
    function IsEnabled:Boolean;
    class function MapToString(const AData:TDataItem; const AMap:TDataMap; const APos:TInteger):String; static;

    property Selected[const Index:TInteger]:Boolean read GetSelected write SetSelected;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
  end;

  TBIComposer=class;

  TVisualizerItems=class(TOwnedCollection)
  private
    IParent : TBIComposer;

    function GetItem(const Index: Integer): TVisualizerItem;
    procedure SetItem(const Index: Integer; const Value: TVisualizerItem);
  public
    type
      TEnabled=record
      private
        IOwner : TVisualizerItems;
      public
        function Count:Integer;
        function First(const Start:Integer):Integer; overload;
        function First:Integer; overload;
        function Last:Integer;
      end;

    var
      Enabled : TEnabled;

    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem):TVisualizerItem;
    procedure Exchange(const A,B:Integer);

    property Items[const Index:Integer]:TVisualizerItem read GetItem write SetItem; default;
  end;

  {$IFDEF FMX}
  TWinControl=TControl;
  TPageControl=TTabControl;
  {$ENDIF}

  TGroupProc<T>={$IFNDEF FPC}reference to{$ENDIF} procedure(const AGroup:T);

  TGroupClasses=Array of TGroupClass;

  TGroupClassesHelper=record helper for TGroupClasses
  public
    procedure Add(const Value:TGroupClass);
  end;

  TGroup=class(TComponent)
  private
  type
    TPendingItem=record
    public
      Group : TGroup;
      Next : Integer;
      Rows : TCursorIndex;
      Data : TDataItem;
      Control : TComponent;
      Position : TInteger;
    end;

  var
    FGroups : Array of TGroup;

    procedure AddGroup(const AGroup:TGroup);
  protected
    CanAddValues : Boolean;

    FItems : Array of TPendingItem;
    FVisualizer : TBIComposer;

    FixedWidth : Boolean;
    Position : TInteger;

    function AddItem(const AData:TDataItem; const ANext:Integer;
                  const ARows:TCursorIndex; const APos:TInteger):TInteger;

    function AsString(const AData:TDataItem):String; overload;
    function AsString(const AIndex:TInteger):String; overload;

    procedure Init; virtual;
    procedure Finished; virtual;

    function GetDataString:String;

    {$IFNDEF FPC}
    procedure Traverse<T:Class>(const AProc:TGroupProc<T>);
    {$ENDIF}
  public
  class var
    GroupClasses:TGroupClasses;

  var
    Parent : TGroup;
    Item : TVisualizerItem;

    Control : TWinControl;
    Data : TDataItem;

    Index : TCursorIndex;

    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); virtual;
    procedure Add(const AIndex:TInteger); virtual;

    procedure AddValues(const AComponent:TComponent;
                        const Values:TVisualizerItems;
                        const AGroup:TVisualizerItem;
                        const ARows:TCursorIndex); virtual;

    class function BestControl(const AIndex,ATotal,AValues:Integer):TGroupClass; virtual;

    class function ClassIndexOf(const AClass:TGroupClass):Integer; static;
    function MapToString:String;
  end;

  {$IFDEF FMX}
  TWinControlClass=class of TWinControl;
  {$ENDIF}

  TGroupControl=class(TGroup)
  private
    FLabel : TLabel;
    FShowLabel : Boolean;

    procedure AddCaptionPanel;
    function CreateControlClass(const AClass:TWinControlClass):TWinControl;
    procedure SetShowLabel(const Value:Boolean);
  protected
    procedure CheckControl(const AIndex: Integer);
    procedure CreateLabel(const AText:String);
    procedure DoSetShowLabel(const Value:Boolean); virtual;
    procedure ShowItem(const AIndex:Integer);
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Assign(Source:TPersistent); override;

    property GroupLabel:TLabel read FLabel;
  published
    property ShowLabel:Boolean read FShowLabel write SetShowLabel default True;
  end;

  TGroupAlign=(Left,Top,Right,Bottom);

  TGroupControlAlign=class(TGroupControl)
  private
    FAlign : TGroupAlign;

    procedure AlignControl(const AControl:TControl);
    procedure ResetWidth;
    procedure SetAlign(const Value:TGroupAlign);
  protected
    function CalcDefaultWidth:Integer; virtual;
    procedure ChangeAlign(const Value: TGroupAlign); virtual;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Align:TGroupAlign read FAlign write SetAlign default TGroupAlign.Left;
  end;

  TGroupList=class(TGroupControlAlign)
  private
    FList  : TCustomListBox;

    procedure ChangeList(const Value: Boolean);
    procedure CheckedList(Sender:TObject);
    procedure ClickedList(Sender:TObject);
    function GetCheckBoxes: Boolean;
    procedure SetCheckBoxes(const Value: Boolean);
  protected
    function CalcDefaultWidth:Integer; override;
    procedure Init; override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    procedure Assign(Source:TPersistent); override;

    property List:TCustomListBox read FList;
  published
    property CheckBoxes:Boolean read GetCheckBoxes write SetCheckBoxes default False;
  end;

  TGroupCombo=class(TGroupControlAlign)
  private
    FCombo : TComboBox;

    IComboLabel : TLabel;

    procedure ClickedCombo(Sender:TObject);
    procedure CreateCombo;
    procedure SetComboLeft;
  protected
    function CalcDefaultWidth:Integer; override;
    procedure Init; override;
    procedure Finished; override;
    procedure DoSetShowLabel(const Value:Boolean); override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    property ComboBox:TComboBox read FCombo;
  published
    property Align default TGroupAlign.Top;
  end;

  TGroupPage=class(TGroupControlAlign)
  private
    FPage : TPageControl;

    procedure ChangedTab(Sender:TObject);
  protected
    procedure ChangeAlign(const Value: TGroupAlign); override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    procedure Assign(Source:TPersistent); override;

    property PageControl:TPageControl read FPage;
  published
    {$IFDEF FMX}
    //property TabStyle...
    {$ENDIF}
  end;

  TGroupGrid=class(TGroupControlAlign)
  private
    FData : TDataItem;
    FGrid : TBIGrid;

    ISplitter : TSplitter;

    procedure DataChange(Sender: TObject; Field: TField);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure Add(const AIndex:TInteger); override;

    procedure AddValues(const AComponent: TComponent;
                        const Values:TVisualizerItems;
                        const AGroup:TVisualizerItem;
                        const ARows: TCursorIndex); override;

    property Grid:TBIGrid read FGrid;
  end;

  TGroupTrackbar=class(TGroupControlAlign)
  private
    FTrack : TTrackbar;

    procedure ClickedTrack(Sender:TObject);
    procedure ResizeTrackBar;
  protected
    procedure ChangeAlign(const Value: TGroupAlign); override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    property TrackBar:TTrackBar read FTrack;
  end;

  TGroupButtons=class(TGroupControlAlign)
  private
    FPanel : TWinControl;

    procedure ClickedButton(Sender:TObject);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;
  end;

  TGroupMultiScroll=(Automatic,Yes,No);

  // Redefine TCoordinate here (it is also at VCLTee.TeeCanvas and FMXTee.Canvas)
  // to not depend on a recent TeeChart version.
  TCoordinate={$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};

  TGroupMultiControl=class(TGroupControl)
  private
    FColumns : Integer;
    FExpandLast : Boolean;
    FRowHeight: Integer;
    FScroll : TGroupMultiScroll;
    FSplitters : Boolean;

    FScrollBox : TScrollBox;

    FScroller : Boolean;

    IExtras : Array of TControl;

    procedure AddExtra(const AControl:TControl);
    function AvailableWidth(const AControl:TWinControl; const AMax:Integer):TCoordinate;
    function CanResizeWidth(const AControl:TControl):Boolean;
    procedure ClearExtras;
    procedure ControlResized(Sender:TObject);
    procedure DoReLayout;
    procedure DoResize(const AControl:TControl; const Columns:Integer);
    procedure ReLayout;
    procedure SetColumns(const Value: Integer);
    procedure SplitHoriz(const AControl:TWinControl);
    procedure SplitVert(const AControl:TWinControl);
    procedure SubPanelControls(const APanel:TWinControl; const Columns:Integer);
    procedure SetExpandLast(const Value: Boolean);
    procedure SetScroll(const Value: TGroupMultiScroll);
    procedure SetRowHeight(const Value: Integer);
    procedure SetSplitters(const Value: Boolean);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Add(const AIndex:TInteger); override;
    procedure Assign(Source:TPersistent); override;

    property ScrollBox:TScrollBox read FScrollBox;
  published
    property Columns:Integer read FColumns write SetColumns default 2;
    property ExpandLast:Boolean read FExpandLast write SetExpandLast default False;
    property RowHeight:Integer read FRowHeight write SetRowHeight default 0;
    property Scroll:TGroupMultiScroll read FScroll write SetScroll default TGroupMultiScroll.Automatic;
    property Splitters:Boolean read FSplitters write SetSplitters default False;
  end;

  {$IFDEF FMX}
  TTreeNode=TTreeViewItem;
  {$ENDIF}

  TGroupTree=class(TGroupControlAlign)
  private
    FTree  : TTreeView;

    IParent : TTreeNode;

    procedure ChangedNode(Sender: TObject{$IFNDEF FMX}; Node: TTreeNode{$ENDIF});
  protected
    procedure Init; override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    property TreeView:TTreeView read FTree;
  end;

  // Nested TBIComposer
  TGroupVisualizer=class(TGroup)
  private
    FVisualizer : TBIComposer;
  protected
    procedure Init; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    property Visualizer:TBIComposer read FVisualizer;
  end;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64
              {$IFDEF FMX}
              or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              {$ENDIF}
              )]
  {$ENDIF}
  {$ENDIF}
  TBIComposer=class(TBIDataControl)
  private
    FData : TDataItem;

    FGroups,
    FValues : TVisualizerItems;

    FMain : TGroup;

    function BestGroup(const AItem:TVisualizerItem; const AIndex:Integer; const APrevious:TGroup):TGroup;

    procedure CreateCurrent;
    procedure CreateGroups;
    procedure Guess;
    function IndexOfEnabled(const AIndex:Integer):Integer;
    procedure ResetGroups;

    procedure SetGroups(const Value: TVisualizerItems);
    procedure SetValues(const Value: TVisualizerItems);
    procedure SortGroups;
  protected
    function AddGroup(const APrevious:TGroup; const AIndex:Integer;
             const AParent:TComponent;
             const ARows:TCursorIndex):TGroup;

    procedure AddItems;
    function GetDataItem:TDataItem; override;
    procedure Loaded; override;
    procedure SetDataItem(const Value:TDataItem); override;
  public
    class var
      ValuesGroupClass : TGroupClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    // Pending: Automatic Guess:
    // procedure BindTo(const ASummary:TSummary);

    procedure BestOrder;
    procedure ReCalculate;

    property Main:TGroup read FMain;
  published
    property Groups:TVisualizerItems read FGroups write SetGroups;
    property Values:TVisualizerItems read FValues write SetValues;
  end;

  // Helper class for both VCL and FMX TBIComposer editor dialogs:
  TBIVisualizerUI=record
  public
    Viz : TBIComposer;

    class procedure AddClasses(const AItems:TStrings); static;
    class procedure AddGroupByItems(const AItems:TStrings; const AData:TDataItem;
                                    const AIndex: Integer); static;
    class procedure AddItems(const AList: TCustomListBox; const AItems: TVisualizerItems); static;

    function ChangeClass(const AGroup,AClass:Integer):Boolean;

    function GetControl(const AIndex:Integer):TGroupControl;
    function GetList(const AIndex:Integer):TGroupList;
    function GetMulti(const AIndex:Integer):TGroupMultiControl;
  end;

implementation
