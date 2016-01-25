{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Control                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Visualizer;
{$DEFINE FMX}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, Data.DB,
  BI.Data, BI.Arrays, BI.DataSource,

  {$IFDEF FMX}
  FMXTee.Constants,
  {$ELSE}
  VCLTee.TeeConst,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}

  {$ENDIF}

  {$IFDEF FMX}
  System.Types,
  Fmx.Types, Fmx.Controls, Fmx.StdCtrls, Fmx.ListBox, Fmx.TabControl,
  Fmx.Layouts, Fmx.Objects, Fmx.TreeView,

  FMXTee.Canvas, FMXTee.Engine, FMXTee.Chart, BI.FMX.Grid

  {$IFDEF TEEPRO}
  , FMXTee.Tools.SubChart
  {$ENDIF}

  {$ELSE}

  Vcl.Controls, VCL.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Forms,
  VCLTee.TeCanvas, VCLTee.TeEngine, VCLTee.Chart, BI.VCL.Grid

  {$IFDEF TEEPRO}
  , VCLTee.TeeSubChart
  {$ENDIF}

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

  TBIVisualizer=class;

  TVisualizerItems=class(TOwnedCollection)
  private
    IParent : TBIVisualizer;

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

  TGroup=class(TComponent)
  private
    CanAddValues : Boolean;

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
    FItems : Array of TPendingItem;

    FVisualizer : TBIVisualizer;

    procedure AddGroup(const AGroup:TGroup);
  protected
    Position : TInteger;

    FixedWidth : Boolean;

    function AddItem(const AData:TDataItem; const ANext:Integer;
                  const ARows:TCursorIndex; const APos:TInteger):TInteger;

    procedure Init; virtual;
    procedure Finished; virtual;
    function AsString(const AData:TDataItem):String; overload;
    function AsString(const AIndex:TInteger):String; overload;

    {$IFNDEF FPC}
    procedure Traverse<T:Class>(const AProc:TGroupProc<T>);
    {$ENDIF}
  public
  class var
    GroupClasses:Array of TGroupClass;

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

    class function ClassIndexOf(const AClass:TGroupClass):Integer; static;
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

    procedure DataChange(Sender: TObject; Field: TField);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure Add(const AIndex:TInteger); override;

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

  TGroupSeries=class;

  TAutoStackSeries=(Automatic,Yes,Yes100,No);

  TGroupSeriesStyle=(Automatic,Series2D,Series3D,Geographic);

  TGroupSeriesOptions=class(TPersistent)
  private
    FAddNulls : Boolean;
    FAutoStack : TAutoStackSeries;
    FSeries2D : TChartSeriesClass;
    FSeries3D : TChartSeriesClass;
    FStyle : TGroupSeriesStyle;

    IParent : TGroupSeries;

    procedure SetAutoStack(const Value: TAutoStackSeries);
    procedure SetSeries2D(const Value: TChartSeriesClass);
    procedure SetSeries3D(const Value: TChartSeriesClass);
    procedure SetStyle(const Value: TGroupSeriesStyle);
    procedure SetAddNulls(const Value: Boolean);
  public
    Constructor Create(const AParent:TGroupSeries);

    procedure Assign(Source:TPersistent); override;
  published
    property AddNulls:Boolean read FAddNulls write SetAddNulls default True;
    property AutoStack:TAutoStackSeries read FAutoStack write SetAutoStack default TAutoStackSeries.Automatic;

    property Series2D:TChartSeriesClass read FSeries2D write SetSeries2D;
    property Series3D:TChartSeriesClass read FSeries3D write SetSeries3D;

    property Style:TGroupSeriesStyle read FStyle write SetStyle default TGroupSeriesStyle.Automatic;
  end;

  TGroupChart=class;

  TBIMultiAxis=(Automatic,Single,Two,Multiple);

  TGroupChartOptions=class(TPersistent)
  private
    FMultiAxes : TBIMultiAxis;
    FRender   : TCanvas3DClass;
    FSettings : Boolean;
    FTemplate : TChart;

    IParent : TGroupChart;

    function GetLegend: Boolean;
    function GetMarks: Boolean;
    function GetTemplate:TChart;
    procedure SetLegend(const Value: Boolean);
    procedure SetMarks(const Value: Boolean);
    procedure SetMultiAxes(const Value: TBIMultiAxis);
    procedure SetRender(const Value:TCanvas3DClass);
    procedure SetSettings(const Value: Boolean);
  public
    Constructor Create;

    procedure Assign(Source:TPersistent); override;
  published
    property Settings:Boolean read FSettings write SetSettings default True;
    property Legend:Boolean read GetLegend write SetLegend default True;
    property Marks:Boolean read GetMarks write SetMarks;
    property MultiAxes:TBIMultiAxis read FMultiAxes write SetMultiAxes default TBIMultiAxis.Automatic;
    property Render:TCanvas3DClass read FRender write SetRender;
    property Template:TChart read GetTemplate;
  end;

  TGroupChart=class(TGroup)
  private
    FChart : TCustomChart;
    FNext : TGroup;
    FOptions : TGroupChartOptions;

    IParent : TGroupChart;

    procedure ClickedSettings(Sender: TObject);
    procedure EnteredChart(Sender:TObject);
    class procedure InitChart(const AChart:TChart); static;
    procedure LeavedChart(Sender:TObject);
    function NewChart:TChart;
    procedure ShowSettingsButton(const Sender:TObject; const AShow:Boolean);
    procedure TryMultipleAxes(const AMulti:TBIMultiAxis);
    procedure SetCanvas(const AChart:TCustomChart);
  protected
    function AddItem:TComponent; virtual;
    procedure ApplyTemplate(const AChart,ATemplate:TCustomChart); overload; virtual;

    procedure ChartResized(Sender:TObject); virtual;

    procedure Init; override;
    procedure Finished; override;

    function NewSeries(const AName:String; const AOptions:TGroupSeriesOptions): TChartSeries;
    class procedure SetAxisTitle(const ASeries:TChartSeries; const AData:TDataItem); static;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure Add(const AIndex:TInteger); override;
    procedure ApplyTemplate; overload;

    procedure Assign(Source:TPersistent); override;

    property Chart:TCustomChart read FChart;
    property Group:TGroupChart read IParent;
  published
    property Options:TGroupChartOptions read FOptions; // write SetOptions
  end;

  TGroupSeries=class(TGroupChart)
  private
    type
      TSaveParams=record
        Component:TComponent;
        Values:TVisualizerItems;
        Group:TVisualizerItem;
        Rows:TCursorIndex;
      end;

    var
    FOptions : TGroupSeriesOptions;

    SingleSeries : TChartSeries;

    ISave : TSaveParams;

    function AddSeries(const AName:String):TChartSeries;
    function AddSingleSeries:TChartSeries;
    procedure CheckAutoStack;
    procedure DisableStack(const ASeries:TChartSeries);
    procedure EnableStack(const AStack:TAutoStackSeries; const ASeries:TChartSeries);
    function GetParentDataString:String;
    function MultipleNormalColumns(const Values:TVisualizerItems):Boolean;
    procedure Reset;
    procedure SetOptions(const Value: TGroupSeriesOptions);
    procedure TrySetTitles(const AGroup:TVisualizerItem; const Values:TVisualizerItems);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure AddValues(const AComponent:TComponent;
                        const Values:TVisualizerItems;
                        const AGroup:TVisualizerItem;
                        const ARows:TCursorIndex); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Options:TGroupSeriesOptions read FOptions write SetOptions;
  end;

  TGroupMultiScroll=(Automatic,Yes,No);

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

  {$IFDEF TEEPRO}
  // Uses TSubChart tool items, one for each item in the group dimension
  TGroupSubChart=class(TGroupChart)
  private
    FColumns : Integer;
    FSameAxisRange : Boolean;
    FTool : TSubChartTool;

    procedure HideChartParts;
    procedure RecalcAxisRange;
    procedure SetColumns(const Value: Integer);
    procedure SetSameAxisRange(const Value: Boolean);
  protected
    function AddItem:TComponent; override;
    procedure ApplyTemplate(const AChart,ATemplate:TCustomChart); override;
    procedure ChartResized(Sender:TObject); override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Assign(Source:TPersistent); override;

    property SubChart:TSubChartTool read FTool;
  published
    property Columns:Integer read FColumns write SetColumns default 0;
    property SameAxisRange:Boolean read FSameAxisRange write SetSameAxisRange default True;
  end;
  {$ENDIF}

  {
  // Uses a TChartScroller Tool as a way to group a dimension
  TGroupChartScroller=class(TGroup)
  public
    Constructor Create(const AParent:TWinControl); override;
    function Add(const AName:String):TComponent; override;
  end;
  }

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

  // Nested TBIVisualizer
  TGroupVisualizer=class(TGroup)
  private
    FVisualizer : TBIVisualizer;
  protected
    procedure Init; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    property Visualizer:TBIVisualizer read FVisualizer;
  end;

  {$IFDEF FMX}
  {$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IFDEF VER250}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IFDEF VER260}or pidAndroid{$ENDIF}
              {$IFDEF VER290}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  {$ENDIF}
  TBIVisualizer=class(TWinControl)
  private
    FData : TDataItem;

    FGroups,
    FValues : TVisualizerItems;

    FMain : TGroup;

    function AddGroup(const APrevious:TGroup; const AIndex:Integer;
             const AParent:TComponent;
             const ARows:TCursorIndex):TGroup;

    function BestControl:TGroupClass;
    function BestGroup(const AItem:TVisualizerItem; const AIndex:Integer; const APrevious:TGroup):TGroup;

    procedure CreateCurrent;
    procedure CreateGroups;
    procedure Guess;
    function IndexOfEnabled(const AIndex:Integer):Integer;
    procedure ResetGroups;
    procedure SetDataItem(const Value: TDataItem);

    procedure ReadOrigin(Reader: TReader);
    procedure WriteOrigin(Writer: TWriter);
    procedure SetGroups(const Value: TVisualizerItems);
    procedure SetValues(const Value: TVisualizerItems);
    procedure SortGroups;
  protected
    procedure AddItems;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    // Automatic Guess:
    // procedure BindTo(const ASummary:TSummary);

    procedure BestOrder;
    procedure ReCalculate;

    property Main:TGroup read FMain;
  published
    property Align;
    property Anchors;

    {$IFDEF FMX}
    property ClipChildren;
    property ClipParent;
    {$ENDIF}

    property Cursor;
    property DragMode;

    {$IFDEF FMX}
    property EnableDragHighlight;
    {$ENDIF}

    property Enabled;

    {$IFDEF FMX}
    property Locked;
    {$ENDIF}

    property Height;

    {$IFDEF FMX}
    property HitTest;
    {$ENDIF}

    {$IFNDEF FPC}
    property Padding;
    {$ENDIF}

    {$IFDEF FMX}
    property Opacity;
    {$ENDIF}

    {$IFNDEF FPC}
    property Margins;
    {$ENDIF}

    property PopupMenu;

    {$IFDEF FMX}
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;

    {$IF CompilerVersion>27}
    property Size;
    {$ENDIF}

    property TouchTargetExpansion;
    {$ENDIF}

    property Visible;
    property Width;
    property TabOrder;

    {$IFDEF FPC}
    property TabStop;
    {$ELSE}
    {$IF CompilerVersion>27}
    property TabStop;
    {$ENDIF}
    {$ENDIF}

    { Events }

    {$IFDEF FMX}
    property OnPainting;
    property OnPaint;
    {$ENDIF}

    property OnResize;
    { Drag and Drop events }

    {$IFDEF FMX}
    property OnDragEnter;
    property OnDragLeave;
    {$ENDIF}

    property OnDragOver;
    property OnDragDrop;

    {$IFDEF FMX}
    property OnDragEnd;
    {$ENDIF}

    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property Data:TDataItem read FData write SetDataItem;
    property Groups:TVisualizerItems read FGroups write SetGroups;
    property Values:TVisualizerItems read FValues write SetValues;
  end;

  // Helper class for both VCL and FMX TBIVisualizer editor dialogs:
  TBIVisualizerUI=record
  public
    Viz : TBIVisualizer;

    class procedure AddClasses(const AItems:TStrings); static;
    class procedure AddGroupByItems(const AItems:TStrings; const AData:TDataItem;
                                    const AIndex: Integer); static;
    class procedure AddItems(const AList: TCustomListBox; const AItems: TVisualizerItems); static;

    function ChangeClass(const AGroup,AClass:Integer):Boolean;

    class function CurrentSeriesClass(const AItems:TComboBox):TChartSeriesClass; static;

    class procedure Fill2DSeries(const AItems:TStrings); static;
    class procedure Fill3DSeries(const AItems:TStrings); static;
    class function FindSeries(const AItems:TStrings; const AClass:TChartSeriesClass):Integer; static;

    function GetChart(const AIndex:Integer):TGroupChart;
    function GetControl(const AIndex:Integer):TGroupControl;
    function GetList(const AIndex:Integer):TGroupList;
    function GetMulti(const AIndex:Integer):TGroupMultiControl;
    function GetSeries(const AIndex:Integer):TGroupSeries;

    {$IFDEF TEEPRO}
    function GetSubChart(const AIndex:Integer):TGroupSubChart;
    {$ENDIF}
  end;

implementation
