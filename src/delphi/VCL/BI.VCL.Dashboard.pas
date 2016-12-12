{*********************************************}
{  TeeBI Software Library                     }
{  Screen Dashboard (VCL and FMX)             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Dashboard;
{.$DEFINE FMX}

interface

{$IFNDEF FPC}
{$IF CompilerVersion>26}
{$DEFINE XE6}
{$ENDIF}
{$ENDIF}

uses
  System.Classes,

  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.UITypes,
  {$ENDIF}

  {$IFDEF FMX}
  FMX.Controls, FMX.Layouts, FMX.ExtCtrls, FMX.TabControl,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21_0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  FMX.StdCtrls, FMX.Types, FMX.Objects,
  {$ELSE}
  WinApi.Messages, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.ComCtrls,
  VCL.Forms,
  {$ENDIF}

  BI.Data, BI.Expression, BI.Dashboard, BI.Dashboard.Layouts;

type
  {$IFDEF FMX}
  TWinControl=TControl;
  TPageControl=TTabControl;
  TPanel=TRectangle;
  TAlign=TAlignLayout;
  TTabSheet=TTabItem;
  TGraphic=TBitmap;
  TControlClass=class of TWinControl;
  {$ENDIF}

  TLayoutPanel=class(TPanel)
  private
    FBackColor: TAlphaColor;
    procedure SetBackColor(const Value: TAlphaColor);
  public
    DefaultColor : TAlphaColor;

    Constructor Create(AOwner:TComponent); override;

    property BackColor:TAlphaColor read FBackColor write SetBackColor default TAlphaColors.Null;
  end;

  TBIVisual=class;

  TScreenRender=class(TRender)
  private
    {$IFDEF AUTOREFCOUNT}
    [Weak]
    {$ENDIF}
    FControl : TBIVisual;

    FLayoutGroup : TControl;

    {$IFDEF AUTOREFCOUNT}
    [Weak]
    {$ENDIF}
    IDashboard : TDashboard;

    type
      TTargetControl=record
      private
        {$IFDEF FMX}
        FFrame : TRectangle;
        FText : TText;
        {$ELSE}
        FFrame : TShape;
        {$ENDIF}
      public
        Name : String;
        Control : TControl;
        Layout : TLayoutItem;

        procedure ShowFrame(const AShow:Boolean);
        procedure ShowText(const AShow:Boolean);
      end;

      TTargetControls=Array of TTargetControl;

    //procedure AddControl(const AControl:TControl; const APosition:String; const AItem:TDashboardItem);
    procedure AddToParent(const AControl:TControl; const AParent:TWinControl; const AItem:TDashboardItem);

    procedure ButtonsChange(Sender:TObject);

    procedure ChangedVariable(const Sender:TChangeListener; const AValue:TExpression);

    procedure ComboChange(Sender:TObject);
    procedure ControlClick(Sender: TObject);
    function ControlOwner:TComponent;

    function CreateLayoutClass(const AClass:TControlClass):TPanel;
    class function CreateSplitter(const AAlign:TAlign; const AParent:TWinControl):TSplitter; static;

    procedure EmitDashboard(const AItem:TDashboardItem; const APosition:String);
    procedure EmitLayout(const ALayout:TLayoutItem; const Splitters:Boolean=True;
                         const ShowNames:Boolean=False);

    function GetTarget(const AName:String; const AItem:TDashboardItem): TObject;

    procedure CheckBoxChange(Sender:TObject);
    procedure ListChange(Sender: TObject);
    procedure TreeChange(Sender:TObject);

    function NewGroupItem(const AText:String):TControl;

    procedure RefreshLabel(const ALabel:TLabel; const AItem:TDashboardItem);
    procedure RemoveNested(AControl: TControl);

    class procedure SetFont(const AVCLFont:TFont; const AFont:TBIFont); static;

    class procedure SetPadding(const AControl:TWinControl; const AItem:TDashboardItem); overload; static;
    class procedure SetPadding(const AControl:TWinControl; const AItem:TDashboard); overload; static;

    procedure TrackBarChange(Sender: TObject);
  protected
    procedure AddListener(const AName:String; const ASource:TObject); override;
    function CanRefreshData(const AControl:TControl):Boolean; virtual;

    function CreateControl(const ALayout:TLayoutItem; const AParent:TWinControl;
                           const APrefix:String;
                           const Splitters,ShowNames:Boolean):TWinControl;

    function CreatePanel(const ABack:TAlphaColor):TLayoutPanel; overload;
    function CreatePanel:TLayoutPanel; overload; inline;

    function EmitPanel(const AItem:TDashboardItem; const AKind:TPanelKind; const APosition:String=''):TControl;
    function EnsurePanelData(const APanel:TCustomBIPanel):TDataItem;
    class function GetItemOf(const Sender:TControl):TDashboardItem; static;
    function NewControl(const AKind:TPanelKind; const AItem:TDashboardItem):TControl; virtual;
    function ParentRender:TScreenRender;
    procedure PostAddControl(const AControl:TControl; const AItem:TDashboardItem); virtual;
    procedure RadioChange(Sender: TObject); virtual;
  public
    Targets : TTargetControls;

    type
      TTargetControlsHelper=record helper for TTargetControls
      public
        function Add(const AName:String; const AControl:TControl; const ALayout:TLayoutItem):Integer;
        function Count:Integer; inline;
        function FindControl(const AName:String):TControl; overload;
        function FindControl(const AItem:TLayoutItem):TControl; overload;
        function FindLayout(const AControl:TControl):TLayoutItem;
      end;

    procedure Clear; override;
    procedure Init(const ADashboard:TDashboard; const ALayout:String=''; const AParams:TStrings=nil); override;
    procedure Finish; override;

    procedure Emit(const ADashboard:TDashboard; const AItem:Integer; const APosition:String); override;
    procedure SetNested(const AItem:TLayoutItem; const AName:String; const Splitters,ShowNames:Boolean);

    property Control:TBIVisual read FControl write FControl;
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
  TBIVisual=class({$IFDEF FMX}TScrollBox{$ELSE}TScrollingWinControl{$ENDIF})
  private
    FCurrent : TDashboard;
    FRender: TRender;
    FTemplate : TBITemplate;
    FVariables : TVariables;

    ICanResize : Boolean;
    ICurrent : String; // temporary during loading

    {$IFNDEF FMX}
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    {$ENDIF}

    procedure DoResize(const AParent:TWinControl);
    function GetDashboards: TDashboards;
    function GetDataItems: TVisualDataItems; // GetData already exists in FMX
    function GetLayouts: TLayouts;
    function GetPanels: TPanels;

    {$IFDEF FMX}
    procedure MovedSplitter(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    {$ELSE}
    procedure MovedSplitter(Sender: TObject);
    {$ENDIF}

    procedure ReadCurrent(Reader: TReader);
    procedure SetCurrent(const Value: TDashboard);
    procedure SetDashboards(const Value: TDashboards);
    procedure SetDataItems(const Value: TVisualDataItems); // SetData already exists in FMX
    procedure SetLayouts(const Value: TLayouts);
    procedure SetPanels(const Value: TPanels);
    procedure SetRender(const Value: TRender);
    procedure SetTemplate(const Value: TBITemplate);
    procedure TryFreeTemplate;
    procedure TryGenerateCurrent;
    procedure TryResize;
    procedure WriteCurrent(Writer: TWriter);

    type
      TSizedControl=record
      private
        procedure TryRefreshItem;
      public
        // [Weak]
        Control : TControl;
        Splitter : TSplitter;

        Width,
        Height : Single;

        WidthUnits,
        HeightUnits : TUnits;

        procedure ReCalc;
        function Resized:Boolean;
        procedure SetSize(const AWidth:Single;
                          const AWidthUnits:TUnits;
                          const AHeight:Single;
                          const AHeightUnits:TUnits);
      end;

      TSizedControls=Array of TSizedControl;

      TSizedControlsHelper=record helper for TSizedControls
      public
        procedure Add(const AControl:TControl; const AWidth,AHeight:TBICoordinate);
        procedure CheckSize(const AItem:TLayoutItem; const AControl:TControl); overload;
        procedure CheckSize(const APanel:TCustomBIPanel; const AControl:TControl); overload;
        procedure CheckSize(const AItem:TDashboardItem; const AControl:TControl); overload;
        function FindSplitter(const ASplitter:TSplitter):Integer;
        function IndexOf(const AControl:TControl):Integer;
        procedure Remove(const AControl:TControl);
        function TryAdd(const AControl:TControl):Integer;
      end;

    var
      ISizedControls : TSizedControls;

  protected
    procedure Clear;
    procedure DefineProperties(Filer: TFiler); override;
    function GetChildOwner: TComponent; override;
    procedure Loaded; override;
    procedure Resize; override;
  public
    class var
      RenderClass : TRenderClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    // Single Dashboard, use first one if ADashboard is nil
    procedure Generate(const ADashboard:TDashboard=nil; const ALayout:String=''); overload;

    // Single Panel
    procedure Generate(const APanel:TBIPanel); overload;

    procedure ResizeLayout;

    property Render:TRender read FRender write SetRender;
    property Template:TBITemplate read FTemplate write SetTemplate;
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

    property Dashboard:TDashboard read FCurrent write SetCurrent stored False;

    property Dashboards : TDashboards read GetDashboards write SetDashboards;
    property Data : TVisualDataItems read GetDataItems write SetDataItems;
    property Layouts : TLayouts read GetLayouts write SetLayouts;
    property Panels : TPanels read GetPanels write SetPanels;

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
  end;

implementation
