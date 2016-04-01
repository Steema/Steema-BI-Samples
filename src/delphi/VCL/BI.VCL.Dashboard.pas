{*********************************************}
{  TeeBI Software Library                     }
{  Screen Dashboard (VCL and FMX)             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Dashboard;
{.$DEFINE FMX}

interface

uses
  System.Classes, System.UITypes,
  {$IFDEF FMX}
  FMX.Controls, FMX.Layouts, FMX.ExtCtrls, FMX.TabControl, FMX.Graphics,
  FMX.StdCtrls, FMX.Types, FMX.Objects,
  {$ELSE}
  VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics, VCL.ComCtrls,
  VCL.Forms,
  {$ENDIF}
  BI.Data, BI.Dashboard;

type
  {$IFDEF FMX}
  TWinControl=TControl;
  TPageControl=TTabControl;
  TPanel=TRectangle;
  TAlign=TAlignLayout;
  TTabSheet=TTabItem;
  TGraphic=TBitmap;
  {$ENDIF}

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
      public
        Name : String;
        Control : TControl;
      end;

      TTargetControls=Array of TTargetControl;

      TTargetControlsHelper=record helper for TTargetControls
      public
        procedure Add(const AName:String; const AControl:TControl);
        function FindControl(const AName:String):TControl;
      end;

    var
      Targets : TTargetControls;

    //procedure AddControl(const AControl:TControl; const APosition:String; const AItem:TDashboardItem);
    procedure AddToParent(const AControl:TControl; const AParent:TWinControl; const AItem:TDashboardItem);

    procedure ButtonsChange(Sender:TObject);

    procedure ChangedVariable(const Sender:TChangeListener; const AValue:String);
    procedure Clear;

    procedure ComboChange(Sender:TObject);
    procedure ControlClick(Sender: TObject);

    function CreateLayoutClass(const AClass:TControlClass):TPanel;
    class function CreateSplitter(const AAlign:TAlign; const AParent:TWinControl):TSplitter; static;

    procedure EmitDashboard(const AItem:TDashboardItem; const APosition:String);
    procedure EmitLayout(const ALayout:TLayoutItem; const ShowNames:Boolean=False);
    function EmitPanel(const AItem:TDashboardItem; const AKind:String; const APosition:String=''):TControl;

    function GetTarget(const AName:String; const AItem:TDashboardItem): TObject;

    procedure CheckBoxChange(Sender:TObject);
    procedure ListChange(Sender: TObject);

    function NewGroupItem(const AText:String):TControl;

    procedure RefreshLabel(const ALabel:TLabel; const AItem:TDashboardItem);

    class procedure SetFont(const AVCLFont:TFont; const AFont:TBIFont); static;

    class procedure SetPadding(const AControl:TWinControl; const AItem:TDashboardItem); overload; static;
    class procedure SetPadding(const AControl:TWinControl; const AItem:TDashboard); overload; static;

    procedure TrackBarChange(Sender: TObject);
  protected
    procedure AddListener(const AName:String; const ADataIndex:Integer); override;
    function CanRefreshData(const AControl:TControl):Boolean; virtual;

    function CreatePanel(const ABack:TAlphaColor):TPanel; overload;
    function CreatePanel:TPanel; overload; inline;

    function EnsurePanelData(const APanel:TBIPanel):TDataItem;
    class function GetItemOf(const Sender:TControl):TDashboardItem; static;
    function NewControl(const AKind:String; const AItem:TDashboardItem):TControl; virtual;
    function ParentRender:TScreenRender;
    procedure PostAddControl(const AControl:TControl; const AItem:TDashboardItem); virtual;
    procedure RadioChange(Sender: TObject); virtual;
  public
    procedure Init(const ADashboard:TDashboard; const ALayout:String=''; const AParams:TStrings=nil); override;
    procedure Finish; override;

    procedure Emit(const ADashboard:TDashboard; const AItem:Integer; const APosition:String); override;

    property Control:TBIVisual read FControl write FControl;
  end;

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
  TBIVisual=class({$IFDEF FMX}TScrollBox{$ELSE}TScrollingWinControl{$ENDIF})
  public
    type
      TSizeUnits=(Pixels, Percent);

  private
    FTemplate : TBITemplate;
    FRender: TRender;
    FVariables : TVariables;

    ICanResize : Boolean;

    type
      TSizedControl=record
      public
        // [Weak]
        Control : TControl;
        Splitter : TSplitter;

        Width,
        Height : Single;

        WidthUnits,
        HeightUnits : TSizeUnits;

        procedure ReCalc;
        function Resized:Boolean;
        procedure SetSize(const AWidth,AHeight:String);
      end;

      TSizedControls=Array of TSizedControl;

      TSizedControlsHelper=record helper for TSizedControls
      public
        procedure Add(const AControl:TControl; const AWidth,AHeight:String);
        procedure CheckSize(const AItem:TLayoutItem; const AControl:TControl); overload;
        procedure CheckSize(const APanel:TBIPanel; const AControl:TControl); overload;
        procedure CheckSize(const AItem:TDashboardItem; const AControl:TControl); overload;
        function FindSplitter(const ASplitter:TSplitter):Integer;
        function IndexOf(const AControl:TControl):Integer;
        procedure Remove(const AControl:TControl);
        function TryAdd(const AControl:TControl):Integer;
      end;

    var
      ISizedControls : TSizedControls;

    procedure DoResize(const AParent:TWinControl);

    {$IFDEF FMX}
    procedure MovedSplitter(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    {$ELSE}
    procedure MovedSplitter(Sender: TObject);
    {$ENDIF}

    procedure SetRender(const Value: TRender);
    procedure SetTemplate(const Value: TBITemplate);
    procedure TryFreeTemplate;
  protected
    procedure Clear;
    procedure Resize; override;
  public
    class var
      RenderClass : TRenderClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    // Single Dashboard
    procedure Generate(const ADashboard:TDashboard; const ALayout:String=''); overload;

    // Single Panel
    procedure Generate(const APanel:TBIPanel); overload;

    // Use first Dashboard, if it exists
    procedure Generate; overload;

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
