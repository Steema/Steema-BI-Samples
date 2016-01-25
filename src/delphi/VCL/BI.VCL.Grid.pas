{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid control for VCL                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Grid;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}
  VCL.Controls, VCL.Forms, VCL.Grids, Data.DB, BI.Data,
  BI.DataSource, BI.Dataset, VCL.Graphics, Vcl.Menus, BI.UI.Colors, BI.UI;

type
  TBIGridPluginClass=class of TBIGridPlugin;

  // Pending: Try to merge VCL and FMX TBIGridPlugin classes into a single one.

  // Abstract class to define Grid control classes as "plugins" of TBIGrid.
  // See BI.VCL.Grid.DBGrid unit for an example, using the VCL TDBGrid.
  TBIGridPlugin=class abstract
  protected
    function GetDataSource: TDataSource; virtual; abstract;
    function GetReadOnly:Boolean; virtual; abstract;
    function GetTotals:Boolean; virtual; abstract;
    procedure SetReadOnly(const Value:Boolean); virtual; abstract;
    procedure SetTotals(const Value:Boolean); virtual; abstract;
    procedure SetDataSource(const Value: TDataSource); virtual; abstract;
  public
    class var
      Engine : TBIGridPluginClass;

    Constructor Create(const AOwner:TComponent); virtual; abstract;

    procedure Colorize(const AItems:TDataColorizers); virtual; abstract;
    procedure BindTo(const ADataSet:TDataSet); virtual; abstract;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); virtual; abstract;
    function GetControl:TControl; virtual; abstract;

    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly;
    property Totals:Boolean read GetTotals write SetTotals;
  end;

  // Generic Grid control that "links" a TDataItem with a Grid.
  TBIGrid = class(TWinControl)
  private
    IDataSet : TBIDataset;
    IPlugin : TBIGridPlugin;

    IDataSetRight : TBIDataset;
    IPluginRight : TBIGridPlugin;

    FShowItems : Boolean;

    {$IFDEF FPC}
    FParentBack : Boolean;
    {$ENDIF}

    procedure ChangedRow(Sender: TObject; Field: TField);

    function GetData:TDataItem;
    function GetDataSource: TDataSource;
    function GetTotals:Boolean;

    procedure ReadOrigin(Reader: TReader);
    procedure SetData(const Value: TDataItem);
    procedure SetTotals(const Value: Boolean);

    procedure WriteOrigin(Writer: TWriter);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetPlugin(const Value: TBIGridPlugin);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure BindTo(const Data:TDataItem);
    procedure Colorize(const AItems:TDataColorizers);
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean);

    property DockManager;
    property Plugin:TBIGridPlugin read IPlugin write SetPlugin;

  published
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderWidth;
    // NO: property Caption;
    property Color;
    property Constraints;
    {$IFNDEF FPC}
    property Ctl3D;
    {$ENDIF}
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    {$IFNDEF FPC}
    property Padding;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentBackground {$IFDEF FPC}:Boolean read FParentBack write FParentBack{$ENDIF};
    property ParentColor;
    {$IFNDEF FPC}
    property ParentCtl3D;
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFNDEF FPC}
    property Touch;
    {$ENDIF}
    property Visible;
    {$IFNDEF FPC}
    property StyleElements;
    {$ENDIF}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFNDEF FPC}
    property OnGesture;
    {$ENDIF}
    property OnGetSiteInfo;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    property Data:TDataItem read GetData write SetData;
    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property ShowItems:Boolean read FShowItems write FShowItems default False;
    property Totals:Boolean read GetTotals write SetTotals default False;
  end;

  // See global TVCLCommon.Diagram property
  TDiagramEvent=procedure(const AOwner:TComponent; const AData:TDataItem);

  // Helper methods for VCL:
  TVCLCommon=record
  public
    // This event is initialized by BI.VCL.LinkDiagram unit (when used).
    class var Diagram : TDiagramEvent;

    class procedure AddForm(const AForm: TCustomForm; const AParent: TWinControl); static;
    class procedure LoadPosition(const AForm:TCustomForm; const Key:String); static;
    class procedure Popup(const APopup:TPopupMenu; const AParent:TControl); static;
    class procedure SavePosition(const AForm:TCustomForm; const Key:String); static;
    class function YesNo(const Message:String):Boolean; static;
  end;

implementation
