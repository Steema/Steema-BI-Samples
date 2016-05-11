unit BI.VCL.DataControl;
{.$DEFINE FMX}

interface

uses
  System.Classes,
  {$IFDEF FMX}
  FMX.Layouts,
  {$ELSE}
  Vcl.Controls, Vcl.Graphics,
  {$ENDIF}
  BI.Data;

type
  // Base abstract Control that includes Data and Provider properties
  TBIDataControl=class({$IFDEF FMX}TLayout{$ELSE}TWinControl{$ENDIF})
  private
    FProvider : TComponent;

    {$IFDEF FPC}
    FParentBack : Boolean;
    {$ENDIF}

    procedure Notify(const AEvent:TBIEvent);
    procedure ReadOrigin(Reader: TReader);
    procedure SetProvider(const Value: TComponent);
    procedure WriteOrigin(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDataItem:TDataItem; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataItem(const Value: TDataItem); virtual; abstract; // <-- do not rename to SetData (FMX conflict)
    procedure TryAddConsumer;
    procedure TryRemoveConsumer;

    {$IFNDEF FMX}
    property DockManager;
    {$ENDIF}
  public
    Destructor Destroy; override;

    procedure DestroyData;
    procedure RefreshData; virtual;
  published
    property Align;
    property Anchors;

    // NO: property Caption;

    {$IFNDEF FMX}
    property AutoSize;
    property BiDiMode;
    {$ENDIF}

    {$IFDEF FMX}
    property ClipChildren;
    property ClipParent;
    {$ENDIF}

    {$IFNDEF FMX}
    property Color default clBtnFace;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    {$ENDIF}

    property DragMode;

    {$IFDEF FMX}
    property EnableDragHighlight;
    {$ENDIF}

    property Enabled;
    property Height;

    {$IFDEF FMX}
    property HitTest;
    property Locked;
    {$ENDIF}

    {$IFDEF FMX}
    property Opacity;
    {$ENDIF}

    {$IFNDEF FPC}
    property Margins;
    property Padding;
    {$ENDIF}

    {$IFNDEF FMX}
    property ParentBiDiMode;
    property ParentBackground {$IFDEF FPC}:Boolean read FParentBack write FParentBack{$ENDIF};
    property ParentColor;
    {$IFNDEF FPC}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentShowHint;
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

    property ShowHint;

    property TabOrder;

    {$IFDEF FPC}
    property TabStop;
    {$ELSE}
    {$IF CompilerVersion>27}
    property TabStop;
    {$ENDIF}
    {$ENDIF}

    {$IFNDEF FPC}
    property Touch;
    {$ENDIF}

    property Visible;
    property Width;

    {$IFNDEF FMX}
    {$IFNDEF FPC}
    property StyleElements;
    {$ENDIF}

    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    {$ENDIF}

    property OnClick;

    {$IFNDEF FMX}
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    {$ENDIF}

    property OnDblClick;
    property OnDragDrop;

    {$IFDEF FMX}
    property OnDragEnd;
    property OnDragEnter;
    property OnDragLeave;
    {$ENDIF}

    property OnDragOver;

    {$IFNDEF FMX}
    property OnEndDock;
    property OnEndDrag;
    {$ENDIF}

    property OnEnter;
    property OnExit;
    {$IFNDEF FPC}
    property OnGesture;
    {$ENDIF}

    {$IFNDEF FMX}
    property OnGetSiteInfo;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    {$ENDIF}

    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;

    {$IFDEF FMX}
    property OnPainting;
    property OnPaint;
    {$ENDIF}

    property OnResize;

    {$IFNDEF FMX}
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    {$ENDIF}

    // TBIDataControl
    property Data:TDataItem read GetDataItem write SetDataItem;
    property Provider:TComponent read FProvider write SetProvider;
  end;

implementation
