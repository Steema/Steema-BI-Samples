unit VCLBI.DataControl;
{.$DEFINE FMX}

interface

uses
  System.Classes,
  {$IFDEF FMX}
  FMX.Controls, FMX.Layouts,
  {$ELSE}
  Vcl.Controls, Vcl.Graphics,
  {$ENDIF}
  BI.DataItem;

type
  // Base abstract Control that includes Data and Provider properties
  TBIDataControl=class({$IFDEF FMX}TLayout{$ELSE}TWinControl{$ENDIF})
  private
    {$IFDEF FPC}
    FParentBack : Boolean;
    {$ENDIF}

    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FData: TDataItem;

    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FProvider : TComponent;

    IOrigin : String;

    procedure DoSetProvider(const Value: TComponent);
    function GetDataItem:TDataItem;
    function GetProvider:TComponent;
    procedure InternalSetProvider(const Value:TComponent);
    function LoadOrigin:TDataItem;
    procedure Notify(const AEvent:TBIEvent);
    procedure NotifyDataDestroy(const AEvent:TBIEvent);
    function Origin:String;
    procedure ReadOrigin(Reader: TReader);
    procedure SetDataItem(const Value: TDataItem); // <-- do not rename to SetData (FMX conflict)
    procedure SetProvider(const Value: TComponent);
    procedure WriteOrigin(Writer: TWriter);
  protected
    procedure AddNotify;
    function ControlOfClass(const AClass:TClass): TControl;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveNotify;
    procedure SetDataDirect(const Value: TDataItem); virtual;
    procedure UpdatedDataValues; virtual;

    {$IFNDEF FMX}
    property DockManager;
    {$ENDIF}
  public
    Constructor Create(AOwner:TComponent); override;
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

    {$IFDEF FMX}
    {$IF CompilerVersion>=32}
    property OnResized;
    {$IFEND}
    {$ENDIF}

    {$IFNDEF FMX}
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    {$ENDIF}

    // TBIDataControl
    property Data:TDataItem read GetDataItem write SetDataItem default nil;
    property Provider:TComponent read GetProvider write SetProvider;
  end;

implementation

uses
  BI.Persist, BI.Store.Component;

{ TBIDataControl }

Constructor TBIDataControl.Create(AOwner: TComponent);
begin
  inherited;

  Height:=250;
  Width:=400;
end;

Destructor TBIDataControl.Destroy;
begin
  RemoveNotify;
  inherited;
end;

function TBIDataControl.Origin:String;
begin
  result:=TComponentImporter.Origin(Provider,Data);
end;

procedure TBIDataControl.NotifyDataDestroy(const AEvent:TBIEvent);
begin
  if AEvent=TBIEvent.Destroyed then
     FData:=nil
end;

procedure TBIDataControl.Notify(const AEvent:TBIEvent);
begin
  case AEvent of
    TBIEvent.Changed: RefreshData;

  TBIEvent.Destroyed:
          begin
            if csDestroying in ComponentState then
               FData:=nil
            else
            begin
              // Dot not use Origin function here, because it forces SetData again
              IOrigin:=TComponentImporter.Origin(Provider,FData);

              FData:=nil;
              SetDataDirect(nil);
            end;
          end;

  else
    UpdatedDataValues;
  end;
end;

type
  TDataAccess=class(TDataItem);
  TDataProviderAccess=class(TDataProvider);

procedure TBIDataControl.AddNotify;
begin
  if FProvider is TDataProvider then
     TDataProviderAccess(Provider).FConsumers.Add(Notify);

  if FData<>nil then
  begin
    TDataAccess(FData).FConsumers.Add(NotifyDataDestroy);
    TDataAccess(FData).FConsumers.Add(Notify);
  end;
end;

procedure TBIDataControl.RemoveNotify;
begin
  if FProvider is TDataProvider then
     TDataProviderAccess(Provider).FConsumers.Remove(Notify);

  if FData<>nil then
  begin
    TDataAccess(FData).FConsumers.Remove(NotifyDataDestroy);
    TDataAccess(FData).FConsumers.Remove(Notify);
  end;
end;

// Refresh same Data property
procedure TBIDataControl.RefreshData;
var tmp : TDataItem;
begin
  SetDataDirect(nil);

  if (FProvider<>nil) and (IOrigin<>'') then
     Data:=LoadOrigin;

  tmp:=Data;

  if tmp<>nil then
  begin
    if FProvider is TBaseDataImporter then
       TBaseDataImporter(FProvider).Data.Load;

    SetDataDirect(Data);
  end;
end;

procedure TBIDataControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent=FProvider) and (Operation=TOperation.opRemove) then
     DoSetProvider(nil);
end;

procedure TBIDataControl.Loaded;
begin
  inherited;
  Data;
end;

function TBIDataControl.LoadOrigin:TDataItem;
begin
  result:=TComponentImporter.TryLoadOrigin(Self,Provider,IOrigin);
end;

function TBIDataControl.GetDataItem: TDataItem;
begin
  if FData=nil then
     Data:=LoadOrigin;

  result:=FData;
end;

function TBIDataControl.GetProvider:TComponent;
begin
  if FProvider=nil then
  begin
    result:=TComponentImporter.ProviderOf(FData);

    if result<>nil then
       DoSetProvider(result);
  end
  else
     result:=FProvider;
end;

procedure TBIDataControl.SetDataDirect(const Value: TDataItem);
begin
end;

procedure TBIDataControl.InternalSetProvider(const Value:TComponent);
begin
  if FProvider<>nil then
     FProvider.RemoveFreeNotification(Self);

  FProvider:=Value;

  if FProvider<>nil then
     FProvider.FreeNotification(Self);
end;

procedure TBIDataControl.SetDataItem(const Value: TDataItem);

  function DifferentProvider(const AComponent:TComponent):Boolean;
  begin
    result:=(AComponent<>FProvider);

    // Special case:
    // When AComponent is equal to FProvider.Source, do not replace
    if result then
       if FProvider is TComponentImporter then
          if TComponentImporter(FProvider).Source=AComponent then
             result:=False;
  end;

var tmp : TComponent;
begin
  if FData<>Value then
  begin
    RemoveNotify;
    FData:=Value;

    tmp:=TComponentImporter.ProviderOf(FData);

    if DifferentProvider(tmp) then
       InternalSetProvider(tmp);

    if FData=nil then
       IOrigin:=''
    else
       IOrigin:=Origin;

    AddNotify;

    SetDataDirect(Value);
  end;
end;

procedure TBIDataControl.DoSetProvider(const Value: TComponent);
begin
  RemoveNotify;

  InternalSetProvider(Value);

  FData:=nil;

  AddNotify;

  if not (csLoading in ComponentState) then
  begin
    IOrigin:='';
    Data;
  end;
end;

procedure TBIDataControl.SetProvider(const Value: TComponent);
var tmp : TComponent;
begin
  tmp:=Provider;

  if tmp<>Value then
     DoSetProvider(Value);
end;

procedure TBIDataControl.UpdatedDataValues;
begin
end;

procedure TBIDataControl.WriteOrigin(Writer: TWriter);
begin
  Writer.WriteString(Origin);
end;

function TBIDataControl.ControlOfClass(const AClass:TClass): TControl;
var t : Integer;
    tmp : Integer;
begin
  tmp:={$IFDEF FMX}ControlsCount{$ELSE}ControlCount{$ENDIF};

  for t:=0 to tmp-1 do
      if Controls[t] is AClass then
         Exit(Controls[t]);

  result:=nil;
end;

procedure TBIDataControl.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor<>nil then
    begin
      result:=True;

      if Filer.Ancestor is TBIDataControl then
         result:=TBIDataControl(Filer.Ancestor).Data<>Data;
    end
    else
      result:=Data<>nil;
  end;

  function NeedsOrigin:Boolean;
  begin
    if (Data=nil) or (Data.Provider is TDataDelayProvider) then
       result:=True
    else
       result:=TComponentImporter.DataOfProvider(Data)<>Data;
  end;

begin
  inherited;
  Filer.DefineProperty('Origin', ReadOrigin, WriteOrigin, NeedsOrigin and DoWrite);
end;

procedure TBIDataControl.ReadOrigin(Reader: TReader);
begin
  IOrigin:=Reader.ReadString;
end;

procedure TBIDataControl.DestroyData;
begin
  {$IFNDEF AUTOREFCOUNT}
  Data.Free;
  {$ENDIF}
  Data:=nil;
end;

end.
