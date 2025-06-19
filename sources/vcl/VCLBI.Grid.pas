{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid control for VCL                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Grid;

interface

(*
  Base class for TBIGrid control, in VCL applications.

  TBIGrid can use different "grid" controls, it uses standard VCL TDBGrid
  by default.

  To use another grid control, like for example a TeeGrid:

  uses
    VCLBI.Grid, VCLBI.Grid.TeeGrid;

  TBIGrid.Engine:=TBITeeGridPlugin;

*)

uses
  System.Classes, System.SysUtils, System.Types,
  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}
  VCL.Controls, VCL.Forms, Data.DB, BI.DataItem,
  BI.DataSource, BI.Dataset, VCL.Graphics, Vcl.ComCtrls,
  BI.UI, VCLBI.DataControl, BI.Expression, BI.Grid.Plugin;

type
  TBIGrid=class;

  TBaseEnabled=class(TPersistent)
  private
    FEnabled : Boolean;

    IGrid : TBIGrid;
  protected
    procedure SetEnabled(const Value: Boolean); virtual; abstract;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default False;
  end;

  TRowNumbers=class(TBaseEnabled)
  protected
    procedure SetEnabled(const Value: Boolean); override;
  end;

  TGridFilters=class(TBaseEnabled)
  protected
    procedure SetEnabled(const Value: Boolean); override;
  end;

  TGridShowItems=(Automatic, Yes, No); //, SubTables);

  TGridSearch=class(TBaseEnabled)
  protected
    procedure SetEnabled(const Value: Boolean); override;
  end;

  // Generic Grid control that "links" a TDataItem with a Grid.
  TBIGrid = class(TBIDataControl)
  private
    IDataSet : TBIDataset;
    IPlugin : TBIGridPlugin;

    IDataSetRight : TBIDataset;
    IPluginRight : TBIGridPlugin;

    FAlternate : TAlternateColor;
    FGridFilters : TGridFilters;
    FOnDataChange : TNotifyEvent;
    FOnUpdateData : TNotifyEvent;
    FRowNumbers : TRowNumbers;
    FSearch : TGridSearch;
    FShowItems : TGridShowItems;

    procedure ChangedRow(Sender: TObject; Field: TField);
    procedure ControlDblClick(Sender:TObject);
    function GetCurrentRow: Integer;
    function GetDataSource: TDataSource;
    function GetFilter: TExpression;
    function GetReadOnly: Boolean;
    function GetTotals:Boolean;
    function HasSubItem: Boolean;
    procedure HideShowItems;
    function PluginControl:TWinControl;

    procedure SetAlternate(const Value: TAlternateColor);
    procedure SetCurrentRow(const Value: Integer);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetFilter(const AFilter:TExpression);
    procedure SetGridFilters(const Value: TGridFilters);
    procedure SetPlugin(const Value: TBIGridPlugin);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRowNumbers(const Value: TRowNumbers);
    procedure SetSearch(const Value: TGridSearch);
    procedure SetShowItems(const Value: TGridShowItems);
    procedure SetTotals(const Value: Boolean);

    procedure UpdatedData(DataSet: TDataSet);

    procedure TryShowItems;
  protected
    procedure SetDataDirect(const Value: TDataItem); override;

    function SubItem:TDataItem;
    function SubGrid:TBIGridPlugin;

    procedure UpdatedDataValues; override;
  public
    class var
      Engine : TBIGridPluginClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure BindTo(const AData:TDataItem);
    procedure Colorize(const AItems:TDataColorizers);
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean);
    procedure Invalidate; override;

    property CurrentRow:Integer read GetCurrentRow write SetCurrentRow;
    property Filter:TExpression read GetFilter write SetFilter;
    property Plugin:TBIGridPlugin read IPlugin write SetPlugin;
  published
    property Alternate:TAlternateColor read FAlternate write SetAlternate;
    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property Filters:TGridFilters read FGridFilters write SetGridFilters;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property RowNumbers:TRowNumbers read FRowNumbers write SetRowNumbers;
    property Search:TGridSearch read FSearch write SetSearch;
    property ShowItems:TGridShowItems read FShowItems write SetShowItems default TGridShowItems.Automatic;
    property Totals:Boolean read GetTotals write SetTotals default False;

    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnUpdateData:TNotifyEvent read FOnUpdateData write FOnUpdateData;
  end;

  // Helper methods for VCL:
  TUICommon=record
  public
    const
      AlignNone=TAlign.alNone;
      AlignClient=TAlign.alClient;
      AlignLeft=TAlign.alLeft;
      AlignTop=TAlign.alTop;
      AlignRight=TAlign.alRight;
      AlignBottom=TAlign.alBottom;

    class procedure AddForm(const AForm: TCustomForm; const AParent: TWinControl); static;
    class function AutoTest:Boolean; static;
    class function EditColor(const AOwner:TComponent; const AColor:TColor; out ANew:TColor):Boolean; static;
    class procedure GotoURL(const AOwner:TWinControl; const AURL:String); static;
    class function Input(const ATitle,ACaption,ADefault:String; out ANew:String):Boolean; static;
    class procedure LoadPosition(const AForm:TCustomForm; const Parent,Key:String); overload; static;
    class procedure LoadPosition(const AForm:TCustomForm; const Key:String); overload; static;
    class function Point(const X,Y:Integer):TPoint; static; inline;
    class procedure SavePosition(const AForm:TCustomForm; const Parent,Key:String); overload; static;
    class procedure SavePosition(const AForm:TCustomForm; const Key:String); overload; static;
    class function SelectFolder(var AFolder:String):Boolean; static;
    class procedure ShowUnique(const ATab:TTabSheet); static;
    class function YesNo(const Message:String):Boolean; static;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.ShellApi,
  {$ENDIF}

  {$IFNDEF FPC}
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl, // <-- Pending: find a multi-platform solution for SelectDirectory
  {$WARN UNIT_PLATFORM ON}
  {$ENDIF}

  VCLBI.Grid.DBGrid, // <-- default plugin
  VCL.Dialogs, VCL.ExtCtrls,
  BI.Persist, BI.Arrays, BI.Languages.English, BI.Store.Component;

type
  TControlAccess=class(TControl);
  TPluginAccess=class(TBIGridPlugin);

{ TBIGrid }

Constructor TBIGrid.Create(AOwner: TComponent);
var tmp : TRect;
    tmpControl : TControl;
begin
  inherited;

  if TBIGrid.Engine=nil then
     raise EBIException.Create(BIMsg_Grid_MissingEngine);

  IPlugin:=TBIGrid.Engine.Create(Self);

  Color:=clBtnFace;

  FAlternate:=TAlternateColor.Create;
  FAlternate.OnChange:=TPluginAccess(IPlugin).ChangedAlternate;
  TPluginAccess(IPlugin).ChangedAlternate(FAlternate);

  FRowNumbers:=TRowNumbers.Create;
  FRowNumbers.IGrid:=Self;

  FGridFilters:=TGridFilters.Create;
  FGridFilters.IGrid:=Self;

  FSearch:=TGridSearch.Create;
  FSearch.IGrid:=Self;

  if IPlugin.GetObject is TControl then
  begin
    tmpControl:=TControl(IPlugin.GetObject);

    tmp:=tmpControl.BoundsRect;
    SetBounds(tmp.Left,tmp.Top,tmp.Right-tmp.Left,tmp.Bottom-tmp.Top);

    TControlAccess(tmpControl).OnDblClick:=ControlDblClick;
  end;
end;

Destructor TBIGrid.Destroy;
begin
  FSearch.Free;
  FRowNumbers.Free;
  FGridFilters.Free;
  FAlternate.Free;

  IPluginRight.Free;
  IPlugin.Free;

  inherited;
end;

procedure TBIGrid.Colorize(const AItems:TDataColorizers);
begin
  IPlugin.Colorize(AItems);
end;

procedure TBIGrid.ControlDblClick(Sender:TObject);
begin
  DblClick;
end;

function TBIGrid.GetCurrentRow: Integer;
begin
  if (DataSource<>nil) and (DataSource.DataSet<>nil) then
     result:=DataSource.DataSet.RecNo-1
  else
     result:=-1;
end;

function TBIGrid.GetDataSource: TDataSource;
begin
  result:=IPlugin.DataSource;
end;

function TBIGrid.GetFilter: TExpression;
begin
  if DataSource.DataSet is TBIDataset then
     result:=TBIDataset(DataSource.DataSet).Cursor.Filter
  else
     result:=nil;
end;

function TBIGrid.GetReadOnly: Boolean;
begin
  result:=IPlugin.ReadOnly;
end;

function TBIGrid.GetTotals: Boolean;
begin
  result:=IPlugin.Totals;
end;

procedure TBIGrid.BindTo(const AData: TDataItem);
begin
  Data:=AData;
end;

type
  TBIDatasetAccess=class(TBIDataset);

procedure TBIGrid.ChangedRow(Sender: TObject; Field: TField);
begin
  if HasSubItem then
  begin
    if IDataSetRight=nil then
       IDataSetRight:=TBIDataset.Create(Self);

    IDataSetRight.Close;

    IDataSetRight.Data:=nil; // <-- set to nil first, to force clear of IDataSet.Cursor.SortBy

    IDataSetRight.Data:=Data.Items[TBIDatasetAccess(IDataSet).GetIndexPosition {RecNo-1}];

    IPluginRight.BindTo(IDataSetRight);

    if IDataSetRight.Data<>nil then
       IDataSetRight.Open;
  end;

  if Assigned(FOnDataChange) then
     FOnDataChange(Self);
end;

type
  TDataItemAccess=class(TDataItem);
  TDataProviderAccess=class(TDataProvider);

procedure TBIGrid.UpdatedData(DataSet: TDataSet);
var tmp : TDataItem;
begin
  // Prevent re-entrancy (possible Consumers.Broadcast)
  RemoveNotify;
  try
    tmp:=Data;

    if tmp<>nil then
       TDataItemAccess(tmp).FConsumers.Broadcast(TBIEvent.ChangedValues);

    if Assigned(FOnUpdateData) then
       FOnUpdateData(Self);
  finally
    AddNotify;
  end;
end;

procedure TBIGrid.UpdatedDataValues;
begin
  inherited;
  IDataSet.Refresh;
end;

procedure TBIGrid.SetAlternate(const Value: TAlternateColor);
begin
  FAlternate.Assign(Value);

  if Assigned(FAlternate.OnChange) then
     FAlternate.OnChange(FAlternate);
end;

procedure TBIGrid.SetCurrentRow(const Value: Integer);
begin
  if (DataSource<>nil) and (DataSource.DataSet<>nil) then
     DataSource.DataSet.RecNo:=Value+1;
end;

procedure TBIGrid.SetDataDirect(const Value: TDataItem);

  procedure DoSetData;
  begin
    if IDataSet=nil then
       IDataSet:=TBIDataset.Create(Self);

    IDataSet.Close;

    IDataSet.Data:=nil; // <-- set to nil first, to force clear of IDataSet.Cursor.SortBy
    IDataSet.Data:=Value;

    IPlugin.BindTo(IDataSet);

    DataSource.OnDataChange:=ChangedRow;
    IDataset.AfterPost:=UpdatedData;
    IDataset.AfterDelete:=UpdatedData;

    IPluginRight.Free;
    IPluginRight:=nil;

    if IDataSet.Data=nil then
       HideShowItems
    else
    begin
      IDataSet.Open;
      TryShowItems;
    end;
  end;

begin
  inherited;
  DoSetData;
end;

procedure TBIGrid.SetDataSource(const Value: TDataSource);
begin
  IPlugin.DataSource:=Value;
end;

procedure TBIGrid.SetFilter(const AFilter: TExpression);
var tmp : TDataCursor;
    tmpDataset : TBIDataset;
begin
  if DataSource.DataSet is TBIDataset then
  begin
    tmpDataset:=TBIDataset(DataSource.DataSet);
    tmp:=tmpDataset.Cursor;
    tmp.Filter:=AFilter;
    tmp.PrepareIndex(nil);
    tmpDataset.PrepareIndex(tmp.Index);
  end;
end;

procedure TBIGrid.SetPlugin(const Value: TBIGridPlugin);
var tmp : TDataSource;
begin
  if IPlugin<>Value then
  begin
    // Try to obtain "IDataSet" from current plugin:
    if IPlugin<>nil then
       if IDataSet=nil then
       begin
         tmp:=IPlugin.DataSource;

         if tmp<>nil then
             if tmp.DataSet is TBIDataset then
                IDataSet:=TBIDataSet(tmp.DataSet);
       end;

    IPlugin.Free;
    IPlugin:=Value;

    // Set dataset again to new plugin:
    if IPlugin<>nil then
       if IDataSet<>nil then
          IPlugin.BindTo(IDataSet);
  end;
end;

procedure TBIGrid.SetReadOnly(const Value: Boolean);
begin
  IPlugin.ReadOnly:=Value;
end;

procedure TBIGrid.SetGridFilters(const Value: TGridFilters);
begin
  if FGridFilters.Enabled<>Value.Enabled then
  begin
    FGridFilters.Enabled:=Value.Enabled;
    TPluginAccess(Plugin).SetFilters(FGridFilters.Enabled);
  end;
end;

procedure TBIGrid.SetRowNumbers(const Value: TRowNumbers);
begin
  if FRowNumbers.Enabled<>Value.Enabled then
  begin
    FRowNumbers.Enabled:=Value.Enabled;
    TPluginAccess(Plugin).SetRowNumber(FRowNumbers.Enabled);
  end;
end;

procedure TBIGrid.SetSearch(const Value: TGridSearch);
begin
  if FSearch.Enabled<>Value.Enabled then
  begin
    FSearch.Enabled:=Value.Enabled;
    TPluginAccess(Plugin).SetSearch(FSearch.Enabled);
  end;
end;

procedure TBIGrid.HideShowItems;
var tmp,
    tmpParent : TWinControl;
begin
  // Destroy right-side grid

  if IPluginRight<>nil then
  begin
    DataSource.OnDataChange:=nil;

    IPluginRight.Free;
    IPluginRight:=nil;
  end;

  // Reset main grid alignment to client

  tmp:=PluginControl;

  if tmp<>nil then
  begin
    tmpParent:=tmp.Parent;

    if (tmpParent<>nil) and (tmpParent.Align<>TAlign.alClient) then
       tmpParent.Align:=TAlign.alClient;
  end;
end;

procedure TBIGrid.Invalidate;
var tmp : TWinControl;
begin
  inherited;

  tmp:=PluginControl;

  if tmp<>nil then
     tmp.Invalidate;
end;

function TBIGrid.PluginControl: TWinControl;
begin
  if IPlugin.GetObject is TWinControl then
     result:=TWinControl(IPlugin.GetObject)
  else
     result:=nil;
end;

procedure TBIGrid.TryShowItems;

  function ExistsSplitter:TSplitter;
  var t : Integer;
  begin
    for t:=0 to ControlCount-1 do
        if Controls[t] is TSplitter then
           Exit(TSplitter(Controls[t]));

    result:=nil;
  end;

  function AddSplitter:TSplitter;
  begin
    result:=TSplitter.Create(Self);
    result.Parent:=Self;
  end;

  procedure CheckSplitter;
  var tmp : TSplitter;
      tmpControl : TControl;
  begin
    tmp:=ExistsSplitter;

    if tmp=nil then
       tmp:=AddSplitter;

    tmpControl:=PluginControl;

    if tmpControl<>nil then
       tmp.Left:=tmpControl.Width+1;
  end;

  procedure RelayoutGrids;
  var tmpControl,
      tmpRight : TWinControl;
  begin
    tmpControl:=PluginControl;

    if tmpControl<>nil then
    begin
      tmpControl.Parent.Align:=TAlign.alLeft;

      TPluginAccess(IPlugin).AutoWidth;

      if tmpControl.Parent.Width>(Width div 2) then
         tmpControl.Parent.Width:=Width div 2;
    end;

    tmpRight:=(IPluginRight.GetObject as TWinControl);
    tmpRight.Parent.Align:=alClient;
  end;

  function CanShowItems:Boolean;
  begin
    result:=(FShowItems=TGridShowItems.Automatic) or
            (FShowItems=TGridShowItems.Yes);
  end;

  function IsDataList:Boolean;
  begin
    result:=(Data<>nil) and (not Data.AsTable) and
            (Data.Kind=TDataKind.dkUnknown);
  end;

var tmp : Boolean;
begin
  tmp:=CanShowItems and IsDataList and (Data.Items.Count>0);

  if tmp then
  begin
    if IPluginRight=nil then
       IPluginRight:=TBIGrid.Engine.Create(Self);

    ReLayoutGrids;

    // nil: IPluginRight.IAlternate.Enabled:=Plugin.IAlternate.Enabled;
    IPluginRight.ReadOnly:=ReadOnly;

    ChangedRow(Self,nil);

    CheckSplitter;
  end
  else
    HideShowItems;
end;

procedure TBIGrid.SetShowItems(const Value: TGridShowItems);
begin
  if FShowItems<>Value then
  begin
    FShowItems:=Value;
    TryShowItems;
  end;
end;

procedure TBIGrid.SetTotals(const Value: Boolean);
begin
  IPlugin.Totals:=Value;
end;

function TBIGrid.HasSubItem: Boolean;
begin
  result:=(IPluginRight<>nil) and (IDataSet<>nil) and (IDataSet.RecNo>0);
end;

function TBIGrid.SubGrid: TBIGridPlugin;
begin
  result:=IPluginRight;
end;

function TBIGrid.SubItem: TDataItem;
begin
  if HasSubItem and (IDataSetRight<>nil) then
     result:=IDataSetRight.Data
  else
     result:=nil;
end;

procedure TBIGrid.Duplicates(const AData:TDataItem; const Hide:Boolean);
begin
  IPlugin.Duplicates(AData,Hide);
end;

{ TUICommon }

class procedure TUICommon.AddForm(const AForm: TCustomForm; const AParent: TWinControl);
begin
  AForm.Align:=alClient;
  AForm.Parent:=AParent;
  AForm.BorderStyle:=bsNone;
  AForm.Show;
end;

const
  EditorsKey='Editors';

class procedure TUICommon.LoadPosition(const AForm: TCustomForm; const Parent,Key: String);

  function IsCentered(const APosition:TPosition):Boolean;
  begin
    result:=APosition in [TPosition.poScreenCenter,
                          TPosition.poDesktopCenter,
                          TPosition.poMainFormCenter,
                          TPosition.poOwnerFormCenter];
  end;

  function ReadNumber(const AItem:String; const ADefault:Integer):Integer;
  begin
    result:=TBIRegistry.ReadInteger(Parent+'\'+Key,AItem,ADefault);
  end;

begin
  if (AForm.Parent=nil) and TBIRegistry.Exists(Parent+'\'+Key,'Left') then
  begin
    AForm.Left:=ReadNumber('Left',AForm.Left);
    AForm.Width:=ReadNumber('Width',AForm.Width);

    if AForm.Left>Screen.Width then
       AForm.Left:=Screen.Width-AForm.Width;

    if AForm.Left<0 then
       AForm.Left:=0;

    AForm.Top:=ReadNumber('Top',AForm.Top);
    AForm.Height:=ReadNumber('Height',AForm.Height);

    if AForm.Top>Screen.Height then
       AForm.Top:=Screen.Height-AForm.Height;

    if AForm.Top<0 then
       AForm.Top:=0;

    if TBIRegistry.ReadBoolean(Parent+'\'+Key,'Maximized',False) then
       AForm.WindowState:=TWindowState.wsMaximized
    else
    begin
      AForm.WindowState:=TWindowState.wsNormal;

      if (AForm is TForm) and (AForm.Parent<>nil) then
         if IsCentered(TForm(AForm).Position) then
            TForm(AForm).Position:=TPosition.poDesigned;
    end;
  end;
end;

class procedure TUICommon.LoadPosition(const AForm: TCustomForm; const Key: String);
begin
  LoadPosition(AForm,EditorsKey,Key);
end;

class function TUICommon.Point(const X,Y:Integer):TPoint;
begin
  {$IFDEF FPC}
  result.X:=X;
  result.Y:=Y;
  {$ELSE}
  result:=TPoint.Create(X,Y);
  {$ENDIF}
end;

class procedure TUICommon.SavePosition(const AForm: TCustomForm; const Parent,Key: String);
var tmp : String;
begin
  if AForm.Parent=nil then
  begin
    tmp:=Parent+'\'+Key;

    TBIRegistry.WriteInteger(tmp,'Left',AForm.Left);
    TBIRegistry.WriteInteger(tmp,'Top',AForm.Top);
    TBIRegistry.WriteInteger(tmp,'Width',AForm.Width);
    TBIRegistry.WriteInteger(tmp,'Height',AForm.Height);
    TBIRegistry.WriteBoolean(tmp,'Maximized',AForm.WindowState=TWindowState.wsMaximized);
  end;
end;

class procedure TUICommon.SavePosition(const AForm: TCustomForm; const Key: String);
begin
  SavePosition(AForm,EditorsKey,Key);
end;

class function TUICommon.SelectFolder(var AFolder: String): Boolean;
{$IFNDEF FPC}
const
  // Options to enable select folder edit box and "make new folder" button
  Options : TSelectDirExtOpts=[sdNewFolder, sdShowEdit, sdShowShares, sdNewUI, sdValidateDir];
{$ENDIF}
begin
  {$IFDEF FPC}
  {$DEFINE HAS_SELECT}
  {$ELSE}
  {$IF CompilerVersion>18}
  {$DEFINE HAS_SELECT}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HAS_SELECT}
  result:=SelectDirectory(BIMsg_ChooseFolder,'',AFolder {$IFNDEF FPC},Options{$ENDIF});
  {$ELSE}
  result:=False; // <-- Pending
  {$ENDIF}
end;

class procedure TUICommon.ShowUnique(const ATab: TTabSheet);
var t : Integer;
    tmp : TPageControl;
begin
  tmp:=ATab.PageControl;

  tmp.Visible:=True;

  tmp.ActivePage:=ATab;

  for t:=0 to tmp.PageCount-1 do
      tmp.Pages[t].TabVisible:=tmp.Pages[t]=ATab;
end;

class function TUICommon.YesNo(const Message:String):Boolean;
begin
  result:=MessageDlg(Message, TMsgDlgType.mtConfirmation, mbYesNo, 0) = {$IFNDEF FPC}System.UITypes.{$ENDIF}mrYes;
end;

class function TUICommon.AutoTest:Boolean;
begin
  result:=(ParamCount>0) and SameText(ParamStr(1),'TEST');
end;

class function TUICommon.EditColor(const AOwner:TComponent; const AColor:TColor; out ANew:TColor):Boolean;
var c : TColorDialog;
begin
  c:=TColorDialog.Create(AOwner);
  try
    ANew:=AColor;
    c.Color:=AColor;

    {$IFNDEF FPC}
    c.Options:=[TColorDialogOption.cdFullOpen];
    {$ENDIF}

    result:=c.Execute;

    if result then
       ANew:=c.Color;
  finally
    c.Free;
  end;
end;

class procedure TUICommon.GotoURL(const AOwner:TWinControl; const AURL:String);
var tmp : UIntPtr;
begin
  if AOwner=nil then
     tmp:=0
  else
     tmp:=AOwner.Handle;

  ShellExecute(tmp,'open',PWideChar(AURL),nil,nil,3 {SW_SHOWMAXIMIZED});
end;

class function TUICommon.Input(const ATitle,ACaption,ADefault: String;
                               out ANew: String): Boolean;
begin
  ANew:=ADefault;
  result:=InputQuery(ATitle,ACaption,ANew);
end;

{ TRowNumbers }

procedure TRowNumbers.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    TPluginAccess(IGrid.Plugin).SetRowNumber(Enabled);
  end;
end;

{ TGridFilters }

procedure TGridFilters.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    TPluginAccess(IGrid.Plugin).SetFilters(Enabled);
  end;
end;

{ TGridSearch }

procedure TGridSearch.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    TPluginAccess(IGrid.Plugin).SetSearch(Enabled);
  end;
end;

procedure InternalShowMessage(const AText:String);
begin
  ShowMessage(AText);
end;

initialization
  TCommonUI.ShowMessage:=InternalShowMessage;
end.
