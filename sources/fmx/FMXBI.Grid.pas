{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid control for FireMonkey             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid;

interface

uses
  System.Classes, System.UITypes,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts, FMX.Menus, FMX.TabControl,

  FMXBI.DataControl,

  Data.DB, BI.DataSet, BI.DataItem, BI.DataSource, BI.UI, BI.Grid.Plugin;

type
  TGridShowItems=(Automatic, Yes, No); //, SubTables);

  // Generic Grid control that "links" a TDataItem with a Grid.
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  TBIGrid = class(TBIDataControl)
  private
    FAlternate : TAlternateColor;
    FOnDataChange : TNotifyEvent;
    FShowItems : TGridShowItems;

    IDataSet : TBIDataSet;
    IPlugin : TBIGridPlugin;

    IDataSetRight : TBIDataset;
    IPluginRight : TBIGridPlugin;

    procedure ControlDblClick(Sender: TObject);
    procedure CopyTable(Sender:TObject);
    function CreatePopup(const AOwner:TBIGridPlugin):TPopupMenu;
    function GetCurrentRow: Integer;
    function GetReadOnly: Boolean;
    function HasSubItem: Boolean;
    procedure HideShowItems;
    function PluginControl:TControl;
    procedure RowChanged(Sender: TObject);

    procedure SetAlternate(const Value: TAlternateColor);
    procedure SetPlugin(const Value: TBIGridPlugin);
    procedure SetDataSet(const Value: TBIDataSet);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetShowItems(const Value: TGridShowItems);
    procedure TryShowItems;
  protected
    procedure SetDataDirect(const Value: TDataItem); override;
    procedure UpdatedDataValues; override;
  public
    class var
      Engine : TBIGridPluginClass;

    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure BindTo(const AData: TDataItem);

    procedure Colorize; overload;
    procedure Colorize(const AItems:TDataColorizers); overload;

    procedure Duplicates(const AData:TDataItem; const Hide:Boolean);

    class function Embedd(const AOwner:TComponent; const AParent:TFmxObject):TBIGrid; static;

    property DataSet:TBIDataSet read IDataSet write SetDataSet;
    property Plugin:TBIGridPlugin read IPlugin write SetPlugin;
    property CurrentRow:Integer read GetCurrentRow;
  published
    property Alternate:TAlternateColor read FAlternate write SetAlternate;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property ShowItems:TGridShowItems read FShowItems write SetShowItems default TGridShowItems.Automatic;

    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;
  end;

  {$IF CompilerVersion<27} // Cannot use FireMonkeyVersion<21 (or 21_0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  // Helper methods for Firemonkey:
  TUICommon=record
  public
    const
      AlignNone=TAlignLayout.{$IFDEF HASFMX20}alNone{$ELSE}None{$ENDIF};
      AlignClient=TAlignLayout.{$IFDEF HASFMX20}alClient{$ELSE}Client{$ENDIF};
      AlignLeft=TAlignLayout.{$IFDEF HASFMX20}alLeft{$ELSE}Left{$ENDIF};
      AlignTop=TAlignLayout.{$IFDEF HASFMX20}alTop{$ELSE}Top{$ENDIF};
      AlignRight=TAlignLayout.{$IFDEF HASFMX20}alRight{$ELSE}Right{$ENDIF};
      AlignBottom=TAlignLayout.{$IFDEF HASFMX20}alBottom{$ELSE}Bottom{$ENDIF};

    class procedure AddForm(const AForm: TCommonCustomForm; const AParent: TFmxObject); static;
    class function AddNewTab(const AForm: TCommonCustomForm; const AParent: TTabControl):TTabItem; static;
    class function AutoTest:Boolean; static;
    class procedure CopyToClipboard(const AText:String); static;
    class procedure GotoURL(const AOwner:TControl; const AURL:String); static;
    class function Input(const ATitle,ACaption,ADefault:String; out AValue:String):Boolean; static;
    class procedure LoadPosition(const AForm:TCommonCustomForm; const Parent,Key:String); overload; static;
    class procedure LoadPosition(const AForm:TCommonCustomForm; const Key:String); overload; static;
    class procedure Popup(const APopup:TPopupMenu; const AControl:TControl); static;
    class procedure SavePosition(const AForm:TCommonCustomForm; const Parent,Key:String); overload; static;
    class procedure SavePosition(const AForm:TCommonCustomForm; const Key:String); overload; static;
    class function SelectFolder(var AFolder: String): Boolean; static;
    class function YesNo(const AMessage:String):Boolean; static;
  end;

implementation

{$IFNDEF FPC}
{$IF CompilerVersion>=31}
{$DEFINE FMXDialogs}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  Winapi.ShellApi,
  {$ENDIF}

  System.Types, System.SysUtils, FMX.Dialogs, FMX.Platform, FMX.StdCtrls,

  FMXBI.Grid.Grid, // <-- default grid plugin

  BI.Persist, BI.DB.DataSet, BI.Languages.English, BI.Arrays,
  BI.Store.Component, BI.CSV;

type
  TPluginAccess=class(TBIGridPlugin);

{ TBIGrid }

Constructor TBIGrid.Create(AOwner: TComponent);
begin
  inherited;

  if TBIGrid.Engine=nil then
     raise EBIException.Create(BIMsg_Grid_MissingEngine);

  IPlugin:=TBIGrid.Engine.Create(Self);

  FAlternate:=TAlternateColor.Create;
  TPluginAccess(IPlugin).IAlternate:=FAlternate;
  FAlternate.OnChange:=TPluginAccess(IPlugin).ChangedAlternate;

  {$IF CompilerVersion>27}
  SetSize(400,250);
  {$ELSE}
  Width:=400;
  Height:=250;
  {$ENDIF}

  TPluginAccess(IPlugin).SetOnRowChanged(RowChanged);

  if PluginControl<>nil then
     PluginControl.OnDblClick:=ControlDblClick;

  TPluginAccess(IPlugin).SetPopup(CreatePopup(IPlugin));
end;

{$IFNDEF AUTOREFCOUNT}
Destructor TBIGrid.Destroy;
begin
  FAlternate.Free;

  IPluginRight.Free;
  IPlugin.Free;

  inherited;
end;
{$ENDIF}

procedure TBIGrid.Colorize;
var tmp : TDataColorizers;
    tmpItem : TDataItem;
begin
  tmp:=nil;

  if Data<>nil then
     for tmpItem in Data.Items.AsArray do
         if tmpItem.Kind.IsNumeric or (tmpItem.Kind=TDataKind.dkDateTime) then
            tmp.Add(tmpItem);

  Colorize(tmp);
end;

function DataSetOf(const AObject:TObject):TDataSet;
begin
  if (AObject is TMenuItem) and (TMenuItem(AObject).TagObject is TBIGridPlugin) then
     result:=(TMenuItem(AObject).TagObject as TBIGridPlugin).DataSource.DataSet
  else
     result:=nil
end;

procedure TBIGrid.CopyTable(Sender:TObject);
var tmp : TDataSet;
begin
  tmp:=DataSetOf(Sender);

  if tmp is TBIDataset then
     TUICommon.CopyToClipboard(TBICSVExport.AsString(TBIDataSet(tmp).Data));
end;

function TBIGrid.CreatePopup(const AOwner:TBIGridPlugin):TPopupMenu;

  function NewItem(const AText:String; const AEvent:TNotifyEvent):TMenuItem;
  begin
    result:=TMenuItem.Create(Self);
    result.Text:=AText;
    result.TagObject:=AOwner;
    result.OnClick:=AEvent;
  end;

var tmp : TMenuItem;
begin
  result:=TPopupMenu.Create(Self);

  tmp:=NewItem('Copy',nil);
  tmp.AddObject(NewItem('Table',CopyTable));

  tmp.Parent:=result;
end;

function TBIGrid.HasSubItem: Boolean;
begin
  result:=(IPluginRight<>nil) and (IDataSet<>nil) and (IDataSet.RecNo>0);
end;

procedure TBIGrid.RowChanged(Sender: TObject);
begin
  if HasSubItem then
  begin
    if IDataSetRight=nil then
       IDataSetRight:=TBIDataset.Create(Self);

    IDataSetRight.Close;

    IDataSetRight.Data:=nil; // <-- set to nil first, to force clear of IDataSet.Cursor.SortBy
    IDataSetRight.Data:=Data.Items[IDataSet.RecNo-1];

    IPluginRight.BindTo(IDataSetRight);

    if IDataSetRight.Data<>nil then
       IDataSetRight.Open;
  end;

  if Assigned(FOnDataChange) then
     FOnDataChange(Self);
end;

procedure TBIGrid.Colorize(const AItems: TDataColorizers);
begin
  IPlugin.Colorize(AItems);
end;

procedure TBIGrid.SetAlternate(const Value: TAlternateColor);
begin
  FAlternate.Assign(Value);
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

    //DataSource.OnDataChange:=RowChanged;

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

procedure TBIGrid.SetDataSet(const Value: TBIDataSet);
begin
  if IDataSet<>Value then
  begin
    IDataSet.Free;
    IDataSet:=Value;
  end;

  IPlugin.BindTo(IDataSet);

  if IDataSet<>nil then
     if IDataSet.Data<>nil then
        IDataSet.Open;
end;

procedure TBIGrid.SetPlugin(const Value: TBIGridPlugin);
var tmp : TDataSource;
begin
  if IPlugin<>Value then
  begin
    // Try to obtain "IDataSet" from current plugin:
    if IDataSet=nil then
       if IPlugin<>nil then
       begin
         tmp:=IPlugin.DataSource;

         if tmp<>nil then
            if tmp.DataSet is TBIDataset then
               IDataSet:=tmp.DataSet as TBIDataset;
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

end;

procedure TBIGrid.SetShowItems(const Value: TGridShowItems);
begin
  if FShowItems<>Value then
  begin
    FShowItems:=Value;
    TryShowItems;
  end;
end;

procedure TBIGrid.TryShowItems;

  function ExistsSplitter:TSplitter;
  var t : Integer;
  begin
    for t:=0 to ControlsCount-1 do
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
       tmp.Position.X:=tmpControl.Width+1;
  end;

  procedure RelayoutGrids;
  var tmpControl,
      tmpRight : TControl;
      tmpW : Single;
  begin
    tmpControl:=PluginControl;

    if tmpControl<>nil then
    begin
      tmpW:=Width*0.5;

      tmpControl.Align:=TUICommon.AlignLeft;

      TPluginAccess(IPlugin).AutoWidth;

      if tmpControl.Width>tmpW then
         tmpControl.Width:=tmpW;
    end;

    tmpRight:=IPluginRight.GetControl as TControl;
    tmpRight.Align:=TUICommon.AlignClient;
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
    begin
      IPluginRight:=TBIGrid.Engine.Create(Self);
      TPluginAccess(IPluginRight).SetPopup(CreatePopup(IPluginRight));
    end;

    ReLayoutGrids;

    // nil: IPluginRight.IAlternate.Enabled:=Plugin.IAlternate.Enabled;
    IPluginRight.ReadOnly:=ReadOnly;

    RowChanged(Self);

    CheckSplitter;
  end
  else
    HideShowItems;
end;

procedure TBIGrid.UpdatedDataValues;
begin
  inherited;
  IDataSet.Refresh;
end;

procedure TBIGrid.BindTo(const AData: TDataItem);
begin
  Data:=AData;
end;

procedure TBIGrid.Duplicates(const AData:TDataItem; const Hide:Boolean);
var tmp : TField;
begin
  if IDataSet<>nil then
     tmp:=TBIDataSetSource.FieldOf(AData,IDataSet)
  else
     tmp:=nil;

  if tmp<>nil then
  begin
    (IDataSet as TBIDataSet).SetFieldOnGetText(tmp,Hide);
    Repaint;
  end;
end;

class function TBIGrid.Embedd(const AOwner: TComponent;
  const AParent: TFmxObject): TBIGrid;
begin
  result:=TBIGrid.Create(AOwner);
  result.Align:=TUICommon.AlignClient;
  result.Parent:=AParent;
end;

procedure TBIGrid.ControlDblClick(Sender: TObject);
begin
  DblClick;
end;

function TBIGrid.GetCurrentRow: Integer;
begin
  if IDataSet<>nil then
     result:=IDataSet.RecNo-1
  else
     result:=-1;
end;

function TBIGrid.GetReadOnly: Boolean;
begin
  result:=IPlugin.ReadOnly;
end;

procedure TBIGrid.HideShowItems;

  procedure ResetMainGridClient;
  var tmp,
      tmpParent : TControl;
  begin
    tmp:=PluginControl;

    if tmp<>nil then
    begin
      tmpParent:=tmp.ParentControl;

      // Warning: This is different than VCL.
      // In VCL, tmpParent is a TPanel that is used for both the main grid
      // and the (optional) "Detail" grid
      if tmpParent<>Self then
         if tmpParent<>nil then
            if tmpParent.Align<>TUICommon.AlignClient then
               tmpParent.Align:=TUICommon.AlignClient;
    end;
  end;

begin
  // Destroy right-side grid

  if IPluginRight<>nil then
  begin
    //DataSource.OnDataChange:=nil;

    IPluginRight.Free;
    IPluginRight:=nil;
  end;

  // Reset main grid alignment to client
  ResetMainGridClient;
end;

function TBIGrid.PluginControl: TControl;
begin
  result:=IPlugin.GetControl as TControl;
end;

{ TUICommon }

class procedure TUICommon.AddForm(const AForm: TCommonCustomForm; const AParent: TFmxObject);
begin
  AForm.Visible:=False;

  while AForm.ChildrenCount>0 do
        AForm.Children[0].Parent:=AParent;
end;

function GetScreenSize:TPointF;
{$IF CompilerVersion<=27}
var ScreenSvc : IFMXScreenService;
{$ENDIF}
begin
  {$IF CompilerVersion>27}
  result.X:=Screen.Width;
  result.Y:=Screen.Height;
  {$ELSE}
  {$IF CompilerVersion>20}
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
     result:=ScreenSvc.GetScreenSize
  else
  begin
    result.X:=0;
    result.Y:=0;
  end;
  {$ELSE}
  result:=Platform.GetScreenSize;
  {$ENDIF}
  {$ENDIF}
end;

const
  EditorsKey='Editors';

class function TUICommon.Input(const ATitle, ACaption, ADefault: String; out AValue: String): Boolean;
{$IFDEF FMXDialogs}
var DialogSvc: IFMXDialogServiceSync;
    tmp : Array[0..0] of String;
{$ENDIF}
begin
  {$IFDEF FMXDialogs}
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogServiceSync, DialogSvc) then
  begin
    tmp[0]:=AValue;

    result:=DialogSvc.InputQuerySync(ATitle,[ACaption],tmp);

    if result then
       AValue:=tmp[0];
  end
  else
     result:=False;
  {$ELSE}
  result:=InputQuery(ATitle,ACaption,AValue);
  {$ENDIF}
end;

class function TUICommon.AddNewTab(const AForm: TCommonCustomForm;
  const AParent: TTabControl): TTabItem;
begin
  result:=TTabItem.Create(AParent.Owner);
  result.Parent:=AParent;

  result.Text:=AForm.Caption;

  TUICommon.AddForm(AForm,result);
end;

class function TUICommon.AutoTest:Boolean;
begin
  result:=(ParamCount>0) and SameText(ParamStr(1),'TEST');
end;

class procedure TUICommon.CopyToClipboard(const AText: String);
var ClipService : IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(ClipService)) then
     ClipService.SetClipboard(AText);
end;

class procedure TUICommon.GotoURL(const AOwner: TControl; const AURL: String);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0,'open',PWideChar(AURL),nil,nil,3 {SW_SHOWMAXIMIZED});
  {$ENDIF}
end;

class procedure TUICommon.LoadPosition(const AForm: TCommonCustomForm;
  const Parent,Key: String);

  function ReadNumber(const AItem:String; const ADefault:Integer):Integer;
  begin
    result:=TBIRegistry.ReadInteger(Parent+'\'+Key,AItem,ADefault);
  end;

  procedure CheckFormBounds;
  var P : TPoint;
  begin
    P:=GetScreenSize.Round;

    if AForm.Left>P.X then
       AForm.Left:=P.X-AForm.Width;

    if AForm.Left<0 then
       AForm.Left:=0;

    if AForm.Top>P.Y then
       AForm.Top:=P.Y-AForm.Height;

    if AForm.Top<0 then
       AForm.Top:=0;
  end;

begin
  AForm.Left:=ReadNumber('Left',AForm.Left);
  AForm.Top:=ReadNumber('Top',AForm.Top);
  AForm.Width:=ReadNumber('Width',AForm.Width);
  AForm.Height:=ReadNumber('Height',AForm.Height);

  CheckFormBounds;

  if TBIRegistry.ReadBoolean(Parent+'\'+Key,'Maximized',False) then
     AForm.WindowState:=TWindowState.wsMaximized;
end;

class procedure TUICommon.LoadPosition(const AForm: TCommonCustomForm;
  const Key: String);
begin
  LoadPosition(AForm,EditorsKey,Key);
end;

class procedure TUICommon.Popup(const APopup: TPopupMenu; const AControl: TControl);

  function FormOf(AControl:TFmxObject): TCommonCustomForm;
  begin
    repeat
      if AControl is TCommonCustomForm then
         Exit(TCommonCustomForm(AControl))
      else
         AControl:=AControl.Parent;
    until AControl=nil;

    result:=nil;
  end;

  function PopupPos:TPointF;
  begin
    result.X:=AControl.AbsoluteRect.Left;
    result.Y:=AControl.AbsoluteRect.Bottom;
    result:=FormOf(AControl).ClientToScreen(result);
  end;

var P : TPointF;
begin
  P:=PopupPos;
  APopup.Popup(P.X,P.Y);
end;

class procedure TUICommon.SavePosition(const AForm: TCommonCustomForm;
  const Parent,Key: String);
var tmp : String;
begin
  tmp:=Parent+'\'+Key;

  TBIRegistry.WriteInteger(tmp,'Left',AForm.Left);
  TBIRegistry.WriteInteger(tmp,'Top',AForm.Top);
  TBIRegistry.WriteInteger(tmp,'Width',AForm.Width);
  TBIRegistry.WriteInteger(tmp,'Height',AForm.Height);
  TBIRegistry.WriteBoolean(tmp,'Maximized',AForm.WindowState=TWindowState.wsMaximized);
end;

class procedure TUICommon.SavePosition(const AForm: TCommonCustomForm;
  const Key: String);
begin
  SavePosition(AForm,EditorsKey,Key);
end;

class function TUICommon.SelectFolder(var AFolder: String): Boolean;
begin
  {$IF CompilerVersion>26}
  result:=SelectDirectory(BIMsg_ChooseFolder,AFolder,AFolder);
  {$ELSE}
  result:=False; // <-- Pending XE4,5
  {$ENDIF}
end;

class function TUICommon.YesNo(const AMessage:String):Boolean;
{$IFDEF FMXDialogs}
var DialogSvc: IFMXDialogServiceSync;
{$ENDIF}
begin
  {$IFDEF FMXDialogs}
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogServiceSync, DialogSvc) then
     result:=DialogSvc.MessageDialogSync(AMessage,TMsgDlgType.mtConfirmation,mbYesNo,TMsgDlgBtn.mbYes,-1)=mrYes
  else
     result:=False;

  {$ELSE}
  result:=MessageDlg(AMessage, TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes;
  {$ENDIF}
end;

procedure InternalShowMessage(const AText:String);
{$IFDEF FMXDialogs}
var DialogSvc: IFMXDialogServiceSync;
{$ENDIF}
begin
  {$IFDEF FMXDialogs}
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogServiceSync, DialogSvc) then
     DialogSvc.ShowMessageSync(AText);
  {$ELSE}
  ShowMessage(AText);
  {$ENDIF}
end;

{ TBIGridPlugin }

initialization
  TCommonUI.ShowMessage:=InternalShowMessage;
end.
