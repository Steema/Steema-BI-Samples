{*********************************************}
{  TeeBI Software Library                     }
{  Data Manager dialog                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.DataManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, 

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation, FMX.EditBox, FMX.NumberBox,
  {$ENDIF}

  {$IF CompilerVersion<=28}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.ScrollBox,
  {$ENDIF}

  {$IF CompilerVersion<=29}
  {$DEFINE HASFMX23}
  {$ENDIF}

  {$IFNDEF HASFMX23}
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  {$ENDIF}

  FMX.Dialogs, FMX.ListView.Types, FMX.ListView, FMX.StdCtrls,
  FMX.Layouts, BI.Persist, FMX.ListBox, FMX.TabControl, FMXBI.Editor.Data,
  FMX.Edit, FMX.Menus, FMX.Memo, BI.DataItem;

type
  TDataManagerEmbedMode=(Choose,Edit);

  TDataManager = class(TForm)
    LayoutStore: TLayout;
    LayoutSources: TLayout;
    Layout3: TLayout;
    Label1: TLabel;
    LayoutAddDelete: TLayout;
    BAdd: TSpeedButton;
    LSources: TListView;
    LStore: TLabel;
    LayoutButtons: TLayout;
    LayoutOkCancel: TLayout;
    BSelect: TButton;
    BCancel: TButton;
    TabControl1: TTabControl;
    TabSettings: TTabItem;
    Splitter1: TSplitter;
    TabSchedule: TTabItem;
    CBRefresh: TCheckBox;
    Label3: TLabel;
    RefreshUnit: TComboBox;
    Period: TNumberBox;
    CBKeep: TCheckBox;
    Label4: TLabel;
    NumberBox1: TNumberBox;
    Label5: TLabel;
    PopupKinds: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    TabData: TTabItem;
    MemoImportLog: TMemo;
    ListData: TListBox;
    Label7: TLabel;
    LNextRefresh: TLabel;
    MenuItem1: TMenuItem;
    CBStores: TComboBox;
    Layout1: TLayout;
    Label6: TLabel;
    LLastImport: TLabel;
    MemoDataInfo: TMemo;
    BImportNow: TButton;
    BViewData: TButton;
    CBParallel: TCheckBox;
    CBStopOnErrors: TCheckBox;
    ImportProgress: TProgressBar;
    Button1: TButton;
    BDelete: TSpeedButton;
    BChange: TButton;
    BRename: TButton;
    MenuItem4: TMenuItem;
    procedure BChangeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LSourcesChange(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BSelectClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure TabControl1Resize(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure BImportNowClick(Sender: TObject);
    procedure BViewDataClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure ListDataChange(Sender: TObject);
    procedure CBRefreshChange(Sender: TObject);
    procedure PeriodChange(Sender: TObject);
    procedure RefreshUnitChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBParallelChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBStoresChange(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { Private declarations }

    FOnSelect : TNotifyEvent;

    ICurrent : TDataItem;
    IEditor : TDataEditor;

    IStore : String;
    ChoosingData : Boolean;

    IChanging : Boolean;

    procedure AddStores;
    function AskName:String;
    function Current:String;
    function CurrentData:TDataItem; // ???
    function CurrentSource:Integer;
    function CurrentStore:String;
    procedure FillData;
    procedure ImportingData(const Sender:TObject; const Percent:Single; var Cancel:Boolean);
    function ImportingError(const Sender:TObject; const Text:String):Boolean;
    procedure LogException(const Text:String);
    procedure ReFill;
    procedure SetLastImport;
    procedure SetScheduling;
    procedure SetSourcesWidth;
    procedure TryAdd(const Kind:TDataDefinitionKind);

    Constructor CreateStore(const AOwner:TComponent; const AStore:String);

    class function CreateChooser(const AOwner: TComponent; const AStore:String=''): TDataManager; static;
  public
    { Public declarations }

    function Selected:TDataItem;

    class function ChooseName(const AOwner:TComponent; const AStore:String=''):String; static;
    class function ChooseData(const AOwner:TComponent; const AStore:String='';
                              const ACurrent:TDataItem=nil):TDataItem; static;
    class procedure Edit(const AOwner:TComponent; const AStore:String=''); static;
    class function Embed(const AOwner:TComponent;
                         const AParent:TControl;
                         const AMode:TDataManagerEmbedMode=TDataManagerEmbedMode.Choose;
                         const AStore:String=''):TDataManager; static;

    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation

{$R *.fmx}

uses
  FMXBI.Grid, System.Diagnostics, System.IOUtils,
  BI.UI, FMXBI.DataViewer, BI.DataSource, FMXBI.Status, FMXBI.Editor.Stores,
  BI.Languages.English, BI.Arrays;

Constructor TDataManager.CreateStore(const AOwner: TComponent; const AStore: String);
begin
  inherited Create(AOwner);
  IStore:=AStore;
end;

procedure TDataManager.BAddClick(Sender: TObject);
begin
  TUICommon.Popup(PopupKinds,BAdd);
end;

function TDataManager.AskName:String;
begin
  result:='';

  repeat
    if TUICommon.Input(BIMsg_NewDataSource,BIMsg_Name,'',result) then
    begin
      result:=Trim(result);

      if result<>'' then
         Exit;
    end
    else
      break;

  until False;
end;

procedure TDataManager.BDeleteClick(Sender: TObject);
begin
  if TUICommon.YesNo(Format(BIMsg_Store_SureToDelete,[Current])) then
  begin
    TStore.RemoveData(IStore,Current);
    LSources.Items.Delete(LSources.ItemIndex);
    LSourcesChange(Self);
  end;
end;

procedure TDataManager.BSelectClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TDataManager.Button1Click(Sender: TObject);
begin
  TStoreStatus.View(Self);
end;

procedure TDataManager.BChangeClick(Sender: TObject);
begin
  TStoreEditor.Edit(Self);
  IStore:='';
  AddStores;
end;

procedure TDataManager.SetLastImport;

  procedure ShowDataInfo(const Data:TDataItem);
  begin
    TCommonUI.AddInfo(Data,MemoDataInfo.Lines);
  end;

var tmp,
    tmpData : String;
    tmpSize : Int64;
    Data : TDataItem;
begin
  MemoDataInfo.Lines.Clear;

  tmp:=IEditor.Data.LastImport;

  if (tmp<>'') and TFile.Exists(tmp) then
  begin
    LLastImport.Text:=DateTimeToStr(TFile.GetLastWriteTime(tmp));

    tmpSize:=TBIFileSource.GetFileSize(tmp);

    tmpData:=TPath.ChangeExtension(tmp,TDataPersistence.Extension);

    try
      if FileExists(tmpData) then
         tmpSize:=tmpSize+TBIFileSource.GetFileSize(tmpData);

      MemoDataInfo.Lines.Add('Size on disk: '+TCommonUI.BytesToString(tmpSize));

      Data:=TPersistence.Load(tmp);
      try
        ShowDataInfo(Data);
      finally
        Data.Free;
      end;
    except
      on E:Exception do
      begin
        MemoDataInfo.Lines.Add(Format(BIMsg_Store_DataLoadError,[IEditor.Data.Name,IStore]));
        MemoDataInfo.Lines.Add(E.Message);
      end;

    end;

    BViewData.Enabled:=True;
  end
  else
  begin
    LLastImport.Text:='(unknown)';

    Data:=TStore.Load(CurrentStore,Current,function(const Sender:TObject; const Text:String):Boolean
      begin
        MemoDataInfo.Lines.Clear;
        MemoDataInfo.Lines.Add('Error: '+Text);

        result:=True;
      end);

    BViewData.Enabled:=Data<>nil;

    if Data<>nil then
       ShowDataInfo(Data);
  end;
end;

procedure TDataManager.ImportingData(const Sender:TObject; const Percent:Single; var Cancel:Boolean);
begin
  ImportProgress.Value:=Percent;
  Application.ProcessMessages; // <-- Pending: Remove
end;

procedure TDataManager.LogException(const Text:String);
begin
  MemoImportLog.Lines.Add(Format(BIMsg_ImportError,[Current]));
  MemoImportLog.Lines.Add('Error: '+Text);
end;

function TDataManager.ImportingError(const Sender:TObject; const Text:String):Boolean;
begin
  LogException(Text);
//  Application.ProcessMessages;

  if CBStopOnErrors.IsChecked then
     raise EBIException.Create(Text)
  else
     result:=True;
end;

procedure TDataManager.BImportNowClick(Sender: TObject);
var t1 : TStopwatch;
    Data : TDataArray;
    tmpFileName : String;
    tmp: TDataItem;
begin
  MemoImportLog.Lines.Clear;

  Data:=nil;

  t1:=TStopwatch.StartNew;
  try
    IEditor.Data.OnImporting:=ImportingData;
    IEditor.Data.OnError:=ImportingError;

    ImportProgress.Value:=0;
    ImportProgress.Visible:=True;
    try
      Data:=IEditor.Data.Import(CurrentStore);
    finally
      ImportProgress.Visible:=False;
      ImportProgress.Value:=0;
    end;

  except
    on E:Exception do
       LogException(E.Message);
  end;

  MemoImportLog.Lines.Add('Import time: '+IntToStr(t1.ElapsedMilliseconds)+' msec');

  if Data<>nil then
  begin
    t1:=TStopwatch.StartNew;

    tmpFileName:=TStore.FullPath(CurrentStore,IEditor.Data.Name);

    TStore.UnLoad(CurrentStore,IEditor.Data.Name);

    tmp:=TDataItem.Create(Data);
    try
      tmp.Name:=IEditor.Data.Name;
      TStore.Save(tmp,tmpFileName);
    finally
      tmp.Free;
    end;

    MemoImportLog.Lines.Add('Saving time: '+IntToStr(t1.ElapsedMilliseconds)+' msec');

    SetLastImport;
  end
  else
    MemoImportLog.Lines.Add('Error: Empty result');

  MemoImportLog.Visible:=True;
end;

procedure TDataManager.BRenameClick(Sender: TObject);
var Old,tmp : String;
begin
  tmp:=Current;
  Old:=tmp;

  if TUICommon.Input(BIMsg_Data_ChangeName,BIMsg_NewName,Old,tmp) then
  begin
    if not SameText(tmp,Old) then
    begin
      // Pending: Check new name "tmp" does not exist already !
      TStore.Rename(IStore,Old,tmp);
      LSources.Items[LSources.ItemIndex].Text:=tmp;
    end;
  end;
end;

procedure TDataManager.BCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TDataManager.BViewDataClick(Sender: TObject);
var Data : TDataItem;
begin
  Data:=TStore.Load(CurrentStore,Current);

  if Data<>nil then
     TDataViewer.View(Self,Data);
end;

function TDataManager.Current:String;
begin
  result:=LSources.Items[LSources.ItemIndex].Text;
end;

function TDataManager.CurrentData:TDataItem;
begin
  if LSources.ItemIndex=-1 then
     result:=nil
  else
     result:=TStore.Load(IStore,Current);
end;

function TDataManager.CurrentStore: String;
begin
  if CBStores.ItemIndex=-1 then
     result:=''
  else
     result:=CBStores.Items[CBStores.ItemIndex];
end;

function TDataManager.Selected: TDataItem;
var tmpData : TDataItem;
begin
  tmpData:=CurrentData;

  if tmpData=nil then
     result:=nil
  else
  if ListData.ItemIndex<>-1 then
     result:=tmpData.Item[ListData.Items[ListData.ItemIndex]]
  else
     result:=tmpData;
end;

class procedure TDataManager.Edit(const AOwner: TComponent; const AStore:String='');
begin
  with TDataManager.CreateStore(AOwner,AStore) do
  try
    BSelect.Visible:=False;
    BCancel.Text:='Close';

    ShowModal;
  finally
    Free;
  end;
end;

class function TDataManager.Embed(const AOwner: TComponent;
                                  const AParent: TControl;
                                  const AMode:TDataManagerEmbedMode;
                                  const AStore: String): TDataManager;
begin
  if AMode=TDataManagerEmbedMode.Choose then
  begin
    result:=CreateChooser(AOwner,AStore);
    result.LayoutButtons.Visible:=False;

    result.TabControl1.Visible:=False;
  end
  else
  begin
    result:=TDataManager.CreateStore(AOwner,AStore);
    result.LayoutOkCancel.Visible:=False;
  end;

  result.FormShow(result);

  TUICommon.AddForm(result,AParent);

  if AMode=TDataManagerEmbedMode.Choose then
  begin
    result.LayoutSources.Width:=AParent.Width*0.5;
    result.ListData.Align:=TUICommon.AlignClient;
  end
  else
     result.SetSourcesWidth;
end;

procedure TDataManager.CBParallelChange(Sender: TObject);
begin
  if not IChanging then
     IEditor.Data['Parallel']:=BoolToStr(CBParallel.IsChecked,True);
end;

procedure TDataManager.CBRefreshChange(Sender: TObject);
begin
  if not IChanging then
     IEditor.Data['AutoRefresh']:=TCommonUI.ToBooleanString(CBRefresh.IsChecked);
end;

procedure TDataManager.CBStoresChange(Sender: TObject);
begin
  BAdd.Enabled:=CBStores.ItemIndex<>-1;
  ReFill;
end;

class function TDataManager.ChooseName(const AOwner: TComponent; const AStore:String=''): String;
begin
  with TDataManager.CreateStore(AOwner,AStore) do
  try
    LayoutStore.Visible:=False;
    TabControl1.Visible:=False;
    Splitter1.Visible:=False;
    LayoutAddDelete.Visible:=False;
    LayoutSources.Align:=TUICommon.AlignClient;
    Width:=440;

    LSources.ItemAppearanceObjects.ItemObjects.Accessory.Visible:=False;

    if ShowModal=mrOk then
       result:=Current
    else
       result:='';
  finally
    Free;
  end;
end;

procedure TDataManager.SetSourcesWidth;
begin
  LayoutSources.Width:=Width*0.5;
end;

class function TDataManager.CreateChooser(const AOwner: TComponent; const AStore:String=''): TDataManager;
begin
  result:=TDataManager.CreateStore(AOwner,AStore);

  result.ChoosingData:=True;

  result.TabControl1.Visible:=False;
  result.LayoutAddDelete.Visible:=False;
  result.BRename.Visible:=False;

  result.LayoutSources.Align:=TUICommon.AlignLeft;

  result.SetSourcesWidth;

  result.ListData.Align:=TUICommon.AlignClient;
  result.ListData.Visible:=True;

  result.Splitter1.Align:=TUICommon.AlignLeft;
end;

class function TDataManager.ChooseData(const AOwner: TComponent;
                 const AStore:String='';
                 const ACurrent:TDataItem=nil): TDataItem;
var tmp : TDataManager;
begin
  tmp:=CreateChooser(AOwner,AStore);
  try
    tmp.ICurrent:=ACurrent;

    if tmp.ShowModal=mrOk then
       result:=tmp.Selected
    else
       result:=ACurrent;
  finally
    tmp.Free;
  end;
end;

procedure TDataManager.FillData;
var Col,
    tmp : TDataItem;
begin
  ListData.BeginUpdate;
  try
    ListData.Items.Clear;

    tmp:=CurrentData;

    if tmp<>nil then
       for Col in tmp.Items.AsArray do
           ListData.Items.AddObject(Col.Name,Col);

    ListDataChange(Self);
  finally
    ListData.EndUpdate;
  end;
end;

procedure TDataManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'DataManager');
end;

procedure TDataManager.AddStores;
begin
  if IStore='' then
     IStore:=TStore.DefaultName;

  TStores.AllTo(CBStores.Items);
  CBStores.ItemIndex:=CBStores.Items.IndexOf(IStore);

  if CBStores.ItemIndex=-1 then
     raise TStore.NotRegistered(IStore);

  BAdd.Enabled:=CBStores.ItemIndex<>-1;
end;

procedure TDataManager.FormCreate(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'DataManager');

  TabControl1.ActiveTab:=TabSettings;

  if IStore='' then
     if TStore.DefaultName='' then
        TStoreEditor.Edit(Self);
end;

procedure TDataManager.FormDestroy(Sender: TObject);
begin
  IEditor.Free;
end;

procedure TDataManager.FormShow(Sender: TObject);
begin
  AddStores;

  if CBStores.Count=0 then
     BChangeClick(Self);
end;

procedure TDataManager.ListDataChange(Sender: TObject);
begin
  BSelect.Enabled:=(ListData.ItemIndex<>-1) or (LSources.ItemIndex>-1);

  if Assigned(FOnSelect) then
     FOnSelect(Self);
end;

procedure TDataManager.LSourcesChange(Sender: TObject);
begin
  BSelect.Enabled:=LSources.ItemIndex>-1;

  BDelete.Enabled:=BSelect.Enabled and (not TStore.IsRemote(IStore));
  BRename.Enabled:=BDelete.Enabled;

  if ChoosingData then
     FillData
  else
  if TabControl1.Visible then
  begin
    MemoImportLog.Lines.Clear;
    MemoImportLog.Visible:=False;

    if BSelect.Enabled then
    begin
      TabControl1.Enabled:=True;

      if IEditor=nil then
      begin
        IEditor:=TDataEditor.Create(nil);
        TUICommon.AddForm(IEditor,TabSettings);
      end;

      IEditor.Select(CurrentStore,Current);

      if TabControl1.ActiveTab=TabData then
         SetLastImport
      else
         LLastImport.Text:='?';

      SetScheduling;

      CBParallel.IsChecked:=IEditor.Data.AsBoolean('Parallel');

      TabSchedule.Enabled:=BDelete.Enabled;
      CBParallel.Enabled:=BDelete.Enabled;
      BImportNow.Enabled:=BDelete.Enabled;
      CBStopOnErrors.Enabled:=BDelete.Enabled;
    end
    else
      TabControl1.Enabled:=False;
  end;
end;

procedure TDataManager.SetScheduling;
begin
  IChanging:=True;

  CBRefresh.IsChecked:=IEditor.Data.AsBoolean('AutoRefresh');
  Period.Text:=IEditor.Data['RefreshPeriod'];
  RefreshUnit.ItemIndex:=StrToIntDef(IEditor.Data['RefreshUnit'],-1);

  IChanging:=False;

  if CBRefresh.IsChecked then
     LNextRefresh.Text:=DateTimeToStr(IEditor.Data.NextRefresh)
  else
     LNextRefresh.Text:='None';
end;

procedure TDataManager.TryAdd(const Kind:TDataDefinitionKind);
var tmp,
    tmpFile : String;
begin
  repeat
    tmp:=AskName;

    if tmp='' then
       break
    else
    begin
      tmpFile:=TStore.DefinitionOf(CurrentStore,tmp);

      if TFile.Exists(tmpFile) then
         TCommonUI.ShowMessage(Format(BIMsg_DataSourceAlreadyExists,[tmp]))
      else
      begin
        TDataDefinition.CreateFile(tmpFile,Kind);

        LSources.Items.Add.Text:=tmp;
        LSources.ItemIndex:=LSources.Items.Count-1;
        LSourcesChange(Self);

        break;
      end;
    end;

  until False;
end;

procedure TDataManager.MenuItem1Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Web);
end;

procedure TDataManager.MenuItem2Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Database);
end;

procedure TDataManager.MenuItem3Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Files);
end;

procedure TDataManager.MenuItem4Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Manual);
end;

procedure TDataManager.PeriodChange(Sender: TObject);
begin
  if not IChanging then
     IEditor.Data['RefreshPeriod']:=IntToStr(Round(Period.Value));
end;

function TDataManager.CurrentSource:Integer;
var tmp : TDataItem;
    tmpStore : TDataItem;
    t : Integer;
begin
  if ICurrent<>nil then
  begin
    tmpStore:=TStore.StoreOf(ICurrent);

    tmp:=ICurrent.Parent;

    while tmp<>nil do
    begin
      if tmp.Parent<>nil then
         if tmp.Parent=tmpStore then
         begin
           for t:=0 to LSources.Items.Count-1 do
               if SameText(LSources.Items[t].Text,tmp.Name) then
                  Exit(t);
         end;

      tmp:=tmp.Parent;
    end;
  end;

  result:=-1;
end;

procedure TDataManager.ReFill;

  procedure TrySelectCurrentData;
  begin
    ListData.ItemIndex:=ListData.Items.IndexOfObject(ICurrent);

    {$IF CompilerVersion>28}
    if ListData.ItemIndex<>-1 then
    begin
      ListData.ApplyStyleLookup;
      ListData.ScrollToItem(ListData.ItemByIndex(ListData.ItemIndex));
    end;
    {$ENDIF}
  end;

var tmp,
    S : String;
begin
  LSources.BeginUpdate;
  try
    LSources.Items.Clear;

    tmp:=CurrentStore;

    if tmp<>'' then
    try
      IStore:=tmp;

      try
        for S in TStore.AllData(IStore) do
            LSources.Items.Add.Text:=S;
      finally
        LSources.ItemIndex:=CurrentSource;
        LSourcesChange(Self);

        if ICurrent<>nil then
           TrySelectCurrentData;
      end;
    except
      on E:Exception do
         TCommonUI.ShowMessage(Format(BIMsg_Store_ErrorOpening,[E.Message,tmp]));
    end;
  finally
    LSources.EndUpdate;
  end;
end;

procedure TDataManager.RefreshUnitChange(Sender: TObject);
begin
  if not IChanging then
     IEditor.Data['RefreshUnit']:=IntToStr(RefreshUnit.ItemIndex);
end;

procedure TDataManager.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab=TabData then
     if LLastImport.Text='?' then
        SetLastImport;
end;

procedure TDataManager.TabControl1Resize(Sender: TObject);
begin
  if IEditor<>nil then
     IEditor.OnResize(Self);
end;

end.
