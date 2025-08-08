{*********************************************}
{  TeeBI Software Library                     }
{  DataManager VCL                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.DataManager;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.DataItem, BI.Persist, BI.DataSource, BI.UI, Vcl.Menus, VCLBI.Editor.Data,
  Vcl.Buttons, VCLBI.DataControl, VCLBI.Grid, VCLBI.Editor.Data.Links;

type
  TDataManagerFilter=class abstract
  public
    procedure Clear; virtual; abstract;
    function Valid(const AName:String):Boolean; overload; virtual; abstract;
    function Valid(const AData:TDataItem):Boolean; overload; virtual; abstract;
  end;

  TDataManagerEmbedMode=(Choose,Edit);

  TDataManager = class(TForm)
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    PanelSearch: TPanel;
    Tree: TTreeView;
    PanelStores: TPanel;
    Label1: TLabel;
    CBStores: TComboBox;
    Panel1: TPanel;
    LSearch: TLabel;
    ESearch: TEdit;
    BAdd: TButton;
    BDelete: TButton;
    PopupMenu1: TPopupMenu;
    Files1: TMenuItem;
    DatabaseServer1: TMenuItem;
    BIWeb1: TMenuItem;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    TabSettings: TTabSheet;
    TabData: TTabSheet;
    MemoImportLog: TMemo;
    DataMenu: TPopupMenu;
    ViewData1: TMenuItem;
    BRename: TButton;
    PanelManage: TPanel;
    BManageStores: TButton;
    TabLinks: TTabSheet;
    Custommanual1: TMenuItem;
    PanelDataTop: TPanel;
    Label2: TLabel;
    LLastImport: TLabel;
    BViewData: TButton;
    MemoDataInfo: TMemo;
    BImportNow: TButton;
    CBParallel: TCheckBox;
    CBStoponerrors: TCheckBox;
    ImportProgress: TProgressBar;
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure ESearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BViewDataClick(Sender: TObject);
    procedure CBStoresChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BManageStoresClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure BIWeb1Click(Sender: TObject);
    procedure DatabaseServer1Click(Sender: TObject);
    procedure CBParallelClick(Sender: TObject);
    procedure BImportNowClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PanelSearchResize(Sender: TObject);
    procedure ViewData1Click(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure PanelStoresResize(Sender: TObject);
    procedure Custommanual1Click(Sender: TObject);
  private
    { Private declarations }

    IEditor : TDataEditor;

    IDataLinks : TDataLinksEditor;

    IStore : String;
    IFilterTree : TDataManagerFilter;

    FOnSelect : TNotifyEvent;

    ICurrent : TDataItem;

    IFillingTree,
    IUpdatingTree : Boolean;

    procedure AddAllStores;
    procedure AddNodeChildren(const ANode:TTreeNode);
    procedure AddNodeData(const AStore:String; const Children:Boolean; const Filter:String='');
    function AskName:String;
    function CanAdd(const AName:String):Boolean; overload;
    function CanAdd(const AData:TDataItem):Boolean; overload;
    procedure CheckEditor;
    function Current:String;
    function DoImportData:TDataArray;
    procedure DoSaveData(const AData:TDataArray);
    procedure FillTree(const AStore:String);
    procedure ImportingData(const Sender:TObject; const Percent:Single; var Cancel:Boolean);
    function ImportingError(const Sender:TObject; const Text:String):Boolean;
    procedure LogException(const Text:String);
    function NodeWithData(const AData:TDataItem):TTreeNode;
    procedure ReplaceDummy(const Node:TTreeNode);
    function SelectedText:String;
    procedure SelectStore;
    procedure SetLastImport;
    procedure ShowDataInfo(SelectAtEditor:Boolean);
    procedure ShowDataLinks;
    procedure TryAdd(const Kind:TDataDefinitionKind);

    constructor CreateStore(const AOwner: TComponent; const AStore: String='');
    procedure SetFilterTree(const Value: TDataManagerFilter);
  protected
    StartEmpty : Boolean; // When True, the Tree of data items will not be filled

    procedure FillData(const AData:TDataArray);
  public
    { Public declarations }

    AddItems : Boolean;

    procedure AddNodeItems(const Node:TTreeNode);

    // Select a data item from any Store
    class function Choose(const AOwner:TComponent; const ACurrent:TDataItem=nil;
                          const FillItems:Boolean=False):TDataItem; static;

    function CurrentStore:String;

    class procedure Edit(const AOwner:TComponent; const AStore:String=''); static;

    class function Embed(const AOwner:TComponent;
                         const AParent:TWinControl;
                         const AMode:TDataManagerEmbedMode=TDataManagerEmbedMode.Choose;
                         const AStore:String='';
                         const ACurrent:TDataItem=nil):TDataManager; static;

    function SelectedData:TDataItem;
    function SelectedDefinition:TDataDefinition;

    function Selected: TDataItem;
    procedure SelectData(const AData:TDataItem);

    property OnFilter:TDataManagerFilter read IFilterTree write SetFilterTree;
    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TMasterFilter=class(TDataManagerFilter)
  private
    IData : TDataArray;
    INames : TStrings;

    class function DataNameOf(const AData:TDataItem):String; static;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Add(const AData:TDataItem);

    procedure Clear; override;
    function Valid(const AName:String):Boolean; override;
    function Valid(const AData:TDataItem):Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.DataViewer, VCLBI.Editor.Stores, BI.Languages.English,
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.IOUtils, System.Diagnostics,
  {$ENDIF}
  BI.Arrays, VCLBI.Menus, VCLBI.DataTree;

Constructor TDataManager.CreateStore(const AOwner: TComponent; const AStore: String='');
begin
  inherited Create(AOwner);
  IStore:=AStore;
end;

type
  TErrorHandler=class
  private
    Error : String;
    function Handler(const Sender:TObject; const Text:String):Boolean;
  end;

function TErrorHandler.Handler(const Sender:TObject; const Text:String):Boolean;
begin
  Error:=Text;
  result:=True;
end;

procedure TDataManager.AddNodeChildren(const ANode:TTreeNode);

  function ErrorText(const AText:String):String;
  begin
    result:='Error loading: '+AText;
  end;

var m : TDataItem;
    Old : Boolean;
    tmp : String;
    tmpError : TErrorHandler;
begin
  Old:=TPersistence.HideErrors;
  TPersistence.HideErrors:=True;
  try
    tmpError:=TErrorHandler.Create;
    try
      m:=TStore.Load(IStore,ANode.Text,tmpError.Handler);
      tmp:=tmpError.Error;
    finally
      tmpError.Free;
    end;

    if m=nil then
       Tree.Items.AddChild(ANode,ErrorText(tmp))
    else
    begin
      ANode.Data:=m;
      AddNodeItems(ANode);
    end;
  finally
    TPersistence.HideErrors:=Old;
  end;
end;

function TDataManager.CanAdd(const AName:String):Boolean;
begin
  result:=(IFilterTree=nil) or IFilterTree.Valid(AName);
end;

function TDataManager.CanAdd(const AData:TDataItem):Boolean;
begin
  result:=(IFilterTree=nil) or IFilterTree.Valid(AData);
end;

const
  DummyNode='**********************';

procedure TDataManager.AddNodeData(const AStore:String; const Children:Boolean; const Filter:String='');

  procedure AddData(const AName:String);
  var Items : TTreeNodes;
  begin
    Items:=Tree.Items;

    if Children then
       Items.AddChild(Items.AddChild(nil,AName),DummyNode)
    else
       Items.AddChild(nil,AName);
  end;

var tmp : String;
begin
  IStore:=AStore;

  for tmp in TStore.AllData(AStore) do
      if CanAdd(tmp) then
         AddData(tmp);
end;

function TDataManager.AskName: String;
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

{ TDataManager }

function TDataManager.Current: String;
begin
  if SelectedData=nil then
     result:=''
  else
     result:=SelectedData.Name;
end;

function TDataManager.CurrentStore: String;
begin
  if CBStores.ItemIndex=-1 then
     result:=''
  else
     result:=CBStores.Items[CBStores.ItemIndex];
end;

procedure TDataManager.Custommanual1Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Manual);
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
         ShowMessageFmt(BIMsg_DataSourceAlreadyExists,[tmp])
      else
      begin
        TDataDefinition.CreateFile(tmpFile,Kind);
        Tree.Items.AddChild(nil,tmp).Selected:=True;
        break;
      end;
    end;

  until False;
end;

procedure TDataManager.ViewData1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=SelectedData;

  if tmp<>nil then
     TDataViewer.View(Self,tmp);
end;

procedure TDataManager.DoSaveData(const AData:TDataArray);
var t1 : TStopwatch;
    tmpName,
    tmpFileName : String;
    tmp: TDataItem;
begin
  t1:=TStopwatch.StartNew;

  tmpName:=IEditor.Data.Description;

  if tmpName='' then
     raise EBIException.Create('Error: Data definition name is missing');

  tmpFileName:=TStore.FullPath(CurrentStore,tmpName);

  TStore.UnLoad(CurrentStore,tmpName);

  tmp:=TDataItem.Create(AData);
  try
    tmp.Name:=tmpName;
    TStore.Save(tmp,tmpFileName);
  finally
    tmp.Free;
  end;

  MemoImportLog.Lines.Add('Saving time: '+IntToStr(t1.ElapsedMilliseconds)+' msec');

  SetLastImport;
end;

function TDataManager.DoImportData:TDataArray;
var t1 : TStopwatch;
begin
  result:=nil;

  t1:=TStopwatch.StartNew;
  try
    IEditor.Data.OnImporting:=ImportingData;
    IEditor.Data.OnError:=ImportingError;

    ImportProgress.Position:=0;
    ImportProgress.Visible:=True;
    try
      result:=IEditor.Data.Import(CurrentStore);
    finally
      ImportProgress.Visible:=False;
      ImportProgress.Position:=0;
    end;

  except
    on E:Exception do
       LogException(E.Message);
  end;

  MemoImportLog.Lines.Add('Import time: '+IntToStr(t1.ElapsedMilliseconds)+' msec');
end;

procedure TDataManager.BImportNowClick(Sender: TObject);
var Data : TDataArray;
begin
  MemoImportLog.Visible:=False;
  MemoImportLog.Lines.BeginUpdate;
  try
    MemoImportLog.Lines.Clear;

    Data:=DoImportData;

    if Data<>nil then
       DoSaveData(Data)
    else
       MemoImportLog.Lines.Add('Error: Empty result');
  finally
    MemoImportLog.Lines.EndUpdate;
    MemoImportLog.Visible:=True;
  end;
end;

procedure TDataManager.BIWeb1Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Web);
end;

procedure TDataManager.BRenameClick(Sender: TObject);
var Old,tmp : String;
begin
  tmp:=SelectedText;
  Old:=tmp;

  if TUICommon.Input(BIMsg_Data_ChangeName,BIMsg_NewName,Old,tmp) then
  begin
    if not SameText(tmp,Old) then
    begin
      // Pending: Check new name "tmp" does not exist already !
      TStore.Rename(IStore,Old,tmp);
      Tree.Selected.Text:=tmp;
    end;
  end;
end;

function TDataManager.SelectedText:String;
begin
  if Tree.Selected=nil then
     result:=''
  else
     result:=Tree.Selected.Text;
end;

procedure TDataManager.BDeleteClick(Sender: TObject);
begin
  if TUICommon.YesNo(Format(BIMsg_Store_SureToDelete,[SelectedText])) then
  begin
    TStore.RemoveData(IStore,SelectedText);
    Tree.Selected.Free;

    TreeChange(Self,Tree.Selected);
  end;
end;

procedure TDataManager.AddAllStores;
begin
  TStores.AllTo(CBStores.Items);
end;

procedure TDataManager.BManageStoresClick(Sender: TObject);
begin
  TStoreEditor.Edit(Self);
  AddAllStores;

  IStore:='';
  SelectStore;
end;

procedure TDataManager.BAddClick(Sender: TObject);
begin
  TBIMenu.Popup(PopupMenu1,BAdd);
end;

procedure TDataManager.BViewDataClick(Sender: TObject);
var Data : TDataItem;
begin
  Data:=TStore.Load(CurrentStore,Current);

  if Data<>nil then
     TDataViewer.View(Self,Data);
end;

procedure TDataManager.CBParallelClick(Sender: TObject);
begin
  IEditor.Data['Parallel']:=BoolToStr(CBParallel.Checked,True);
end;

// Slow?
function Roots(const ATree:TTreeView):Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to ATree.Items.Count-1 do
      if ATree.Items[t].Parent=nil then
         Inc(result);
end;

procedure TDataManager.FillData(const AData: TDataArray);
begin
  TDataTree.Fill(AData,Tree,True);

  if Roots(Tree)=1 then
  begin
    Tree.Items[0].Expanded:=True;
    Tree.Items[0].Selected:=True;
  end;
end;

procedure TDataManager.FillTree(const AStore:String);
begin
  IFillingTree:=True;
  try
    Tree.Items.BeginUpdate;
    try
      Screen.Cursor:=crHourGlass;
      try
        IUpdatingTree:=True;

        Tree.Items.Clear;
        AddNodeData(AStore,AddItems);

        BAdd.Enabled:=True;
      finally
        Screen.Cursor:=crDefault;
      end;
    finally
      Tree.Items.EndUpdate;

      IUpdatingTree:=False;

      if ICurrent<>nil then
         Tree.Selected:=NodeWithData(ICurrent);

      TreeChange(Tree,Tree.Selected);
    end;
  finally
    IFillingTree:=False;
  end;
end;

procedure TDataManager.CBStoresChange(Sender: TObject);
begin
  FillTree(CBStores.Items[CBStores.ItemIndex]);
end;

class function TDataManager.Choose(const AOwner: TComponent; const ACurrent: TDataItem;
                                   const FillItems:Boolean): TDataItem;
begin
  result:=ACurrent;

  with TDataManager.Create(AOwner) do
  try
    AddItems:=FillItems;

    BAdd.Hide;
    BDelete.Hide;
    BRename.Hide;

    PageControl1.Hide;
    Splitter1.Hide;
    Tree.Align:=TAlign.alClient;

    ICurrent:=ACurrent;

    if ShowModal=mrOk then
       result:=SelectedData;
  finally
    Free;
  end;
end;

function FindRootNode(const ATree:TTreeView; const AText:String):TTreeNode;
var t : Integer;
    tmp : TTreeNode;
begin
  for t:=0 to ATree.Items.Count-1 do
  begin
    tmp:=ATree.Items[t];

    if tmp.Parent=nil then
       if SameText(tmp.Text,AText) then
          Exit(tmp);
  end;

  result:=nil;
end;

procedure TDataManager.DatabaseServer1Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Database);
end;

class function TDataManager.Embed(const AOwner:TComponent;
                       const AParent:TWinControl;
                       const AMode:TDataManagerEmbedMode;
                       const AStore:String;
                       const ACurrent:TDataItem):TDataManager;
begin
  result:=TDataManager.CreateStore(AOwner,AStore);

  result.AddItems:=True;

  if AMode=TDataManagerEmbedMode.Choose then
  begin
    result.PageControl1.Hide;
    result.Tree.Align:=alClient;
    result.PanelButtons.Hide;
  end
  else
    result.PanelOk.Hide;

  result.IStore:=AStore;
  result.ICurrent:=ACurrent;

  TUICommon.AddForm(result,AParent);
end;

class procedure TDataManager.Edit(const AOwner: TComponent; const AStore:String='');
begin
  with TDataManager.CreateStore(AOwner,AStore) do
  try
    BCancel.Hide;

    BOk.Caption:='OK';
    BOk.Enabled:=True;
    BViewData.Enabled:=True;

    PanelOk.Width:=BOk.Width+2*BOk.Left;

    ShowModal;
  finally
    Free;
  end;
end;

procedure TDataManager.ESearchChange(Sender: TObject);
var S : String;
    t : Integer;
begin
  S:=UpperCase(ESearch.Text);

  if S<>'' then
     for t:=0 to Tree.Items.Count-1 do
        if Pos(S,UpperCase(Tree.Items[t].Text))>0 then
        begin
          Tree.Items[t].Selected:=True;
          break;
        end;
end;

procedure TDataManager.Files1Click(Sender: TObject);
begin
  TryAdd(TDataDefinitionKind.Files);
end;

procedure TDataManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'DataManager');
end;

procedure TDataManager.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSettings;
  ActiveControl:=Tree;
end;

procedure TDataManager.FormResize(Sender: TObject);
begin
  if PanelStores.Visible then
  begin
    if Width<(270+274) then
    begin
      PanelStores.Parent:=Self;
      PanelStores.Align:=alTop;
      PanelStores.Height:=PanelSearch.Height;

      PanelSearch.Align:=alBottom;
      PanelSearch.Align:=alTop;
    end
    else
    begin
      PanelStores.Parent:=PanelSearch;
      PanelStores.Align:=alRight;
      PanelStores.Width:=274;
    end;
  end;
end;

procedure TDataManager.SelectStore;
begin
  if IStore='' then
     IStore:=TStore.DefaultName;

  if IStore='' then
  begin
    if CBStores.Items.Count>0 then
    begin
      CBStores.ItemIndex:=0;
      CBStoresChange(Self);
    end;
  end
  else
  begin
    CBStores.ItemIndex:=CBStores.Items.IndexOf(IStore);

    if CBStores.ItemIndex=-1 then
       raise TStore.NotRegistered(IStore);

    CBStoresChange(Self);
  end;
end;

procedure TDataManager.FormShow(Sender: TObject);
begin
  if StartEmpty then
     Exit;

  TUICommon.LoadPosition(Self,'DataManager');

  if IStore='' then
     if TStore.DefaultName='' then
        TStoreEditor.Edit(Self);

  if IStore='' then
     IStore:=TStore.DefaultName;

  if PanelStores.Visible then
  begin
    AddAllStores;
    SelectStore;
  end
  else
  if IStore<>'' then
     FillTree(IStore);
end;

procedure TDataManager.ImportingData(const Sender: TObject; const Percent: Single; var Cancel:Boolean);
var tmp : Integer;
begin
  if ImportProgress.Visible then
  begin
    tmp:=Round(Percent);

    if ImportProgress.Position<tmp then
       ImportProgress.Position:=tmp;

    // Cancel:=DoCancel;
  end;
end;

function TDataManager.ImportingError(const Sender: TObject; const Text:String):Boolean;
begin
  LogException(Text);
  result:=not CBStopOnErrors.Checked;
end;

procedure TDataManager.LogException(const Text:String);
begin
  {$IFNDEF FPC}
  TThread.Queue(nil,procedure
  begin
  {$ENDIF}

    MemoImportLog.Lines.Add(Format(BIMsg_ImportError,[SelectedText]));
    MemoImportLog.Lines.Add('Error: '+Text);

  {$IFNDEF FPC}
  end);
  {$ENDIF}
end;

// Returns Tree node that is associated with AData.
// If necessary, nodes are expanded to allow filling node children,
// and collapsed when the AData cannot be found
function TDataManager.NodeWithData(const AData: TDataItem): TTreeNode;

  // Returns the item in the cache that corresponds to the "root" node
  // of AData, for the Store that we are currently presenting nodes
  function StoreCache(AData:TDataItem):TDataItem;
  var tmp : TDataItem;
  begin
    result:=nil;

    if AData<>nil then
    begin
      tmp:=AData;

      repeat
        if AData.Parent=TStores.GlobalCache then
        begin
          if AData.Name=IStore then
             Exit(tmp)
          else
             Exit(nil);
        end
        else
        begin
          tmp:=AData;
          AData:=AData.Parent;
        end;

      until AData=nil;
    end;
  end;

  // Try to find AData in ANode or ANode children, recursively
  function DoFind(const ANode:TTreeNode):TTreeNode;

    // Returns the tree childre node of AItem that has AData as data
    function FindIn(const AItem:TTreeNode):TTreeNode; overload;
    var t : Integer;
    begin
      for t:=0 to AItem.Count-1 do
          if AItem[t].Data=AData then
             Exit(AItem[t]);

      result:=nil;
    end;

    function FindInner(const AParent:TTreeNode):TTreeNode;
    var t : Integer;
    begin
      // Try first to find in first children level
      result:=FindIn(AParent);

      // If not found, then try to find in depth
      if result=nil then
         for t:=0 to AParent.Count-1 do
         begin
           result:=DoFind(AParent[t]);

           if result<>nil then
              Exit;
         end;
    end;

  begin
    result:=nil;

    if ANode.Expanded then
    begin
      // ANode is Expanded, then we don't need to force Expand, as
      // ANode.Data and ANode children have already been loaded and filled

      if ANode.Data=AData then
         Exit(ANode)
      else
      if ANode.Count>0 then
      begin
        result:=FindInner(ANode);

        if result<>nil then
           Exit;
      end;
    end
    else
    begin
      // Perform first a Expand to allow filling ANode Data and childen
      ANode.Expand(False);

      if ANode.Data=AData then
      begin
        ANode.Collapse(True);
        Exit(ANode);
      end
      else
      if ANode.Count>0 then
      begin
        result:=FindInner(ANode);

        if result<>nil then
           Exit;
      end;

      // Collapse if AData cannot be found to restore status
      ANode.Collapse(True);
    end;
  end;

  // Returns the tree root node that has the same text as AText
  function FindRootNode(const AText:String):TTreeNode;
  var tmp : TTreeNode;
  begin
    tmp:=Tree.Items.GetFirstNode;

    repeat
      // Skip non-root nodes
      while (tmp<>nil) and (tmp.Parent<>nil) do
            tmp:=tmp.GetNext;

      if tmp<>nil then
      begin
        if SameText(tmp.Text,AText) then
           Exit(tmp);

        tmp:=tmp.GetNext;
      end

    until tmp=nil;

    result:=nil;
  end;

var tmp : TTreeNode;
    tmpRoot : TDataItem;
begin
  result:=nil;

  tmpRoot:=StoreCache(AData);

  if tmpRoot<>nil then
  begin
    tmp:=FindRootNode(tmpRoot.Name);

    if tmp<>nil then
       result:=DoFind(tmp);
  end;
end;

procedure TDataManager.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabData then
     ShowDataInfo(True)
  else
  if PageControl1.ActivePage=TabLinks then
     ShowDataLinks;
end;

procedure TDataManager.PanelSearchResize(Sender: TObject);
var tmp : Integer;
begin
  if ESearch.BoundsRect.Right>PanelSearch.Width then
  begin
    tmp:=PanelSearch.Width-ESearch.Left;

    if tmp<10 then
       PanelSearch.Visible:=False
    else
    begin
      PanelSearch.Visible:=True;
      ESearch.Width:=tmp;
    end;
  end
  else
    PanelSearch.Visible:=True;
end;

procedure TDataManager.PanelStoresResize(Sender: TObject);
var tmp : Integer;
begin
  PanelManage.Visible:=PanelStores.Width>150;

  if PanelManage.Visible then
     tmp:=PanelStores.Width-PanelManage.Width-CBStores.Left
  else
     tmp:=PanelStores.Width-CBStores.Left-4;

  if tmp>20 then
     CBStores.Width:=tmp;
end;

procedure TDataManager.SelectData(const AData: TDataItem);
var tmp : TDataItem;
begin
  ICurrent:=AData;

  if AData=nil then
     Tree.Selected:=nil
  else
  begin
    tmp:=TStore.StoreOf(ICurrent);

    if tmp<>nil then
       IStore:=tmp.Name;

    if Tree.Items.Count>0 then
       Tree.Selected:=NodeWithData(AData);
  end;
end;

function TDataManager.Selected: TDataItem;
begin
  if (Tree.Selected<>nil) and (Tree.Selected.Data<>nil) then
     result:=TDataItem(Tree.Selected.Data)
  else
     result:=nil;
end;

function TDataManager.SelectedData: TDataItem;
begin
  if Tree.Selected=nil then
     result:=nil
  else
  if Tree.Selected.Data=nil then
     result:=TStore.Load(CurrentStore,SelectedText)
  else
     result:=TDataItem(Tree.Selected.Data);
end;

procedure TDataManager.SetFilterTree(const Value: TDataManagerFilter);
begin
  IFilterTree:=Value;
  FillTree(IStore);
end;

procedure TDataManager.SetLastImport;

  procedure ShowDataInfo(const Data:TDataItem);
  begin
    TCommonUI.AddInfo(Data,MemoDataInfo.Lines);
  end;

  procedure ShowDataInfoFromFile(const AFile:String);
  var Data : TDataItem;
      tmpData : String;
      tmpSize : Int64;
  begin
    LLastImport.Caption:=DateTimeToStr(TFile.GetLastWriteTime(AFile));

    tmpSize:=TBIFileSource.GetFileSize(AFile);

    tmpData:=TPath.ChangeExtension(AFile,TDataPersistence.Extension);

    try
      if FileExists(tmpData) then
         tmpSize:=tmpSize+TBIFileSource.GetFileSize(tmpData);

      MemoDataInfo.Lines.Add('Size on disk: '+TCommonUI.BytesToString(tmpSize));

      Data:=TPersistence.Load(AFile);
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
  end;

  procedure ShowDataInfoFromStore;
  var Data : TDataItem;
  begin
    LLastImport.Caption:='(unknown)';

    Data:=TStore.Load(CurrentStore,SelectedText
            {$IFNDEF FPC}
            ,
            function(const Sender:TObject; const Text:String):Boolean
            begin
              MemoDataInfo.Clear;
              MemoDataInfo.Lines.Add(Text);

              result:=True;
            end
            {$ENDIF}
            );

    BViewData.Enabled:=Data<>nil;

    if Data<>nil then
       ShowDataInfo(Data);
  end;

var tmp : String;
begin
  MemoDataInfo.Lines.Clear;

  tmp:=IEditor.Data.LastImport;

  if (tmp<>'') and TFile.Exists(tmp) then
     ShowDataInfoFromFile(tmp)
  else
     ShowDataInfoFromStore;
end;

function TDataManager.SelectedDefinition:TDataDefinition;
begin
  if IEditor=nil then
     result:=nil
  else
     result:=IEditor.Data;
end;

procedure TDataManager.CheckEditor;
begin
  if IEditor=nil then
  begin
    IEditor:=TDataEditor.Create(nil);
    IEditor.PanelButtons.Visible:=False;
    TUICommon.AddForm(IEditor,TabSettings);
  end;
end;

procedure TDataManager.ShowDataInfo(SelectAtEditor:Boolean);
begin
  MemoImportLog.Lines.Clear;
  MemoImportLog.Visible:=False;

  if SelectAtEditor then
  begin
    CheckEditor;
    IEditor.Select(CurrentStore,SelectedText);
  end;

  if PageControl1.ActivePage=TabData then
     SetLastImport
  else
     LLastImport.Caption:='?';

  CBParallel.Checked:=IEditor.Data.AsBoolean('Parallel');

  BImportNow.Enabled:=BDelete.Enabled;
  CBParallel.Enabled:=BDelete.Enabled;
  CBStoponerrors.Enabled:=BDelete.Enabled;
end;

type
  TDataDefinitionAccess=class(TDataDefinition);

procedure TDataManager.ShowDataLinks;
begin
  TDataDefinitionAccess(IEditor.Data).TryLoadDetails(IEditor.Data.Store);

  if IDataLinks=nil then
     IDataLinks:=TDataLinksEditor.Embed(Self,TabLinks);

  IDataLinks.Refresh(IEditor.Data);
end;

procedure TDataManager.TreeChange(Sender: TObject; Node: TTreeNode);

  procedure EnableButtons;
  begin
    if BCancel.Visible then
    begin
      BOk.Enabled:=(Node<>nil);
      BViewData.Enabled:=BOk.Enabled;
    end;

    BDelete.Enabled:=(Node<>nil) and (Node.Parent=nil) and (not TStore.IsRemote(IStore));
    BRename.Enabled:=BDelete.Enabled;
  end;

  procedure ShowHideEditorTabs;
  var Old : Boolean;
  begin
    IEditor.TabSources.Enabled:=not TStore.IsRemote(IStore);

    TabData.TabVisible:=(IEditor.Data=nil) or (IEditor.Data.Kind<>TDataDefinitionKind.Manual);

    Old:=TabSettings.TabVisible;

    TabSettings.TabVisible:=(IEditor.Data<>nil) and (IEditor.Data.Kind<>TDataDefinitionKind.Unknown);

    if Old<>TabSettings.TabVisible then
       if TabSettings.TabVisible then
          PageControl1.ActivePage:=TabSettings;
  end;

begin
  if not IUpdatingTree then
  begin
    if PanelButtons.Visible then
    begin
      EnableButtons;

      if Node=nil then
      begin
        PageControl1.Enabled:=False;
        PageControl1.ActivePage:=TabSettings;

        if IEditor<>nil then
           IEditor.Hide;
      end
      else
      if PageControl1.Visible then
      begin
        PageControl1.Enabled:=True;

        CheckEditor;

        IEditor.Select(CurrentStore,SelectedText);

        IEditor.Show;

        ShowHideEditorTabs;

        if PageControl1.ActivePage=TabData then
           ShowDataInfo(False)
        else
        if PageControl1.ActivePage=TabLinks then
           ShowDataLinks;
      end;
    end;

    if not IFillingTree then
       if Assigned(FOnSelect) then
          FOnSelect(Self);
  end;
end;

procedure TDataManager.TreeDblClick(Sender: TObject);
begin
  if BOk.Enabled and PanelButtons.Visible and BCancel.Visible then
     ModalResult:=mrOk;
end;

// Add all DataItem children to tree Node, recursively
procedure TDataManager.AddNodeItems(const Node:TTreeNode);
var tmp : TDataItem;
    tmpNode : TTreeNode;
begin
  if TObject(Node.Data) is TDataItem  then
     for tmp in TDataItem(Node.Data).Items.AsArray do
     begin
       if CanAdd(tmp) then
       begin
         tmpNode:=Tree.Items.AddChildObject(Node,tmp.Name,tmp);
         AddNodeItems(tmpNode);
       end;
     end;
end;

procedure TDataManager.ReplaceDummy(const Node:TTreeNode);
var tmp : TTreeNode;
begin
  Tree.Items.BeginUpdate;
  try
    try
      tmp:=Node.{$IFDEF FPC}Items{$ELSE}Item{$ENDIF}[0];

      if Node.Data=nil then
         AddNodeChildren(Node)
      else
      if AddItems then
         AddNodeItems(Node);

      tmp.Free;
    except
      on E:Exception do
         ShowMessage(Format(BIMsg_Store_ErrorOpening,[E.Message,Node.Text]));
    end;
  finally
    Tree.Items.EndUpdate;
  end;
end;

procedure TDataManager.TreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  AllowExpansion:=False;

  if (Node.Count=1) and (Node.{$IFDEF FPC}Items{$ELSE}Item{$ENDIF}[0].Text=DummyNode) then
  begin
    Screen.Cursor:=crHourGlass;
    try
      ReplaceDummy(Node);
      AllowExpansion:=True;
    finally
      Screen.Cursor:=crDefault;
    end;
  end
  else
    AllowExpansion:=True;

  if Tree.Selected=nil then
     Tree.Selected:=Node;
end;

type
  TDataAcess=class(TDataItem);

{ TMasterFilter }

Constructor TMasterFilter.Create;
begin
  inherited;
  INames:=TStringList.Create;
end;

Destructor TMasterFilter.Destroy;
begin
  INames.Free;
  inherited;
end;

procedure TMasterFilter.Clear;
begin
  INames.Clear;
  IData:=nil;
end;

function TMasterFilter.Valid(const AName: String): Boolean;
begin
  result:=(INames.Count=0) or (INames.IndexOf(AName)<>-1);
end;

function TMasterFilter.Valid(const AData: TDataItem): Boolean;
begin
  result:=(AData.Kind<>TDataKind.dkUnknown) or
          (IData.Count=0) or
          (IData.Exists(AData));
end;

procedure TMasterFilter.Add(const AData:TDataItem);

  procedure TryAddStore(const AName:String);
  begin
    if AName<>'' then
       if INames.IndexOf(AName)=-1 then
          INames.Add(AName);
  end;

  procedure DoAdd(const AData:TDataItem);

    procedure DoAddTable(const AData:TDataItem);

      procedure TryAdd(const AData:TDataItem);
      var tmp : TDataItem;
      begin
        if TDataAcess(AData).HasMaster then
        begin
          tmp:=AData.Master.Parent;

          if not IData.Exists(tmp) then
             DoAdd(tmp);
        end;
      end;

    var tmp : TDataItem;
    begin
      for tmp in AData.Items.AsArray do
          TryAdd(tmp);
    end;

  begin
    IData.Add(AData);
    TryAddStore(DataNameOf(AData));
    DoAddTable(AData);
  end;

begin
  if (AData<>nil) and (AData.Parent<>nil) then
     DoAdd(AData.Parent);
end;

class function TMasterFilter.DataNameOf(const AData:TDataItem):String;
var tmp : TDataItem;
begin
  result:='';

  if AData<>nil then
  begin
    tmp:=AData.Parent;

    while tmp<>nil do
    begin
      if tmp.Parent<>nil then
         if tmp.Parent.Parent=TStores.GlobalCache then
            Exit(tmp.Name);

      tmp:=tmp.Parent;
    end;
  end;
end;

end.

