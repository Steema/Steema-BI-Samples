{*********************************************}
{  TeeBI Software Library                     }
{  TBITree control for VCL and FireMonkey     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Tree;
{$DEFINE FMX}

interface

// This unit implements a visual control (TBITree), capable of displaying data
// from TDataItem instances in hierarchical layout, using a TreeView or any other
// compatible control using the "Plugin" property.

// TBITree "Fill..." methods offer several modes to add tree nodes.

uses
  System.Classes,
  {$IFDEF FMX}
  FMX.Controls, FMX.TreeView, FMXBI.DataControl,
  {$ELSE}
  VCL.Controls, VCL.Graphics, VCLBI.DataControl,
  {$ENDIF}
  BI.Arrays, BI.DataItem;

type
  TBITreeNode=class(TObject);

  TBITreeExpandingEvent=procedure(Sender: TObject; const Node: TBITreeNode;
                                  var AllowExpansion: Boolean) of object;

  TBITreePlugin=class abstract
  public
  type
    TNodeData=class
    public
      Index : TInteger;

      {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
      Tag : TObject;
    end;
  private
  protected
    IData : Array of TNodeData;

    FOnDelete : TNotifyEvent;

    procedure BeginUpdating; virtual; abstract;
    procedure Clear(const ANode:TBITreeNode=nil); virtual; abstract;
    procedure ClearData; overload;
    procedure ClearData(const ANode:TBITreeNode); overload; virtual; abstract;
    procedure DeleteData(const AData:TNodeData);
    procedure EndUpdating; virtual; abstract;
    function GetAllowDelete: Boolean; virtual; abstract;
    function GetControl:TControl; virtual; abstract;
    function GetCount:Integer; virtual; abstract;
    function GetNode(const AIndex:Integer):TBITreeNode; virtual; abstract;
    function GetOnChange: TNotifyEvent; virtual; abstract;
    function GetOnCheck: TNotifyEvent; virtual; abstract;
    function GetOnExpanding: TBITreeExpandingEvent; virtual; abstract;
    function GetSelected:TBITreeNode; virtual; abstract;
    function GetSelectedData:TBITreePlugin.TNodeData; virtual; abstract;
    function NewNodeData(const ATag:TObject; const AIndex:TInteger):TNodeData;
    function NodeAt(const X,Y:Integer):TBITreeNode; virtual; abstract;
    function SelectedText:String; virtual; abstract;
    procedure SetAllowDelete(const Value: Boolean); virtual; abstract;
    procedure SetData(const ANode:TBITreeNode; const AData:TObject); virtual; abstract;
    procedure SetOnChange(const Value: TNotifyEvent); virtual; abstract;
    procedure SetOnCheck(const Value: TNotifyEvent); virtual; abstract;
    procedure SetOnExpanding(const Value: TBITreeExpandingEvent); virtual; abstract;
    procedure SetSelected(const Value: TBITreeNode); virtual; abstract;
  public
    Constructor Create(const AOwner:TComponent); virtual; abstract;
    Destructor Destroy; override;

    function Children(const ANode:TBITreeNode; const AIndex:Integer):TBITreeNode; virtual; abstract;
    function ChildrenCount(const ANode:TBITreeNode):Integer; virtual; abstract;

    function DataOf(const ANode:TBITreeNode):TObject; virtual; abstract;
    procedure Expand(const AIndex:Integer); overload; virtual; abstract;

    procedure Expand(const ANode:TBITreeNode;
                     const DoExpand:Boolean=True;
                     const Recursive:Boolean=False); overload; virtual; abstract;

    function Find(const ATag:TObject; const AIndex:Integer=-1):TBITreeNode; virtual; abstract;

    function FirstNode:TBITreeNode; virtual; abstract;
    function NextNode(const ANode:TBITreeNode):TBITreeNode; virtual; abstract;

    function IsChecked(const ANode:TBITreeNode):Boolean; virtual; abstract;
    function IsExpanded(const ANode:TBITreeNode):Boolean; virtual; abstract;

    function NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject=nil;
              const AIndex:TInteger=-1):TBITreeNode; virtual; abstract;

    function ParentOf(const ANode:TBITreeNode):TBITreeNode; virtual; abstract;

    procedure SetChecked(const ANode:TBITreeNode; const Value:Boolean); virtual; abstract;
    procedure SetText(const ANode:TBITreeNode; const Value:String); virtual; abstract;
    function SiblingIndex(const ANode:TBITreeNode):Integer; virtual; abstract;
    function TextOf(const ANode:TBITreeNode):String; virtual; abstract;

    property AllowDelete:Boolean read GetAllowDelete write SetAllowDelete;
    property Control:TControl read GetControl;
    property Count:Integer read GetCount;
    property Selected:TBITreeNode read GetSelected write SetSelected;
    property SelectedData:TBITreePlugin.TNodeData read GetSelectedData;

    property OnChange:TNotifyEvent read GetOnChange write SetOnChange;
    property OnCheck:TNotifyEvent read GetOnCheck write SetOnCheck;
    property OnExpanding:TBITreeExpandingEvent read GetOnExpanding write SetOnExpanding;
  end;

  TBITreePluginClass=class of TBITreePlugin;

  // Generic Tree control that fills nodes from TDataItem relationships

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  {$ENDIF}
  TBITree=class(TBIDataControl)
  public
    type
      TBITreeNodes=class
      private
        {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
        ITree : TBITree;

        function Get(const Index: Integer): TBITreeNode;
      public
        function Count:Integer;
        property Items[const Index:Integer]:TBITreeNode read Get; default;
      end;

  private
    FNodes : TBITreeNodes;
    FOnDeleting : TNotifyEvent;
    FPlugin : TBITreePlugin;

    function GetAllowDelete: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetSelected: TBITreeNode;
    procedure SetAllowDelete(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetSelected(const Value: TBITreeNode);
    procedure SetPlugin(const APlugin:TBITreePlugin);
    function GetExpanding: TBITreeExpandingEvent;
    procedure SetExpanding(const Value: TBITreeExpandingEvent);
  protected
    procedure ChangeDragMode;
    procedure DeletedNode(Sender:TObject);
    procedure Loaded; override;
    procedure SetDataDirect(const Value: TDataItem); override;
  public
    class var
      Engine : TBITreePluginClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function Add(const AParent:TBITreeNode; const AText:String; const AObject:TObject=nil):TBITreeNode; overload;
    function Add(const AText:String; const AObject:TObject=nil):TBITreeNode; overload; inline;

    procedure BeginUpdating; // <-- not "BeginUpdate" due to conflict with FMX

    procedure ChangePlugin(const APluginClass:TBITreePluginClass);

    procedure Clear(const ANode:TBITreeNode=nil);
    function DataOf(const ANode:TBITreeNode):TObject;

    procedure EndUpdating;

    procedure Expand(const ANode:TBITreeNode;
                     const DoExpand:Boolean=True;
                     const Recursive:Boolean=False);

    procedure Fill(const AData:TDataItem); overload;
    procedure Fill(const AMaster,ADetail:TDataItem); overload;
    procedure FillParent(const AKey,AParent:TDataItem; const ADisplay:TDataItem=nil);

    procedure FillStore(const AStore:String);
    procedure FillStores;

    function Find(const AText:String):TBITreeNode;

    function NodeAt(const X,Y:Integer):TBITreeNode;

    function ParentOf(const ANode:TBITreeNode):TBITreeNode;

    function SelectedData:TDataItem;
    function SelectedText:String;

    procedure SetNodeData(const ANode:TBITreeNode; const AData:TObject);

    property Nodes:TBITreeNodes read FNodes;
    property Plugin:TBITreePlugin read FPlugin write SetPlugin;
    property Selected:TBITreeNode read GetSelected write SetSelected;
  published
    property AllowDelete:Boolean read GetAllowDelete write SetAllowDelete default False;

    {$IFNDEF FMX}
    property Color default clWhite;
    {$ENDIF}

    property OnChange:TNotifyEvent read GetOnChange write SetOnChange;
    property OnDeleting:TNotifyEvent read FOnDeleting write FOnDeleting;
    property OnExpanding:TBITreeExpandingEvent read GetExpanding write SetExpanding;
  end;

implementation

uses
  BI.DataSource,
  {$IFDEF FMX}
  FMXBI.Tree.TreeView, FMXBI.Grid,
  {$ELSE}
  VCLBI.Tree.TreeView, VCLBI.Grid,
  {$ENDIF}
  BI.Persist, BI.Store.Component, BI.Expressions;

{ TBITree }

Constructor TBITree.Create(AOwner: TComponent);
begin
  inherited;

  {$IFNDEF FMX}
  Color:=clWhite;
  {$ENDIF}

  FNodes:=TBITreeNodes.Create;
  FNodes.ITree:=Self;

  SetPlugin(Engine.Create(Self));
end;

Destructor TBITree.Destroy;
begin
  FNodes.Free;

  FPlugin.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};

  inherited;
end;

procedure TBITree.SetPlugin(const APlugin:TBITreePlugin);
begin
  FPlugin:=APlugin;

  FPlugin.GetControl.Align:=TUICommon.AlignClient;
  FPlugin.GetControl.Parent:=Self;

  FPlugin.FOnDelete:=DeletedNode;
end;

procedure TBITree.DeletedNode(Sender:TObject);
begin
  if Assigned(FOnDeleting) then
     FOnDeleting(Self);

  if Selected<>nil then
  begin
    {$IFDEF AUTOREFCOUNT}
    Selected.DisposeOf;
    {$ELSE}
    Selected.Free;
    {$ENDIF}
  end;
end;

procedure TBITree.Expand(const ANode: TBITreeNode; const DoExpand,Recursive: Boolean);
begin
  Plugin.Expand(ANode,DoExpand,Recursive);
end;

function TBITree.Add(const AParent: TBITreeNode; const AText: String;
  const AObject: TObject): TBITreeNode;
begin
  result:=Plugin.NewNode(AParent,AText,AObject);
end;

function TBITree.Add(const AText: String; const AObject:TObject=nil): TBITreeNode;
begin
  result:=Add(nil,AText,AObject)
end;

procedure TBITree.BeginUpdating;
begin
  Plugin.BeginUpdating;
end;

procedure TBITree.EndUpdating;
begin
  Plugin.EndUpdating;
end;

procedure TBITree.Clear(const ANode:TBITreeNode);
begin
  Plugin.Clear(ANode);
end;

procedure TBITree.ChangePlugin(const APluginClass:TBITreePluginClass);
var
  OldSelected,
  NewSelected : TBITreeNode;

  OldPlugin,
  NewPlugin : TBITreePlugin;

  function CreateNew(const AParent,AOld:TBITreeNode):TBITreeNode;
  begin
    result:=NewPlugin.NewNode(AParent,OldPlugin.TextOf(AOld),OldPlugin.DataOf(AOld));

    if AOld=OldSelected then
       NewSelected:=result;
  end;

  procedure AddNodes;

    procedure AddChildren(const AOld,ANew:TBITreeNode);
    var t : Integer;
        tmpOld : TBITreeNode;
    begin
      for t:=0 to OldPlugin.ChildrenCount(AOld)-1 do
      begin
        tmpOld:=OldPlugin.Children(AOld,t);
        AddChildren(tmpOld,CreateNew(ANew,tmpOld));
      end;

      if OldPlugin.IsExpanded(AOld) then
         NewPlugin.Expand(ANew);
    end;

  var tmp : TBITreeNode;
  begin
    tmp:=OldPlugin.FirstNode;

    while tmp<>nil do
    begin
      if OldPlugin.ParentOf(tmp)=nil then
         AddChildren(tmp,CreateNew(nil,tmp));

      tmp:=OldPlugin.NextNode(tmp);
    end;

  end;

var Old : TControl;
begin
  OldPlugin:=Plugin;

  if OldPlugin.ClassType<>APluginClass then
  begin
    Old:=Plugin.Control;
    OldSelected:=Plugin.Selected;

    NewSelected:=nil;

    NewPlugin:=APluginClass.Create(Self);

    Plugin:=NewPlugin;

    Plugin.BeginUpdating;
    try
      AddNodes;
    finally
      Plugin.EndUpdating;
    end;

    Old.Free;
    OldPlugin.Free;

    Plugin.Control;

    Plugin.Selected:=NewSelected;
  end;
end;

function TBITree.DataOf(const ANode: TBITreeNode): TObject;
begin
  result:=Plugin.DataOf(ANode);
end;

procedure TBITree.Fill(const AData:TDataItem);

  function NewNode(const AParent:TBITreeNode; const AItem:TDataItem):TBITreeNode;
  var tmp : String;
  begin
    tmp:=AItem.Name;

    if tmp='' then
       if AItem.Provider<>nil then
          tmp:=AItem.Provider.Name;

    result:=Plugin.NewNode(AParent,tmp,AItem);
  end;

  procedure AddItems(const AParent:TBITreeNode; const AItem:TDataItem);
  var t : Integer;
      tmpNode : TBITreeNode;
      tmpItem : TDataItem;
  begin
    for t:=0 to AItem.Items.Count-1 do
    begin
      tmpItem:=AItem.Items[t];
      tmpNode:=NewNode(AParent,tmpItem);
      AddItems(tmpNode,tmpItem);
    end;
  end;

  procedure AddMap(const AData:TDataItem);
  var t : Integer;
  begin
    AData.Stats;

    for t:=0 to AData.DataMap.Count-1 do
        Plugin.NewNode(nil,AData.DataMap.AsString(t));
  end;

begin
  Plugin.Clear;

  if AData<>nil then
  begin
    AData.Load;

    if AData.AsTable or (AData.Kind=TDataKind.dkUnknown) then
       AddItems(NewNode(nil,AData),AData)
    else
       AddMap(AData);
  end;
end;

procedure TBITree.Fill(const AMaster, ADetail: TDataItem);

  function GuessItems:TDataArray;
  begin
    SetLength(result,2);
    result[0]:=AMaster;
    result[1]:=ADetail;
  end;

var t : Integer;
    tmpNodes : Array of TBITreeNode;
    Hops : TDataHops;
    tmpMaster,
    tmpDetail,
    tmpCount : TInteger;
    HopsIndex : TInt32Array;
begin
  Plugin.Clear;

  Hops:=TDataHops.Create;
  try
    HopsIndex:=TDataSelect.SetupHops(Hops,GuessItems);

    AMaster.Load;
    AMaster.Stats;

    tmpCount:=AMaster.DataMap.Count;
    SetLength(tmpNodes,tmpCount);

    for t:=0 to tmpCount-1 do
        tmpNodes[t]:=Plugin.NewNode(nil,AMaster.DataMap.AsString(t),AMaster);

    ADetail.Load;

    for t:=0 to Hops.Main.Count-1 do
    begin
      Hops.Invalidate(t);

      tmpMaster:=Hops.Hops[HopsIndex[0]].Index;
      tmpDetail:=Hops.Hops[HopsIndex[1]].Index;

      Plugin.NewNode(tmpNodes[tmpMaster],ADetail.DataToString(tmpDetail),ADetail,tmpDetail);
    end;
  finally
    Hops.Free;
  end;
end;

procedure TBITree.FillParent(const AKey, AParent, ADisplay: TDataItem);

  function AddNode(const AIndex:TInteger):TBITreeNode;
  begin
    if ADisplay=nil then
       result:=Plugin.NewNode(nil,AParent.DataToString(AIndex),AKey,AIndex)
    else
       result:=Plugin.NewNode(nil,ADisplay.DataToString(AIndex),ADisplay,AIndex);
  end;

  function FindNode(const AIndex:TInteger):TBITreeNode;
  begin
    if ADisplay=nil then
       result:=Plugin.Find(AKey,AIndex)
    else
       result:=Plugin.Find(ADisplay,AIndex);
  end;

  function FindParent(const AIndex:TInteger):TInteger;
  begin
    if (AIndex=-1) or AParent.Missing[AIndex] then
       result:=-1
    else
    begin
      case AParent.Kind of
        TDataKind.dkInt32: result:=AParent.Int32Data[AIndex];
        TDataKind.dkInt64: result:=AParent.Int64Data[AIndex];
      else
        result:=-1;
      end;

      if result<>-1 then
      case AKey.Kind of
        TDataKind.dkInt32: result:=AKey.Int32Data.IndexOf(result);
        TDataKind.dkInt64: result:=AKey.Int64Data.IndexOf(result);
      end;
    end;
  end;

var
  HopsIndex : TInt32Array;

  procedure AddKeyParent(const AHops:TDataHops);
  var tmpParentIndex : TInteger;
      tmpParentNode : TBITreeNode;
      tmpDisplay : Integer;
      tmpKey,
      tmpParent : TInteger;
  begin
    tmpParent:=AHops.Hops[HopsIndex[1]].Index;
    tmpParentIndex:=FindParent(tmpParent);

    if tmpParentIndex=-1 then
    begin
      tmpParentNode:=FindNode(tmpParent);

      if tmpParentNode=nil then
         AddNode(tmpParent);
    end
    else
    begin
      tmpParentNode:=FindNode(tmpParentIndex);

      if tmpParentNode=nil then
         tmpParentNode:=AddNode(tmpParentIndex);

      if ADisplay=nil then
      begin
        tmpKey:=AHops.Hops[HopsIndex[0]].Index;
        Plugin.NewNode(tmpParentNode,AKey.DataToString(tmpKey),AKey,tmpKey);
      end
      else
      begin
        tmpDisplay:=AHops.Hops[HopsIndex[2]].Index;
        Plugin.NewNode(tmpParentNode,ADisplay.DataToString(tmpDisplay),ADisplay,tmpDisplay);
      end;
    end;
  end;

  function GuessItems:TDataArray;
  begin
    if ADisplay=nil then
       SetLength(result,2)
    else
       SetLength(result,3);

    result[0]:=AKey;
    result[1]:=AParent;

    if ADisplay<>nil then
       result[2]:=ADisplay;
  end;

var t : Integer;
    Hops : TDataHops;
    tmpItems : TDataArray;
begin
  Plugin.Clear;

  Hops:=TDataHops.Create;
  try
    tmpItems:=GuessItems;
    HopsIndex:=TDataSelect.SetupHops(Hops,tmpItems);

    for t:=0 to High(tmpItems) do
        tmpItems[t].Load;

    for t:=0 to Hops.Main.Count-1 do
    begin
      Hops.Invalidate(t);
      AddKeyParent(Hops);
    end;
  finally
    Hops.Free;
  end;
end;

procedure TBITree.FillStore(const AStore: String);
var tmp : TBITreeNode;
    s : String;
begin
  if AStore='' then
     s:=TStore.DefaultName
  else
     s:=AStore;

  Plugin.Clear;

  tmp:=Plugin.NewNode(nil,s);

  for s in TStore.AllData(AStore) do
      Plugin.NewNode(tmp,s);
end;

procedure TBITree.FillStores;
var s : String;
begin
  Plugin.Clear;

  for s in TStores.All do
      Plugin.NewNode(nil,s);
end;

function TBITree.Find(const AText: String): TBITreeNode;
begin
  result:=Plugin.FirstNode;

  while result<>nil do
    if Plugin.TextOf(result)=AText then
       Exit
    else
       result:=Plugin.NextNode(result);
end;

function TBITree.GetAllowDelete: Boolean;
begin
  result:=Plugin.AllowDelete;
end;

function TBITree.GetExpanding: TBITreeExpandingEvent;
begin
  result:=Plugin.OnExpanding;
end;

function TBITree.GetOnChange: TNotifyEvent;
begin
  result:=Plugin.OnChange;
end;

function TBITree.GetSelected: TBITreeNode;
begin
  result:=Plugin.Selected;
end;

type
  TControlAccess=class({$IFDEF FMX}TControl{$ELSE}TWinControl{$ENDIF});

procedure TBITree.ChangeDragMode;
begin
  TControlAccess(Plugin.GetControl).DragMode:=DragMode;

  // Route events
  TControlAccess(Plugin.GetControl).OnDragOver:=OnDragOver;
  TControlAccess(Plugin.GetControl).OnDragDrop:=OnDragDrop;
end;

procedure TBITree.Loaded;
begin
  inherited;
  ChangeDragMode;
end;

function TBITree.NodeAt(const X, Y: Integer): TBITreeNode;
begin
  result:=Plugin.NodeAt(X,Y);
end;

function TBITree.ParentOf(const ANode: TBITreeNode): TBITreeNode;
begin
  result:=Plugin.ParentOf(ANode);
end;

procedure TBITree.SetNodeData(const ANode: TBITreeNode; const AData: TObject);
begin
  Plugin.SetData(ANode,AData);
end;

function TBITree.SelectedData: TDataItem;
var tmp : TBITreeNode;
    tmpData : TObject;
begin
  tmp:=Selected;

  if tmp=nil then
     result:=nil
  else
  begin
    tmpData:=DataOf(tmp);

    if tmpData is TDataItem then
       result:=TDataItem(tmpData)
    else
       result:=nil;
  end;
end;

function TBITree.SelectedText: String;
begin
  result:=Plugin.SelectedText;
end;

procedure TBITree.SetAllowDelete(const Value: Boolean);
begin
  Plugin.AllowDelete:=Value;
end;

procedure TBITree.SetDataDirect(const Value: TDataItem);
begin
  inherited;
  Fill(Value);
end;

procedure TBITree.SetExpanding(const Value: TBITreeExpandingEvent);
begin
  Plugin.OnExpanding:=Value;
end;

procedure TBITree.SetOnChange(const Value: TNotifyEvent);
begin
  Plugin.OnChange:=Value;
end;

procedure TBITree.SetSelected(const Value: TBITreeNode);
begin
  Plugin.Selected:=Value;
end;

{ TBITreePlugin }

procedure TBITreePlugin.DeleteData(const AData:TNodeData);
var t,
    tt : Integer;
begin
  for t:=0 to High(IData) do
      if IData[t]=AData then
      begin
        // Delete

        for tt:=t to High(IData)-1 do
            IData[tt]:=IData[tt+1];

        SetLength(IData,High(IData));

        break;
      end;
end;

Destructor TBITreePlugin.Destroy;
begin
  ClearData;
  inherited;
end;

procedure TBITreePlugin.ClearData;
var t : Integer;
begin
  for t:=0 to High(IData) do
      IData[t].Free;

  IData:=nil;
end;

{
function TBITreePlugin.FindData(const ANode: Integer): TNodeData;
var t : Integer;
begin
  for t:=0 to High(IDatas) do
      if IDatas[t].Node=ANode then
         Exit(IDatas[t]);

  result:=nil;
end;
}

{
function TBITreePlugin.FindNode(const ATag: TObject;
  const AIndex: Integer): Integer;
var t : Integer;
begin
  for t:=0 to High(IDatas) do
      if (IDatas[t].Tag=ATag) and
         (IDatas[t].Index=AIndex) then
            Exit(IDatas[t].Node);

  result:=-1;
end;
}

function TBITreePlugin.NewNodeData(const ATag:TObject; const AIndex:TInteger):TNodeData;
var  L : Integer;
begin
  if ATag=nil then
     result:=nil
  else
  begin
    result:=TNodeData.Create;
    result.Tag:=ATag;
    result.Index:=AIndex;

    L:=Length(IData);
    SetLength(IData,L+1);
    IData[L]:=result;
  end;
end;

{ TBITree.TBITreeNodes }

function TBITree.TBITreeNodes.Count: Integer;
begin
  result:=ITree.Plugin.Count;
end;

function TBITree.TBITreeNodes.Get(const Index: Integer): TBITreeNode;
begin
  result:=ITree.Plugin.GetNode(Index);
end;

initialization
  TBITree.Engine:=TBITreeViewPlugin;
end.
