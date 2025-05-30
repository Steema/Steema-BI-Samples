{************************************************}
{  TeeBI Software Library                        }
{  Plugin TBITree class for VCL TVirtualTreeView }
{  Copyright (c) 2015-2016 by Steema Software    }
{  All Rights Reserved                           }
{************************************************}
unit VCLBI.Tree.VirtualTreeView;

interface

uses
  System.Classes, System.Generics.Collections,
  VCL.Controls, VirtualTrees,
  BI.Arrays, VCLBI.Tree;

type
  TVirtualTreeTexts=TDictionary<PVirtualNode,String>;

  TVirtualTreePlugin=class(TBITreePlugin)
  private
    FAllowDelete : Boolean;
    FTexts : TVirtualTreeTexts;
    FTree : TVirtualStringTree;
    FTreeOnChange : TNotifyEvent;
    FTreeOnCheck : TNotifyEvent;
    FTreeOnExpanding : TBITreeExpandingEvent;

    procedure ChangedTree(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ToggleChecked(const ANode:TBITreeNode);
    procedure TreeClick(Sender:TObject);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
                          TextType: TVSTTextType; var CellText: string);
    procedure TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
  protected
    procedure BeginUpdating; override;
    procedure Clear(const ANode:TBITreeNode=nil); override;
    procedure ClearData(const ANode:TBITreeNode); override;
    procedure EndUpdating; override;
    function GetAllowDelete: Boolean; override;
    function GetControl:TControl; override;
    function GetCount:Integer; override;
    function GetNode(const AIndex:Integer):TBITreeNode; override;
    function GetOnChange: TNotifyEvent; override;
    function GetOnCheck: TNotifyEvent; override;
    function GetOnExpanding: TBITreeExpandingEvent; override;
    function GetSelected:TBITreeNode; override;
    function GetSelectedData: TBITreePlugin.TNodeData; override;
    function NodeAt(const X,Y:Integer):TBITreeNode; override;
    function SelectedText:String; override;
    procedure SetAllowDelete(const Value: Boolean); override;
    procedure SetData(const ANode: TBITreeNode; const AData: TObject); override;
    procedure SetOnChange(const Value: TNotifyEvent); override;
    procedure SetOnCheck(const Value: TNotifyEvent); override;
    procedure SetOnExpanding(const Value: TBITreeExpandingEvent); override;
    procedure SetSelected(const Value: TBITreeNode); override;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    function Children(const ANode:TBITreeNode; const AIndex:Integer):TBITreeNode; override;
    function ChildrenCount(const ANode:TBITreeNode):Integer; override;

    function DataOf(const ANode:TBITreeNode):TObject; override;
    procedure Expand(const AIndex:Integer); override;
    procedure Expand(const ANode:TBITreeNode; const DoExpand:Boolean=True; const Recursive:Boolean=False); override;

    function Find(const ATag:TObject; const AIndex:Integer):TBITreeNode; override;
    function FirstNode:TBITreeNode; override;
    function IsChecked(const ANode:TBITreeNode):Boolean; override;
    function IsExpanded(const ANode:TBITreeNode):Boolean; override;
    function NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject=nil; const AIndex:TInteger=-1):TBITreeNode; override;
    function NextNode(const ANode:TBITreeNode):TBITreeNode; override;
    function ParentOf(const ANode:TBITreeNode):TBITreeNode; override;
    procedure SetChecked(const ANode:TBITreeNode; const Value:Boolean); override;
    procedure SetText(const ANode:TBITreeNode; const Value:String); override;
    function SiblingIndex(const ANode:TBITreeNode):Integer; override;
    function TextOf(const ANode:TBITreeNode):String; override;
  end;

implementation

uses
  System.Types, System.UITypes, VCL.Graphics;

const
  Img_UnChecked=1;
  Img_Checked=2;

{ TVirtualTreePlugin }

Constructor TVirtualTreePlugin.Create(const AOwner:TComponent);
begin
  inherited;

  FTexts:=TVirtualTreeTexts.Create;

  FTree:=TVirtualStringTree.Create(AOwner);

  // Cannot set ReadOnly because it also means new nodes cannot be added:
  //FTree.TreeOptions.MiscOptions:=FTree.TreeOptions.MiscOptions+[toReadOnly];

  FTree.OnGetText:=TreeGetText;

  FTree.OnChange:=ChangedTree;

  //FTree.HideSelection:=False;

  FTree.OnClick:=TreeClick;
  FTree.OnKeyUp:=TreeKeyUp;
end;

function TVirtualTreePlugin.TextOf(const ANode: TBITreeNode): String;
begin
  if ANode=nil then
     result:=''
  else
  if not FTexts.TryGetValue(PVirtualNode(ANode),result) then
     result:='';
end;

procedure TVirtualTreePlugin.ToggleChecked(const ANode:TBITreeNode);
begin
  SetChecked(ANode,not IsChecked(ANode));
end;

procedure TVirtualTreePlugin.TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
var tmp : TBITreeNode;
begin
  tmp:=Selected;

  if Key=vkDelete then
  begin
    if AllowDelete and (tmp<>nil) then
       FOnDelete(Self);
  end
  else
  if Key=vkSpace then
     if (tmp<>nil) and (FTree.StateImages<>nil) then
        ToggleChecked(tmp);
end;

procedure TVirtualTreePlugin.TreeClick(Sender:TObject);
var tmp : TPoint;
    tmpNode : TBITreeNode;
begin
  tmpNode:=Selected;

  if tmpNode<>nil then
  begin
    tmp:=FTree.ScreenToClient(Mouse.CursorPos);

    //if (htOnStateIcon in FTree.GetHitTestInfoAt(tmp.X,tmp.Y)) then
    //   ToggleChecked(tmpNode);
  end;
end;

procedure TVirtualTreePlugin.TreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  CellText:=FTexts[Node];
end;

procedure TVirtualTreePlugin.EndUpdating;
begin
  FTree.EndUpdate;
end;

procedure TVirtualTreePlugin.Expand(const ANode: TBITreeNode;
  const DoExpand,Recursive: Boolean);
var tmp : PVirtualNode;
begin
  if ANode=nil then
  begin
    if Count>0 then
       tmp:=FTree.RootNode
    else
       tmp:=nil;
  end
  else
    tmp:=PVirtualNode(ANode);

  if tmp<>nil then
     FTree.Expanded[tmp]:=DoExpand; // Recursive
end;

procedure TVirtualTreePlugin.Expand(const AIndex: Integer);
begin
//  FTree.Expanded[GetNodeAt(AIndex)]:=True
end;

function TVirtualTreePlugin.Find(const ATag: TObject; const AIndex: Integer): TBITreeNode;
var tmp : PVirtualNode;
    tmpData : TNodeData;
begin
  tmp:=FTree.GetFirst(True);

  while tmp<>nil do
  begin
    if TObject(tmp.GetData) is TNodeData then
    begin
      tmpData:=TNodeData(tmp.GetData);

      if (tmpData.Tag=ATag) and (tmpData.Index=AIndex) then
         Exit(TBITreeNode(tmp));
    end;

    tmp:=FTree.GetNext(tmp,True);
  end;

  result:=nil;
end;

function TVirtualTreePlugin.FirstNode: TBITreeNode;
begin
  result:=TBITreeNode(FTree.GetFirst(True));
end;

function TVirtualTreePlugin.GetAllowDelete: Boolean;
begin
  result:=FAllowDelete;
end;

function TVirtualTreePlugin.GetControl: TControl;
begin
  result:=FTree;
end;

function TVirtualTreePlugin.GetCount: Integer;
begin
  result:=FTree.TotalCount;
end;

function TVirtualTreePlugin.DataOf(const ANode: TBITreeNode): TObject;
var tmp : TObject;
begin
  if ANode=nil then
     result:=nil
  else
  begin
    tmp:=TObject(PVirtualNode(ANode).GetData);

    if tmp=nil then
       result:=nil
    else
       result:=TBITreePlugin.TNodeData(tmp).Tag;
  end;
end;

destructor TVirtualTreePlugin.Destroy;
begin
  FTexts.Free;
  inherited;
end;

function TVirtualTreePlugin.GetNode(const AIndex: Integer): TBITreeNode;
var tmp : PVirtualNode;
begin
  for tmp in FTree.Nodes do
      if tmp.Index=Cardinal(AIndex) then
         Exit(TBITreeNode(tmp));

  result:=nil;
end;

function TVirtualTreePlugin.GetOnChange: TNotifyEvent;
begin
  result:=FTreeOnChange;
end;

function TVirtualTreePlugin.GetOnCheck: TNotifyEvent;
begin
  result:=FTreeOnCheck;
end;

function TVirtualTreePlugin.GetOnExpanding: TBITreeExpandingEvent;
begin
  result:=FTreeOnExpanding;
end;

function TVirtualTreePlugin.GetSelected: TBITreeNode;
var tmp : TVTVirtualNodeEnumeration;
    tmpEnum : TVTVirtualNodeEnumerator;
begin
  if FTree.SelectedCount>0 then
  begin
    tmp:=FTree.SelectedNodes(True);

    tmpEnum:=tmp.GetEnumerator;

    result:=TBITreeNode(tmpEnum.Current);
  end
  else
     result:=nil
end;

function TVirtualTreePlugin.GetSelectedData: TBITreePlugin.TNodeData;
var tmp : PVirtualNode;
begin
  tmp:=PVirtualNode(Selected);

  if (tmp<>nil) and (TObject(tmp.GetData) is TBITreePlugin.TNodeData) then
     result:=TBITreePlugin.TNodeData(tmp.GetData)
  else
     result:=nil;
end;

function TVirtualTreePlugin.IsChecked(const ANode: TBITreeNode): Boolean;
begin
  result:=PVirtualNode(ANode).CheckState=TCheckState.csCheckedNormal;
end;

function TVirtualTreePlugin.IsExpanded(const ANode: TBITreeNode): Boolean;
begin
  result:=TVirtualNodeState.vsExpanded in PVirtualNode(ANode).States;
end;

procedure TVirtualTreePlugin.BeginUpdating;
begin
  FTree.BeginUpdate;
end;

procedure TVirtualTreePlugin.ChangedTree(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Assigned(FTreeOnChange) then
     FTreeOnChange(Sender);
end;

function TVirtualTreePlugin.Children(const ANode: TBITreeNode;
  const AIndex: Integer): TBITreeNode;
var tmp : PVirtualNode;
    tmpIndex : Integer;
begin
  if ANode=nil then
     result:=nil
  else
  begin
    tmp:=PVirtualNode(ANode);

    if tmp.ChildCount>Cardinal(AIndex) then
    begin
      tmpIndex:=AIndex;

      result:=TBITreeNode(tmp.FirstChild);

      while tmpIndex>0 do
      begin
        result:=TBITreeNode(PVirtualNode(result).NextSibling);
        Dec(tmpIndex);
      end;
    end
    else
      result:=nil;
  end;
end;

function TVirtualTreePlugin.ChildrenCount(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=0
  else
     result:=PVirtualNode(ANode).ChildCount;
end;

procedure TVirtualTreePlugin.Clear(const ANode:TBITreeNode=nil);
begin
  if ANode=nil then
  begin
    ClearData;
    FTree.Clear;
  end
  else
  begin
    ClearData(ANode);
    FTree.DeleteChildren(PVirtualNode(ANode));
  end;
end;

procedure TVirtualTreePlugin.ClearData(const ANode: TBITreeNode);
var tmp : TObject;
    t : Integer;
begin
  tmp:=TObject(PVirtualNode(ANode).GetData);

  if tmp<>nil then
     DeleteData(TNodeData(tmp));

  for t:=0 to ChildrenCount(ANode)-1 do
      ClearData(Children(ANode,t));
end;

function TVirtualTreePlugin.NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject; const AIndex:TInteger):TBITreeNode;
var tmp : PVirtualNode;
begin
  FTree.BeginUpdate;
  try
    tmp:=FTree.AddChild(PVirtualNode(AParent),NewNodeData(ATag,AIndex));
    result:=TBITreeNode(tmp);

    SetText(result,AText);
  finally
    FTree.EndUpdate;
  end;
end;

function TVirtualTreePlugin.NextNode(const ANode: TBITreeNode): TBITreeNode;
begin
  if ANode=nil then
     result:=nil
  else
     result:=TBITreeNode(FTree.GetNext(PVirtualNode(ANode),True));
end;

function TVirtualTreePlugin.NodeAt(const X, Y: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.GetNodeAt(X,Y));
end;

function TVirtualTreePlugin.ParentOf(const ANode: TBITreeNode): TBITreeNode;
begin
  if PVirtualNode(ANode).Parent=FTree.RootNode then
     result:=nil
  else
     result:=TBITreeNode(PVirtualNode(ANode).Parent);
end;

function TVirtualTreePlugin.SelectedText: String;
begin
  result:=TextOf(Selected);
end;

procedure TVirtualTreePlugin.SetAllowDelete(const Value: Boolean);
begin
  FAllowDelete:=Value;
end;

procedure TVirtualTreePlugin.SetChecked(const ANode: TBITreeNode;
  const Value: Boolean);
var tmp : Boolean;
begin
  tmp:=IsChecked(ANode);

  if tmp<>Value then
  begin
    if PVirtualNode(ANode).CheckState=TCheckState.csUncheckedNormal then
       PVirtualNode(ANode).CheckState:=TCheckState.csCheckedNormal
    else
       PVirtualNode(ANode).CheckState:=TCheckState.csUnCheckedNormal;

    if Assigned(FTreeOnCheck) then
       FTreeOnCheck(FTree);
  end;
end;

procedure TVirtualTreePlugin.SetData(const ANode: TBITreeNode; const AData: TObject);
begin
  TBITreePlugin.TNodeData(TObject(PVirtualNode(ANode).GetData)).Tag:=AData;
end;

procedure TVirtualTreePlugin.SetOnChange(const Value: TNotifyEvent);
begin
  FTreeOnChange:=Value;
end;

procedure TVirtualTreePlugin.SetOnCheck(const Value: TNotifyEvent);
begin
  FTreeOnCheck:=Value;
end;

procedure TVirtualTreePlugin.SetOnExpanding(const Value: TBITreeExpandingEvent);
begin
  FTreeOnExpanding:=Value;
end;

procedure TVirtualTreePlugin.SetSelected(const Value: TBITreeNode);
begin
  FTree.Selected[PVirtualNode(Value)]:=True;
end;

procedure TVirtualTreePlugin.SetText(const ANode: TBITreeNode;
  const Value: String);
begin
  FTexts.AddOrSetValue(PVirtualNode(ANode),Value);
end;

function TVirtualTreePlugin.SiblingIndex(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=-1
  else
     result:=PVirtualNode(ANode).Index;
end;

end.
