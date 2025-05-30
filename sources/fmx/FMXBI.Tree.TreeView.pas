{*********************************************}
{  TeeBI Software Library                     }
{  Plugin TBITree class for FMX TTreeView     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Tree.TreeView;

interface

uses
  System.Classes,
  FMX.Controls, FMX.TreeView,
  BI.Arrays, FMXBI.Tree;

type
  TBITreeViewPlugin=class(TBITreePlugin)
  private
    FAllowDelete : Boolean;
    FTree : TTreeView;
    FTreeOnExpanding : TBITreeExpandingEvent;
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
    function GetSelectedData:TBITreePlugin.TNodeData; override;
    function NodeAt(const X,Y:Integer):TBITreeNode; override;
    function SelectedText:String; override;
    procedure SetAllowDelete(const Value: Boolean); override;
    procedure SetData(const ANode:TBITreeNode; const AData:TObject); override;
    procedure SetOnChange(const Value: TNotifyEvent); override;
    procedure SetOnCheck(const Value: TNotifyEvent); override;
    procedure SetOnExpanding(const Value: TBITreeExpandingEvent); override;
    procedure SetSelected(const Value: TBITreeNode); override;
  public
    Constructor Create(const AOwner:TComponent); override;

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
              const ATag:TObject=nil;
              const AIndex:TInteger=-1):TBITreeNode; override;
    function NextNode(const ANode:TBITreeNode):TBITreeNode; override;
    function ParentOf(const ANode:TBITreeNode):TBITreeNode; override;
    procedure SetChecked(const ANode:TBITreeNode; const Value:Boolean); override;
    procedure SetText(const ANode:TBITreeNode; const Value:String); override;
    function SiblingIndex(const ANode:TBITreeNode):Integer; override;
    function TextOf(const ANode:TBITreeNode):String; override;
  end;

implementation

{ TBITreeViewPlugin }

procedure TBITreeViewPlugin.ClearData(const ANode: TBITreeNode);
var tmp : TObject;
    t : Integer;
begin
  tmp:=DataOf(ANode);

  if tmp<>nil then
     DeleteData(TNodeData(tmp));

  for t:=0 to ChildrenCount(ANode)-1 do
      ClearData(Children(ANode,t));
end;

Constructor TBITreeViewPlugin.Create(const AOwner:TComponent);
begin
  inherited;

  FTree:=TTreeView.Create(AOwner);
  FTree.Stored:=False;
end;

function TBITreeViewPlugin.DataOf(const ANode: TBITreeNode): TObject;
begin
  result:=TTreeViewItem(ANode).TagObject;
end;

procedure TBITreeViewPlugin.EndUpdating;
begin
  FTree.EndUpdate;
end;

procedure TBITreeViewPlugin.Expand(const ANode: TBITreeNode; const DoExpand,
  Recursive: Boolean);
begin
  if DoExpand then
     if Recursive then
        TTreeViewItem(ANode).ExpandAll
     else
        TTreeViewItem(ANode).Expand
  else
  if Recursive then
     TTreeViewItem(ANode).CollapseAll
  else
     TTreeViewItem(ANode).Collapse;
end;

procedure TBITreeViewPlugin.Expand(const AIndex: Integer);
begin
  FTree.ItemByGlobalIndex(AIndex).Expand;
end;

function TBITreeViewPlugin.Find(const ATag: TObject;
  const AIndex: Integer): TBITreeNode;

  function Matches(const ANode:TTreeViewItem):Boolean;
  var tmp : TNodeData;
  begin
    if ANode.TagObject is TNodeData then
    begin
      tmp:=TNodeData(ANode.TagObject);

      result:=(tmp.Tag=ATag) and (tmp.Index=AIndex);
    end
    else
      result:=False;
  end;

  function Traverse(const ANode:TTreeViewItem):TTreeViewItem;
  var t : Integer;
  begin
    if Matches(ANode) then
       Exit(ANode)
    else
    begin
      for t:=0 to ANode.Count-1 do
      begin
        result:=Traverse(ANode.Items[t]);

        if result<>nil then
           Exit;
      end;
    end;

    result:=nil;
  end;

var tmp : TTreeViewItem;
    t : Integer;
begin
  for t:=0 to FTree.Count-1 do
  begin
    tmp:=Traverse(FTree.Items[t]);

    if tmp<>nil then
       Exit(TBITreeNode(tmp));
  end;

  result:=nil;
end;

function TBITreeViewPlugin.FirstNode: TBITreeNode;
begin
  if Count=0 then
     result:=nil
  else
     result:=TBITreeNode(FTree.Items[0]);
end;

function TBITreeViewPlugin.GetSelected: TBITreeNode;
begin
  result:=TBITreeNode(FTree.Selected);
end;

function TBITreeViewPlugin.GetSelectedData: TBITreePlugin.TNodeData;
var tmp : TTreeViewItem;
begin
  tmp:=TTreeViewItem(Selected);

  if (tmp<>nil) and (tmp.TagObject is TBITreePlugin.TNodeData) then
     result:=TBITreePlugin.TNodeData(tmp.TagObject)
  else
     result:=nil;
end;

function TBITreeViewPlugin.IsChecked(const ANode: TBITreeNode): Boolean;
begin
  result:=TTreeViewItem(ANode).IsChecked;
end;

function TBITreeViewPlugin.GetAllowDelete: Boolean;
begin
  result:=FAllowDelete;
end;

function TBITreeViewPlugin.IsExpanded(const ANode: TBITreeNode): Boolean;
begin
  result:=TTreeViewItem(ANode).IsExpanded;
end;

function TBITreeViewPlugin.GetControl: TControl;
begin
  result:=FTree;
end;

function TBITreeViewPlugin.GetCount: Integer;
begin
  result:=FTree.GlobalCount;
end;

function TBITreeViewPlugin.GetNode(const AIndex: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.ItemByGlobalIndex(AIndex));
end;

function TBITreeViewPlugin.GetOnChange: TNotifyEvent;
begin
  result:=FTree.OnChange;
end;

procedure TBITreeViewPlugin.BeginUpdating;
begin
  FTree.BeginUpdate;
end;

function TBITreeViewPlugin.GetOnCheck: TNotifyEvent;
begin
  result:=FTree.OnChangeCheck;
end;

function TBITreeViewPlugin.GetOnExpanding: TBITreeExpandingEvent;
begin
  result:=FTreeOnExpanding;
end;

function TBITreeViewPlugin.Children(const ANode: TBITreeNode;
  const AIndex: Integer): TBITreeNode;
begin
  if ANode=nil then
     result:=nil
  else
     result:=TBITreeNode(TTreeViewItem(ANode).Items[AIndex]);
end;

function TBITreeViewPlugin.ChildrenCount(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=0
  else
     result:=TTreeViewItem(ANode).Count
end;

procedure TBITreeViewPlugin.Clear(const ANode:TBITreeNode=nil);
begin
  if ANode=nil then
  begin
    ClearData;
    FTree.Clear;
  end
  else
  begin
    ClearData(ANode);
    TTreeViewItem(ANode).DeleteChildren;
  end;
end;

function TBITreeViewPlugin.NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject; const AIndex:TInteger):TBITreeNode;
var tmp : TNodeData;
begin
  tmp:=NewNodeData(ATag,AIndex);

  result:=TBITreeNode(TTreeViewItem.Create(FTree));
  TTreeViewItem(result).Text:=AText;
  TTreeViewItem(result).TagObject:=tmp;

  if AParent=nil then
     FTree.AddObject(TTreeViewItem(result))
  else
     TTreeViewItem(result).Parent:=TTreeViewItem(AParent);
end;

function TBITreeViewPlugin.NextNode(const ANode: TBITreeNode): TBITreeNode;
var tmp : Integer;
begin
  if ANode=nil then
     result:=nil
  else
  begin
    tmp:=TTreeViewItem(ANode).GlobalIndex;

    if tmp<Count-1 then
       result:=TBITreeNode(FTree.ItemByGlobalIndex(tmp+1))
    else
       result:=nil;
  end;
end;

function TBITreeViewPlugin.NodeAt(const X, Y: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.ItemByPoint(X,Y));
end;

function TBITreeViewPlugin.ParentOf(const ANode: TBITreeNode): TBITreeNode;
begin
  result:=TBITreeNode(TTreeViewItem(ANode).ParentItem);
end;

function TBITreeViewPlugin.SelectedText: String;
begin
  if FTree.Selected=nil then
     result:=''
  else
     result:=FTree.Selected.Text;
end;

procedure TBITreeViewPlugin.SetAllowDelete(const Value: Boolean);
begin
  FAllowDelete:=Value;
end;

procedure TBITreeViewPlugin.SetChecked(const ANode: TBITreeNode;
  const Value: Boolean);
var tmp : Boolean;
begin
  tmp:=IsChecked(ANode)<>Value;

  if tmp then
  begin
    TTreeViewItem(ANode).IsChecked:=Value;

    //if Assigned(FTreeOnCheck) then
    //   FTreeOnCheck(FTree);
  end;
end;

procedure TBITreeViewPlugin.SetData(const ANode: TBITreeNode;
  const AData: TObject);
begin
end;

procedure TBITreeViewPlugin.SetOnChange(const Value: TNotifyEvent);
begin
  FTree.OnChange:=Value;
end;

procedure TBITreeViewPlugin.SetOnCheck(const Value: TNotifyEvent);
begin
  FTree.OnChangeCheck:=Value;
end;

procedure TBITreeViewPlugin.SetOnExpanding(const Value: TBITreeExpandingEvent);
begin
  FTreeOnExpanding:=Value;
end;

procedure TBITreeViewPlugin.SetSelected(const Value: TBITreeNode);
begin
  FTree.Selected:=TTreeViewItem(Value);
end;

procedure TBITreeViewPlugin.SetText(const ANode: TBITreeNode;
  const Value: String);
begin
  TTreeViewItem(ANode).Text:=Value;
end;

function TBITreeViewPlugin.SiblingIndex(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=-1
  else
     result:=TTreeViewItem(ANode).Index;
end;

function TBITreeViewPlugin.TextOf(const ANode: TBITreeNode): String;
begin
  if ANode=nil then
     result:=''
  else
     result:=TTreeViewItem(ANode).Text;
end;

end.
