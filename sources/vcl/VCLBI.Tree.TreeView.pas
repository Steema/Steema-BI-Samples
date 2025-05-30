{*********************************************}
{  TeeBI Software Library                     }
{  Plugin TBITree class for VCL TTreeView     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Tree.TreeView;

interface

uses
  System.Classes, VCL.Controls, VCL.ComCtrls,
  BI.Arrays, VCLBI.Tree;

type
  TBITreeViewPlugin=class(TBITreePlugin)
  private
    FAllowDelete : Boolean;
    FTree : TTreeView;
    FTreeOnChange : TNotifyEvent;
    FTreeOnCheck : TNotifyEvent;
    FTreeOnExpanding : TBITreeExpandingEvent;

    IClearing : Boolean;

    procedure ChangedTree(Sender: TObject; Node: TTreeNode);
    procedure ToggleChecked(const ANode:TBITreeNode);
    procedure TreeClick(Sender:TObject);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
                            var AllowExpansion: Boolean);
    procedure TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
  System.Types,
  {$IFDEF FPC}
  LCLType,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  VCL.Graphics;

const
  Img_UnChecked=1;
  Img_Checked=2;

{ TBITreeViewPlugin }

Constructor TBITreeViewPlugin.Create(const AOwner:TComponent);
begin
  inherited;

  FTree:=TTreeView.Create(AOwner);
  FTree.ReadOnly:=True;
  FTree.OnChange:=ChangedTree;
  FTree.HideSelection:=False;

  FTree.OnMouseUp:=TreeMouseUp;
  FTree.OnClick:=TreeClick;
  FTree.OnKeyUp:=TreeKeyUp;
  FTree.OnExpanding:=TreeExpanding;
end;

procedure TBITreeViewPlugin.TreeMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var tmp : TBITreeNode;
begin
  tmp:=NodeAt(X,Y);

  if tmp=nil then
     Selected:=nil;
end;

function TBITreeViewPlugin.TextOf(const ANode: TBITreeNode): String;
begin
  if ANode=nil then
     result:=''
  else
     result:=TTreeNode(ANode).Text;
end;

procedure TBITreeViewPlugin.ToggleChecked(const ANode:TBITreeNode);
begin
  SetChecked(ANode,not IsChecked(ANode));
end;

procedure TBITreeViewPlugin.TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
var tmp : TBITreeNode;
begin
  tmp:=Selected;

  if Key={$IFDEF FPC}vk_Delete{$ELSE}vkDelete{$ENDIF} then
  begin
    if AllowDelete and (tmp<>nil) then
       FOnDelete(Self);
  end
  else
  if Key={$IFDEF FPC}vk_Space{$ELSE}vkSpace{$ENDIF} then
     if (tmp<>nil) and (FTree.StateImages<>nil) then
        ToggleChecked(tmp);
end;

procedure TBITreeViewPlugin.TreeExpanding(Sender: TObject; Node: TTreeNode;
    var AllowExpansion: Boolean);
begin
  if Assigned(FTreeOnExpanding) then
     FTreeOnExpanding(Sender,TBITreeNode(Node),AllowExpansion);
end;

procedure TBITreeViewPlugin.TreeClick(Sender:TObject);
var tmp : TPoint;
    tmpNode : TBITreeNode;
begin
  tmpNode:=Selected;

  if tmpNode<>nil then
  begin
    tmp:=FTree.ScreenToClient(Mouse.CursorPos);

    if (htOnStateIcon in FTree.GetHitTestInfoAt(tmp.X,tmp.Y)) then
       ToggleChecked(tmpNode);
  end;
end;

procedure TBITreeViewPlugin.EndUpdating;
begin
  FTree.Items.EndUpdate;
end;

procedure TBITreeViewPlugin.Expand(const ANode: TBITreeNode;
  const DoExpand,Recursive: Boolean);
var tmp : TTreeNode;
begin
  if ANode=nil then
  begin
    if Count>0 then
       tmp:=FTree.Items[0]
    else
       tmp:=nil;
  end
  else
    tmp:=TTreeNode(ANode);

  if tmp<>nil then
     if DoExpand then
        tmp.Expand(Recursive)
     else
        tmp.Collapse(Recursive);
end;

procedure TBITreeViewPlugin.Expand(const AIndex: Integer);
begin
  FTree.Items[AIndex].Expand(False);
end;

function TBITreeViewPlugin.Find(const ATag: TObject; const AIndex: Integer): TBITreeNode;
var tmp : TTreeNode;
    tmpData : TNodeData;
begin
  tmp:=FTree.Items.GetFirstNode;

  while tmp<>nil do
  begin
    if TObject(tmp.Data) is TNodeData then
    begin
      tmpData:=TNodeData(tmp.Data);

      if (tmpData.Tag=ATag) and (tmpData.Index=AIndex) then
         Exit(TBITreeNode(tmp));
    end;

    tmp:=tmp.GetNext;
  end;

  result:=nil;
end;

function TBITreeViewPlugin.FirstNode: TBITreeNode;
begin
  result:=TBITreeNode(FTree.Items.GetFirstNode);
end;

function TBITreeViewPlugin.GetAllowDelete: Boolean;
begin
  result:=FAllowDelete;
end;

function TBITreeViewPlugin.GetControl: TControl;
begin
  result:=FTree;
end;

function TBITreeViewPlugin.GetCount: Integer;
begin
  result:=FTree.Items.Count;
end;

function TBITreeViewPlugin.DataOf(const ANode: TBITreeNode): TObject;
var tmp : TObject;
begin
  if ANode=nil then
     result:=nil
  else
  begin
    tmp:=TObject(TTreeNode(ANode).Data);

    if tmp=nil then
       result:=nil
    else
       result:=TBITreePlugin.TNodeData(tmp).Tag;
  end;
end;

function TBITreeViewPlugin.GetNode(const AIndex: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.Items[AIndex]);
end;

function TBITreeViewPlugin.GetOnChange: TNotifyEvent;
begin
  result:=FTreeOnChange;
end;

function TBITreeViewPlugin.GetOnCheck: TNotifyEvent;
begin
  result:=FTreeOnCheck;
end;

function TBITreeViewPlugin.GetOnExpanding: TBITreeExpandingEvent;
begin
  result:=FTreeOnExpanding;
end;

function TBITreeViewPlugin.GetSelected: TBITreeNode;
begin
  result:=TBITreeNode(FTree.Selected);
end;

function TBITreeViewPlugin.GetSelectedData: TBITreePlugin.TNodeData;
var tmp : TTreeNode;
begin
  tmp:=TTreeNode(Selected);

  if (tmp<>nil) and (TObject(tmp.Data) is TBITreePlugin.TNodeData) then
     result:=TBITreePlugin.TNodeData(tmp.Data)
  else
     result:=nil;
end;

function TBITreeViewPlugin.IsChecked(const ANode: TBITreeNode): Boolean;
begin
  result:=(FTree.StateImages<>nil) and (TTreeNode(ANode).StateIndex=Img_Checked);
end;

function TBITreeViewPlugin.IsExpanded(const ANode: TBITreeNode): Boolean;
begin
  result:=TTreeNode(ANode).Expanded;
end;

procedure TBITreeViewPlugin.BeginUpdating;
begin
  FTree.Items.BeginUpdate;
end;

procedure TBITreeViewPlugin.ChangedTree(Sender: TObject; Node: TTreeNode);
begin
  if not IClearing then
     if Assigned(FTreeOnChange) then
        FTreeOnChange(Sender);
end;

function TBITreeViewPlugin.Children(const ANode: TBITreeNode;
  const AIndex: Integer): TBITreeNode;
begin
  if ANode=nil then
     result:=nil
  else
     result:=TBITreeNode(TTreeNode(ANode)[AIndex]);
end;

function TBITreeViewPlugin.ChildrenCount(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=0
  else
     result:=TTreeNode(ANode).Count;
end;

procedure TBITreeViewPlugin.Clear(const ANode:TBITreeNode=nil);
begin
  IClearing:=True;
  try
    if ANode=nil then
    begin
      ClearData;
      FTree.Items.Clear;
    end
    else
    begin
      ClearData(ANode);
      TTreeNode(ANode).DeleteChildren;
    end;
  finally
    IClearing:=False;
  end;
end;

procedure TBITreeViewPlugin.ClearData(const ANode: TBITreeNode);
var tmp : TObject;
    t : Integer;
begin
  tmp:=TObject(TTreeNode(ANode).Data);

  if tmp<>nil then
     DeleteData(TNodeData(tmp));

  for t:=0 to ChildrenCount(ANode)-1 do
      ClearData(Children(ANode,t));
end;

function TBITreeViewPlugin.NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject; const AIndex:TInteger):TBITreeNode;
begin
  result:=TBITreeNode(FTree.Items.AddChildObject(TTreeNode(AParent),AText,NewNodeData(ATag,AIndex)));
end;

function TBITreeViewPlugin.NextNode(const ANode: TBITreeNode): TBITreeNode;
begin
  if ANode=nil then
     result:=nil
  else
     result:=TBITreeNode(TTreeNode(ANode).GetNext);
end;

function TBITreeViewPlugin.NodeAt(const X, Y: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.GetNodeAt(X,Y));
end;

function TBITreeViewPlugin.ParentOf(const ANode: TBITreeNode): TBITreeNode;
begin
  result:=TBITreeNode(TTreeNode(ANode).Parent);
end;

function TBITreeViewPlugin.SelectedText: String;
begin
  result:=TextOf(Selected);
end;

procedure TBITreeViewPlugin.SetAllowDelete(const Value: Boolean);
begin
  FAllowDelete:=Value;
end;

procedure TBITreeViewPlugin.SetChecked(const ANode: TBITreeNode;
  const Value: Boolean);

  function Blank:TBitmap;
  begin
    result:=TBitmap.Create;
    result.SetSize(16,16);
    result.Canvas.Rectangle(2,2,14,14);
  end;

  function Checked:TBitmap;
  begin
    result:=TBitmap.Create;
    result.SetSize(16,16);
    result.Canvas.Rectangle(2,2,14,14);
    result.Canvas.MoveTo(3,3);
    result.Canvas.LineTo(13,13);
    result.Canvas.MoveTo(3,13);
    result.Canvas.LineTo(13,3);
  end;

  function UnChecked:TBitmap;
  begin
    result:=TBitmap.Create;
    result.SetSize(16,16);
    result.Canvas.Rectangle(1,1,15,15);
  end;

  procedure AddImage(const AList:TImageList; const ABitmap:TBitmap);
  begin
    AList.Add(ABitmap,nil);
    ABitmap.Free;
  end;

  procedure AddCheckImages;
  var tmp : TImageList;
  begin
    tmp:=TImageList.Create(FTree);

    AddImage(tmp,Blank);
    AddImage(tmp,Unchecked);
    AddImage(tmp,Checked);

    FTree.StateImages:=tmp;
  end;

var tmp : Boolean;
begin
  tmp:=IsChecked(ANode)<>Value;

  if FTree.StateImages=nil then
     AddCheckImages;

  if Value then
     TTreeNode(ANode).StateIndex:=Img_Checked
  else
     TTreeNode(ANode).StateIndex:=Img_UnChecked;

  if tmp and Assigned(FTreeOnCheck) then
     FTreeOnCheck(FTree);
end;

procedure TBITreeViewPlugin.SetData(const ANode: TBITreeNode; const AData: TObject);
begin
  TBITreePlugin.TNodeData(TObject(TTreeNode(ANode).Data)).Tag:=AData;
end;

procedure TBITreeViewPlugin.SetOnChange(const Value: TNotifyEvent);
begin
  FTreeOnChange:=Value;
end;

procedure TBITreeViewPlugin.SetOnCheck(const Value: TNotifyEvent);
begin
  FTreeOnCheck:=Value;
end;

procedure TBITreeViewPlugin.SetOnExpanding(const Value: TBITreeExpandingEvent);
begin
  FTreeOnExpanding:=Value;
end;

procedure TBITreeViewPlugin.SetSelected(const Value: TBITreeNode);
begin
  FTree.Selected:=TObject(Value) as TTreeNode;
end;

procedure TBITreeViewPlugin.SetText(const ANode: TBITreeNode;
  const Value: String);
begin
  TTreeNode(ANode).Text:=Value;
end;

function TBITreeViewPlugin.SiblingIndex(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=-1
  else
     result:=TTreeNode(ANode).Index;
end;

end.
