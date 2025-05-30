{*********************************************}
{  TeeBI Software Library                     }
{  Plugin TBITree class for VCL TeeTree       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Tree.TeeTree;
{$DEFINE FMX}

interface

uses
  System.Classes,
  TeeTree,

  {$IFDEF FMX}
  FMX.Controls, FMXBI.Tree,
  {$ELSE}
  VCL.Controls, VCLBI.Tree,
  {$ENDIF}

  BI.Arrays;

type
  TTeeTreePlugin=class(TBITreePlugin)
  private
    FAllowDelete : Boolean;
    FTree : TTree;
    FTreeOnChange : TNotifyEvent;
    FTreeOnCheck : TNotifyEvent;
    FTreeOnExpanding : TBITreeExpandingEvent;

    procedure TreeChecked(Sender:TObject);
    procedure TreeClick(Sender:TObject);

    {$IFDEF FMX}
    procedure TreeKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    {$ELSE}
    procedure TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
    {$ENDIF}

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
  System.Types, System.UITypes;

{ TTeeTreePlugin }

Constructor TTeeTreePlugin.Create(const AOwner:TComponent);
begin
  inherited;

  FTree:=TTree.Create(AOwner);

  {$IFDEF FMX}
  FTree.Stored:=False;
  {$ENDIF}

  FTree.ReadOnly:=True;

  {$IFNDEF FMX}
  FTree.BevelInner:=bvNone;
  FTree.BevelOuter:=bvNone;
  FTree.Border.Show;
  {$ENDIF}

  FTree.Page.UsePrinter:=False;
  FTree.GlobalFormat.Border.Hide;

//  FTree.Selected.Hide:=False;

  FTree.OnClick:=TreeClick;
  FTree.OnKeyUp:=TreeKeyUp;
  FTree.OnCheckedShape:=TreeChecked;
end;

procedure TTeeTreePlugin.TreeChecked(Sender: TObject);
begin
  if Assigned(FTreeOnCheck) then
     FTreeOnCheck(Sender);
end;

procedure TTeeTreePlugin.TreeClick(Sender:TObject);
begin
  if Assigned(FTreeOnChange) then
     FTreeOnChange(Sender);
end;

function TTeeTreePlugin.TextOf(const ANode: TBITreeNode): String;
begin
  if ANode=nil then
     result:=''
  else
     result:=TTreeNodeShape(ANode).SimpleText;
end;

type
  TTreeNodeShapeAccess=class(TTreeNodeShape);

{$IFDEF FMX}
procedure TTeeTreePlugin.TreeKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ELSE}
procedure TTeeTreePlugin.TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
{$ENDIF}
begin
  if Key=vkDelete then
     if AllowDelete and (Selected<>nil) then
        FOnDelete(Self);
end;

procedure TTeeTreePlugin.EndUpdating;
begin
  FTree.EndUpdate;
end;

procedure TTeeTreePlugin.Expand(const ANode: TBITreeNode;
  const DoExpand,Recursive: Boolean);
var tmp : TTreeNodeShape;
begin
  if ANode=nil then
  begin
    if Count>0 then
       tmp:=FTree.Items[0]
    else
       tmp:=nil;
  end
  else
    tmp:=TTreeNodeShape(ANode);

  if tmp<>nil then
     tmp.Expanded:=DoExpand; // Pending: (Recursive)
end;

procedure TTeeTreePlugin.Expand(const AIndex: Integer);
begin
  FTree.Items[AIndex].Expanded:=True;
end;

function TTeeTreePlugin.Find(const ATag: TObject; const AIndex: Integer): TBITreeNode;
var tmp : TTreeNodeShape;
    tmpData : TNodeData;
    t : Integer;
begin
  for t:=0 to FTree.Items.Count-1 do
  begin
    tmp:=FTree.Items[t];

    if tmp.TagObject is TNodeData then
    begin
      tmpData:=TNodeData(tmp.TagObject);

      if (tmpData.Tag=ATag) and (tmpData.Index=AIndex) then
         Exit(TBITreeNode(tmp));
    end;
  end;

  result:=nil;
end;

function TTeeTreePlugin.FirstNode: TBITreeNode;
begin
  if FTree.Items.Count=0 then
     result:=nil
  else
     result:=TBITreeNode(FTree.Items[0]);
end;

function TTeeTreePlugin.GetAllowDelete: Boolean;
begin
  result:=FAllowDelete;
end;

function TTeeTreePlugin.GetControl: TControl;
begin
  result:=FTree;
end;

function TTeeTreePlugin.GetCount: Integer;
begin
  result:=FTree.Items.Count;
end;

function TTeeTreePlugin.DataOf(const ANode: TBITreeNode): TObject;
var tmp : TObject;
begin
  if ANode=nil then
     result:=nil
  else
  begin
    tmp:=TTreeNodeShape(ANode).TagObject;

    if tmp=nil then
       result:=nil
    else
       result:=TBITreePlugin.TNodeData(tmp).Tag;
  end;
end;

function TTeeTreePlugin.GetNode(const AIndex: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.Items[AIndex]);
end;

function TTeeTreePlugin.GetOnChange: TNotifyEvent;
begin
  result:=FTreeOnChange;
end;

function TTeeTreePlugin.GetOnCheck: TNotifyEvent;
begin
  result:=FTreeOnCheck;
end;

function TTeeTreePlugin.GetOnExpanding: TBITreeExpandingEvent;
begin
  result:=FTreeOnExpanding;
end;

function TTeeTreePlugin.GetSelected: TBITreeNode;
begin
  result:=TBITreeNode(FTree.Selected);
end;

function TTeeTreePlugin.GetSelectedData: TBITreePlugin.TNodeData;
var tmp : TTreeNodeShape;
begin
  tmp:=TTreeNodeShape(Selected);

  if (tmp<>nil) and (tmp.TagObject is TBITreePlugin.TNodeData) then
     result:=TBITreePlugin.TNodeData(tmp.TagObject)
  else
     result:=nil;
end;

function TTeeTreePlugin.IsChecked(const ANode: TBITreeNode): Boolean;
begin
  result:=TTreeNodeShapeAccess(ANode).HasCheckBox and
          TTreeNodeShape(ANode).Checked;
end;

function TTeeTreePlugin.IsExpanded(const ANode: TBITreeNode): Boolean;
begin
  result:=TTreeNodeShape(ANode).Expanded;
end;

procedure TTeeTreePlugin.BeginUpdating;
begin
  FTree.BeginUpdate;
end;

function TTeeTreePlugin.Children(const ANode: TBITreeNode;
  const AIndex: Integer): TBITreeNode;
begin
  if ANode=nil then
     result:=nil
  else
     result:=TBITreeNode(TTreeNodeShape(ANode).Children[AIndex]);
end;

function TTeeTreePlugin.ChildrenCount(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=0
  else
     result:=TTreeNodeShape(ANode).Count;
end;

procedure TTeeTreePlugin.Clear(const ANode:TBITreeNode=nil);
begin
  if ANode=nil then
  begin
    ClearData;
    FTree.Clear;
  end
  else
  begin
    ClearData(ANode);

    // Pending: ANode.Children.Clear

    while TTreeNodeShape(ANode).Children.Count>0 do
          TTreeNodeShape(ANode).Children[0].Free;
  end;
end;

procedure TTeeTreePlugin.ClearData(const ANode: TBITreeNode);
var tmp : TObject;
    t : Integer;
begin
  tmp:=TTreeNodeShape(ANode).TagObject;

  if tmp<>nil then
     DeleteData(TNodeData(tmp));

  for t:=0 to ChildrenCount(ANode)-1 do
      ClearData(Children(ANode,t));
end;

function TTeeTreePlugin.NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject; const AIndex:TInteger):TBITreeNode;
var tmp : TTreeNodeShape;
begin
  tmp:=FTree.Items.AddChild(TTreeNodeShape(AParent),AText);
  tmp.TagObject:=NewNodeData(ATag,AIndex);

  result:=TBITreeNode(tmp);
end;

function TTeeTreePlugin.NextNode(const ANode: TBITreeNode): TBITreeNode;
var tmp : Integer;
begin
  if ANode=nil then
     result:=nil
  else
  begin
    tmp:=FTree.Items.IndexOf(ANode);

    if tmp=-1 then
       result:=nil
    else
    if tmp<FTree.Items.Count-1 then
       result:=TBITreeNode(FTree.Items[tmp+1])
    else
       result:=nil
  end;
end;

function TTeeTreePlugin.NodeAt(const X, Y: Integer): TBITreeNode;
begin
  result:=TBITreeNode(FTree.Items.Clicked(X,Y));
end;

function TTeeTreePlugin.ParentOf(const ANode: TBITreeNode): TBITreeNode;
begin
  result:=TBITreeNode(TTreeNodeShape(ANode).Parent);
end;

function TTeeTreePlugin.SelectedText: String;
begin
  result:=TextOf(Selected);
end;

procedure TTeeTreePlugin.SetAllowDelete(const Value: Boolean);
begin
  FAllowDelete:=Value;
end;

procedure TTeeTreePlugin.SetChecked(const ANode: TBITreeNode;
  const Value: Boolean);
var tmp : Boolean;
begin
  tmp:=IsChecked(ANode);

  if tmp<>Value then
  begin
    TTreeNodeShape(ANode).Checked:=not tmp;

    if Assigned(FTreeOnCheck) then
       FTreeOnCheck(FTree);
  end;
end;

procedure TTeeTreePlugin.SetData(const ANode: TBITreeNode; const AData: TObject);
begin
  TBITreePlugin.TNodeData(TTreeNodeShape(ANode).TagObject).Tag:=AData;
end;

procedure TTeeTreePlugin.SetOnChange(const Value: TNotifyEvent);
begin
  FTreeOnChange:=Value;
end;

procedure TTeeTreePlugin.SetOnCheck(const Value: TNotifyEvent);
begin
  FTreeOnCheck:=Value;
end;

procedure TTeeTreePlugin.SetOnExpanding(const Value: TBITreeExpandingEvent);
begin
  FTreeOnExpanding:=Value;
end;

procedure TTeeTreePlugin.SetSelected(const Value: TBITreeNode);
begin
  FTree.Selected.Clear;

  if Value<>nil then
     FTree.Selected.Add(TTreeNodeShape(Value));
end;

procedure TTeeTreePlugin.SetText(const ANode: TBITreeNode;
  const Value: String);
begin
  TTreeNodeShape(ANode).SimpleText:=Value;
end;

function TTeeTreePlugin.SiblingIndex(const ANode: TBITreeNode): Integer;
begin
  if ANode=nil then
     result:=-1
  else
     result:=TTreeNodeShape(ANode).BrotherIndex;
end;

end.
