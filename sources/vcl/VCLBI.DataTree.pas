unit VCLBI.DataTree;

interface

uses
  VCL.ComCtrls, BI.Arrays,
  {$IFNDEF FPC}
  BI.Tree,
  {$ENDIF}
  BI.DataItem;

// Fills a TreeView control with all Data items, recursively

type
  TDataTree=class
  {$IFNDEF FPC}
  public
    type
      TNodeGetText<T>=reference to function(const ANode:TNode<T>):String;

  private
    class procedure DoFill<T>(const AParent:TTreeNode; const ANode:TNode<T>;
                              const GetText:TNodeGetText<T>); static;
  {$ENDIF}
  public
    class procedure Fill(const Data:TDataItem; const Rows:Boolean; const Tree:TTreeView); overload; static;

    {$IFNDEF FPC}
    class procedure Fill<T>(const Node:TNode<T>; const Rows:Boolean;
                            const Tree:TTreeView;
                            const GetText:TNodeGetText<T>); overload; static;
    {$ENDIF}

    class procedure Fill(const AData: TDataArray; const ATree: TTreeView;
                         const AddLeaves:Boolean=True;
                         const AddNames:Boolean=True); overload; static;

    class procedure Fill(const AData: TDataItem; const ATree: TTreeView;
                         const AddLeaves:Boolean=True;
                         const AddNames:Boolean=True); overload; static;
  end;

implementation

uses
  BI.Languages.English;

{ TDataTree }

class procedure TDataTree.Fill(const Data:TDataItem; const Rows:Boolean; const Tree:TTreeView);

  procedure DoAddItems(const AParent:TTreeNode; const AData:TDataItem);
  var tmpItem : TDataItem;
      tmp : TTreeNode;
  begin
    tmp:=Tree.Items.AddChild(AParent,AData.Name);

    for tmpItem in AData.Items.AsArray do
        DoAddItems(tmp,tmpItem);
  end;

  function HasChildren(const AData:TDataItem):Boolean;
  var tmp : TDataItem;
  begin
    for tmp in AData.Parent.Items.AsArray do
        if tmp.ParentGroup=AData then
           Exit(True);

    result:=False;
  end;

  function MasterHas(const AMaster:TDataItem; const AMasterIndex:TInteger;
                     const ADetail:TDataItem; const ADetailIndex:TInteger):Boolean;
  begin
    result:=True;
  end;

  procedure AddNodes(const AParent:TTreeNode; const AData,Master:TDataItem; const MasterIndex:TInteger);
  var tmp,
      tmp2 : TTreeNode;
      t : TLoopInteger;
      Item : TDataItem;
      Items : TDataItems;
  begin
    tmp:=Tree.Items.AddChild(AParent,AData.Name);

    Items:=AData.Parent.Items;

    for t:=0 to AData.Count-1 do
    begin
      if (t=0) or (not AData.SameData(t,t-1)) then
      if (Master=nil) or MasterHas(Master,MasterIndex,AData,t) then
      begin
        tmp2:=Tree.Items.AddChild(tmp,AData.DataToString(t));

        for Item in Items.AsArray do
            if Item.ParentGroup=AData then
               if HasChildren(Item) then
                  AddNodes(tmp2,Item,AData,t);
      end;
    end;
  end;

var t : Integer;
    tmp : TDataItem;
begin
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;

    if Rows then
    begin
      for t:=0 to Data.Items.Count-1 do
          if Data.Items[t].ParentGroup<>nil then
          begin
            tmp:=Data.Items[t].ParentGroup;

            repeat
              if tmp.ParentGroup=nil then
              begin
                // Found "top" parentgroup node, now add nodes:
                AddNodes(nil,tmp,nil,-1);
                break;
              end
              else
                 tmp:=tmp.ParentGroup;

            until tmp=nil;

            break;
          end;
    end
    else
    begin
      for tmp in Data.Items.AsArray do
          if tmp.ParentGroup=nil then
             if not HasChildren(tmp) then
                DoAddItems(nil,tmp);
    end;

  finally
    Tree.Items.EndUpdate;
  end;
end;

{$IFNDEF FPC}
class procedure TDataTree.DoFill<T>(const AParent:TTreeNode; const ANode:TNode<T>;
                                    const GetText:TNodeGetText<T>);
var i : Integer;
    tmp : TTreeNode;
begin
  for i:=0 to ANode.Count-1 do
  begin
    tmp:=TTreeView(AParent.TreeView).Items.AddChildObject(AParent,GetText(ANode[i]),ANode[i]);
    DoFill<T>(tmp,ANode[i],GetText);
  end;
end;

class procedure TDataTree.Fill<T>(const Node: TNode<T>; const Rows: Boolean;
  const Tree: TTreeView; const GetText:TNodeGetText<T>);
begin
  DoFill<T>(Tree.Items.AddChildObject(nil,GetText(Node),Node),Node,GetText);
end;
{$ENDIF}

class procedure TDataTree.Fill(const AData: TDataArray; const ATree: TTreeView;
          const AddLeaves:Boolean; const AddNames:Boolean);

  function CheckName(const S:String):String;
  begin
    if S='' then
       result:=BIMsg_UnNamed
    else
       result:=S;
  end;

  procedure Add(const AParent:TTreeNode; const ACol:TDataItem);
  var tmp : TDataItem;
      tmpNode : TTreeNode;
  begin
    if AddLeaves or (ACol.Items.Count>0) then
    begin
      if AddNames or ACol.AsTable then
         tmpNode:=ATree.Items.AddChildObject(AParent,CheckName(ACol.Name),ACol)
      else
         tmpNode:=AParent;

      for tmp in ACol.Items.AsArray do
          Add(tmpNode,tmp);
    end;
  end;

var t : Integer;
begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    for t:=Low(AData) to High(AData) do
        Add(nil,AData[t]);
  finally
    ATree.Items.EndUpdate;
  end;
end;

class procedure TDataTree.Fill(const AData: TDataItem; const ATree: TTreeView;
          const AddLeaves:Boolean; const AddNames:Boolean);
begin
  Fill([AData],ATree,AddLeaves,AddNames);
end;

end.
