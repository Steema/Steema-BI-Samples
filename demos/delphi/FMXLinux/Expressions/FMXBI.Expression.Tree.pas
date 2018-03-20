{*********************************************}
{  TeeBI Software Library                     }
{  TExpression Object Tree                    }
{  Copyright (c) 2015-2018 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Expression.Tree;

interface

uses
  BI.Expression, FMX.TreeView;

// Adds all AExpression nodes into ATree

procedure FillTree(const ATree:TTreeView; const AExpression:TExpression);

implementation

procedure FillTree(const ATree:TTreeView; const AExpression:TExpression);

  // Recursive
  procedure AddNodes(const AParent:TTreeViewItem; const AExpression:TExpression);

    function NewNode(const AText:String; const ATag:TObject):TTreeViewItem;
    begin
      result:=TTreeViewItem.Create(ATree);
      result.Text:=AText;

      result.TagObject:=ATag;

      if AParent=nil then
         result.Parent:=ATree
      else
         result.Parent:=AParent;
    end;

    procedure AddArray(const A:TArrayExpression);
    var tmp : TTreeViewItem;
        t : Integer;
    begin
      if A.IsParams then
         tmp:=NewNode('( )',A)
      else
         tmp:=NewNode('[ ]',A);

      for t:=0 to A.Count-1 do
          AddNodes(tmp,A[t]);
    end;

  var tmp : TTreeViewItem;
  begin
    if AExpression=nil then
       NewNode('?',nil)
    else
    if AExpression is TArrayExpression then
       AddArray(TArrayExpression(AExpression))
    else
    if AExpression is TArithmeticExpression then
    begin
      tmp:=NewNode(TArithmeticExpression(AExpression).Operand.ToString,AExpression);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TLogicalExpression then
    begin
      tmp:=NewNode(TLogicalExpression(AExpression).Operand.ToString,AExpression);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TTextLogicalExpression then
    begin
      tmp:=NewNode(TTextLogicalExpression(AExpression).Operand.ToString,AExpression);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TTextOperandExpression then
    begin
      tmp:=NewNode(TTextOperandExpression(AExpression).Operand.ToString,AExpression);
      AddNodes(tmp,TTextOperandExpression(AExpression).Expression);
    end
    else
    if AExpression is TMathExpression then
    begin
      tmp:=NewNode(TMathExpression(AExpression).Operand.ToString,AExpression);
      AddNodes(tmp,TUnaryExpression(AExpression).Expression);
    end
    else
    if AExpression is TUnaryTextExpression then
    begin
      tmp:=NewNode(TUnaryTextExpression(AExpression).Operand.ToString,AExpression);
      AddNodes(tmp,TUnaryTextExpression(AExpression).Expression);
    end
    else
    if AExpression is TUnaryNotExpression then
    begin
      tmp:=NewNode('not',AExpression);
      AddNodes(tmp,TUnaryExpression(AExpression).Expression);
    end
    else
    if AExpression is TIfExpression then
    begin
      tmp:=NewNode(TIfExpression(AExpression).Condition.ToString,AExpression);

      AddNodes(tmp,TIfExpression(AExpression).ThenExpression);
      AddNodes(tmp,TIfExpression(AExpression).ElseExpression);
    end
    else
       NewNode(AExpression.ToString,AExpression)
  end;

begin
  ATree.BeginUpdate;
  try
    ATree.Clear;

    if AExpression<>nil then
       AddNodes(nil,AExpression);
  finally
    ATree.EndUpdate;
  end;
end;

end.
