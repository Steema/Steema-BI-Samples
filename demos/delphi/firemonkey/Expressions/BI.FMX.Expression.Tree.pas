{*********************************************}
{  TeeBI Software Library                     }
{  TExpression Object Tree                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Expression.Tree;

interface

uses
  BI.Expression, FMX.TreeView;

// Adds all AExpression nodes into ATree

procedure FillTree(const ATree:TTreeView; const AExpression:TExpression);

implementation

procedure FillTree(const ATree:TTreeView; const AExpression:TExpression);

  // Recursive
  procedure AddNodes(const AParent:TTreeViewItem; const AExpression:TExpression);

    function NewNode(const AText:String):TTreeViewItem;
    begin
      result:=TTreeViewItem.Create(ATree);
      result.Text:=AText;

      if AParent=nil then
         result.Parent:=ATree
      else
         result.Parent:=AParent;
    end;

  var tmp : TTreeViewItem;
  begin
    if AExpression=nil then
       NewNode('?')
    else
    if AExpression is TArithmeticExpression then
    begin
      tmp:=NewNode(TArithmeticExpression(AExpression).Operand.ToString);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TLogicalExpression then
    begin
      tmp:=NewNode(TLogicalExpression(AExpression).Operand.ToString);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TTextLogicalExpression then
    begin
      tmp:=NewNode(TTextLogicalExpression(AExpression).Operand.ToString);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TMathExpression then
    begin
      tmp:=NewNode(TMathExpression(AExpression).Operand.ToString);
      AddNodes(tmp,TUnaryExpression(AExpression).Expression);
    end
    else
    if AExpression is TTextUnaryExpression then
    begin
      tmp:=NewNode(TTextUnaryExpression(AExpression).Operand.ToString);
      AddNodes(tmp,TTextUnaryExpression(AExpression).Expression);
    end
    else
    if AExpression is TUnaryNotExpression then
    begin
      tmp:=NewNode('not');
      AddNodes(tmp,TUnaryExpression(AExpression).Expression);
    end
    else
    if AExpression is TIfExpression then
    begin
      tmp:=NewNode(TIfExpression(AExpression).Condition.ToString);

      AddNodes(tmp,TIfExpression(AExpression).ThenExpression);
      AddNodes(tmp,TIfExpression(AExpression).ElseExpression);
    end
    else
       NewNode(AExpression.ToString)
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
