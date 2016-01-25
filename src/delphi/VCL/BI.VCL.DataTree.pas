unit BI.VCL.DataTree;

interface

uses
  VCL.ComCtrls, BI.Arrays,
  {$IFNDEF FPC}
  BI.Tree,
  {$ENDIF}
  BI.Data;

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

    class procedure Fill(const AData: TDataItem; const ATree: TTreeView;
                         const AddLeaves:Boolean=True;
                         const AddNames:Boolean=True); overload; static;
  end;

implementation
