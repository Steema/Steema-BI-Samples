unit BI.VCL.Tree.TreeView;

interface

uses
  System.Classes, VCL.Controls, VCL.ComCtrls,
  BI.Arrays, BI.VCL.Tree;

type
  TBITreeViewPlugin=class(TBITreePlugin)
  private
    FAllowDelete : Boolean;
    FTree : TTreeView;
    FTreeOnChange : TNotifyEvent;
    FTreeOnCheck : TNotifyEvent;

    procedure ChangedTree(Sender: TObject; Node: TTreeNode);
    procedure ToggleChecked(const ANode:TBITreeNode);
    procedure TreeClick(Sender:TObject);
    procedure TreeKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
  protected
    procedure BeginUpdating; override;
    procedure Clear; override;
    procedure EndUpdating; override;
    function GetAllowDelete: Boolean; override;
    function GetControl:TControl; override;
    function GetCount:Integer; override;
    function GetData(const ANode:TBITreeNode):TObject; override;
    function GetNode(const AIndex:Integer):TBITreeNode; override;
    function GetOnChange: TNotifyEvent; override;
    function GetOnCheck: TNotifyEvent; override;
    function GetSelected:TBITreeNode; override;
    function GetSelectedData: TBITreePlugin.TNodeData; override;
    function NodeAt(const X,Y:Integer):TBITreeNode; override;
    function SelectedText:String; override;
    procedure SetAllowDelete(const Value: Boolean); override;
    procedure SetData(const ANode: TBITreeNode; const AData: TObject); override;
    procedure SetOnChange(const Value: TNotifyEvent); override;
    procedure SetOnCheck(const Value: TNotifyEvent); override;
    procedure SetSelected(const Value: TBITreeNode); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    function Children(const ANode:TBITreeNode; const AIndex:Integer):TBITreeNode; override;
    function ChildrenCount(const ANode:TBITreeNode):Integer; override;

    procedure Expand(const AIndex:Integer); override;
    procedure Expand(const ANode:TBITreeNode; const DoExpand:Boolean=True; const Recursive:Boolean=False); override;

    function Find(const ATag:TObject; const AIndex:Integer):TBITreeNode; override;

    function FirstNode:TBITreeNode; override;
    function NextNode(const ANode:TBITreeNode):TBITreeNode; override;

    function IsChecked(const ANode:TBITreeNode):Boolean; override;
    function NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject=nil; const AIndex:TInteger=-1):TBITreeNode; override;
    function ParentOf(const ANode:TBITreeNode):TBITreeNode; override;
    procedure SetChecked(const ANode:TBITreeNode; const Value:Boolean); override;
    function SiblingIndex(const ANode:TBITreeNode):Integer; override;
    function TextOf(const ANode:TBITreeNode):String;  override;
  end;

implementation
