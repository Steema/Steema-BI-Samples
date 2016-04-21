unit BI.VCL.Tree.TreeView;

interface

uses
  System.Classes, VCL.Controls, VCL.ComCtrls,
  BI.Arrays, BI.VCL.Tree;

type
  TBITreeViewPlugin=class(TBITreePlugin)
  private
    FTree : TTreeView;
    FTreeOnChange : TNotifyEvent;

    procedure ChangedTree(Sender: TObject; Node: TTreeNode);
  protected
    procedure Clear; override;
    function GetControl:TControl; override;
    function GetCount:Integer; override;
    function GetOnChange: TNotifyEvent; override;
    function GetSelected:TBITreeNode; override;
    function GetSelectedData: TBITreePlugin.TNodeData; override;
    procedure SetOnChange(const Value: TNotifyEvent); override;
    procedure SetSelected(const Value: TBITreeNode); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure Expand(const AIndex:Integer); override;
    function Find(const ATag:TObject; const AIndex:Integer):TBITreeNode; override;
    function NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject=nil; const AIndex:TInteger=-1):TBITreeNode; override;
  end;

implementation
