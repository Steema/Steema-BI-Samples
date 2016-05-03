unit BI.VCL.DataWorkflow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BI.VCL.Editor.Data, BI.VCL.DataManager,
  Vcl.ExtCtrls, BI.VCL.Grid,
  VCL.ComCtrls, BI.Data, Vcl.StdCtrls, Vcl.Menus, BI.Data.Workflow,
  BI.VCL.Editor.WorkflowItem, BI.Persist, BI.VCL.NewColumn,
  BI.VCL.DataSelect, BI.VCL.Tree;

type
  TBIWorkflowEditor = class(TForm)
    BIGrid1: TBIGrid;
    PanelSelector: TPanel;
    Panel2: TPanel;
    BAdd: TButton;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Column1: TMenuItem;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Rename1: TMenuItem;
    Sort1: TMenuItem;
    Reorder1: TMenuItem;
    Filter1: TMenuItem;
    Button1: TButton;
    ranspose1: TMenuItem;
    Change1: TMenuItem;
    Panel3: TPanel;
    LError: TLabel;
    PopupMenu2: TPopupMenu;
    Files1: TMenuItem;
    Database1: TMenuItem;
    Web1: TMenuItem;
    N1: TMenuItem;
    Algorithm1: TMenuItem;
    Function1: TMenuItem;
    MachineLearning1: TMenuItem;
    Button2: TButton;
    BITree1: TBITree;
    BDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Tree1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Tree1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure BAddClick(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ranspose1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Query1Click(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure Database1Click(Sender: TObject);
    procedure Web1Click(Sender: TObject);
    procedure Function1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
  private
    { Private declarations }

    Data : TDataSelector;
    FWorkflow: TBIWorkflow;

    ItemEditor:TWorkflowItemEditor;

    procedure ActionChanged(Sender: TObject);
    procedure AddNewRoot(const AData:TDataItem; const X,Y:Integer);
    function DataOf(const ANode:TBITreeNode):TDataItem;
    function DoAddNode(const AParent:TBITreeNode; const AItem:TWorkflowActionItem):TBITreeNode;

    procedure FillTree;
    procedure FilterSelf(Sender: TComponent; var Valid:Boolean);

    procedure ItemChanged(Sender: TObject);
    function ItemOf(const ANode:TBITreeNode):TWorkflowActionItem;

    function NewShape(const AParent:TBITreeNode;
                      const AData:TDataItem):TBITreeNode; overload;

    function NewShape(const AParent:TBITreeNode; const AProvider:TDataProvider;
                      const AName:String):TBITreeNode; overload;

    procedure SetWorkflow(const Value: TBIWorkflow);
    procedure TryAddImport(const AKind:TDataDefinitionKind);
    procedure TryRefresh(const ANode:TBITreeNode);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const AWorkflow:TBIWorkflow):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AWorkflow:TBIWorkflow):TBIWorkflowEditor; static;

    property Workflow:TBIWorkflow read FWorkflow write SetWorkflow;
  end;

implementation
