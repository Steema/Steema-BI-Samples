{*********************************************}
{  TeeBI Software Library                     }
{  Workflow Editor Dialog                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Workflow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BI.VCL.Editor.Data, BI.VCL.DataManager,
  Vcl.ExtCtrls, BI.VCL.Grid,
  VCL.ComCtrls, BI.Data, Vcl.StdCtrls, Vcl.Menus, BI.Data.Workflow,
  BI.VCL.Editor.WorkflowItem, BI.Persist, BI.VCL.NewColumn,
  BI.VCL.DataSelect, BI.VCL.Tree, BI.VCL.DataControl, BI.VCL.GridForm,
  Vcl.Buttons;

type
  TBIWorkflowEditor = class(TForm)
    PanelSelector: TPanel;
    PopupAdd: TPopupMenu;
    Splitter1: TSplitter;
    Column1: TMenuItem;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Rename1: TMenuItem;
    Sort1: TMenuItem;
    Reorder1: TMenuItem;
    Filter1: TMenuItem;
    ranspose1: TMenuItem;
    Change1: TMenuItem;
    N1: TMenuItem;
    Function1: TMenuItem;
    MachineLearning1: TMenuItem;
    PanelMain: TPanel;
    Panel2: TPanel;
    BAdd: TButton;
    BDelete: TButton;
    SplitterPreview: TSplitter;
    Integer32bit1: TMenuItem;
    Integer64bit1: TMenuItem;
    Singlefloat1: TMenuItem;
    Doublefloat1: TMenuItem;
    Extendedfloat1: TMenuItem;
    ext1: TMenuItem;
    DateTime1: TMenuItem;
    Boolean1: TMenuItem;
    Split1: TMenuItem;
    Shuffle1: TMenuItem;
    Duplicate1: TMenuItem;
    Singlerow1: TMenuItem;
    Regression1: TMenuItem;
    Normalize1: TMenuItem;
    Rank1: TMenuItem;
    Gridify1: TMenuItem;
    Clone1: TMenuItem;
    PopupNew: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    BNew: TButton;
    SBSelector: TSpeedButton;
    PanelTree: TPanel;
    BITree1: TBITree;
    SplitterEditor: TSplitter;
    PanelEditor: TPanel;
    PanelPreviewButton: TPanel;
    BPreview: TButton;
    PopupPreview: TPopupMenu;
    Grid1: TMenuItem;
    Chart1: TMenuItem;
    PanelPreview: TPanel;
    PanelChart: TPanel;
    SplitterChart: TSplitter;
    Groupby1: TMenuItem;
    Join1: TMenuItem;
    Common1: TMenuItem;
    Different1: TMenuItem;
    CrossTableConfusionMatrix1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Tree1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Tree1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Add1Click(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure ranspose1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Query1Click(Sender: TObject);
    procedure Function1Click(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure PopupAddPopup(Sender: TObject);
    procedure Boolean1Click(Sender: TObject);
    procedure Reorder1Click(Sender: TObject);
    procedure Split1Click(Sender: TObject);
    procedure Sort1Click(Sender: TObject);
    procedure Shuffle1Click(Sender: TObject);
    procedure Filter1Click(Sender: TObject);
    procedure NewCustomData1Click(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure DatabaseServer1Click(Sender: TObject);
    procedure BIWebServer1Click(Sender: TObject);
    procedure Duplicate1Click(Sender: TObject);
    procedure Singlerow1Click(Sender: TObject);
    procedure Normalize1Click(Sender: TObject);
    procedure Rank1Click(Sender: TObject);
    procedure Gridify1Click(Sender: TObject);
    procedure Clone1Click(Sender: TObject);
    procedure BITree1Deleting(Sender: TObject);
    procedure Compare1Click(Sender: TObject);
    procedure SBSelectorClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure BPreviewClick(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure Chart1Click(Sender: TObject);
    procedure Groupby1Click(Sender: TObject);
    procedure Regression1Click(Sender: TObject);
    procedure Join1Click(Sender: TObject);
    procedure Common1Click(Sender: TObject);
    procedure Different1Click(Sender: TObject);
    procedure CrossTableConfusionMatrix1Click(Sender: TObject);
  private
    { Private declarations }

    Data : TDataSelector;
    FWorkflow: TBIWorkflow;

    IGrid : TBIGridForm;

    ItemEditor : TWorkflowItemEditor;

    procedure AddDummy(const AParent:TBITreeNode);
    procedure AddItems(const AParent:TBITreeNode; const AData:TDataItem);
    procedure AddMachineLearning(Sender: TObject);
    procedure AddNewRoot(const AData:TDataItem; const X,Y:Integer);

    procedure CheckPreviewSettings;
    function ChooseItems(out ASelected:TDataItem):Boolean; overload;
    function ChooseItems(out ASelected:TDataArray; const Compatible:Boolean):Boolean; overload;

    function DataOf(const ANode:TBITreeNode):TDataItem;
    procedure DeleteSelected;

    function DirectNewShape(const AParent:TBITreeNode;
                            const AProvider:TDataProvider;
                            const AName:String):TBITreeNode;

    function DoAddNode(const AParent:TBITreeNode; const AItem:TWorkflowItem):TBITreeNode;
    procedure DoChangeWorkflow(const Value: TBIWorkflow);
    function DoDummyExpansion(const Node:TBITreeNode):Boolean;

    procedure Expanding(Sender: TObject; const Node: TBITreeNode;
                        var AllowExpansion: Boolean);

    procedure FillTree;
    procedure FilterSelf(Sender: TComponent; var Valid:Boolean);

    function HasDummy(const ANode:TBITreeNode):Boolean;

    procedure ItemChanged(Sender: TObject);
    function ItemOf(const ANode:TBITreeNode):TWorkflowItem;

    function NameOf(const AItem:TWorkflowItem):String;

    function NewShape(const AParent:TBITreeNode;
                      const AData:TDataItem):TBITreeNode; overload;

    function NewShape(const AProvider:TDataProvider;
                      const AName:String):TBITreeNode; overload;

    procedure SelectedData(Sender:TObject);

    procedure SetWorkflow(const Value: TBIWorkflow);
    procedure TryAddModels;
    procedure TryImport(const AKind:TDataDefinitionKind);
    procedure TryMerge(const AStyle:TMergeStyle; const ACaption:String);
    procedure TryPreview;
    //procedure TryRefresh(const ANode:TBITreeNode);
    //procedure TryRefreshNodes(const AItem:TWorkflowItem);
  protected
    const
      TeeMsg_ItemsWorkflow='...';

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const AWorkflow:TBIWorkflow):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AWorkflow:TBIWorkflow):TBIWorkflowEditor; static;

    procedure Refresh(const Value: TBIWorkflow);

    property Workflow:TBIWorkflow read FWorkflow write SetWorkflow;
  end;

implementation
