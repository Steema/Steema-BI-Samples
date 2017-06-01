{*********************************************}
{  TeeBI Software Library                     }
{  DataManager VCL                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.DataManager;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.DataItem, BI.Persist, BI.DataSource, BI.UI, Vcl.Menus, VCLBI.Editor.Data,
  Vcl.Buttons, VCLBI.DataControl, VCLBI.Grid, VCLBI.Editor.Data.Links;

type
  TDataManagerFilter=class abstract
  public
    procedure Clear; virtual; abstract;
    function Valid(const AName:String):Boolean; overload; virtual; abstract;
    function Valid(const AData:TDataItem):Boolean; overload; virtual; abstract;
  end;

  TDataManagerEmbedMode=(Choose,Edit);

  TDataManager = class(TForm)
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    PanelSearch: TPanel;
    Tree: TTreeView;
    PanelStores: TPanel;
    Label1: TLabel;
    CBStores: TComboBox;
    Panel1: TPanel;
    LSearch: TLabel;
    ESearch: TEdit;
    BAdd: TButton;
    BDelete: TButton;
    PopupMenu1: TPopupMenu;
    Files1: TMenuItem;
    DatabaseServer1: TMenuItem;
    BIWeb1: TMenuItem;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    TabSettings: TTabSheet;
    TabData: TTabSheet;
    MemoImportLog: TMemo;
    DataMenu: TPopupMenu;
    ViewData1: TMenuItem;
    BRename: TButton;
    PanelManage: TPanel;
    BManageStores: TButton;
    TabLinks: TTabSheet;
    Custommanual1: TMenuItem;
    PanelDataTop: TPanel;
    Label2: TLabel;
    LLastImport: TLabel;
    BViewData: TButton;
    MemoDataInfo: TMemo;
    BImportNow: TButton;
    CBParallel: TCheckBox;
    CBStoponerrors: TCheckBox;
    ImportProgress: TProgressBar;
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure ESearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BViewDataClick(Sender: TObject);
    procedure CBStoresChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BManageStoresClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure BIWeb1Click(Sender: TObject);
    procedure DatabaseServer1Click(Sender: TObject);
    procedure CBParallelClick(Sender: TObject);
    procedure BImportNowClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PanelSearchResize(Sender: TObject);
    procedure ViewData1Click(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure PanelStoresResize(Sender: TObject);
    procedure Custommanual1Click(Sender: TObject);
  private
    { Private declarations }

    IEditor : TDataEditor;

    IDataLinks : TDataLinksEditor;

    IStore : String;
    IFilterTree : TDataManagerFilter;

    FOnSelect : TNotifyEvent;

    ICurrent : TDataItem;

    IFillingTree,
    IUpdatingTree : Boolean;

    procedure AddAllStores;
    procedure AddNodeChildren(const ANode:TTreeNode);
    procedure AddNodeData(const AStore:String; const Children:Boolean; const Filter:String='');
    function AskName:String;
    function CanAdd(const AName:String):Boolean; overload;
    function CanAdd(const AData:TDataItem):Boolean; overload;
    procedure CheckEditor;
    function Current:String;
    function DoImportData:TDataArray;
    procedure DoSaveData(const AData:TDataArray);
    procedure FillTree(const AStore:String);
    procedure ImportingData(const Sender:TObject; const Percent:Single; var Cancel:Boolean);
    function ImportingError(const Sender:TObject; const Text:String):Boolean;
    procedure LogException(const Text:String);
    function NodeWithData(const AData:TDataItem):TTreeNode;
    procedure ReplaceDummy(const Node:TTreeNode);
    function SelectedText:String;
    procedure SelectStore;
    procedure SetLastImport;
    procedure ShowDataInfo(SelectAtEditor:Boolean);
    procedure ShowDataLinks;
    procedure TryAdd(const Kind:TDataDefinitionKind);

    constructor CreateStore(const AOwner: TComponent; const AStore: String='');
    procedure SetFilterTree(const Value: TDataManagerFilter);
  public
    { Public declarations }

    AddItems : Boolean;

    procedure AddNodeItems(const Node:TTreeNode);

    // Select a data item from any Store
    class function Choose(const AOwner:TComponent; const ACurrent:TDataItem=nil;
                          const FillItems:Boolean=False):TDataItem; static;

    function CurrentStore:String;

    class procedure Edit(const AOwner:TComponent; const AStore:String=''); static;

    class function Embed(const AOwner:TComponent;
                         const AParent:TWinControl;
                         const AMode:TDataManagerEmbedMode=TDataManagerEmbedMode.Choose;
                         const AStore:String='';
                         const ACurrent:TDataItem=nil):TDataManager; static;

    function SelectedData:TDataItem;
    function SelectedDefinition:TDataDefinition;

    function Selected: TDataItem;
    procedure SelectData(const AData:TDataItem);

    property OnFilter:TDataManagerFilter read IFilterTree write SetFilterTree;
    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TMasterFilter=class(TDataManagerFilter)
  private
    IData : TDataArray;
    INames : TStrings;

    class function DataNameOf(const AData:TDataItem):String; static;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Add(const AData:TDataItem);

    procedure Clear; override;
    function Valid(const AName:String):Boolean; override;
    function Valid(const AData:TDataItem):Boolean; override;
  end;

implementation
