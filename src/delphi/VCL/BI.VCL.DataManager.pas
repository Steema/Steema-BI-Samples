{*********************************************}
{  TeeBI Software Library                     }
{  DataManager VCL                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.DataManager;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Data, BI.Persist, BI.DataSource, BI.UI, Vcl.Menus, BI.VCL.Editor.Data;

type
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
    Button1: TButton;
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
    TabSheet3: TTabSheet;
    BViewData: TButton;
    Label2: TLabel;
    LLastImport: TLabel;
    MemoDataInfo: TMemo;
    BImportNow: TButton;
    CBParallel: TCheckBox;
    CBStoponerrors: TCheckBox;
    MemoImportLog: TMemo;
    ImportProgress: TProgressBar;
    DataMenu: TPopupMenu;
    ViewData1: TMenuItem;
    BRename: TButton;
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
    procedure Button1Click(Sender: TObject);
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
  private
    { Private declarations }

    IEditor : TDataEditor;
    IStore : String;
    FOnSelect : TNotifyEvent;

    ICurrent : TDataItem;

    IUpdatingTree : Boolean;

    procedure AddNodeChildren(const ANode:TTreeNode);
    procedure AddNodeDatas(const AStore:String; const Children:Boolean; const Filter:String='');
    procedure AddStores;
    function AskName:String;
    procedure CheckEditor;
    function Current:String;
    procedure ImportingData(const Sender:TObject; const Percent:Single; var Cancel:Boolean);
    function ImportingError(const Sender:TObject; const Text:String):Boolean;
    procedure LogException(const Text:String);
    function NodeWithData(const AData:TDataItem):TTreeNode;
    function SelectedText:String;
    procedure SetLastImport;
    procedure SetScheduling;
    procedure ShowDataInfo(SelectAtEditor:Boolean);
    procedure TryAdd(const Kind:TDataDefinitionKind);

    constructor CreateStore(const AOwner: TComponent; const AStore: String='');
  public
    { Public declarations }

    AddItems : Boolean;

    procedure AddNodeItems(const Node:TTreeNode);

    class function Choose(const AOwner:TComponent; const ACurrent:TDataItem=nil;
                          const FillItems:Boolean=False):TDataItem; static;

    function CurrentStore:String;

    class procedure Edit(const AOwner:TComponent; const AStore:String=''); static;
    class function EmbedChoose(const AOwner:TComponent;
                               const AParent:TWinControl;
                               const AStore:String='';
                               const ACurrent:TDataItem=nil):TDataManager; static;

    function SelectedDatas:TDataItem;

    function Selected: TDataItem;
    procedure SelectData(const AData:TDataItem);

    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation
