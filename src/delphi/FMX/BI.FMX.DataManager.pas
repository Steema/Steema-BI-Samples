{*********************************************}
{  TeeBI Software Library                     }
{  Data Manager dialog                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.DataManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, 

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation, FMX.EditBox, FMX.NumberBox,
  {$ENDIF}

  {$IF CompilerVersion<=28}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.ScrollBox,
  {$ENDIF}

  {$IF CompilerVersion<=29}
  {$DEFINE HASFMX23}
  {$ENDIF}

  {$IFNDEF HASFMX23}
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  {$ENDIF}

  FMX.Dialogs, FMX.ListView.Types, FMX.ListView, FMX.StdCtrls,
  FMX.Layouts, BI.Persist, FMX.ListBox, FMX.TabControl, BI.FMX.DataEditor,
  FMX.Edit, FMX.Menus, FMX.Memo, BI.Data;

type
  TDataManager = class(TForm)
    LayoutStore: TLayout;
    LayoutSources: TLayout;
    Layout3: TLayout;
    Label1: TLabel;
    LayoutAddDelete: TLayout;
    BAdd: TSpeedButton;
    LSources: TListView;
    LStore: TLabel;
    LayoutButtons: TLayout;
    Layout7: TLayout;
    BSelect: TButton;
    BCancel: TButton;
    TabControl1: TTabControl;
    TabSettings: TTabItem;
    Splitter1: TSplitter;
    TabSchedule: TTabItem;
    CBRefresh: TCheckBox;
    Label3: TLabel;
    RefreshUnit: TComboBox;
    Period: TNumberBox;
    CBKeep: TCheckBox;
    Label4: TLabel;
    NumberBox1: TNumberBox;
    Label5: TLabel;
    PopupKinds: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    TabData: TTabItem;
    MemoImportLog: TMemo;
    ListData: TListBox;
    Label7: TLabel;
    LNextRefresh: TLabel;
    MenuItem1: TMenuItem;
    CBStores: TComboBox;
    Layout1: TLayout;
    Label6: TLabel;
    LLastImport: TLabel;
    MemoDataInfo: TMemo;
    BImportNow: TButton;
    BViewData: TButton;
    CBParallel: TCheckBox;
    CBStopOnErrors: TCheckBox;
    ImportProgress: TProgressBar;
    Button1: TButton;
    BDelete: TSpeedButton;
    BChange: TButton;
    BRename: TButton;
    procedure BChangeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LSourcesChange(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BSelectClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure TabControl1Resize(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure BImportNowClick(Sender: TObject);
    procedure BViewDataClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure ListDataChange(Sender: TObject);
    procedure CBRefreshChange(Sender: TObject);
    procedure PeriodChange(Sender: TObject);
    procedure RefreshUnitChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBParallelChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBStoresChange(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
  private
    { Private declarations }

    FOnSelect : TNotifyEvent;

    ICurrent : TDataItem;
    IEditor : TDataEditor;

    IStore : String;
    ChoosingData : Boolean;

    IChanging : Boolean;

    procedure AddStores;
    function AskName:String;
    function Current:String;
    function CurrentData:TDataItem; // ???
    function CurrentSource:Integer;
    function CurrentStore:String;
    procedure FillData;
    procedure ImportingData(const Sender:TObject; const Percent:Single; var Cancel:Boolean);
    function ImportingError(const Sender:TObject; const Text:String):Boolean;
    procedure LogException(const Text:String);
    procedure ReFill;
    procedure SetLastImport;
    procedure SetScheduling;
    procedure TryAdd(const Kind:TDataDefinitionKind);

    Constructor CreateStore(const AOwner:TComponent; const AStore:String);

    class function CreateChooser(const AOwner: TComponent; const AStore:String=''): TDataManager; static;
  public
    { Public declarations }

    function Selected:TDataItem;

    class function ChooseName(const AOwner:TComponent; const AStore:String=''):String; static;
    class function ChooseData(const AOwner:TComponent; const AStore:String='';
                              const ACurrent:TDataItem=nil):TDataItem; static;
    class procedure Edit(const AOwner:TComponent; const AStore:String=''); static;
    class function EmbedChoose(const AOwner:TComponent; const AParent:TControl; const AStore:String=''):TDataManager; static;

    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation
