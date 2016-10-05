unit BI.FMX.Editor.Stores;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,

  {$IF CompilerVersion<=28}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.EditBox, FMX.NumberBox,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Edit, FMX.TabControl, FMX.ListBox, BI.Persist, FMX.Menus,
  BI.FMX.Editor.Data;

type
  TStoreEditor = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    BRemove: TButton;
    LayoutButtons: TLayout;
    BRename: TButton;
    Layout2: TLayout;
    Button2: TButton;
    LBStores: TListBox;
    TabControl1: TTabControl;
    TabFolder: TTabItem;
    Label8: TLabel;
    EFolder: TEdit;
    BSelectFolder: TSpeedButton;
    LBadFolder: TLabel;
    TabWeb: TTabItem;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    CBDefault: TCheckBox;
    LabelEmpty: TLabel;
    procedure BSelectFolderClick(Sender: TObject);
    procedure EFolderChange(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BRemoveClick(Sender: TObject);
    procedure LBStoresChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBDefaultChange(Sender: TObject);
  private
    { Private declarations }

    IWebEditor : TDataEditor;

    procedure AddPrefix(const APrefix:String);
    procedure ChangeWeb(Sender:TObject);
    function Store:String;
    function WebPath:String;
  public
    { Public declarations }
    class procedure Edit(const AOwner:TComponent); static;
  end;

implementation
