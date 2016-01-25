{*********************************************}
{  TeeBI Software Library                     }
{  Data Import Editor                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.DataEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF FireMonkeyVersion<21}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,

  {$IF FireMonkeyVersion<22}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.ScrollBox,
  {$ENDIF}

  FMX.EditBox, FMX.NumberBox, FMX.ComboEdit,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.TabControl, BI.Persist, System.Math,
  FMX.ListBox, FMX.Memo, FMX.Layouts;

type
  TDataEditor = class(TForm)
    TabSources: TTabControl;
    TabFiles: TTabItem;
    TabDatabase: TTabItem;
    OpenFile: TOpenDialog;
    LFolderMissing: TLabel;
    MemoDBTest: TMemo;
    TabUnknown: TTabItem;
    Label7: TLabel;
    TabControlDB: TTabControl;
    Connection: TTabItem;
    SQL: TTabItem;
    Label2: TLabel;
    CBDBDriver: TComboBox;
    BDBTest: TButton;
    Label4: TLabel;
    EDBServer: TEdit;
    Label3: TLabel;
    EDBDatabase: TEdit;
    Label5: TLabel;
    EDBUser: TEdit;
    Label6: TLabel;
    EDBPassword: TEdit;
    CBLogin: TCheckBox;
    TabControlDBItems: TTabControl;
    TabItemAllDB: TTabItem;
    TabSQL: TTabItem;
    MemoSQL: TMemo;
    CBAllItems: TCheckBox;
    TabControlFile: TTabControl;
    TabFile: TTabItem;
    TabFolder: TTabItem;
    Label1: TLabel;
    EFile: TEdit;
    BSelectFile: TSpeedButton;
    Label8: TLabel;
    EFolder: TEdit;
    BSelectFolder: TSpeedButton;
    CBAllFolder: TCheckBox;
    EFileMask: TEdit;
    CBFileType: TComboBox;
    CBRecursive: TCheckBox;
    TabWeb: TTabItem;
    Button1: TButton;
    LayoutOkCancel: TLayout;
    BOK: TButton;
    Button3: TButton;
    LFileMissing: TLabel;
    Label11: TLabel;
    EExcluded: TEdit;
    EDBInclude: TEdit;
    LDBExclude: TLabel;
    EDBExclude: TEdit;
    LDBInclude: TLabel;
    Button2: TButton;
    TabWebHost: TTabControl;
    TabHttpServer: TTabItem;
    TabHttpProxy: TTabItem;
    LayoutWebData: TLayout;
    LWebData: TLabel;
    CBWebData: TComboEdit;
    BWebTest: TButton;
    CBZip: TCheckBox;
    EWebServer: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    LWebTest: TLabel;
    NPort: TNumberBox;
    Label12: TLabel;
    EProxyHost: TEdit;
    Label13: TLabel;
    ProxyPort: TNumberBox;
    Label14: TLabel;
    EProxyUser: TEdit;
    Label15: TLabel;
    EProxyPassword: TEdit;
    PasswordEditButton1: TPasswordEditButton;
    Label16: TLabel;
    EDBPort: TEdit;
    CBDBSystem: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure EFileChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BSelectFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBDBDriverChange(Sender: TObject);
    procedure EDBDatabaseChange(Sender: TObject);
    procedure EDBServerChange(Sender: TObject);
    procedure EDBUserChange(Sender: TObject);
    procedure EDBPasswordChange(Sender: TObject);
    procedure CBLoginChange(Sender: TObject);
    procedure BDBTestClick(Sender: TObject);
    procedure MemoSQLChange(Sender: TObject);
    procedure CBAllItemsChange(Sender: TObject);
    procedure EFolderChange(Sender: TObject);
    procedure CBAllFolderChange(Sender: TObject);
    procedure EFileMaskChange(Sender: TObject);
    procedure BSelectFolderClick(Sender: TObject);
    procedure CBFileTypeChange(Sender: TObject);
    procedure CBRecursiveChange(Sender: TObject);
    procedure EWebServerChangeTracking(Sender: TObject);
    procedure NPortChangeTracking(Sender: TObject);
    procedure CBWebDataClick(Sender: TObject);
    procedure CBWebDataChangeTracking(Sender: TObject);
    procedure BWebTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabSourcesChange(Sender: TObject);
    procedure CBZipChange(Sender: TObject);
    procedure EExcludedChange(Sender: TObject);
    procedure EDBIncludeChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure EProxyHostChange(Sender: TObject);
    procedure ProxyPortChange(Sender: TObject);
    procedure EProxyUserChange(Sender: TObject);
    procedure EProxyPasswordChange(Sender: TObject);
    procedure EDBPortChange(Sender: TObject);
    procedure CBDBSystemChange(Sender: TObject);
  private
    { Private declarations }

    FOnChangeWeb : TNotifyEvent;

    IChanging : Boolean;

    IWebPath,
    IStore : String;

    procedure DatabaseSettings;
    function DBDriverID:String;

    procedure FilesSettings;
    function FileTypeExtension(const Index:Integer):String;
    procedure FillDBDrivers;

    procedure RefreshSettings;
    procedure ResetWebTest;
    procedure TryChange(const ATag,AText:String);
    procedure TryWebChange(const ATag,AText:String);
    procedure TryFillWebDatas;
    procedure WebSettings;
  protected
    procedure ClearWeb;
    procedure SetWebPath(const APath:String);
    function WebPath:String;
  public
    { Public declarations }
    Data : TDataDefinition;

    class function NewWeb(const AOwner:TComponent; out AWeb:String):Boolean; static;
    procedure Select(const AStore,AName:String); overload;
    procedure Select(const AData:TDataDefinition); overload;

    property OnChangeWeb:TNotifyEvent read FOnChangeWeb write FOnChangeWeb;
  end;

implementation
