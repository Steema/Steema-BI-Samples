{*********************************************}
{  TeeBI Software Library                     }
{  Store Editor Dialog                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Stores;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, BI.VCL.Editor.Data;

type
  TStoreEditor = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    Folder1: TMenuItem;
    WebServer1: TMenuItem;
    BRemove: TButton;
    LBStores: TListBox;
    PanelButtons: TPanel;
    Panel4: TPanel;
    BClose: TButton;
    PageControl1: TPageControl;
    TabFolder: TTabSheet;
    TabWeb: TTabSheet;
    Label1: TLabel;
    EFolder: TEdit;
    SpeedButton1: TSpeedButton;
    LBadFolder: TLabel;
    BRename: TButton;
    CBDefault: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure BRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBStoresClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure EFolderChange(Sender: TObject);
    procedure WebServer1Click(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure CBDefaultClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBStoresDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    IWebEditor : TDataEditor;

    procedure AddPrefix(const APrefix:String);
    procedure ChangeWeb(Sender:TObject);
    function CurrentStore:String;
    function StoreAt(const AIndex:Integer): String;
    function WebPath:String;
  public
    { Public declarations }
    class procedure Edit(const AOwner:TComponent); static;
  end;

implementation
