{*********************************************}
{  TeeBI Software Library                     }
{  Store Editor Dialog                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Stores;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, VCLBI.Editor.Data;

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

{$R *.dfm}

uses
  {$IFNDEF FPC}
  System.Types,
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  {$ENDIF}
  VCLBI.Grid, VCLBI.Menus, BI.Persist, BI.Web, BI.Languages.English, BI.UI;

type
  TDataEditorAccess=class(TDataEditor);

procedure TStoreEditor.BRenameClick(Sender: TObject);
var Old,tmp : String;
begin
  tmp:=CurrentStore;
  Old:=tmp;

  if TUICommon.Input(BIMsg_Store_ChangeName,BIMsg_NewName,Old,tmp) then
  begin
    TStores.ChangeName(Old,tmp);
    LBStores.Items[LBStores.ItemIndex]:=tmp;
  end;
end;

procedure TStoreEditor.Button1Click(Sender: TObject);
begin
  TBIMenu.Popup(PopupMenu1,Button1);
end;

procedure TStoreEditor.BRemoveClick(Sender: TObject);
begin
  if TUICommon.YesNo(Format(BIMsg_Store_SureRemove,[CurrentStore])) then
  begin
    TStores.Remove(CurrentStore);

    LBStores.Items.Delete(LBStores.ItemIndex);
    LBStoresClick(Self);
  end;
end;

procedure TStoreEditor.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TStoreEditor.WebServer1Click(Sender: TObject);
begin
  AddPrefix('web:');
end;

procedure TStoreEditor.CBDefaultClick(Sender: TObject);
begin
  if CBDefault.Checked then
  begin
    TStore.DefaultName:=CurrentStore;

    CBDefault.Checked:=True;
    CBDefault.Enabled:=False;

    LBStores.Repaint;
  end;
end;

class procedure TStoreEditor.Edit(const AOwner: TComponent);
begin
  with TStoreEditor.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TStoreEditor.EFolderChange(Sender: TObject);
begin
  if {$IFNDEF FPC}System.{$ENDIF}SysUtils.DirectoryExists(EFolder.Text) then
  begin
    LBadFolder.Visible:=False;
    TStores.ChangePath(CurrentStore,EFolder.Text);
  end
  else
     LBadFolder.Visible:=True;
end;

function TStoreEditor.WebPath:String;
begin
  result:=TDataEditorAccess(IWebEditor).WebPath;
end;

procedure TStoreEditor.ChangeWeb(Sender:TObject);
var tmp : String;
begin
  tmp:=WebPath;
  TStores.ChangePath(CurrentStore,'web:'+tmp);

  IWebEditor.BWebTest.Enabled:=tmp<>'';
end;

procedure TStoreEditor.AddPrefix(const APrefix:String);
var tmp : String;
begin
  tmp:=TStores.NewName;
  TStores.Add(tmp,APrefix);

  LBStores.Items.Add(tmp);
  LBStores.ItemIndex:=LBStores.Items.IndexOf(tmp);
  LBStoresClick(Self);

  if LBStores.Count=-1 then
  begin
    CBDefault.Checked:=True;
    CBDefaultClick(Self);
  end;
end;

procedure TStoreEditor.Folder1Click(Sender: TObject);
begin
  AddPrefix('');
end;

procedure TStoreEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'StoreManager');
end;

procedure TStoreEditor.FormCreate(Sender: TObject);
begin
  TStores.AllTo(LBStores.Items);
end;

procedure TStoreEditor.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'StoreManager');

  LBStores.ItemIndex:=LBStores.Items.IndexOf(TStore.DefaultName);

  if LBStores.ItemIndex=-1 then
     if LBStores.Count>0 then
        LBStores.ItemIndex:=0;

  LBStoresClick(Self);
end;

procedure TryFocus(const AControl:TWinControl);
begin
  if AControl.Showing then
     AControl.SetFocus;
end;

procedure TStoreEditor.LBStoresClick(Sender: TObject);

  procedure SetupWebTab(const APath:String);
  begin
    PageControl1.ActivePage:=TabWeb;

    if IWebEditor=nil then
    begin
      IWebEditor:=TDataEditor.Create(Self);
      IWebEditor.OnChangeWeb:=ChangeWeb;
      IWebEditor.LayoutWebData.Visible:=False;

      IWebEditor.PageControlWeb.Parent:=TabWeb;
    end
    else
      TDataEditorAccess(IWebEditor).ClearWeb;

    TDataEditorAccess(IWebEditor).SetWebPath(APath);

    TryFocus(IWebEditor.EWebServer);

    IWebEditor.PageControlWeb.ActivePage:=IWebEditor.TabHttpServer;
  end;

var tmpPath : String;
begin
  BRename.Enabled:=LBStores.ItemIndex<>-1;
  BRemove.Enabled:=BRename.Enabled;

  if LBStores.ItemIndex=-1 then
  begin
    CBDefault.Checked:=False;
    CBDefault.Enabled:=False;

    PageControl1.Enabled:=False;
    TabWeb.TabVisible:=False;
    TabFolder.TabVisible:=False;
  end
  else
  begin
    PageControl1.Enabled:=True;

    tmpPath:=TStore.PathOf(CurrentStore);

    TabWeb.TabVisible:=SameText('web:',Copy(tmpPath,1,4));
    TabFolder.TabVisible:=not TabWeb.TabVisible;

    if TabFolder.TabVisible then
    begin
      PageControl1.ActivePage:=TabFolder;

      EFolder.Text:=tmpPath;
      EFolderChange(Self);
    end
    else
      SetupWebTab(tmpPath);

    CBDefault.Checked:=SameText(CurrentStore,TStore.DefaultName);
    CBDefault.Enabled:=not CBDefault.Checked;
  end;
end;

type
  TListBoxAccess=class(TListBox);

procedure TStoreEditor.LBStoresDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if TStore.DefaultName=StoreAt(Index) then
     LBStores.Canvas.Font.Style:=[fsBold]
  else
     LBStores.Canvas.Font.Style:=[];

  LBStores.OnDrawItem:=nil;
  try
    TListBoxAccess(LBStores).DrawItem(Index,Rect,State);
  finally
    LBStores.OnDrawItem:=LBStoresDrawItem;
  end;
end;

procedure TStoreEditor.SpeedButton1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=EFolder.Text;

  if TUICommon.SelectFolder(tmp) then
     EFolder.Text:=tmp;
end;

function TStoreEditor.StoreAt(const AIndex:Integer): String;
begin
  if AIndex=-1 then
     result:=''
  else
     result:=LBStores.Items[AIndex];
end;

function TStoreEditor.CurrentStore: String;
begin
  result:=StoreAt(LBStores.ItemIndex);
end;

end.
