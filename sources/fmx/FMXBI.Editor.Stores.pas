unit FMXBI.Editor.Stores;

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
  FMXBI.Editor.Data;

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

{$R *.fmx}

uses
  BI.Web, FMXBI.Grid, System.IOUtils, BI.Languages.English;

procedure TStoreEditor.BRemoveClick(Sender: TObject);
begin
  if TUICommon.YesNo(Format(BIMsg_Store_SureRemove,[Store])) then
  begin
    TStores.Remove(Store);

    LBStores.Items.Delete(LBStores.ItemIndex);
    LBStoresChange(Self);
  end;
end;

procedure TStoreEditor.BRenameClick(Sender: TObject);
var Old,tmp : String;
begin
  tmp:=Store;
  Old:=tmp;

  if TUICommon.Input(BIMsg_Store_ChangeName,BIMsg_NewName,Old,tmp) then
  begin
    LBStores.Items[LBStores.ItemIndex]:=tmp;
    TStores.ChangeName(Old,tmp);
  end;
end;

procedure TStoreEditor.BSelectFolderClick(Sender: TObject);
var tmp : String;
begin
  tmp:=EFolder.Text;

  if TUICommon.SelectFolder(tmp) then
     EFolder.Text:=tmp;
end;

procedure TStoreEditor.Button1Click(Sender: TObject);
begin
  TUICommon.Popup(PopupMenu1,Button1);
end;

procedure TStoreEditor.CBDefaultChange(Sender: TObject);
begin
  if CBDefault.IsChecked then
  begin
    TStore.DefaultName:=Store;

    CBDefault.IsChecked:=True;
    CBDefault.Enabled:=False;
  end;
end;

procedure TStoreEditor.ChangeWeb(Sender:TObject);
var tmp : String;
begin
  tmp:=WebPath;
  TStores.ChangePath(Store,'web:'+tmp);

  IWebEditor.BWebTest.Enabled:=tmp<>'';
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
  if System.SysUtils.DirectoryExists(EFolder.Text) then
  begin
    LBadFolder.Visible:=False;
    TStores.ChangePath(Store,EFolder.Text);
  end
  else
     LBadFolder.Visible:=True;
end;

procedure TStoreEditor.FormCreate(Sender: TObject);
begin
  TStores.AllTo(LBStores.Items);

  LBStores.ItemIndex:=LBStores.Items.IndexOf(TStore.DefaultName);
  LBStoresChange(Self);
end;

type
  TDataEditorAccess=class(TDataEditor);

procedure TStoreEditor.LBStoresChange(Sender: TObject);
var tmpPath : String;
begin
  LabelEmpty.Visible:=LBStores.Count=0;

  BRename.Enabled:=LBStores.ItemIndex<>-1;
  BRemove.Enabled:=BRename.Enabled;

  if LBStores.ItemIndex=-1 then
  begin
    CBDefault.IsChecked:=False;
    CBDefault.Enabled:=False;

    TabControl1.Visible:=False;
  end
  else
  begin
    TabControl1.Visible:=True;

    tmpPath:=TStore.PathOf(Store);

    TabWeb.Visible:=SameText('web:',Copy(tmpPath,1,4));
    TabFolder.Visible:=not TabWeb.Visible;

    if TabFolder.Visible then
    begin
      TabControl1.ActiveTab:=TabFolder;

      EFolder.Text:=tmpPath;
      EFolderChange(Self);

      EFolder.SetFocus;
    end
    else
    begin
      TabControl1.ActiveTab:=TabWeb;

      if IWebEditor=nil then
      begin
        IWebEditor:=TDataEditor.Create(Self);
        IWebEditor.LayoutWebData.Visible:=False;

        while IWebEditor.ChildrenCount>0 do
              IWebEditor.Children[0].Parent:=TabWeb;

        IWebEditor.OnChangeWeb:=ChangeWeb;
      end
      else
        TDataEditorAccess(IWebEditor).ClearWeb;

      TDataEditorAccess(IWebEditor).SetWebPath(tmpPath);

      IWebEditor.EWebServer.SetFocus;
    end;

    CBDefault.IsChecked:=SameText(Store,TStore.DefaultName);
    CBDefault.Enabled:=not CBDefault.IsChecked;
  end;
end;

procedure TStoreEditor.MenuItem1Click(Sender: TObject);
begin
  AddPrefix('');
end;

procedure TStoreEditor.MenuItem2Click(Sender: TObject);
begin
  AddPrefix('web:');
end;

function TStoreEditor.Store: String;
begin
  if LBStores.ItemIndex=-1 then
     result:=''
  else
     result:=LBStores.Items[LBStores.ItemIndex];
end;

function TStoreEditor.WebPath: String;
begin
  result:=TDataEditorAccess(IWebEditor).WebPath;
end;

procedure TStoreEditor.AddPrefix(const APrefix:String);
var tmp : String;
begin
  tmp:=TStores.NewName;
  TStores.Add(tmp,APrefix);

  LBStores.Items.Add(tmp);
  LBStores.ItemIndex:=LBStores.Count-1;
  LBStoresChange(Self);

  if LBStores.Count=1 then
     CBDefault.IsChecked:=True;
end;

end.
