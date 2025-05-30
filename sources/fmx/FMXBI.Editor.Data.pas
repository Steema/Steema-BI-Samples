{*********************************************}
{  TeeBI Software Library                     }
{  Data Import Editor                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.Data;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,

  {$IF CompilerVersion<=28}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.ScrollBox,
  {$ENDIF}

  FMX.EditBox, FMX.NumberBox, FMX.ComboEdit,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.TabControl, BI.Persist, System.Math,
  FMX.ListBox, FMX.Memo, FMX.Layouts, FMXBI.Editor.Items;

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
    TabManual: TTabItem;
    Layout1: TLayout;
    BOK: TButton;
    BCancel: TButton;
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

    OwnsData,
    IChanging : Boolean;

    IWebPath,
    IStore : String;

    IManual : TItemsEditor;

    procedure DatabaseSettings;
    function DBDriverID:String;

    procedure FilesSettings;
    function FileTypeExtension(const Index:Integer):String;
    procedure FillDBDrivers;

    procedure ManualChanged(Sender: TObject);
    procedure ManualSettings;
    procedure RefreshSettings;
    procedure ResetWebTest;
    procedure TryChange(const ATag,AText:String);
    procedure TryChangeMultiLine(const ATag,AText:String);
    procedure TryWebChange(const ATag,AText:String);
    procedure TryFillWebData;
    procedure WebSettings;
  protected
    procedure ClearWeb;
    procedure SetWebPath(const APath:String);
    function WebPath:String;
  public
    { Public declarations }
    Data : TDataDefinition;

    class function Edit(const AOwner:TComponent; const AData:TDataDefinition):Boolean; static;

    class function NewWeb(const AOwner:TComponent; out AWeb:String):Boolean; static;
    procedure Select(const AStore,AName:String); overload;
    procedure Select(const AData:TDataDefinition); overload;

    property OnChangeWeb:TNotifyEvent read FOnChangeWeb write FOnChangeWeb;
  end;

implementation

{$R *.fmx}

uses
  System.IOUtils, BI.Arrays, BI.DataSource,
  BI.DataItem, BI.DB, BI.Web, BI.UI, BI.Languages.English, FMXBI.Grid,
  FMXBI.GridForm;

{ TDataEditor }

procedure TDataEditor.TabSourcesChange(Sender: TObject);
begin
  FormResize(Self);
end;

procedure TDataEditor.BSelectFileClick(Sender: TObject);
begin
  OpenFile.FileName:=EFile.Text;

  if OpenFile.Execute then
     EFile.Text:=OpenFile.FileName;
end;

procedure TDataEditor.BSelectFolderClick(Sender: TObject);
var tmp : String;
begin
  tmp:=EFolder.Text;

  if TUICommon.SelectFolder(tmp) then
     EFolder.Text:=tmp;
end;

procedure TDataEditor.Button1Click(Sender: TObject);
var tmp : TStringDynArray;
    tmpData : TDataItem;
    tmpFTP : TBIFtp;
    tmpLocal : Boolean;
begin
  tmpLocal:=Data['FTPServer']='';

  if tmpLocal then
     tmp:=TBIFileSource.IncludedFiles(IStore,Data)
  else
  begin
    tmpFTP:=TBIWebClient.FTP(Data);
    try
      tmp:=tmpFTP.IncludedFiles(Data['Folder'],Data['IncludeMask']);
    finally
      tmpFTP.Free;
    end;
  end;

  tmpData:=TBIFileSource.DataFromFiles(tmp,tmpLocal);
  try
    TBIGridForm.Present(Self,tmpData);
  finally
    tmpData.Free;
  end;
end;

procedure TDataEditor.Button2Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TBIDB.IncludedItems(Data);
  try
    TBIGridForm.Present(Self,tmp);
  finally
    tmp.Free;
  end;
end;

procedure TDataEditor.BWebTestClick(Sender: TObject);
begin
  try
    TryFillWebData;
    LWebTest.Text:='OK';
  except
    on E:Exception do
    begin
      LWebTest.Text:='ERROR: '+E.Message;
    end;
  end;
end;

procedure TDataEditor.EFileChange(Sender: TObject);
var tmp : String;
begin
  TryChange('FileName',EFile.Text);

  tmp:=EFile.Text;

  if TCommonUI.IsURL(tmp) or TStore.IsRemote(IStore) then
     LFileMissing.Visible:=False
  else
  begin
    if TPath.IsRelativePath(tmp) then
       tmp:=TStore.FullPath(IStore,tmp);

    LFileMissing.Visible:=not FileExists(tmp);
  end;
end;

procedure TDataEditor.EFileMaskChange(Sender: TObject);
var t : Integer;
begin
  TryChange('IncludeMask',EFileMask.Text);

  for t:=1 to CBFileType.Count-1 do
      if SameText(EFileMask.Text,'*.'+FileTypeExtension(t)) then
      begin
        CBFileType.ItemIndex:=t;
        Exit;
      end;

  CBFileType.ItemIndex:=0;
end;

procedure TDataEditor.EDBDatabaseChange(Sender: TObject);
begin
  TryChange('DBDatabase',EDBDatabase.Text);
end;

procedure TDataEditor.EDBIncludeChange(Sender: TObject);
begin
  TryChange('IncludeMask',EDBInclude.Text);
end;

procedure TDataEditor.EDBPasswordChange(Sender: TObject);
begin
  TryChange('DBPassword',TCrypto.Encrypt(EDBPassword.Text));
end;

procedure TDataEditor.EDBPortChange(Sender: TObject);
begin
  TryChange('DBPort',EDBPort.Text);
end;

procedure TDataEditor.EDBServerChange(Sender: TObject);
begin
  TryChange('DBServer',EDBServer.Text);
end;

procedure TDataEditor.EDBUserChange(Sender: TObject);
begin
  TryChange('DBUser',EDBUser.Text);
end;

class function TDataEditor.Edit(const AOwner: TComponent;
  const AData: TDataDefinition): Boolean;
begin
  with TDataEditor.Create(AOwner) do
  try
    OwnsData:=False;
    Select(AData);

    LayoutOkCancel.Visible:=True;

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TDataEditor.EExcludedChange(Sender: TObject);
begin
  TryChange('ExcludeMask',(Sender as TEdit).Text);
end;

procedure TDataEditor.EFolderChange(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EFolder.Text);

  TryChange('Folder',tmp);

  if TStore.IsRemote(IStore) then
     LFolderMissing.Visible:=False
  else
  begin
    if tmp='' then
       LFolderMissing.Visible:=True
    else
    begin
      if TPath.IsRelativePath(tmp) then
         tmp:=TStore.FullPath(IStore,tmp);

      LFolderMissing.Visible:=not DirectoryExists(tmp);
    end;
  end;
end;

procedure TDataEditor.ResetWebTest;
begin
  CBWebData.Clear;
  CBWebData.Enabled:=(Trim(EWebServer.Text)<>'') and (Round(NPort.Value)>0);

  BWebTest.Enabled:=CBWebData.Enabled;
  LWebTest.Text:='';

  BOk.Enabled:=BWebTest.Enabled;
end;

procedure TDataEditor.TryChange(const ATag,AText:String);
begin
  if not IChanging then
     Data[ATag]:=AText;
end;

procedure TDataEditor.TryChangeMultiLine(const ATag, AText: String);
begin
  TryChange(ATag,StringReplace(AText,sLineBreak,'\CRLF', [rfReplaceAll]));
end;

procedure TDataEditor.TryWebChange(const ATag,AText:String);
begin
  if not IChanging then
  begin
    if Assigned(FOnChangeWeb) then
       FOnChangeWeb(Self)
    else
       Data[ATag]:=AText;

    ResetWebTest;
  end;
end;

procedure TDataEditor.EProxyHostChange(Sender: TObject);
begin
  TryWebChange('WebProxyServer',EProxyHost.Text);
end;

procedure TDataEditor.EProxyPasswordChange(Sender: TObject);
begin
  TryWebChange('WebProxyPass',TCrypto.Encrypt(EProxyPassword.Text));
end;

procedure TDataEditor.EProxyUserChange(Sender: TObject);
begin
  TryWebChange('WebProxyUser',EProxyUser.Text);
end;

procedure TDataEditor.EWebServerChangeTracking(Sender: TObject);
begin
  TryWebChange('WebServer',EWebServer.Text);
end;

procedure TDataEditor.BDBTestClick(Sender: TObject);
var tmp : TBIDBTesterClass;
begin
  MemoDBTest.Lines.Clear;
  MemoDBTest.Visible:=True;
  try
    tmp:=TBIDB.Engine.Tester;

    tmp.Test(Data['DBDriver'],
      Data['DBDatabase'],
      Data['DBServer'],
      Data['DBPort'],
      Data['DBUser'],
      Data['DBPassword'],
      Data.AsBoolean('DBLogin'));

    MemoDBTest.Lines.Add(BIMsg_ConnectionTestPassed);
  except
    on E:Exception do
    begin
      MemoDBTest.Lines.Add(E.Message);

      if E.InnerException<>nil then
         MemoDBTest.Lines.Add(E.InnerException.Message);
    end;
  end;
end;

procedure TDataEditor.CBAllFolderChange(Sender: TObject);
begin
  if CBAllFolder.IsChecked then
  begin
    CBFileType.ItemIndex:=0;
    EFileMask.Text:='*.*';
  end;

  TryChange('AllFiles',TCommonUI.ToBooleanString(CBAllFolder.IsChecked));
end;

procedure TDataEditor.CBAllItemsChange(Sender: TObject);
var tmp : Boolean;
begin
  tmp:=CBAllItems.IsChecked;

  TryChange('DBALL',TCommonUI.ToBooleanString(tmp));

  LDBInclude.Enabled:=not tmp;
  LDBExclude.Enabled:=not tmp;

  EDBInclude.Enabled:=not tmp;
  EDBExclude.Enabled:=not tmp;
end;

procedure TDataEditor.CBDBDriverChange(Sender: TObject);
begin
  TryChange('DBDriver',DBDriverID);

  BDBTest.Enabled:=DBDriverID<>'';
end;

procedure TDataEditor.CBDBSystemChange(Sender: TObject);
begin
  TryChange('DBSystem',TCommonUI.ToBooleanString(CBDBSystem.IsChecked));
end;

function TDataEditor.FileTypeExtension(const Index:Integer):String;
begin
  case Index of
    0: result:='';
    1: result:='csv';
    2: result:='txt';
    3: result:='xls';
    4: result:='cds';
    5: result:='json';
    6: result:='xml';
  else
    result:='*.*';
  end;
end;

procedure TDataEditor.CBFileTypeChange(Sender: TObject);
begin
  if CBFileType.ItemIndex>0 then
     EFileMask.Text:='*.'+FileTypeExtension(CBFileType.ItemIndex);
end;

procedure TDataEditor.CBLoginChange(Sender: TObject);
begin
  TryChange('DBLogin',TCommonUI.ToBooleanString(CBLogin.IsChecked));
end;

procedure TDataEditor.CBRecursiveChange(Sender: TObject);
begin
  TryChange('Recursive',TCommonUI.ToBooleanString(CBRecursive.IsChecked));
end;

procedure TDataEditor.TryFillWebData;
var B : TBIWebClient;
    Old : String;
    tmp : Integer;
begin
  Old:=CBWebData.Text;

  if Data=nil then
     B:=TBIWebClient.FromPath(IWebPath)
  else
     B:=TBIWebClient.Create(Data);

  try
    //B.Compress:=TWebCompression.Yes;
    CBWebData.Items.Text:=B.GetData;

    tmp:=CBWebData.Items.IndexOf(Old);
    CBWebData.ItemIndex:=tmp;

    if tmp=-1 then
       CBWebData.Text:=Old;
  finally
    B.Free;
  end;
end;

procedure TDataEditor.CBWebDataChangeTracking(Sender: TObject);
begin
  TryChange('WebData',CBWebData.Text);
end;

procedure TDataEditor.CBWebDataClick(Sender: TObject);
begin
  if CBWebData.Items.Count=0 then
     TryFillWebData;
end;

procedure TDataEditor.CBZipChange(Sender: TObject);
begin
  TryWebChange('Compress',TCommonUI.ToBooleanString(CBZip.IsChecked));
end;

function TDataEditor.DBDriverID:String;
begin
  if CBDBDriver.ItemIndex=-1 then
     result:=''
  else
     result:=TBIDB.Engine.GetDriver(CBDBDriver.ItemIndex);
end;

procedure TDataEditor.FillDBDrivers;
var S : String;
begin
  if CBDBDriver.Count=0 then
  begin
    CBDBDriver.BeginUpdate;
    try
      for S in TBIDB.Engine.DriverNames do
          CBDBDriver.Items.Add(S);
    finally
      CBDBDriver.EndUpdate;
    end;
  end;
end;

procedure TDataEditor.FormCreate(Sender: TObject);
begin
  OwnsData:=True;

  TabSources.TabPosition:=TTabPosition.{$IF CompilerVersion>26}None{$ELSE}tpNone{$ENDIF};
  TabControlFile.ActiveTab:=TabFile;
  TabControlDB.ActiveTab:=Connection;
  TabWebHost.ActiveTab:=TabHttpServer;
end;

procedure TDataEditor.FormDestroy(Sender: TObject);
begin
  if OwnsData then
     Data.Free;
end;

procedure TDataEditor.FormResize(Sender: TObject);
begin
  if TabSources.ActiveTab=TabFiles then
  begin
    BSelectFile.Position.X:=Min(456,TabSources.Width-96);
    EFile.Width:=Max(16,BSelectFile.Position.X-16);

    BSelectFolder.Position.X:=BSelectFile.Position.X;
    EFolder.Width:=EFile.Width;
  end;
end;

procedure TDataEditor.FormShow(Sender: TObject);
begin
  FormResize(Self);
end;

procedure TDataEditor.MemoSQLChange(Sender: TObject);
begin
  TryChangeMultiLine('SQL',MemoSQL.Text);
end;

procedure TDataEditor.NPortChangeTracking(Sender: TObject);
begin
  TryWebChange('WebPort',IntToStr(Round(NPort.Value)));
end;

procedure TDataEditor.ProxyPortChange(Sender: TObject);
begin
  TryWebChange('WebProxyPort',IntToStr(Round(ProxyPort.Value)));
end;

procedure TDataEditor.ManualChanged(Sender: TObject);
begin
  if LayoutOkCancel.Visible then
     BOK.Enabled:=True;
end;

procedure TDataEditor.ManualSettings;
begin
  TabSources.ActiveTab:=TabManual;

  // Force AsTable (note: remove this when ItemsEditor is capable of multi-table)
  Data.Data.AsTable:=True;

  if IManual=nil then
  begin
    IManual:=TItemsEditor.Embedd(Self,TabManual,Data.Data.Items);
    IManual.OnChanged:=ManualChanged;
  end
  else
     IManual.Refresh(Data.Data.Items);
end;

procedure TDataEditor.FilesSettings;
begin
  TabSources.ActiveTab:=TabFiles;

  EFile.Text:=Data['FileName'];
  EFileChange(Self);

  EFolder.Text:=Data['Folder'];
  EFolderChange(Self);

  CBAllFolder.IsChecked:=Data.AsBoolean('AllFiles');
  EFileMask.Text:=Data['IncludeMask'];
  EExcluded.Text:=Data['ExcludeMask'];

  CBRecursive.IsChecked:=Data.AsBoolean('Recursive');

  if EFolder.Text='' then
     TabControlFile.ActiveTab:=TabFile
  else
     TabControlFile.ActiveTab:=TabFolder;
end;

procedure TDataEditor.DatabaseSettings;
var tmp : String;
begin
  MemoDBTest.Lines.Clear;
  MemoDBTest.Visible:=False;

  TabSources.ActiveTab:=TabDatabase;

  FillDBDrivers;

  tmp:=TBIDB.Engine.DriverToName(Data['DBDriver']);

  if tmp='' then
     CBDBDriver.ItemIndex:=-1
  else
     CBDBDriver.ItemIndex:=CBDBDriver.Items.IndexOf(tmp);

  EDBDatabase.Text:=Data['DBDatabase'];
  EDBServer.Text:=Data['DBServer'];
  EDBPort.Text:=Data['DBPort'];
  EDBUser.Text:=Data['DBUser'];
  EDBPassword.Text:=TCrypto.Decrypt(Data['DBPassword']);
  CBLogin.IsChecked:=Data.AsBoolean('DBLogin');

  CBAllItems.IsChecked:=Data.AsBoolean('DBALL');
  CBDBSystem.IsChecked:=Data.AsBoolean('DBSystem');

  EDBInclude.Text:=Data['IncludeMask'];
  EDBExclude.Text:=Data['ExcludeMask'];

  MemoSQL.Text:=Data.MultiLineText('SQL');

  BDBTest.Enabled:=DBDriverID<>'';

  if CBAllItems.IsChecked or (MemoSQL.Text='') then
     TabControlDBItems.ActiveTab:=TabItemAllDB
  else
     TabControlDBItems.ActiveTab:=TabSQL;
end;

procedure TDataEditor.WebSettings;
var tmpPort : Integer;
begin
  TabSources.ActiveTab:=TabWeb;

  EWebServer.Text:=Data['WebServer'];

  if TryStrToInt(Data['WebPort'],tmpPort) then
     NPort.Value:=tmpPort;

  CBWebData.Text:=Data['WebData'];

  CBZip.IsChecked:=Data.AsBoolean('Compress');

  EProxyHost.Text:=Data['WebProxyServer'];

  if TryStrToInt(Data['WebProxyPort'],tmpPort) then
     ProxyPort.Value:=tmpPort;

  EProxyUser.Text:=Data['WebProxyUser'];
  EProxyPassword.Text:=TCrypto.Decrypt(Data['WebProxyPass']);

  ResetWebTest;
end;

procedure TDataEditor.RefreshSettings;
begin
  case Data.Kind of
     TDataDefinitionKind.Files: FilesSettings;
  TDataDefinitionKind.Database: DatabaseSettings;
       TDataDefinitionKind.Web: WebSettings;
    TDataDefinitionKind.Manual: ManualSettings;
  else
    TabSources.ActiveTab:=TabUnknown;
  end;
end;

procedure TDataEditor.Select(const AData: TDataDefinition);
begin
  Data:=AData;
  Data.Store:=IStore;

  IChanging:=True;
  try
    RefreshSettings;
  finally
    IChanging:=False;
  end;
end;

class function TDataEditor.NewWeb(const AOwner:TComponent; out AWeb:String):Boolean;
var tmp : TDataEditor;
    tmpData : TDataDefinition;
begin
  tmpData:=TDataDefinition.Create(nil);
  tmpData['Kind']:='WEB';

  tmp:=TDataEditor.Create(AOwner);
  try
    tmp.Select(tmpData);
    tmp.LayoutOkCancel.Visible:=True;

    tmp.Caption:=BIMsg_Store_SelectRemote;
    tmp.Position:=TFormPosition.{$IF CompilerVersion>26}OwnerFormCenter{$ELSE}poOwnerFormCenter{$ENDIF};

    tmp.CBWebData.Visible:=False;
    tmp.LWebData.Visible:=False;

    tmp.EWebServer.SetFocus;

    result:=tmp.ShowModal=mrOk;

    if result then
       AWeb:='web:'+tmp.EWebServer.Text+':'+IntToStr(Round(tmp.NPort.Value));
  finally
    tmp.Free;
  end;
end;

procedure TDataEditor.Select(const AStore,AName: String);
begin
  IStore:=AStore;

  Data.Free;
  Data:=TStore.GetDefinition(IStore,AName);

  Select(Data);
end;

function TDataEditor.WebPath:String;

  function PortToString(const APort:TNumberBox):String;
  begin
    if APort.Value<>TBIWebClient.DefaultPort then
       result:=result+':'+IntToStr(Round(APort.Value))
    else
       result:=result+':';
  end;

begin
  result:=EWebServer.Text+PortToString(NPort);

  if CBZip.IsChecked then
     result:=result+':zip';

  if EProxyHost.Text='' then
     result:=result+':'
  else
     result:=result+':'+EProxyHost.Text;

  result:=result+':'+PortToString(ProxyPort);

  if EProxyUser.Text='' then
     result:=result+':'
  else
     result:=result+':'+EProxyUser.Text;

  if EProxyPassword.Text<>'' then
     result:=result+':'+TCrypto.Encrypt(EProxyPassword.Text);

  while (result.Length>0) and (result.Chars[result.Length-1]=':') do
        result:=result.Remove(result.Length-1);
end;

procedure TDataEditor.ClearWeb;
begin
  IChanging:=True;
  try
    EWebServer.Text:='';
    NPort.Value:=TBIWebClient.DefaultPort;
    CBZip.IsChecked:=False;
    LWebTest.Text:='';

    EProxyHost.Text:='';
    ProxyPort.Value:=TBIWebClient.DefaultPort;
    EProxyUser.Text:='';
    EProxyPassword.Text:='';
  finally
    IChanging:=False;
  end;
end;

procedure TDataEditor.SetWebPath(const APath:String);
var tmp : TBIWebClient;
begin
  IChanging:=True;
  try
    tmp:=TBIWebClient.FromPath(APath);

    if tmp<>nil then
    try
      EWebServer.Text:=tmp.Server;
      NPort.Value:=tmp.Port;
      CBZip.IsChecked:=tmp.Compress=TWebCompression.Yes;

      EProxyHost.Text:=tmp.Proxy.Host;
      ProxyPort.Value:=tmp.Proxy.Port;
      EProxyUser.Text:=tmp.Proxy.User;
      EProxyPassword.Text:=tmp.Proxy.Password;

      IWebPath:=APath;

      ResetWebTest;
    finally
      tmp.Free;
    end;
  finally
    IChanging:=False;
  end;
end;

end.
