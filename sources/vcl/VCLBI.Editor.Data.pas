{*********************************************}
{  TeeBI Software Library                     }
{  Data Import Definition Editor dialog       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Data;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  BI.DataItem, BI.Persist, BI.DataSource, VCLBI.Editor.Items;

type
  TDataEditor = class(TForm)
    TabSources: TPageControl;
    TabFiles: TTabSheet;
    TabDatabase: TTabSheet;
    TabWeb: TTabSheet;
    PanelButtons: TPanel;
    Panel1: TPanel;
    BOK: TButton;
    Button2: TButton;
    TabStyle: TTabSheet;
    PageControlFile: TPageControl;
    TabFile: TTabSheet;
    TabFolder: TTabSheet;
    Label2: TLabel;
    EFile: TEdit;
    Button3: TButton;
    LFileMissing: TLabel;
    OpenDialog1: TOpenDialog;
    Label3: TLabel;
    EFolder: TEdit;
    Button4: TButton;
    LFolderMissing: TLabel;
    CBAllFolder: TCheckBox;
    CBRecursive: TCheckBox;
    EFileMask: TEdit;
    CBFileType: TComboBox;
    Button5: TButton;
    PageControlDBItems: TPageControl;
    TabConnection: TTabSheet;
    SQL: TTabSheet;
    PageControl3: TPageControl;
    TabItemAllDB: TTabSheet;
    TabSQL: TTabSheet;
    Label7: TLabel;
    CBDBDriver: TComboBox;
    BDBTest: TButton;
    Label8: TLabel;
    EDBServer: TEdit;
    EDBDatabase: TEdit;
    Label9: TLabel;
    EDBUser: TEdit;
    Label10: TLabel;
    EDBPassword: TEdit;
    Label11: TLabel;
    CBLogin: TCheckBox;
    CBAllItems: TCheckBox;
    MemoSQL: TMemo;
    MemoDBTest: TMemo;
    Label12: TLabel;
    EExcluded: TEdit;
    EDBInclude: TEdit;
    LDBExclude: TLabel;
    EDBExclude: TEdit;
    LDBInclude: TLabel;
    Button1: TButton;
    LayoutWebData: TPanel;
    Label6: TLabel;
    CBWebData: TComboBox;
    PageControlWeb: TPageControl;
    TabHttpServer: TTabSheet;
    TabHttpProxy: TTabSheet;
    BWebTest: TButton;
    CBZip: TCheckBox;
    EPort: TEdit;
    EWebServer: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    LWebTest: TLabel;
    UDPort: TUpDown;
    Label13: TLabel;
    EProxyHost: TEdit;
    Label14: TLabel;
    EProxyPort: TEdit;
    UDProxyPort: TUpDown;
    Label15: TLabel;
    EProxyUser: TEdit;
    Label16: TLabel;
    EProxyPassword: TEdit;
    Label17: TLabel;
    EDBPort: TEdit;
    CBDBSystem: TCheckBox;
    TabFTP: TTabSheet;
    BFTPTest: TButton;
    Label18: TLabel;
    EFTPServer: TEdit;
    Label19: TLabel;
    EFTPPort: TEdit;
    UDFTPPort: TUpDown;
    Label20: TLabel;
    EFTPUser: TEdit;
    Label21: TLabel;
    EFTPPassword: TEdit;
    LFTPStatus: TLabel;
    Button6: TButton;
    OpenDialogDatabase: TOpenDialog;
    Label1: TLabel;
    EWebURL: TEdit;
    RGKind: TRadioGroup;
    Label22: TLabel;
    ETimeout: TEdit;
    UDTimeout: TUpDown;
    Label23: TLabel;
    TabDBOptions: TTabSheet;
    CBParallel: TCheckBox;
    CBStats: TCheckBox;
    CBDBViews: TCheckBox;
    TabManual: TTabSheet;
    Panel2: TPanel;
    Button7: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure EFileChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure EFolderChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CBAllFolderClick(Sender: TObject);
    procedure CBRecursiveClick(Sender: TObject);
    procedure CBFileTypeChange(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure EWebServerChange(Sender: TObject);
    procedure EPortChange(Sender: TObject);
    procedure CBZipClick(Sender: TObject);
    procedure BWebTestClick(Sender: TObject);
    procedure CBWebDataChange(Sender: TObject);
    procedure CBWebDataDropDown(Sender: TObject);
    procedure CBDBDriverChange(Sender: TObject);
    procedure BDBTestClick(Sender: TObject);
    procedure EDBServerChange(Sender: TObject);
    procedure EDBDatabaseChange(Sender: TObject);
    procedure EDBUserChange(Sender: TObject);
    procedure EDBPasswordChange(Sender: TObject);
    procedure CBLoginClick(Sender: TObject);
    procedure CBAllItemsClick(Sender: TObject);
    procedure MemoSQLChange(Sender: TObject);
    procedure EFileMaskChange(Sender: TObject);
    procedure EExcludedChange(Sender: TObject);
    procedure EDBIncludeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EProxyHostChange(Sender: TObject);
    procedure EProxyUserChange(Sender: TObject);
    procedure EProxyPasswordChange(Sender: TObject);
    procedure EProxyPortChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EDBPortChange(Sender: TObject);
    procedure CBDBSystemClick(Sender: TObject);
    procedure EFTPServerChange(Sender: TObject);
    procedure EFTPPortChange(Sender: TObject);
    procedure BFTPTestClick(Sender: TObject);
    procedure EFTPUserChange(Sender: TObject);
    procedure EFTPPasswordChange(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure LayoutWebDataResize(Sender: TObject);
    procedure EWebURLChange(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure ETimeoutChange(Sender: TObject);
    procedure CBParallelClick(Sender: TObject);
    procedure CBStatsClick(Sender: TObject);
    procedure CBDBViewsClick(Sender: TObject);
    procedure PageControlFileChange(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FOnChangeWeb : TNotifyEvent;

    OwnsData,
    IChanging : Boolean;

    IStore : String;

    procedure CheckFileFilter(const ADialog:TOpenDialog);
    procedure CheckDBFileFilter(const ADialog:TOpenDialog);
    procedure DatabaseSettings;
    function DBDriverID:String;
    procedure DoTest;
    procedure EnableTestButton;
    procedure FileSettings;
    procedure FilesSettings;
    function FileTypeExtension(const Index:Integer):String;
    procedure FillDBDrivers;
    procedure FinishAddFilter(const ADialog:TOpenDialog; const AFilter,All:String);
    procedure ManualSettings;
    procedure RefreshSettings;
    procedure ResetWebTest;
    procedure ShowSingleTab(const ATab:TTabSheet);
    procedure TryChange(const ATag,AText:String);
    procedure TryChangeMultiLine(const ATag,AText:String);
    procedure TryEnableOk;
    procedure TryFillWebData;
    procedure TrySetHostPort;
    procedure TryWebChange(const ATag, AText: String);
    procedure WebSettings;
    function WebURL:String;
  protected
    ITabFormats : TTabSheet;
    ItemsEditor : TItemsEditor;

    procedure ClearWeb;
    procedure SetWebPath(const APath:String);
    function WebPath:String;
  public
    { Public declarations }
    Data : TDataDefinition;

    function Description:String;

    class function Edit(const AOwner:TComponent; const AData:TDataDefinition):Boolean; static;

    class function NewDefinition(const AOwner:TComponent;
                       const ADataOwner:TComponent;
                       const AKind:TDataDefinitionKind;
                       out AName:String):TDataDefinition; static;
    procedure Select(const AStore,AName:String); overload;
    procedure Select(const AData:TDataDefinition); overload;

    property OnChangeWeb:TNotifyEvent read FOnChangeWeb write FOnChangeWeb;
  end;

implementation

{$R *.dfm}

uses
  BI.Arrays, BI.UI, VCLBI.Grid, System.Types,

  {$IFDEF FPC}
  BI.FPC, BI.SqlDB,
  {$ELSE}
  System.IOUtils,
  {$ENDIF}

  BI.DB, BI.Web, VCLBI.GridForm,
  VCLBI.Editor.Formats, BI.Languages.English;

{ TDataEditor }

procedure TDataEditor.BFTPTestClick(Sender: TObject);

  procedure TryConnect;
  var FTP : TBIFtp;
  begin
    FTP:=TBIWebClient.FTP(Data);
    try
      FTP.Connect;
    finally
      FTP.Free;
    end;
  end;

begin
  Screen.Cursor:=crHourGlass;
  try
    LFTPStatus.Caption:='';
    try
      TryConnect;
      LFTPStatus.Caption:='Success';
    except
      on E:Exception do
         LFTPStatus.Caption:=E.Message;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TDataEditor.BOKClick(Sender: TObject);
begin
  if RGKind.Visible then
  begin
    Data.Kind:=TDataDefinitionKind(RGKind.ItemIndex);
    Select(Data);

    BOk.Caption:='OK';
    BOk.ModalResult:=mrOk;
  end;
end;

procedure TDataEditor.Button1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TBIDB.IncludedItems(Data);
  try
    TBIGridForm.Present(Self,tmp);
  finally
    tmp.Free;
  end;
end;

procedure TDataEditor.FinishAddFilter(const ADialog:TOpenDialog; const AFilter,All:String);
begin
  ADialog.Filter:='All files|'+All+'|'+AFilter;
  ADialog.FilterIndex:=0;
end;

procedure TDataEditor.CheckFileFilter(const ADialog:TOpenDialog);
var tmpClass : TBIFileSourceClass;
    tmp : String;
    tmpAll : String;
begin
  if ADialog.Filter='' then
  begin
    tmp:='';
    tmpAll:='';

    for tmpClass in TBIFileImporters.Items do
        tmpClass.FileFilter.ToDialogFilter(tmp,tmpAll);

    FinishAddFilter(ADialog,tmp,tmpAll);
  end;
end;

procedure TDataEditor.CheckDBFileFilter(const ADialog:TOpenDialog);
var tmp : String;
    tmpAll : String;
begin
  if ADialog.Filter='' then
  begin
    tmp:='';
    tmpAll:='';

    TBIDB.FileFilter.ToDialogFilter(tmp,tmpAll);

    FinishAddFilter(ADialog,tmp,tmpAll);
  end;
end;

procedure TDataEditor.Button3Click(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EFile.Text);

  if tmp<>'' then
     if TPath.IsRelativePath(tmp) then
        if IStore<>'' then
           tmp:=TStore.FullPath(IStore,tmp);

  CheckFileFilter(OpenDialog1);

  if tmp<>'' then
     OpenDialog1.InitialDir:=TPath.GetDirectoryName(tmp);

  OpenDialog1.FileName:=tmp;

  if OpenDialog1.Execute then
     EFile.Text:=OpenDialog1.FileName;
end;

procedure TDataEditor.Button4Click(Sender: TObject);
var tmp : String;
begin
  tmp:=EFolder.Text;

  if TUICommon.SelectFolder(tmp) then
     EFolder.Text:=tmp;
end;

procedure TDataEditor.Button5Click(Sender: TObject);
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

procedure TDataEditor.Button6Click(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EDBDatabase.Text);

  CheckDBFileFilter(OpenDialogDatabase);

  OpenDialogDatabase.FileName:=tmp;

  if OpenDialogDatabase.Execute then
     EDBDatabase.Text:=OpenDialogDatabase.FileName;
end;

procedure TDataEditor.Button7Click(Sender: TObject);
begin
  TUICommon.GotoURL(Self,'http://'+EWebServer.Text+':'+EPort.Text);
end;

procedure TDataEditor.DoTest;
{$IFNDEF FPC}
var tmp : TBIDBTesterClass;
{$ENDIF}
begin
  {$IFNDEF FPC}
  tmp:=TBIDB.Engine.Tester;

  tmp.Test(Data['DBDriver'],
      Data['DBDatabase'],
      Data['DBServer'],
      Data['DBPort'],
      Data['DBUser'],
      Data['DBPassword'],
      Data.AsBoolean('DBLogin'));
  {$ENDIF}
end;

procedure TDataEditor.BDBTestClick(Sender: TObject);
begin
  MemoDBTest.Lines.Clear;
  MemoDBTest.Visible:=True;
  try
    Screen.Cursor:=crHourGlass;
    try
      DoTest;

      MemoDBTest.Lines.Add(BIMsg_ConnectionTestPassed);
    finally
      Screen.Cursor:=crDefault;
    end;
  except
    on E:Exception do
    begin
      MemoDBTest.Lines.Add(E.Message);

      {$IFNDEF FPC}
      if E.InnerException<>nil then
         MemoDBTest.Lines.Add(E.InnerException.Message);
      {$ENDIF}
    end;
  end;
end;

procedure TDataEditor.BWebTestClick(Sender: TObject);
begin
  try
    TryFillWebData;
    LWebTest.Caption:='OK';
  except
    on E:Exception do
    begin
      LWebTest.Caption:='ERROR: '+E.Message;
    end;
  end;
end;

procedure TDataEditor.CBAllFolderClick(Sender: TObject);
begin
  if CBAllFolder.Checked then
  begin
    CBFileType.ItemIndex:=0;
    EFileMask.Text:='*.*';
  end;

  TryChange('AllFiles',TCommonUI.ToBooleanString(CBAllFolder.Checked));
end;

procedure TDataEditor.CBAllItemsClick(Sender: TObject);
var tmp : Boolean;
begin
  tmp:=CBAllItems.Checked;
  TryChange('DBALL',TCommonUI.ToBooleanString(tmp));

  LDBInclude.Enabled:=not tmp;
  LDBExclude.Enabled:=not tmp;

  EDBInclude.Enabled:=not tmp;
  EDBExclude.Enabled:=not tmp;
end;

procedure TDataEditor.EnableTestButton;
begin
  BDBTest.Enabled:=(DBDriverID<>'') and (TBIDB.Engine.Tester<>nil);
end;

procedure TDataEditor.CBDBDriverChange(Sender: TObject);
begin
  TryChange('DBDriver',DBDriverID);
  EnableTestButton;
end;

procedure TDataEditor.CBDBSystemClick(Sender: TObject);
begin
  Data.AsDatabase.IncludeSystem:=CBDBSystem.Checked;
end;

procedure TDataEditor.CBFileTypeChange(Sender: TObject);
begin
  if CBFileType.ItemIndex>0 then
     EFileMask.Text:='*.'+FileTypeExtension(CBFileType.ItemIndex);
end;

procedure TDataEditor.CBLoginClick(Sender: TObject);
begin
  TryChange('DBLogin',TCommonUI.ToBooleanString(CBLogin.Checked));
end;

procedure TDataEditor.CBParallelClick(Sender: TObject);
begin
  Data.Parallel:=CBParallel.Checked;
end;

procedure TDataEditor.CBRecursiveClick(Sender: TObject);
begin
  TryChange('Recursive',TCommonUI.ToBooleanString(CBRecursive.Checked));
end;

procedure TDataEditor.CBStatsClick(Sender: TObject);
begin
  Data.CalcStats:=CBStats.Checked;
end;

procedure TDataEditor.CBWebDataChange(Sender: TObject);
begin
  TryChange('WebData',CBWebData.Text);
end;

procedure TDataEditor.CBWebDataDropDown(Sender: TObject);
begin
  if CBWebData.Items.Count=0 then
     TryFillWebData;
end;

procedure TDataEditor.CBZipClick(Sender: TObject);
begin
  TryWebChange('Compress',TCommonUI.ToBooleanString(CBZip.Checked));
end;

procedure TDataEditor.CBDBViewsClick(Sender: TObject);
begin
  Data.AsDatabase.IncludeViews:=CBDBViews.Checked;
end;

procedure TDataEditor.ClearWeb;
begin
  IChanging:=True;
  try
    EWebServer.Text:='';
    UDPort.Position:=TBIWebClient.DefaultPort;
    UDTimeout.Position:=TBIWebClient.DefaultTimeout;
    CBZip.Checked:=False;
    LWebTest.Caption:='';

    EProxyHost.Text:='';
    UDProxyPort.Position:=TBIWebClient.DefaultPort;
    EProxyUser.Text:='';
    EProxyPassword.Text:='';
  finally
    IChanging:=False;
  end;
end;

function TDataEditor.DBDriverID: String;
begin
  if CBDBDriver.ItemIndex=-1 then
     result:=''
  else
     result:=TBIDB.Engine.GetDriver(CBDBDriver.ItemIndex);
end;

function TDataEditor.Description: String;
begin
  case Data.Kind of
       Files: begin
                if EFolder.Text='' then
                   result:=TPath.GetFileName(EFile.Text)
                else
                if EFTPServer.Text='' then
                   result:=TPath.GetFileName(EFolder.Text)
                else
                   result:=EFTPServer.Text;
              end;

    Database: result:=EDBDatabase.Text;
         Web: result:=CBWebData.Text;
  else
    result:=BIMsg_UnNamed;
  end;
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

procedure TDataEditor.TryEnableOk;

  function TryFiles:Boolean;
  begin
    if PageControlFile.ActivePage=TabFile then
       result:=not LFileMissing.Visible
    else
    if PageControlFile.ActivePage=TabFolder then
       result:=not LFolderMissing.Visible
    else
       result:=BFTPTest.Enabled;
  end;

  function TryDatabase:Boolean;
  begin
    result:=Trim(EDBServer.Text)<>'';
  end;

  function TryWeb:Boolean;
  begin
    result:=BWebTest.Enabled or (Trim(EWebURL.Text)<>'');
  end;

  function TryManual:Boolean;
  begin
    result:=True;
  end;

begin
  case RGKind.ItemIndex of
    0: BOk.Enabled:=TryFiles;
    1: BOk.Enabled:=TryDatabase;
    2: BOk.Enabled:=TryWeb;
    3: BOk.Enabled:=TryManual;
  else
    BOk.Enabled:=False;
  end;
end;

procedure TDataEditor.EDBServerChange(Sender: TObject);
begin
  TryChange('DBServer',EDBServer.Text);
  TryEnableOk;
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

    RGKind.Visible:=(AData=nil) or (AData.Kind=TDataDefinitionKind.Unknown);

    IStore:=AData.Store;
    Data:=AData;

    PanelButtons.Visible:=True;

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TDataEditor.EExcludedChange(Sender: TObject);
begin
  TryChange('ExcludeMask',(Sender as TEdit).Text);
end;

procedure TDataEditor.EFileChange(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EFile.Text);
  TryChange('FileName',tmp);

  if TCommonUI.IsURL(tmp) or TStore.IsRemote(IStore) or (EFTPServer.Text<>'') then
     LFileMissing.Visible:=False
  else
  begin
    if TPath.IsRelativePath(tmp) then
       tmp:=TStore.FullPath(IStore,tmp);

    LFileMissing.Visible:=not FileExists(tmp);
  end;

  TryEnableOk;
end;

procedure TDataEditor.EFileMaskChange(Sender: TObject);
var t : Integer;
begin
  TryChange('IncludeMask',EFileMask.Text);

  for t:=1 to CBFileType.Items.Count-1 do
      if SameText(EFileMask.Text,'*.'+FileTypeExtension(t)) then
      begin
        CBFileType.ItemIndex:=t;
        Exit;
      end;

  CBFileType.ItemIndex:=0;
end;

procedure TDataEditor.EFolderChange(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EFolder.Text);
  TryChange('Folder',tmp);

  if TStore.IsRemote(IStore) or (EFTPServer.Text<>'') then
     LFolderMissing.Visible:=False
  else
  begin
    if tmp='' then
       LFolderMissing.Visible:=True
    else
    begin
      if TPath.IsRelativePath(tmp) then
         tmp:=TStore.FullPath(IStore,tmp);

      LFolderMissing.Visible:=not {$IFNDEF FPC}System.{$ENDIF}SysUtils.DirectoryExists(tmp);
    end;
  end;

  TryEnableOk;
end;

procedure TDataEditor.EFTPPasswordChange(Sender: TObject);
begin
  TryChange('FTPPass',TCrypto.Encrypt(EFTPPassword.Text));
end;

procedure TDataEditor.EFTPPortChange(Sender: TObject);
begin
  TryChange('FTPPort',IntToStr(UDFTPPort.Position));
end;

procedure TDataEditor.EFTPServerChange(Sender: TObject);
var s : String;
begin
  s:=Trim(EFTPServer.Text);
  TryChange('FTPServer',s);

  BFTPTest.Enabled:=s<>'';

  TryEnableOk;
end;

procedure TDataEditor.EFTPUserChange(Sender: TObject);
begin
  TryChange('FTPUser',EFTPUser.Text);
end;

procedure TDataEditor.EPortChange(Sender: TObject);
begin
  TryWebChange('WebPort',IntToStr(UDPort.Position));
end;

procedure TDataEditor.EProxyHostChange(Sender: TObject);
begin
  TryWebChange('WebProxyServer',EProxyHost.Text);
end;

procedure TDataEditor.EProxyPasswordChange(Sender: TObject);
begin
  TryWebChange('WebProxyPass',TCrypto.Encrypt(EProxyPassword.Text));
end;

procedure TDataEditor.EProxyPortChange(Sender: TObject);
begin
  TryWebChange('WebProxyPort',IntToStr(UDProxyPort.Position));
end;

procedure TDataEditor.EProxyUserChange(Sender: TObject);
begin
  TryWebChange('WebProxyUser',EProxyUser.Text);
end;

procedure TDataEditor.ETimeoutChange(Sender: TObject);
begin
  TryWebChange('Timeout',IntToStr(UDTimeout.Position));
end;

procedure TDataEditor.EWebServerChange(Sender: TObject);
begin
  TryWebChange('WebServer',EWebServer.Text);
  TryEnableOk;
end;

function TDataEditor.WebURL:String;
begin
  result:=Data['WebURL'];
end;

procedure TDataEditor.TrySetHostPort;
var tmp : TWebURL;
begin
  tmp:=TBIHttp.Engine.Parse(WebURL);

  if tmp.Host<>'' then
     EWebServer.Text:=tmp.Host;

  if tmp.Port>0 then
     UDPort.Position:=tmp.Port;
end;

procedure TDataEditor.EWebURLChange(Sender: TObject);
begin
  Data['WebURL']:=Trim(EWebURL.Text);

  if WebURL<>'' then
  begin
    TrySetHostPort;
    CBWebData.ItemIndex:=-1;
  end;

  ResetWebTest;
end;

function TDataEditor.FileTypeExtension(const Index: Integer): String;
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

procedure TDataEditor.FillDBDrivers;
var S : String;
begin
  if CBDBDriver.Items.Count=0 then
  begin
    CBDBDriver.Items.BeginUpdate;
    try
      for S in TBIDB.Engine.DriverNames do
          CBDBDriver.Items.Add(S);
    finally
      CBDBDriver.Items.EndUpdate;
    end;
  end;
end;

procedure TDataEditor.FormCreate(Sender: TObject);

  procedure FixPort(const AUpDown:TUpDown; const APosition:Integer);
  begin
    AUpDown.Min:=0;
    AUpDown.Max:={$IFDEF FPC}32767{$ELSE}65535{$ENDIF};
    AUpDown.Position:=APosition;
  end;

begin
  IChanging:=True;
  OwnsData:=True;

  PageControlFile.ActivePage:=TabFile;
  PageControlDBItems.ActivePage:=TabConnection;

  // Just in case: (min max get -1 strange values depending the ide opening this form)
  FixPort(UDPort,TBIWebClient.DefaultPort);
  FixPort(UDTimeout,TBIWebClient.DefaultTimeout);
  FixPort(UDProxyPort,TBIWebClient.DefaultPort);
  FixPort(UDFTPPort,TBIFtp.DefaultPort);
end;

procedure TDataEditor.FormDestroy(Sender: TObject);
begin
  if OwnsData then
     Data.Free;
end;

procedure TDataEditor.FormShow(Sender: TObject);
begin
  Select(Data);

  IChanging:=False;
end;

procedure TDataEditor.LayoutWebDataResize(Sender: TObject);
begin
  EWebURL.Width:=LayoutWebData.Width-10-EWebURL.Left;
end;

procedure TDataEditor.MemoSQLChange(Sender: TObject);
begin
  TryChangeMultiLine('SQL',MemoSQL.Text);
end;

class function TDataEditor.NewDefinition(const AOwner: TComponent;
                       const ADataOwner:TComponent;
                       const AKind: TDataDefinitionKind;
                       out AName:String): TDataDefinition;
var tmp : TDataEditor;
begin
  tmp:=TDataEditor.Create(AOwner);
  try
    result:=TDataDefinition.Create(ADataOwner);
    try
      tmp.Data:=result;
      tmp.Caption:='New Import';
      tmp.Position:=TPosition.poOwnerFormCenter;

      result['Kind']:=TDataDefinition.KindToString(AKind);

      if tmp.ShowModal=mrOk then
         AName:=tmp.Description
      else
      begin
        result.Free;
        result:=nil;
      end;

    except
      on Exception do
      begin
        result.Free;

        {$IFDEF FPC}
        result:=nil;
        {$ENDIF}

        raise;
      end;
    end;
  finally
    tmp.Data:=nil;
    tmp.Free;
  end;
end;

procedure TDataEditor.PageControlFileChange(Sender: TObject);
begin
  TryEnableOk;
end;

procedure TDataEditor.Select(const AStore, AName: String);
begin
  IStore:=AStore;

  Data.Free;
  Data:=TStore.GetDefinition(IStore,AName);

  Select(Data);
end;

procedure TDataEditor.FileSettings;
var tmpPort : Integer;
begin
  ShowSingleTab(TabFiles);

  // First, load FTP related fields
  EFTPServer.Text:=Data['FTPServer'];

  if TryStrToInt(Data['FTPPort'],tmpPort) then
     UDFTPPort.Position:=tmpPort;

  EFTPUser.Text:=Data['FTPUser'];
  EFTPPassword.Text:=TCrypto.Decrypt(Data['FTPPass']);

  // Then, normal file or folder settings

  EFile.Text:=Data['FileName'];
  EFileChange(Self);

  EFolder.Text:=Data['Folder'];
  EFolderChange(Self);

  CBAllFolder.Checked:=Data.AsBoolean('AllFiles');
  EFileMask.Text:=Data['IncludeMask'];
  EExcluded.Text:=Data['ExcludeMask'];

  CBRecursive.Checked:=Data.AsBoolean('Recursive');

  if EFolder.Text='' then
     PageControlFile.ActivePage:=TabFile
  else
     PageControlFile.ActivePage:=TabFolder;
end;

procedure TDataEditor.ShowSingleTab(const ATab:TTabSheet);
var t : Integer;
begin
  TabSources.ActivePage:=ATab;

  for t:=0 to TabSources.PageCount-1 do
      TabSources.Pages[t].TabVisible:=TabSources.Pages[t]=ATab;
end;

procedure TDataEditor.FilesSettings;
begin
  FileSettings;

  ITabFormats:=TTabSheet.Create(Self);
  ITabFormats.Caption:='Options';
  ITabFormats.PageControl:=TabSources;

  TDataFormatEditor.Embedd(Data,ITabFormats);
end;

procedure TDataEditor.DatabaseSettings;
var tmp : String;
begin
  MemoDBTest.Lines.Clear;
  MemoDBTest.Visible:=False;

  ShowSingleTab(TabDatabase);

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
  CBLogin.Checked:=Data.AsBoolean('DBLogin');

  CBAllItems.Checked:=Data.AsBoolean('DBALL');
  CBDBSystem.Checked:=Data.AsDatabase.IncludeSystem;
  CBDBViews.Checked:=Data.AsDatabase.IncludeViews;

  CBParallel.Checked:=Data.Parallel;
  CBStats.Checked:=Data.CalcStats;

  EDBInclude.Text:=Data['IncludeMask'];
  EDBExclude.Text:=Data['ExcludeMask'];

  MemoSQL.Text:=Data.MultiLineText('SQL');

  EnableTestButton;

  if CBAllItems.Checked or (MemoSQL.Text='') then
     PageControlDBItems.ActivePage:=TabItemAllDB
  else
     PageControlDBItems.ActivePage:=TabSQL;
end;

procedure TDataEditor.ManualSettings;
begin
  ShowSingleTab(TabManual);

  // Force AsTable (note: remove this when ItemsEditor is capable of multi-table)
  Data.Data.AsTable:=True;

  if ItemsEditor=nil then
     ItemsEditor:=TItemsEditor.Embedd(Self,TabManual,Data.Data.Items)
  else
     ItemsEditor.Refresh(Data.Data.Items);
end;

procedure TDataEditor.WebSettings;
var tmpPort : Integer;
    tmpTimeout : Integer;
    tmp : String;
begin
  ShowSingleTab(TabWeb);

  EWebServer.Text:=Data['WebServer'];

  if TryStrToInt(Data['WebPort'],tmpPort) then
     UDPort.Position:=tmpPort;

  if TryStrToInt(Data['Timeout'],tmpTimeout) then
     UDTimeout.Position:=tmpTimeout;

  CBZip.Checked:=Data.AsBoolean('Compress');

  EWebURL.Text:=WebURL;

  tmp:=Data['WebData'];

  if tmp<>'' then
  begin
    CBWebDataDropDown(Self);
    CBWebData.ItemIndex:=CBWebData.Items.IndexOf(tmp);

    // Strange situation (case-sensitive?), try to solve:
    if CBWebData.ItemIndex=-1 then
    begin
      CBWebData.Items.Add(tmp);
      CBWebData.ItemIndex:=CBWebData.Items.Count-1;
    end;
  end;

  ResetWebTest;
end;

procedure TDataEditor.RefreshSettings;
begin
  ITabFormats.Free;
  ITabFormats:=nil;

  case Data.Kind of
       TDataDefinitionKind.Files: FilesSettings;
    TDataDefinitionKind.Database: DatabaseSettings;
         TDataDefinitionKind.Web: WebSettings;
      TDataDefinitionKind.Manual: ManualSettings;
  else
  begin
    ShowSingleTab(TabStyle);

    if RGKind.Visible then
    begin
      BOk.Enabled:=True;
      BOk.ModalResult:=mrNone;
      BOK.Caption:='&Next';
    end
    else
      TabStyle.TabVisible:=False;
  end;
  end;

  TryEnableOk;
end;

procedure TDataEditor.ResetWebTest;
begin
  CBWebData.Enabled:=(Trim(EWebServer.Text)<>'') and (UDPort.Position>0);

  BWebTest.Enabled:=CBWebData.Enabled;
  LWebTest.Caption:='';

  TryEnableOk;
end;

procedure TDataEditor.Select(const AData: TDataDefinition);
begin
  Data:=AData;

  if Data<>nil then
  begin
    Data.Store:=IStore;

    IChanging:=True;
    try
      RefreshSettings;
    finally
      IChanging:=False;
    end;
  end;
end;

procedure TDataEditor.SetWebPath(const APath: String);
var tmp : TBIWebClient;
begin
  IChanging:=True;
  try
    tmp:=TBIWebClient.FromPath(APath);

    if tmp<>nil then
    try
      EWebServer.Text:=tmp.Server;
      UDPort.Position:=tmp.Port;

      CBZip.Checked:=tmp.Compress=TWebCompression.Yes;

      EProxyHost.Text:=tmp.Proxy.Host;
      UDProxyPort.Position:=tmp.Proxy.Port;
      EProxyUser.Text:=tmp.Proxy.User;
      EProxyPassword.Text:=tmp.Proxy.Password;

      CBWebData.Clear;
      ResetWebTest;
    finally
      tmp.Free;
    end;
  finally
    IChanging:=False;
  end;
end;

procedure TDataEditor.TryChange(const ATag, AText: String);
begin
  if (not IChanging) and (Data<>nil) then
     Data[ATag]:=AText;
end;

procedure TDataEditor.TryChangeMultiLine(const ATag, AText: String);
begin
  TryChange(ATag,StringReplace(AText,sLineBreak,'\CRLF', [rfReplaceAll]));
end;

procedure TDataEditor.TryWebChange(const ATag, AText: String);
begin
  if not IChanging then
  begin
    if (Data=nil) or (Data[ATag]<>AText) then
    begin
      if Assigned(FOnChangeWeb) then
         FOnChangeWeb(Self)
      else
         Data[ATag]:=AText;

      CBWebData.Clear;
      ResetWebTest;
    end;
  end;
end;

procedure TDataEditor.TryFillWebData;
var B : TBIWebClient;
    Old : String;
    tmp : Integer;
begin
  Old:=CBWebData.Text;

  if Data=nil then
     B:=TBIWebClient.FromPath('web:'+WebPath)
  else
     B:=TBIWebClient.Create(Data);

  if B<>nil then
  try
    //B.Compress:=TWebCompression.Yes;
    CBWebData.Items.Text:=B.GetData;

    CBWebData.Items.Insert(0,'');

    tmp:=CBWebData.Items.IndexOf(Old);
    CBWebData.ItemIndex:=tmp;

    if tmp=-1 then
       CBWebData.Text:=Old;
  finally
    B.Free;
  end;
end;

function TDataEditor.WebPath: String;

  function PortToString(const APort:TUpDown):String;
  begin
    if APort.Position<>TBIWebClient.DefaultPort then
       result:=result+':'+IntToStr(APort.Position)
    else
       result:=result+':';
  end;

begin
  result:=EWebServer.Text+PortToString(UDPort);

  if CBZip.Checked then
     result:=result+':zip';

  if EProxyHost.Text='' then
     result:=result+':'
  else
     result:=result+':'+EProxyHost.Text;

  result:=result+':'+PortToString(UDProxyPort);

  if EProxyUser.Text='' then
     result:=result+':'
  else
     result:=result+':'+EProxyUser.Text;

  if EProxyPassword.Text<>'' then
     result:=result+':'+TCrypto.Encrypt(EProxyPassword.Text);

  {$IFDEF FPC}
  while Copy(result,Length(result),1)=':' do
        Delete(result,Length(result),1);
  {$ELSE}
  while (result.Length>0) and (result.Chars[result.Length-1]=':') do
        result:=result.Remove(result.Length-1);
  {$ENDIF}
end;

end.
