unit FMXBI.Web.Users;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMXBI.DataControl,
  FMXBI.Grid, BI.Web.Users, FMX.Objects;

type
  TStringEvent=procedure(Sender:TObject; const S:String) of object;

  TUsersForm = class(TForm)
    BIGridUsers: TBIGrid;
    Layout4: TLayout;
    BRemove: TButton;
    Label6: TLabel;
    EUsersFolder: TEdit;
    Avatar: TImage;
    Image1: TImage;
    procedure BIGridUsersDataChange(Sender: TObject);
    procedure BRemoveClick(Sender: TObject);
    procedure EUsersFolderChangeTracking(Sender: TObject);
  private
    { Private declarations }

    FOnChangePublicFolder : TStringEvent;

  public
    { Public declarations }

    Users : TUsers;

    function AvatarStream: TStream;
    property OnChangePublicFolder:TStringEvent read FOnChangePublicFolder write FOnChangePublicFolder;
  end;

implementation

{$R *.fmx}

uses
  Data.DB, System.IOUtils, BI.Persist, BI.Web.Common, BI.Web.Modules,
  BI.Web.Modules.Users, BI.Web.Context, BI.Web.Users.DataItem,
  BI.Web, BI.Web.Indy.Multipart, FMX.TabControl;

procedure TUsersForm.BIGridUsersDataChange(Sender: TObject);
begin
  BRemove.Enabled:=True;

//  Image1.Bitmap:=AvatarBitmap(Users.ID);
end;

procedure TUsersForm.BRemoveClick(Sender: TObject);
var tmp : TField;
begin
  if TUICommon.YesNo('Are you TOTALLY sure? (User will be DELETED completely)') then
  begin
    tmp:=BIGridUsers.DataSet.FieldByName('ID');

    Users.Delete(tmp.Value);

    BIGridUsers.RefreshData;
  end;
end;

function TUsersForm.AvatarStream: TStream;
begin
  result:=TMemoryStream.Create;
  Avatar.Bitmap.SaveToStream(result);
end;

procedure TUsersForm.EUsersFolderChangeTracking(Sender: TObject);
var tmp : String;
begin
  if Assigned(FOnChangePublicFolder) then
  begin
    tmp:=Trim(EUsersFolder.Text);

    if TDirectory.Exists(tmp) then
    begin
      FOnChangePublicFolder(Self,tmp);
      TBIWebConfig.WriteString('UsersFolder',tmp);
    end;
  end;
end;

type
  TUserAvatar=class(TModule)
  public
    Users : TUsersModule;
    UsersForm : TUsersForm;

    function ProcessFile(const ADocument:String; const AContext: TWebContext):Boolean; override;
    procedure Setup; override;
  end;

  TUsersUI=class(TUIModule)
  public
    UI : TUsersForm;

    procedure Add(const Sender: TObject); override;
    procedure Refresh(const Sender: TObject); override;

    procedure Setup; override;
  end;

function TUserAvatar.ProcessFile(const ADocument:String; const AContext: TWebContext):Boolean;

  function AvatarFile(const AID:TUserID):String;
  begin
    result:=TPath.Combine(Users.UserRoot(AID),'useravatar.png');
  end;

  function CheckAvatar(const ABitmap:TBitmap):TBitmap;
  const IconWidth=128;
        IconHeight=128;
  var w,h : Integer;
  begin
    w:=ABitmap.Width;
    h:=ABitmap.Height;

    if (w=IconWidth) and (h=IconHeight) then
       result:=ABitmap
    else
    begin
      result:=TBitmap.Create(IconWidth,IconHeight);
      try
        result.Canvas.BeginScene;
        try
          result.Canvas.DrawBitmap(ABitmap,ABitmap.BoundsF,result.BoundsF,1);
        finally
          result.Canvas.EndScene;
        end;
      finally
        ABitmap.Free;
      end;
    end;
  end;

  function BitmapFromURL(const AURL:String):TBitmap;
  var tmp : TStream;
  begin
    tmp:=TBIURLSource.StreamFrom(AURL);
    try
      result:=TBitmap.CreateFromStream(tmp);
    finally
      tmp.Free;
    end;
  end;

  procedure SaveUserAvatar(const AID:String; const ABitmap:TBitmap);
  var tmp : TBitmap;
  begin
    tmp:=CheckAvatar(ABitmap);
    tmp.SaveToFile(AvatarFile(AID));
  end;

  procedure ChangeAvatarURL;
  var tmpID : TUserID;
      tmpURL : String;
      tmpBitmap : TBitmap;
  begin
    tmpID:=Users.IDOfSession(AContext);

    if tmpID='' then
       AContext.ContentText:=Document(AContext)
    else
    begin
      tmpURL:=Trim(AContext.Params.Values['url']);

      if tmpURL<>'' then
      begin
        tmpBitmap:=BitmapFromURL(tmpURL);
        try
          SaveUserAvatar(tmpID,tmpBitmap);
        finally
          tmpBitmap.Free;
        end;
      end;

      Users.HomePage(AContext);
    end;
  end;

  procedure ChangeAvatarFile;

    procedure SaveFromStream(const AStream:TStream; const AID:String);
    var Dest : TStream;
        tmpName : String;
        tmpBitmap : TBitmap;
    begin
      Dest:=TIndyMultipart.From(AStream,AContext.GetContentType,tmpName,True);

      if Dest=nil then
         Users.RaiseInternalError
      else
      try
        Dest.Position:=0;

        tmpBitmap:=TBitmap.CreateFromStream(Dest);
        try
          SaveUserAvatar(AID,tmpBitmap);
        finally
          tmpBitmap.Free;
        end;
      finally
        Dest.Free;
      end;
    end;

  var tmp : TStream;
      tmpID : TUserID;
  begin
    tmpID:=Users.IDOfSession(AContext);

    if tmpID='' then
       AContext.ContentText:=Document(AContext)
    else
    begin
      tmp:=AContext.GetStream;

      if tmp=nil then
         Users.RaiseInternalError
      else
      try
        SaveFromStream(tmp,tmpID);

        Users.HomePage(AContext);
      finally
        tmp.Free;
      end;
    end;
  end;

  function AvatarBitmap(const AID:TUserID):TBitmap;
  var tmpFile : String;
  begin
    tmpFile:=AvatarFile(AID);

    if TFile.Exists(tmpFile) then
       result:=TBitmap.CreateFromFile(tmpFile)
    else
       result:=nil;
  end;

  procedure ReturnAvatar;
  var tmpID : TUserID;
      tmpStream : TStream;
      tmpBitmap : TBitmap;
  begin
    tmpID:=Users.IDOfSession(AContext);

    if tmpID='' then
       tmpStream:=UsersForm.AvatarStream
    else
    begin
      tmpBitmap:=AvatarBitmap(tmpID);

      if tmpBitmap=nil then
         tmpStream:=UsersForm.AvatarStream
      else
      try
        tmpStream:=TMemoryStream.Create;
        tmpBitmap.SaveToStream(tmpStream);
      finally
        tmpBitmap.Free;
      end;
    end;

    AContext.ReturnIcon(tmpStream)
  end;

begin
  result:=True;

  if SameText(ADocument,'/avatar') then
     ReturnAvatar
  else
  if SameText(ADocument,'/change_avatar_url') then
     ChangeAvatarUrl
  else
  if SameText(ADocument,'/change_avatar_file') then
     ChangeAvatarFile
  else
     result:=False;
end;

procedure TUserAvatar.Setup;
begin
  Users:=TBIWebCommon(Owner).ModuleInstance(TUsersModule) as TUsersModule;
  UsersForm:=(TBIWebCommon(Owner).ModuleInstance(TUsersUI) as TUsersUI).UI;
end;

{ TUsersUI }

procedure TUsersUI.Add(const Sender: TObject);
begin
  UI:=TUsersForm.Create(nil);
  UI.EUsersFolder.Text:=TBIWebCommon(Owner).PublicFolder.Path;

  TUICommon.AddNewTab(UI,Sender as TTabControl);
end;

procedure TUsersUI.Refresh(const Sender: TObject);
begin
  if Sender=UI.BIGridUsers.Parent then

     if UI.Users is TUsersDataItem then
        UI.BIGridUsers.RefreshData
     else
        UI.BIGridUsers.Data:=UI.Users.AsDataItem;
end;

procedure TUsersUI.Setup;
begin
  UI.Users:=(TBIWebCommon(Owner).ModuleInstance(TUsersModule) as TUsersModule).Users;
  UI.BIGridUsers.Data:=UI.Users.AsDataItem;

//  UI.OnChangePublicFolder:=PublicFolderChanged;
end;

initialization
  TBIWebCommon.AddModule(TUserAvatar);
  TBIWebCommon.AddModule(TUsersUI);
end.
