unit FMXBI.Web.Settings.Email;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TEmailSettings = class(TForm)
    Label7: TLabel;
    EServerName: TEdit;
    Label8: TLabel;
    EEmailFrom: TEdit;
    Label9: TLabel;
    ESMTPHost: TEdit;
    Label10: TLabel;
    ESMTPUser: TEdit;
    Label11: TLabel;
    ESMTPPassword: TEdit;
    procedure EEmailFromChangeTracking(Sender: TObject);
    procedure EServerNameChangeTracking(Sender: TObject);
    procedure ESMTPHostChangeTracking(Sender: TObject);
    procedure ESMTPPasswordChangeTracking(Sender: TObject);
    procedure ESMTPUserChangeTracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure SetupConfig(const AServer:String);
    class function EmailSetting(const AName:String):String;
  end;

implementation

{$R *.fmx}

uses
  BI.Persist, BI.UI, BI.Web.Modules, BI.Web.Common,

  FMXBI.Grid, FMX.TabControl;

function EmailKey:String;
begin
  result:=TBIWebConfig.Key+'\Email';
end;

procedure TEmailSettings.EEmailFromChangeTracking(Sender: TObject);
begin
  TBIRegistry.WriteString(EmailKey,'From',EEmailFrom.Text);
end;

class function TEmailSettings.EmailSetting(const AName:String):String;
begin
  result:=TBIRegistry.ReadString(EmailKey,AName);
end;

procedure TEmailSettings.EServerNameChangeTracking(Sender: TObject);
begin
  TBIWebConfig.WriteString('Server',EServerName.Text);
end;

procedure TEmailSettings.ESMTPHostChangeTracking(Sender: TObject);
begin
  TBIRegistry.WriteString(EmailKey,'Host',ESMTPHost.Text);
end;

procedure TEmailSettings.ESMTPPasswordChangeTracking(Sender: TObject);
begin
  TBIRegistry.WriteString(EmailKey,'Password',TCrypto.Encrypt(ESMTPPassword.Text));
end;

procedure TEmailSettings.ESMTPUserChangeTracking(Sender: TObject);
begin
  TBIRegistry.WriteString(EmailKey,'Username',ESMTPUser.Text);
end;

procedure TEmailSettings.SetupConfig(const AServer:String);
begin
  EServerName.Text:=AServer;
  EEmailFrom.Text:=EmailSetting('From');
  ESMTPHost.Text:=EmailSetting('Host');
  ESMTPUser.Text:=EmailSetting('Username');
  ESMTPPassword.Text:=TCrypto.Decrypt(EmailSetting('Password'));
end;

type
  TEmailUI=class(TUIModule)
  public
    procedure Add(const Sender:TObject); override;
  end;

procedure TEmailUI.Add(const Sender:TObject);
var Email : TEmailSettings;
begin
  Email:=TEmailSettings.Create(Sender as TComponent);
  TUICommon.AddNewTab(Email,Sender as TTabControl);
  Email.SetupConfig(TBIWebCommon.ServerName);
end;

initialization
  TBIWebCommon.AddModule(TEmailUI);
end.

