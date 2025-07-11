// Note: Not available for Lazarus yet.
// This small class is used to send an email, using Indy SMTP components.
unit BI.Email.Indy;

interface

type
  TBIEmailIndy=class
  public
    Host,
    EmailFrom,
    Username,
    Password : String;

    procedure Send(const AUser,AEmail,ASubject,ATextContent,AHtmlContent:String);
    class function ValidAddress(const Email:String):Boolean; static;
  end;

implementation

uses
  {System.}Classes,

  {$IFNDEF FPC}
  IdSMTP, IdMessage, IdText,

  //IdMessageBuilder,

  IdSSLOpenSSL, IdExplicitTLSClientServerBase,
  {$ENDIF}

  {System.}SysUtils;

procedure TBIEmailIndy.Send(const AUser,AEmail,ASubject,ATextContent,AHtmlContent:String);
{$IFDEF FPC}
begin
  raise Exception.Create('Indy idSmtp not supported in Lazarus, yet');
end;
{$ELSE}
var SMTP : TIdSMTP;
    Msg : TIdMessage;
//    tmpBuilder : TIdCustomMessageBuilder;
begin
  SMTP:=TIdSMTP.Create(nil);
  try
    Msg := TIdMessage.Create(nil);
    try
      Msg.Subject := ASubject;
      Msg.Recipients.EMailAddresses := AEmail;
      Msg.From.Address := Username; // ? EmailFrom;
      Msg.From.Text := EmailFrom;

      if (ATextContent<>'') and (AHtmlContent<>'') then
      begin
        with TIdText.Create(Msg.MessageParts, nil) do
        begin
          Body.Text:=ATextContent;
          ContentType:='text/plain';
        end;

        with TIdText.Create(Msg.MessageParts, nil) do
        begin
          Body.Text:=AHtmlContent;
          ContentType:='text/html';
          CharSet:='utf-8';
        end;

        Msg.ContentType:='multipart/alternative';
      end
      else
      begin
        Msg.Body.Text:=ATextContent;

        Msg.ContentType:='text/plain';
        Msg.CharSet:='us-ascii';
        Msg.ContentTransferEncoding:='';
      end;

      (*
      if IsHtml then
      begin
        tmpBuilder:=TIdMessageBuilderHtml.Create;
        TIdMessageBuilderHtml(tmpBuilder).Html.Text:=AContent;

        TIdMessageBuilderHtml(tmpBuilder).Attachments.Add(TStringStream.Create(AContent),'text/plain');
      end
      else
        tmpBuilder:=TIdMessageBuilderPlain.Create;

      try
        {
        if Attachments <> nil then
           for s in Attachments do
               builder.Attachments.Add(s);
        }
        tmpBuilder.FillMessage(Msg);
      finally
        tmpBuilder.Free;
      end;

      if not IsHtml then
         Msg.Body.Text := AContent;
      *)

      SMTP.IOHandler:=TIdSSLIOHandlerSocketOpenSSL.Create(SMTP);
      SMTP.UseTLS:=utUseExplicitTLS;
      TIdSSLIOHandlerSocketOpenSSL(SMTP.IOHandler).SSLOptions.Method:=sslvSSLv3;

      SMTP.Host:=Host;
      SMTP.Connect;
      try
        // SMTP.Port:= 587;

        SMTP.Username:=Username;
        SMTP.Password:=Password;
        SMTP.AuthType:=satDefault;

        SMTP.Authenticate;
        SMTP.Send(Msg);
      finally
        SMTP.Disconnect;
      end;
    finally
      Msg.Free;
    end;
  finally
    SMTP.Free;
  end;
end;
{$ENDIF}

class function TBIEmailIndy.ValidAddress(const Email:String):Boolean;
var tmp : String;
    i : Integer;
begin
  tmp:=Trim(Email);

  result:=(tmp<>'');

  if result then
  begin
    i:=Pos('@',tmp);

    if i>0 then
    begin
      result:=(i>1) and (i<Length(tmp)-1);
    end
    else
      result:=False;
  end;
end;

end.
