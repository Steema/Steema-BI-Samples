unit WebBrowser_Load;

interface

uses
  ShDocVW;

//http://delphi.about.com/cs/adptips2004/a/bltip0104_4.htm
procedure WBLoadHTML(const WebBrowser: TWebBrowser; const HTMLCode: string);

implementation

uses
  Windows, ActiveX, Classes, Forms;

//http://delphi.about.com/cs/adptips2004/a/bltip0104_4.htm
procedure WBLoadHTML(const WebBrowser: TWebBrowser; const HTMLCode: string);
var sl: TStringList;
    ms: TMemoryStream;
begin
   WebBrowser.Navigate('about:blank') ;

   while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
         Application.ProcessMessages;

   if Assigned(WebBrowser.Document) then
   begin
     sl := TStringList.Create;
     try
       ms := TMemoryStream.Create;
       try
         sl.Text := HTMLCode;
         sl.SaveToStream(ms) ;
         ms.Seek(0, 0) ;

         (WebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms)) ;
       finally
         ms.Free;
       end;
     finally
       sl.Free;
     end;
   end;
end;

end.
