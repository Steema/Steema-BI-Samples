unit TeeBIRegisterAbout;

interface

procedure RegisterAboutBox;
procedure RegisterSplashScreen;

implementation

uses
  Windows, ToolsApi, System.SysUtils, 
  BI.Languages.English;
  
function GetSplashBitmap:HBITMAP;
begin
  result:=LoadBitmap(FindResourceHInstance(HInstance), 'TEEBILOGO');
end;

const
  BIMsg_AboutTitle='TeeBI '+TeeBI_VersionString+' '+TeeBI_VersionMode;
  BIMsg_AboutDescription='Business Intelligence and Data Visualization Tools';

var
  AboutBoxIndex: Integer = -1;
  SplashScreenInitialized: Boolean = False;

procedure RegisterAboutBox;
var Image: HBITMAP;
    AboutBoxServices: IOTAAboutBoxServices;
begin
  if AboutBoxIndex = -1 then
  begin
    Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);

    if Assigned(AboutBoxServices) then
    begin
      Image:=GetSplashBitmap;

      if Image<>0 then
      begin
        AboutBoxIndex:=AboutBoxServices.AddPluginInfo(
              BIMsg_AboutTitle,
              BIMsg_AboutDescription,
              Image, False, TeeBI_Copyright);
      end;
    end;
  end;
end;

procedure UnRegisterAboutBox;
var AboutBoxServices: IOTAAboutBoxServices;
begin
  if AboutBoxIndex<>-1 then
  begin
    Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);

    if Assigned(AboutBoxServices) then
    begin
      AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
      AboutBoxIndex:=-1;

      //  AV at IDE shutdown??? --> AboutBoxServices:=nil;
    end;
  end;
end;

procedure RegisterSplashScreen;
var Image: HBITMAP;
begin
  if Assigned(SplashScreenServices) and (not SplashScreenInitialized) then
  begin
    Image:=GetSplashBitmap;

    if Image<>0 then
    begin
      SplashScreenServices.AddPluginBitmap(BIMsg_AboutTitle, Image,
                           False, TeeBI_Copyright);
      SplashScreenInitialized:=True;
    end;
  end;
end;

initialization
finalization
  UnRegisterAboutBox;
end.
