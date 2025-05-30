{*********************************************}
{  TeeBI Software Library                     }
{  Base class to implement "Modules"          }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Modules;

interface

uses
  {System.}Classes,
  BI.Web.Context;

type
  TModule=class
  protected
    Root : String;
    Owner : TObject;

    function GetTemplate(const AFile:String; const AContext: TWebContext):String; overload;
    function GetTemplate(const AFile:String; const AContext: TWebContext; const Variables:TStrings):String; overload;
    function GetVariable(const AVariable:String; const AContext: TWebContext):String; virtual;
    class function Host:String; static;
  public
    Constructor Create(const AOwner:TObject); virtual;

    function Document(const AContext: TWebContext):String; virtual;
    function FileOf(const AFile:String):String;
    function Get(const AContext: TWebContext):Boolean; virtual;
    procedure HomePage(const AContext: TWebContext);

    function Post(const AContext: TWebContext):Boolean; virtual;
    function ProcessFile(const ADocument:String; const AContext: TWebContext):Boolean; virtual;
    procedure RaiseInternalError;
    procedure Setup; virtual;
  end;

  TModuleClass=class of TModule;

  TUIModule=class(TModule)
  public
    procedure Add(const Sender:TObject); virtual;
    procedure Refresh(const Sender:TObject); virtual;
  end;

implementation

uses
  {System.}SysUtils,
  BI.Web.Common, BI.DataSource;

{ TModule }

Constructor TModule.Create(const AOwner:TObject);
begin
  inherited Create;
  Owner:=AOwner;
end;

function TModule.Document(const AContext: TWebContext): String;
begin
  result:='';
end;

function TModule.Get(const AContext: TWebContext): Boolean;
begin
  AContext.ContentText:=Document(AContext);
  result:=AContext.ContentText<>'';
end;

function TModule.Post(const AContext: TWebContext): Boolean;
begin
  result:=False;
end;

function TModule.ProcessFile(const ADocument: String;
  const AContext: TWebContext): Boolean;
begin
  result:=False;
end;

procedure TModule.Setup;
begin
end;

class function TModule.Host:String;
begin
  result:=TBIWebCommon.ServerName+':'+IntToStr(TBIWebCommon.ServerPort);
end;

procedure TModule.RaiseInternalError;
begin
  raise Exception.Create('Internal Error. Sorry. Please retry.');
end;

function TModule.FileOf(const AFile:String):String;
begin
  result:=TBIWebCommon.TWebPublic.PathOf(Root,AFile);
end;

procedure TModule.HomePage(const AContext: TWebContext);
begin
  AContext.Redirect('/');
end;

function TModule.GetTemplate(const AFile:String; const AContext: TWebContext;
                             const Variables:TStrings):String;

  function TryReplace(const S:String):String;
  var i : Integer;

    function CloseTag(const S:String; out i2:Integer):String;
    begin
      i2:=i+1;
      result:='';

      while i2<=Length(S) do
      begin
        Inc(i2);

        if S[i2]='>' then
        begin
          Inc(i2);
          break;
        end
        else
           result:=result+S[i2];
      end;
    end;

  var tmp : String;
      i2 : Integer;
  begin
    result:=S;

    repeat
      tmp:='';

      i:=Pos('<$',result);

      if i>0 then
         tmp:=GetTemplate(CloseTag(result,i2),AContext)
      else
      begin
        i:=Pos('<&',result);

        if i>0 then
           tmp:=GetVariable(CloseTag(result,i2),AContext);
      end;

      if i>0 then
         result:=Copy(result,1,i-1)+
                 tmp+
                 Copy(result,i2,Length(S));

    until i=0;
  end;

begin
  result:=TryReplace(TBITextSource.LoadFromFile(FileOf(AFile)));
end;

function TModule.GetVariable(const AVariable: String;
  const AContext: TWebContext): String;
begin
  result:='';
end;

function TModule.GetTemplate(const AFile:String; const AContext: TWebContext):String;
begin
  result:=GetTemplate(AFile,AContext,nil);
end;

{ TUIModule }

procedure TUIModule.Add(const Sender: TObject);
begin
end;

procedure TUIModule.Refresh(const Sender: TObject);
begin
end;

end.
