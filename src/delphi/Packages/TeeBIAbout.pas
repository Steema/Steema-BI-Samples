{*********************************************}
{  TeeBI Software Library                     }
{  About...                                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit TeeBIAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, jpeg, ExtCtrls;

type
  TAboutBI = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BOK: TButton;
    Image1: TImage;
    Label1: TLabel;
    LVersion: TLabel;
    Label2: TLabel;
    LCopyright: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Show(const AOwner:TComponent);
  end;

const
  TeeBI_Version=20160523;
  TeeBI_VersionString='v1 20160523';
  TeeBI_VersionMode='(BETA 11.1)';
  TeeBI_Description='TeeBI';
  TeeBI_CopyRight='Copyright '+{$IFDEF LCL}'(C)'{$ELSE}'©'{$ENDIF}+' 2015-2016 by Steema Software';

procedure TeeBIGotoURL(const Handle:HWND; const URL:String);

implementation
