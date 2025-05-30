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
  Controls, Forms, Dialogs, StdCtrls,
  {$IFDEF LCL}
  jpeglib,
  {$ELSE}
  jpeg,
  {$ENDIF}
  ExtCtrls;

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

implementation

{$R *.dfm}
{$R 'BI_Images.res'}

uses
  BI.Languages.English, VCLBI.Grid;

class procedure TAboutBI.Show(const AOwner: TComponent);
begin
  with TAboutBI.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TAboutBI.FormCreate(Sender: TObject);
begin
  Image1.Picture.Bitmap.LoadFromResourceName(HInstance,'TeeBILogo');

  LVersion.Caption:='v'+IntToStr(TeeBI_Version)+' '+TeeBI_VersionMode;
  LCopyright.Caption:=TeeBI_Copyright;
end;

procedure TAboutBI.BOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBI.Label2Click(Sender: TObject);
begin
  TUICommon.GotoURL(Self,Label2.Caption+'/product/teebi');
end;

end.
