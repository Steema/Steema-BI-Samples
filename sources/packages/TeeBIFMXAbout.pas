{*********************************************}
{  TeeBI Software Library                     }
{  About...                                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit TeeBIFMXAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21_0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<28}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.Controls.Presentation,
  {$ENDIF}
  
  FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Objects;

type
  TAboutBI = class(TForm)
    Image1: TImage;
    Layout1: TLayout;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    LCopyright: TLabel;
    LVersion: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Show(const AOwner:TComponent);
  end;

implementation

{$R *.fmx}

uses
  BI.Languages.English, FMXBI.Grid;

procedure TAboutBI.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBI.FormCreate(Sender: TObject);
begin
  LVersion.Text:='v'+IntToStr(TeeBI_Version)+' '+TeeBI_VersionMode;
  LCopyright.Text:=TeeBI_Copyright;
end;

procedure TAboutBI.Label1Click(Sender: TObject);
begin
  TUICommon.GotoURL(nil,Label2.Text+'/product/teebi');
end;

class procedure TAboutBI.Show(const AOwner: TComponent);
begin
  with TAboutBI.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
