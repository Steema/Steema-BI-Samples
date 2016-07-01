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
