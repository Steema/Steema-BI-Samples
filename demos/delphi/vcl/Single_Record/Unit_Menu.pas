unit Unit_Menu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TSingleRecordMenu = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SingleRecordMenu: TSingleRecordMenu;

implementation

{$R *.dfm}

uses Unit_Main, Unit_UsingProvider;

procedure TSingleRecordMenu.Button1Click(Sender: TObject);
begin
  with TSingleRecordDemo.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TSingleRecordMenu.Button2Click(Sender: TObject);
begin
  with TSingleRecordProviderDemo.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
