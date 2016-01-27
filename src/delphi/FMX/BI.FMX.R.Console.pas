unit BI.FMX.R.Console;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ScrollBox, FMX.Memo;

type
  TBIRConsole = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    BGo: TButton;
    EStatement: TEdit;
    Memo: TMemo;
    Button1: TButton;
    procedure EStatementChangeTracking(Sender: TObject);
    procedure BGoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EStatementKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure Add(const Value: TStrings);
  end;

implementation
