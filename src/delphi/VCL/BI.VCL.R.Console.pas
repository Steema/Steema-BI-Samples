unit BI.VCL.R.Console;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, BI.Plugins.R;

type
  TBIRConsole = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    EStatement: TEdit;
    Panel2: TPanel;
    BGo: TButton;
    Panel3: TPanel;
    Button1: TButton;
    procedure EStatementChange(Sender: TObject);
    procedure BGoClick(Sender: TObject);
    procedure EStatementKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure Add(const Value:TStrings);
  end;

implementation
