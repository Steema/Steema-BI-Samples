unit VCLBI.R.Console;

interface

{
  Use this form to interact with R Language engine.

  Type commands at the bottom Edit box to execute them and obtain the output
  in the central Memo.
}

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, BI.Plugins.R;

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
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure Add(const Value:TStrings);
  end;

implementation
