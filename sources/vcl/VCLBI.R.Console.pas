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

{$R *.dfm}

procedure TBIRConsole.Add(const Value: TStrings);
begin
  Memo.Lines.AddStrings(Value);
end;

procedure ScrollToEnd(const AMemo:TMemo);
begin
  SendMessage(AMemo.Handle, EM_LINESCROLL, 0, AMemo.Lines.Count);
end;

procedure TBIRConsole.BGoClick(Sender: TObject);
var S : String;
begin
  S:=Trim(EStatement.Text);

  if S<>'' then
  begin
    Memo.Lines.BeginUpdate;
    try
      TBIREngine.Engine.Statement(S);
    finally
      Memo.Lines.EndUpdate;
      ScrollToEnd(Memo);
    end;
  end;
end;

procedure TBIRConsole.Button1Click(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TBIRConsole.EStatementChange(Sender: TObject);
begin
  BGo.Enabled:=(TBIREngine.Engine<>nil) and (Trim(EStatement.Text)<>'');
end;

procedure TBIRConsole.EStatementKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
  begin
    Key:=#0; // skip beep
    BGoClick(Self);
  end;
end;

procedure TBIRConsole.Panel1Resize(Sender: TObject);
var tmp : Integer;
begin
  tmp:=Panel1.Width-Panel2.Width-EStatement.Left-8;

  if tmp>8 then
     EStatement.Width:=tmp;
end;

end.
