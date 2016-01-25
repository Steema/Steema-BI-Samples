unit BI.VCL.FilterEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Summary, BI.Data, Vcl.ComCtrls, BI.Expression;

type
  TDataFilterEditor = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    TreeView1: TTreeView;
    LError: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { Private declarations }
    Expression : TExpression;
    Data : TDataItem;
  public
    { Public declarations }
    class function Edit(const AOwner:TComponent; const AExpression:TExpression; const AData:TDataItem):TExpression;
  end;

implementation
