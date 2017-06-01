unit Unit_BITree_Example;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.DataControl, VCLBI.Tree;

type
  TForm2 = class(TForm)
    BITree1: TBITree;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  VCLBI.Tree.TeeTree;

procedure TForm2.FormCreate(Sender: TObject);
var h : TBITreeNode;
begin
  BITree1.ChangePlugin(TTeeTreePlugin);

  h:=BITree1.Add('Hello');

  BITree1.Add(h,'World !');
end;

end.
