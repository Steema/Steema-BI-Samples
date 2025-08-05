unit Query_BigData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.DataControl, VCLBI.Visualizer,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFormQuery = class(TForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    BIComposer1: TBIComposer;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormQuery: TFormQuery;

implementation

{$R *.dfm}

end.
