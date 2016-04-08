unit Import_RTTI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BI.VCL.Grid, Vcl.ExtCtrls;

type
  TFromRTTI = class(TForm)
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FromRTTI: TFromRTTI;

implementation

{$R *.dfm}

end.
