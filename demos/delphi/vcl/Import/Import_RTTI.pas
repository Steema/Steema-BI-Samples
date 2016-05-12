unit Import_RTTI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BI.VCL.Grid, Vcl.ExtCtrls,
  BI.Data, BI.VCL.DataControl;

type
  TFromRTTI = class(TForm)
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    class procedure Show(const AOwner: TComponent); static;
  end;

implementation

{$R *.dfm}

uses
  BI.Data.RTTI, Customers, Space_Flights;

{ TFromRTTI }

procedure TFromRTTI.Button1Click(Sender: TObject);
var tmp : TTypeProvider<TCustomer>;
begin
  tmp:=TTypeProvider<TCustomer>.Create(Self);
  tmp.Add(CustomerArray);

  BIGrid1.Data:=tmp.Data;
end;

class procedure TFromRTTI.Show(const AOwner: TComponent);
begin
  with TFromRTTI.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
