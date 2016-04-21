unit Import_RTTI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BI.VCL.Grid, Vcl.ExtCtrls,
  BI.Data;

type
  TFromRTTI = class(TForm)
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Data : TDataItem;
  public
    { Public declarations }

    class procedure Show(const AOwner: TComponent); static;
  end;

implementation

{$R *.dfm}

uses
  BI.Data.RTTI;

type
  TPerson=record  // <-- can also be a class
  public
    Name : String;
    Salary  : Single;
    BirthDate : TDateTime;
    IsDeveloper : Boolean;
  end;

var
  Persons : Array[0..0] of TPerson;

procedure FillSamplePersons;
begin
  Persons[0].  (Name: "John"; Salary: 1234; Birth
{ TFromRTTI }

procedure TFromRTTI.Button1Click(Sender: TObject);
var tmp : TTypeProvider<TPerson>;
begin
  tmp:=TTypeProvider<TPerson>.Create;
  tmp.Add(Persons);

  Data:=TDataItem.Create();
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
