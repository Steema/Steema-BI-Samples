unit Unit1;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BI_VCL_Grid, BI_Persist;

{ TForm1 }

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }

    Grid : TBIGrid;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Grid:=TBIGrid.Create(Self);
  Grid.Align:=alClient;
  Grid.Parent:=Self;

  Grid.Data:=TStore.Load('BISamples','SQLite_Demo')['Products'];
end;

end.

