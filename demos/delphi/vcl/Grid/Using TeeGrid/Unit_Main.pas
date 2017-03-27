unit Unit_Main;

interface

(*
  Example using TeeGrid control as the desired grid in TBIGrid.

  To activate, simply add BI.VCL.Grid.TeeGrid unit to your "uses":

  uses
    BI.VCL.Grid.TeeGrid;

  to set the desired plugin class:

  TBIGridPlugin.Engine:=TBITeeGridPlugin;

  TeeGrid provides automatic support for many TeeBI features like column
  totals, custom cell coloring, column sorting etc.

*)

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.StdCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, BI.Data,
  BI.VCL.DataControl,

  BI.VCL.Grid.TeeGrid,
  BI.VCL.Grid;

type
  TFormTeeGrid = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTeeGrid: TFormTeeGrid;

implementation

{$R *.dfm}

uses
  BI.VCL.DataManager;

procedure TFormTeeGrid.Button1Click(Sender: TObject);
begin
  BIGrid1.Data:=TDataManager.Choose(Self,BIGrid1.Data);
end;

procedure TFormTeeGrid.FormCreate(Sender: TObject);
begin
  // Optional, manually change the "grid plugin" to the desired grid class:

  //  TBIGridPlugin.Engine:=TBITeeGridPlugin;
end;

end.
