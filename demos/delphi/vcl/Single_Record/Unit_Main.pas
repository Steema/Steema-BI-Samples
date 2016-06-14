unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Data, BI.VCL.DataControl, BI.VCL.Grid,
  BI.VCL.DataSelect, Vcl.ExtCtrls;

type
  TSingleRecordDemo = class(TForm)
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    Panel1: TPanel;
    procedure BIGrid1DataChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    FSelector : TDataSelector;

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  SingleRecordDemo: TSingleRecordDemo;

implementation

{$R *.dfm}

uses
  BI.Data.SingleRecord;

procedure TSingleRecordDemo.BIGrid1DataChange(Sender: TObject);
begin
  BIGrid2.Data.Free;
  BIGrid2.Data:=TSingleRecord.From(BIGrid1.Data,BIGrid1.DataSource.DataSet.RecNo-1);
end;

procedure TSingleRecordDemo.SelectedData(Sender: TObject);
begin
  BIGrid1.Data:=FSelector.Selected;
end;

procedure TSingleRecordDemo.FormCreate(Sender: TObject);
begin
  FSelector:=TDataSelector.Embedd(Self,Panel1,BIGrid1);
  FSelector.OnSelect:=SelectedData;
end;

end.
