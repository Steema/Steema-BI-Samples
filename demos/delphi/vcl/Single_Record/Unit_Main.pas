unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Data, BI.VCL.DataControl, BI.VCL.Grid,
  BI.VCL.DataSelect, Vcl.ExtCtrls;

// Example using TSingleRecord class in "manual" mode

// The difference between automatic and manual is, in manual mode we
// need to re-create a new TDataItem whenever the current record is changed.

type
  TSingleRecordDemo = class(TForm)
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    Panel1: TPanel;
    procedure BIGrid1DataChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    FSelector : TDataSelector;

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  BI.Data.SingleRecord, BI.Persist;

// When the current row in BIGrid1 is changed,
// manually create a single-record view and display it at BIGrid2
procedure TSingleRecordDemo.BIGrid1DataChange(Sender: TObject);
var tmp : TDataItem;
begin
  BIGrid2.Data.Free; // <-- release old memory

  // Create a new record-view:
  tmp:=TSingleRecord.From(BIGrid1.Data,BIGrid1.CurrentRow);

  // Show current record-view at BIGrid2:
  BIGrid2.Data:=tmp;
end;

procedure TSingleRecordDemo.SelectedData(Sender: TObject);
begin
  // Change the main data table to display at BIGrid1:
  BIGrid1.Data:=FSelector.Selected;
end;

procedure TSingleRecordDemo.FormCreate(Sender: TObject);
begin
  // Create a new data selector panel:
  FSelector:=TDataSelector.Embedd(Self,Panel1,BIGrid1);
  FSelector.OnSelect:=SelectedData;

  // Start with a sample table:
  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Products'];

  // Tell the selector to locate our sample table:
  FSelector.Select(BIGrid1.Data);
end;

procedure TSingleRecordDemo.FormDestroy(Sender: TObject);
begin
  // Just release memory
  BIGrid2.Data.Free;
end;

end.
