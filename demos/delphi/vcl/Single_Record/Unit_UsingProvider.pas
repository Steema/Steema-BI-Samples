unit Unit_UsingProvider;

interface

// Example using TSingleRecord class in "automatic" mode.

// The difference between automatic and manual is, in automatic mode we
// dont need to re-create a new TDataItem whenever the current record is changed.

// In automatic mode we just change the "Row" property of a TSingleRecord object,
// and all connected controls (grid, chart, etc) will automatically refresh using
// the new record-view

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, VCLBI.DataSelect, BI.SingleRecord, VCLBI.DataControl,
  VCLBI.Grid;

type
  TSingleRecordProviderDemo = class(TForm)
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    procedure FormCreate(Sender: TObject);
    procedure BIGrid1DataChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    FSelector : TDataSelector;

    FSingle : TSingleRecord;

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  BI.DataItem, BI.Persist;

procedure TSingleRecordProviderDemo.SelectedData(Sender: TObject);
begin
  // Change the main data table to display at BIGrid1:
  BIGrid1.Data:=FSelector.Selected;

  // Change it also at our FSingle object:
  FSingle.Source:=BIGrid1.Data;
end;

// When the current row in BIGrid1 is changed,
// we just need to change the "Row" property of our FSingle object:
procedure TSingleRecordProviderDemo.BIGrid1DataChange(Sender: TObject);
begin
  FSingle.Row:=BIGrid1.CurrentRow;
end;

procedure TSingleRecordProviderDemo.FormCreate(Sender: TObject);
begin
  // Create a new data selector panel:
  FSelector:=TDataSelector.Embedd(Self,Panel1,BIGrid1);
  FSelector.OnSelect:=SelectedData;

  // Create a TSingleRecord object:
  FSingle:=TSingleRecord.Create(Self);

  // Start with a sample table:
  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Products'];

  // Tell the selector to locate our sample table:
  FSelector.Select(BIGrid1.Data);

  // Set our FSingle object data:
  FSingle.Source:=BIGrid1.Data;

  // Tell BIGrid2 to use our FSingle object as automatic data "provider":
  BIGrid2.Provider:=FSingle;
end;

procedure TSingleRecordProviderDemo.FormDestroy(Sender: TObject);
begin
  // Just release memory:
  BIGrid2.Data.Free;
end;

end.
