unit Query_BigData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.DataControl, VCLBI.Visualizer,
  Vcl.ExtCtrls, Vcl.StdCtrls, BI.DataItem, BI.Persist, BI.Query;

// Comment to disable using TeeChart
{$DEFINE USE_CHARTS}

type
  TFormQuery = class(TForm)
    Panel1: TPanel;
    LBExample: TListBox;
    Splitter1: TSplitter;
    BIQuery1: TBIQuery;
    BIComposer1: TBIComposer;
    BOptions: TButton;
    BQuery: TButton;
    procedure LBExampleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BOptionsClick(Sender: TObject);
    procedure BQueryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Data : TDataItem;
  end;

implementation

{$R *.dfm}

uses
  // These units enable Chart visualization (using TeeChart)
  {$IFDEF USE_CHARTS}
  VCLBI.Visualizer.Chart, VCLBI.Chart.Geo, VCLBI.Chart.ThreeD,
  VCLBI.Chart.Financial, VCLBI.Editor.Visualizer.Chart,
  {$ENDIF}

  VCLBI.Editor.Visualizer, VCLBI.Editor.Query,

  BI.Summary, BI.Geographic;

procedure TFormQuery.BOptionsClick(Sender: TObject);
begin
  TVisualizerEditor.Edit(Self,BIComposer1);
end;

procedure TFormQuery.BQueryClick(Sender: TObject);
begin
  TBIQueryEditor.Edit(Self,BIQuery1);
end;

procedure TFormQuery.FormCreate(Sender: TObject);
begin
  TGeo.Check;
end;

procedure TFormQuery.FormDestroy(Sender: TObject);
begin
  BIComposer1.DestroyData;
end;

procedure TFormQuery.LBExampleClick(Sender: TObject);

  procedure ShowExample(const AData:TDataItem);
  begin
    BIComposer1.DestroyData;
    BIComposer1.Data:=AData;
  end;

begin
  case LBExample.ItemIndex of
    0 : begin
          BIQuery1.Clear;
          BIQuery1.Measures.Add(Data['Customers'],TAggregate.Count);
          BIQuery1.Dimensions.Add(TGeo.Country.Name);

          ShowExample(BIQuery1.Calculate);
        end;
  end;


  BQuery.Enabled:=LBExample.ItemIndex<>-1;
  BOptions.Enabled:=BQuery.Enabled;
end;

end.
