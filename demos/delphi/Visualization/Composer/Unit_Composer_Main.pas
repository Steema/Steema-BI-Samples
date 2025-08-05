{*********************************************}
{  TeeBI Software Library                     }
{  Composer Example                           }
{  Copyright (c) 2025 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_Composer_Main;

{
  This TeeBI example shows how the TBIComposer control automatically
  creates the necessary charts and controls from any Data (TDataItem) object.

  BIComposer1.Data := MyData
}

interface

uses
  Winapi.Windows, Winapi.Messages,

  // RTL
  System.SysUtils, System.Variants, System.Classes,

  // VCL
  Vcl.Graphics, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  // TeeBI core
  BI.DataItem, BI.Summary,

  // TeeBI controls
  VCLBI.DataControl, VCLBI.Visualizer, VCLBI.Grid,

  // Important, use this optional unit to display charts:
  VCLBI.Visualizer.Chart;

type
  TMainForm = class(TForm)
    PanelTop: TPanel;
    PanelExample: TPanel;
    Label1: TLabel;
    LBTest: TListBox;
    Splitter1: TSplitter;
    PanelRight: TPanel;
    MemoSQL: TMemo;
    BIGrid1: TBIGrid;
    BIComposer1: TBIComposer;
    PanelLeft: TPanel;
    Splitter2: TSplitter;
    Button1: TButton;
    ButtonQuery: TButton;
    procedure LBTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonQueryClick(Sender: TObject);
  private
    { Private declarations }

    Data : TDataItem;
    Summary : TSummary;

    procedure ExecuteQuery;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


uses
  // Detect which TeeChart we have, "Pro" or "Lite
  VCLTee.TeeConst, VCLTee.TeeProcs,

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF TEEPRO}
  // Enable "Pro" charting styles
  VCLBI.Chart.ThreeD, VCLBI.Chart.Financial, VCLBI.Chart.Geo,
  {$ENDIF}

  BI.Tests.SummarySamples, BI.SQL,

  VCLBI.Editor.Visualizer, VCLBI.Editor.Summary;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TVisualizerEditor.Edit(Self,BIComposer1);  // show the composer editor
end;

procedure TMainForm.ButtonQueryClick(Sender: TObject);
begin
  if TSummaryEditor.Edit(Self,Summary) then // show the query editor
     ExecuteQuery;
end;

type
  TDataAccess=class(TDataItem);

procedure TMainForm.ExecuteQuery;
begin
  // Obtain the SQL as text from the query:
  MemoSQL.Text:=TBISQL.From(Summary);

  // Execute the query:
  Data.Free;
  Data:=TDataItem.Create(Summary);

  TDataAccess(Data).KeepProvider:=True; // <-- hack, to remove in future updates

  // Set the query results to grid and composer:
  BIGrid1.Data:=Data;
  BIComposer1.Data:=Data;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TSampleSummaries.AddExamples(LBTest.Items);  // adds the list of examples
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

procedure TMainForm.LBTestClick(Sender: TObject);
begin
  // Create the query
  Summary.Free;
  Summary:=Samples.CreateSummary(Self,LBTest.ItemIndex);

  Summary.Description:=LBTest.Items[LBTest.ItemIndex];

  ExecuteQuery;

  ButtonQuery.Enabled:=True;
end;

end.
