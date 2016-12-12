unit unit_main_teebi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, BI_Dataset,
  BI_Query, BI_VCL_Grid, BI_VCL_Tree, BI_VCL_Visualizer, BI_VCL_Dashboard,
  BI_Data_Workflow, BI_VCL_Component, BI_Persist, BI_Data_SingleRecord;

type

  { TForm1 }

  TForm1 = class(TForm)
    BIDataset1: TBIDataset;
    BIGrid1: TBIGrid;
    BIQuery1: TBIQuery;
    BITree1: TBITree;
    BIWorkflow1: TBIWorkflow;
    ControlImporter1: TControlImporter;
    DataDefinition1: TDataDefinition;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

end.

