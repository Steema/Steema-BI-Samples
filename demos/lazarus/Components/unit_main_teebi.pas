unit unit_main_teebi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BI.Dataset, BI.Query,
  VCLBI.Grid, VCLBI.Tree, VCLBI.Visualizer, VCLBI.Component, BI.Persist;

type

  { TForm2 }

  TForm2 = class(TForm)
    BIComposer1: TBIComposer;
    BIDataset1: TBIDataset;
    BIGrid1: TBIGrid;
    BIQuery1: TBIQuery;
    BITree1: TBITree;
    ControlImporter1: TControlImporter;
    DataDefinition1: TDataDefinition;
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

end.

