unit VCLBI.GridChart;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}

  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  {$IFDEF MSWINDOWS}
  VclTee.TeeGDIPlus,
  {$ENDIF}

  VCLTee.TeeComma, VCLTee.TeEngine, VCLTee.TeeLegendPalette, VCLTee.TeeSurfa,
  Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart,

  BI.DataItem, BI.Arrays;

type
  TGridChart = class(TForm)
    Chart1: TChart;
    Series1: TColorGridSeries;
    ChartTool1: TLegendPaletteTool;
    TeeCommander1: TTeeCommander;
  private
    { Private declarations }
  public
    { Public declarations }
    class function Show(const AOwner:TComponent; const Data:TDataItem; const Grid:TSquareGrid):TModalResult;
  end;

implementation
