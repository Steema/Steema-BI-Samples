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

{$R *.dfm}

{ TGridChart }

class function TGridChart.Show(const AOwner: TComponent; const Data: TDataItem;
  const Grid: TSquareGrid): TModalResult;
var //tmp,
    x,z : Integer;
begin
  with TGridChart.Create(AOwner) do
  try
    Series1.Marks.Transparent:=True;
    Series1.Marks.Show;

    Chart1.Title.Caption:=Data.Name;

    Series1.CenteredPoints:=True;
    Series1.GetHorizAxis.Increment:=1;
    Series1.GetVertAxis.Increment:=1;

    Series1.Clear;
    Series1.NumXValues:=Length(Grid);

    if Series1.NumXValues>0 then
    begin
      Series1.NumZValues:=Length(Grid[0]);

      Series1.BeginUpdate;
      try
        for x:=1 to Series1.NumXValues do
            for z:=1 to Series1.NumZValues do
            begin
              //tmp:=
              Series1.AddXYZ(x,Grid[x-1,z-1],z);

              {
              if x=z then
                 Series1.SetNull(tmp);
              }
            end;
      finally
        Series1.EndUpdate;
      end;
    end;

    result:=ShowModal;
  finally
    Free;
  end;
end;

end.
