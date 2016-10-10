{*********************************************}
{  TeeBI Software Library                     }
{  Speed Benchmark Tests                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.Layouts, FMX.StdCtrls,

  BI.Data, BI.FMX.DataControl, BI.FMX.Grid, BI.Tests.Speed;

type
  TFormSpeed = class(TForm)
    Layout1: TLayout;
    BRun: TButton;
    BIGrid1: TBIGrid;
    LTotal: TLabel;
    procedure BRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    Speed : TBISpeedTest;
  public
    { Public declarations }
  end;

var
  FormSpeed: TFormSpeed;

implementation

{$R *.fmx}

uses
  System.Diagnostics;

procedure TFormSpeed.BRunClick(Sender: TObject);
var t1 : TStopwatch;
begin
  BRun.Enabled:=False;
  try
    // Clear results
    Speed.Clear;

    t1:=TStopwatch.StartNew;

    Speed.Run;

    LTotal.Text:='Total time: '+t1.ElapsedMilliseconds.ToString+' msec';

    // Refresh results at Grid
    BIGrid1.RefreshData;
  finally
    BRun.Enabled:=True;
  end;
end;

procedure TFormSpeed.FormCreate(Sender: TObject);
begin
  Speed:=TBISpeedTest.Create;

  // Set data to grid
  BIGrid1.Data:=Speed.Results;
end;

procedure TFormSpeed.FormDestroy(Sender: TObject);
begin
  // Free Speed object (to avoid memory leak)
  Speed.Free;
end;

end.
