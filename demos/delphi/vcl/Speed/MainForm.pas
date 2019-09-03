{*********************************************}
{  TeeBI Software Library                     }
{  Speed Benchmark Tests                      }
{  Copyright (c) 2015-2018 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.DataItem, BI.Tests.Speed, VCLBI.DataControl, VCLBI.Grid;

type
  TFormSpeed = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    BIGrid1: TBIGrid;
    BExport: TButton;
    Label1: TLabel;
    LTotal: TLabel;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BExportClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }

    Speed : TBISpeedTest;
  public
    { Public declarations }
  end;

var
  FormSpeed: TFormSpeed;

implementation

{$R *.dfm}

uses
  System.Diagnostics, Vcl.Clipbrd, BI.CSV;

// Execute all tests
procedure TFormSpeed.BExportClick(Sender: TObject);
begin
  Clipboard.AsText:=TBICSVExport.AsString(Speed.Results);
  Label1.Visible:=True;
end;

procedure TFormSpeed.Button1Click(Sender: TObject);
var t1 : TStopwatch;
begin
  Screen.Cursor:=crHourGlass;
  try
    Label1.Visible:=False;

    // Clear results
    Speed.Clear;

    t1:=TStopwatch.StartNew;

    Speed.Run;

    LTotal.Caption:='Total time: '+t1.ElapsedMilliseconds.ToString+' msec';

    // Refresh results at Grid
    BIGrid1.RefreshData;

    // Enable export button
    BExport.Enabled:=True;
 finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFormSpeed.CheckBox1Click(Sender: TObject);
begin
  Speed.Parallel_SQL:=CheckBox1.Checked
end;

procedure TFormSpeed.FormCreate(Sender: TObject);
begin
  // Create Speed test object
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
