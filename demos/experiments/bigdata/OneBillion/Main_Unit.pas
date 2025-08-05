unit Main_Unit;

interface

{
  This example shows how TeeBI can work with a big quantity of data.

  1) It creates a dummy database of One Billion cells (thousand millions)

  2) Data is saved to a disk file in your TEMP folder: "big_data.bi" (4.5GB)

  3) It is a Windows 64bit executable because more than 3GB of memory are needed.


  The "Query and Visualize" form uses this big data to do some visualizations.

  Note: Use of charts (free TeeChart or "Pro" version) is optional.
        See $DEFINE USE_CHARTS at Query_BigData unit
}


uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.IOUtils,

  BI.DataItem;

// Please enable the 64bit platform, 32bit can only use 3GB of memory
{$IFNDEF CPUX64}
{$WARN 'This example requires more than 3GB of available RAM memory.'}
{$ENDIF}

type
  TMainForm = class(TForm)
    BCreate: TButton;
    BLoad: TButton;
    Button3: TButton;
    LFileName: TLabel;
    BView: TButton;
    BQuery: TButton;
    LLoadTime: TLabel;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure BCreateClick(Sender: TObject);
    procedure BViewClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BQueryClick(Sender: TObject);
  private
    { Private declarations }

    procedure ShowDataInfo;
  public
    { Public declarations }

    Data : TDataItem;  // <-- the data
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Create_BigData, Query_BigData,

  BI.Persist, BI.UI, VCLBI.DataViewer;

// Show the dialog to create and save a dummy big data file
procedure TMainForm.BCreateClick(Sender: TObject);
begin
  Data.Free;  // clear previous data

  with TFormCreate.Create(Self) do
  try
    ShowModal;

    Self.Data:=BigData;

    BLoad.Enabled:=TFile.Exists(LFileName.Caption);

    ShowDataInfo;
  finally
    Free;
  end;
end;

// Load data from file into memory
procedure TMainForm.BLoadClick(Sender: TObject);
var s : TStopwatch;
begin
  Screen.Cursor:=crHourGlass;
  try
    Data.Free;

    s:=TStopwatch.StartNew;

    Data:=TDataItemPersistence.Load(LFileName.Caption);

    LLoadTime.Caption:=TCommonUI.MSecToString(s.ElapsedMilliseconds);

    ShowDataInfo;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

// Show the example queries form
procedure TMainForm.BQueryClick(Sender: TObject);
begin
  with TFormQuery.Create(Self) do
  try
    Data:=Self.Data;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BViewClick(Sender: TObject);
begin
  TDataViewer.View(Self,Data); // show the data viewer
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LFileName.Caption:=TPath.Combine(TPath.GetTempPath,'big_data.bi');

  BLoad.Enabled:=TFile.Exists(LFileName.Caption);  // can we load it?
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.Free; // free used memory
end;

procedure TMainForm.ShowDataInfo;
begin
  BView.Enabled:=True;
  BQuery.Enabled:=True;

  Memo1.Clear;
  TFormCreate.ShowDataSizes(Data,Memo1.Lines);
end;

end.
