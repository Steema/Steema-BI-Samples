unit Main_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BI.DataItem;

type
  TMainForm = class(TForm)
    BCreate: TButton;
    BLoad: TButton;
    Button3: TButton;
    LFileName: TLabel;
    BView: TButton;
    BQuery: TButton;
    LLoadTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure BCreateClick(Sender: TObject);
    procedure BViewClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }

    Data : TDataItem;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Create_BigData, System.IOUtils, BI.Persist, BI.UI,
  VCLBI.DataViewer;

// Show the dialog to create and save a dummy big data file
procedure TMainForm.BCreateClick(Sender: TObject);
begin
  Data.Free;  // clear previous data

  with TFormCreate.Create(Self) do
  try
    ShowModal;

    Self.Data:=CreateData;

    BLoad.Enabled:=TFile.Exists(LFileName.Caption);
    BView.Enabled:=True;
    BQuery.Enabled:=True;
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

    BView.Enabled:=True;
    BQuery.Enabled:=True;
  finally
    Screen.Cursor:=crDefault;
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

  BLoad.Enabled:=TFile.Exists(LFileName.Caption);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.Free; // free used memory
end;

end.
