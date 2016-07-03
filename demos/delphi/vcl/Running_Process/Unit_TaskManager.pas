unit Unit_TaskManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.DataControl, BI.VCL.Grid,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormTaskManager = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    CBAutoRefresh: TCheckBox;
    BRefresh: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure CBAutoRefreshClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BRefreshClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTaskManager: TFormTaskManager;

implementation

{$R *.dfm}

uses
  BI.Data.Process;

procedure TFormTaskManager.BRefreshClick(Sender: TObject);
var Old : Integer;
begin
  Old:=BIGrid1.CurrentRow;
  try
    BIGrid1.Data.UnLoadData;
    BIGrid1.RefreshData;
  finally
    BIGrid1.CurrentRow:=Old;
  end;
end;

procedure TFormTaskManager.CBAutoRefreshClick(Sender: TObject);
begin
  Timer1.Enabled:=CBAutoRefresh.Checked;
  BRefresh.Enabled:=not CBAutoRefresh.Checked;
end;

procedure TFormTaskManager.FormCreate(Sender: TObject);
begin
  BIGrid1.Provider:=TProcessList.Create(Self);
end;

procedure TFormTaskManager.Timer1Timer(Sender: TObject);
begin
  BRefreshClick(Self);
end;

end.
