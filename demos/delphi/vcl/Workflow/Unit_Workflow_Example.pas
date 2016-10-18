unit Unit_Workflow_Example;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BI.Data.Workflow, BI.VCL.Editor.Workflow;

type
  TFormWorkflow = class(TForm)
    BIWorkflow1: TBIWorkflow;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWorkflow: TFormWorkflow;

implementation

{$R *.dfm}

procedure TFormWorkflow.FormShow(Sender: TObject);
begin
  TBIWorkflowEditor.Embedd(Self,Self,BIWorkflow1);
end;

end.
