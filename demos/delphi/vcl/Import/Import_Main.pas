unit Import_Main;

{$IF CompilerVersion>26}
{$DEFINE HASFIREDAC}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BI.VCL.Grid,

  {$IFDEF HASFIREDAC}
  FireDAC.Comp.UI, FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Stan.Intf,
  {$ENDIF}

  Vcl.ComCtrls;

type
  TImportDemoForm = class(TForm)
    Panel1: TPanel;
    BNext: TButton;
    Panel2: TPanel;
    Button2: TButton;
    Panel3: TPanel;
    Label1: TLabel;
    Panel4: TPanel;
    Samples: TTreeView;
    BPrevious: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabStructure: TTabSheet;
    BIGrid1: TBIGrid;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BNextClick(Sender: TObject);
    procedure BPreviousClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SamplesChange(Sender: TObject; Node: TTreeNode);
    procedure SamplesDblClick(Sender: TObject);
  private
    { Private declarations }

    {$IFDEF HASFIREDAC}
    WaitCursor : TFDGUIxWaitCursor;
    {$ENDIF}
  public
    { Public declarations }
  end;

var
  ImportDemoForm: TImportDemoForm;

implementation

{$R *.dfm}

uses
  FindSampleData, Import_File, BI.VCL.DataViewer;

procedure TImportDemoForm.BNextClick(Sender: TObject);
begin
 if Samples.Visible then
  begin
    BIGrid1.Data:=ImportFile(PathOf(Samples.Selected));

    Samples.Visible:=False;
    PageControl1.Visible:=True;

    BPrevious.Enabled:=True;
    BNext.Enabled:=False;
  end;
end;

procedure TImportDemoForm.BPreviousClick(Sender: TObject);
begin
  if PageControl1.Visible then
  begin
    BIGrid1.Data:=nil;

    PageControl1.Visible:=False;

    while TabStructure.ControlCount>0 do
       TabStructure.Controls[0].Free;

    Samples.Visible:=True;

    BPrevious.Enabled:=False;

    SamplesChange(Samples,Samples.Selected);
  end;
end;

procedure TImportDemoForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TImportDemoForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;

  FillSampleData(Samples);

  if Samples.Items.Count=0 then
     ShowMessage('No sample data can be found on this machine')
  else
  begin
    Samples.Select(Samples.Items[0]);
    SamplesChange(Samples,Samples.Selected);
  end;

  BIGrid1.ShowItems:=True;

  {$IFDEF HASFIREDAC}
  WaitCursor:=TFDGUIxWaitCursor.Create(Self);
  {$ENDIF}
end;

procedure TImportDemoForm.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabStructure then
     if TabStructure.ControlCount=0 then
        TDataViewer.Embedd(Self,TabStructure,BIGrid1.Data);
end;

procedure TImportDemoForm.SamplesChange(Sender: TObject; Node: TTreeNode);
begin
  BNext.Enabled:=(Node<>nil) and (Node.Parent<>nil) and (Node.Parent.Parent<>nil);
end;

procedure TImportDemoForm.SamplesDblClick(Sender: TObject);
begin
  if Samples.Selected<>nil then
     BNextClick(Self);
end;

end.
