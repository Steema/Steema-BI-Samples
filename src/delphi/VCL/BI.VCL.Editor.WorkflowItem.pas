unit BI.VCL.Editor.WorkflowItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.Data.Workflow, BI.Data;

type
  TWorkflowItemEditor = class(TForm)
    PageControl1: TPageControl;
    TabRename: TTabSheet;
    ItemNames: TComboBox;
    ENewName: TEdit;
    TabDelete: TTabSheet;
    DeleteItemNames: TComboBox;
    TabAdd: TTabSheet;
    EAdd: TEdit;
    CBKind: TComboBox;
    TabQuery: TTabSheet;
    MemoSQL: TMemo;
    TabData: TTabSheet;
    BClear: TButton;
    Button3: TButton;
    BEditQuery: TButton;
    procedure CBKindChange(Sender: TObject);
    procedure ENewNameChange(Sender: TObject);
    procedure EAddChange(Sender: TObject);
    procedure MemoSQLChange(Sender: TObject);
    procedure ItemNamesChange(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure BEditQueryClick(Sender: TObject);
  private
    { Private declarations }

    FItem : TWorkflowItem;

    procedure SetAddProperties(const AData:TDataItem);
    procedure SetDataProperties(const AData:TDataItem);
    procedure SetDeleteProperties(const AData:TDataItem);
    procedure SetQueryProperties(const AData:TDataItem);
    procedure SetRenameProperties(const AData:TDataItem);
    procedure ShowHideTabs(const AItem:TWorkflowItem);
  public
    { Public declarations }

    procedure Refresh(const AItem:TWorkflowItem);

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TWorkflowItemEditor; static;
  end;

implementation
