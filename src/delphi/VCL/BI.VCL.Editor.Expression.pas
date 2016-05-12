unit BI.VCL.Editor.Expression;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Variants,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  BI.VCL.DataControl, BI.VCL.Tree, BI.Expression;

type
  TExpressionEditor = class(TForm)
    Panel1: TPanel;
    BITree1: TBITree;
    Panel2: TPanel;
    BITree2: TBITree;
    Splitter1: TSplitter;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    BCancel: TButton;
    Label2: TLabel;
    Panel3: TPanel;
    Label1: TLabel;
    LError: TLabel;
    EditExp: TEdit;
    LValue: TLabel;
    Panel4: TPanel;
    BDelete: TButton;
    procedure Panel2Resize(Sender: TObject);
    procedure EditExpChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BITree2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BITree2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BITree2Change(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    NilReference,
    IExp : TExpression;

    procedure AddPredefined;
    procedure DeletedNode(Sender: TObject);
    procedure FillTree(const ATree:TBITree; const AExpression:TExpression);
    function IsValid(const AExpression:TExpression):Boolean;
    procedure NilReferences(const AExpression: TExpression);
    function ParseExpression(const S:String):TExpression;
    procedure RefreshEdit(const AExpression: TExpression);
    procedure RefreshValue(const AExpression:TExpression);
  public
    { Public declarations }

    Resolver : TResolveProc;

    class function EditNew(const AOwner:TComponent):TExpression; static;
    class function Edit(const AOwner:TComponent; var AExpression:TExpression):Boolean; static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TExpressionEditor; static;

    procedure Refresh(const AExpression:TExpression);
  end;

implementation
