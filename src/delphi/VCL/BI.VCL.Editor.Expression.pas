{*********************************************}
{  TeeBI Software Library                     }
{  Expression Editor                          }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Expression;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls,
  BI.VCL.DataControl, BI.VCL.Tree, BI.Expression, BI.VCL.Editor.Completion,
  Vcl.Buttons;

type
  TExpressionEditor = class(TForm)
    PanelFunctions: TPanel;
    BITree1: TBITree;
    Panel2: TPanel;
    Splitter1: TSplitter;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PageControl1: TPageControl;
    TabExpression: TTabSheet;
    PanelError: TPanel;
    LError: TLabel;
    EditExp: TMemo;
    TabTree: TTabSheet;
    BITree2: TBITree;
    Panel4: TPanel;
    Label2: TLabel;
    LValue: TLabel;
    BDelete: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    SBToogle: TSpeedButton;
    procedure Panel2Resize(Sender: TObject);
    procedure EditExpChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BITree2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BITree2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BITree2Change(Sender: TObject);
    procedure EditExpKeyPress(Sender: TObject; var Key: Char);
    procedure PageControl1Change(Sender: TObject);
    procedure SBToogleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditExpKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }

    FOnChange : TNotifyEvent;
    FOnGetFields : TCompleteEvent;

    IChanging : Boolean;

    NilReference,
    IExp : TExpression;

    Old : TPoint;

    Completion : TExpressionCompletion;
    CompletionHandled : Boolean;

    procedure AddCompletionItems;
    procedure AddPredefined;
    procedure Complete(Sender:TObject; const Text:String);
    procedure CompletionGetFields(Sender: TObject; const Text: String);
    procedure CompletionResized(Sender:TObject);
    procedure DeletedNode(Sender: TObject);
    procedure FillTree(const ATree:TBITree; const AExpression:TExpression);
    function IsValid(const AExpression:TExpression):Boolean;
    procedure NilReferences(const AExpression: TExpression);
    function ParseExpression(const S:String):TExpression;
    procedure RefreshEdit(const AExpression: TExpression);
    procedure RefreshTree;
    procedure RefreshValue(const AExpression:TExpression);
    procedure SetupCompletionSize;
  public
    { Public declarations }

    Resolver : TResolveProc;

    procedure AddFields(const AItems:Array of String);

    class function Edit(const AOwner:TComponent; var AExpression:TExpression;
                        const AResolver:TResolveProc=nil):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TExpressionEditor; static;

    function ParseError(const APos:Integer; const AMessage:String):Boolean;

    procedure Refresh(const AExpression:TExpression);

    property Expression:TExpression read IExp;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property OnGetFields:TCompleteEvent read FOnGetFields write FOnGetFields;
  end;

implementation
