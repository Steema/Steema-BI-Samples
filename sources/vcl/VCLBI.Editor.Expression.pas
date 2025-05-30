{*********************************************}
{  TeeBI Software Library                     }
{  Expression Editor                          }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Expression;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons,
  VCLBI.DataControl, VCLBI.Tree, BI.Expression, VCLBI.Editor.Completion;

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

{$R *.dfm}

uses
  VCLBI.Grid, BI.Persist;

procedure TExpressionEditor.Complete(Sender: TObject; const Text: String);
begin
  EditExp.SelText:=Text;
end;

procedure TExpressionEditor.CompletionGetFields(Sender: TObject; const Text: String);
begin
  if Assigned(FOnGetFields) then
     FOnGetFields(Sender,Text);
end;

type
  TBITreeAccess=class(TBITree);

procedure TExpressionEditor.BDeleteClick(Sender: TObject);
begin
  TBITreeAccess(BITree2).DeletedNode(Self);
end;

procedure TExpressionEditor.BITree2Change(Sender: TObject);
begin
  BDelete.Enabled:=BITree2.Selected<>nil;
end;

procedure TExpressionEditor.BITree2DragDrop(Sender, Source: TObject; X,
  Y: Integer);

var tmp : TExpression;
    tmpNew,
    tmpParent : TBITreeNode;
begin
  tmpParent:=BITree2.NodeAt(X,Y);

  tmpNew:=BITree2.Add(tmpParent,BITree1.SelectedText);

  if tmpParent<>nil then
     BITree2.Expand(tmpParent);

  BITree2.Selected:=tmpNew;

  tmp:=nil; // <-- pending, create tmp new expression from tmpNew node

  RefreshEdit(tmp);
end;

procedure TExpressionEditor.BITree2DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=(Source=BITree1.Plugin.Control) and
          (BITree1.DataOf(BITree1.Selected)<>nil);
end;

class function TExpressionEditor.Edit(const AOwner: TComponent;
  var AExpression: TExpression;
  const AResolver:TResolveProc): Boolean;
begin
  with TExpressionEditor.Create(AOwner) do
  try
    Resolver:=AResolver;

    Refresh(AExpression);

    result:=ShowModal=mrOk;

    if result then
    begin
      AExpression:=IExp;
      IExp:=nil;
    end;
  finally
    Free;
  end;
end;

procedure TExpressionEditor.FillTree(const ATree:TBITree; const AExpression:TExpression);

  // Recursive
  procedure AddNodes(const AParent:TBITreeNode; const AExpression:TExpression);

    function NewNode(const AText:String; const AExpression:TExpression):TBITreeNode;
    begin
      result:=ATree.Add(AParent,AText,AExpression);
    end;

  var tmp : TBITreeNode;
  begin
    if AExpression=nil then
       NewNode('?',nil)
    else
    if AExpression is TDateExpression then
    begin
      tmp:=NewNode('Date',AExpression);
      AddNodes(tmp,TDateExpression(AExpression).Expression);
    end
    else
    if AExpression is TTimeExpression then
    begin
      tmp:=NewNode('Time',AExpression);
      AddNodes(tmp,TDateExpression(AExpression).Expression);
    end
    else
    if AExpression is TDateTimePartExpression then
    begin
      tmp:=NewNode(TDateTimePartExpression(AExpression).Part.ToString,AExpression);
      AddNodes(tmp,TDateTimePartExpression(AExpression).Expression);
    end
    else
    if AExpression is TArithmeticExpression then
    begin
      tmp:=NewNode(TArithmeticExpression(AExpression).Operand.ToString,AExpression);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TLogicalExpression then
    begin
      tmp:=NewNode(TLogicalExpression(AExpression).Operand.ToString,AExpression);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TTextLogicalExpression then
    begin
      tmp:=NewNode(TTextLogicalExpression(AExpression).Operand.ToString,AExpression);

      AddNodes(tmp,TOperandExpression(AExpression).Left);
      AddNodes(tmp,TOperandExpression(AExpression).Right);
    end
    else
    if AExpression is TMathExpression then
    begin
      tmp:=NewNode(TMathExpression(AExpression).Operand.ToString,AExpression);
      AddNodes(tmp,TUnaryExpression(AExpression).Expression);
    end
    else
    if AExpression is TUnaryTextExpression then
    begin
      tmp:=NewNode(TUnaryTextExpression(AExpression).Operand.ToString,AExpression);
      AddNodes(tmp,TUnaryTextExpression(AExpression).Expression);
    end
    else
    if AExpression is TUnaryNotExpression then
    begin
      tmp:=NewNode('not',AExpression);
      AddNodes(tmp,TUnaryExpression(AExpression).Expression);
    end
    else
      NewNode(AExpression.ToString,AExpression);
  end;

begin
  ATree.BeginUpdating;
  try
    ATree.Clear;

    if AExpression<>nil then
       AddNodes(nil,AExpression);
  finally
    ATree.EndUpdating;
  end;
end;

procedure TExpressionEditor.AddCompletionItems;

  procedure Add(const S:String); overload;
  begin
    Completion.Items.Add(S);
  end;

  procedure Add(const Items:Array of String); overload;
  var t : Integer;
  begin
    for t:=Low(Items) to High(Items) do
        Completion.Items.Add(Items[t]);
  end;

var t : Integer;
begin
  Add(['and','Cos','Exp','in','Ln','Log','mod','or','Power','Round','Sin','Sqr','Sqrt','Tan','Trunc']);

  Add(['Lower','Upper','IsEmpty','Length','Trim']);

  for t:=0 to TDateTimepart.Max do
      Add(TDateTimePart.ToCode(t));

  Add(['Starts','Ends','Contains']);

  Add(['IndexOf','Pad','Split','Insert','Remove','Replace','Count','SubString']);
end;

procedure TExpressionEditor.AddFields(const AItems: Array of String);
var t : Integer;
begin
  for t:=Low(AItems) to High(AItems) do
      Completion.Fields.Add(AItems[t]);
end;

procedure TExpressionEditor.AddPredefined;

  procedure AddDateTimeParts(const AParent:TBITreeNode);
  var t : Integer;
  begin
    for t:=1 to TDateTimePart.Max do
        BITree1.Add(AParent,TDateTimePart.ToString(t),TDateTimePartExpression.ClassInfo);
  end;

  procedure AddMath(const ANode:TBITreeNode);
  begin
    BITree1.Add(ANode,'Sin',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Cos',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Tan',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Sqr',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Sqrt',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Log',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Ln',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Exp',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Round',TMathExpression.ClassInfo);
    BITree1.Add(ANode,'Trunc',TMathExpression.ClassInfo);
  end;

  procedure AddText(const ANode:TBITreeNode);
  begin
    BITree1.Add(ANode,'Lower',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'Upper',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'IsEmpty',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'Length',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'Trim',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'Starts',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'Ends',TTextExpression.ClassInfo);
    BITree1.Add(ANode,'Contains',TTextExpression.ClassInfo);
  end;

  procedure AddLogical(const ANode:TBITreeNode);
  begin
    BITree1.Add(ANode,'Equal =',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'NotEqual <>',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'Greater >',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'Lower <',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'GreaterOrEqual >=',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'LowerOrEqual <=',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'And',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'Or',TLogicalExpression.ClassInfo);
    BITree1.Add(ANode,'In',TLogicalExpression.ClassInfo);
  end;

  procedure AddArithmetic(const ANode:TBITreeNode);
  begin
    BITree1.Add(ANode,'Add +',TArithmeticExpression.ClassInfo);
    BITree1.Add(ANode,'Subtract -',TArithmeticExpression.ClassInfo);
    BITree1.Add(ANode,'Multiply *',TArithmeticExpression.ClassInfo);
    BITree1.Add(ANode,'Divide /',TArithmeticExpression.ClassInfo);
    BITree1.Add(ANode,'Mod %',TArithmeticExpression.ClassInfo);
    BITree1.Add(ANode,'Power ^',TArithmeticExpression.ClassInfo);
  end;

  procedure AddTextOperand(const ANode:TBITreeNode);
  begin
    BITree1.Add(ANode,'IndexOf',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'Pad',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'Split',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'Insert',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'Remove',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'Replace',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'Count',TTextOperandExpression.ClassInfo);
    BITree1.Add(ANode,'SubString',TTextOperandExpression.ClassInfo);
  end;

var tmp,
    tmp2,
    tmp3 : TBITreeNode;
begin
  BITree1.Clear;

  {
  tmp:=BITree1.Add('Values');

  BITree1.Add(tmp,'Integer',TIntegerExpression.ClassInfo);
  BITree1.Add(tmp,'Float',TFloatExpression.ClassInfo);
  BITree1.Add(tmp,'Text',TTextExpression.ClassInfo);
  BITree1.Add(tmp,'Date Time',TDateTimeExpression.ClassInfo);
  BITree1.Add(tmp,'Boolean',TBooleanExpression.ClassInfo);
  }

  tmp:=BITree1.Add('Functions');

  AddArithmetic(BITree1.Add(tmp,'Arithmetic'));

  AddLogical(BITree1.Add(tmp,'Logical'));

  tmp2:=BITree1.Add(tmp,'Unary');
    BITree1.Add(tmp2,'Not',TUnaryNotExpression.ClassInfo);

  tmp2:=BITree1.Add(tmp,'Text');
  AddText(tmp2);
  AddTextOperand(tmp2);

  tmp2:=BITree1.Add(tmp,'Date Time');
    BITree1.Add(tmp2,'Date',TDateExpression.ClassInfo);
    BITree1.Add(tmp2,'Time',TTimeExpression.ClassInfo);
    tmp3:=BITree1.Add(tmp2,'Part');
      AddDateTimeParts(tmp3);

  AddMath(BITree1.Add(tmp,'Math'));

  BITree1.Expand(nil,True,True);
end;

procedure TExpressionEditor.FormCreate(Sender: TObject);
begin
  Completion:=TExpressionCompletion.Create(EditExp);

  Completion.OnComplete:=Complete;
  Completion.OnGetFields:=CompletionGetFields;
  Completion.OnResize:=CompletionResized;

  AddCompletionItems;
  AddPredefined;

  BITree2.AllowDelete:=True;
  BITree2.OnDeleting:=DeletedNode;
end;

function TExpressionEditor.IsValid(const AExpression:TExpression):Boolean;
var tmp : TExpression;
begin
  tmp:=ParseExpression(AExpression.ToString);

  result:=tmp<>nil;

  if result then
     tmp.Free;
end;

procedure TExpressionEditor.DeletedNode(Sender: TObject);
var tmp : TBITreeNode;
    tmpExp : TObject;
begin
  tmp:=(Sender as TBITree).Selected;

  if tmp<>nil then
  begin
    tmpExp:=(Sender as TBITree).DataOf(tmp);

    if tmpExp is TExpression then
    begin
      if tmpExp=IExp then
         IExp:=nil;

      if IExp<>nil then
      begin
        NilReference:=TExpression(tmpExp);
        IExp.Traverse(NilReferences);
      end;

      tmpExp.Free;

      RefreshEdit(IExp);

      if IExp=nil then
      begin
        LError.Caption:='';
        LValue.Caption:='';
      end
      else
      if IsValid(IExp) then
         RefreshValue(IExp)
      else
         LValue.Caption:='(error)';
    end;
  end;
end;

procedure TExpressionEditor.FormDestroy(Sender: TObject);
begin
  IExp.Free;
  Completion.Free;
end;

const
  RegKey='Editors\Expression\Completion';

procedure TExpressionEditor.SetupCompletionSize;
var tmp : TControl;
begin
  Old.X:=TBIRegistry.ReadInteger(RegKey,'Width',0);
  Old.Y:=TBIRegistry.ReadInteger(RegKey,'Height',0);

  tmp:=Completion.Control;

  if Old.X=0 then
     Old.X:=tmp.Width
  else
     tmp.Width:=Old.X;

  if Old.Y=0 then
     Old.Y:=tmp.Height
  else
     tmp.Height:=Old.Y;
end;

procedure TExpressionEditor.FormShow(Sender: TObject);
begin
  EditExp.SetFocus;

  SetupCompletionSize;
end;

procedure TExpressionEditor.CompletionResized(Sender:TObject);
var tmp : TControl;
begin
  tmp:=Completion.Control;

  if (tmp.Width<>Old.X) or (tmp.Height<>Old.Y) then
  begin
    Old.X:=tmp.Width;
    Old.Y:=tmp.Height;

    TBIRegistry.WriteInteger(RegKey,'Width',Old.X);
    TBIRegistry.WriteInteger(RegKey,'Height',Old.Y);
  end;
end;

procedure TExpressionEditor.NilReferences(const AExpression: TExpression);
var t : Integer;
    tmpItems : TExpressions;
begin
  if AExpression is TArrayExpression then
  begin
    tmpItems:=TArrayExpression(AExpression).Items;

    for t:=Low(tmpItems) to High(tmpItems) do
        if tmpItems[t]=NilReference then
           tmpItems[t]:=nil;
  end
  else
  if AExpression is TOperandExpression then
  begin
    if TOperandExpression(AExpression).Left=NilReference then
       TOperandExpression(AExpression).Left:=nil;

    if TOperandExpression(AExpression).Right=NilReference then
       TOperandExpression(AExpression).Right:=nil;
  end
  else
  if AExpression is TUnaryExpression then
  begin
    if TUnaryExpression(AExpression).Expression=NilReference then
       TUnaryExpression(AExpression).Expression:=nil;
  end;
end;

procedure TExpressionEditor.RefreshValue(const AExpression:TExpression);
begin
  if AExpression=nil then
     LValue.Caption:=''
  else
  if AExpression.Value=TExpression.Null then
     LValue.Caption:='(null)'
  else
     LValue.Caption:=AExpression.Value;
end;

procedure TExpressionEditor.SBToogleClick(Sender: TObject);
begin
  PanelFunctions.Visible:=SBToogle.Down;
end;

function TExpressionEditor.ParseError(const APos:Integer; const AMessage:String):Boolean;
begin
  LError.Caption:=AMessage;
  result:=True;
end;

function TExpressionEditor.ParseExpression(const S:String):TExpression;
begin
  result:=TExpression.FromString(S,Resolver,ParseError);
end;

procedure TExpressionEditor.EditExpChange(Sender: TObject);
var tmp : TExpression;
begin
  if IChanging then
     Exit;

  if not CompletionHandled then
     if Completion.Visible then
        Completion.AddItems(False);

  LError.Caption:='';

  tmp:=ParseExpression(EditExp.Text);

  if tmp=nil then
  begin
    LValue.Caption:='';
    BITree2.Clear;
    BOk.Enabled:=False;
  end
  else
  begin
    IExp.Free;
    IExp:=tmp;

    if PageControl1.ActivePage=TabTree then
       RefreshTree;

    BOk.Enabled:=True;

    if Assigned(FOnChange) then
       FOnChange(Self);
  end;
end;

procedure TExpressionEditor.RefreshTree;
begin
  RefreshValue(IExp);

  FillTree(BITree2,IExp);

  BITree2.Expand(nil,True,True);
end;

procedure TExpressionEditor.EditExpKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CompletionHandled:=Completion.Handle(Key,Shift);
end;

procedure TExpressionEditor.EditExpKeyPress(Sender: TObject; var Key: Char);
begin
  if CompletionHandled then
  begin
    if Key=#13 then
       Key:=#0;
  end
  else
     CompletionHandled:=Completion.Handle(Key);
end;

class function TExpressionEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl): TExpressionEditor;
begin
  result:=TExpressionEditor.Create(AOwner);
  result.PanelButtons.Visible:=False;

  result.SBToogle.Top:=4;
  result.SBToogle.Parent:=result.PanelError;
  result.LError.Left:=42;

  TUICommon.AddForm(result,AParent);
end;

procedure TExpressionEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabTree then
     RefreshTree;
end;

procedure TExpressionEditor.Panel2Resize(Sender: TObject);
begin
  EditExp.Width:=Panel2.Width-36;
end;

procedure TExpressionEditor.RefreshEdit(const AExpression: TExpression);
begin
  IChanging:=True;
  try
    if AExpression=nil then
       EditExp.Text:=''
    else
       EditExp.Text:=AExpression.ToString;
  finally
    IChanging:=False;
  end;
end;

procedure TExpressionEditor.Refresh(const AExpression: TExpression);
begin
  IExp.Free;
  IExp:=TExpression.Clone(AExpression);

  RefreshEdit(IExp);
  EditExpChange(Self);
end;

end.
