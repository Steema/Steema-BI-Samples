{*********************************************}
{  TeeBI Software Library                     }
{  Expression Completion ListBox              }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Completion;

interface

uses
  System.Classes, System.Types, VCL.Controls, VCL.StdCtrls;

type
  TExpressionCompletion=class;

  TSizeListBox=class(TListBox)
  private
    ICompletion : TExpressionCompletion;
  protected
    {$IFNDEF FPC}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    procedure Resize; override;
  end;

  TCompleteEvent=procedure(Sender:TObject; const Text:String) of object;

  TExpressionCompletion=class
  private
    FControl : TSizeListBox;

    FFields,
    FItems : TStrings;

    FOnComplete : TCompleteEvent;
    FOnGetFields: TCompleteEvent;
    FOnResize : TNotifyEvent;

    FParent : TWinControl;

    HiddenByReturn : Boolean;

    function CaretPosition:TPoint; // in pixels
    procedure CheckControl;
    procedure ControlExit(Sender: TObject);
    procedure ControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function LineHeight:Integer;
    function GetControl: TSizeListBox; // in pixels
    function WordBeforeCaret:String;
  public
    Constructor Create(const AParent:TWinControl);
    Destructor Destroy; override;

    procedure AddItems(const FieldsOnly:Boolean);
    function Handle(var Key:Char):Boolean; overload;
    function Handle(var Key: Word; Shift: TShiftState):Boolean; overload;

    procedure Hide;
    procedure Show(const FieldsOnly:Boolean);
    function Visible:Boolean;

    property Fields:TStrings read FFields;
    property Items:TStrings read FItems;
    property Control:TSizeListBox read GetControl;

    property OnComplete:TCompleteEvent read FOnComplete write FOnComplete;
    property OnGetFields:TCompleteEvent read FOnGetFields write FOnGetFields;
    property OnResize:TNotifyEvent read FOnResize write FOnResize;
  end;

implementation

uses
  Winapi.Windows, Winapi.Messages, VCL.Forms, BI.Expression,
  System.SysUtils;

// Note: Duplicated at BI.Delphi.Generator unit, move both to TUICommon ??
function CharIn(const C:Char; const AChars:TSysCharSet):Boolean;
begin
  result:={$IFDEF FPC}
          (C in AChars)
          {$ELSE}
          {$IF CompilerVersion>12}
          CharInSet(C,AChars)
          {$ELSE}
          (C in AChars)
          {$ENDIF}
          {$ENDIF}
end;

function IsIdentChar(const C:Char):Boolean;
const
  IdentChars = ['A'..'Z', 'a'..'z', '_'{,'0'..'9'}];
begin
  result:=CharIn(C,IdentChars);
end;

{ TExpressionCompletion }

Constructor TExpressionCompletion.Create(const AParent: TWinControl);
begin
  inherited Create;

  FParent:=AParent;
  FItems:=TStringList.Create;
  FFields:=TStringList.Create;
end;

Destructor TExpressionCompletion.Destroy;
begin
  FFields.Free;
  FItems.Free;
  FControl.Free;
  inherited;
end;

function TExpressionCompletion.GetControl: TSizeListBox;
begin
  CheckControl;
  result:=FControl;
end;

procedure TExpressionCompletion.CheckControl;
begin
  if FControl=nil then
  begin
    FControl:=TSizeListBox.Create(FParent);
    FControl.ICompletion:=Self;

    FControl.OnKeyUp:=ControlKeyUp;
    FControl.OnExit:=ControlExit;
  end;
end;

procedure TExpressionCompletion.ControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
     Hide;
end;

procedure TExpressionCompletion.ControlExit(Sender: TObject);
begin
  Hide;
end;

function TExpressionCompletion.Visible: Boolean;
begin
  result:=(FControl<>nil) and FControl.Showing;
end;

function TExpressionCompletion.Handle(var Key: Char): Boolean;
var tmp : String;
begin
  result:=False;

  if Key='.' then
  begin
    Fields.Clear;

    tmp:=WordBeforeCaret;

    if tmp<>'' then
    begin
      if Assigned(FOnGetFields) then
         FOnGetFields(Self,tmp);

      if Visible then
         AddItems(True)
      else
         Show(True);
    end;

    result:=True;
  end
  else
  if (Key=' ') and Visible then
  begin
    Key:=#0;
    result:=True;
  end
  else
  if (Key=#13) and (not Visible) then
     if HiddenByReturn then
        Key:=#0;

  HiddenByReturn:=False;
end;

function TExpressionCompletion.Handle(var Key: Word;
  Shift: TShiftState): Boolean;

  procedure MoveItem(const ADelta:Integer);
  var tmp : Integer;
  begin
    tmp:=FControl.ItemIndex;

    if ADelta<0 then
    begin
      if tmp>0 then
         FControl.ItemIndex:=tmp+ADelta;
    end
    else
      if tmp<FControl.Count-1 then
         FControl.ItemIndex:=tmp+ADelta;
  end;

  function PendingPart(const S:String):String;
  var tmp : String;
  begin
    result:=S;

    tmp:=WordBeforeCaret;

    while result<>'' do
    begin
      if SameText(Copy(result,1,1),Copy(tmp,1,1)) then
      begin
        Delete(result,1,1);
        Delete(tmp,1,1);

        if tmp='' then
           break;
      end
      else
        break;
    end;
  end;

  procedure TryCompletion;
  var tmp : String;
  begin
    if Assigned(FOnComplete) then
    begin
      tmp:=PendingPart(FControl.Items[FControl.ItemIndex]);

      if tmp<>'' then
         FOnComplete(Self,tmp);
    end;
  end;

  function Handled:Boolean;
  begin
    result:=False;

    if Shift=[] then
    begin
      case Key of
          VK_UP: begin result:=True; MoveItem(-1); end;
        VK_DOWN: begin result:=True; MoveItem(1); end;

      VK_ESCAPE: begin result:=True; Hide; end;

      VK_RETURN: if FControl.ItemIndex<>-1 then
                 begin
                   result:=True;

                   TryCompletion;

                   HiddenByReturn:=True;
                   Hide;
                 end;
      end;
    end;
  end;

begin
  HiddenByReturn:=False;

  if (Key=VK_SPACE) and (Shift=[ssCtrl]) then
  begin
    Show(False); // <-- pending: sub-fields
    result:=True;
  end
  else
    result:=Visible and Handled;

  if result then
     Key:=0
end;

procedure TExpressionCompletion.Hide;
begin
  FControl.Hide;
  FParent.SetFocus;
end;

function TExpressionCompletion.WordBeforeCaret:String;
var tmp : Integer;
    tmpC : Char;
begin
  result:='';

  if FParent is TCustomMemo then
  begin
    tmp:=TCustomMemo(FParent).SelStart;

    if Length(TCustomMemo(FParent).Text)>=tmp then
       Dec(tmp);

    while tmp>=0 do
    begin
      tmpC:=TCustomMemo(FParent).Text[tmp+1];

      if IsIdentChar(tmpC) then
         result:=tmpC+result
      else
         break;

      Dec(tmp);
    end;
  end;
end;

procedure TExpressionCompletion.AddItems(const FieldsOnly:Boolean);
var L : Integer;
    tmpAll : Boolean;
    tmp : String;

  procedure TryAdd(const AItems:TStrings);
  var t : Integer;
  begin
    for t:=0 to AItems.Count-1 do
        if tmpAll or SameText(tmp,Copy(AItems[t],1,L)) then
           FControl.Items.Add(AItems[t]);
  end;

begin
  FControl.Items.BeginUpdate;
  try
    FControl.Sorted:=False;
    FControl.Clear;

    if FieldsOnly then
       tmpAll:=True
    else
    begin
      tmp:=WordBeforeCaret;
      tmpAll:=tmp='';
      L:=Length(tmp);
    end;

    TryAdd(Fields);

    if not FieldsOnly then
       TryAdd(Items);

    FControl.Sorted:=True;
  finally
    FControl.Items.EndUpdate;
  end;
end;

function TExpressionCompletion.LineHeight:Integer; // in pixels
var DC         : HDC;
    SaveFont   : HFont;
    Metrics    : TTextMetric;
begin
  if FParent is TMemo then
  begin
    DC:=GetDC(FParent.Handle);
    try
      SaveFont:=SelectObject(DC,TMemo(FParent).Font.Handle);
      try
        GetTextMetrics(DC,Metrics);
        result:=Metrics.tmHeight;
      finally
        SelectObject(DC,SaveFont);
      end;
    finally
      ReleaseDC(FParent.Handle,DC);
    end;
  end
  else
    result:=0;
end;

function TExpressionCompletion.CaretPosition:TPoint; // in pixels
var C : Longint;
    tmp : Integer;
begin
  result.X:=0;
  result.Y:=0;

  if FParent is TCustomMemo then
  begin
    tmp:=TCustomMemo(FParent).SelStart;

    if tmp>=TCustomMemo(FParent).SelLength-1 then
       Dec(tmp);

    C:=FParent.Perform(EM_POSFROMCHAR,tmp,0);

    if C>=0 then
    begin
      result.X:=LoWord(C);
      result.Y:=HiWord(C);
    end;
  end;
end;

procedure TExpressionCompletion.Show(const FieldsOnly:Boolean);
var P : TPoint;
begin
  CheckControl;

  P:=CaretPosition;

  FControl.Left:=P.X;
  FControl.Top:=P.Y+LineHeight+2;

  FControl.Parent:=FParent;
  FControl.Show;

  AddItems(FieldsOnly);

  FControl.ItemIndex:=0;
end;

{$IFNDEF FPC}
procedure TSizeListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.WindowClass.Style:=Params.WindowClass.Style or
                            (CS_VREDRAW+CS_HREDRAW{+CS_DROPSHADOW});

  Params.Style:=Params.Style or WS_THICKFRAME;
end;
{$ENDIF}

procedure TSizeListBox.Resize;
begin
  inherited;

  if Assigned(ICompletion.FOnResize) then
     ICompletion.FOnResize(ICompletion);
end;

end.
