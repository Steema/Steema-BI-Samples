unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, VCLBI.DataControl, VCLBI.Grid;

type
  TCustomFuncTest = class(TForm)
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    procedure TestByCode;
    procedure TestParsing;
    procedure TestSQL;
  public
    { Public declarations }
  end;

var
  CustomFuncTest: TCustomFuncTest;

implementation

{$R *.dfm}

uses
  BI.DataItem, BI.SQL, BI.Expression.Custom, BI.Expression;

// Return a 10 row table with two fields random values: X and Y
function SampleXY:TDataItem;
var X,Y : TDataItem;
    t : Integer;
begin
  result:=TDataItem.Create(True);

  X:=TDataItem.Create(TDataKind.dkInt32,'X');
  Y:=TDataItem.Create(TDataKind.dkInt32,'Y');

  result.Items.Add(X);
  result.Items.Add(Y);

  result.Resize(10);

  for t:=0 to result.Count-1 do
  begin
    X.Int32Data[t]:=Random(10);
    Y.Int32Data[t]:=Random(10);
  end;
end;

// Create THypot by parsing a string
procedure TCustomFuncTest.TestParsing;
var h : THypot;
begin
  // Create THypot by parsing a string
  h:=TExpression.FromString('hypot(3,4)') as THypot;
  try
    Label1.Caption:=Label1.Caption+' '+String(h.Value);
  finally
    h.Free;
  end;
end;

{$IF CompilerVersion>27}
{$DEFINE XE7}
{$ENDIF}

// Create THypot by code
procedure TCustomFuncTest.TestByCode;
var h : THypot;
    Three,Four : TExpression;

    {$IFNDEF XE7}
    Parameters : TExpressions;
    {$ENDIF}
begin
  // Pass numbers directly
  h:=THypot.Create([2,3]);
  try
    Label1.Caption:='Hypot: '+FormatFloat('0.##',h.Value);

    // Use two number expressions, as an example
    Three:=TIntegerExpression.Create(3);
    Four:=TIntegerExpression.Create(4);

    // Change XY parameters

    {$IFDEF XE7}
    h.Expression:=h.Call([Three,Four]);
    {$ELSE}

    // Pre-XE7, inline array constructor not allowed

    SetLength(Parameters,2);
    Parameters[0]:=Three;
    Parameters[1]:=Four;

    h.Expression:=h.Call(Parameters);

    {$ENDIF}

    Label1.Caption:=Label1.Caption+' '+String(h.Value);

    //  Four.Free;
    //  Three.Free;

  finally
    h.Free;
  end;
end;

// Use Hypot in a SQL select query
procedure TCustomFuncTest.TestSQL;
const
  SQL='select hypot(x,y), x*x, y*y, sqrt((x*x)+(y*y))';

begin
  BIGrid2.Data:=TBISQL.From(BIGrid1.Data,SQL);
end;

procedure TCustomFuncTest.FormCreate(Sender: TObject);
begin
  TestByCode;
  TestParsing;

  BIGrid1.Data:=SampleXY;

  TestSQL;
end;

procedure TCustomFuncTest.FormDestroy(Sender: TObject);
begin
  BIGrid2.Data.Free;
end;

end.
