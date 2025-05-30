{*********************************************}
{  TeeBI Software Library                     }
{  Data Diagram of Relationships              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.LinkDiagram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, Vcl.ExtCtrls,
  VCLTee.TeeProcs, BI.DataItem, VCLTee.TeCanvas,
  System.Types, TeeTree, Vcl.StdCtrls, System.IOUtils, BI.Persist, VCLBI.Grid,
  Data.DB, BI.DataSource, BI.DataSet, Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids,
  VCLBI.DataControl;

type
  TDataDiagram = class(TForm)
    Tree1: TTree;
    Panel1: TPanel;
    Button1: TButton;
    BLoad: TButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    DataSource1: TDataSource;
    Label1: TLabel;
    TBFont: TTrackBar;
    CBGDIPlus: TCheckBox;
    Grid: TBIGrid;
    BIDataset1: TBIDataset;
    procedure FormShow(Sender: TObject);
    procedure Tree1BeforeDraw(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure Tree1SelectShape(Sender: TTreeNodeShape);
    procedure TBFontChange(Sender: TObject);
    procedure CBGDIPlusClick(Sender: TObject);
  private
    { Private declarations }
    Data : TDataItem;

    procedure AddConnections;
    procedure AddData;
    function DiagramFile:String;

    function ShapeOf(const AObject:TObject):TTreeNodeShape;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl; const AData:TDataItem):TDataDiagram; static;
    class function Show(const AOwner:TComponent; const AData:TDataItem):TModalResult; static;
  end;

implementation

{$R *.dfm}

uses
  System.Math, BI.Arrays.Strings;

function TDataDiagram.ShapeOf(const AObject:TObject):TTreeNodeShape;
var t : Integer;
begin
  for t:=0 to Tree1.Shapes.Count-1 do
      if Tree1.Shapes[t].TagObject=AObject then
         Exit(Tree1.Shapes[t]);

  result:=nil;
end;

type
  TDataAccess=class(TDataItem);

procedure TDataDiagram.AddConnections;

  procedure Add(const ACol:TDataItem);
  var A,B : TTreeNodeShape;
      tmp : TTreeConnection;
      Col : TDataItem;
  begin
    for Col in ACol.Items.AsArray do
      if TDataAccess(Col).HasMaster then
      begin
        A:=ShapeOf(Col);

        if A<>nil then
        begin
          B:=ShapeOf(Col.Master);

          if B<>nil then
          begin
            tmp:=A.AddConnection(B);

            tmp.Border.Color:=clBlack;
            tmp.Border.Style:=psSolid;
            tmp.Border.SmallDots:=False;
            tmp.Style:=csLine;

            B.Font.Color:=clMaroon;
          end;

          A.Font.Color:=clNavy;
        end;
      end;
  end;

var Col : TDataItem;
begin
  for Col in Data.Items.AsArray do
      Add(Col);
end;

procedure TDataDiagram.AddData;

  procedure DoAddItem(const AShape:TTreeNodeShape; const AItem:TDataItem);
  var tmp : TTreeNodeShape;
      Item : TDataItem;
  begin
    tmp:=AShape.AddChild(AItem.Name);
    tmp.TagObject:=AItem;

    for Item in AItem.Items.AsArray do
        DoAddItem(tmp,Item);
  end;

var
  MaxYPos,
  XPos,
  YPos : Integer;

  procedure AddTable(const AData:TDataItem);
  var Item : TDataItem;
      tmp : TTreeNodeShape;
      tmpY : Integer;
  begin
    tmp:=TTreeNodeShape.Create(Self);
    tmp.SimpleText:=AData.Name;

    tmp.Left:=XPos;
    tmp.Top:=YPos;

    tmp.Font.Style:=[fsBold];

    tmp.Tree:=Tree1;
    tmp.TagObject:=AData;

    tmp.ImageIndex:=tiNone;
    tmp.Transparent:=True;

    for Item in AData.Items.AsArray do
        DoAddItem(tmp,Item);

    tmp.Expanded:=True;

    tmp.ReCalcPositions(0);

    tmpY:=tmp.MaxHeightExpandedChilds-tmp.Top;

    if tmpY>MaxYPos then
       MaxYPos:=tmpY;

    Inc(XPos,170);

    if XPos>1024 then
    begin
      XPos:=30;
      Inc(YPos,MaxYPos+10);
      MaxYPos:=0;
    end;
  end;

var Col : TDataItem;
begin
  XPos:=30;
  YPos:=10;
  MaxYPos:=0;

  if not Data.AsTable then
     for Col in Data.Items.AsArray do
         AddTable(Col);
end;

function TDataDiagram.DiagramFile:String;
const
  DiagramExtension='.diagram';
begin
  result:=TPath.Combine(TStore.DefaultName,Data.Name+DiagramExtension);
end;

procedure TDataDiagram.BLoadClick(Sender: TObject);
var s : TStrings;
    a,b,
    tmp : String;
    t,i : Integer;
    tmpNode : TTreeNodeShape;
    p : TStringArray;
begin
  s:=TStringList.Create;
  try
    s.LoadFromFile(DiagramFile);

    for t:=0 to s.Count-1 do
    begin
      tmp:=s[t];

      i:=Pos('=',tmp);

      if i>0 then
      begin
        a:=Trim(Copy(tmp,1,i-1));
        b:=Trim(Copy(tmp,i+1,Length(tmp)));

        tmpNode:=Tree1.Roots.Find(a);

        if tmpNode<>nil then
        begin
          p:=TStringArray.Split(b,',');

          if p.Count>1 then
          begin
            tmpNode.Left:=StrToIntDef(p[0],tmpNode.Left);
            tmpNode.Top:=StrToIntDef(p[1],tmpNode.Top);
          end;

          if p.Count>2 then
             tmpNode.Expanded:=StrToBoolDef(p[2],tmpNode.Expanded);
        end;
      end;
    end;
  finally
    s.Free;
  end;
end;

procedure TDataDiagram.Button1Click(Sender: TObject);
var s : TStrings;
    t : Integer;
    R : TTreeNodeShape;
begin
  s:=TStringList.Create;
  try
    for t:=0 to Tree1.Roots.Count-1 do
    begin
      R:=Tree1.Roots[t];
      s.Add(R.SimpleText+' = '+IntToStr(R.Left)+','+IntToStr(R.Top)+','+BoolToStr(R.Expanded,True));
    end;

    s.SaveToFile(DiagramFile);

    BLoad.Enabled:=True;
  finally
    s.Free;
  end;
end;

procedure TDataDiagram.CBGDIPlusClick(Sender: TObject);
begin
  if CBGDIPlus.Checked then
     Tree1.Canvas:=TGDIPlusCanvas.Create
  else
     Tree1.Canvas:=TTeeCanvas3D.Create;
end;

procedure TDataDiagram.FormShow(Sender: TObject);
begin
  Tree1.Canvas:=TTeeCanvas3D.Create;

  Tree1.AllowDelete:=False;
  //Tree1.AllowResize:=False;

  Tree1.Page.UsePrinter:=False;
  Tree1.Page.Border.Hide;

  Tree1.GlobalFormat.Border.Hide;
  Tree1.GlobalFormat.ImageIndex:=tiNone;
  Tree1.GlobalFormat.Transparent:=True;

  TBFont.Position:=Tree1.GlobalFormat.Font.Size;

  AddData;

  AddConnections;

  Tree1.Designing:=True;

  BLoad.Enabled:=FileExists(DiagramFile);

  if BLoad.Enabled then
     BLoadClick(Self);
end;

class function TDataDiagram.Embedd(const AOwner:TComponent; const AParent:TWinControl; const AData:TDataItem):TDataDiagram;
begin
  result:=TDataDiagram.Create(AOwner);
  result.Data:=AData;

  TUICommon.AddForm(result,AParent);
end;

class function TDataDiagram.Show(const AOwner:TComponent; const AData: TDataItem): TModalResult;
begin
  with TDataDiagram.Create(AOwner) do
  try
    Data:=AData;
    result:=ShowModal;
  finally
    Free;
  end;
end;

procedure TDataDiagram.TBFontChange(Sender: TObject);
begin
  Tree1.GlobalFormat.Font.Size:=TBFont.Position;
end;

procedure TDataDiagram.Tree1BeforeDraw(Sender: TObject);

  function MaxTextWidth(const AList:TNodeShapeList):Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to AList.Count-1 do
        result:=Max(result,AList[t].AdjustedRectangle.Right);
  end;

  // Recursive, find last children
  function LastChildrenOf(const AShape:TTreeNodeShape):TTreeNodeShape;
  begin
    if AShape.Count>0 then
       result:=LastChildrenOf(AShape.Children.Last)
    else
       result:=AShape;
  end;

var t : Integer;
    tmp : TRect;
    r : TTreeNodeShape;
begin
  Tree1.Canvas.Pen.Hide;
  Tree1.Canvas.Brush.Style:=bsSolid;
  Tree1.Canvas.Brush.Color:=RGB($E0,$E0,$E0);

  for t:=0 to Tree1.Roots.Count-1 do
  begin
    r:=Tree1.Roots[t];

    r.ReCalcPositions(t);

    tmp:=r.Bounds;

    if r.Expanded then
    begin
      Dec(tmp.Left,Tree1.CrossBox.Size+8);

      if r.Count>0 then
      begin
        tmp.Right:=MaxTextWidth(r.Children);

        r:=LastChildrenOf(r);
        tmp.Bottom:=r.MaxHeightExpandedChilds;
      end;
    end;

    Tree1.Canvas.Rectangle(tmp,0);
  end;
end;

procedure TDataDiagram.Tree1SelectShape(Sender: TTreeNodeShape);
begin
  if Sender<>nil then
  begin
    BIDataset1.Close;
    BIDataset1.Data:=Sender.TagObject as TDataItem;
    BIDataset1.Open;
  end;
end;

procedure ShowDiagram(const AOwner:TComponent; const AData:TDataItem);
begin
  TDataDiagram.Show(AOwner,AData);
end;

initialization
  TUICommon.Diagram:=ShowDiagram;
finalization
  TUICommon.Diagram:=nil;
end.
