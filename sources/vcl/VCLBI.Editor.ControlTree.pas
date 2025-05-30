{*********************************************}
{  TeeBI Software Library                     }
{  VCL Control Tree Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.ControlTree;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  {$IFDEF FPC}
  ColorBox,
  {$ENDIF}
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls;

// Displays a "tree" with all children controls of AControl.
// Useful for debugging purposes.

type
  TBIControlTree = class(TForm)
    TreeView1: TTreeView;
    Panel1: TPanel;
    RGAlign: TRadioGroup;
    CBVisible: TCheckBox;
    EWidth: TEdit;
    UDWidth: TUpDown;
    EHeight: TEdit;
    UDHeight: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    ColorListBox1: TColorListBox;
    BEditChart: TButton;
    Splitter1: TSplitter;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    UDLeft: TUpDown;
    Edit2: TEdit;
    UDTop: TUpDown;
    CBParentColor: TCheckBox;
    CBParentBack: TCheckBox;
    Button1: TButton;
    CBParentFont: TCheckBox;
    FontDialog1: TFontDialog;
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure RGAlignClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure EWidthChange(Sender: TObject);
    procedure EHeightChange(Sender: TObject);
    procedure ColorListBox1Click(Sender: TObject);
    procedure BEditChartClick(Sender: TObject);
    procedure CBParentColorClick(Sender: TObject);
    procedure CBParentBackClick(Sender: TObject);
    procedure CBParentFontClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    procedure RefreshBounds(const AControl:TControl);
  public
    { Public declarations }
    procedure Refresh(const AControl:TWinControl);

    class procedure Edit(const AOwner:TComponent; const AControl:TWinControl); static;
  end;

implementation

{$R *.dfm}

uses
  VCLTee.Chart, VCLTee.EditChar;

type
  TControlAccess=class(TControl);

{ TFormControlTree }

procedure TBIControlTree.RGAlignClick(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
    begin
      TControl(tmp.Data).Align:=TAlign(RGAlign.ItemIndex);
      RefreshBounds(TControl(tmp.Data));
    end;
  end;
end;

procedure TBIControlTree.BEditChartClick(Sender: TObject);
begin
  EditChart(Self,TCustomChart(TreeView1.Selected.Data));
end;

procedure TBIControlTree.Button1Click(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TreeView1.Selected;

  if tmp<>nil then
  begin
    FontDialog1.Font:=TControlAccess(tmp.Data).Font;

    if FontDialog1.Execute then
    begin
      CBParentFont.Checked:=False;
      CBParentFontClick(Self);

      TControlAccess(tmp.Data).Font:=FontDialog1.Font;
    end;
  end;
end;

procedure TBIControlTree.CBParentBackClick(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
       if CBParentBack.Checked then
          TControlAccess(tmp.Data).ControlStyle:=TControlAccess(tmp.Data).ControlStyle+[csParentBackground]
       else
          TControlAccess(tmp.Data).ControlStyle:=TControlAccess(tmp.Data).ControlStyle-[csParentBackground];
  end;
end;

procedure TBIControlTree.CBParentColorClick(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
    begin
      TControlAccess(tmp.Data).ParentColor:=CBParentColor.Checked;

      if not CBParentColor.Checked then
         TControlAccess(tmp.Data).Color:=ColorListBox1.Selected;
    end;
  end;
end;

procedure TBIControlTree.CBParentFontClick(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
       TControlAccess(tmp.Data).ParentFont:=CBParentFont.Checked;
  end;
end;

procedure TBIControlTree.CBVisibleClick(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
       TControl(tmp.Data).Visible:=CBVisible.Checked;
  end;
end;

procedure TBIControlTree.ColorListBox1Click(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TreeView1.Selected;

  if tmp<>nil then
  begin
    TControlAccess(tmp.Data).Color:=ColorListBox1.Selected;

    CBParentBack.Checked:=False;
    CBParentBackClick(Self);
  end;
end;

class procedure TBIControlTree.Edit(const AOwner: TComponent;
  const AControl: TWinControl);
begin
  with TBIControlTree.Create(AOwner) do
  try
    Refresh(AControl);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBIControlTree.EHeightChange(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
       TControl(tmp.Data).Height:=UDHeight.Position;
  end;
end;

procedure TBIControlTree.EWidthChange(Sender: TObject);
var tmp : TTreeNode;
begin
  if not IChanging then
  begin
    tmp:=TreeView1.Selected;

    if tmp<>nil then
       TControl(tmp.Data).Width:=UDWidth.Position;
  end;
end;

procedure TBIControlTree.FormCreate(Sender: TObject);
begin
  TreeView1.ReadOnly:=True;
  TreeView1.HideSelection:=False;
end;

procedure TBIControlTree.Refresh(const AControl: TWinControl);

  procedure Add(const AParent:TTreeNode; const AControl:TControl);
  var t : Integer;
      tmp : TTreeNode;
      tmpS : String;
  begin
    tmpS:=TControlAccess(AControl).Caption;

    if tmpS='' then
    begin
      if AControl is TCustomChart then
         tmpS:=TCustomChart(AControl).Title.Caption;
    end;

    tmp:=TreeView1.Items.AddChildObject(AParent,
               Copy(AControl.ClassName,2,Length(AControl.ClassName))+
               ' '+tmpS,AControl);

    if AControl is TWinControl then
       for t:=0 to TWinControl(AControl).ControlCount-1 do
           Add(tmp,TWinControl(AControl).Controls[t]);
  end;

begin
  Add(nil,AControl);

  if TreeView1.Items.Count>0 then
     TreeView1.Items[0].Expand(True);
end;

procedure TBIControlTree.RefreshBounds(const AControl:TControl);
begin
  UDWidth.Position:=AControl.Width;
  UDHeight.Position:=AControl.Height;

  UDLeft.Position:=AControl.Left;
  UDTop.Position:=AControl.Top;
end;

procedure TBIControlTree.TreeView1Change(Sender: TObject; Node: TTreeNode);

  procedure SetCurrentNode;
  var tmp : TControl;
  begin
    if Node=nil then
       Panel1.Enabled:=False
    else
    begin
      Panel1.Enabled:=True;

      tmp:=TControl(Node.Data);
      RGAlign.ItemIndex:=Ord(tmp.Align);
      CBVisible.Checked:=tmp.Visible;

      RefreshBounds(tmp);

      CBParentColor.Checked:=TControlAccess(tmp).ParentColor;
      CBParentBack.Checked:=csParentBackground in TControlAccess(tmp).ControlStyle;
      CBParentFont.Checked:=TControlAccess(tmp).ParentFont;

      ColorListBox1.Selected:=TControlAccess(tmp).Color;

      BEditChart.Enabled:=tmp is TCustomChart;
    end;
  end;

begin
  IChanging:=True;
  try
    SetCurrentNode;
  finally
    IChanging:=False;
  end;
end;

end.
