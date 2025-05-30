{*********************************************}
{  TeeBI Software Library                     }
{  VCL Control Tree Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.ControlTree;

interface

// Displays a "tree" with all children controls of AControl.
// Useful for debugging purposes.

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}
  
  {$IF CompilerVersion>=28}
  {$DEFINE D21}
  {$ENDIF}
  
  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation, FMX.EditBox,
  {$ENDIF}

  FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, FMX.StdCtrls, FMX.ListBox, FMX.Colors,
  FMX.Objects,{$IFDEF D21}FMX.SpinBox, FMX.ComboEdit, FMX.ComboTrackBar,{$ENDIF} 
  FMX.Edit;


type
  TBIControlTree = class(TForm)
    TreeView1: TTreeView;
    Panel1: TLayout;
    CBVisible: TCheckBox;
    RGAlign: TListBox;
    Label1: TLabel;
    BEditChart: TButton;
    ColorPanel1: TColorPanel;
    Label2: TLabel;
    SBLeft: TSpinBox;
    Label3: TLabel;
    SBTop: TSpinBox;
    Label4: TLabel;
    SBWidth: TSpinBox;
    Label5: TLabel;
    SBHeight: TSpinBox;
    CBOpacity: TComboTrackBar;
    Label6: TLabel;
    procedure CBVisibleChange(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGAlignChange(Sender: TObject);
    procedure BEditChartClick(Sender: TObject);
    procedure ColorPanel1Change(Sender: TObject);
    procedure CBOpacityChangeTracking(Sender: TObject);
  private
    { Private declarations }

    procedure RefreshBounds(const AControl:TControl);
  public
    { Public declarations }
    procedure Refresh(const AControl:TControl);

    class procedure Edit(const AOwner:TComponent; const AControl:TControl); static;
  end;

var
  BIControlTree: TBIControlTree;

implementation

{$R *.fmx}

uses
  FMXTee.Chart, FMXTee.Editor.Chart;

{ TFormControlTree }

procedure TBIControlTree.BEditChartClick(Sender: TObject);
begin
  TChartEditForm.Edit(Self,TCustomChart(TreeView1.Selected.TagObject));
end;

procedure TBIControlTree.CBOpacityChangeTracking(Sender: TObject);
var tmp : TTreeViewItem;
begin
  tmp:=TreeView1.Selected;

  if tmp<>nil then
     TControl(tmp.TagObject).Opacity:=CBOpacity.Value*0.01;
end;

procedure TBIControlTree.CBVisibleChange(Sender: TObject);
var tmp : TTreeViewItem;
begin
  tmp:=TreeView1.Selected;

  if tmp<>nil then
     TControl(tmp.TagObject).Visible:=CBVisible.IsChecked;
end;

procedure TBIControlTree.ColorPanel1Change(Sender: TObject);
var tmp : TTreeViewItem;
begin
  tmp:=TreeView1.Selected;

  if tmp<>nil then
     TShape(tmp.TagObject).Fill.Color:=ColorPanel1.Color;
end;

class procedure TBIControlTree.Edit(const AOwner: TComponent; const AControl: TControl);
begin
  with TBIControlTree.Create(AOwner) do
  try
    Refresh(AControl);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBIControlTree.FormCreate(Sender: TObject);
begin
//  TreeView1.ReadOnly:=True;
//  TreeView1.HideSelection:=False;
end;

procedure TBIControlTree.Refresh(const AControl: TControl);

  procedure Add(const AParent:TTreeViewItem; const AControl:TControl);
  var t : Integer;
      tmp : TTreeViewItem;
      tmpS : String;
  begin
    if AControl is TTextControl then
       tmpS:=TTextControl(AControl).Text
    else
       tmpS:='';

    if tmpS='' then
    begin
      if AControl is TCustomChart then
         tmpS:=TCustomChart(AControl).Title.Caption;
    end;

    tmp:=TTreeViewItem.Create(Self);
    tmp.Text:= Copy(AControl.ClassName,2,Length(AControl.ClassName))+' '+tmpS;

    tmp.TagObject:=AControl;

    if AParent=nil then
       tmp.Parent:=TreeView1
    else
       tmp.Parent:=AParent;

    for t:=0 to AControl.ControlsCount-1 do
           Add(tmp,AControl.Controls[t]);
  end;

begin
  Add(nil,AControl);

  if TreeView1.Count>0 then
     TreeView1.Items[0].Expand;
end;

procedure TBIControlTree.RefreshBounds(const AControl:TControl);
begin
  SBWidth.Value:=AControl.Width;
  SBHeight.Value:=AControl.Height;

  SBLeft.Value:=AControl.Position.X;
  SBTop.Value:=AControl.Position.Y;
end;

procedure TBIControlTree.RGAlignChange(Sender: TObject);
var tmp : TTreeViewItem;
begin
  tmp:=TreeView1.Selected;

  if tmp<>nil then
  begin
    TControl(tmp.TagObject).Align:=TAlignLayout(RGAlign.ItemIndex);
    RefreshBounds(TControl(tmp.TagObject));
  end;
end;

procedure TBIControlTree.TreeView1Change(Sender: TObject);
var tmp : TControl;
begin
  if TreeView1.Selected=nil then
     Panel1.Enabled:=False
  else
  begin
    Panel1.Enabled:=True;

    tmp:=TControl(TreeView1.Selected.TagObject);
    RGAlign.ItemIndex:=Ord(tmp.Align);
    CBVisible.IsChecked:=tmp.Visible;

    RefreshBounds(tmp);

    CBOpacity.Value:=tmp.Opacity*100;

    //CBParentColor.Checked:=TControlAccess(tmp).ParentColor;
    //CBParentBack.Checked:=csParentBackground in TControlAccess(tmp).ControlStyle;
    //CBParentFont.Checked:=TControlAccess(tmp).ParentFont;

    //ColorListBox1.Selected:=TControlAccess(tmp).Color;

    BEditChart.Enabled:=tmp is TCustomChart;

    ColorPanel1.Visible:=tmp is TShape;

    if ColorPanel1.Visible then
       ColorPanel1.Color:=TShape(tmp).Fill.Color;
  end;
end;

end.
