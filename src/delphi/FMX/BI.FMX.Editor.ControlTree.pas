{*********************************************}
{  TeeBI Software Library                     }
{  VCL Control Tree Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Editor.ControlTree;

interface

// Displays a "tree" with all children controls of AControl.
// Useful for debugging purposes.

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.Colors,
  FMX.Objects, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.ComboEdit,
  FMX.ComboTrackBar;

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
