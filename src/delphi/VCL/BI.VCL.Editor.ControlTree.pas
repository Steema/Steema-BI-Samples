{*********************************************}
{  TeeBI Software Library                     }
{  VCL Control Tree Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.ControlTree;

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
