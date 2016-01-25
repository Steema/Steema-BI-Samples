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
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure RGAlignClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure EWidthChange(Sender: TObject);
    procedure EHeightChange(Sender: TObject);
    procedure ColorListBox1Click(Sender: TObject);
    procedure BEditChartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Refresh(const AControl:TWinControl);

    class procedure Edit(const AOwner:TComponent; const AControl:TWinControl); static;
  end;

implementation
