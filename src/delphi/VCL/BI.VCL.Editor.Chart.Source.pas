{*********************************************}
{  TeeBI Software Library                     }
{  TTeeBISource VCL Editor Dialog             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Chart.Source;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.TeeSourceEdit, Vcl.StdCtrls,
  VCLTee.TeCanvas, Vcl.ExtCtrls, BI.VCL.Chart.Source;

type
  TValueControl=record
  public
    Text : TLabel;
    Combo : TComboBox;
  end;

  TBISourceEditor = class(TBaseSourceEditor)
    GroupFields: TScrollBox;
    LLabels: TLabel;
    CBLabelsField: TComboFlat;
    PanelData: TPanel;
    BSelectData: TButton;
    LData: TLabel;
    procedure BSelectDataClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BApplyClick(Sender: TObject);
    procedure CBLabelsFieldChange(Sender: TObject);
  private
    { Private declarations }

    FValueControls : Array of TValueControl;

    procedure ChangedCombo(Sender: TObject);
    procedure CreateControls;
    procedure DestroyCombos;
    procedure FillCombos;
    procedure RefreshControls;
    procedure RefreshDataLabel;
    function Source:TTeeBISource;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const ASource:TTeeBISource); static;
  end;

implementation
