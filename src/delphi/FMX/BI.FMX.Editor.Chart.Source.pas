{*********************************************}
{  TeeBI Software Library                     }
{  TTeeBISource FMX Editor Dialog             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Editor.Chart.Source;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  FMX.StdCtrls, FMX.ListBox, FMX.Edit, FMX.Layouts,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMXTee.Constants, FMXTee.Engine, FMXTee.Editor.Source, BI.FMX.Chart.Source;

type
  TValueControl=record
  public
    Text : TLabel;
    Combo : TComboBox;
  end;

  TBISourceEditor = class(TBaseSourceEditor)
    LayoutData: TLayout;
    GroupFields: TLayout;
    LLabels: TLabel;
    CBLabelsField: TComboBox;
    BSelectData: TButton;
    LData: TLabel;
    procedure BApplyClick(Sender: TObject);
    procedure CBLabelsFieldChange(Sender: TObject);
    procedure BSelectDataClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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
