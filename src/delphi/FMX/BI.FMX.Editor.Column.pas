{*********************************************}
{  TeeBI Software Library                     }
{  Data Column Editor                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Editor.Column;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, BI.Data, FMX.ListBox;

type
  TColumnEditor = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CBKind: TComboBox;
    Button1: TButton;
    Label4: TLabel;
    CBMaster: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
    procedure CBMasterChange(Sender: TObject);
  private
    { Private declarations }
    Col : TDataItem;
  public
    { Public declarations }
    class procedure Edit(const AOwner:TComponent; const ACol:TDataItem); static;
  end;

implementation
