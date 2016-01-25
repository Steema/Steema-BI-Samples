{*********************************************}
{  TeeBI Software Library                     }
{  TBIDataSet editor dialog                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.DataSet;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.DataSet, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.VCL.Editor.Summary, Data.DB, Vcl.Grids, Vcl.DBGrids, BI.VCL.Grid,
  Vcl.ExtCtrls, BI.VCL.DataManager, BI.VCL.GridForm,
  BI.VCL.Editor.DataSelect, BI.Data;

type
  TBIDataSetEditor = class(TForm)
    Panel1: TPanel;
    RGMode: TRadioGroup;
    Panel2: TPanel;
    CBPreview: TCheckBox;
    PanelMain: TPanel;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    Backup: TBIDataset;
    procedure CBPreviewClick(Sender: TObject);
    procedure RGModeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }

    Manager : TDataManager;
    DataSet : TBIDataSet;

    SelectEditor : TDataSelectEditor;
    SummaryEditor : TSummaryEditor;

    IPreview : TBIGridForm;

    procedure ClosedPreview(Sender: TObject; var Action: TCloseAction);
    procedure CreateManager;
    function NewData:TDataItem;
    procedure PreviewNewData;
    procedure Recalculate(Sender:TObject);
    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const ADataSet:TBIDataSet):Boolean; static;
    procedure Refresh(const ADataSet:TBIDataSet);
  end;

implementation
