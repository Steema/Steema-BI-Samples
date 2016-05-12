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
  Data.DB, Vcl.Grids, Vcl.DBGrids, BI.VCL.Grid,
  Vcl.ExtCtrls, BI.VCL.GridForm,
  BI.VCL.DataSelect, BI.Data, BI.Query, BI.Persist, BI.VCL.DataControl,
  BI.VCL.Tree;

type
  TBIDataSetEditor = class(TForm)
    Panel2: TPanel;
    CBPreview: TCheckBox;
    Panel4: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PageControl1: TPageControl;
    TabOptions: TTabSheet;
    TabData: TTabSheet;
    Backup: TBIDataset;
    Panel3: TPanel;
    CBActive: TCheckBox;
    CBReadonly: TCheckBox;
    Label1: TLabel;
    BITree1: TBITree;
    Button1: TButton;
    procedure CBPreviewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControl1Change(Sender: TObject);
    procedure CBActiveClick(Sender: TObject);
    procedure CBReadonlyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    DataSet : TBIDataSet;

    ISelect : TDataSelector;
    IPreview : TBIGridForm;

    IChanging,
    IModified : Boolean;

    procedure ChangeData(const AData:TDataItem);
    procedure ClosedPreview(Sender: TObject; var Action: TCloseAction);
    procedure PreviewNewData;
    procedure SelectedData(Sender: TObject);
    procedure SetModified;
    procedure SetStructure;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const ADataSet:TBIDataSet):Boolean; static;

    procedure Refresh(const ADataSet:TBIDataSet);
  end;

implementation
