{*********************************************}
{  TeeBI Software Library                     }
{  Data Diagram of Relationships              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.LinkDiagram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, Vcl.ExtCtrls,
  VCLTee.TeeProcs, BI.Data, VCLTee.TeCanvas,
  System.Types, TeeTree, Vcl.StdCtrls, System.IOUtils, BI.Persist, BI.VCL.Grid,
  Data.DB, BI.DataSource, BI.DataSet, Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids;

type
  TDataDiagram = class(TForm)
    Tree1: TTree;
    Panel1: TPanel;
    Button1: TButton;
    BLoad: TButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    DataSource1: TDataSource;
    Label1: TLabel;
    TBFont: TTrackBar;
    CBGDIPlus: TCheckBox;
    Grid: TBIGrid;
    BIDataset1: TBIDataset;
    procedure FormShow(Sender: TObject);
    procedure Tree1BeforeDraw(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure Tree1SelectShape(Sender: TTreeNodeShape);
    procedure TBFontChange(Sender: TObject);
    procedure CBGDIPlusClick(Sender: TObject);
  private
    { Private declarations }
    Data : TDataItem;

    procedure AddConnections;
    procedure AddDatas;
    function DiagramFile:String;

    function ShapeOf(const AObject:TObject):TTreeNodeShape;
  public
    { Public declarations }
    class function Show(const AOwner:TComponent; const AData:TDataItem):TModalResult;
  end;

implementation
