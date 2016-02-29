{*********************************************}
{  TeeBI Software Library                     }
{  DataViewer VCL                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.DataViewer;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids, BI.Data, Vcl.StdCtrls, BI.Arrays,
  BI.VCL.Grid, Vcl.Menus, BI.DataSet, Vcl.ComCtrls, Vcl.DBCtrls;

type
  TDataViewer = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    LName: TLabel;
    Splitter1: TSplitter;
    DataSource2: TDataSource;
    PanelItems: TPanel;
    SplitterData: TSplitter;
    CBViewData: TCheckBox;
    PanelDatas: TPanel;
    DataTotals: TStringGrid;
    PopupMenu1: TPopupMenu;
    View1: TMenuItem;
    BDiagram: TButton;
    ItemsGrid: TBIGrid;
    Button2: TButton;
    PanelData: TPanel;
    PanelDataGrid: TPanel;
    DataGrid: TBIGrid;
    PanelNav: TPanel;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    LRow: TLabel;
    Items: TBIDataset;
    CBView: TComboBox;
    N1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBViewDataClick(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BDiagramClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure CBViewChange(Sender: TObject);
  private
    { Private declarations }
    FData : TDataItem;

    DataMap : TDataItem;

    Tree : TTreeView;

    procedure CheckPanelDataAlign;
    procedure FillData(const AData:TDataItem);
    procedure GridDataChange(Sender: TObject);
    procedure NewItems(const AData:TDataItem);

    function Selected:TDataItem;
    procedure SelectedChange(Sender: TObject);
  protected
    ICustomPosition : Boolean;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl; const AData:TDataItem):TDataViewer; static;
    procedure Select(const AData:TDataItem);
    class procedure View(const AOwner:TComponent; const AData:TDataItem); static;
  end;

implementation
