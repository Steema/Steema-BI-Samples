{*********************************************}
{  TeeBI Software Library                     }
{  DataViewer for Firemonkey                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.DataViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,

  {$IF CompilerVersion<=27}
  {$DEFINE FMX2}
  {$ENDIF}

  FMX.Controls, FMX.Forms,

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IFNDEF FMX2}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, BI.FMX.Grid, FMX.Menus, BI.Data,
  BI.DataSet, FMX.TreeView, System.Rtti, FMX.Grid, BI.FMX.Grid.Grid, FMX.ListBox;

type
  TDataViewer = class(TForm)
    LayoutTree: TLayout;
    Layout2: TLayout;
    Splitter1: TSplitter;
    Layout3: TLayout;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    LName: TLabel;
    CBViewData: TCheckBox;
    PopupMenu2: TPopupMenu;
    MenuItem2: TMenuItem;
    Tree: TTreeView;
    SplitData: TSplitter;
    CBView: TComboBox;
    ItemsGrid: TBIGrid;
    DataGrid: TBIGrid;
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure TreeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBViewDataChange(Sender: TObject);
    procedure CBViewChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FData,
    DataMap,
    Items : TDataItem;

    function Selected:TDataItem;
    procedure ShowItems(const AData:TDataItem);
  public
    { Public declarations }

    class function Embedd(const AOwner: TComponent; const AParent: TControl; const AData: TDataItem): TDataViewer; static;

    procedure Select(const AData:TDataItem);
    class function View(const AOwner:TComponent; const AData:TDataItem):TModalResult; static;
  end;

  TDataTree=class
  public
    class procedure Fill(const AData:TDataItem; const ATree:TTreeView; const AddLeaves:Boolean=True); static;
  end;

implementation
