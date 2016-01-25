{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid editor dialog                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Grid;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, BI.VCL.Grid,
  Vcl.Grids, Vcl.CheckLst, Vcl.DBGrids, Data.DB, BI.VCL.Grid.DBGrid;

type
  {$IFDEF FPC}
  HWND=Cardinal;
  {$ENDIF}

  TBIGridEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CBOptions: TCheckListBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    EColWidth: TEdit;
    ERowHeight: TEdit;
    UDColWidth: TUpDown;
    UDRowHeight: TUpDown;
    Label5: TLabel;
    ELineWidth: TEdit;
    UDLineWidth: TUpDown;
    TabSheet3: TTabSheet;
    CBStyle: TComboBox;
    GroupBox1: TGroupBox;
    CBAltRows: TCheckBox;
    BAltColor: TButton;
    CBBorder: TCheckBox;
    BBackColor: TButton;
    BTitleFont: TButton;
    FontDialog1: TFontDialog;
    BCellsFont: TButton;
    FontDialog2: TFontDialog;
    CBReadOnly: TCheckBox;
    Tree: TTreeView;
    GBColumn: TGroupBox;
    BColColor: TButton;
    CBColVisible: TCheckBox;
    Button1: TButton;
    FontDialog3: TFontDialog;
    CBColAlign: TComboBox;
    CBColExpanded: TCheckBox;
    Label1: TLabel;
    procedure CBStyleChange(Sender: TObject);
    procedure CBAltRowsClick(Sender: TObject);
    procedure BAltColorClick(Sender: TObject);
    procedure CBOptionsClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBBorderClick(Sender: TObject);
    procedure BBackColorClick(Sender: TObject);
    procedure EColWidthChange(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure ELineWidthChange(Sender: TObject);
    procedure BTitleFontClick(Sender: TObject);
    procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
    procedure BCellsFontClick(Sender: TObject);
    procedure FontDialog2Apply(Sender: TObject; Wnd: HWND);
    procedure CBReadOnlyClick(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure BColColorClick(Sender: TObject);
    procedure CBColVisibleClick(Sender: TObject);
    procedure FontDialog3Apply(Sender: TObject; Wnd: HWND);
    procedure Button1Click(Sender: TObject);
    procedure CBColAlignChange(Sender: TObject);
    procedure CBColExpandedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Grid : TBIDBGrid;

    ISetting : Boolean;

    function Column:TColumn;

    {$IFDEF FPC}
    procedure SetColumnAlignment(const Col:TColumn);
    procedure SetColumnColor(const Col:TColumn);
    procedure SetColumnFont(const Col:TColumn);
    {$ENDIF}
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const AGrid:TBIDBGrid); static;
    procedure FillColumns;
    procedure Refresh(const AGrid:TBIDBGrid);
  end;

implementation
