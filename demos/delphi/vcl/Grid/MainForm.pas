{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid VCL Example                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.Grid, Vcl.ExtCtrls, Vcl.ComCtrls,
  BI.VCL.Editor.Grid, BI.VCL.DataManager, Vcl.DBCtrls, BI.VCL.DataControl;

type
  TGridDemoForm = class(TForm)
    PageControl1: TPageControl;
    TabOptions: TTabSheet;
    TabData: TTabSheet;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BIGrid1: TBIGrid;
    Panel2: TPanel;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    GridEditor : TBIGridEditor;

    procedure SelectedData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  GridDemoForm: TGridDemoForm;

implementation

{$R *.dfm}

uses
  BI.Persist, BI.Data;

procedure TGridDemoForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabOptions;

  // Editor dialog to customize Grid options
  GridEditor:=TBIGridEditor.Embedd(Self,TabOptions,BIGrid1);

  // Load "Customers" table into Grid
  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Customers'];

  // Editor dialog to choose a Data
  TDataManager.EmbedChoose(Self,TabData,'BISamples',BIGrid1.Data).OnSelect:=SelectedData;

  // Set Navigator control source
  DBNavigator1.DataSource:=BIGrid1.DataSource;

  // Initialize Grid editor
  GridEditor.FillColumns;

  // Several BIGrid features that can be activated:

  BIGrid1.ShowItems:=True; // <-- Show Sub-Items at a secondary BIGrid

//  BIGrid1.Alternate.Enabled:=True;
//  BIGrid1.Filters.Enabled:=True;
//  BIGrid1.ReadOnly:=False;
//  BIGrid1.RowNumbers.Enabled:=True;
//  BIGrid1.Search.Enabled:=True;
end;

// When a new Data is selected, reset Grid:
procedure TGridDemoForm.SelectedData(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager(Sender).SelectedData;

  if tmp<>nil then
  begin
    BIGrid1.Data:=tmp;
    GridEditor.FillColumns;
  end;
end;

end.
