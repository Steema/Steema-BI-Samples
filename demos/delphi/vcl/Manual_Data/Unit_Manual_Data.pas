{*********************************************}
{  TeeBI Software Library                     }
{  TDataItem manual structures examples       }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_Manual_Data;

interface

{
  This example contains minimal code to show the different kinds of TDataItem
  structures that can be created manually by code.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.DataControl, BI.VCL.Grid,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,

  BI.Data;

type
  TFormManual = class(TForm)
    Panel1: TPanel;
    LBExample: TListBox;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    Splitter2: TSplitter;
    procedure LBExampleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    procedure CreateMasterDetail;
    procedure CreateMasterDetail_Embedded;
    procedure DestroyExampleData;
    procedure ShowMasterDetail(const AMaster,ADetail:TDataItem);
  public
    { Public declarations }
  end;

var
  FormManual: TFormManual;

implementation

{$R *.dfm}

uses
  BI.Arrays, BI.DataSet, Unit_Examples;

// This helper method displays two tables in two grids, linked as a
// master-detail relationship

procedure TFormManual.ShowMasterDetail(const AMaster,ADetail:TDataItem);
var tmp1,
    tmp2 : TBIDataSet;
begin
  // Show data in grids
  BIGrid1.Data:=AMaster;
  BIGrid2.Data:=ADetail;

  // Now link both grids so Grid 2 will display rows for the selected "master"
  // row in Grid 1

  tmp1:=(BIGrid1.DataSource.DataSet as TBIDataSet);
  tmp2:=(BIGrid2.DataSource.DataSet as TBIDataSet);

  tmp2.Master:=tmp1;

  BIGrid2.Visible:=True;
end;

procedure TFormManual.FormCreate(Sender: TObject);
begin
  BIGrid1.ShowItems:=True; // <-- useful property, to display sub-tables
end;

// Manually created data must also be manually destroyed
procedure TFormManual.DestroyExampleData;
begin
  BIGrid1.Data.Free;
  BIGrid2.Data.Free;
end;

procedure TFormManual.FormDestroy(Sender: TObject);
begin
  DestroyExampleData;
end;

// Create 2 TDataItem, one is the "master" and the other is the "detail".
// Each row in "detail" is associated to one row in "master".
procedure TFormManual.CreateMasterDetail;
var AMaster,
    ADetail : TDataItem;
begin
  // Create master and detail TDataItems
  AMaster:=CreateMaster;
  ADetail:=CreateDetail(AMaster);

  ShowMasterDetail(AMaster,ADetail);
end;

type
  TDataAccess=class(TDataItem);

// ADVANCED:
// Embedded detail is a little more advanced example.

// The good thing is its not necessary to have an "ID" field at each
// master and detail table to link them.

// The only requeriment is detail rows must be ordered on their "master" row,
// ie: first all rows for master row 1, then all rows for master row 2, etc

procedure TFormManual.CreateMasterDetail_Embedded;
var AMaster,
    ADetail : TDataItem;
begin
  // Create master and detail TDataItems
  AMaster:=CreateMaster;
  ADetail:=CreateDetail_Embedded;

  // "ADetail" is added to AMaster,
  // that is: it is "embedded" inside AMaster instead of being a separate table
  AMaster.Items.Add(ADetail);

  // Set ADetail Master to be AMaster
  ADetail.Master:=AMaster;

  // Then we set the quantity of detail rows that belong to each row in AMaster.
  // This is the "advanced" part
  TDataAccess(ADetail).IMaster.AppendIndex(5);  // master row 0
  TDataAccess(ADetail).IMaster.AppendIndex(12); // master row 1
  TDataAccess(ADetail).IMaster.AppendIndex(13); // master row 2

  ShowMasterDetail(AMaster,ADetail);
end;

function CreateExample(const AIndex:Integer):TDataItem;
begin
  case AIndex of
    1: result:=SimpleTable;
    2: result:=GroupOfTables;
    3: result:=NestedTable;
  else
    result:=SimpleColumn;
  end;
end;

procedure TFormManual.LBExampleClick(Sender: TObject);
begin
  DestroyExampleData;

  BIGrid2.Visible:=False; // <-- Grid 2 is used only for master-detail examples

  if LBExample.ItemIndex=-1 then
     BIGrid1.Data:=nil  // <-- clear grid
  else
  if LBExample.ItemIndex=4 then
     CreateMasterDetail
  else
  if LBExample.ItemIndex=5 then
     CreateMasterDetail_Embedded
  else
     BIGrid1.Data:=CreateExample(LBExample.ItemIndex);
end;

end.
