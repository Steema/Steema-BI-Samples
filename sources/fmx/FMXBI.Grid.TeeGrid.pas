{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for TeeGrid                 }
{                                             }
{  https://www.steema.com/product/gridvcl     }
{                                             }
{  Copyright (c) 2016-2018 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid.TeeGrid;

interface

(*
  This unit connects TBIGrid with TTeeGrid.

  To activate, simply add this unit to your "uses",
  or manually set the desired plugin class:

  TBIGrid.Engine:=TBITeeGridPlugin;

  TeeGrid provides automatic support for many TeeBI features like column
  totals, custom cell coloring, column sorting etc.

*)

uses
  System.Types, System.UITypes, System.Classes, System.SysUtils,
  FMX.Controls, FMX.Menus, Data.DB,

  BI.Grid.Plugin, BI.DataItem, BI.UI,

  BI.GridData,
  FMXTee.Grid,
  Tee.Grid.Totals;

type
  TBITeeGrid=class(TTeeGrid)
  private
    IData : TBIGridData;
    IDataSource : TDataSource;

    FHeader : TTotalsHeader;
    FTotals: TColumnTotals;

    procedure BindTo(const ADataSet:TDataSet);
    function GetTotals:Boolean;
    procedure SetTotals(const Value:Boolean);
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TBITeeGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBITeeGrid;

  protected
    procedure AutoWidth; override;
    procedure ChangedAlternate(Sender:TObject); override;
    function GetCanSort: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetEditorClass:String; override;
    function GetReadOnly:Boolean; override;
    function GetSortEnabled: Boolean; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetFilters(const Value:Boolean); override;
    procedure SetOnRowChanged(const AEvent:TNotifyEvent); override;
    procedure SetPopup(const Value:TObject); override;
    procedure SetReadOnly(const Value:Boolean); override;
    procedure SetRowNumber(const Value:Boolean); override;
    procedure SetSearch(const Value:Boolean); override;
    procedure SetSortEnabled(const Value: Boolean); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Colorize(const AItems:TDataColorizers); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetControl:TObject; override;
    function GetObject:TObject; override;
  end;

implementation

uses
  FMXBI.Grid, // <-- needed only for TUICommon.AlignClient
  BI.DataSet,
  BI.DB.DataSet,
  BI.DataSource;

{ TBITeeGridPlugin }

Constructor TBITeeGridPlugin.Create(const AOwner: TComponent);
begin
  inherited;

  IGrid:=TBITeeGrid.Create(AOwner);
  IGrid.Align:=TUICommon.AlignClient;

  if AOwner is TControl then
     IGrid.Parent:=TControl(AOwner);
end;

procedure TBITeeGridPlugin.ChangedAlternate(Sender: TObject);
begin
  IAlternate:=TAlternateColor(Sender);

  IGrid.Rows.Alternate.Visible:=IAlternate.Enabled;
  IGrid.Rows.Alternate.Brush.Color:=IAlternate.Color;
end;

procedure TBITeeGridPlugin.AutoWidth;
begin
end;

procedure TBITeeGridPlugin.Colorize(const AItems: TDataColorizers);
begin
end;

procedure TBITeeGridPlugin.BindTo(const ADataSet:TDataSet);
begin
  IGrid.BindTo(ADataSet);
end;

procedure TBITeeGridPlugin.SetDataSource(const Value: TDataSource);
begin
  IGrid.IDataSource:=Value;

  if Value=nil then
     IGrid.IData.Data:=nil
  else
     BindTo(Value.DataSet);
end;

procedure TBITeeGridPlugin.SetFilters(const Value: Boolean);
begin
end;

procedure TBITeeGridPlugin.SetOnRowChanged(const AEvent: TNotifyEvent);
begin
  // IGrid.OnRowChanged:=AEvent
end;

procedure TBITeeGridPlugin.SetPopup(const Value: TObject);
begin
  IGrid.PopupMenu:=Value as TPopupMenu;
end;

procedure TBITeeGridPlugin.SetReadOnly(const Value: Boolean);
begin
  IGrid.ReadOnly:=Value;
end;

procedure TBITeeGridPlugin.SetRowNumber(const Value: Boolean);
begin
end;

procedure TBITeeGridPlugin.SetSearch(const Value: Boolean);
begin
end;

procedure TBITeeGridPlugin.SetSortEnabled(const Value: Boolean);
begin
  IGrid.Header.Sortable:=Value;
end;

procedure TBITeeGridPlugin.SetTotals(const Value:Boolean);
begin
  IGrid.SetTotals(Value);
end;

procedure TBITeeGridPlugin.Duplicates(const AData:TDataItem; const Hide:Boolean);
var tmpSource : TComponent;
    tmpDataSet : TDataSet;
    tmp : TField;
begin
  tmpSource:=IGrid.DataSource;

  if tmpSource is TDataSource then
     tmpDataSet:=TDataSource(tmpSource).DataSet
  else
  if tmpSource is TDataSet then
     tmpDataSet:=TDataSet(tmpSource)
  else
     tmpDataSet:=nil;

  if tmpDataSet<>nil then
  begin
    tmp:=TBIDataSetSource.FieldOf(AData,tmpDataSet);

    if tmp<>nil then
    begin
      if tmpDataSet is TBIDataset then
         (tmpDataSet as TBIDataSet).SetFieldOnGetText(tmp,Hide);

      IGrid.Repaint;
    end;
  end;
end;

function TBITeeGridPlugin.GetObject: TObject;
begin
  result:=IGrid;
end;

function TBITeeGridPlugin.GetCanSort: Boolean;
begin
  result:=True;
end;

function TBITeeGridPlugin.GetControl: TObject;
begin
  result:=IGrid;
end;

function TBITeeGridPlugin.GetDataSource: TDataSource;
begin
  result:=IGrid.IDataSource;
end;

function TBITeeGridPlugin.GetEditorClass: String;
begin
  result:='';
end;

function TBITeeGridPlugin.GetReadOnly: Boolean;
begin
  result:=IGrid.ReadOnly;
end;

function TBITeeGridPlugin.GetSortEnabled: Boolean;
begin
  result:=IGrid.Header.Sortable;
end;

function TBITeeGridPlugin.GetTotals: Boolean;
begin
  result:=IGrid.GetTotals;
end;

{ TBISMDBGrid }

Constructor TBITeeGrid.Create(AOwner: TComponent);
begin
  inherited;

  IData:=TBIGridData.Create;
  Data:=IData;

  ReadOnly:=True;
end;

function TBITeeGrid.GetTotals: Boolean;
begin
  result:=(FTotals<>nil) and FTotals.Visible;
end;

procedure TBITeeGrid.SetTotals(const Value: Boolean);

  procedure ShowHide(const Value:Boolean);
  begin
    FTotals.Visible:=Value;
    FHeader.Visible:=Value;
  end;

begin
  if Value then
  begin
    if FTotals=nil then
    begin
      FTotals:=TColumnTotals.Create(Footer);

      FHeader:=TTotalsHeader.Create(Footer);
      FHeader.Totals:=FTotals;
    end
    else
      ShowHide(True);
  end
  else
  if FTotals<>nil then
     ShowHide(False);
end;

procedure TBITeeGrid.BindTo(const ADataSet:TDataSet);
begin
  if ADataSet is TBIDataset then
  begin
    IData.Data:=TBIDataset(ADataSet).Data;

    if IDataSource=nil then
    begin
      IDataSource:=TDataSource.Create(Self);
      IDataSource.DataSet:=ADataSet;
    end;
  end
  else
  begin
    IData.Data:=nil;

    if IDataSource<>nil then
       IDataSource.DataSet:=nil;
  end;

  Grid.Columns.Clear;
  Grid.RefreshData;
end;

initialization
  TBIGrid.Engine:=TBITeeGridPlugin;
finalization
  TBIGrid.Engine:=nil;
end.
