{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for TMS Firemonkey Grid     }
{  http://www.tmssoftware.com                 }
{                                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid.TMS;

interface

uses
  System.Classes, Data.DB, FMXBI.Grid, BI.DataItem, FMX.Controls,
  FMX.TMSLiveGrid, BI.DataSet, FMX.Grid,
  FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.Bind.Grid, FMX.Bind.Grid,
  FMX.TMSLiveGridDataBinding, BI.UI;

type
  TBITMSGrid=class(TTMSFMXLiveGrid)
  private
    FBindingEditor: TTMSFMXBindLiveGridEditor;
    FLastTitle : Integer;
    IDataSet : TBIDataSet;

    BindingList : TBindingsList;
    LinkGrid: TLinkGridToDataSource;

    FOnRowChanged : TNotifyEvent;

    {$IF CompilerVersion>25}
    //procedure ClickedHeader(Column:TColumn);
    {$ENDIF}

    procedure DataChange(Sender: TObject; Field: TField);
    //procedure SetHeaderStyle(const AStyle:TFontStyles);
  protected
    BindSource : TBindSourceDB;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
  end;

  TBITMSGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBITMSGrid;

    IItems : TDataColorizers;

    {
    OldGetCellColor : TTMSGridGetCellColor;

    procedure GetCellColor(Sender: TObject; ARow,ACol: Integer;
                AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
    }
  protected
    function GetDataSource: TDataSource; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Colorize(const AItems:TDataColorizers); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetObject:TObject; override;
  end;

implementation

uses
  FMX.Types, BI.Dataset;

{ TBITMSGridPlugin }

Constructor TBITMSGridPlugin.Create(const AOwner: TComponent);
begin
  inherited;

  IGrid:=TBITMSGrid.Create(AOwner); // <-- "nil" to avoid embedding it as a children
  IGrid.Stored:=False;

  IGrid.Align:=TUICommon.AlignClient;

  if AOwner is TControl then
     IGrid.Parent:=TControl(AOwner);
end;

procedure TBITMSGridPlugin.BindTo(const ADataSet: TDataSet);
begin
  IGrid.FLastTitle:=-1;
//  IGrid.SetHeaderStyle([]);

  IGrid.BindSource.DataSet:=ADataSet;

  if ADataSet is TBIDataset then
     IGrid.IDataSet:=TBIDataSet(ADataSet)
  else
     IGrid.IDataSet:=nil;

  IGrid.Repaint;
end;

procedure TBITMSGridPlugin.Colorize(const AItems: TDataColorizers);
begin
  IItems:=AItems;

  {
  if IItems=nil then
     IGrid.OnGetCellColor:=OldGetCellColor
  else
  begin
    OldGetCellColor:=IGrid.OnGetCellColor;
    IGrid.OnGetCellColor:=GetCellColor;
  end;
  }
end;

procedure TBITMSGridPlugin.Duplicates(const AData: TDataItem;
  const Hide: Boolean);
var tmpD : TDataSet;
    tmp : TField;
begin
  tmpD:=IGrid.IDataSet;

  if tmpD<>nil then
  begin
    tmp:=TBIDataSetSource.FieldOf(AData,tmpD);

    if tmp<>nil then
    begin
      (tmpD as TBIDataSet).SetFieldOnGetText(tmp,Hide);
      IGrid.Repaint;
    end;
  end;
end;

function TBITMSGridPlugin.GetObject: TObject;
begin
  result:=IGrid;
end;

function TBITMSGridPlugin.GetDataSource: TDataSource;
begin
  result:=nil; //IGrid.DataSource;
end;

function TBITMSGridPlugin.GetTotals: Boolean;
begin
  result:=False;
end;

procedure TBITMSGridPlugin.SetDataSource(const Value: TDataSource);
begin
end;

procedure TBITMSGridPlugin.SetTotals(const Value: Boolean);
begin
end;

{ TBITMSGrid }

Constructor TBITMSGrid.Create(AOwner: TComponent);
begin
  inherited;

  FBindingEditor:=TTMSFMXBindLiveGridEditor.Create(Self);
  FLastTitle:=-1;

  BindSource:=TBindSourceDB.Create(Self);
  BindingList:=TBindingsList.Create(Self);

  {$WARNINGS OFF}
  LinkGrid:=TLinkGridToDataSource.Create(BindingList);
  {$WARNINGS ON}

  LinkGrid.DataSource:=BindSource;
  LinkGrid.GridControl:=Self;

  (*
  {$IF CompilerVersion>25}
  OnHeaderClick:=ClickedHeader;
  {$ENDIF}
  *)

  BindSource.DataSource.OnDataChange:=DataChange;
end;

Destructor TBITMSGrid.Destroy;
begin
  LinkGrid.Free;
  BindingList.Free;
  BindSource.Free;

  inherited;
end;

procedure TBITMSGrid.DataChange(Sender: TObject; Field: TField);
begin
  if Assigned(FOnRowChanged) then
     FOnRowChanged(Self);
end;

{
procedure TBITMSGrid.SetHeaderStyle(const AStyle: TFontStyles);
begin
end;
}

end.
