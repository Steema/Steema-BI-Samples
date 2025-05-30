{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for Woll2Woll FirePower     }
{  http://www.woll2woll.com                   }
{                                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid.Woll;

interface

uses
  System.Classes, Data.DB, FMXBI.Grid, BI.DataItem, FMX.Controls,
  FMX.wwDataGrid, FMX.wwLayouts, FMX.wwBaseGrid, BI.DataSet, FMX.Grid;

type
  TBIWollGrid=class(TwwDataGrid)
  private
    FLastTitle : Integer;
    IDataSet : TBIDataSet;

    FOnRowChanged : TNotifyEvent;

    {$IF CompilerVersion>25}
    //procedure ClickedHeader(Column:TColumn);
    {$ENDIF}

    procedure DataChange(Sender: TObject; Field: TField);
    //procedure SetHeaderStyle(const AStyle:TFontStyles);
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TBIWollGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBIWollGrid;

  protected
    function GetDataSource: TDataSource; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetObject:TObject; override;
  end;

implementation

uses
  FMX.Types, BI.Dataset;

{ TBIWollGridPlugin }

Constructor TBIWollGridPlugin.Create(const AOwner: TComponent);
begin
  inherited;

  IGrid:=TBIWollGrid.Create(AOwner); // <-- "nil" to avoid embedding it as a children
  IGrid.Stored:=False;

  IGrid.Align:=TUICommon.AlignClient;

  if AOwner is TControl then
     IGrid.Parent:=TControl(AOwner);
end;

procedure TBIWollGridPlugin.BindTo(const ADataSet: TDataSet);
begin
  IGrid.FLastTitle:=-1;
//  IGrid.SetHeaderStyle([]);

  if IGrid.DataLink.DataSource=nil then
     IGrid.DataLink.DataSource:=TDataSource.Create(IGrid);

  IGrid.DataLink.DataSource.DataSet:=ADataSet;

  if ADataSet is TBIDataset then
     IGrid.IDataSet:=TBIDataSet(ADataSet)
  else
     IGrid.IDataSet:=nil;

  IGrid.Repaint;
end;

procedure TBIWollGridPlugin.Duplicates(const AData: TDataItem;
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

function TBIWollGridPlugin.GetObject: TObject;
begin
  result:=IGrid;
end;

function TBIWollGridPlugin.GetDataSource: TDataSource;
begin
  result:=nil; //IGrid.DataSource;
end;

function TBIWollGridPlugin.GetTotals: Boolean;
begin
  result:=False;
end;

procedure TBIWollGridPlugin.SetDataSource(const Value: TDataSource);
begin
end;

procedure TBIWollGridPlugin.SetTotals(const Value: Boolean);
begin
end;

{ TBIWollGrid }

Constructor TBIWollGrid.Create(AOwner: TComponent);
begin
  inherited;

  //FBindingEditor:=TBindListGridEditor.Create(Self);
  FLastTitle:=-1;

  (*
  {$IF CompilerVersion>25}
  OnHeaderClick:=ClickedHeader;
  {$ENDIF}
  *)
end;

procedure TBIWollGrid.DataChange(Sender: TObject; Field: TField);
begin
  if Assigned(FOnRowChanged) then
     FOnRowChanged(Self);
end;

{
procedure TBIWollGrid.SetHeaderStyle(const AStyle: TFontStyles);
begin
end;
}

end.
