{*********************************************}
{  TeeBI Software Library                     }
{  Re-import data at given time intervals     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Scheduler;

interface

uses
  BI.DataItem, BI.Persist;

type
  TImportScheduler=class
  private
    FData : TDataItem;
    FStore : String;

    procedure AddAll;
    function Definition(const Index:Integer):TDataDefinition;
    function GetData: TDataItem;
    function NextRefresh(const Index:Integer):TDateTime;
    procedure TryReImport(const Index:Integer);
  public
    Enabled : Boolean;

    Destructor Destroy; override;

    procedure Process;
    procedure Refresh(const AStore:String);

    property Data:TDataItem read GetData;
  end;

implementation

uses
  System.SysUtils,
  BI.Arrays.Strings;

{ TImportScheduler }

Destructor TImportScheduler.Destroy;
begin
  FData.Free;
  inherited;
end;

function UnitsToString(const AUnits:TRefreshUnit):String;
begin
  case AUnits of
    Seconds: result:='Seconds';
    Minutes: result:='Minutes';
      Hours: result:='Hours';
       Days: result:='Days';
      Weeks: result:='Weeks';
     Months: result:='Months';
      Years: result:='Years';
  end;
end;

type
  TDataDefinitionAccess=class(TDataDefinition);

procedure TImportScheduler.AddAll;
var t,L : Integer;
    tmp : TStringArray;
    tmpDef : TDataDefinition;
begin
  tmp:=TStore.AllData(FStore);

  L:=Length(tmp);
  FData.Resize(L);

  for t:=0 to L-1 do
  begin
    FData[1].TextData[t]:=tmp[t];

    tmpDef:=Definition(t);

    if tmpDef<>nil then
    try
      FData[0].BooleanData[t]:=TDataDefinitionAccess(tmpDef).Refresh.Enabled;
      FData[2].Int32Data[t]:=TDataDefinitionAccess(tmpDef).Refresh.Period;
      FData[3].TextData[t]:=UnitsToString(TDataDefinitionAccess(tmpDef).Refresh.Units);
    finally
      tmpDef.Free;
    end
    else
    begin
      FData[0].BooleanData[t]:=False;
      FData[2].Int32Data[t]:=10;
      FData[3].TextData[t]:=UnitsToString(TRefreshUnit.Hours);
    end;
  end;
end;

function TImportScheduler.GetData: TDataItem;
begin
  if FData=nil then
  begin
    FData:=TDataItem.Create(True);
    FData.Items.Add('Enabled',TDataKind.dkBoolean);
    FData.Items.Add('Data',TDataKind.dkText);
    FData.Items.Add('Every',TDataKind.dkInt32);
    FData.Items.Add('TimeUnit',TDataKind.dkText);

    AddAll;
  end;

  result:=FData;
end;

function TImportScheduler.Definition(const Index:Integer):TDataDefinition;
begin
  result:=TStore.GetDefinition(FStore,FData[1].TextData[Index]);
end;

function TImportScheduler.NextRefresh(const Index:Integer):TDateTime;
var tmp : TDataDefinition;
begin
  tmp:=Definition(Index);
  try
    result:=tmp.NextRefresh;
  finally
    tmp.Free;
  end;
end;

procedure TImportScheduler.TryReImport(const Index:Integer);
begin
  // Import to temp *.bi file, delete old,
  // try to rename current to old, then rename new to current
end;

procedure TImportScheduler.Process;
var t : Integer;
begin
  if Enabled and (FData<>nil) then
     for t:=0 to FData.Count-1 do
         if FData[0].BooleanData[t] then
            if NextRefresh(t)>Now then
               TryReImport(t);
end;

procedure TImportScheduler.Refresh(const AStore:String);
begin
  FStore:=AStore;

  if FData<>nil then
  begin
    FData.ClearData(True);
    AddAll;
  end;
end;

end.
