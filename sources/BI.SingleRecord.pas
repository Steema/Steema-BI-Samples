{*********************************************}
{  TeeBI Software Library                     }
{  Single Record from a TDataItem row         }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.SingleRecord;

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, BI.Persist;

type
  // Simple helper class that defines a single "needed" data item
  TSingleSourceProvider=class(TDataProvider)
  private
    FSource : TDataItem;

    procedure TryRemoveNotify;
  protected
    procedure ClearSource; virtual;
    procedure Notify(const AEvent:TBIEvent);
    procedure SetSource(const Value: TDataItem); virtual;
  public
    Destructor Destroy; override;

    property Source:TDataItem read FSource write SetSource;
  end;

  // Returns a TDataItem that is a record-view of another AData item,
  // for the specified ARow record
  TSingleRecord=class(TSingleSourceProvider)
  private
    FRow : TInteger;

    procedure SetRow(const Value:TInteger);
  protected
    procedure ClearSource; override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure SetSource(const Value: TDataItem); override;
  public
    Constructor Create(AOwner:TComponent); override;

    class function From(const AData:TDataItem; const ARow:TInteger):TDataItem; static;
  published
    property Row:TInteger read FRow write SetRow;
  end;

implementation

uses
  BI.DataSource;

{ TSingleRecord }

Constructor TSingleRecord.Create(AOwner: TComponent);
begin
  inherited;
  FRow:=-1;
end;

type
  TDataItemAccess=class(TDataItem);

procedure TSingleRecord.Load(const AData:TDataItem; const Children:Boolean);
var
  tmpStart : TLoopInteger;

  tmpItem,
  tmpValue : TDataItem;

  procedure AddItems(const ASource:TDataItem);

    procedure AddItem(const ASource:TDataItem);
    begin
      tmpItem.TextData[tmpStart]:=ASource.Name;

      if ASource.AsTable then
         AddItems(ASource)
      else
      if (ASource.Count<=FRow) or ASource.Missing[FRow] then
         tmpValue.Missing[tmpStart]:=True
      else
         tmpValue.TextData[tmpStart]:=ASource.DataToString(FRow);

      Inc(tmpStart);
    end;

  var tmp : TDataArray;
      t : Integer;
  begin
    if ASource.AsTable then
    begin
      tmp:=ASource.Items.AsArray;
      AData.Resize(AData.Count+tmp.Count);

      for t:=0 to tmp.Count-1 do
          AddItem(tmp[t]);
    end
    else
    begin
      AData.Resize(tmpStart+1);
      AddItem(ASource);
    end;
  end;

var tmp : TDataItem;
begin
  inherited;

  tmp:=Source;

  if tmp<>nil then
  begin
    AData.Clear;
    AData.AsTable:=True;

    tmpItem:=AData.Items.Add('Item',TDataKind.dkText);
    tmpValue:=AData.Items.Add('Value',TDataKind.dkText);

    if FRow>-1 then
    begin
      tmpStart:=0;

      tmp.Load;

      AddItems(tmp);
    end;

    TDataItemAccess(AData).ClearDelay;
  end;
end;

procedure TSingleRecord.SetRow(const Value: TInteger);
begin
  if FRow<>Value then
  begin
    FRow:=Value;
    Changed;
  end;
end;

class function TSingleRecord.From(const AData: TDataItem; const ARow:TInteger):TDataItem;
var tmp : TSingleRecord;
begin
  tmp:=TSingleRecord.Create(nil);
  try
    tmp.Source:=AData;
    tmp.FRow:=ARow;

    result:=TDataItem.Create(True);
    tmp.Load(result,True);
  finally
    tmp.Free;
  end;
end;

procedure TSingleRecord.ClearSource;
begin
  inherited;
  FRow:=-1;
end;

procedure TSingleRecord.SetSource(const Value: TDataItem);
begin
  inherited;

  if Source<>nil then
     if Source.Count>0 then
        Row:=0;
end;

{ TSingleSourceProvider }

Destructor TSingleSourceProvider.Destroy;
begin
  TryRemoveNotify;
  inherited;
end;

type
  TDataAccess=class(TDataItem);

procedure TSingleSourceProvider.TryRemoveNotify;
var tmp : TDataItem;
begin
  tmp:=Source;

  // Remove our Notify method from broadcast
  if tmp<>nil then
     TDataAccess(tmp).FConsumers.Remove(Notify);
end;

// Remove data
procedure TSingleSourceProvider.ClearSource;
begin
  FSource.Clear;
end;

procedure TSingleSourceProvider.Notify(const AEvent:TBIEvent);
begin
  if AEvent=TBIEvent.Destroyed then
     ClearSource
  else
  if AEvent=TBIEvent.Changed then
     if Source<>nil then
        Changed;
end;

procedure TSingleSourceProvider.SetSource(const Value: TDataItem);
begin
  if Source<>Value then
  begin
    TryRemoveNotify;

    ClearSource;

    if Value<>nil then
    begin
      // Set new data into Needs[0]
      FSource:=Value;

      // Make sure our Notify method will be called
      TDataAccess(Value).FConsumers.Add(Notify);
    end;
  end;
end;

end.
