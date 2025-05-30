{*********************************************}
{  TeeBI Software Library                     }
{  Summary Consolidations (Grand Totals)      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Summary.Totals;

interface

uses
  System.Classes, BI.DataItem, BI.Summary;

type
  // Creates one TDataItem for each active Group in ASummary with the subtotals
  // of all measures for that group, and finally the Grand Totals for all measures.

  // Usage example:
  // BIGrid1.Data := TDataItem.Create(TSummaryTotals.Create(MySummary));

  TSummaryTotals=class(TDataProvider)
  private
    FSummary : TSummary;

    procedure CheckSummary;
    procedure DestroySummaries;
    procedure SetSummary(const Value: TSummary);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor CreateSummary(const AOwner:TComponent; const ASummary:TSummary);
    Destructor Destroy; override;

    procedure Calculate(const AData:TDataItem);
  published
    property Summary:TSummary read FSummary write SetSummary;
  end;

implementation

uses
  System.SysUtils, BI.Arrays;

// Returns a TDataItem containing one item for each Group in ASummary except the last one.
// Each resulting TDataItem contains the sub-sub-totals of ASummary measures.
// The first TDataItem contains the "Grand Totals".

{ TSummaryTotals }

Constructor TSummaryTotals.CreateSummary(const AOwner:TComponent; const ASummary: TSummary);
begin
  Create(AOwner);
  FSummary:=ASummary;
end;

Destructor TSummaryTotals.Destroy;
begin
  DestroySummaries;
  inherited;
end;

procedure TSummaryTotals.DestroySummaries;
var t : Integer;
begin
  t:=0;

  while t<ComponentCount do
        if Components[t] is TSummary then
           {$IFDEF AUTOREFCOUNT}
           Components[t].DisposeOf
           {$ELSE}
           Components[t].Free
           {$ENDIF}
        else
           Inc(t);
end;

procedure TSummaryTotals.CheckSummary;
begin
  if FSummary=nil then
     raise EBIException.Create('Error: Summary property is not assigned');
end;

type
  TDataItemAccess=class(TDataItem);
  TGroupByAccess=class(TGroupBy);

procedure TSummaryTotals.Calculate(const AData:TDataItem);

  // Returns the count of Summary GroupBy that are Active
  function ActiveGroups(const ASum:TSummary):Integer;
  var tmp : TGroupBy;
  begin
    result:=0;

    for tmp in ASum.By do
        if tmp.Active then
           Inc(result);
  end;

  // Returns the count of Summary Measures that are Active
  function ActiveMeasures(const ASum:TSummary):Integer;
  var tmp : TMeasure;
  begin
    result:=0;

    for tmp in ASum.Measures do
        if tmp.Active then
           Inc(result);
  end;

  // Creates a new Summary that will be used to Totalize the first GroupBy
  function Totalize(const ASum:TSummary):TSummary;

    procedure AddMeasures(const ADest:TSummary);
    var tmp,
        tmpNew : TMeasure;
        tmpAgg : TAggregate;
    begin
      // Add all active measures
      for tmp in ASum.Measures do
          if tmp.Active then
          begin
            tmpAgg:=tmp.Aggregate;

            // For "Count" measures, substitute it with Sum:
            if tmpAgg=TAggregate.Count then
               tmpAgg:=TAggregate.Sum;

            tmpNew:=result.AddMeasure(tmp.DestData,tmpAgg);
            tmpNew.Name:=tmpNew.Expression.ToString;
          end;
    end;

    procedure AddGroups(const ADest:TSummary; const ACount:Integer);
    var t : Integer;
        tmpBy,
        tmpGroup : TGroupBy;
    begin
      for t:=0 to ASum.By.Count-1 do
      begin
        tmpGroup:=ASum.By[t];

        if tmpGroup.Active and (TGroupByAccess(tmpGroup).RealLayout=TGroupByLayout.Rows) then
        begin
          tmpBy:=result.AddGroupBy(tmpGroup.DestData);

          // DO NOT set original tmpGroup options,
          // as they are "lost" when original summary is calculated, and also the
          // original "Kind" DateTime is converted to Int32 (so its lost)

          //tmpBy.DatePart:=tmpGroup.DatePart;
          //tmpBy.Histogram:=tmpGroup.Histogram;

          // Force new By layout to "rows" (pending: test with original tmpGroup.Layout)
          tmpBy.Layout:=TGroupByLayout.Rows;

          if ADest.By.Count>=ACount-1 then
             break;
        end;
      end;
    end;

  var tmpActive : Integer;
  begin
    result:=TSummary.Create(Self);

    // Add all active measures:
    AddMeasures(result);

    // Add all active groups except the last active one:
    tmpActive:=ActiveGroups(ASum);

    if tmpActive>1 then
       AddGroups(result,tmpActive);
  end;

var tmp : TSummary;
    tmpResult : TDataItem;
begin
  CheckSummary;

  AData.Name:='Totals';

  if ActiveMeasures(FSummary)>0 then
  begin
    tmp:=FSummary;

    while ActiveGroups(tmp)>0 do
    begin
      tmp:=Totalize(tmp);

      tmpResult:=tmp.NewData;

      // Set result Name to last Group
      if tmp.By.Count>0 then
         tmpResult.Name:=tmp.By[tmp.By.Count-1].ToString
      else
         tmpResult.Name:='Total';

      tmpResult.Load;

      AData.Items.Add(tmpResult);
    end;
  end;
end;

procedure TSummaryTotals.Load(const AData:TDataItem; const Children:Boolean);
begin
  DestroySummaries;
  AData.Clear;
  Calculate(AData);
  TDataItemAccess(AData).ClearDelay;
end;

procedure TSummaryTotals.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent=FSummary) and (Operation=TOperation.opRemove) then
     Summary:=nil;
end;

procedure TSummaryTotals.SetSummary(const Value: TSummary);
begin
  if FSummary<>Value then
  begin
    if FSummary<>nil then
       FSummary.RemoveFreeNotification(Self);

    FSummary:=Value;

    if FSummary<>nil then
       FSummary.FreeNotification(Self);
  end;
end;

end.
