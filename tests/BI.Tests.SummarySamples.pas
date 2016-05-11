{*********************************************}
{  TeeBI Software Library                     }
{  Summary queries examples                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.SummarySamples;

interface

uses
  System.Classes, BI.Data, BI.Summary;

type
  TSampleSummaries=class
  public
    Demo,

    City,
    Customers,
    Orders,
    OrderDetails,
    Suppliers,
    Categories,
    Shippers,
    Products,
    Movies : TDataItem;

    // Temporary
    SumData1,
    SumData2,
    SumData3 : TDataItem;

    Destructor Destroy; override;

    function Count:Integer;
    function CreateSummary(const AOwner:TComponent; const AIndex:Integer):TSummary;
    procedure LoadData(const AStore:String='');
  end;

var
  Samples : TSampleSummaries;

implementation

uses
  BI.Persist, BI.Expression;

function TSampleSummaries.Count:Integer;
begin
  result:=26;
end;

function TSampleSummaries.CreateSummary(const AOwner:TComponent; const AIndex:Integer):TSummary;
begin
  result:=TSummary.Create(AOwner);

  case AIndex of
    0: result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

    1: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);
         result.AddMeasure(OrderDetails['Discount'],TAggregate.Average);
       end;

    2: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);
         result.AddMeasure(OrderDetails['Discount'],TAggregate.Sum);
         result.AddMeasure(OrderDetails['UnitPrice'],TAggregate.Average);
       end;

    3: begin
         result.AddMeasure(Customers,TAggregate.Count);
         result.AddGroupBy(Customers['Country']);
       end;

    4: begin
         result.AddMeasure(Customers,TAggregate.Count);
         result.AddGroupBy(Customers['Country']);

         result.By[0].Layout:=TGroupByLayout.Items;
       end;

    5: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Products['CategoryID']);
         result.AddGroupBy(Customers['Country']);
       end;

    6: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Customers['Country']);
         result.AddGroupBy(Products['CategoryID']);

         result.By[1].Layout:=TGroupByLayout.Rows;
       end;

    7: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Customers['Country']);
         result.AddGroupBy(Suppliers['CompanyName']);
         result.AddGroupBy(Categories['CategoryName']);
       end;

    8: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Customers['Country']);
         result.AddGroupBy(Suppliers['CompanyName']);
         result.AddGroupBy(Categories['CategoryName']);

         result.By[1].Layout:=TGroupByLayout.Rows;
       end;

    9: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);
         result.Measures[0].Missing.AsZero:=True;

         result.AddGroupBy(Customers['Country']);
         result.AddGroupBy(Suppliers['CompanyName']);
         result.AddGroupBy(Categories['CategoryName']);

         result.By[0].Layout:=TGroupByLayout.Items;
         result.By[2].Layout:=TGroupByLayout.Items;
       end;

   10: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);
         result.AddMeasure(OrderDetails['Discount'],TAggregate.Average);

         result.AddGroupBy(Customers['Country']);
       end;

   11: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;
       end;

   12: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Orders['ShipVia']);
       end;

   13: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Orders['OrderDate']);
         result.By[1].DateOptions.Part:=TDateTimePart.Quarter;

         result.AddGroupBy(Orders['ShipVia']);
       end;

   14: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Orders['OrderDate']);
         result.By[1].DateOptions.Part:=TDateTimePart.Quarter;

         result.AddGroupBy(Orders['OrderDate']);
         result.By[2].DateOptions.Part:=TDateTimePart.ShortMonthName;

         result.AddGroupBy(Orders['ShipVia']);
       end;

   15: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Orders['OrderDate']);
         result.By[1].DateOptions.Part:=TDateTimePart.Quarter;

         result.AddGroupBy(Orders['OrderDate']);
         result.By[2].DateOptions.Part:=TDateTimePart.ShortMonthName;

         result.AddGroupBy(Orders['ShipVia']);
         result.AddGroupBy(Categories['CategoryName']);
       end;

   16: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Products['Discontinued']); // <-- boolean
       end;

   17: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Products['UnitPrice']);
         result.By[1].Histogram.Active:=True;
       end;

   18: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Orders['OrderDate']);
         result.By[1].DateOptions.Part:=TDateTimePart.Quarter;

         result.AddGroupBy(Orders['ShipVia']);
         result.AddGroupBy(Shippers['ShipperID']);
         result.AddGroupBy(Shippers['CompanyName']);
       end;

   19: result.AddMeasure(TDataExpression.FromString(OrderDetails,'Quantity * Products.UnitPrice'),TAggregate.Sum);

   20: begin
         result.AddMeasure(TDataExpression.FromString(OrderDetails,'Quantity * Products.UnitPrice'),TAggregate.Sum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;
       end;

   21: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(TDataExpression.FromString(Orders,'ShipVia*2'));
       end;

   22: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);

         result.AddGroupBy(TDataExpression.FromString(Customers,'Length(CompanyName)'));
       end;

   23: begin
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Minimum);
         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Maximum);

         result.AddGroupBy(Orders['OrderDate']);
         result.By[0].DateOptions.Part:=TDateTimePart.Year;

         result.AddGroupBy(Orders['ShipVia']);
       end;

   24: begin
         // Histogram by Text string data (A..Z)

         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);
         result.AddGroupBy(Customers['CompanyName']).Histogram.Active:=True;
       end;

   25: begin
         // Histogram by Boolean data
         // (redundant: this is the same as a normal GroupBy without Histogram)
         // Just to test, and to make Histogram more "complete" supporting boolean too

         result.AddMeasure(OrderDetails['Quantity'],TAggregate.Sum);
         result.AddGroupBy(Products['Discontinued']).Histogram.Active:=True;
       end;
  end;
end;

destructor TSampleSummaries.Destroy;
begin
  SumData3.Free;
  SumData2.Free;
  SumData1.Free;

  inherited;
end;

procedure TSampleSummaries.LoadData(const AStore:String);
begin
  Demo.Free;

  if AStore='' then
     Demo:=TStore.Load('SQLite_Demo')
  else
     Demo:=TStore.Load(AStore,'SQLite_Demo');

  Products:=Demo['Products'];
  Orders:=Demo['Orders'];
  OrderDetails:=Demo['"Order Details"'];

  Customers:=Demo['Customers'];
  City:=Customers['City'];

  Suppliers:=Demo['Suppliers'];
  Categories:=Demo['Categories'];
  Shippers:=Demo['Shippers'];

  Movies:=TStore.Load('BISamples','MovieDB');
end;

initialization
  Samples:=TSampleSummaries.Create;
  Samples.LoadData;
finalization
  Samples.Free;
  Samples:=nil;
end.
