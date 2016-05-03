{*********************************************}
{  TeeBI Software Library                     }
{  Select queries examples                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.SelectSamples;

interface

uses
  BI.DataSource;

type
  TSelectSamples=record
  public
    class function Count:Integer; static;
    class function Select(const AIndex:Integer):TDataSelect; static;
  end;

implementation

uses
  System.SysUtils,
  BI.Data, BI.Persist, BI.Summary, BI.Expression,
  BI.Tests.SummarySamples;

{ TSelectSamples }

class function TSelectSamples.Count: Integer;
begin
  result:=31;
end;

type
  TDataItemAccess=class(TDataItem);

// Return an example Query
class function TSelectSamples.Select(const AIndex: Integer): TDataSelect;

  function CitiesOf(const ACountry:String):TDataItem;
  var tmp : TDataSelect;
  begin
    // select distinct City, Country from Customers where Country=ACountry
    tmp:=TDataSelect.Create(nil);

    tmp.Data:=Samples.Demo;

    tmp.Add(Samples.Customers['City']);
    tmp.Add(Samples.Customers['Country']);

    tmp.Distinct:=True;
    tmp.Filter:=TDataFilter.FromString(Samples.Customers,'Country="'+ACountry+'"');

    result:=TDataItem.Create(tmp);

    // Ensure proper destroy to avoid memory leak
    TStores.GlobalCache.Items.Add(result);
  end;

  // Average(UnitPrice)
  function CalculateAverage:TSummary;
  begin
    result:=TSummary.Create(nil);
    result.Measures.Add(Samples.Products['UnitPrice'],TAggregate.Average);
  end;

  // UnitPrice > Avg(UnitPrice)
  function BiggerThanAverage:TLogicalExpression;
  begin
    // UnitPrice > Average
    result:=TLogicalExpression.Create(
                TDataItemExpression.Create(Samples.Products['UnitPrice']),
                TLogicalOperand.Greater,
                TDataItemExpression.Create(TDataItem.Create(CalculateAverage),True));
  end;

  procedure DistinctOrderDetails(const AQuery:TDataSelect);
  var ProductID,
      Discount,
      OrderDetails : TDataItem;
  begin
    AQuery.Distinct:=True;

    OrderDetails:=Samples.Demo['"Order Details"'];
    ProductID:=OrderDetails['ProductID'];
    Discount:=OrderDetails['Discount'];

    AQuery.Add(ProductID);
    AQuery.Add(Discount);

    AQuery.SortBy.Add(ProductID);
    AQuery.SortBy.Add(Discount);
  end;

  procedure CreateExample(const AIndex:Integer);
  var tmpSum : TSummary;
  begin
    case AIndex of
      0: begin
           result.Add(Samples.Customers);
         end;

      1: begin
           result.Add(Samples.Customers['CompanyName']);
           result.Add(Samples.City);
         end;

      2: begin
           result.Add(Samples.Customers['CompanyName']);
           result.Add(Samples.City);

           result.SortBy.Add(Samples.City);
         end;

      3: begin
           result.Add(Samples.City);
         end;

      4: begin
           result.Add(Samples.City);
           result.Distinct:=True;
         end;

      5: begin
           result.Add(Samples.Customers['Country']);
           result.SortBy.Add(result.Items[0].Data);
           result.Distinct:=True;
         end;

      6: begin
           result.Add(Samples.Customers['CompanyName']);

           result.Add(Samples.Orders['ShipVia']);
           result.Add(Samples.Orders['ShipVia']); // <-- repeat field, just to test it is ignored

           result.Add(Samples.Shippers['CompanyName']);
         end;

      7: begin
           result.Add(Samples.Customers['Country']);
           result.SortBy.Add(result.Items[0].Data);
           result.Distinct:=True;
           result.Filter:=TDataFilter.FromString(Samples.Customers,'Country<>"Mexico"');
         end;

      8: begin
           result.Add(Samples.Customers['Country']);
           result.SortBy.Add(result.Items[0].Data);
           result.Distinct:=True;
           result.Filter:=TDataFilter.FromString(Samples.Customers,'City<>"Cork"');
         end;

      9: begin
           result.Add(Samples.Customers['Country']);
           result.SortBy.Add(result.Items[0].Data);
           result.Distinct:=True;
           result.Filter:=TDataFilter.FromString(Samples.Customers,'(City<>"Salzburg") and (City<>"Graz")');
         end;

     10: begin
           result.Add(Samples.Customers['Country']);
           result.SortBy.Add(result.Items[0].Data);
           result.Filter:=TDataFilter.FromString(Samples.Customers,'City="Madrid"');
         end;

     11: begin
           result.Add(Samples.Suppliers['CompanyName']);
           result.Add(Samples.Suppliers['Region']);
         end;

     12: begin
           result.Add(Samples.Suppliers['Region']);
           result.Distinct:=True;
         end;

     13: begin
           result.Add(Samples.Customers);
           result.SortBy.Add(Samples.Customers['ContactTitle']);
         end;

     14: begin
           result.Add(Samples.Customers);
           result.Max:=10;
         end;

     15: begin
           result.Add(Samples.Customers);
           result.SortBy.Add(Samples.City);
           result.Max:=10;

           // Test sorting with "Start" offset
           //result.Start:=30;
         end;

     16: begin
           result.Add(Samples.City);
           result.Distinct:=True;
           result.Max:=10;
         end;

     17: begin
           result.Add(Samples.Products['UnitPrice']);
           result.SortBy.Add(Samples.Products['UnitPrice'], False);
           result.Max:=5;
         end;

     18: begin
           result.Add(Samples.Products['ProductID']);
           result.Add(Samples.Products,'UnitPrice * (UnitsInStock+UnitsOnOrder)');
         end;

     19: begin
           result.Add(Samples.Orders,'Year(OrderDate)');
           result.Add(Samples.Orders,'Month(OrderDate)');
           result.Add(Samples.Orders,'ShortMonth(OrderDate)');
           result.Add(Samples.Orders,'Day(OrderDate)');
           result.Add(Samples.Orders,'ShortWeekDay(OrderDate)');
           result.Add(Samples.Orders,'Quarter(OrderDate)');
         end;

     20: begin
           // Using nested "select" in expressions at SQL From clause, for example:
           // SELECT ProductName, Price FROM SELECT * from Products

           // Add City column from nested select query:
           result.Add(CitiesOf('France')['City']);
         end;

     21: begin
           // Using nested "select" in expressions at SQL Where clause:
           // SELECT ProductName, UnitPrice FROM Products
           // WHERE UnitPrice>(SELECT AVG(UnitPrice) FROM Products);

           result.Add(Samples.Products['ProductName']);
           result.Add(Samples.Products['UnitPrice']);

           result.Filter:=BiggerThanAverage;
         end;

     22: begin
           result.Add(Samples.movies['movies']['title']);
           result.Add(Samples.movies['movies']['year']);
           result.Add(Samples.movies['movies']['length']);
         end;

     23: begin
           // Test some individual column(s) and a complete table (Products.*)
           result.Add(Samples.Demo['Categories']['CategoryName']);
           result.Add(Samples.Products);
           result.Add(Samples.Demo['Categories']['CategoryID']);
         end;

     24: begin
           // Test selecting a sub-sub-table using a summary as data sample

           tmpSum:=Samples.CreateSummary(13);
           try
             Samples.SumData1.Free;
             Samples.SumData1:=tmpSum.Calculate;

             result.Add(Samples.SumData1['Sum of Quantity']['Quarter of OrderDate']);
           finally
             tmpSum.Free;
           end;

         end;

     25: DistinctOrderDetails(result);

     26: begin
           // Test selecting a sub-table (that has another sub-table) using a summary as data sample

           tmpSum:=Samples.CreateSummary(13);
           try
             tmpSum.Measures[0].Aggregate:=TAggregate.Count;

             Samples.SumData2.Free;
             Samples.SumData2:=tmpSum.Calculate;

             // Summary Layout is: Items (columns)

             result.Add(Samples.SumData2['Count of Quantity']);

             result.Data:=Samples.SumData2;
           finally
             tmpSum.Free;
           end;

         end;

     27: begin

           tmpSum:=Samples.CreateSummary(13);
           try
             tmpSum.Measures[0].Aggregate:=TAggregate.Count;

             tmpSum.By.Delete(0);
             tmpSum.By.Delete(1);

             tmpSum.By[0].DateOptions.Part:=TDateTimePart.WeekOfYear;

             Samples.SumData3.Free;
             Samples.SumData3:=tmpSum.Calculate;

             result.Add(Samples.SumData3['Count of Quantity']);
             result.SortBy.Add(Samples.SumData3['Count of Quantity'],False);

             result.Data:=Samples.SumData3;
           finally
             tmpSum.Free;
           end;

         end;

     28: begin
           result.Add(Samples.movies['movies']['year']);
           result.Add(Samples.movies['movies']['length']);

           result.Start:=15000;
           result.Max:=100;
         end;

     29: begin
           result.Add(Samples.Customers['Country']);
           //result.SortBy.Add(result.Items[0].Data);

           result.Filter:=TDataFilter.FromString(Samples.Customers,'City="Madrid"');

           result.Start:=2;
           result.Max:=1;
         end;

     30: begin
           result.Add(Samples.Products,'UnitPrice * (UnitsInStock+UnitsOnOrder)');

           // Test of "distinct" clause on expressions
           result.Distinct:=True;
         end;

     // Pending:

     // Top/Limit as percentages:
     // select top 10 percent * from Customers

     // JOIN modes (inner left, right, full outer) and UNION modes (union, union all)
    end;
  end;

begin
  result:=TDataSelect.Create(nil);

  try
    CreateExample(AIndex);
  except
    on Exception do
    begin
      result.Free; // <-- avoid memory leak
      result:=nil;
    end;
  end;
end;

end.
