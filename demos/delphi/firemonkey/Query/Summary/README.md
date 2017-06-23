TeeBI Summary
==============

The example TeeBI_FMX_Summary shows how to start working with basic Query and Summaries options.
The summaries used in this example are created at runtime, via code, also groups the data results by Text.

Here we can see a small part of the demo, where we get the data create a Sumamry  with grouped data result, hide the duplicated content on Grid and customize the columns width :

```
  // Load data
  Demo:=TStore.Load('SQLite_Demo');

  // Create Summary
  Summary:=TSummary.Create(Self);

  // Add Sum of Quantity
  Summary.AddMeasure(Demo['"Order Details"']['Quantity'],TAggregate.Sum);

  // Assign a name for Field,which will appear at the Column Header
  Summary.Measures[0].Name := 'Sum of Quantity of Orders';

  // Add "By CustomerID"
  ByCustomer:=Summary.AddGroupBy(Demo['Orders']['CustomerID']);
  ByCustomer.Layout:=TGroupByLayout.Rows; // <-- optional

  // Add "By CompanyName" to show grouped by Shippers Company
  ByCompany:=Summary.AddGroupBy(Demo['Shippers']['CompanyName']);
  ByCompany.Name := 'Shippers';

  // NOTE: Delphi 10.1 Berlin Firemonkey Grid is capable of displaying
  // sub-tables, thus TGroupByLayout.Columns is supported

  ByCompany.Layout:=TGroupByLayout.Rows; // <-- optional

  // Show Summary in BIGrid
  BIGrid1.Data:=Summary.NewData;

  // Tell grid to not display cells with duplicate content
  BIGrid1.Duplicates(BIGrid1.Data['CustomerID'],True);

  // Accessing Grid properties and methods
  Grid.Columns[0].Width := Grid.Columns[0].Width + 25;
  Grid.Columns[1].Width := Grid.Columns[0].Width + 25;
  Grid.Columns[2].Width := 150;
  ```

![screenshot](https://raw.githubusercontent.com/Steema/BI/master/demos/delphi/firemonkey/Query/Summary/img/Summary.jpg "TeeBI Summary")


