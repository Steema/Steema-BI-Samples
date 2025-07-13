# TeeBI
### Telegram forums: [https://t.me/steema_bi](https://t.me/steema_bi)

![TeeBI Gridify and Colorize](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Gridify_colored.png)

## Datamining, Visualization, Multidimensional Queries, Pivot Tables, Big data
 
### What is TeeBI ?

A simple class to implement in-memory database complex structures, to provide:

- [Ultra fast](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Speed) speed (every column or field is a simple array)

- Big-data ready, billions of cells supported

- Multi-platform (Windows, Mac OSX, Android, iOS, Linux and more), for VCL, Firemonkey, Delphi and C++

- Automatic [visualizations](https://github.com/Steema/BI/wiki/visualization) of complex structures ([charts](https://github.com/Steema/BI/wiki/bichart), grids, optional [TeeGrid](https://github.com/Steema/TeeGrid-VCL-FMX-Samples) and more)

- SQL-like high-speed [queries and summaries](https://github.com/Steema/BI/wiki/queries) by code or "select" scripts

- Geographic database (Countries, States, Provinces...) to automatically create summary geo queries, TeeChart world maps, and [statistics](https://github.com/Steema/BI/wiki/statistics)

  ![](https://raw.github.com/Steema/BI/master/docs/img/geo_chart_usa_counties.png)


- Transparent [remote web server](https://rawgit.com/Steema/BI/master/demos/online/remote_web/index.htm) fast data access (compressed binary streams of raw arrays)

- Automatic relationships between columns (master-detail), indexes and foreign keys

- [Import data](https://github.com/Steema/BI/wiki/importing) from files, databases, objects (ORM), [AI](https://github.com/Steema/TeeBI/tree/master/demos/3rd_party/AI) in one or few lines of code

- [Export data](https://github.com/Steema/BI/wiki/exporting) (to csv,pdf,html,xml,excel,json), comparisons and manipulations

- Supported development environments: 

  *  Embarcadero RAD Studio Athens 12.3, and from Studio XE4 and up (Delphi and C++)
  *  Lazarus / FreePascal
  
### Getting started

- [Installer Tool](https://github.com/Steema/BI/tree/master/install) and [Installing info](https://steema.com/docs/teebi/tutorials/Installing-TeeBI)

- [Documentation](https://github.com/Steema/TeeBI/wiki)

- [API Reference](https://steema.com/docs/TeeBIVCLReference.htm)

- [Examples](https://github.com/Steema/BI/tree/master/demos)

- [Screenshots](https://github.com/Steema/TeeBI/blob/master/docs/img/readme.md)

### Example code

```pascal
uses BI.DataItem, BI.CSV, BI.Db, BI.Xml, BI.Json;

var Data1 : TDataItem;
Data1 := TBICSV.FromFile( 'mydata.csv' );  // also From TStrings, String etc

// other importing methods:
Data1 := TBIDB.From( SQLConnection1 ); // loads all tables in one line of code
Data1 := TBIJson.From ...
Data1 := TBIXML.From ... // import different formats

// import from Artificial Intelligence AI agents, like Google Gemini:
Data1 := TBIAI.From('Give me the list of the highest 10 mountains﻿ by elevation in csv format, just the list');

// from arrays, TCollection, custom Records (via RTTI):
Data1 : TTypeProvider<TCustomer>.Create(Self, MyArrayOfCustomers).Data; 

// import from components with automatic detection:
Data1 := TComponentImporter.From(Self, Memo1.Lines);
Data1 := TComponentImporter.From(Self, DataSource1);

// importing from any URL, automatic detection of content format:
Data1 := TBIURLSource.From('https://www.mysite.com/get/mydata?param=123');

// queries, including group by, sort, expressions, having, sub-select, distinct, date operators etc
Data1 := TBISQL.From( Data2, 'sum(Amount) where (Customer=123) and (Product<456) group by Country, Year');
Data1 := TBISQL.From( Data2, 'ProductName, UnitPrice where UnitPrice > select Average(UnitPrice)');
Data1 := TBISQL.From( Movies, 'top 100 offset 15000 year, length');

// visualizing
BIGrid1.Data := Data1;
BIChart1.Data := Data1;

BIGrid2.Data := Data1['Products']; // sub-tables

// importing data from a standard TeeChart Series:
Data1 := TChartData.From(Series1);

```
#### Online grids and charts from remote queries using BIWeb Server:
[http://steema.cat:15015/?data=SQLite_demo&format=.jpg](http://steema.cat:15015/?data=SQLite_demo|%22Order%20Details%22&format=.jpg&summary=sum(Quantity);Shippers.ShipperID)


### Related

- [TeeChart VCL/FMX](https://www.steema.com/product/vcl)
- [TeeGrid Control](https://www.steema.com/product/gridvcl)

### Deprecated

The following features have been marked as obsolete / incompatible, and moved to a separate folder outside the product:

- [Machine-learning](https://github.com/Steema/BI/wiki/machine-learning) (native data pass to R Language and Python Scikit)

- Interactive [Dashboards](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Dashboard_VCL.png) rendered to screen (desktop and mobile) and HTML Web pages
