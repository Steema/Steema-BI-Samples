## Importing Data

Data items can be easily created by code, or imported.

This example shows how to import data from different sources into TDataItem classes.


[Documentation about the importing TeeBI features](https://github.com/Steema/TeeBI/wiki/importing)

### Example screenshot:

![TeeBI Import Demo](https://raw.github.com/Steema/BI/master/demos/delphi/vcl/Import/TeeBI_Import_Demo.png)

- Several ways to import data by code

  Import data from files and text in CSV, JSON, XML, Excel formats and from database components (Connections, any TDataset like ClientDataset, etc)
  
  For example:
  
  ```delphi
  uses
    BI.DataItem;
  
  var Data : TDataItem;`
  
  Data:=TBICSV.FromFile('test.csv');`
  Data:=TBICSV.FromStrings(Memo1.Lines);
  ```

- From files with automatic content detection

  Import data from files, automatically guessing the file format based on extension:
  
  `Data:=TBIFileSource.FromFile('myfile.json');`

- From Components

  Import data from a component, like Memos and Datasets:
  
  `Data:=TBIDB.FromDataSet(ClientDataSet1);`
  
  `Data:=TBICSV.FromStrings(Memo1.Lines);`

- From a TeeBI "Store"

  Import data from a local "store" (a folder in any disk that is configured using the Store Editor dialog)
  
  `Data:=TStore.Load('My Local Store','SQLite_Demo');`
  
  or from a remote store at a BIWeb server like for example at: www.steema.cat
  
  ```delphi
  Web:=TBIWebClient.Create('steema.cat'); // or: 'localhost'
  try
    Data:=Web.GetMetaData('Acme Database', TWebCompression.No);
  finally
    Web.Free;
  end;
  ```

- From records or object instances using RTTI

  Load custom records or classes into TDataItem:
  
  ```delphi
  var tmp : TTypeProvider<TPerson>;
  tmp:=TTypeProvider<TPerson>.Create;
  
  tmp.Add( JohnDoe );   // <-- any TPerson instance
  tmp.Add( MyPersonArray );  // <-- or List or Collection (any IEnumerable)
  ...
  
  Data:=TDataItem.Create( tmp );
  ```
  
  Note: For a more advanced usage of RTTI with TeeBI, please see the ORM_RTTI demo included with TeeBI installer.

  More code samples:
  
```delphi
uses
  BI.DataItem, BI.CSV, BI.Db, BI.Xml, BI.Json, BI.AI;

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

// importing data from a standard TeeChart Series:
Data1 := TChartData.From(Series1);
```
  
