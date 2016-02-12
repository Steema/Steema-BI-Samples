# TeeBI Release Notes
-------------------

## 12-Feb-2016  Beta 8

- Installation

  * A new checkbox at TeeBIRecompile.exe installer tool determines if TeeBI packages will be recompiled using TeeChart (any version, Lite or Pro)
  
- TDataSearch

  * New class to perform data search on any TDataItem.
    
    It returns the array of row indices with search matches and also the array of individual "cell hits".

    The [Data Search demo](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Search) shows how to use it to filter and/or highlight grid cells.
    
    Several properties control case-sensitive text, partial matches and if the search is performed using a background thread to not block typing in the search box when searching in huge data quantities (millions of cells).
    
    ```delphi
    uses BI.Data.Search;
    var Search : TDataSearch;
        Search.Source := MyData;
        BIDataset1.PrepareIndex( Search.Find('Hello') );
    ```

- PDF Exporting

  * New TBIPDFExport class generates a PDF file or stream from a TDataItem.
  
    Several properties can be used to define PDF font styles, headers, footers and page numbering.
    Note: PDF exporting uses the TPDFCanvas class which is available in TeeChart version Pro only.


    ```delphi
    uses BI.VCL.PDF;  // or BI.FMX.PDF for Firemonkey
    TBIPDFExport.SaveToFile( MyData, 'test.pdf' );
    ```

- BIWeb server

  * Data can be returned in PDF format using ".pdf" in the URL request:
 
    http://steema.cat:15015/?data=SQLite_demo|Customers&format=.pdf
  
- BI.Arrays
 
  * Speed optimizations and a new overload Copy method in all array classes
 
- TDataItem

  * New optional parameter "Count" at Delete method to delete multiples rows
  
- Database

  * Support for FireDAC "ADS" (Advantadge Database Server) driver
  * TBIDB.ImportFile now supports Microsoft Access (*.mdb and *.accdb) database files
  * New TBIDataset PrepareIndex method, enables setting the array of row indices that BIDataset uses to filter records
  
- Queries and SQL

  * New support for "offset" SQL clause, to specify the first row to return after the query is finished. Default "offset" is zero.
  * More combinations of queries supported (queries from summaries with TDataItem complex structures)
  * Big speed optimization, data items are directly copied (cloned) when possible, instead of looping all rows
  
- XML

  * New TBIXML ExcludeNodes property
  
    Defines the node <tags> in the source xml content that should be excluded from importing.

  * Fixed bug when importing nested master-detail xml tags (sub-tags)

- Microsoft Excel

  * New tab at Data Manager dialog enabled specifying the number of rows of an Excel spreadsheet that will be used as the "header" of the imported columns.  This option corresponds to TBIExcel.HeaderCount property
  
- Expressions

  * New TDateTimeExpression Date and Time properties, return only the current date or current time, and not both
  
- Additional

  * New TStores.Exist function, returns True if a given store is registered
  * New TBIREngine Version property returns the "R Language" engine version
  * Several fixes in BI.Data.CSV import class
  * Fixed saving a multi-line SQL string in the Data Manager dialog
  * Fixed in the TBISQL parser
  * Fix for TSummary queries that involve datetime expressions using "Decade"
  * Fix when renaming a data item at the Data Manager dialog, renames its associated folder if it exists
  * Several fixes for FreePascal / Lazarus
  

## 27-Jan-2016  Beta 7

- Algorithms
  * Native access to R Language (32bit and 64bit) using [opaR library](https://github.com/SigmaSciences/opaR)
  
    The new [BI.Plugins.R.opaR](https://github.com/Steema/BI/blob/master/src/delphi/Algorithms/BI.Plugins.R.opaR.pas) unit contains the code that uses opaR library.

  * New VCL and FMX forms for "Console" interactive R calls.
    
    These forms interact with R to call any R method or obtain R variables.

    The console can be easily embedded in your projects:

   ```pascal
    uses BI.VCL.RConsole, BI.VCL.Grid;
      var R : TBIRConsole; 
        R:= TBIRConsole.Create(Self);
        TVCLCommon.AddForm(R, MyPanel); // "MyPanel" can be any parent control
        TBIREngine.Engine.Output:= R.Memo.Lines;
    ```
  
  * Instructions to setup TeeBI for machine-learning [click here](https://github.com/Steema/BI/wiki/Installing-R-and-Python)

- Summary
 
 * New [TSummaryTotals](https://github.com/Steema/BI/blob/master/src/delphi/BI.Summary.Totals.pas) class to calculate summary subtotals and grand-totals.
 
   ```pascal
   uses BI.Summary.Totals;
   BIGrid1.Data:= TDataItem.Create( TSummaryTotals.Create( MySummary ));
   ```

 * New class function TSummaryItem.GuessType
 
   Returns if a TDataItem can be used as a summary "Measure", "Group", or "Both".

   This is useful to create summaries without knowing the kind of data (integers, text, etc)

   ```pascal
   uses BI.Summary;
   if TSummaryItem.GuessType( MyData[2] ) <> TSummaryItemType.GroupBy then 
      MySummary.AddMeasure( MyData[2], TAggregate.Sum );
   ```
 
- Queries

 * New [TBISQL](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.SQL.pas) class function to convert a summary or query from / to SQL language.
 
   ```pascal
   // Note: Only a subset of SQL "select" clause is supported.
   uses BI.Data.SQL;
   Memo1.Text:= TBISQL.From( MySummary ); 
   
   var MyData : TDataItem;
   MyData:= TBISQL.From( Data, Memo1.Text);  // "Data" contains items referred in text
   ```   
      
 
- Importing Objects (ORM using RTTI)

 The new [BI.Data.RTTI](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.RTTI.pas) unit contains a generic class to import any Record or TObject instance (or a TList of Array etc) into a TDataItem.
 
 ```pascal
 uses BI.Data.RTTI;
 var D : TDataItem; 
 D:= TDataItem.Create( TTypeProvider<TCustomer>.Create( MyCustomers ));
 
 // "MyCustomers" can be an "array of TCustomer", TList<TCustomer>, etc
 ```
 
 The TTypeProvider class has methods to Add, Find, Remove and Update items, so its basically mapping a TDataItem and its children Items with your custom record or class fields.
 
 
 
- BIWeb server
  * Remote query execution
 
    Adding the "sql=select * from customers..." tag to the [TBIWebClient](https://github.com/Steema/BI/blob/master/src/delphi/BI.Web.pas) URL request.

    [Live Example](http://steema.cat:15015/?data=SQLite_demo&format=.htm&sql=select * from Customers where City="Madrid")

  * Remote query by code
  
  ```pascal
  BIGrid1.Data:= TBIWebClient.Query('Steema','SQLite_Demo','select * from Customers where City="Madrid"');
  ```

  
- Data Filtering
  * New "IsNull" embedded function to use it in filters to skip or not "missing" null data.
  
  ```pascal
var Query : TDataSelect;
begin
  Query:=TDataSelect.Create;
  Query.Add(Demo['Orders']);
  Query.Filter:=TDataFilter.FromString(Demo,'(not IsNull(Orders.Date)) and (Customers.Name="intel")');

  result:=TDataItem.Create(Query);  // <-- Set Query as Provider for result
end;
  ```
  
- Data Provider

  * A new class TDataProvider enables automatic nested TDataItem data loading.
  
   For example, a TDataItem might be filled with data that is the output result of a query, which in turn is using other TDataItem datas that might have also a Provider (recursively).

  This mechanism has been added to queries, summaries and other classes that are now capable of doing "select ... from select ..." nested queries.  Other classes also have been refactored to be "Providers" of data, like the class that handles remote Web requests of TDataItem data streams.
  
  With this new change, its no longer necessary to "calculate" queries or summaries. Simply construct a TDataItem passing a Provider parameter, and the summary or query will be automatically calculated "just-in-time" when its necessary:
  
  ```pascal
  BIGrid1.Data:= TDataItem.Create( MyQuery );
  ```
  
- Lazarus and FreePascal
  * Fixes and improvements to support TeeBI with Lazarus and FreePascal
  
    TeeBI can be used in Lazarus 1.7 (FreePascal 3.0) under Windows and Linux.

    The [BI.FPC](https://github.com/Steema/BI/blob/master/src/delphi/BI.FPC.pas) unit contains several helper methods internally used to avoid adding "$IFDEF" in other units.

    Note: Lazarus TDataset field does not support nested "ADT" dataset fields.

  * Support for importing database data with Lazarus
 
    The new [BI.Data.SqlDB](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.SqlDB.pas) unit plugs with Lazarus SqlDB engine and all of its supported database formats.

    (Firebird, Interbase, Oracle, Postgres, ODBC, SQLite, Microsoft SQL Server and MySQL)


- New in [BI.Arrays](https://github.com/Steema/BI/blob/master/src/delphi/BI.Arrays.pas) unit 
  * New helper method "Append" for all TxxxArray classes to append another array into it.

- Data Comparison
  * The [TDataCompare](https://github.com/Steema/BI/blob/master/src/delphi/BI.Compare.pas) class has been improved alot. It can be used to test if two TDataItem instances are equal or not (in both structure and data), and optionally obtain the differences in an output TDataItem.
  
  ```pascal
  uses BI.Compare;
  var Diff : TDataItem;
  if not TDataCompare.Same( MyData1, MyData2, Diff) then
     BIGrid1.Data:= Diff;
  ```
  
- CSV format
 * Improvements in CSV data import.
 
   Several new features in [TBICSV](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.CSV.pas) class enable importing huge amounts of data at fast speed (1 billion cells in 120 seconds).

   For detailed CSV data import usage, please [follow this link](https://github.com/Steema/BI/wiki/Importing-CSV-data)
   
 
- Other changes and fixes
  * Fixes for XE4 Firemonkey forms
  * BIWeb server project support for XE4, using SQLExpress instead of FireDAC
  * TDataArray new IndexOf method:
    ```pascal
    var D : TDataArray; D:=[ MyData1, MyData2, ...];
    if D.IndexOf('Hello') <> -1 then ...
    ```
    
  * TSortItem new Active (boolean) property, to enable / disable sort order items

  * New [BI.Data.Xml.OXml](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.XML.OXml.pas) unit to support importing Xml data with Oxml. (Also available [Delphi Xml](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.XML.MSXML.pas) and [OmniXml](https://github.com/Steema/BI/blob/master/src/delphi/BI.Data.XML.Omni.pas) )
 
  * New OnProgress event in base [TBISource](https://github.com/Steema/BI/blob/master/src/delphi/BI.DataSource.pas) class
 
    Optional OnProgress event is called while data is being imported, enabling cancelling the import.

  * Executables TeeBIRecompile and BIWeb now codesigned using SHA256 secure hash.

  
## 22-Dec-2015  Beta 6

- New TBIVisualizer control (VCL and FMX), first beta.

- New TBIVisualizer editor dialog.

- TBICSV can now import 1 billion cells using the 64bit compiler.

- MariaDB database now supported (using the MySQL driver)

- Support for customizing database server port in Data Manager.

- Support for Excel exporting of sub-sub-dataitem tables.

- Binding a TBIDataset to a TBIGrid is now much faster.

- Added "R Datasets" in binary format to "Sample Data" folder.

- Support for Geographical (World maps) charts.

- Initial support for sub-folders in TStore folders.

- Updated documentation and examples with new features.

- Bug fixing and speed optimizations in base TDataItem class.


## 27-Nov-2015  Beta 5

- Encryption mechanism using TurboPack LockBox (optional).

- Summary "Having" filter property (same as SQL).

- Microsoft Excel exporting using TMS Flexcel (optional).

- Support for remote TDataSelect queries in BIWeb http server.

- Multiple expressions are now allowed for SortBy data ordering.

- TBIGrid Colorize properties to fill cell backgrounds based on values.

- Helper method to convert select and summary queries to SQL language.

- Delphi unit generator emits Pascal code from any TDataItem or database.

- FreePascal v3.0 initial support for TeeBI core units.

- Design-time IDE top menu "TeeBI" to access the "Data Manager", etc.

- Updated documentation and examples with new features.

- Bug fixing and speed optimizations, specially for big Summary queries.


## 20-Nov-2015  Beta 4

- New TDataSelect class, implements a big part of SQL language in native Delphi code.

- Cumulative and/or percentage calculations in summaries.

- microOLAP DAC MySQL import support.

- Links between data in different storage locations or www servers.

- Extra data Links can be created at import time using TDataDefinition.SetMasters method.

- Enabled Sorting data reordering the source rows.

- Updated Starting Guide doc with new content in beta4

- Starting Guide doc in Spanish: "Guía de Introducción"

- Convert from queries and summaries to SQL language.

- Percent format for BIDataset numeric columns.

- Optional "raw" persistence mode (BI.Streams.pas).

- Fixes and speed optimizations.

## 6-Nov-2015  Beta 3

- Initial support for "R" language and Python Scikit-learn machine-learning algorithms. 
 Several algorithms included.

- "Starting Guide" document has been updated.

- Many fixes and small improvements.

- Design-time debug visualizer for TDataItem objects.

- More demos included.

- TeeBIRecompile automatic recompilation tool improved.


## 8-Oct-2015 Initial Betas (1 and 2)

