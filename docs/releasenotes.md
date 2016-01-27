# TeeBI Release Notes
-------------------

## 27-Jan-2016  Beta 7

- Algorithms
  * Native access to R Language (32bit and 64bit) using [opaR library](https://github.com/SigmaSciences/opaR)
  * New "Console" interactive forms VCL and FMX for R Language.
    
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
 
 * New class to calculate summary subtotals and grand-totals.
 
   ```pascal
   uses BI.Summary.Totals;
   BIGrid1.Data:= TDataItem.Create( TSummaryTotals.Create( MySummary ));
   ```

- Queries

 * New class to convert a summary or query from / to SQL language.
 
   ```pascal
   // Note: Only a subset of SQL "select" clause is supported.
   uses BI.Data.SQL;
   Memo1.Text:= TBISQL.From( MySummary ); 
   
   var MyData : TDataItem;
   MyData:= TBISQL.From( Data, Memo1.Text);  // "Data" contains items referred in text
   ```   
      
 
- BIWeb server
  * Remote query execution
 
    Adding the "sql=select * from customers" tag to the BIWeb URL request.

    [Live Example](http://steema.cat:15015/?data=SQLite_demo&format=.htm&sql=select * from Customers")

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

