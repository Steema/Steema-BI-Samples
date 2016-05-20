# TeeBI Release Notes
-------------------

## 20th-May-2016  Beta 11

- **RAD Studio 10.1 Berlin**

  Added TeeBI support for 10.1 Berlin release.
  
  Also working TeeBI controls with new "FireUI Live Preview" app in Berlin:
  
  ![](https://raw.github.com/Steema/BI/master/docs/img/Studio10_1_Berlin_TeeBI_FireUI_Live_Preview.png)
  
  For more information about integrating TeeBI and TeeChart with Live Preview, click here:
  
  [Berlin FireUI Live Preview and TeeBI, TeeChart](https://steema.com/wp/blog/2016/04/19/rad-studio-10-1-berlin-add-teechart-to-fireui-live-preview/)
  
- **Pivot-Tables**

  New BIQuery component to create select queries, summaries and pivot-tables at both design and run-time.
  Editor dialog enables drag-drop of data item fields and output preview.
  
  ![](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Pivot_Query_Editor.png)
  
- **BIChart improvements**

  A complete refactoring of the BIChart control provides a new Options property with several settings to create charts from any kind of data structure and content in a smart way.
  
  The automatic decisions done by BIChart have been summarized here:
  
  [BIChart logic](https://plus.google.com/+DavidBerneda/posts/T6jVGbcFyak)
  
  ![](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Chart_Design_Time.png)
  
- **Dynamic Filter**

  New class and editor dialog to enable visual building of filter "expressions" by checking / unchecking items on a tree view.
  Linked data tables are automatically included in the tree, together with its individual items.
  The BIQuery editor includes this new Dynamic Filter editor to define the query "where" clause.
  
  ![](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Dynamic_Filter.png)
  
- **Dashboards**

  New TDashboard Layout property to customize the arrangement of dashboard panels inside a BIVisual control.
  Predefined layouts are included in TLayouts.Predefined property.
  
- **Component Importing**

  New class and editor dialog to "link" any BI control (BIGrid, BIChart etc) and BI components (BIQuery, etc) with any supported  Component living on any accessible Form or DataModule.
  
  The edialog dialog shows the available supported components, so selecting them creates an internal TDataItem and imports the component content into it automatically.
  
  For example, text from Memo controls (in csv,json,xml,etc) , any TDataset-derived component, ListBoxes, TXXConnection components are recognized and made available.
  
  Importing data from components is done transparent and at fast speed, so the BI controls receiving the output just get normal TDataItem(s) as if they were already imported and persisted from a TStore or by code.
  
  ![](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Component_design-time.png)
  

- **Data Selector**

  New dialog that includes both the already existing data selector tree, and the new Component selector dialog.
  This dialog is now the default editor for design-time choosing of all components "Data" property.
  
- **Provider events**

  Many classes and components issue internal events when data is changed or destroyed.
  Other components and controls (BIGrid, BIChart, etc) get notified of these events to refresh its output or make sure there are no memory leaks. This mechanism works at design-time too, so changes are refreshed automatically.
  
- **BI Arrays**

  Improved all the Array classes with a new Copy method that can optionally copy a subset of the array based on an index parameter.
  The "index" is just a TInt32Array containing the positions of the items to copy.
  
  These methods are also internally used to fix the issue of creating a "Map" of the array items considering only the non-null values.
  TDataItem is now passing its "Missing" index array when creating the DataMap property.
  
  
  
  
  
## 1st-April-2016  Beta 10

- **Dashboards**

  ![](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Dashboard_VCL_Small.png)

  New units and a new [TBIVisual](https://github.com/Steema/BI/blob/master/src/delphi/BI.Dashboard.pas) component to create visual dashboards.  *Note: Preview Alpha Release*
  
  Dashboards provide a way to create complex multi-panel layouts of grids, charts and other controls using a "template".
  
  The template is just a TDataItem which can be imported for example from a JSON string.
  
  Templates can be rendered in VCL, Firemonkey and also as [HTML Web pages](https://github.com/Steema/BI/blob/master/src/delphi/BI.Dashboard.HTML.pas).
  
  Templates contain named definitions of data (queries, summaries, custom data, external files, etc) and then panels can optionally use these data and specify the render style (grid, chart, listbox, text, etc) for each panel independently.
  
  Templates can also contain "layout" definitions. Layouts can be organized [nesting predefined layouts](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Nested_Dashboard_Layouts.png) to create complex visualizations.
  
  Finally, templates define "dashboards" that consist of a group of "panels" with an optionally specified layout.
  
  TBIVisual can then generate a template using its Render property, which can be a VCL / Firemonkey render, or an HTML Web render component to output web pages.
  
  Interactivity between panels is controlled by using "variables" defined in the template. Variables can be used as parameters everywhere in the template, for example to filter sql "where" clauses or any other purpose.
  
  For example, clicking a listbox panel can refresh other panels that depend on the current listbox selected item, for example grids and charts that are displaying data using SQL queries using the variables as parameters in the query.
  
  *Note: No demo for dashboards is provided yet. It'll be uploaded soon*
  
  New Dashboard Units:
  
    * BI.Dashboard
    * BI.Dashboard.HTML
    * BI.VCL.Dashboard
    * BI.FMX.Dashboard
    * BI.VCL.Dashboard.Chart
    * BI.FMX.Dashboard.Chart
    
    
- **Demos**

  A new example has been added to both GitHub repository and TeeBI installer:
  
  * [Expressions](https://github.com/Steema/BI/tree/master/demos/delphi/firemonkey/Expressions) Tests and benchmarks to TExpression class (base class to parse and evaluate expressions).
  
- **BIWeb Server**
 
  Several improvements and fixes in BIWeb server, including the capability of returning HTML dashboards from templates as URL parameter.

  BIWeb can now be used to provide static files under a "public" subfolder structure (folder location is customizable)
  
- **HTML Export**

  * TBIHtmlExport class can now generate "colorized" HTML tables from TDataItem data, using the new Colorizers property.
  
  In addition, TDataColorizers has a new method "TryColorize" that is used also by TBIGrid VCL control to [colorize cells](https://raw.github.com/Steema/BI/master/docs/img/bigrid_colorizer.png).

  
- **JSON Import**

  * Improved conversion of JSON types. 
    When JSON content includes sub-objects of different types (for example, strings and floats), TBIJSON component now detects this situation and "upgrades" types to their best common type.

  * New Hierarchical mode
  
    The default mode of importing JSON content is to try to "flatten" all objects and sub-objects into a single "table" containing all columns that appear in the JSON source, filling Missing (null) cells when no values exist.

    The new Hierarchical property can be set to True to change this import behaviour, where the JSON structure is returned as a "tree" TDataItem structure that reflects the original JSON content, instead of a "flat" table.  
    Each JSON value is returned as a TDataItem following the parent-child of the source. 
    This means JSON arrays are returned also as a TDataItem with all Items being TDataItem objects too.
    
    *Example:*
    
    `var JSON : TBIJSON;
    JSON := TBIJSON.Create;
    JSON.Hierarchical := True;
    Data := JSON.ImportText( AString );`

- **TDataMerge**

  New TDataMerge class (at BI.Data.Merge unit) creates and returns a TDataItem from a list of data items that have the same structure.
  Structure compatibility (data items Kind) is recursively checked before doing the merge.
  
  `Data := TDataMerge.FromDatas( [ One, Two, Three ] );`
  
  TDataMerge SameStructure function is also useful in itself, as it returns if two or more data items are compatible or not.
  
  
- **TBISQL**

  Improvements in both SQL parsing and SQL ToString generation from queries and summaries.
  "select .. from select" and other nested combinations ("select" in expressions) are now better handled.
  
  TDataSelect and TSummary classes ToString method now returns its equivalent SQL representation.

- **Expressions**

  New TDateExpression and TTimeExpression classes, to return just the date or the time of the source expression respectively.
  
  `ShowMessage( TExpression.Evaluate( 'Date("5/6/2016")' ).ToString )`

- **FTP Support**

  Importing data from FTP server is now possible, setting the FTP [server and username properties](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_FTP_import.png) in the Data Manager dialog.
  Files are transferred from the specified server folder and imported into a TDataItem structure.
  Each file is tested to determine its content (CSV, JSON etc) and imported using the appropiate import class.
  
- **TBIChart**

  New support for [Financial Charts](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_Quandl_multistock.png) (Candle Series) for VCL, FMX and also HTML dashboards (using TeeChart.js HTML5).
  
- **TBIComposer**

  Existing TBIVisualizer control in beta 9 has been renamed to TBIComposer.
  The plan is to convert this control to a dashboard template, thus allowing nesting multiple "composers" inside any dashboard panel, and also reducing the quantity of code.  
  Dashboards are much more powerful than TBIVisualizer and functionality is somewhat overlapped.
  
- **TBIControlTree**

  Mostly for debugging purposes, VCL TBIControlTree editor has been improved and also ported to Firemonkey.
  
  `uses BI.VCL.Editor.ControlTree;
  TBIControlTree.Edit( Self, MyControl );`
  
  This dialog is useful to display the inner internals of TBIVisual dashboard controls.
  
- **Other changes**

  * Types TFMXCommon and TVCLCommon have been renamed to TUICommon (same name for both)
  * New TTextArray IndexOf method overload with a new CaseSensitive boolean parameter
  * TDataItem Count property is now read-only. Use Data.Resize (or protected cast) to change Count.
  * New TBIHTMLHelper Color method, returns a TColor in HTML format (#bbggrr)
  * New TDataItems Exists function, equivalent shortcut to `Find( 'foo' ) <> nil`
  * New TDataItems IndexOf function, returns the data item that has AName string parameter
  * New TBISource FromDatas function, returns a single TDataItem from an array of TDataItems
  * New FromURL function in several file import formats (CSV, JSON, XML etc)
  * Fixed TDataSelect queries returning rows from sub-sub-tables (more than one depth level)
  * New TGroupBy IndexOf and Remove methods (for TSummary.By property)
  * New TDataViewer Embedd method for Firemonkey (to insert a data viewer form inside another form)

## 29-Feb-2016  Beta 9

- **Demos**

  New examples have been added to both GitHub repository and TeeBI installer:
  
  * [BIDataSet_Speed](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/BIDataSet_Speed) speed comparison between standard TDataset (TFDMemTable) and TBIDataSet
  
  * [Export](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Export) example to show the different formats to output a TeeBI TDataItem (Csv, Html, JSON, Xml, Excel, PDF)
  
  * [Grid](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Grid) example to show all possibilities of TBIGrid control (grouping, sorting, filtering and searching)
  
  * [Master-Detail](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Master-Detail) demo displays two BIGrid controls with master-detail linked TBIDatasets
  
  * [Speed](https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Speed) demo is a benchmark of several TeeBI features (adding, deleting records, queries, saving, loading, etc)
  
  * [Online BIWeb](https://github.com/Steema/BI/tree/master/demos/online/remote_web) adds a new example to obtain a live HTML5 Javascript chart from BIWeb server
  
- **IDE Help**

  * A compiled TeeBI API reference help file in CHM format has been included.
  This file is a mirror or TeeBI API [online html documentation](http://docs.steebi.com/vclfmx/docs/Docs/API_Reference/Doc/Html/)
  
- **BIWeb server**

  * More information (data size and modified time) is returned when requesting a list of all available data sources:
  
     http://steema.cat:15015/?data&format=.htm
     
  * Logs are now optionally persisted under a customizable ".\Logs" folder.
  Data in BIWeb logs is saved in standard binary TDataItem format, so they can be loaded, queried and analyzed like as any other TDataItem data.
  
  * Public folders can be optionally enabled to use BIWeb server as a file provider (for any kind of file).
  The "root" public folder is named ".\Public" default.
  
  For example, a picture is available under .\public\img folder at Steema's BIWeb server:
  
  http://steema.cat:15015/img/summary_links.png
  
  * Live HTML5 Javascript charts are now returned by BIWeb server, just specifying ".html5" as the requested format in the URL.
  For example: [this link](http://steema.cat:15015/?data=SQLite_demo|%22Order%20Details%22&format=.html5&summary=sum(Quantity);Shippers.ShipperID) returns a summary query as a live chart.
  
  * Grand Totals is a new experimental feature to return totalizers for numeric columns, by adding the "totals=xxx" parameter in the URL, being "xxx" the requested aggregator (for example "sum" or "count")
  
  [Totals Example, last row is the grand total sum](http://steema.cat:15015/?data=SQLite_demo|%22Order%20Details%22&format=.htm&summary=sum(Quantity);Categories.CategoryName&totals=sum)
  
- **TBIGrid**

  Several new features in TBIGrid (VCL only for most of them) to improve the display of TDataItem data objects.
  
  * New local grid menu, accessible by clicking the top-left most grid cell. This menu is optional and offers menu items to control the following features:
  
  * "Alternate" property to paint grid row backgrounds in alternate colors
  
  * "Filters" property to display a top row of edit boxes and combo boxes, one for each grid column, to enable filtering grid rows that match the typed characters.
  
  * Grouping: A grid column can be used to group data based on distinct values of the selected column.
  When grouping, a secondary grid appears side to the main grid with the list of values. Selecting a row filters and displays the data rows that belong to the selected group value. For example show Customers group by Country, filters Customers for each Country.
  
  * "RowNumbers" property to display the row IDs numbers as an extra grid column
  
  * "Search" property to display an edit box to enable highlighting grid cells that match the typed text.
  
  * New "OnDataChange" event that is called whenever the grid is refreshed after filtering or grouping.
  
- **BI.Arrays**

  * For 32bit applications only, all array index access has been converted to use 32bit Integer instead of 64bit.
  (Use of 64bit Integer was not necessary and not optimized for 32bit)
  
  * Added missing "Insert" method for TBooleanArray and TTextArray types.
  
- **HTML Exporting**

  * New FloatFormat property to customize the html output of float values (default is "0.##")
  
- **BI.Data.PDF**

  * Several improvements for better text positioning and more accurate calculation of total PDF pages for large documents, when displaying the "page N of M" annotations.
  
- **BI.Data**

  * New MissingCount function in TMissingData class returns the number of items that are Missing (null)
  
  ```ShowMessage('There are '+IntToStr( MyData.Missing.MissingCount )+' missing items in '+MyData.Name)```
  
  * New Insert method at TDataItem adds a new empty "row" at the specified Index position. It also recursively inserts a new row in sub-tables if they exist.
  
  ```MyData.Insert( 10 )  // adds a new row at position 10 (count starts at zero)```
  
  * Several public fields have been converted to read-only properties to prevent unintented modification.
  
- **BI.Data.Search**

  * New Index property in TDataSearch class enables searching or filtering values from a subset of the total data rows.
  The Index array determines the row IDs to consider when searching or filtering.
  
- **TSQLParser**

  * Parsing SQL now returns TDataItem instances that contain the SQL query as a "Provider" object.
  This means the SQL query is not automatically executed at the time the SQL is parsed, but it will be executed in delay-mode, the first time data is necessary (accessed via LoadData).
  
- **TBIDataSet**

  * New RowNumbers boolean property (default is False), when True the dataset adds a new colum with the effective Row numbers.
  This might be useful to show the row IDs when the dataset is filtered or sorted, where IDs would not be a sequence.
  
  * Editing support
  
  BIDataSet now supports the standard inserting, deleting and modificating mechanism that can be used by code (BIDataSet.Insert, etc) or by manually editing cells on a DBGrid or BIGrid control.
  
- **TDateTimePartExpression**

  * New "DecadeOfYear" enum to return the decade part of a TDateTime (from 1 to 10)

- **Multi-CPU / Multi-Thread support**

  * Initial support for multi-thread (parallel CPU) operations using TDataItem datas.
  Queries can be executed in parallel, for example using System.Thread TParallel.For loops, Futures or Tasks.
  
- **TBIChart**

  * New Fill overload method to pass a TDataCursor object. This enables creating a chart with only the subset of data rows specified in the Cursor Index, and with the optional Cursor sorting.
  
- New TDataMapAsData parameters to optionally return the map values sorted.

- Miscellaneous fixes and speed improvements, specially at Logical and Arithmetic BI Expression classes.
  

  
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

