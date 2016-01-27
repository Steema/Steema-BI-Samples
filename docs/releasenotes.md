# TeeBI Release Notes
-------------------

## 22-Dec-2015  Beta 6

-New TBIVisualizer control (VCL and FMX), first beta.
-New TBIVisualizer editor dialog.
-TBICSV can now import 1 billion cells using the 64bit compiler.
-MariaDB database now supported (using the MySQL driver)
-Support for customizing database server port in Data Manager.
-Support for Excel exporting of sub-sub-dataitem tables.
-Binding a TBIDataset to a TBIGrid is now much faster.
-Added "R Datasets" in binary format to "Sample Data" folder.
-Support for Geographical (World maps) charts.
-Initial support for sub-folders in TStore folders.
-Updated documentation and examples with new features.
-Bug fixing and speed optimizations in base TDataItem class.


## 27-Nov-2015  Beta 5

-Encryption mechanism using TurboPack LockBox (optional).
-Summary "Having" filter property (same as SQL).
-Microsoft Excel exporting using TMS Flexcel (optional).
-Support for remote TDataSelect queries in BIWeb http server.
-Multiple expressions are now allowed for SortBy data ordering.
-TBIGrid Colorize properties to fill cell backgrounds based on values.
-Helper method to convert select and summary queries to SQL language.
-Delphi unit generator emits Pascal code from any TDataItem or database.
-FreePascal v3.0 initial support for TeeBI core units.
-Design-time IDE top menu "TeeBI" to access the "Data Manager", etc.
-Updated documentation and examples with new features.
-Bug fixing and speed optimizations, specially for big Summary queries.


## 20-Nov-2015  Beta 4

-New TDataSelect class, implements a big part of SQL language in native Delphi code.
-Cumulative and/or percentage calculations in summaries.
-microOLAP DAC MySQL import support.
-Links between data in different storage locations or www servers.
-Extra data Links can be created at import time using TDataDefinition.SetMasters method.
-Enabled Sorting data reordering the source rows.
-Updated Starting Guide doc with new content in beta4
-Starting Guide doc in Spanish: "Guía de Introducción"
-Convert from queries and summaries to SQL language.
-Percent format for BIDataset numeric columns
-Optional "raw" persistence mode (BI.Streams.pas)
-Fixes and speed optimizations?


## 6-Nov-2015  Beta 3

-Initial support for "R" language and Python Scikit-learn machine-learning algorithms. 
 Several algorithms included.

-"Starting Guide" document has been updated.
-Many fixes and small improvements.
-Design-time debug visualizer for TDataItem objects.
-More demos included.
-TeeBIRecompile automatic recompilation tool improved.?


## 8-Oct-2015 Initial Betas (1 and 2)
