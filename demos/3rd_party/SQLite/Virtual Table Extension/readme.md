## SQLite extension for TeeBI

[SQLite can be downloaded here](https://www.sqlite.org/download.html)

This project can be compiled using Delphi to create a DLL that SQLite loads as an extension.

Once the extension is loaded, you can then use SQLite code to access TeeBI data:

```
.load ./teebi
CREATE VIRTUAL TABLE temp.t1 USING teebi(data='DBDemos.Customer');
SELECT * FROM t1;
```

Data can also be loaded from *.bi and *.databi files directly:

```
CREATE VIRTUAL TABLE temp.t1 USING teebi(file='c:\BI\SampleData\DBDemos.bi', data='Customer');
```


An extra optional "store" parameter can be used to select your desired folder or BI web remote:

```
CREATE VIRTUAL TABLE temp.t1 USING teebi(store='BISamples', data='DBDemos.Customer');
```

If no store is specified, the default one is used (if configured).
In Windows this is at the registry key: 

HKEY_CURRENT_USER\Software\Steema Software\TeeBI\Store

