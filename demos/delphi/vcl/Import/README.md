This example shows how to import and convert data from different formats into TeeBI classes.

- By code

  Import data from files and text in CSV, JSON, XML, Excel formats and from database components (Connections, any TDataset like ClientDataset, etc)
  
  For example:
  
  `Data:=TBICSV.FromFile('test.csv');`
  
  `Data:=TBICSV.FromStrings(Memo1.Lines);`

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
  
  ```
  Web:=TBIWebClient.Create('steema.cat'); // or: 'localhost'
  try
    Data:=Web.GetMetaData('Acme Database', TWebCompression.No);
  finally
    Web.Free;
  end;
  ```

- From records or object instances using RTTI

  Load custom records or classes into TDataItem:
  
  ```
  var tmp : TTypeProvider<TPerson>;
  tmp:=TTypeProvider<TPerson>.Create;
  
  tmp.Add( JohnDoe );   // <-- any TPerson instance
  tmp.Add( MyPersonArray );  // <-- or List or Collection (any IEnumerable)
  ...
  
  Data:=TDataItem.Create( tmp );
  ```
  
  Note: For a more advanced usage of RTTI with TeeBI, please see the ORM_RTTI demo included with TeeBI installer.
  
