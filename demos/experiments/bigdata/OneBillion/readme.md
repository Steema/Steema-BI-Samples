# One Billion cells

## Big quantity of Data and TeeBI

This demo shows TeeBI capabilities with big quantities of data, rows and cells.

- It first creates a default dummy database of One Billion cells (thousand millions).
- Data is saved to a disk file in your TEMP folder: "big_data.bi" (4.5GB) in aprox 4 seconds.


<img width="262" height="383" alt="image" src="https://github.com/user-attachments/assets/c71c3833-62d6-413e-9b9d-da724493c90b" />

```delphi
var BigData : TDataItem;
    TDataItemPersistence.Save(BigData,'c:\temp\big_data.bi');
```
Once data has been created it can be loaded again from disk in aprox 2.5 seconds.

<img width="262" height="296" alt="image" src="https://github.com/user-attachments/assets/303495b7-d94d-42ce-ac47-504475d77d44" />

```delphi
var BigData : TDataItem;
    BigData := TDataItemPersistence.Load('c:\temp\big_data.bi');
```

- The "Query and Visualize" form uses this big data to do some visualizations.

### Queries traversing so many millions of rows are not immediate, of course !
But you can run them in a normal laptop.

<img width="1453" height="695" alt="image" src="https://github.com/user-attachments/assets/dc443141-9c46-469c-b68b-fa1d4c83ad20" />

```delphi
  // Sum the amount of Sales year by year, all rows:

   BIQuery1.Measures.Add(BigData['Sales']['Total'], TAggregate.Sum);
   BIQuery1.Dimensions.Add(BigData['Sales']['Date']).DatePart := TDateTimePart.Year;

   BIComposer1.Data := BIQuery1.Calculate;
   
```


<img width="338" height="459" alt="image" src="https://github.com/user-attachments/assets/67fb1d6b-26df-4370-b206-0350602023b9" />

```delphi
  // Number of Customers per Country

  BIQuery1.Measures.Add(Data['Customers'], TAggregate.Count);
  BIQuery1.Dimensions.Add(TGeo.Country.Name);   

  BIComposer1.Data := BIQuery1.Calculate;
   
```



Notes:
- It is a Windows 64bit executable because more than 3GB of memory are needed.
- Use of charts (free TeeChart or "Pro" version) is optional.
