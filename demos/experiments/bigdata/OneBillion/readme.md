# One Billion cells

## Big quantity of Data and TeeBI

This demo shows TeeBI capabilities with big quantities of data, rows and cells.

- It first creates a default dummy database of One Billion cells (thousand millions).
- Data is saved to a disk file in your TEMP folder: "big_data.bi" (4.5GB) in aprox 4 seconds.

![TeeBI Create Big Data One Billion cells](https://raw.github.com/Steema/BI/master/demos/experiments/bigdata/OneBillion/img/TeeBI_OneBillion_bigdata_Create.png)

```delphi
var BigData : TDataItem;
    TDataItemPersistence.Save(BigData,'c:\temp\big_data.bi');
```
Once data has been created it can be loaded again from disk in aprox 2.5 seconds.

![TeeBI Create Big Data One Billion cells](https://raw.github.com/Steema/BI/master/demos/experiments/bigdata/OneBillion/img/TeeBI_OneBillion_bigdata_Load.png)

```delphi
var BigData : TDataItem;
    BigData := TDataItemPersistence.Load('c:\temp\big_data.bi');
```

- The "Query and Visualize" form uses this big data to do some visualizations.

### Queries traversing so many millions of rows are not immediate, of course !
But you can run them in a normal laptop.

![TeeBI Create Big Data One Billion cells](https://raw.github.com/Steema/BI/master/demos/experiments/bigdata/OneBillion/img/TeeBI_OneBillion_Query_Yearly_Sales.png)

```delphi
  // Sum the amount of Sales year by year, all rows:

   BIQuery1.Measures.Add(BigData['Sales']['Total'], TAggregate.Sum);
   BIQuery1.Dimensions.Add(BigData['Sales']['Date']).DatePart := TDateTimePart.Year;

   BIComposer1.Data := BIQuery1.Calculate;
   
```

![TeeBI Create Big Data One Billion cells](https://raw.github.com/Steema/BI/master/demos/experiments/bigdata/OneBillion/img/TeeBI_OneBillion_Query_by_Country.png)


```delphi
  // Number of Customers per Country

  BIQuery1.Measures.Add(Data['Customers'], TAggregate.Count);
  BIQuery1.Dimensions.Add(TGeo.Country.Name);   

  BIComposer1.Data := BIQuery1.Calculate;
   
```



Notes:
- It is a Windows 64bit executable because more than 3GB of memory are needed.
- Use of charts (free TeeChart or "Pro" version) is optional.
