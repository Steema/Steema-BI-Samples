###Example using TDataRank methods

Calculate "rankings" of TDataItem values similar to SQL "rank over partition" query clause.

In this example, the "Rank" field is calculated using the "Happiness" field values, grouped by each different "Year".

The syntax to create this Rank field is:

```delphi
uses BI.Data.Rank;
TDataRank.From(MyTable, [MyTable['Year']], MyTable['Happiness'] );
```

![Rankings in BIGrid](https://raw.githubusercontent.com/Steema/BI/master/docs/img/Data_Rankings.png)
