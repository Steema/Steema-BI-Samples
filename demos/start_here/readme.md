# TeeBI minimal demo

![TeeBI Start Here demo](https://raw.github.com/Steema/BI/master/demos/start_here/TeeBI_StartHere.png)

This demo showcases TeeBI with a very small code that:

1) Imports a file into a TDataItem class

```
Data1 := TBICSV.FromFile('products.csv')
```

2) Performs a basic summary query

```
BIQuery1.Parse(Data1, 'Sum(Stock) group by Category, Color')
```

3) Displays the results at a BIGrid and BIChart

```
Data2 := BIQuery1.Calculate;  // Run the query
BIGrid1.Data  := Data2;     // Show results at Grid
BIChart1.Data := Data2;    // Show results at Chart
```



