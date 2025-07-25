## Geo Database demo

This demo uses TeeBI Geographic database features, together with TeeChart Pro World maps.

### Click to watch the Youtube video:

[![Watch the video](https://raw.github.com/Steema/BI/master/demos/delphi/Geographic/USA_Counties/TeeBI_USA_Counties_demo.png)](https://youtu.be/WBMO9sXZzKQ)

A example TDataItem table, containing Education information per USA county, is connected to a FIPS (USA County code) table inside the TeeBI default Geo Database.

TeeBI Chart automatically detects this field and uses the TeeChart USA Counties map to display the query results.

```pascal
  // Init the Geographic database
  TGeo.Check;

  // Load the example data
  Education:=TStore.Load('BISamples','US Education 1970-2014 by counties')['USA_Counties_Education'];

  // Setup the query
  BIQuery1.Clear;

  BIQuery1.Dimensions.Add(Education['FIPS Code']);
  BIQuery1.Dimensions.Add(EducationField);

  // Run the query and display it at grid and chart
  BIGrid1.Data:=BIQuery1.Calculate;
  BIChart1.Data:=BIGrid1.Data;

  // Cosmetics
  BIChart1.Options.Legend:=TBIChartLegend.Hide;
  
```

### The Geo Database 

It contains a big quantity of countries with all their related administrative sub-divisions, that work together with TeeChart Pro World maps:


![](https://raw.github.com/Steema/BI/master/docs/img/geo_database2.png)
