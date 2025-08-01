## TeeBI Composer

```TBIComposer``` is a control (for both VCL and FMX) to automatically display a TDataItem using charts, grids, listboxes, tabs, etc.

Data is investigated and for each "dimension" in the data, best controls are choosen to visualize it.

![](https://github.com/Steema/TeeBI/blob/6e319af71c7fc245b4b9779b433e93c8ae11092c/demos/delphi/Visualization/Composer/TeeBI_Composer_Example_Screenshot.png)

```delphi
var
  Data : TDataItem;
  Summary : TSummary;

procedure TMainForm.ExecuteQuery;
begin
  // Obtain the SQL as text from the query:
  MemoSQL.Text:=TBISQL.From(Summary);

  // Execute the query:
  Data.Free;
  Data:=TDataItem.Create(Summary);

  // Set the query results to grid and composer:
  BIGrid1.BindTo(Data);
  BIComposer1.Data:=Data;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TSampleSummaries.AddExamples(LBTest.Items);  // adds the list of examples
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

procedure TMainForm.LBTestClick(Sender: TObject);
begin
  // Create the query
  Summary.Free;
  Summary:=Samples.CreateSummary(Self,LBTest.ItemIndex);

  Summary.Description:=LBTest.Items[LBTest.ItemIndex];

  ExecuteQuery;

  ButtonQuery.Enabled:=True;
end;

```
