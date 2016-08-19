{*********************************************}
{  TeeBI Software Library                     }
{  Speed Benchmark Tests                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Speed;

interface

uses
  System.SysUtils, BI.Data;

type
  TBISpeedTest=class
  private
    procedure Bench(const AName:String; const Times:Integer; const Run:TProc);
  public
    Results : TDataItem;

    Constructor Create;
    Destructor Destroy; override;

    procedure Clear;
    procedure Run;
  end;

implementation

uses
  System.Classes, BI.DataSource, BI.Summary, System.Diagnostics,
  BI.Data.Expressions, BI.DataSet, Data.DB, BI.Persist;

Constructor TBISpeedTest.Create;
begin
  inherited Create;

  // Create test results table
  Results:=TDataItem.Create(True);
  Results.Items.Add('Description',TDataKind.dkText);
  Results.Items.Add('Times',TDataKind.dkInt32);
  Results.Items.Add('Total Milliseconds',TDataKind.dkInt64);
  Results.Items.Add('Times per Second',TDataKind.dkInt32);
  Results.Items.Add('Milliseconds per Time',TDataKind.dkSingle);
end;

Destructor TBISpeedTest.Destroy;
begin
  Results.Free;
  inherited;
end;

procedure TBISpeedTest.Clear;
begin
  Results.Resize(0);
end;

// Execute a test
procedure TBISpeedTest.Bench(const AName:String; const Times:Integer; const Run:TProc);
var t1 : TStopwatch;
    Pos : Integer;
    Elapsed : Int64;
begin
  t1:=TStopwatch.StartNew;

  Run;

  Elapsed:=t1.ElapsedMilliseconds;

  // Add a new row to results table
  Pos:=Results.Count;
  Results.Resize(Pos+1);

  // Fill new row with test name and results
  Results[0].TextData[Pos]:=AName;
  Results[1].Int32Data[Pos]:=Times;
  Results[2].Int64Data[Pos]:=Elapsed;

  // Times per second:
  if Elapsed=0 then
     Results[3].Int32Data[Pos]:=0
  else
     Results[3].Int32Data[Pos]:=Round(1000*Times/Elapsed);

  // Milliseconds per Time:
  Results[4].SingleData[Pos]:=Elapsed/Times;
end;

procedure TBISpeedTest.Run;
const
  SampleNames:Array[0..5] of String=('Sam','Jane','Peter','Carla','Alex','Julie');

  Times_CreateDestroy=100000;
  Times_to_Sort=10;
  Times_to_Save=10;
  Times_to_Load=10;

  Quantity_Add=300000;
  Quantity_Insert=1000;
  Quantity_Delete=10000;
  Quantity_RandomDelete=10000;

var Persons : TDataItem;
    Query : TDataSelect;
    Alex : TDataItem;
    Average : TSummary;
    BySalary : TDataItem;
    SortedSalary : TDataItem;
    PersonsStream : TMemoryStream;
begin
  Bench('Create and Destroy Table (3 columns)',Times_CreateDestroy, procedure
    var Data : TDataItem;
        t : Integer;
    begin
      for t:=1 to Times_CreateDestroy do
      begin
        // Create table structure
        Data:=TDataItem.Create(True);
          Data.Items.Add('ID',TDataKind.dkInt32);
          Data.Items.Add('Name',TDataKind.dkText);
          Data.Items.Add('Price',TDataKind.dkSingle);

        Data.Free;
      end;
    end);

  // Create a sample table
  Persons:=TDataItem.Create(True);
        Persons.Items.Add('ID',TDataKind.dkInt32);
        Persons.Items.Add('Name',TDataKind.dkText);
        Persons.Items.Add('Salary',TDataKind.dkSingle);

  Bench('Add '+Quantity_Add.ToString+' Records',Quantity_Add, procedure
    var t : Integer;
    begin
      // Prepare space
      Persons.Resize(Quantity_Add);

      // Add all rows
      for t:=0 to Quantity_Add-1 do
      begin
        // Fill row
        Persons[0].Int32Data[t]:=t;
        Persons[1].TextData[t]:=SampleNames[t mod High(SampleNames)];
        Persons[2].SingleData[t]:=456.789;
      end;
    end);

  Bench('Insert '+Quantity_Insert.ToString+' Records',Quantity_Insert, procedure
    const AtPosition=5;
    var t : Integer;
    begin
      // Insert rows
      for t:=1 to Quantity_Insert do
      begin
        Persons.Insert(AtPosition);  // <-- new record inserted at "AtPosition" index

        Persons[0].Int32Data[AtPosition]:=t;
        Persons[1].TextData[AtPosition]:=SampleNames[t mod High(SampleNames)];
        Persons[2].SingleData[AtPosition]:=456.789;
      end;
    end);

  Bench('Delete last '+Quantity_Delete.ToString+' Records',Quantity_Delete, procedure
    var t : Integer;
    begin
      for t:=1 to Quantity_Delete do
          Persons.Delete(Persons.Count-1); // <-- delete last record
    end);

  Bench('Delete random '+Quantity_RandomDelete.ToString+' Records',Quantity_RandomDelete, procedure
    var t : Integer;
    begin
      for t:=1 to Quantity_RandomDelete do
          Persons.Delete(Random(Persons.Count)); // <-- delete random record
    end);

  Bench('SQL ('+Persons.Count.ToString+' rows): Select * from Persons where Name="Alex"',1, procedure
    begin
      Query:=TDataSelect.Create(nil);
      try
        Query.Add(Persons); // * all fields

        Query.Filter:=TDataFilter.FromString(Persons,'Name="Alex"');

        // Execute Query and after that, just destroy results
        Alex:=Query.Calculate;
        Alex.Free;

      finally
        Query.Free;
      end;
    end);

  Bench('SQL ('+Persons.Count.ToString+' rows): Select Average(Salary) from Persons group by Name',1, procedure
    begin
      Average:=TSummary.Create(nil);
      try
        Average.AddMeasure(Persons['Salary'],TAggregate.Average);
        Average.AddGroupBy(Persons['Name']);

        // Execute summary and destroy results
        BySalary:=Average.Calculate;
        BySalary.Free;
      finally
        Average.Free;
      end;
    end);

  Bench('SQL ('+Persons.Count.ToString+' rows): Select ID,Salary from Persons order by Salary DESC',1, procedure
    begin
      Query:=TDataSelect.Create(nil);
      try
        Query.Add(Persons['ID']);
        Query.Add(Persons['Salary']);
        Query.SortBy.Add(Persons['Salary'],False);

        // Execute query and destroy results
        SortedSalary:=Query.Calculate;
        SortedSalary.Free;
      finally
        Query.Free;
      end;
    end);

  Bench('TDataSet Traverse to Sum ('+Persons.Count.ToString+' rows)',1, procedure
    var Dataset : TBIDataset;
        Salary : TSingleField;
        Sum : Double;
    begin
      DataSet:=TBIDataset.Create(nil);
      try
        DataSet.Data:=Persons;
        DataSet.Open;

        Salary:=DataSet.FieldByName('Salary') as TSingleField;

        // Do a simple sum of all rows
        Sum:=0;

        DataSet.First;

        while not DataSet.Eof do
        begin
          Sum:=Sum+Salary.Value;
          DataSet.Next;
        end;

      finally
        DataSet.Free;
      end;
    end);

  Bench('Sorting '+Persons.Count.ToString+' rows',Times_to_Sort, procedure
    var t : Integer;
    begin
      for t:=1 to Times_to_Sort do
      begin
        // Sort by text
        Persons.SortBy(Persons['Name']);

        // Sort again by float
        Persons.SortBy(Persons['Salary']);
      end;
    end);

  Bench('Save table ('+Persons.Count.ToString+' rows) to stream',Times_to_Save, procedure
    var t : Integer;
        tmp : TMemoryStream;
    begin
      for t:=1 to Times_to_Save do
      begin
        // Save Persons data to a stream
        tmp:=TMemoryStream.Create;
        try
          TDataItemPersistence.Save(Persons,tmp);
        finally
          tmp.Free; // destroy stream
        end;
      end;
    end);

  // Load back to a temporary stream

  PersonsStream:=TMemoryStream.Create;
  try
    // Save Persons data to stream
    TDataItemPersistence.Save(Persons,PersonsStream);

    Bench('Load table ('+Persons.Count.ToString+' rows) from stream',Times_to_Load, procedure
      var t : Integer;
          tmpData : TDataItem;
      begin
        for t:=1 to Times_to_Load do
        begin
          // Reset stream to start
          PersonsStream.Position:=0;

          // Load data from stream
          tmpData:=TDataItemPersistence.Load(PersonsStream);

          tmpData.Free;
        end;
      end);
  finally
    PersonsStream.Free;
  end;

  Persons.Free;
end;

end.
