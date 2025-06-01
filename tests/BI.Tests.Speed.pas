{*********************************************}
{  TeeBI Software Library                     }
{  Speed Benchmark Tests                      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tests.Speed;

interface

uses
  System.SysUtils, BI.DataItem;

type
  TBISpeedTest=class
  private
    procedure Bench(const AName:String; const Times:Integer; const Run:TProc); overload;

    procedure Bench(const ANames:Array of String;
                    const ATimes:Array of Integer;
                    const Runs:Array of TProc); overload;

    procedure SQL1_Proc;
    procedure SQL2_Proc;
    procedure SQL3_Proc;
  public
    Parallel_SQL : Boolean;

    Persons : TDataItem;

    Results : TDataItem;

    Constructor Create;
    Destructor Destroy; override;

    procedure Clear;
    procedure Run;
  end;

  TNamePattern = (npRandom, npSorted, npReverseSorted, npDuplicateCycle);
  TIDPattern = (ipSequential, ipReverse, ipRandom);
  TSalaryPattern = (spConstant, spRandom);
  TDateTimePattern = (dpRandom, dpConstant); // Added for optional DateTime

implementation

uses
  System.Classes, System.Threading, SyncObjs, System.Math, // Added System.Math

  BI.DataSource, BI.Summary, System.Diagnostics,
  BI.Expressions, BI.DataSet, Data.DB, BI.Persist;

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

procedure TBISpeedTest.Bench(const ANames: array of String;
  const ATimes: array of Integer; const Runs: array of TProc);

type
  TTaskInfo=record Name:String; Times:Integer; Proc:TProc; end;

var t : Integer;
    tmpCount : Integer;

    tmp : Array of TTaskInfo;
begin
  // This tmp array is just to capture the params:
  tmpCount:=Length(ANames);
  SetLength(tmp,tmpCount);

  for t:=0 to tmpCount-1 do
  begin
    tmp[t].Name:=ANames[t];
    tmp[t].Times:=ATimes[t];
    tmp[t].Proc:=Runs[t];
  end;

  // Loop all benchmarks in parallel (using multiple CPU logical cores)
  TParallel.&For(0,High(ANames),procedure(Index:Integer)
  begin
    Bench(tmp[Index].Name,tmp[Index].Times,tmp[Index].Proc);
  end);
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
    tmp : TCriticalSection;
begin
  t1:=TStopwatch.StartNew;

  Run;

  Elapsed:=t1.ElapsedMilliseconds;

  tmp:=TCriticalSection.Create;
  try
    tmp.Enter;

    // Add a new row to results table
    Pos:=Results.Count;
    Results.Resize(Pos+1);

    tmp.Leave;
  finally
    tmp.Free;
  end;

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

procedure TBISpeedTest.SQL1_Proc;
var Query : TDataSelect;
    Alex : TDataItem;
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
end;

procedure TBISpeedTest.SQL2_Proc;
var Average : TSummary;
    BySalary : TDataItem;
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
end;

procedure TBISpeedTest.SQL3_Proc;
var Query : TDataSelect;
    SortedSalary : TDataItem;
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
end;

procedure TBISpeedTest.Run;

  function RowCount:String;
  begin
    result:='SQL ('+Persons.Count.ToString+' rows): ';
  end;

  function SQL1:String;
  begin
    result:=RowCount+' Select * from Persons where Name="Alex"';
  end;

  function SQL2:String;
  begin
    result:=RowCount+' Select Average(Salary) from Persons group by Name';
  end;

  function SQL3:String;
  begin
    result:=RowCount+' Select ID,Salary from Persons order by Salary DESC';
  end;

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

var PersonsStream : TMemoryStream;
    NumTestRecords: Integer = 100000; // For new specific sort tests
begin
  Randomize; // Called once at the beginning of Run

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
        Persons[0].Int32Data[t]:=Quantity_Add - 1 - t; // Reverse order IDs
        Persons[1].TextData[t]:=SampleNames[t mod High(SampleNames)];
        Persons[2].SingleData[t]:=System.Random * 100000; // Random salaries
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

  if Parallel_SQL then
     Bench([SQL1,SQL2,SQL3],
           [1,1,1],
           [SQL1_Proc,SQL2_Proc,SQL3_Proc])
  else
  begin
    Bench(SQL1,1,SQL1_Proc);
    Bench(SQL2,1,SQL2_Proc);
    Bench(SQL3,1,SQL3_Proc);
  end;

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

  // --- New Specific Sorting Benchmarks ---
  var TestData: TDataItem;

  // Text Sorting Scenarios
  TestData := CreatePersonsVariant('RandomText', NumTestRecords, npRandom, ipSequential, spConstant, dpConstant); // dpConstant for this set
  try
    Bench('Sort ' + IntToStr(NumTestRecords) + ' Random Names', Times_to_Sort, procedure
    var i: Integer;
    begin
      for i := 1 to Times_to_Sort do
        TestData.SortBy(TestData['Name']);
    end);
  finally
    TestData.Free;
  end;

  TestData := CreatePersonsVariant('SortedText', NumTestRecords, npSorted, ipSequential, spConstant, dpConstant);
  try
    Bench('Sort ' + IntToStr(NumTestRecords) + ' Sorted Names', Times_to_Sort, procedure
    var i: Integer;
    begin
      for i := 1 to Times_to_Sort do
        TestData.SortBy(TestData['Name']);
    end);
  finally
    TestData.Free;
  end;

  TestData := CreatePersonsVariant('ReverseText', NumTestRecords, npReverseSorted, ipSequential, spConstant, dpConstant);
  try
    Bench('Sort ' + IntToStr(NumTestRecords) + ' Reverse Sorted Names', Times_to_Sort, procedure
    var i: Integer;
    begin
      for i := 1 to Times_to_Sort do
        TestData.SortBy(TestData['Name']);
    end);
  finally
    TestData.Free;
  end;

  // Using the main 'Persons' table for duplicate name sort.
  // Its 'Name' field is already populated with SampleNames (duplicates)
  // Its 'ID' is reverse, 'Salary' is random due to earlier modification.
  Bench('Sort ' + Persons.Count.ToString + ' Duplicate Names (Original Table)', Times_to_Sort, procedure
  var i: Integer;
  begin
    for i := 1 to Times_to_Sort do
      Persons.SortBy(Persons['Name']);
  end);

  // Integer Sorting Scenario (using main Persons table with modified ID population - reverse sorted)
  Bench('Sort ' + Persons.Count.ToString + ' Reverse IDs (Original Table)', Times_to_Sort, procedure
  var i: Integer;
  begin
    for i := 1 to Times_to_Sort do
      Persons.SortBy(Persons['ID']); // ID is already reverse sorted
  end);

  // Float Sorting Scenario (using main Persons table with modified Salary population - random)
  Bench('Sort ' + Persons.Count.ToString + ' Random Salaries (Original Table)', Times_to_Sort, procedure
  var i: Integer;
  begin
    for i := 1 to Times_to_Sort do
      Persons.SortBy(Persons['Salary']);
  end);

  // DateTime Sorting Scenario
  TestData := CreatePersonsVariant('RandomDateTime', NumTestRecords, npRandom, ipSequential, spConstant, dpRandom); // Using dpRandom for BirthDate
  try
    Bench('Sort ' + IntToStr(NumTestRecords) + ' Random BirthDates', Times_to_Sort, procedure
    var i: Integer;
    begin
      for i := 1 to Times_to_Sort do
        TestData.SortBy(TestData['BirthDate']);
    end);
  finally
    TestData.Free;
  end;
  // --- End of New Specific Sorting Benchmarks ---

  Bench('Sorting '+Persons.Count.ToString+' rows (Original Combined)',Times_to_Sort, procedure // Renamed slightly for clarity
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

function TBISpeedTest.CreatePersonsVariant(const NameSuffix: String; NumRecords: Integer; NamePattern: TNamePattern; IDPattern: TIDPattern; SalaryPattern: TSalaryPattern; DateTimePattern: TDateTimePattern = dpRandom): TDataItem;
var
  t: Integer;
  SampleNames:Array[0..5] of String=('Sam','Jane','Peter','Carla','Alex','Julie'); // Local for this function
begin
  result := TDataItem.Create(True);
  result.Items.Add('ID', TDataKind.dkInt32);
  result.Items.Add('Name', TDataKind.dkText);
  result.Items.Add('Salary', TDataKind.dkSingle);
  result.Items.Add('BirthDate', TDataKind.dkDateTime); // Added BirthDate column

  result.Resize(NumRecords);

  for t := 0 to NumRecords - 1 do
  begin
    // ID Pattern
    case IDPattern of
      ipSequential: result[0].Int32Data[t] := t;
      ipReverse:    result[0].Int32Data[t] := NumRecords - 1 - t;
      ipRandom:     result[0].Int32Data[t] := Random(NumRecords * 5); // Allow some duplicates, range can be adjusted
    end;

    // Name Pattern
    case NamePattern of
      npRandom:         result[1].TextData[t] := NameSuffix + '_Rnd' + IntToStr(Random(NumRecords*10)); // Increased randomness pool
      npSorted:         result[1].TextData[t] := NameSuffix + '_' + Format('%.6d', [t]);
      npReverseSorted:  result[1].TextData[t] := NameSuffix + '_' + Format('%.6d', [NumRecords - 1 - t]);
      npDuplicateCycle: result[1].TextData[t] := SampleNames[t mod Length(SampleNames)];
    end;

    // Salary Pattern
    case SalaryPattern of
      spConstant: result[2].SingleData[t] := 1234.56 + IntToSingle(t mod 100); // Slight variation for spConstant
      spRandom:   result[2].SingleData[t] := System.Random * 100000.0;
    end;

    // DateTime Pattern
    case DateTimePattern of
      dpRandom:   result[3].DateTimeData[t] := Now - Random(365 * 60) - Random(24*60*60*1000) + (System.Random / 1000.0); // Random date in last 60 years, random time
      dpConstant: result[3].DateTimeData[t] := EncodeDate(2000, 1, 1) + EncodeTime(12,0,0,0);
    end;
  end;
end;

end.
