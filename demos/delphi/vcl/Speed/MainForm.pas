{*********************************************}
{  TeeBI Software Library                     }
{  Speed Benchmark Tests                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.VCL.Grid, BI.Data, BI.DataSource, BI.Summary, BI.Persist;

type
  TFormSpeed = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    BIGrid1: TBIGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    Results : TDataItem;

    procedure Bench(const AName:String; const Times:Integer; const Run:TProc);
    procedure Test;
  public
    { Public declarations }
  end;

var
  FormSpeed: TFormSpeed;

implementation

{$R *.dfm}

uses
  System.Diagnostics;

// Execute all tests
procedure TFormSpeed.Button1Click(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    // Clear results
    Results.Resize(0);

    Test;

    // Show results at Grid
    BIGrid1.RefreshData;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFormSpeed.FormCreate(Sender: TObject);
begin
  // Create test results table
  Results:=TDataItem.Create(True);
  Results.Items.Add('Description',TDataKind.dkText);
  Results.Items.Add('Times',TDataKind.dkInt32);
  Results.Items.Add('Milliseconds',TDataKind.dkInt64);
  Results.Items.Add('Times per Second',TDataKind.dkInt32);

  // Set data to grid
  BIGrid1.Data:=Results;
end;

procedure TFormSpeed.FormDestroy(Sender: TObject);
begin
  // Free results table (to avoid memory leak)
  Results.Free;
end;

// Execute a test
procedure TFormSpeed.Bench(const AName:String; const Times:Integer; const Run:TProc);
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

  if Elapsed=0 then
     Results[3].Int32Data[Pos]:=0
  else
     Results[3].Int32Data[Pos]:=Round(1000*Times/Elapsed);
end;

procedure TFormSpeed.Test;
const
  SampleNames:Array[0..5] of String=('Sam','Jane','Peter','Carla','Alex','Julie');

var Persons : TDataItem;
    Query : TDataSelect;
    Alex : TDataItem;
    Average : TSummary;
    BySalary : TDataItem;
    SortedSalary : TDataItem;
    PersonsStream : TMemoryStream;
begin
  Bench('Create and Destroy Table',100000, procedure
    var Data : TDataItem;
        t : Integer;
    begin
      for t:=1 to 100000 do
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

  Bench('Add Records',100000, procedure
    var t : Integer;
    begin
      // Prepare space
      Persons.Resize(100000);

      // Add all rows
      for t:=1 to 100000 do
      begin
        Persons[0].Int32Data[t]:=t;
        Persons[1].TextData[t]:=SampleNames[t mod High(SampleNames)];
        Persons[2].SingleData[t]:=456.789;
      end;
    end);

  Bench('Delete last Records',10000, procedure
    begin
      while Persons.Count>90000 do
            Persons.Delete(Persons.Count-1);
    end);

  Bench('Delete random Records',10000, procedure
    begin
      while Persons.Count>80000 do
            Persons.Delete(Random(Persons.Count));
    end);

  Bench('SQL: Select * from Persons where Name="Alex"',1, procedure
    begin
      Query:=TDataSelect.Create;
      try
        Query.Add(Persons); // *
        Query.Filter:=TDataFilter.FromString(Persons,'Name="Alex"');

        // Execute Query and destroy results
        Alex:=Query.Calculate;
        Alex.Free;
      finally
        Query.Free;
      end;
    end);

  Bench('SQL: Select Average(Salary) from Persons group by Name',1, procedure
    begin
      Average:=TSummary.Create;
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

  Bench('SQL: Select ID,Salary from Persons order by Salary DESC',1, procedure
    begin
      Query:=TDataSelect.Create;
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

  Bench('Save table to stream',10, procedure
    var t : Integer;
        tmp : TMemoryStream;
    begin
      for t:=0 to 9 do
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

  PersonsStream:=TMemoryStream.Create;
  try
    // Save Persons data to stream
    TDataItemPersistence.Save(Persons,PersonsStream);

    Bench('Load table from stream',10, procedure
      var t : Integer;
          tmpData : TDataItem;
      begin
        for t:=0 to 9 do
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
