{*********************************************}
{  TeeBI Software Library                     }
{  Speed test TBIDataset vs FireDAC           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_BIDataSet_vs_FireDAC;

interface

{
 This example compares the speed of a TBIDataset component with a
 FireDAC memory table component (TFDMemTable).

 Both datasets are:

 1) Created with a single "double" float field
 2) Filled with a big quantity of records (10 million by default)
 3) Traversed to calculate a total sum
 4) Saved to a file.

 Benchmark:

 Using RAD Studio Seattle Release mode, on a Windows 10 machine
 with an i7 4770 cpu, total time results:

 32bit:
    FDMemTable : 27.5 seconds
    BIDataset  :  1.8 seconds

 64bit:
    FDMemTable : 21.7 seconds
    BIDataset  :  1.4 seconds

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Diagnostics, DB, Vcl.StdCtrls,

  // FireDAC units
  FireDAC.Comp.Client, FireDAC.Stan.Intf,

  {$IF CompilerVersion>26}
  FireDAC.Stan.StorageBin,
  {$ENDIF}

  // TeeBI units
  BI.Data, BI.DataSet, BI.Persist, BI.DataSource, Vcl.ExtCtrls;

type
  TDatasetSpeed = class(TForm)
    BTestFireDAC: TButton;
    BTestBIDataset: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    BIDataset1: TBIDataset;
    ETemp: TEdit;
    Label2: TLabel;
    RGLoopMode: TRadioGroup;
    procedure BTestFireDACClick(Sender: TObject);
    procedure BTestBIDatasetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }

    Samples : Integer;

    {$IF CompilerVersion>26}
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    {$ENDIF}

    procedure AddSamples(const Data:TDataSet);
    procedure AddTime(const ACaption:String; const Elapsed:Int64);
    procedure Loop(const Data:TDataSet); overload;
    procedure Loop(const Data:TDataItem); overload;
    function SampleData:TDataItem;
  public
    { Public declarations }
  end;

var
  DatasetSpeed: TDatasetSpeed;

implementation

{$R *.dfm}

uses
  System.IOUtils;

// Log elapsed time to Memo1
procedure TDatasetSpeed.AddTime(const ACaption:String; const Elapsed:Int64);
begin
  Memo1.Lines.Add(ACaption+': '+Elapsed.ToString+' msec');
end;

// Add records with sample data
procedure TDatasetSpeed.AddSamples(const Data:TDataSet);
var t : Integer;
    t1 : TStopwatch;
begin
  t1:=TStopwatch.StartNew;

  // One Float field
  Data.FieldDefs.Add('A',TFieldType.ftFloat);

  Data.Open;
  Data.DisableControls;
  try
    // Add all records
    for t:=0 to Samples-1 do
        Data.AppendRecord([t]);
  finally
    Data.EnableControls;
  end;

  AddTime('Adding data',t1.ElapsedMilliseconds);
end;

// Traverse a Dataset, adding all records float field value
procedure TDatasetSpeed.Loop(const Data:TDataSet);
var Sum : Double;
    t1 : TStopwatch;
    A : TFloatField;
begin
  t1:=TStopwatch.StartNew;

  Data.DisableControls;
  try
   Sum:=0;

   A:=Data.Fields[0] as TFloatField;

   Data.First;

   while not Data.Eof do
   begin
     // Add field value
     Sum:=Sum+A.Value;

     Data.Next;
   end;

  finally
    Data.EnableControls;
  end;

  AddTime('Loop',t1.ElapsedMilliseconds);
end;

procedure TDatasetSpeed.BTestFireDACClick(Sender: TObject);
var Data : TFDMemTable;
    Total,
    Saving : TStopWatch;
begin
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Test: TFDMemTable');

  Total:=TStopwatch.StartNew;

  Data:=TFDMemTable.Create(Self);
  try
    AddSamples(Data);
    Loop(Data);

    Saving:=TStopwatch.StartNew;
    Data.SaveToFile(ETemp.Text+'\fdmem.dat',sfBinary);
    AddTime('Saving',Saving.ElapsedMilliseconds);
  finally
    Data.Free;
  end;

  AddTime('Total time',Total.ElapsedMilliseconds);
end;

// Create a new TDataItem with a float field, and add rows
function TDatasetSpeed.SampleData:TDataItem;
var t : Integer;
    t1 : TStopwatch;
    A : TDataItem;
begin
  t1:=TStopwatch.StartNew;

  // Create Data
  result:=TDataItem.Create(True);

  // Add one float field
  A:=result.Items.Add('A',TDataKind.dkDouble);

  // Resize data
  result.Resize(Samples);

  // Set all rows sample values
  for t:=0 to Samples-1 do
      A.DoubleData[t]:=t;

  AddTime('Adding data',t1.ElapsedMilliseconds);
end;

procedure TDatasetSpeed.BTestBIDatasetClick(Sender: TObject);
var Total,
    Saving : TStopWatch;
begin
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Test: TBIDataSet');

  Total:=TStopwatch.StartNew;

  BIDataset1.Data:=SampleData;
  BIDataset1.Open;

  if RGLoopMode.ItemIndex=0 then
     Loop(BIDataset1)
  else
     Loop(BIDataset1.Data);

  Saving:=TStopwatch.StartNew;

  // Save data to file
  TDataItemPersistence.Save(BIDataset1.Data,ETemp.Text+'\bi.dat');

  AddTime('Saving',Saving.ElapsedMilliseconds);

  AddTime('Total time',Total.ElapsedMilliseconds);
end;

procedure TDatasetSpeed.Edit1Change(Sender: TObject);
begin
  Samples:=StrToInt(Edit1.Text);
end;

procedure TDatasetSpeed.FormCreate(Sender: TObject);
begin
  {$IF CompilerVersion>26}
  FDStanStorageBinLink1:=TFDStanStorageBinLink.Create(Self);
  {$ENDIF}

  ETemp.Text:=TPath.GetTempPath;

  Memo1.Clear;
  Samples:=StrToInt(Edit1.Text);
end;

// Loop a TDataItem, adding all rows float value
procedure TDatasetSpeed.Loop(const Data: TDataItem);
var Sum : Double;
    t1 : TStopwatch;
    A : TDataItem;
    t : Integer;
begin
  t1:=TStopwatch.StartNew;

  Sum:=0;

  // Obtain alias "A" to first field
  A:=Data.Items[0];

  // Sum all rows
  for t:=0 to Data.Count-1 do
      Sum:=Sum+A.DoubleData[t];

  AddTime('Loop',t1.ElapsedMilliseconds);
end;

end.
