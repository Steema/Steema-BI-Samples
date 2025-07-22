unit Unit_Main_d7zip;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Persist, BI.Web, BI.DataSource,

  BI.Compression.d7Zip, Vcl.StdCtrls;

type
  TForm35 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    TestStream: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure TestStreamClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form35: TForm35;

implementation

{$R *.dfm}

uses
  System.Diagnostics, BI.DataItem, BI.Compression;

// Returns True is streams A and B are not empty and contain identical bytes
function Same(const A,B:TStream):Boolean;
var tmp : Int64;
begin
  tmp:=A.Size;

  if tmp=0 then
     raise Exception.Create('Error: Stream is empty');


  result:=tmp=B.Size;

  if result then
     if (A is TMemoryStream) and (B is TMemoryStream) then
        result:=CompareMem(TMemoryStream(A).Memory,TMemoryStream(B).Memory,tmp);
end;

const
  Data='TechProducts'; //'R Datasets';

procedure TForm35.Button1Click(Sender: TObject);
const
  BenchTimes=20;

  procedure TestNormal;
  var tmp : TStream;
      t1 : TStopWatch;
      tmpSize,
      tmpTime : Int64;
      t : Integer;
  begin
    t1:=TStopwatch.StartNew;

    for t:=1 to BenchTimes do
        TStore.DataToStream(Data).Free;

    tmpTime:=t1.ElapsedMilliseconds;

    tmp:=TStore.DataToStream(Data);
    try
      tmpSize:=tmp.Size;
    finally
      tmp.Free;
    end;

    Memo1.Lines.Add('Normal size: '+tmpSize.ToString+' time: '+tmpTime.ToString);
    Memo1.Lines.Add('');
  end;

  procedure TestZip(const AName:String);
  var tmp : TStream;
      tmpOut : TStream;
      tmpNormal : TStream;
      t1 : TStopWatch;
      tmpSize,
      tmpTime : Int64;
      t : Integer;
  begin
    // Test zip compression
    t1:=TStopwatch.StartNew;

    // This is to bench the time it takes to save DataItem to a stream, nothing to do with zip or unzip
    for t:=1 to BenchTimes do
        TStore.DataToStream(Data,True).Free;  // <-- just data to a dummy stream

    tmpTime:=t1.ElapsedMilliseconds;

    tmp:=TStore.DataToStream(Data,True);
    tmpSize:=tmp.Size;
    tmp.Free;

    Memo1.Lines.Add('Zipped '+AName+' size: '+tmpSize.ToString+' time: '+tmpTime.ToString+' count: '+BenchTimes.ToString);

    // Now test unzip decompression
    tmp:=TStore.DataToStream(Data,True);
    try
      t1:=TStopwatch.StartNew;

      for t:=1 to BenchTimes do
      begin
        tmp.Position:=0;
        TCompression.DeCompress(tmp,'data').Free;
      end;

      tmpTime:=t1.ElapsedMilliseconds;

      Memo1.Lines.Add('UnZip '+AName+' time: '+tmpTime.ToString);

      tmp.Position:=0;
      tmpOut:=TCompression.DeCompress(tmp,'data');
      try
        tmpNormal:=TStore.DataToStream(Data);
        try
          if Same(tmpNormal,tmpOut) then
             Memo1.Lines.Add('Ok Identical !')
          else
             Memo1.Lines.Add('ERROR: Different content !');
        finally
          tmpNormal.Free;
        end;
      finally
        tmpOut.Free;
      end;

    finally
      tmp.Free;
    end;

    Memo1.Lines.Add('');
  end;

begin
  // Normal
  TestNormal;

  // Zipped

  TCompression.Plugin:=TSystemCompression;
  TestZip('System');

  TCompression.Plugin:=Td7zipCompression;
  TestZip('d7Zip');
end;

procedure TForm35.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm35.TestStreamClick(Sender: TObject);
const
  BenchTimes=1;

  procedure TestZip(const AStream:TStream; const AName:String);
  var tmp : TStream;
      t1 : TStopWatch;
      tmpSize,
      tmpTime : Int64;
      t : Integer;

      tmpOut : TStream;
  begin
    // Test compression speed
    t1:=TStopwatch.StartNew;

    for t:=1 to BenchTimes do
    begin
      AStream.Position:=0;
      tmp:=TCompression.Compress(AStream,'test');
      tmp.Free;
    end;

    tmpTime:=t1.ElapsedMilliseconds;

    AStream.Position:=0;
    tmp:=TCompression.Compress(AStream,'test');

    try
      tmpSize:=tmp.Size;

      Memo1.Lines.Add('Zipped '+AName+' size: '+tmpSize.ToString+' time: '+tmpTime.ToString+' count: '+BenchTimes.ToString);

      // Test de-compression speed
      t1:=TStopwatch.StartNew;

      for t:=1 to BenchTimes do
      begin
        tmp.Position:=0;
        TCompression.DeCompress(tmp,'test').Free;
      end;

      tmpTime:=t1.ElapsedMilliseconds;

      Memo1.Lines.Add('UnZip '+AName+' time: '+tmpTime.ToString);

      tmp.Position:=0;
      tmpOut:=TCompression.DeCompress(tmp,'test');
      try
        if Same(AStream,tmpOut) then
           Memo1.Lines.Add('Ok Identical !')
        else
           Memo1.Lines.Add('ERROR: Different content !');
      finally
        tmpOut.Free;
      end;

    finally
      tmp.Free;
    end;

    Memo1.Lines.Add('');
  end;

var tmp : TMemoryStream;
    tmpData : TDataItem;
begin
  tmp:=TMemoryStream.Create;
  try
    tmpData:=TStore.Load(Data);
    TDataPersistence.Save(tmpData,tmp);

    Memo1.Lines.Add('Data size: '+tmp.Size.ToString);

    TCompression.Plugin:=TSystemCompression;
    TestZip(tmp,'System');

    TCompression.Plugin:=Td7ZipCompression;
    TestZip(tmp,'d7Zip');

  finally
    tmp.Free;
  end;
end;

end.
