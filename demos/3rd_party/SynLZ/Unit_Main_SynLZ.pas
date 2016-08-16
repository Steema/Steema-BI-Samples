unit Unit_Main_SynLZ;

interface

// Uncomment to test Snappy 64bit engine

{$IFDEF CPUX64}
{$DEFINE SNAPPY}
{$ENDIF}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Persist, BI.Web, BI.DataSource,

  {$IFDEF SNAPPY}
  BI.Compression.Snappy,
  {$ENDIF}

  BI.Compression.SynZip, Vcl.StdCtrls;

type
  TForm35 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Stream: TButton;
    procedure Button1Click(Sender: TObject);
    procedure StreamClick(Sender: TObject);
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
  System.Diagnostics, BI.Data, BI.Compression;

function Same(const A,B:TStream):Boolean;
var tmp : Int64;
begin
  tmp:=A.Size;

  result:=tmp=B.Size;

  if result then
     if (A is TMemoryStream) and (B is TMemoryStream) then
        result:=CompareMem(TMemoryStream(A).Memory,TMemoryStream(B).Memory,tmp);
end;

const
  Data='R Datasets'; //'TechProducts';

procedure TForm35.Button1Click(Sender: TObject);
const
  BenchTimes=10;

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
    t1:=TStopwatch.StartNew;

    for t:=1 to BenchTimes do
        TStore.DataToStream(Data,True).Free;

    tmpTime:=t1.ElapsedMilliseconds;

    tmp:=TStore.DataToStream(Data,True);
    tmpSize:=tmp.Size;
    tmp.Free;

    Memo1.Lines.Add('Zipped '+AName+' size: '+tmpSize.ToString+' time: '+tmpTime.ToString);

    // Unzip
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

  TCompression.Plugin:=TSynZipCompression;
  TestZip('SynZip');

  {$IFDEF SNAPPY}
  TCompression.Plugin:=TSnappyCompression;
  TestZip('Snappy');
  {$ENDIF}
end;

procedure TForm35.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm35.StreamClick(Sender: TObject);
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

      Memo1.Lines.Add('Zipped '+AName+' size: '+tmpSize.ToString+' time: '+tmpTime.ToString);

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

    TCompression.Plugin:=TSynZipCompression;
    TestZip(tmp,'SynZip');

    {$IFDEF SNAPPY}
    TCompression.Plugin:=TSnappyCompression;
    TestZip(tmp,'Snappy');
    {$ENDIF}

  finally
    tmp.Free;
  end;
end;

end.
