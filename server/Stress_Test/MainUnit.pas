{*********************************************}
{  TeeBI Software Library                     }
{  Web Server Stress Test                     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Diagnostics,

  // Indy
  BI.Web.Indy, IdStack, IdHTTP,

  // Seattle Http
  BI.Web.Net,

  BI.Web, Vcl.ExtCtrls, Vcl.CheckLst;

type
  // Optionally execute different kinds of tests
  TRequests=record
  public
    AsString,
    DataItem,
    Query,
    PNGChart,
    JSON : Boolean;
  end;

  TMainForm = class(TForm)
    BRun: TButton;
    Label1: TLabel;
    UDThreads: TUpDown;
    EThreads: TEdit;
    Label2: TLabel;
    LRequests: TLabel;
    CheckListBox1: TCheckListBox;
    Label3: TLabel;
    EWorkers: TEdit;
    UDWorkers: TUpDown;
    Label4: TLabel;
    LPerSecond: TLabel;
    CBCompression: TCheckBox;
    RGEngine: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure BRunClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure EWorkersChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBCompressionClick(Sender: TObject);
    procedure RGEngineClick(Sender: TObject);
  private
    { Private declarations }

    // Main class to execute web requests
    BIWeb : TBIWebClient;

    // Flag when "Cancel" button is pressed
    DoCancel : Boolean;

    // Counter to display the number of web requests
    NumRequests : Integer;

    // List of optional tests
    Requests : TRequests;

    // Use zip compression over http?
    Compression : TWebCompression;

    // Use Indy HTTP or standard RTL System.Net?
    UseIndy : Boolean;

    procedure DoRequests(const AWeb:TBIWebClient);

    procedure GetCheckedTests;
    procedure ResetUI;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.Threading, System.SyncObjs, BI.DataItem;

const
  Run_Caption='&Run !';
  Cancel_Caption='&Cancel';

procedure TMainForm.CBCompressionClick(Sender: TObject);
begin
  if CBCompression.Checked then
     Compression:=TWebCompression.Yes
  else
     Compression:=TWebCompression.No;
end;

procedure TMainForm.CheckListBox1ClickCheck(Sender: TObject);
begin
  GetCheckedTests;
end;

procedure TMainForm.GetCheckedTests;
begin
  Requests.AsString:=CheckListBox1.Checked[0];
  Requests.DataItem:=CheckListBox1.Checked[1];
  Requests.PNGChart:=CheckListBox1.Checked[2];
  Requests.Query:=CheckListBox1.Checked[3];
  Requests.JSON:=CheckListBox1.Checked[4];
end;

// Execute different kinds of web requests
procedure TMainForm.DoRequests(const AWeb:TBIWebClient);
const
  PNGChart='data=SQLite_demo%7C%22Order%20Details%22&format=.html5&summary=sum(Quantity);Categories.CategoryName';
  SQL='select distinct CategoryID from Products';

var tmp : String;
    tmpData : TDataItem;
    tmpStream : TStream;
begin
  if Requests.AsString then
     tmp:=AWeb.GetData;

  if Requests.DataItem then
  begin
    // Lock problem with delay-load (two requests, one for metadata and another for data):
    // tmpData:=AWeb.GetData('SQLite_Demo',TWebCompression.No);

    tmpData:=AWeb.GetData('data=SQLite_Demo|Customers',True,Compression);
    tmpData.Free;
  end;

  if Requests.PNGChart then
  begin
    if Compression=TWebCompression.Yes then
       tmpStream:=AWeb.GetStream('zip=yes&'+PNGChart)
    else
       tmpStream:=AWeb.GetStream(PNGChart);

    tmpStream.Free;
  end;

  if Requests.Query then
  begin
    tmpData:=AWeb.GetData('data=SQLite_Demo&sql='+SQL,True,Compression);
    tmpData.Free;
  end;

  if Requests.JSON then
     tmp:=AWeb.GetString('data=SQLite_Demo|Categories&format=.json');

  // Add one to total counter to display speed
  TInterlocked.Increment(NumRequests);
end;

procedure TMainForm.EWorkersChange(Sender: TObject);
begin
  // Max internal threads for multi-thread Parallel
  TThreadPool.Current.SetMaxWorkerThreads(UDWorkers.Position);
end;

procedure TMainForm.BRunClick(Sender: TObject);

  // Execute web requests in parallel
  procedure ParallelRequest(const NumThreads:Integer);
  begin
    TParallel.&For(0,NumThreads-1,procedure(Index:Integer)
    begin
      if UseIndy then
      begin
        // Indy raises exceptions that should not stop this test
        try
          DoRequests(BIWeb);
        except
          on E:EIdSocketError do
          begin
          end;

          on Exception do
             raise;
        end;
      end
      else
        DoRequests(BIWeb);

    end);
  end;

var NumThreads,
    OldNum : Integer;
    tmpMsec : Int64;
    t1 : TStopWatch;
begin
  if BRun.Caption=Run_Caption then
  begin
    // Start test

    BRun.Caption:=Cancel_Caption;
    DoCancel:=False;
    EThreads.Enabled:=False;
    UDThreads.Enabled:=False;
    RGEngine.Enabled:=False;

    GetCheckedTests;

    NumRequests:=0;
    OldNum:=0;

    NumThreads:=UDThreads.Position;

    // Loop

    t1:=TStopWatch.StartNew;

    repeat
      if NumThreads>1 then
         ParallelRequest(NumThreads)
      else
         DoRequests(BIWeb);

      tmpMsec:=t1.ElapsedMilliseconds;

      if tmpMsec>1000 then
      begin
        // Every second, display speed (requests per second)

        LRequests.Caption:=NumRequests.ToString;
        LPerSecond.Caption:=FormatFloat('0.##',1000*(NumRequests-OldNum)/tmpMsec);
        Application.ProcessMessages;

        // Reset counter
        OldNum:=NumRequests;
        t1:=TStopWatch.StartNew;
      end;

    until DoCancel;

    // Re-Enable controls after test is finished
    ResetUI;
  end
  else
  if DoCancel then
     ResetUI
  else
     DoCancel:=True;
end;

// Switch http engine (Indy or standard RTL System.Net)
procedure TMainForm.RGEngineClick(Sender: TObject);
begin
  BIWeb.Free;

  UseIndy:=RGEngine.ItemIndex=0;

  if UseIndy then
     TBIHttp.Engine:=TBIIndy
  else
     TBIHttp.Engine:=TBIHttpClient;

  BIWeb:=TBIWebClient.Create('localhost');

  // Just testing special options for this stress test

  if UseIndy then
  begin
    (BIWeb.Http as TBIIndy).Http.ProtocolVersion:=TIdHTTPProtocolVersion.pv1_1;
    (BIWeb.Http as TBIIndy).Http.ConnectTimeout:=0; // <-- For this stress test
  end
  else
  begin
    (BIWeb.Http as TBIHttpClient).Http.UserAgent:='TeeBI Stress Test';
  end;
end;

// Re-Enable controls
procedure TMainForm.ResetUI;
begin
  BRun.Caption:=Run_Caption;
  EThreads.Enabled:=True;
  UDThreads.Enabled:=True;
  RGEngine.Enabled:=True;
end;

// Set flag to stop test loop
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DoCancel:=True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UseIndy:=False;

  CheckListBox1.CheckAll(TCheckBoxState.cbChecked);

  Compression:=TWebCompression.No;

  BRun.Caption:=Run_Caption;

  UDWorkers.Position:=TThreadPool.Current.MaxWorkerThreads;

  RGEngineClick(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Clean-up memory
  BIWeb.Free;
end;

end.
