{*********************************************}
{  TeeBI Software Library                     }
{  Export Demo                                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.DBCtrls, Vcl.ComCtrls, VCLBI.DataManager, BI.Persist, BI.DataItem,

  // Determine if we have TeeChart "Pro" version (necessary for PDF exporting)

  {$IFDEF FMX}
  FMXTee.Constants,
  {$ELSE}
  VCLTee.TeeConst,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}

  {$ENDIF}

  {$IFDEF TEEPRO}
  VCLBI.PDF,
  {$ENDIF}

  ShDocVW, Clipbrd, ShellAPI, VCLBI.DataControl, VCLBI.Grid;

type
  TExportDemo = class(TForm)
    PageControl1: TPageControl;
    TabGrid: TTabSheet;
    DBNavigator1: TDBNavigator;
    BIGrid1: TBIGrid;
    TabExport: TTabSheet;
    Memo1: TMemo;
    Panel2: TPanel;
    RGExport: TRadioGroup;
    Button3: TButton;
    TabHTML: TTabSheet;
    Panel1: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    LRows: TLabel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Button1: TButton;
    TabPDF: TTabSheet;
    Panel5: TPanel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RGExportClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    WebBrowser1: TWebBrowser;

    procedure ExportData;
    procedure ExportHTML;
    procedure SelectedData(Sender: TObject);
    procedure SetNumberOfRows;
    procedure TryCreateWebBrowser;
  public
    { Public declarations }
  end;

var
  ExportDemo: TExportDemo;

implementation

{$R *.dfm}

uses
  BI.Json, BI.XmlData, BI.HTML, BI.CSV, BI.DataSource,
  WebBrowser_Load, System.IOUtils;

procedure TExportDemo.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabGrid;

  // Load "Customers" table into Grid
  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Customers'];

  // Embedd a Data Manager dialog at left panel
  TDataManager.Embed(Self,Panel1,TDataManagerEmbedMode.Choose,'BISamples',BIGrid1.Data).OnSelect:=SelectedData;

  // Set Navigator control source
  DBNavigator1.DataSource:=BIGrid1.DataSource;

  SetNumberOfRows;
end;

procedure TExportDemo.SetNumberOfRows;
begin
  if BIGrid1.Data=nil then
     LRows.Caption:='?'
  else
  if (BIGrid1.Data.Kind=TDataKind.dkUnknown) and (not BIGrid1.Data.AsTable) then
     LRows.Caption:=IntToStr(BIGrid1.Data.Items.Count)
  else
     LRows.Caption:=IntToStr(BIGrid1.Data.Count);
end;

procedure TExportDemo.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabExport then
     ExportData
  else
  if PageControl1.ActivePage=TabHTML then
     ExportHTML;
end;

procedure TExportDemo.RGExportClick(Sender: TObject);
begin
  ExportData;
end;

procedure TExportDemo.SelectedData(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager(Sender).SelectedData;

  if tmp<>nil then
  begin
    // Change data
    BIGrid1.Data:=tmp;

    // Try to re-fill export Memo or Web Browser
    PageControl1Change(Self);

    SetNumberOfRows;
  end;
end;

procedure TExportDemo.Button2Click(Sender: TObject);
{$IFDEF TEEPRO}
var TempFile : String;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  TempFile:=TPath.ChangeExtension(TPath.GetTempFileName,'.pdf');

  TBIPDFExport.SaveToFile(BIGrid1.Data,TempFile);

  ShellExecute(Handle,'open',PWideChar(TempFile),'','',SW_SHOWMAXIMIZED);
  {$ENDIF}
end;

procedure TExportDemo.Button3Click(Sender: TObject);
begin
  Clipboard.AsText:=Memo1.Text;
end;

// Returns AData exported to HTML format
function GetHTMLText(const AData:TDataItem):String;
const
  CSS='<style type="text/css"> td {border: 1px solid LightGrey;} '+
      'th {border: 1px solid navy;}'+
      'table {border-collapse: collapse; font-family:Tahoma; font-size:0.8em}</style>';

var E : TBIHTMLExport;
begin
  // Create HTML export object
  E:=TBIHTMLExport.Create;
  try
    // Example settings
    E.Borders:=True;
    E.SortIcons:=False;
    E.FloatFormat:='0.##';

    E.Data:=AData;

    result:='<!DOCTYPE html><html><head>'+CSS+'</head><body>'+E.AsString+'</body></html>';
  finally
    E.Free;
  end;
end;

// Returns AData exported to AClass format
function GetExportText(const AData:TDataItem; const AClass:TBIExportClass):String;
var E : TBIExport;
    tmp : TStringList;
begin
  E:=AClass.Create;
  try
    E.Data:=AData;

    tmp:=TStringList.Create;
    try
      E.EmitTo(tmp);

      // "tmp" is used here instead of directly EmitTo(Memo1.Lines) for
      // speed reasons (Memo Lines is a very slow TStrings class)

      result:=tmp.Text;
    finally
      tmp.Free;
    end;
  finally
    E.Free;
  end;
end;

procedure TExportDemo.ExportData;
var tmp : String;
begin
  // Export to selected format
  case RGExport.ItemIndex of
   0: tmp:=GetExportText(BIGrid1.Data, TBIJSONExport);
   1: tmp:=GetExportText(BIGrid1.Data, TBIXMLExport);
   2: tmp:=GetExportText(BIGrid1.Data, TBICSVExport);
   3: tmp:=GetHTMLText(BIGrid1.Data);
  else
    tmp:='';
  end;

  Memo1.Text:=tmp;
end;

procedure TExportDemo.ExportHTML;
begin
  TryCreateWebBrowser;

  // Load Web Browser with HTML content
  WBLoadHTML(WebBrowser1,GetHTMLText(BIGrid1.Data));
end;

procedure TExportDemo.TryCreateWebBrowser;
begin
  // Delayed creation of WebBrowser
  if WebBrowser1=nil then
  begin
    WebBrowser1:=TWebBrowser.Create(Self);
    WebBrowser1.Align:=alClient;
    TWinControl(WebBrowser1).Parent:=TabHTML;
  end;
end;

procedure TExportDemo.Button1Click(Sender: TObject);
var tmp : TStringList;
    TempFile : String;
begin
  tmp:=TStringList.Create;
  try
    tmp.Text:=GetHTMLText(BIGrid1.Data);

    TempFile:=TPath.ChangeExtension(TPath.GetTempFileName,'.html');
    tmp.SaveToFile(TempFile);

    ShellExecute(Handle,'open',PWideChar(TempFile),'','',SW_SHOWMAXIMIZED);
  finally
    tmp.Free;
  end;
end;
initialization
ReportMemoryLeaksOnShutdown := True;
finalization
CheckSynchronize;
end.
