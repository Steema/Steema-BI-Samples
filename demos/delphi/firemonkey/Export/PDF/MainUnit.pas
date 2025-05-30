{*********************************************}
{  TeeBI Software Library                     }
{  PDF Export Example                         }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainUnit;

interface

// This example generates a PDF file from a TDataItem and provides controls
// to change several properties of the PDF output.

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  FMXTee.Constants,

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$ELSE}
  // TeeChart Lite !

  {$MESSAGE 'TeeChart Pro version is necessary to compile this example'}

  // WARNING:

  (*
    This example TeeBI_PDF_Export can only be compiled on a RAD Studio that has
    installed the "Pro" version of TeeChart.

    The FMXBI.PDF and VLCBI.PDF units require one unit that is only present in
    the "Pro" version of TeeChart  (VCLTee.TeePDFCanvas / FMXTee.Canvas.PDF)
  *)

  {$ENDIF}

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.WebBrowser, FMXBI.Grid,
  FMX.TabControl, FMX.Layouts, BI.Persist, FMXBI.PDF,
  FMXTee.Canvas.PDF, FMX.ListBox, FMXBI.DataControl;

type
  TMainPDF = class(TForm)
    Layout1: TLayout;
    TabControl1: TTabControl;
    TabGrid: TTabItem;
    TabPDF: TTabItem;
    BIGrid1: TBIGrid;
    WebBrowserPDF: TWebBrowser;
    BCreatePDF: TButton;
    CBAuto: TCheckBox;
    CBPageNumbers: TCheckBox;
    CBGridLines: TCheckBox;
    CBHeaderBack: TCheckBox;
    CBAlternate: TCheckBox;
    CBAlign: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BCreatePDFClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabGridClick(Sender: TObject);
  private
    { Private declarations }

    procedure PreparePDF(const PDF:TBIPDFExport);
  public
    { Public declarations }
  end;

var
  MainPDF: TMainPDF;

implementation

{$R *.fmx}

uses
  System.IOUtils, BI.DataItem;

const
  BrushKindSolid:TBrushKind=TBrushKind.{$IF CompilerVersion<26}bkSolid{$ELSE}Solid{$ENDIF};

procedure TMainPDF.PreparePDF(const PDF:TBIPDFExport);
begin
  // Header font
  PDF.Header.Font.Name:='Courier New';

  // Fill header background ?
  if CBHeaderBack.IsChecked then
  begin
    PDF.Header.Font.Color:=TAlphaColors.White;
    PDF.Header.Font.SizeFloat:=10;
    PDF.Header.Brush.Style:=BrushKindSolid;
    PDF.Header.Brush.Color:=TAlphaColors.Green;
  end;

  // Rows font size
  PDF.RowFormat.Font.SizeFloat:=8;

  if CBAlternate.IsChecked then
  begin
    PDF.RowFormat.Brush.Style:=BrushKindSolid;

    // Paint rows in alternate background
    PDF.RowFormat.Alternate.Visible:=True;
  end;

  // Global horizontal Alignment
  case CBAlign.ItemIndex of
    0 : PDF.Alignment:=TAlignment.Left;
    1 : PDF.Alignment:=TAlignment.Center;
  else
    PDF.Alignment:=TAlignment.Right;
  end;

  // Footer
  PDF.Footer.Text.Clear;
  PDF.Footer.Text.Add('This is a footer text');
  PDF.Footer.Font.Color:=TAlphaColors.Navy;
  PDF.Footer.Alignment:=TAlignment.Right;

  // Font Size automatic ? (auto-fit all items)
  PDF.AutoWidth:=CBAuto.IsChecked;

  // Document Page properties
  PDF.Page.Size:=TPDFPageSize.psA4;
  PDF.Page.Orientation:=TPDFPageOrientation.poPortrait;

  // Page Numbers
  PDF.Page.Numbering.Visible:=CBPageNumbers.IsChecked;
  PDF.Page.Numbering.Font.Style:=[TFontStyle.fsItalic];

  // Grid lines
  PDF.GridLines.Horizontal.Visible:=CBGridLines.IsChecked;
  PDF.GridLines.Vertical.Visible:=CBGridLines.IsChecked;
end;

procedure TMainPDF.TabGridClick(Sender: TObject);
begin
  Layout1.Enabled := true;
end;

procedure TMainPDF.BCreatePDFClick(Sender: TObject);
var tmp : TBIPDFExport;
    tmpFile : String;
begin
  Layout1.Enabled := false;

  // Create a PDF Export object
  tmp:=TBIPDFExport.Create;
  try
    PreparePDF(tmp);

    // Set Data item
    tmp.Data:=BIGrid1.Data;

    // Temporary file name to save PDF
    tmpFile:=TPath.Combine(TPath.GetTempPath,'deleteme.pdf');

    if TFile.Exists(tmpFile) then
       TFile.Delete(tmpFile);


    // Just in case, release browser locked pdf file, before saving it again
    WebBrowserPDF.Navigate('about:blank');
    WebBrowserPDF.FinishLoading;

    // Let some time to web browser to unlock the file
    //Sleep(500);

    tmp.SaveToFile(tmpFile);

    // Show PDF in web browser control
    WebBrowserPDF.Navigate(tmpFile);
    TabControl1.ActiveTab := TabPDF;
  finally
    tmp.Free;
  end;
end;

procedure TMainPDF.FormCreate(Sender: TObject);
begin
  // Load sample data
  BIGrid1.Data:=TStore.Load('BISamples','SQLite_Demo')['Products'];

  // Go directly to PDF preview
  TabControl1.ActiveTab:=TabPDF;
  BCreatePDFClick(Self);
end;

procedure TMainPDF.FormShow(Sender: TObject);
begin
  if TUICommon.AutoTest then
     Close;
end;

initialization
ReportMemoryLeaksOnShutdown := True;
finalization
CheckSynchronize;
end.
