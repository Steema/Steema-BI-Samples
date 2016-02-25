{*********************************************}
{  TeeBI Software Library                     }
{  PDF Export Example                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit MainUnit;

interface

// This example generates a PDF file from a TDataItem and provides controls
// to change several properties of the PDF output.

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.WebBrowser, BI.FMX.Grid,
  FMX.TabControl, FMX.Layouts, BI.Persist, BI.FMX.PDF,
  FMXTee.Canvas.PDF, FMX.ListBox;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainPDF: TMainPDF;

implementation

{$R *.fmx}

uses
  System.IOUtils, BI.Data;

procedure TMainPDF.BCreatePDFClick(Sender: TObject);
var tmp : TBIPDFExport;
    tmpFile : String;
begin
  // Create a PDF Export object
  tmp:=TBIPDFExport.Create;
  try
    // Header font
    tmp.Header.Font.Name:='Courier New';

    // Fill header background ?
    if CBHeaderBack.IsChecked then
    begin
      tmp.Header.Font.Color:=TAlphaColors.White;
      tmp.Header.Font.SizeFloat:=10;
      tmp.Header.Brush.Style:=TBrushKind.Solid;
      tmp.Header.Brush.Color:=TAlphaColors.Green;
    end;

    // Rows font size
    tmp.RowFormat.Font.SizeFloat:=8;

    if CBAlternate.IsChecked then
    begin
      tmp.RowFormat.Brush.Style:=TBrushKind.Solid;

      // Paint rows in alternate background
      tmp.RowFormat.Alternate.Visible:=True;
    end;

    // Global horizontal Alignment
    case CBAlign.ItemIndex of
      0 : tmp.Alignment:=TAlignment.Left;
      1 : tmp.Alignment:=TAlignment.Center;
    else
      tmp.Alignment:=TAlignment.Right;
    end;

    // Footer
    tmp.Footer.Text.Clear;
    tmp.Footer.Text.Add('This is a footer text');
    tmp.Footer.Font.Color:=TAlphaColors.Navy;
    tmp.Footer.Alignment:=TAlignment.Right;

    // Font Size automatic ? (auto-fit all items)
    tmp.AutoWidth:=CBAuto.IsChecked;

    // Document Page properties
    tmp.Page.Size:=TPDFPageSize.psA4;
    tmp.Page.Orientation:=TPDFPageOrientation.poPortrait;

    // Page Numbers
    tmp.Page.Numbering.Visible:=CBPageNumbers.IsChecked;
    tmp.Page.Numbering.Font.Style:=[TFontStyle.fsItalic];

    // Grid lines
    tmp.GridLines.Horizontal.Visible:=CBGridLines.IsChecked;
    tmp.GridLines.Vertical.Visible:=CBGridLines.IsChecked;

    // Set Data item
    tmp.Data:=BIGrid1.Data;

    // Temporary file name to save PDF
    tmpFile:=TPath.Combine(TPath.GetTempPath,'deleteme.pdf');

    // Just in case, release browser locked pdf file, before saving it again
    WebBrowserPDF.Navigate('about:blank');
    WebBrowserPDF.FinishLoading;

    // Let some time to web browser to unlock the file
    //Sleep(500);

    tmp.SaveToFile(tmpFile);

    // Show PDF in web browser control
    WebBrowserPDF.Navigate(tmpFile);
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

end.
