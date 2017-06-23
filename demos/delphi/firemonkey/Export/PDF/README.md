TeeBI PDF Export
=================

This example TeeBI_PDF_Export can only be compiled on a RAD Studio that has 
installed the "Pro" version of TeeChart.

The BI.FMX.PDF and BI.VCL.PDF units require one unit that is only present in 
the "Pro" version of TeeChart  (VCLTee.TeePDFCanvas / FMXTee.Canvas.PDF)


The example shows how to export the Data displayed into the TeeBI Grid to a PDF Document.
Several export options like Header, Footer, Titles, Fonts, Brushes, Grid Lines and more are available in order to configure the PDF Page and Document, all can be found inside the TBIPDFExport class.

The PDF Document can be generated and displayed inside a WebBrowser by using the following code :

```
procedure TMainPDF.BCreatePDFClick(Sender: TObject);
var tmp : TBIPDFExport;
    tmpFile : String;
begin
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
  finally
    tmp.Free;
  end;
end;

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
```

![screenshot](https://raw.githubusercontent.com/Steema/BI/master/demos/delphi/firemonkey/Export/PDF/img/TeeBI_PDFExport.gif "TeeBI PDF Export")


