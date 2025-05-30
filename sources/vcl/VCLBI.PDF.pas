{*********************************************}
{  TeeBI Software Library                     }
{  PDF data export                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.PDF;
{.$DEFINE FMX}

interface

uses
  BI.Arrays, BI.DataItem, BI.DataSource,
  {$IFDEF FMX}
  FMXTee.Canvas, FMXTee.Canvas.PDF, FMXTee.Constants,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21.0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ELSE}
  FMX.Types,
  {$ENDIF}

  {$ELSE}
  VCL.Graphics, VCLTee.TeCanvas, VCLTee.TeePDFCanvas, VCLTee.TeeConst,
  {$ENDIF}
  System.Classes, System.Types;

type
  TBIPDFExport=class;

  TFormat=class(TPersistent)
  private
    FBrush : TTeeBrush;
    FFont: TTeeFont;
    FVisible : Boolean;

    {$IFDEF AUTOREFCOUNT}
    [Weak]
    {$ENDIF}
    IParent : TBIPDFExport;

    procedure SetFont(const Value: TTeeFont);
    procedure SetBrush(const Value: TTeeBrush);
  public
    Constructor Create(const AParent:TBIPDFExport); virtual;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Brush:TTeeBrush read FBrush write SetBrush;
    property Font:TTeeFont read FFont write SetFont;
    property Visible:Boolean read FVisible write FVisible default True;
  end;

  THeaderPosition=(AllPages, FirstPage);

  THeader=class(TFormat)
  private
    FPosition : THeaderPosition;
  public
    Constructor Create(const AParent:TBIPDFExport); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Position:THeaderPosition read FPosition write FPosition default AllPages;
  end;

  TAlignment=(Left,Center,Right);

  TTitle=class(THeader)
  private
    FAlign  : TAlignment;
    FHeight : TCoordinate;
    FText   : TStrings;

    ILineHeight : Integer;

    procedure CalcHeight;
    procedure SetText(const Value: TStrings);
  public
    Constructor Create(const AParent:TBIPDFExport); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Alignment:TAlignment read FAlign write FAlign default TAlignment.Center;
    property Height:TCoordinate read FHeight;
    property Text:TStrings read FText write SetText;
  end;

  TGridLines=class(TPersistent)
  private
    FHorizontal: TTeePen;
    FVertical: TTeePen;

    procedure SetHorizontal(const Value: TTeePen);
    procedure SetVertical(const Value: TTeePen);
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Horizontal:TTeePen read FHorizontal write SetHorizontal;
    property Vertical:TTeePen read FVertical write SetVertical;
  end;

  TPageNumbering=class(TTitle)
  private
    FExpression : String;
  public
    Constructor Create(const AParent:TBIPDFExport); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Alignment default TAlignment.Right;
    property Expression:String read FExpression write FExpression;
  end;

  TPage=class(TPersistent)
  private
    FCustomHeight : Integer;
    FCustomWidth : Integer;
    FCurrent : Integer;
    FNumbering : TPageNumbering;
    FOrientation : TPDFPageOrientation;
    FSize : TPDFPageSize;
    FTotal : Integer;

    procedure SetNumbering(const Value: TPageNumbering);
  public
    Constructor Create(const AParent:TBIPDFExport);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    property Current:Integer read FCurrent;
    property Total:Integer read FTotal;
  published
    property CustomHeight:Integer read FCustomHeight write FCustomHeight default 0;
    property CustomWidth:Integer read FCustomWidth write FCustomWidth default 0;

    property Numbering:TPageNumbering read FNumbering write SetNumbering;
    property Orientation:TPDFPageOrientation read FOrientation write FOrientation default TPDFPageOrientation.poPortrait;
    property Size:TPDFPageSize read FSize write FSize default TPDFPageSize.psDefault;
  end;

  TRowFormat=class(TFormat)
  private
    FAlternate : TFormat;

    procedure SetAlternate(const Value: TFormat);
  public
    Constructor Create(const AParent:TBIPDFExport); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Alternate:TFormat read FAlternate write SetAlternate;
  end;

  TBIPDFExport=class(TBITextExport)
  private
    FAlignment : TAlignment;
    FAutoWidth : Boolean;
    FCanvas : TPDFCanvas;
    FDoc : TPDFDocument;
    FFooter : TTitle;
    FGridLines: TGridLines;
    FHeader : THeader;
    FPage : TPage;
    FRowFormat : TRowFormat;
    FTitle: TTitle;

    // Temporary:

    FAutoFont : Boolean;
    FAutoFontSize : Single;
    FColPositions : TSingleArray;
    FColWidths : TSingleArray;
    FItemAligns : TInt32Array;
    FHeaderShorter : TBooleanArray;
    FColumnSpacing : Single;
    FHeaderTop : TCoordinate;
    FMaxItem : Integer;

    YPos : TCoordinate;

    // Dummy Bitmap for TextWidth and TextHeight
    FBitmap : TBitmap;

  const
    LeftMargin=10;
    RightMargin=10;
    TopMargin=10;
    BottomMargin=10;

    procedure BeginPage;
    procedure CalcMaxItem;
    procedure CalcTotalPages;
    procedure CheckAutoFont;
    procedure DrawCell(const ACol:Integer; const AText:String);
    procedure DrawTitle(const ATitle:TTitle);
    procedure EmitHeader;
    procedure EmitCursor;
    procedure EmitRow(const AIndex:TInteger);
    procedure FillBackground(const AFormat:TFormat);
    procedure FinishPage;
    function HeaderMargin:Integer;
    function LineHeight:TCoordinate;
    procedure NewLine;
    procedure PrepareHeader;
    procedure ReCreateCanvas;
    procedure SetGridLines(const Value: TGridLines);
    procedure SetRowFont;
    function ShouldDraw(const AItem:THeader):Boolean;
    function XLeft:TCoordinate;
    function XRight:TCoordinate;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    class function Supports(const Extension:String):Boolean; override;

    property Alignment:TAlignment read FAlignment write FAlignment default TAlignment.Center;
    property AutoWidth:Boolean read FAutoWidth write FAutoWidth default True;
    property Footer:TTitle read FFooter;
    property GridLines:TGridLines read FGridLines write SetGridLines;
    property Header:THeader read FHeader;
    property Page:TPage read FPage;
    property RowFormat:TRowFormat read FRowFormat;
    property Title:TTitle read FTitle;
  end;

implementation

uses
  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}
  System.SysUtils;

type
  TCoordinate={$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};

{ TBIPDFExport }

Constructor TBIPDFExport.Create;
begin
  inherited;

  FAlignment:=TAlignment.Center;
  FAutoWidth:=True;

  FFooter:=TTitle.Create(Self);
  FRowFormat:=TRowFormat.Create(Self);
  FHeader:=THeader.Create(Self);
  FTitle:=TTitle.Create(Self);

  FGridLines:=TGridLines.Create;

  FPage:=TPage.Create(Self);

  FHeaderTop:=TopMargin;
end;

Destructor TBIPDFExport.Destroy;
begin
  FPage.Free;
  FTitle.Free;
  FGridLines.Free;
  FHeader.Free;
  FRowFormat.Free;
  FFooter.Free;

  FBitmap.Free;

  inherited;
end;

class function TBIPDFExport.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.PDF');
end;

procedure TBIPDFExport.ReCreateCanvas;
begin
  FCanvas.Free;
  FCanvas:=TPDFCanvas.CreatePage(FDoc,Page.FCurrent-1);

  // Necessary for compatibility with old versions of TeeChart and TPDFExportFormat
  if FBitmap=nil then
     FBitmap:=TBitmap.Create{$IFDEF FMX}{$IF CompilerVersion<26}(0,0){$ENDIF}{$ENDIF};

  FCanvas.ReferenceCanvas:=FBitmap.Canvas;
end;

procedure TBIPDFExport.DoEmit(const AItems: TStrings);
var tmp : TStringStream;
begin
  inherited;

  FDoc:=TPDFDocument.Create;
  try
    FDoc.Size:=Page.Size;

    if Page.Size=psCustom then
    begin
      FDoc.Width:=Page.CustomWidth;
      FDoc.Height:=Page.CustomHeight;
    end;

    FDoc.Orientation:=Page.Orientation;

    Page.FCurrent:=1;

    ReCreateCanvas;
    try
      YPos:=TopMargin;

      Cursor.LoadData;

      if Title.Visible then
         if Title.Text.Text='' then
            Title.Text.Text:=Cursor.Data.Name;

      Title.CalcHeight;
      Footer.CalcHeight;

      PrepareHeader;
      CalcTotalPages;

      EmitCursor;

      tmp:=TStringStream.Create('');
      try
        FDoc.SaveToStream(tmp);

        tmp.Position:=0;
        AItems.LoadFromStream(tmp);
      finally
        tmp.Free;
      end;

    finally
      FCanvas.Free;
    end;
  finally
    FDoc.Free;
  end;
end;

const
  TA_LEFT = 0;
  TA_RIGHT = 2;
  TA_CENTER = 6;

procedure TBIPDFExport.PrepareHeader;

  function TextWidthOf(const S:String):Single;
  begin
    result:=FCanvas.TextSize(S+' ').x {$IFDEF FMX}*1.1{$ENDIF};
  end;

  // Returns the longest width in "pixels" of a given item
  function MaxItemWidth(const AItem:TDataItem):Single;

    // For text data item, calculate the longest text string
    function LongestWidth:Single;
    var t : TLoopInteger;
        tmp : Single;
    begin
      result:=0;

      for t:=0 to AItem.Count-1 do
      begin
        tmp:=TextWidthOf(AItem.TextData[t]);

        if tmp>result then
           result:=tmp;
      end;
    end;

    function MaxBooleanWidth:Single;

      function BoolWidth(const Value:Boolean):Single;
      begin
        result:=TextWidthOf(BoolToStr(Value,True));
      end;

    var tmp : Single;
    begin
      result:=BoolWidth(True);
      tmp:=BoolWidth(False);

      if tmp>result then
         result:=tmp;
    end;

  begin
    case AItem.Kind of
      // Pending:
      // Numeric Kinds (use max of Stats Min,Max,etc, to avoid loop?)

      dkText: result:=LongestWidth;
      dkDateTime: result:=TextWidthOf(DateTimeToStr(Now));
      dkBoolean: result:=MaxBooleanWidth;
    else
      result:=0;
    end;
  end;

  // Determine the left/center/right alignment for each item
  procedure CalcItemAligns;
  var t : Integer;
  begin
    for t:=0 to High(Items) do
        case Items[t].Kind of
          dkInt32,
          dkInt64,
          dkSingle,
          dkDouble,
          dkExtended : FItemAligns[t]:=TA_RIGHT;
        else
          FItemAligns[t]:=TA_LEFT;
        end;
  end;

  procedure CalcColWidths(const ASize:Single);
  var t : Integer;
      tmp,
      ItemWidth : Single;
  begin
    FCanvas.AssignFont(FHeader.Font);

    if ASize>0 then
       FCanvas.Font.SizeFloat:=ASize;

    FColumnSpacing:=TextWidthOf('  ');

    for t:=0 to High(Items) do
        FColWidths[t]:=TextWidthOf(Items[t].Name);

    FCanvas.AssignFont(FRowFormat.Font);

    if ASize>0 then
       FCanvas.Font.SizeFloat:=ASize;

    for t:=0 to High(Items) do
    begin
      ItemWidth:=MaxItemWidth(Items[t]);

      FHeaderShorter[t]:=ItemWidth>FColWidths[t];

      if FHeaderShorter[t] then
         FColWidths[t]:=ItemWidth;
    end;

    tmp:=TextWidthOf('  ');

    if tmp>FColumnSpacing then
       FColumnSpacing:=tmp;
  end;

  // Returns the total width of all items, including spacing between columns
  function TotalWidth:Single;
  begin
    result:=FColWidths.Sum+(FColumnSpacing*High(FColWidths));
  end;

  // Fill the column positions array
  procedure CalcColPositions;
  var t : Integer;
  begin
    if Alignment=TAlignment.Left then
       FColPositions[0]:=LeftMargin
    else
    if Alignment=TAlignment.Right then
       FColPositions[0]:=FDoc.Width-RightMargin-TotalWidth
    else
       FColPositions[0]:=(FDoc.Width-TotalWidth)*0.5;

    for t:=1 to High(Items) do
        FColPositions[t]:=FColPositions[t-1]+FColWidths[t-1]+FColumnSpacing;
  end;

  // Try to reduce font size until all items can fit in page width
  procedure FitFontSize;
  begin
    while (LeftMargin+RightMargin+TotalWidth)>FDoc.Width do
    begin
      if FCanvas.Font.SizeFloat>2 then
      begin
        CalcColWidths(FCanvas.Font.SizeFloat*0.95);

        FAutoFont:=True;
      end
      else
        break;
    end;
  end;

var L : Integer;
begin
  L:=Items.Count;

  // Init arrays
  SetLength(FItemAligns,L);
  SetLength(FColPositions,L);
  SetLength(FColWidths,L);
  SetLength(FHeaderShorter,L);

  if L>0 then
  begin
    CalcItemAligns;
    CalcColWidths(0);

    FAutoFont:=False;

    if AutoWidth then
       FitFontSize;

    FAutoFontSize:=FCanvas.Font.SizeFloat;

    CalcColPositions;

    CalcMaxItem;
  end;
end;

// Change the canvas font size to the "automatic fit" font size
procedure TBIPDFExport.CheckAutoFont;
begin
  if FAutoFont and (FAutoFontSize<FCanvas.Font.SizeFloat) then
     FCanvas.Font.SizeFloat:=FAutoFontSize;
end;

function TBIPDFExport.HeaderMargin:Integer;
begin
  result:=(FCanvas.FontHeight div 4);
end;

procedure TBIPDFExport.EmitHeader;
var t : Integer;
begin
  FCanvas.AssignFont(FHeader.Font);
  CheckAutoFont;

  FillBackground(FHeader);

  // Draw header texts
  for t:=0 to FMaxItem do
      DrawCell(t,Items[t].Name);

  NewLine;

  // Vertical spacing between header and rows
  YPos:=YPos+HeaderMargin;
end;

procedure TBIPDFExport.DrawTitle(const ATitle:TTitle);
var t : Integer;
    tmpWidth,
    tmpX : Integer;
begin
  FCanvas.AssignFont(ATitle.Font);

  for t:=0 to ATitle.Text.Count-1 do
  begin
    if ATitle.Alignment=TAlignment.Left then
       tmpX:=LeftMargin
    else
    begin
      tmpWidth:=FCanvas.TextWidth(ATitle.Text[t]);

      if ATitle.Alignment=TAlignment.Right then
         tmpX:=FDoc.Width-RightMargin-tmpWidth
      else
         tmpX:=(FDoc.Width div 2)-(tmpWidth div 2); // center position
    end;

    FCanvas.TextOut(tmpX,YPos,ATitle.Text[t]);
    NewLine;
  end;
end;

procedure TBIPDFExport.FinishPage;

  procedure DrawVerticalLines;
  var t : Integer;
      x : TCoordinate;
  begin
    FCanvas.AssignVisiblePen(FGridLines.Vertical);

    for t:=1 to FMaxItem do
    begin
      x:=Round(FColPositions[t]-(FColumnSpacing*0.5));
      FCanvas.DoVertLine(x,FHeaderTop,YPos);
    end;
  end;

begin
  if FGridLines.Vertical.Visible then
     DrawVerticalLines;

  if ShouldDraw(Footer) then
  begin
    YPos:=FDoc.Height-Footer.Height-BottomMargin;
    DrawTitle(Footer);
  end;
end;

procedure TBIPDFExport.SetRowFont;
begin
  FCanvas.AssignFont(FRowFormat.Font);
  CheckAutoFont;
end;

// Accurate calculation of available space for row lines,
// and then divide total number of rows to obtain the (very approximate)
// total number of pages in the document.
procedure TBIPDFExport.CalcTotalPages;
var tmp : Integer;
    tmpH,
    tmpFont : TCoordinate;
    tmpItems : Integer;
begin
  tmp:=1;

  if Data.Count>0 then
  begin
    tmpH:=FDoc.Height-TopMargin-BottomMargin;

    if ShouldDraw(Page.Numbering) then
       tmpH:=tmpH-Page.Numbering.Height-Page.Numbering.ILineHeight;

    if ShouldDraw(Title) then
       tmpH:=tmpH-Title.Height-2*Title.ILineHeight; // 2=Extra newline

    if Footer.Visible then
       tmpH:=tmpH-Footer.Height-Footer.ILineHeight;

    if Header.Visible then
    begin
      FCanvas.AssignFont(FHeader.Font);
      CheckAutoFont;

      tmpH:=tmpH-(2*FCanvas.FontHeight)-HeaderMargin;
    end;

    SetRowFont;

    tmpFont:=LineHeight;

    // Extra margin between last row and footer or end of page
    tmpH:=tmpH-(FCanvas.FontHeight);

    if tmpFont>0 then
    begin
      tmpItems:=Round(tmpH/tmpFont);

      if tmpItems>0 then
      begin
        tmp:=Data.Count div tmpItems;

        if tmp*tmpItems < Data.Count then
           Inc(tmp); // <-- last partial page
      end;
    end;
  end;

  Page.FTotal:=tmp;
end;

procedure TBIPDFExport.EmitCursor;

  procedure EmitTotals;
  var Old : TDataArray;
  begin
    Old:=Items;

    Items:=Totals.Items.AsArray;
    try
      EmitRow(0);
    finally
      Items:=Old;
    end;
  end;

begin
  BeginPage;

  if not SchemaOnly then
     if Items.Count>0 then
     begin
       SetRowFont;
       Cursor.Loop(EmitRow);

       if Totals<>nil then
          EmitTotals;

       FinishPage;
     end;
end;

procedure TBIPDFExport.DrawCell(const ACol:Integer; const AText:String);
var X : TCoordinate;
begin
  if FItemAligns[ACol]=TA_RIGHT then
  begin
    FCanvas.TextAlign:=TA_RIGHT;
    X:={$IFNDEF FMX}Round{$ENDIF}(FColPositions[ACol]+FColWidths[ACol]);
  end
  else
  begin
    FCanvas.TextAlign:=TA_LEFT;
    X:={$IFNDEF FMX}Round{$ENDIF}(FColPositions[ACol]);
  end;

  FCanvas.TextOut(X,YPos,AText);
end;

function TBIPDFExport.LineHeight:TCoordinate;
begin
  result:={$IFNDEF FMX}Round{$ENDIF}(FCanvas.FontHeight*1.1);
end;

procedure TBIPDFExport.NewLine;
begin
  YPos:=YPos+LineHeight;
end;

procedure TBIPDFExport.SetGridLines(const Value: TGridLines);
begin
  FGridLines.Assign(Value);
end;

// Calculate the maximum item index, that can be displayed at page right edge
procedure TBIPDFExport.CalcMaxItem;
begin
  FMaxItem:=High(Items);

  if FMaxItem>=0 then
  while (FColPositions[FMaxItem]+FColWidths[FMaxItem])>=FDoc.Width-RightMargin do
  begin
    Dec(FMaxItem);

    if FMaxItem<0 then
       break;
  end;
end;

// Returns the X position of the first item (the left edge)
function TBIPDFExport.XLeft:TCoordinate;
begin
  result:={$IFNDEF FMX}Round{$ENDIF}(FColPositions[0]);
end;

// Returns the X position of the last visible item (the right edge)
function TBIPDFExport.XRight:TCoordinate;
begin
  result:={$IFNDEF FMX}Round{$ENDIF}(FColPositions[FMaxItem]+FColWidths[FMaxItem]+FColumnSpacing);
end;

// Fills a rectangle covering the current Y row, from XLeft to XRight
procedure TBIPDFExport.FillBackground(const AFormat:TFormat);
const
  BrushNone={$IFDEF FMX}TBrushKind.{$IF CompilerVersion<27}bkNone{$ELSE}None{$ENDIF}{$ELSE}TBrushStyle.bsClear{$ENDIF};

var tmpTop : TCoordinate;
begin
  if AFormat.FBrush.Style<>BrushNone then
  begin
    FCanvas.AssignBrush(AFormat.Brush,AFormat.Brush.Color,AFormat.Brush.BackColor);
    FCanvas.Pen.Hide;

    // Space above current YPos (margin)
    tmpTop:={$IFNDEF FMX}Round{$ENDIF}(FCanvas.FontHeight*0.11);

    FCanvas.FillRect(TeeRect(XLeft,YPos-tmpTop,XRight,YPos+LineHeight));
  end;
end;

procedure TBIPDFExport.EmitRow(const AIndex: TInteger);

  procedure DrawHorizontalLine;
  begin
    FCanvas.AssignVisiblePen(FGridLines.Horizontal);
    FCanvas.DoHorizLine(XLeft,XRight,YPos);
  end;

var t : Integer;
begin
  if FMaxItem>-1 then
  begin
    if RowFormat.Alternate.Visible and ((AIndex mod 2)=0) then
       FillBackground(RowFormat.Alternate)
    else
    if RowFormat.Visible then
       FillBackground(RowFormat);

    for t:=0 to FMaxItem do
        DrawCell(t,DataToString(Items[t],AIndex));
  end;

  NewLine;

  if YPos>FDoc.Height-(FCanvas.FontHeight+2)-Footer.Height-BottomMargin then
  begin
    FinishPage;

    FDoc.NewPage;

    Inc(Page.FCurrent);

    YPos:=TopMargin;

    ReCreateCanvas;

    BeginPage;

    SetRowFont;
  end
  else
  if FGridLines.Horizontal.Visible and (FMaxItem>-1) then
     DrawHorizontalLine;
end;

function TBIPDFExport.ShouldDraw(const AItem:THeader):Boolean;
begin
  result:=AItem.Visible and
          ((AItem.Position=THeaderPosition.AllPages) or (Page.FCurrent=1));
end;

procedure TBIPDFExport.BeginPage;
begin
  if ShouldDraw(Page.Numbering) then
  begin
    Page.Numbering.Text.Text:=Format(Page.Numbering.Expression,[Page.Current,Page.Total]);
    DrawTitle(Page.Numbering);
  end;

  if ShouldDraw(Title) then
  begin
    DrawTitle(Title);
    NewLine; // <-- extra margin
  end;

  FHeaderTop:=YPos;

  if ShouldDraw(Header) then
     EmitHeader;
end;

const
  SilverColor={$IFDEF FMX}TAlphaColors.Silver{$ELSE}clSilver{$ENDIF};
  BeigeColor={$IFDEF FMX}TAlphaColors.Beige{$ELSE}$F5F5DC{$ENDIF};

{ TFormat }

Constructor TFormat.Create(const AParent:TBIPDFExport);
begin
  inherited Create;

  IParent:=AParent;

  // Default background is not visible
  FBrush:=TTeeBrush.Create;
  FBrush.Color:=SilverColor;

  // Warning: "Clear" must be called after setting FBrush.Color
  FBrush.Clear;

  FFont:=TTeeFont.Create;

  FVisible:=True;
end;

Destructor TFormat.Destroy;
begin
  FFont.Free;
  FBrush.Free;

  inherited;
end;

procedure TFormat.Assign(Source: TPersistent);
begin
  if Source is TFormat then
  begin
    Brush:=TFormat(Source).Brush;
    Font:=TFormat(Source).Font;
    Visible:=TFormat(Source).Visible;
  end
  else
    inherited;
end;

procedure TFormat.SetBrush(const Value: TTeeBrush);
begin
  FBrush.Assign(Value);
end;

procedure TFormat.SetFont(const Value: TTeeFont);
begin
  FFont.Assign(Value);
end;

{ THeader }

Constructor THeader.Create(const AParent:TBIPDFExport);
begin
  inherited;
  FFont.Style:=[TFontStyle.fsBold];
end;

procedure THeader.Assign(Source: TPersistent);
begin
  if Source is THeader then
  begin
    FPosition:=THeader(Source).FPosition;
  end;

  inherited;
end;

{ TGridLines }

Constructor TGridLines.Create;
begin
  inherited Create;

  FHorizontal:=TTeePen.Create;
  FHorizontal.Color:=SilverColor;  // InitDefColor
  FHorizontal.Visible:=False;
  FHorizontal.Width:=0;

  FVertical:=TTeePen.Create;
  FVertical.Color:=SilverColor;  // InitDefColor
  FVertical.Width:=0;
end;

Destructor TGridLines.Destroy;
begin
  FVertical.Free;
  FHorizontal.Free;
  inherited;
end;

procedure TGridLines.Assign(Source: TPersistent);
begin
  if Source is TGridLines then
  begin
    Horizontal:=TGridLines(Source).Horizontal;
    Vertical:=TGridLines(Source).Vertical;
  end
  else
    inherited;
end;

procedure TGridLines.SetHorizontal(const Value: TTeePen);
begin
  FHorizontal.Assign(Value);
end;

procedure TGridLines.SetVertical(const Value: TTeePen);
begin
  FVertical.Assign(Value);
end;

{ TTitle }

Constructor TTitle.Create(const AParent:TBIPDFExport);
begin
  inherited;
  FText:=TStringList.Create;
  FAlign:=TAlignment.Center;
end;

Destructor TTitle.Destroy;
begin
  FText.Free;
  inherited;
end;

procedure TTitle.Assign(Source: TPersistent);
begin
  if Source is TTitle then
  begin
    FAlign:=TTitle(Source).FAlign;
    FHeight:=0; // <-- reset
    Text:=TTitle(Source).Text;
  end;

  inherited;
end;

procedure TTitle.CalcHeight;
begin
  if Visible and (Text.Text<>'') and (IParent.FCanvas<>nil) then
  begin
    IParent.FCanvas.AssignFont(Font);

    ILineHeight:=IParent.FCanvas.FontHeight;
    FHeight:=Text.Count*ILineHeight;
  end
  else
  begin
    FHeight:=0;
    ILineHeight:=0;
  end;
end;

procedure TTitle.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

{ TPageNumbering }

Constructor TPageNumbering.Create(const AParent: TBIPDFExport);
begin
  inherited;

  FExpression:='Page %d of %d';
  FAlign:=TAlignment.Right;
end;

procedure TPageNumbering.Assign(Source: TPersistent);
begin
  if Source is TPageNumbering then
  begin
    FExpression:=TPageNumbering(Source).FExpression;
  end;

  inherited;
end;

{ TPage }

Constructor TPage.Create(const AParent:TBIPDFExport);
begin
  inherited Create;
  FNumbering:=TPageNumbering.Create(AParent);
end;

Destructor TPage.Destroy;
begin
  FNumbering.Free;
  inherited;
end;

procedure TPage.SetNumbering(const Value: TPageNumbering);
begin
  FNumbering.Assign(Value);
end;

procedure TPage.Assign(Source: TPersistent);
begin
  if Source is TPage then
  begin

  end
  else
    inherited;
end;

{ TRowFormat }

Constructor TRowFormat.Create(const AParent: TBIPDFExport);
const
  BrushSolid={$IFDEF FMX}TBrushKind.{$IF CompilerVersion<27}bkSolid{$ELSE}Solid{$ENDIF}{$ELSE}TBrushStyle.bsSolid{$ENDIF};
begin
  inherited;

  FAlternate:=TFormat.Create(AParent);

  FAlternate.FVisible:=False;
  FAlternate.Brush.Style:=BrushSolid;
  FAlternate.Brush.Color:=BeigeColor;
end;

Destructor TRowFormat.Destroy;
begin
  FAlternate.Free;
  inherited;
end;

procedure TRowFormat.Assign(Source: TPersistent);
begin
  if Source is TRowFormat then
  begin
    Alternate:=TRowFormat(Source).Alternate;
  end;

  inherited;
end;

procedure TRowFormat.SetAlternate(const Value: TFormat);
begin
  FAlternate.Assign(Value);
end;

initialization
  TBIExporters.RegisterClass(TBIPDFExport);
finalization
  TBIExporters.UnRegisterClass(TBIPDFExport);
end.
