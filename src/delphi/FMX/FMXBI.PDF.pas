{*********************************************}
{  TeeBI Software Library                     }
{  PDF data export                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.PDF;
{$DEFINE FMX}

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
