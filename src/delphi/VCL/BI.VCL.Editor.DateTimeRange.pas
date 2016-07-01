unit BI.VCL.Editor.DateTimeRange;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  {$IFDEF FPC}
  Calendar,
  {$ENDIF}

  BI.Data, Vcl.ComCtrls, BI.VCL.Editor.ListItems, Vcl.ExtCtrls,
  BI.Expression, BI.VCL.Editor.NumericFromTo, BI.Arrays;

type
  TSelectedPart=record
  public
    Enabled : Boolean;
    Part : TDateTimePart;
    Value : Integer;
    DateTime : TDateTime;
  end;

  {$IFDEF FPC}
  TMonthCalendar=TCalendar;
  {$ENDIF}

  TDateTimeRangeEditor = class(TForm)
    PageControl1: TPageControl;
    TabRange: TTabSheet;
    TabSheet3: TTabSheet;
    PanelMonths: TPanel;
    PanelWeeks: TPanel;
    Splitter1: TSplitter;
    PageRange: TPageControl;
    TabCombo: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    CBDay: TComboBox;
    CBMonth: TComboBox;
    CBYear: TComboBox;
    CBDayTo: TComboBox;
    CBMonthTo: TComboBox;
    CBYearTo: TComboBox;
    CBPeriod: TComboBox;
    CBPeriod2: TComboBox;
    TabCalendar: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    CalendarFrom: TMonthCalendar;
    CalendarTo: TMonthCalendar;
    TabFromTo: TTabSheet;
    TabSelected: TTabSheet;
    Panel1: TPanel;
    Label5: TLabel;
    CBPart: TComboBox;
    procedure CBYearChange(Sender: TObject);
    procedure CBMonthChange(Sender: TObject);
    procedure CBYearToChange(Sender: TObject);
    procedure CBMonthToChange(Sender: TObject);
    procedure CBPeriodChange(Sender: TObject);
    procedure CBPeriod2Change(Sender: TObject);
    procedure CalendarFromClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBDayChange(Sender: TObject);
    procedure CBDayToChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageRangeChange(Sender: TObject);
    procedure CBPartChange(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    FOnChanged : TNotifyEvent;

    IDatePart : TDateTimePart;

    IFromTo,
    ISelected : TNumericFromTo;

    IMonths,
    IWeeks : TFormListItems;

    procedure ChangedFromTo(Sender: TObject);
    procedure ChangedIncluded(Sender: TObject);
    procedure ChangedSelected(Sender: TObject);
    function DateFrom(const AYear,AMonth,ADay:TComboBox):TDateTime;
    procedure DoAddDays(const AYear,AMonth,ADay:TComboBox; First:Boolean);
    procedure DoAddMonths(const AYear,AMonth:TComboBox; First:Boolean);
    procedure Modified;
    procedure SelectCombos(const AFrom,ATo:TDateTime);
    function SelectedPart:TDateTimePart;
    procedure SetCustom;
  public
    { Public declarations }

    Data : TDataItem;

    //function Filter:TLogicalExpression;

    function FromDate:TDateTime;
    function ToDate:TDateTime;

    function Months:TBooleanArray;
    function Part:TSelectedPart;
    function WeekDays:TBooleanArray;

    procedure Refresh(const AData:TDataItem);

    procedure SelectRange(const AFrom,ATo:TDateTime); overload;
    procedure SelectRange(const AData:TDataItem); overload;

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
