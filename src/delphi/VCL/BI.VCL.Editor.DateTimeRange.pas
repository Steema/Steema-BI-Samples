unit BI.VCL.Editor.DateTimeRange;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BI.Data, Vcl.ComCtrls, BI.VCL.Editor.ListItems, Vcl.ExtCtrls,
  BI.Expression;

type
  TDateTimeRangeEditor = class(TForm)
    PageControl1: TPageControl;
    TabRange: TTabSheet;
    TabCalendar: TTabSheet;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    CBDay: TComboBox;
    CBMonth: TComboBox;
    CBYear: TComboBox;
    Label2: TLabel;
    CBDayTo: TComboBox;
    CBMonthTo: TComboBox;
    CBYearTo: TComboBox;
    CBPeriod: TComboBox;
    CBPeriod2: TComboBox;
    CalendarFrom: TMonthCalendar;
    CalendarTo: TMonthCalendar;
    Label3: TLabel;
    Label4: TLabel;
    PanelMonths: TPanel;
    PanelWeeks: TPanel;
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
  private
    { Private declarations }

    IChanging : Boolean;

    FOnChanged : TNotifyEvent;

    IMonths,
    IWeeks : TFormListItems;

    procedure ChangedSelected(Sender: TObject);
    function DateFrom(const AYear,AMonth,ADay:TComboBox):TDateTime;
    procedure DoAddDays(const AYear,AMonth,ADay:TComboBox; First:Boolean);
    procedure DoAddMonths(const AYear,AMonth:TComboBox; First:Boolean);
    procedure Modified;
    procedure SelectCombos(const AFrom,ATo:TDateTime);
    procedure SetCustom;
  public
    { Public declarations }

    Data : TDataItem;

    function Filter:TLogicalExpression;

    function FromDate:TDateTime;
    function ToDate:TDateTime;

    procedure Refresh(const AData:TDataItem);
    procedure SelectRange(const AFrom,ATo:TDateTime); overload;
    procedure SelectRange(const AData:TDataItem); overload;

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
