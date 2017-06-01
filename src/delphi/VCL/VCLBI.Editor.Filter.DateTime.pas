{*********************************************}
{  TeeBI Software Library                     }
{  TFilterItem DateTime editor dialog         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Filter.DateTime;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  BI.Arrays, BI.Expression.Filter, VCLBI.Editor.DateTimeRange,
  VCLBI.Editor.ListItems;

(*
   Select a datetime "range" using multiple options.

   1) Predefined ranges:

       "This month", "Last year" etc etc

   2) Custom from/to range

   3) Optional: include/exclude specific months and weekdays

*)

type
  TDateTimeFilterEditor = class(TForm)
    PageControl1: TPageControl;
    TabCommon: TTabSheet;
    TabCustom: TTabSheet;
    LBCommon: TListBox;
    TabIncluded: TTabSheet;
    PanelMonths: TPanel;
    PanelWeeks: TPanel;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure LBCommonClick(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    Item : TFilterItem;

    IMonths,
    IWeeks : TFormListItems;

    IRange : TDateTimeRangeEditor;

    FOnChanged : TNotifyEvent;

    procedure ChangedIncluded(Sender: TObject);
    procedure ChangedRange(Sender: TObject);

    procedure DoChanged;

    function Months: TBooleanArray;
    function WeekDays: TBooleanArray;
  public
    { Public declarations }

    class function Embed(const AOwner:TComponent; const AParent:TWinControl;
                         const AOnChange:TNotifyEvent):TDateTimeFilterEditor; static;

    procedure Refresh(const AItem:TFilterItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
