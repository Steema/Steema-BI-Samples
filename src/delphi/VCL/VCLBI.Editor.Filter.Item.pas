{*********************************************}
{  TeeBI Software Library                     }
{  TFilterItem editor dialog                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Filter.Item;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.Expression.Filter, VCLBI.Editor.Filter.DateTime,
  VCLBI.Editor.NumericFromTo, VCLBI.Editor.SelectText;

(*
  This dialog is used embedded by TDynamicFilterEditor dialog.

  It can also be used standalone.

  TFilterItem class is used by TBIQuery to configure the "where ..." expressions.
*)

type
  TFilterItemEditor = class(TForm)
    PageItem: TPageControl;
    TabDateTime: TTabSheet;
    TabBoolean: TTabSheet;
    CBTrue: TCheckBox;
    CBFalse: TCheckBox;
    TabNumeric: TTabSheet;
    PageNumeric: TPageControl;
    TabNumericRange: TTabSheet;
    TabNumericSelected: TTabSheet;
    TabText: TTabSheet;
    PageControl2: TPageControl;
    TabTextOptions: TTabSheet;
    Label1: TLabel;
    LBTextStyle: TListBox;
    EText: TEdit;
    CBTextCase: TCheckBox;
    TabIncluded: TTabSheet;
    TabExcluded: TTabSheet;
    procedure CBTextCaseClick(Sender: TObject);
    procedure CBTrueClick(Sender: TObject);
    procedure CBFalseClick(Sender: TObject);
    procedure LBTextStyleClick(Sender: TObject);
    procedure ETextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;
    Item : TFilterItem;
    FOnChange: TNotifyEvent;

    INumericFromTo,
    INumericSelected : TNumericFromTo;

    ITextInclude,
    ITextExclude   : TSelectTextItems;
    IDateTime : TDateTimeFilterEditor;

    procedure ChangedDateTime(Sender: TObject);
    procedure ChangedNumeric(Sender: TObject);
    procedure ChangedText(Sender: TObject);
    procedure CheckedText(const Sender: TObject; const AText:String; const IsChecked:Boolean);
    procedure DoChanged;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AItem:TFilterItem):TFilterItemEditor; static;

    procedure Refresh(const AItem:TFilterItem);

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
