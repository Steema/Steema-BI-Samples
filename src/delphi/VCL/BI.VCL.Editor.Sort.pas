{*********************************************}
{  TeeBI Software Library                     }
{  Sort Editor Dialog                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Sort;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Query,
  BI.VCL.Editor.ListItems, Vcl.StdCtrls, Vcl.ExtCtrls, BI.Data,
  BI.Data.Expressions;

type
  TSortEditor = class(TForm)
    Panel1: TPanel;
    CBAscending: TCheckBox;
    BDelete: TButton;
    Splitter1: TSplitter;
    CBIgnoreCase: TCheckBox;
    LBAvailable: TListBox;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    BAdd: TButton;
    procedure FormShow(Sender: TObject);
    procedure CBAscendingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure LBAvailableClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure CBIgnoreCaseClick(Sender: TObject);
  private
    { Private declarations }

    FOnChanged : TNotifyEvent;
    Sort : TQuerySort;

    IList : TFormListItems;

    IChanging : Boolean;

    procedure AddItem(const AItem:TQuerySortItem);
    procedure AddItems;
    function Current:TQuerySortItem;
    procedure CheckedAll(Sender: TObject);
    procedure CheckedItem(Sender: TObject);
    procedure ExchangedItem(const Sender:TObject; const A,B:Integer);
    procedure Modified;
    procedure SelectedItem(Sender: TObject);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent;
                        const ASort:TQuerySort):Boolean; overload; static;

    class function Edit(const AOwner: TComponent;
                        const AData:TDataItem;
                        out AItems:TSortItems):Boolean; overload; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ASort:TQuerySort):TSortEditor; static;

    procedure Refresh(const ASort:TQuerySort);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
