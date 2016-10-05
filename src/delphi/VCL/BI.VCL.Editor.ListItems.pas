unit BI.VCL.Editor.ListItems;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.CheckLst, BI.Data;

type
  TListItemsEvent=procedure(const AItems:TDataArray; const ARefresh:Boolean) of object;

  TListExchangedEvent=procedure(const Sender:TObject; const A,B:Integer) of object;

  TFormListItems = class(TForm)
    LBItems: TCheckListBox;
    Panel1: TPanel;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    BAll: TButton;
    BNone: TButton;
    procedure LBItemsClick(Sender: TObject);
    procedure LBItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBItemsClickCheck(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure BAllClick(Sender: TObject);
    procedure BNoneClick(Sender: TObject);
  private
    { Private declarations }

    FOnChanged,
    FOnSelected,
    FOnChecked,
    FOnCheckedAll : TNotifyEvent;

    FOnExchanged : TListExchangedEvent;

    procedure DoChanged;
    procedure Exchange(const A,B:Integer);
    procedure Recreate;

  public
    { Public declarations }

    class procedure AddCombo(const ACombo:TComboBox; const AData:Array of TDataItem); static;

    class procedure AddItems(const AItems:TStrings; const AData:Array of TDataItem;
                             const AddBlank:Boolean=True); overload; static;

    procedure AddItems(const AData:Array of TDataItem;
                       const AddBlank:Boolean=True); overload;

    class procedure AddMap(const AData: TDataItem; const AItems:TStrings); overload; static;
    procedure AddMap(const AData:TDataItem); overload;

    procedure Check(const AIndex:Integer; const ACheck:Boolean);
    procedure CheckAll(const ACheck:Boolean=True);
    function CheckedCount:Integer;

    class function Embed(const AOwner:TComponent; const AParent:TWinControl):TFormListItems; static;

    procedure EnableCheckButtons;
    procedure EnableDrag(const AEnabled:Boolean);

    class procedure DoAddItems(const AItems:TStrings; const AData:Array of TDataItem); static;

    function Items:TDataArray;

    procedure TryAdd(const AData:TDataItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnChecked:TNotifyEvent read FOnChecked write FOnChecked;
    property OnCheckedAll:TNotifyEvent read FOnCheckedAll write FOnCheckedAll;
    property OnExchanged:TListExchangedEvent read FOnExchanged write FOnExchanged;
    property OnSelected:TNotifyEvent read FOnSelected write FOnSelected;
  end;

implementation
