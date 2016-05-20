unit BI.VCL.Editor.ListItems;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.CheckLst, BI.Data;

type
  TListItemsEvent=procedure(const AItems:TDataArray; const ARefresh:Boolean) of object;

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
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }

    FOnChanged : TNotifyEvent;

    procedure DoChanged;
    procedure EnableCheckButtons;
    procedure Exchange(const A,B:Integer);
    procedure Recreate;

  public
    { Public declarations }

    class procedure AddCombo(const ACombo:TComboBox; const AData:Array of TDataItem); static;

    class procedure AddItems(const AItems:TStrings; const AData:Array of TDataItem;
                             const AddBlank:Boolean=True); overload; static;

    procedure AddItems(const AData:Array of TDataItem;
                       const AddBlank:Boolean=True); overload;

    procedure CheckAll(const ACheck:Boolean);
    function CheckedCount:Integer;

    procedure EnableDrag(const AEnabled:Boolean);

    class procedure DoAddItems(const AItems:TStrings; const AData:Array of TDataItem); static;

    function Items:TDataArray;

    procedure TryAdd(const AData:TDataItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
