unit BI.VCL.Editor.SelectText;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.VCL.Editor.ListItems, BI.Data;

type
  TSelectTextCheckEvent=procedure(const Sender:TObject;
                                  const AText:String;
                                  const IsChecked:Boolean) of object;

  TSelectTextItems = class(TForm)
    PageText: TPageControl;
    TabMultiText: TTabSheet;
    TabSingleText: TTabSheet;
    LBSingleText: TListBox;
    procedure LBSingleTextClick(Sender: TObject);
    procedure PageTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    FData : TDataItem;

    ITextItems : TFormListItems;

    FOnChanged : TNotifyEvent;
    FOnChecked : TSelectTextCheckEvent;

    procedure ChangedText(Sender: TObject);
    procedure CheckedText(Sender: TObject);
    procedure DoChanged;
  public
    { Public declarations }

    function Selected:TStringList;

    procedure Refresh(const AData:TDataItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnChecked:TSelectTextCheckEvent read FOnChecked write FOnChecked;
  end;

implementation
