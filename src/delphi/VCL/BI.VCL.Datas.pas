unit BI.VCL.Datas;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, BI.Data;

type
  TSelect=procedure(Sender:TObject; const Items:TDataArray) of object;
  TValidData=procedure(Sender:TObject; const AData:TDataItem; var Valid:Boolean) of object;

  TFormDatas = class(TForm)
    ListDatas: TListBox;
    Splitter1: TSplitter;
    ListItems: TListBox;
    procedure ListDatasClick(Sender: TObject);
    procedure ListItemsClick(Sender: TObject);
  private
    { Private declarations }
    FOnSelect : TSelect;
    FOnValidData : TValidData;
  public
    { Public declarations }

    class function Embedd(const AParent:TWinControl; const Data:TDataItem):TFormDatas;

    function SelectedItem:TDataItem;
    function SelectedData:TDataItem;

    property OnSelect:TSelect read FOnSelect write FOnSelect;
    property OnValidData:TValidData read FOnValidData write FOnValidData;
  end;

implementation
