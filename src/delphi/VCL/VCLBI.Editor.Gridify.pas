unit VCLBI.Editor.Gridify;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.DataItem, BI.Workflow, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, VCLBI.Editor.ListItems,
  VCLBI.Editor.DataComponent;

type
  TGridifyEditor = class(TForm)
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    PageControl1: TPageControl;
    TabValue: TTabSheet;
    TabRows: TTabSheet;
    TabColumns: TTabSheet;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    FData : TDataItem;

    IRows,
    IColumns : TFormListItems;

    IValue : TDataComponent;

    procedure SelectedChanged(Sender: TObject);
  public
    { Public declarations }

    class function Choose(const AOwner:TComponent;
                          const AData:TDataItem;
                          const AGridOwner:TComponent;
                          out AGrid:TDataGridifyItem):Boolean; static;
  end;

implementation
