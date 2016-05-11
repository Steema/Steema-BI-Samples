unit BI.VCL.DataSelect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Data, BI.VCL.DataManager, BI.VCL.Editor.DataComponent;

type
  TDataSelector = class(TForm)
    PageControl1: TPageControl;
    TabStore: TTabSheet;
    TabComponent: TTabSheet;
    PanelButtons: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    IManager : TDataManager;
    IComp : TDataComponent;

    procedure FilterSelf(Sender: TComponent; var Valid:Boolean);
  public
    { Public declarations }

    class function Choose(const AOwner:TComponent;
                          const AEdited:TComponent):TDataItem; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AEdited: TComponent):TDataSelector; static;

    property ComponentSelector:TDataComponent read IComp;
    property Manager:TDataManager read IManager;
  end;

implementation
