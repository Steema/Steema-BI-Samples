unit BI.VCL.Editor.BIGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BI.VCL.Grid, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TBIGridEditor = class(TForm)
    PageControl1: TPageControl;
    TabBIGrid: TTabSheet;
    GroupBox1: TGroupBox;
    CBAltRows: TCheckBox;
    BAltColor: TButton;
    CBRowNumbers: TCheckBox;
    CBFilter: TCheckBox;
    CBSearch: TCheckBox;
    TabPlugin: TTabSheet;
    Label1: TLabel;
    CBShowItems: TComboBox;
    CBColorize: TCheckBox;
    procedure BAltColorClick(Sender: TObject);
    procedure CBAltRowsClick(Sender: TObject);
    procedure CBFilterClick(Sender: TObject);
    procedure CBRowNumbersClick(Sender: TObject);
    procedure CBSearchClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBShowItemsChange(Sender: TObject);
    procedure CBColorizeClick(Sender: TObject);
  private
    { Private declarations }

    Grid : TBIGrid;
    ISetting : Boolean;

    IPlugin : TCustomForm;

    function PluginClass:TCustomFormClass;
    procedure ShowPluginTabs;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const ABIGrid:TBIGrid); static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGrid:TBIGrid):TBIGridEditor; static;

    procedure Refresh(const AGrid:TBIGrid);
  end;

implementation
