unit BI.VCL.Editor.Split;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Data.Tools;

type
  TSplitEditor = class(TForm)
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    PageBy: TPageControl;
    TabPercent: TTabSheet;
    TabCount: TTabSheet;
    Panel1: TPanel;
    RGMode: TRadioGroup;
    LPercent: TLabel;
    TBPercent: TTrackBar;
    LValue: TLabel;
    ECount: TEdit;
    procedure TBPercentChange(Sender: TObject);
    procedure PageByChange(Sender: TObject);
    procedure ECountChange(Sender: TObject);
  private
    { Private declarations }

    procedure EnableOkButton;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; var ASplit:TSplitOptions):Boolean; static;
  end;

implementation
