unit VCLBI.Editor.Split;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Tools;

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

{$R *.dfm}

procedure TSplitEditor.ECountChange(Sender: TObject);
begin
  EnableOkButton;
end;

class function TSplitEditor.Edit(const AOwner: TComponent;
  var ASplit: TSplitOptions): Boolean;
begin
  with TSplitEditor.Create(AOwner) do
  try
    RGMode.ItemIndex:=Ord(ASplit.Mode);
    TBPercent.Position:=Round(ASplit.Percent);
    ECount.Text:=IntToStr(ASplit.Count);

    if ASplit.By=TSplitBy.Percent then
       PageBy.ActivePage:=TabPercent
    else
       PageBy.ActivePage:=TabCount;

    result:=ShowModal=mrOk;

    if result then
    begin
      ASplit.Mode:=TSplitMode(RGMode.ItemIndex);
      ASplit.Percent:=TBPercent.Position;
      TryStrToInt(ECount.Text,ASplit.Count);

      if PageBy.ActivePage=TabPercent then
         ASplit.By:=TSplitBy.Percent
      else
         ASplit.By:=TSplitBy.Count;

    end;
  finally
    Free;
  end;
end;

procedure TSplitEditor.EnableOkButton;

  function HasCount:Boolean;
  var tmp : Integer;
  begin
    result:=TryStrToInt(ECount.Text,tmp) and (tmp>0);
  end;

begin
  BOk.Enabled:=(PageBy.ActivePage=TabPercent) or
               ((PageBy.ActivePage=TabCount) and HasCount);
end;

procedure TSplitEditor.PageByChange(Sender: TObject);
begin
  EnableOkButton;
end;

procedure TSplitEditor.TBPercentChange(Sender: TObject);
begin
  LValue.Caption:=IntToStr(TBPercent.Position)+'%';
end;

end.
