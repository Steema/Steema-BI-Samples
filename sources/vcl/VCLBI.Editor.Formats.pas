{*********************************************}
{  TeeBI Software Library                     }
{  Data Formats Editor                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Formats;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.DataSource, BI.Persist, Vcl.ExtCtrls;

type
  TDataFormatEditor = class(TForm)
    PageFormat: TPageControl;
    TabCSV: TTabSheet;
    Label1: TLabel;
    CBDelimiter: TComboBox;
    ECustomDelimiter: TEdit;
    Label2: TLabel;
    CBQuote: TComboBox;
    Label3: TLabel;
    EMissing: TEdit;
    Label4: TLabel;
    CBHeader: TComboBox;
    Label5: TLabel;
    Edit1: TEdit;
    UDHeader: TUpDown;
    TabZip: TTabSheet;
    EZipPassword: TEdit;
    Label6: TLabel;
    TabExcel: TTabSheet;
    Label7: TLabel;
    EExcelHeader: TEdit;
    UDExcelHeader: TUpDown;
    TabJSON: TTabSheet;
    RGJSONStyle: TRadioGroup;
    RGJSONFormat: TRadioGroup;
    TabXML: TTabSheet;
    RGXMLStyle: TRadioGroup;
    TabSheet1: TTabSheet;
    CBIncludeViews: TCheckBox;
    procedure CBDelimiterChange(Sender: TObject);
    procedure ECustomDelimiterChange(Sender: TObject);
    procedure CBQuoteChange(Sender: TObject);
    procedure CBHeaderChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure EMissingChange(Sender: TObject);
    procedure EZipPasswordChange(Sender: TObject);
    procedure EExcelHeaderChange(Sender: TObject);
    procedure RGJSONStyleClick(Sender: TObject);
    procedure RGJSONFormatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RGXMLStyleClick(Sender: TObject);
    procedure CBIncludeViewsClick(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    IData : TDataDefinition;

    function CanChange(const AControl:TWinControl=nil):Boolean;

    procedure RefreshCSV;
    procedure RefreshDatabase;
    procedure RefreshExcel;
    procedure RefreshJSON;
    procedure RefreshXML;
    procedure RefreshZip;
  public
    { Public declarations }
    class procedure Embedd(const AData:TDataDefinition; const AParent:TWinControl); static;
    procedure Refresh(const AData:TDataDefinition);
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Grid, BI.UI;

function TDataFormatEditor.CanChange(const AControl:TWinControl):Boolean;
begin
  result:=Showing and (not IChanging) and
          (
            (AControl=nil)
            or
            AControl.Showing
          );
end;

procedure TDataFormatEditor.CBDelimiterChange(Sender: TObject);
begin
  if CanChange then
  begin
    ECustomDelimiter.Enabled:=CBDelimiter.ItemIndex=3;

    case CBDelimiter.ItemIndex of
      0: IData['Delimiter']:='';
      1: IData['Delimiter']:=',';
      2: IData['Delimiter']:=#9;
    else
      IData['Delimiter']:=ECustomDelimiter.Text;
    end;
  end;
end;

procedure TDataFormatEditor.RefreshZip;
var tmp : String;
begin
  tmp:=IData['ZipPassword'];

  if tmp<>'' then
     EZipPassword.Text:=TCrypto.Decrypt(tmp);
end;

procedure TDataFormatEditor.RGJSONFormatClick(Sender: TObject);
begin
  if RGJSONFormat.ItemIndex=0 then
     IData['FORMAT']:=''
  else
     IData['FORMAT']:='ARRAY';
end;

procedure TDataFormatEditor.RGJSONStyleClick(Sender: TObject);
begin
  IData['Hierarchical']:=IntToStr(RGJSONStyle.ItemIndex);
end;

procedure TDataFormatEditor.RGXMLStyleClick(Sender: TObject);
begin
  IData['Hierarchical']:=IntToStr(RGXMLStyle.ItemIndex);
end;

procedure TDataFormatEditor.RefreshExcel;
begin
  UDExcelHeader.Position:=StrToIntDef(IData['ExcelHeaderCount'],0);
end;

procedure TDataFormatEditor.RefreshJSON;
begin
  RGJSONStyle.ItemIndex:=Ord(IData.AsBoolean('HIERARCHICAL',False));

  if SameText(IData['FORMAT'],'ARRAY') then
     RGJSONFormat.ItemIndex:=1
  else
     RGJSONFormat.ItemIndex:=0;
end;

procedure TDataFormatEditor.RefreshXML;
begin
  RGXMLStyle.ItemIndex:=Ord(IData.AsBoolean('HIERARCHICAL',False));
end;

procedure TDataFormatEditor.RefreshDatabase;
begin
  CBIncludeViews.Checked:=IData.AsDatabase.IncludeViews;
end;

procedure TDataFormatEditor.RefreshCSV;
var tmp : String;
begin
  ECustomDelimiter.Text:='';

  tmp:=IData['Delimiter'];

  if tmp='' then
     CBDelimiter.ItemIndex:=0
  else
  if tmp=',' then
     CBDelimiter.ItemIndex:=1
  else
  if tmp=#9 then
     CBDelimiter.ItemIndex:=2
  else
  begin
    CBDelimiter.ItemIndex:=3;
    ECustomDelimiter.Text:=tmp;
  end;

  CBDelimiterChange(Self);

  tmp:=IData['Quote'];

  if tmp='"' then
     CBQuote.ItemIndex:=1
  else
  if tmp='''' then
     CBQuote.ItemIndex:=2
  else
     CBQuote.ItemIndex:=0;

  tmp:=IData['Headers'];

  if SameText(tmp,'YES') then
     CBHeader.ItemIndex:=1
  else
  if SameText(tmp,'NO') then
     CBHeader.ItemIndex:=2
  else
     CBHeader.ItemIndex:=0;

  UDHeader.Position:=StrToIntDef(IData['HeaderCount'],1);

  EMissing.Text:=IData['MissingValue'];
end;

procedure TDataFormatEditor.CBHeaderChange(Sender: TObject);
begin
  if CanChange then
  case CBHeader.ItemIndex of
    0: IData['Headers']:='';
    1: IData['Headers']:='Yes';
  else
    IData['Headers']:='No';
  end;
end;

procedure TDataFormatEditor.CBIncludeViewsClick(Sender: TObject);
begin
  IData.AsDatabase.IncludeViews:=CBIncludeViews.Checked;
end;

procedure TDataFormatEditor.CBQuoteChange(Sender: TObject);
begin
  if CanChange then
  case CBQuote.ItemIndex of
    0: IData['Quote']:='';
    1: IData['Quote']:='"';
  else
    IData['Quote']:='''';
  end;
end;

procedure TDataFormatEditor.ECustomDelimiterChange(Sender: TObject);
begin
  if CanChange then
     IData['Delimiter']:=ECustomDelimiter.Text;
end;

procedure TDataFormatEditor.Edit1Change(Sender: TObject);
begin
  if CanChange then
     IData['HeaderCount']:=IntToStr(UDHeader.Position);
end;

procedure TDataFormatEditor.EExcelHeaderChange(Sender: TObject);
begin
  if CanChange(UDExcelHeader) then
     IData['ExcelHeaderCount']:=IntToStr(UDExcelHeader.Position);
end;

class procedure TDataFormatEditor.Embedd(const AData: TDataDefinition;
  const AParent: TWinControl);
var tmp : TDataFormatEditor;
begin
  tmp:=TDataFormatEditor.Create(AParent.Owner);
  tmp.Align:=TAlign.alClient;
  tmp.Refresh(AData);

  TUICommon.AddForm(tmp,AParent);
end;

procedure TDataFormatEditor.EMissingChange(Sender: TObject);
begin
  if CanChange then
     IData['MissingValue']:=EMissing.Text;
end;

procedure TDataFormatEditor.EZipPasswordChange(Sender: TObject);
begin
  if CanChange then
     IData['ZipPassword']:=TCrypto.Encrypt(EZipPassword.Text);
end;

procedure TDataFormatEditor.FormCreate(Sender: TObject);
begin
  IChanging:=True;
end;

procedure TDataFormatEditor.FormShow(Sender: TObject);
begin
  IChanging:=False;
end;

procedure TDataFormatEditor.Refresh(const AData:TDataDefinition);
begin
  IChanging:=True;
  try
    IData:=AData;

    RefreshDatabase;
    RefreshCSV;
    RefreshExcel;
    RefreshJSON;
    RefreshXML;
    RefreshZip;

    PageFormat.ActivePage:=TabCSV;
  finally
    IChanging:=False;
  end;
end;

end.
