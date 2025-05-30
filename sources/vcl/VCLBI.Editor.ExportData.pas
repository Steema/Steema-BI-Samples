{*********************************************}
{  TeeBI Software Library                     }
{  TDataItem Export Dialog                    }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.ExportData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BI.DataItem, BI.DataSource, Vcl.ExtCtrls, VCLBI.Editor.ListItems;

type
  TExportData = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    LSize: TLabel;
    LBFormat: TListBox;
    BCopy: TButton;
    BSave: TButton;
    CBSchema: TCheckBox;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Label3: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    BClose: TButton;
    procedure BSaveClick(Sender: TObject);
    procedure LBFormatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
  private
    { Private declarations }

    Data : TDataItem;

    IItems : TFormListItems;

    FExports : Array of TBIExport;

    function Current:TBIExport;
    function CurrentExtension:String;
    function DialogFilter:String;
    class function GetExportFormat(const AFile:String):TBIExport; static;
    function Prepare(const AExport:TBIExport; const AData:TDataItem):TBIExport;
    procedure ShowSize(const ASize:UInt64);
  public
    { Public declarations }

    class function HasAnyFormat:Boolean; static;

    class procedure SaveToFile(const AData:TDataItem; const AFile:String); overload; static;
    procedure SaveToFile(const AFile:String); overload;

    class procedure Show(const AOwner:TComponent; const AData:TDataItem); static;
  end;

implementation

{$R *.dfm}

uses
  VCL.Clipbrd, BI.UI,
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.IOUtils,
  {$ENDIF}
  BI.Arrays;

function TExportData.Prepare(const AExport:TBIExport; const AData:TDataItem):TBIExport;
begin
  AExport.Data:=AData;

  AExport.SchemaOnly:=CBSchema.Checked;

  AExport.Cursor.Items.Clear;
  AExport.Cursor.Add(IItems.Items);

  result:=AExport;
end;

function TExportData.Current:TBIExport;
begin
  result:=TBIExport(LBFormat.Items.Objects[LBFormat.ItemIndex]);
end;

procedure TExportData.BCopyClick(Sender: TObject);
var tmpS : String;
begin
  tmpS:=Prepare(Current,Data).AsString;
  Clipboard.AsText:=tmpS;

  ShowSize(Length(tmpS)*SizeOf(Char));
end;

procedure TExportData.ShowSize(const ASize:UInt64);
begin
  LSize.Caption:=TCommonUI.BytesToString(ASize);
end;

function TExportData.CurrentExtension:String;
var tmp : TFileFilters;
begin
  tmp:=Current.FileFilter;

  if tmp=nil then
     result:=''
  else
     result:='.'+tmp[0].FirstExtension;
end;

procedure TExportData.BSaveClick(Sender: TObject);
var tmp : String;
begin
  SaveDialog1.FilterIndex:=1+LBFormat.ItemIndex;

  SaveDialog1.FileName:='';
  SaveDialog1.FilterIndex:=LBFormat.ItemIndex+1;

  if SaveDialog1.Execute then
  begin
    tmp:=SaveDialog1.FileName;

    if TPath.GetExtension(tmp)='' then
       tmp:=TPath.ChangeExtension(tmp,CurrentExtension);

    SaveToFile(tmp);

    ShowSize(TBIFileSource.GetFileSize(tmp));
  end;
end;

class function TExportData.HasAnyFormat:Boolean;
var tmp : TBIExportClass;
begin
  for tmp in TBIExporters.Items do
      if Length(tmp.FileFilter)>0 then
         Exit(True);

  result:=False;
end;

procedure TExportData.FormCreate(Sender: TObject);

  procedure AddFormats;
  var tmpClass : TBIExportClass;
      tmp : TFileFilters;
      tmpExport : TBIExport;
      tmpFilter : TFileFilter;

      L : Integer;
  begin
    FExports:=nil;
    L:=0;

    LBFormat.Clear;

    for tmpClass in TBIExporters.Items do
    begin
      tmpExport:=tmpClass.Create;

      SetLength(FExports,L+1);
      FExports[L]:=tmpExport;
      Inc(L);

      tmp:=tmpExport.FileFilter;

      for tmpFilter in tmp do
          LBFormat.Items.AddObject(tmpFilter.Description,tmpExport);
    end;
  end;

begin
  LBFormat.Items.BeginUpdate;
  try
    //LBFormat.Sorted:=False;
    AddFormats;
    //LBFormat.Sorted:=True; <-- breaks savedialog FilterIndex
  finally
    LBFormat.Items.EndUpdate;
  end;

  SaveDialog1.Filter:=DialogFilter;

  IItems:=TFormListItems.Embed(Self,Self);
end;

function TExportData.DialogFilter:String;
var tmp,
    tmpAll : String;
    t : Integer;
begin
  tmp:='';
  tmpAll:='';

  for t:=0 to High(FExports) do
      FExports[t].FileFilter.ToDialogFilter(tmp,tmpAll);

  result:=tmp;
end;

procedure TExportData.FormDestroy(Sender: TObject);
var t : Integer;
begin
  for t:=0 to High(FExports) do
      FExports[t].Free;
end;

procedure TExportData.LBFormatClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBFormat.ItemIndex;

  BSave.Enabled:=(Data<>nil) and (tmp<>-1);
  BCopy.Enabled:=BSave.Enabled and (not Current.BinaryOnly);
end;

class function TExportData.GetExportFormat(const AFile:String):TBIExport;

  procedure DoError;
  begin
    raise EBIException.Create('Error: Cannot save data to file: '+AFile+' Extension not supported')
  end;

var tmp : TBIExportClass;
begin
  result:=nil;

  tmp:=TBIExporters.GuessExtension(TPath.GetExtension(AFile));

  if tmp=nil then
     DoError
  else
     result:=tmp.Create;
end;

class procedure TExportData.SaveToFile(const AData: TDataItem; const AFile: String);
var tmp : TBIExport;
begin
  tmp:=GetExportFormat(AFile);

  if tmp<>nil then
  try
    tmp.Data:=AData;
    tmp.SaveToFile(AFile);
  finally
    tmp.Free;
  end;
end;

procedure TExportData.SaveToFile(const AFile: String);
var tmp : TBIExport;
begin
  tmp:=GetExportFormat(AFile);

  if tmp<>nil then
  try
    Prepare(tmp,Data);
    tmp.SaveToFile(AFile);
  finally
    tmp.Free;
  end;
end;

class procedure TExportData.Show(const AOwner: TComponent;
  const AData: TDataItem);
begin
  with TExportData.Create(AOwner) do
  try
    Data:=AData;

    if Data<>nil then
    begin
      IItems.AddItems(AData.Items.AsArray,False);
      IItems.CheckAll;
    end;

    ShowModal;
  finally
    Free;
  end;
end;

end.
