{*********************************************}
{  TeeBI Software Library                     }
{  Microsoft Excel data import and export     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Excel;

interface

uses
  System.Classes, System.SysUtils,
  {$IFDEF MSWINDOWS}
  WinAPI.ActiveX, System.Win.ComObj,
  {$ENDIF}
  BI.DataSource, BI.DataItem, BI.Arrays, BI.Persist;

type
  EExcelException=class(EBIException);

  TBIExcel=class(TBITextSource)
  private
    FHeaderCount : Integer;
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    Range     : String;
    WorkSheet : String;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    class function FileFilter: TFileFilters; override;
    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;

    class function Supports(const Extension: String): Boolean; override;

    property HeaderCount:Integer read FHeaderCount write FHeaderCount default 1;
  end;

  TBIExcelEngineClass=class of TBIExcelEngine;

  TBIExcelEngine=class
  public
    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); virtual;
  end;

  TBIExcelExport=class(TBIExport)
  public
    class
      var Engine : TBIExcelEngineClass;

    Constructor Create; override;

    class function FileFilter: TFileFilters; override;

    procedure SaveToFile(const AFileName:String); override;
    class function Supports(const Extension: String): Boolean; override;
  end;

implementation

uses
  System.Types, System.Variants, BI.CSV;

const
  ExcelNotInstalled='Cannot start Microsoft Excel. Is it correctly installed?';

const
  ExcelApp='Excel.Application';

function IsExcel_Installed:Boolean;
{$IFDEF MSWINDOWS}
var tmp : String;
    ID  : TCLSID;
begin
  tmp:=ExcelApp;
  result:=CLSIDFromProgID(PWideChar(WideString(tmp)),ID)=S_OK;
{$ELSE}
begin
  result:=True; // <-- assume True
{$ENDIF}
end;

function OpenExcel(const AFileName:String; out Excel:Variant):Boolean;

  {$IFDEF MSWINDOWS}
  function GetExcel:IDispatch;
  const
    MK_E_UNAVAILABLE=-2147221021;
  var
    ClassID: TGUID;
    Unknown: IUnknown;
    HR: HResult;
  begin
    ClassID:=ProgIDToClassID(ExcelApp);
    HR:=GetActiveObject(ClassID, nil, Unknown);

    if HR=MK_E_UNAVAILABLE then
       result := CreateComObject(ClassID) as IDispatch
    else
    begin
      OleCheck(HR);
      OleCheck(Unknown.QueryInterface(IDispatch, Result));
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  Excel:=GetExcel;


  (*
  // Alternative way to start Excel:
  try
    //tmp:=nil;
    //CoInitialize(tmp);

    Excel:=GetActiveOleObject(ExcelApp);
  except
    try
      Excel:=CreateOleObject(ExcelApp);
    except
    on E:Exception do
       Raise EExcelException.Create(ExcelNotInstalled+#13#13+E.Message);
    end;
  end;
  *)
  {$ENDIF}

  if VarIsEmpty(Excel) then
     raise EExcelException.Create(ExcelNotInstalled);

  Excel.Interactive:=False;
  Excel.Visible:=False;

  if AFileName<>'' then
     Excel.Workbooks.Open(FileName:=AFileName, ReadOnly:=True);

  //Excel.ScreenUpdating:=False;
  //Excel.Calculation:=0;

  result:=True;
end;

// Example: ARange = 'A1:H100'
constructor TBIExcel.Create(const Definition: TDataDefinition;
  const MultiThread: Boolean);
var tmp : String;
begin
  inherited;

  FHeaderCount:=1;

  if Definition<>nil then
  begin
    tmp:=Definition['ExcelHeaderCount'];

    if tmp<>'' then
       FHeaderCount:=StrToIntDef(tmp,FHeaderCount);
  end;
end;

function TBIExcel.DoImportFile(const FileName:String):TDataArray;
var Excel : Variant;

  function RangeToStrings(const ARange:Variant):TStrings;
  var x0,x1 : Integer;
      Cells : Variant;

    function ParseRow(const Y:Integer):String;
    var x : Integer;
        Value : String;
        tmpResult : HResult;
        tmp : Variant;
    begin
      result:='';

      for x:=x0 to x1 do
      begin
        tmp:=Cells[Y,x];

        if VarIsError(tmp,tmpResult) then
           Value:=''
        else
           Value:=tmp;

        if result='' then
           result:=Value
        else
           result:=result+#9+Value;
      end;
    end;

  var y : Integer;
      y0,y1 : Integer;
  begin
    Cells:=ARange.Cells.Value;

    if VarIsClear(Cells) or VarIsNull(Cells) then
       result:=nil
    else
    begin
      result:=TStringList.Create;
      result.BeginUpdate;
      try
        y0:=VarArrayLowBound(Cells,1);
        y1:=VarArrayHighBound(Cells,1);

        x0:=VarArrayLowBound(Cells,2);
        x1:=VarArrayHighBound(Cells,2);

        for y:=y0 to y1 do
            result.Add(ParseRow(y));
      finally
        result.EndUpdate;
      end;
    end;
  end;

  function ImportSheet(const Sheet:Variant):TDataArray;
  var tmpRange : Variant;
      Strings : TStrings;
      CSV : TBICSV;
  begin
    result:=nil;

    if not VarIsNull(Sheet) then
    if (WorkSheet='') or SameText(WorkSheet,Sheet.Name) then
    begin
      if Range<>'' then
         tmpRange:=Sheet.Range(Range)
      else
         tmpRange:=Sheet.UsedRange;

      if not VarIsNull(tmpRange) then
      begin
        Strings:=RangeToStrings(tmpRange);

        if Strings<>nil then
        try
          CSV:=TBICSV.Create(IDefinition);
          try
            CSV.Delimiter:=#9;
            CSV.Header.Count:=HeaderCount;

            result:=CSV.Import(Strings);

            if Length(result)=1 then
               result[0].Name:=Sheet.Name;
          finally
            CSV.Free;
          end;
        finally
          Strings.Free;
        end;
      end;
    end;
  end;

  procedure CloseExcel;
  begin
    Excel.DisplayAlerts:=False;
    Excel.Workbooks.Close;
    Excel.Quit;
  end;

var t,tt,L : Integer;
    Data : TDataArray;
begin
  result:=nil;

  if IsExcel_Installed then
  begin
    // Just in case we're being called from a Thread:
    {$IFDEF MSWINDOWS}
    CoInitialize(nil);
    {$ENDIF}
    try
      if OpenExcel(FileName,Excel) then
      try
        for t:=1 to Excel.WorkSheets.Count do
        begin
          Data:=ImportSheet(Excel.WorkSheets[t]);

          if Data<>nil then
          begin
            L:=Length(result);
            SetLength(result,L+Data.Count);

            for tt:=0 to High(Data) do
                result[L+tt]:=Data[tt];
          end;
        end;

        if Length(result)=1 then
           result[0].Name:=NameOfFile(FileName);

      finally
        CloseExcel;
      end
      else
        Raise EExcelException.Create('Cannot import from Excel file. Cannot open file: '+FileName)
    finally
      {$IFDEF MSWINDOWS}
      CoUninitialize;
      {$ENDIF}
    end;
  end
  else
     Raise EExcelException.Create('Cannot import from Excel file. Excel not installed: '+FileName)
end;

function TBIExcel.Import(const Folder:String; Recursive: Boolean): TDataArray;
begin
  result:=Import(Folder,'*.xls',ExcludePattern,Recursive);
end;

class function TBIExcel.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('Microsoft Excel files','*.xls;*.xlsx');
end;

class function TBIExcel.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.XLS') or SameText(Extension,'.XLSX');
end;

{ TBIExcelExport }

// XLSX format, ODS format

Constructor TBIExcelExport.Create;
begin
  inherited;
  BinaryOnly:=True;
end;

class function TBIExcelExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('Microsoft Excel','*.xls;*.xlsx');
end;

procedure TBIExcelExport.SaveToFile(const AFileName: String);
begin
  Engine.SaveToFile(Data,AFileName);
end;

class function TBIExcelExport.Supports(const Extension: String): Boolean;
begin
  result:=TBIExcel.Supports(Extension);
end;

{ TBIExcelEngine }

class procedure TBIExcelEngine.SaveToFile(const AData: TDataItem;
  const AFileName: String);
var
  WorkBook : Variant;

  // Excel restriction:
  function CorrectSheetName(const S:String):String;
  begin
    if S='' then
       result:='Sheet1'
    else
       result:=S;

    if Length(result)>31 then
       result:=Copy(result,1,31);
  end;

  function AddNewSheet(const AName:String):Variant;
  begin
    result:=Workbook.Worksheets.Add;
    result.Activate;
    result.Select;

    if AName<>'' then
       result.Name:=CorrectSheetName(AName);
  end;

  // Return the number of table-subtable levels in all items:
  function CalcLevels(const AData:TDataItem):Integer;
  var tmp : TDataItem;
      tmpCount : Integer;
  begin
    if AData.AsTable then
    begin
      result:=1;

      for tmp in AData.Items.AsArray do
      begin
        if tmp.AsTable then
        begin
          tmpCount:=CalcLevels(tmp)+1;

          if tmpCount>result then
             result:=tmpCount;
        end;
      end;
    end
    else
       result:=0;
  end;

  procedure AddValues(const ASheet:Variant; const AData:TDataItem; const FirstRow,ACol:Integer);
  var t : TLoopInteger;
  begin
    for t:=0 to AData.Count-1 do
        if not AData.Missing[t] then
           ASheet.Cells[FirstRow+t,ACol].Value:=AData.DataToString(t);
  end;

  procedure AddTable(const AData:TDataItem);
  var Sheet : Variant;
      FirstRow : Integer;
      tmpRow : Integer;
      t : Integer;
      tt : TLoopInteger;
      tmp : TDataItem;
      tmpData : TDataItem;
  begin
    Sheet:=AddNewSheet(AData.Name);

    FirstRow:=CalcLevels(AData)+1;

    for t:=0 to AData.Items.Count-1 do
    begin
      tmp:=AData.Items[t];
      tmpData:=tmp;

      // Add headers

      tmpRow:=1;

      repeat
        Sheet.Cells[tmpRow,1+t].Value:=tmp.Name;

        if tmp.AsTable then
        begin
          tmpData:=tmp;

          if tmp.Items.Count>0 then
          begin
            tmp:=tmp.Items[0];
            Inc(tmpRow);
          end;
        end;

      until not tmp.AsTable;

      // Add values

      if tmpData.AsTable then
         for tt:=0 to tmpData.Items.Count-1 do
         begin
           Sheet.Cells[tmpRow,1+t+tt].Value:=tmpData.Items[tt].Name;
           AddValues(Sheet,tmpData.Items[tt],FirstRow,1+t+tt);
         end
      else
         AddValues(Sheet,tmpData,FirstRow,1+t);
    end;
  end;

  procedure AddColumn(const AData:TDataItem);
  begin
    AddValues(AddNewSheet(AData.Name),AData,1,1);
  end;

  procedure AddData(const AData:TDataItem);
  var tmp : TDataItem;
  begin
    if AData.AsTable then
       AddTable(AData)
    else
    if AData.Kind=TDataKind.dkUnknown then
    begin
      for tmp in AData.Items.AsArray do
          AddData(tmp);
    end
    else
      AddColumn(AData);
  end;

const
  // https://msdn.microsoft.com/en-us/library/bb241279(v=office.12).aspx
  xlOpenXMLWorkbook=51;

var Excel : Variant;
    OldThousand,
    OldDecimal : Char;
begin
  if IsExcel_Installed then
  begin
    // Just in case we're being called from a Thread:
    {$IFDEF MSWINDOWS}
    CoInitialize(nil);
    {$ENDIF}
    try
      if OpenExcel('',Excel) then
      try
        Workbook:=Excel.WorkBooks.Add;

        // Set number separators as US format
        OldThousand:=FormatSettings.ThousandSeparator;
        OldDecimal:=FormatSettings.DecimalSeparator;

        FormatSettings.ThousandSeparator:=',';
        FormatSettings.DecimalSeparator:='.';
        try
          AddData(AData);
        finally
          FormatSettings.ThousandSeparator:=OldThousand;
          FormatSettings.DecimalSeparator:=OldDecimal;
        end;

        WorkBook.SaveAs(AFileName, xlOpenXMLWorkbook, '', '', False, False);

        Excel.DisplayAlerts:=False;
        Workbook.Close(SaveChanges:=False);
      finally
        Excel.Quit;
      end;
    finally
      {$IFDEF MSWINDOWS}
      CoUninitialize;
      {$ENDIF}
    end;
  end
  else
     Raise EExcelException.Create('Cannot import from Excel file. Excel not installed: '+AFileName)
end;

initialization
  TBIExcelExport.Engine:=TBIExcelEngine;
  TBIFileImporters.RegisterClass(TBIExcel);
  TBIExporters.RegisterClass(TBIExcelExport);
finalization
  TBIExcelExport.Engine:=nil;
  TBIExporters.UnRegisterClass(TBIExcelExport);
  TBIFileImporters.UnRegisterClass(TBIExcel);
end.
