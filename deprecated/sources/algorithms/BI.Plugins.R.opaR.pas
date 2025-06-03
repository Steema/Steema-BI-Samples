{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for opaR native Delphi R language   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Plugins.R.opaR;

// Note: When using this unit, TeeBI automatically will plug with
//       the "opaR" library to pass/get data to R scripts.

{
  https://github.com/SigmaSciences/opaR

  opaR (object pascal for R) is a port of R.NET 1.6.5 to Embarcadero Delphi,
  allowing you to integrate the popular R statistical language into your Delphi apps.
}

// "R language" installer for Windows (32bit and 64bit) can be downloaded from:

// https://cran.r-project.org/bin/windows/base

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, BI.Plugins.R,
  opaR.Interfaces, opaR.Engine, opaR.Devices.NullCharacterDevice, opaR.Utils;

type
  TOpaR=class(TBIREngine)
  private
    function CreateArray<T>(const Index:TNativeIntArray; const Value:TArray<T>):TArray<T>;
  protected
    function Finish:Boolean; override;
  public
    Constructor Create;

    procedure AddVariable(const AName: String; const Values: TArray<Double>); overload;
    procedure AddVariable(const AName:String; const Index:TNativeIntArray;
                          const AData:TDataArray; const UseMissing:Boolean=True); override;

    procedure GetVariable(const AName:String; const AData:TDataItem); override;
    procedure LoadPackage(const APackage:String); override;
    procedure ParseOutput(const ADest:TDataItem); override;
    procedure ParseRawMap(const AMap,ADest:TDataItem); override;
    procedure Statement(const AStatement:String); override;
    function Version:String; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils, opaR.Vector, opaR.CharacterVector, opaR.IntegerVector,
  opaR.NumericVector, opaR.LogicalVector,

  // For TInt64Vector
  opaR.SEXPREC, opaR.DLLFunctions, opaR.ProtectedPointer,
  opaR.DataFrame, opaR.GenericVector, opaR.SymbolicExpression,

  opaR.StartupParameter, opaR.Matrix;

type
  TInt64Vector=class(TIntegerVector)
  private
    allocVec: TRfnAllocVector;
  public
    Constructor Create(const engine: IREngine; const vector: TArray<Int64>);
  end;

Constructor TInt64Vector.Create(const engine: IREngine; const vector: TArray<Int64>);
var
  pExpr: PSEXPREC;
  L,
  t : Integer;

  pp: TProtectedPointer;
//  PData: PInteger;
//  offset: integer;
begin
  // -- There's no base constructor that uses a TArray parameter, so build
  // -- everything we need here.

  if not Assigned(allocVec) then
     allocVec := GetProcAddress(engine.Handle, 'Rf_allocVector');

  // -- First get the pointer to the R expression.
  pExpr := allocVec(TSymbolicExpressionType.IntegerVector, Length(vector));

  inherited Create(engine, pExpr);

  // Copy data loop
  L:=Low(vector);

  pp := TProtectedPointer.Create(Self);
  try
    for t:=L to High(vector) do
    begin
      ValueByIndex[t-L]:=vector[t];
      {
      offset := GetOffset(t-L);
      PData := PInteger(NativeInt(DataPointer) + offset);
      PData^ := vector[t];
      }
    end;
  finally
    pp.Free;
  end;
end;

type
  TFloatVector=class(TNumericVector)
  private
    allocVec: TRfnAllocVector;
  public
    Constructor Create(const engine: IREngine; const vector: TArray<Single>); overload;
    Constructor Create(const engine: IREngine; const vector: TArray<Extended>); overload;
  end;

Constructor TFloatVector.Create(const engine: IREngine; const vector: TArray<Single>);
var
  pExpr: PSEXPREC;
  L,
  t : Integer;

  pp: TProtectedPointer;
//  PData: PDouble;
//  offset: integer;
begin
  // -- There's no base constructor that uses a TArray parameter, so build
  // -- everything we need here.

  if not Assigned(allocVec) then
     allocVec := GetProcAddress(engine.Handle, 'Rf_allocVector');

  // -- First get the pointer to the R expression.
  pExpr := allocVec(TSymbolicExpressionType.NumericVector, Length(vector));

  inherited Create(engine, pExpr);

  // Copy data loop
  L:=Low(vector);

  pp := TProtectedPointer.Create(Self);
  try
    for t:=L to High(vector) do
    begin
      ValueByIndex[t-L]:=vector[t];
      {
      offset := GetOffset(t-L);
      PData := PDouble(NativeInt(DataPointer) + offset);
      PData^ := vector[t];
      }
    end;
  finally
    pp.Free;
  end;
end;

Constructor TFloatVector.Create(const engine: IREngine; const vector: TArray<Extended>);
var
  pExpr: PSEXPREC;
  L,
  t : Integer;

  pp: TProtectedPointer;
//  PData: PDouble;
//  offset: integer;
begin
  // -- There's no base constructor that uses a TArray parameter, so build
  // -- everything we need here.

  if not Assigned(allocVec) then
     allocVec := GetProcAddress(engine.Handle, 'Rf_allocVector');

  // -- First get the pointer to the R expression.
  pExpr := allocVec(TSymbolicExpressionType.NumericVector, Length(vector));

  inherited Create(engine, pExpr);

  // Copy data loop
  L:=Low(vector);

  pp := TProtectedPointer.Create(Self);
  try
    for t:=L to High(vector) do
    begin
      ValueByIndex[t-L]:=vector[t];
      {
      offset := GetOffset(t-L);
      PData := PDouble(NativeInt(DataPointer) + offset);
      PData^ := vector[t];
      }
    end;
  finally
    pp.Free;
  end;
end;

type
  TBooleanVector=class(TLogicalVector)
  private
    allocVec: TRfnAllocVector;
  public
    Constructor Create(const engine: IREngine; const vector: TArray<Boolean>); overload;
  end;

Constructor TBooleanVector.Create(const engine: IREngine; const vector: TArray<Boolean>);
var
  pExpr: PSEXPREC;
  L,
  t : Integer;

  pp: TProtectedPointer;
  //PData: PLongBool;
  //offset: integer;
begin
  // -- There's no base constructor that uses a TArray parameter, so build
  // -- everything we need here.

  if not Assigned(allocVec) then
     allocVec := GetProcAddress(engine.Handle, 'Rf_allocVector');

  // -- First get the pointer to the R expression.
  pExpr := allocVec(TSymbolicExpressionType.LogicalVector, Length(vector));

  inherited Create(engine, pExpr);

  // Copy data loop
  L:=Low(vector);

  pp := TProtectedPointer.Create(Self);
  try
    for t:=L to High(vector) do
    begin
      ValueByIndex[t-L]:=vector[t];
      {
      offset := GetOffset(t);
      PData := PLongBool(NativeInt(DataPointer) + offset);
      PData^ := vector[t];
      }
    end;
  finally
    pp.Free;
  end;
end;

type
  TStringsDevice = class(TNullCharacterDevice, ICharacterDevice)
  public
    procedure WriteConsole(output: string; length: integer; outputType: TConsoleOutputType);
  end;

{ TopaR }

var
  R : IREngine = nil;

Constructor TopaR.Create;
var tmpParam : TStartupParameter;
begin
  inherited Create;

  if R=nil then
  begin
    TREngine.SetEnvironmentVariables;

    tmpParam:=TStartupParameter.Create;

    tmpParam.Start.Common.R_Quiet:=True;
    tmpParam.Start.Common.R_Slave:=True;
    tmpParam.Start.Common.R_Interactive:=True;
    tmpParam.Start.Common.R_Verbose:=not tmpParam.Start.Common.R_Quiet;

  //???  tmpParam.Vanilla:=True;

    R:=TREngine.GetInstance({'',}True,tmpParam,TStringsDevice.Create as ICharacterDevice);
  end;
end;

function TopaR.CreateArray<T>(const Index:TNativeIntArray; const Value:TArray<T>):TArray<T>;
var c,L : Integer;
begin
  if Index=nil then
     result:=Value
  else
  begin
    SetLength(result,Length(Index));
    L:=Low(Index);

    for c:=L to High(Index) do
        result[c-L]:=Value[Index[c]];
  end;
end;

procedure TopaR.AddVariable(const AName: String; const Values: TArray<Double>);
begin
  R.SetSymbol(AName,TNumericVector.Create(R,Values));
end;

procedure TopaR.AddVariable(const AName: String; const Index: TNativeIntArray;
  const AData: TDataArray; const UseMissing: Boolean);

  function CreateSymbol(const AData:TDataItem):ISymbolicExpression;
  begin
    // Ensure arrays are loaded
    AData.Load;

    case AData.Kind of
         dkInt32: result:=TIntegerVector.Create(R,CreateArray<Integer>(Index,TArray<Integer>(AData.Int32Data)));
         dkInt64: result:=TInt64Vector.Create(R,CreateArray<Int64>(Index,TArray<Int64>(AData.Int64Data)));
        dkSingle: result:=TFloatVector.Create(R,CreateArray<Single>(Index,TArray<Single>(AData.SingleData)));
        dkDouble: result:=TNumericVector.Create(R,CreateArray<Double>(Index,TArray<Double>(AData.DoubleData)));
      dkExtended: result:=TFloatVector.Create(R,CreateArray<Extended>(Index,TArray<Extended>(AData.ExtendedData)));
          dkText: result:=TCharacterVector.Create(R,CreateArray<String>(Index,TArray<String>(AData.TextData)));
      dkDateTime: result:=TNumericVector.Create(R,CreateArray<Double>(Index,TArray<Double>(AData.DateTimeData)));
       dkBoolean: result:=TBooleanVector.Create(R,CreateArray<Boolean>(Index,TArray<Boolean>(AData.BooleanData)));
       dkUnknown: result:=nil;
    end;
  end;

  function AllNames:String;
  var t : TLoopInteger;
  begin
    result:='';

    for t:=Low(AData) to High(AData) do
    begin
      result:=result+AName+IntToStr(t);

      if t<High(AData) then
         result:=result+',';
    end;
  end;

var L,t : Integer;
    tmp : TArray<IDynamicVector>;
//    tmpFrame : IDataFrame;
begin
  if High(AData)=0 then
     R.SetSymbol(AName, CreateSymbol(AData[0]))
  else
  begin
    // R cbind

    SetLength(tmp,AData.Count);

    L:=Low(AData);

    for t:=L to High(AData) do
    begin
      tmp[t]:=CreateSymbol(AData[t-L]).AsVector;

      R.SetSymbol(AName+IntToStr(t), tmp[t] as ISymbolicExpression);
    end;

    R.Evaluate(AName+'=cbind('+AllNames+')');

    {
    tmpFrame:=TDataFrame.Create(R, TSymbolicExpressionType.ExpressionVector, Length(tmp));
    tmpFrame.SetVectorDirect(tmp);
    R.SetSymbol(AName, (tmpFrame as ISymbolicExpression));
    }
  end;
end;

function TopaR.Finish: Boolean;
begin
  result:=True;
end;

type
  TDataAccess=class(TDataItem);

procedure TopaR.GetVariable(const AName: String; const AData: TDataItem);

  procedure GetVector(const AData:TDataItem; const AVector: ISymbolicExpression);

    function DataFromVector(const AVector:IGenericVector):TDataItem; overload;
    begin
      result:=TDataItem.Create; // ???
    end;

    procedure ChangeAndResize(const AKind:TDataKind; const ASize:Integer);
    begin
      TDataAccess(AData).FKind:=AKind;
      AData.Resize(ASize);
    end;

  var tmpInt : IIntegerVector;
      tmpFloat : INumericVector;
      tmpChar : ICharacterVector;
      tmpLogical : ILogicalVector;

      tmpList : IGenericVector;
      tmpListVectors : TArray<ISymbolicExpression>;

      tmpNames : ISymbolicExpression;

      tmpNamesData,
      tmpData : TDataItem;

      pp: TProtectedPointer;

      t : TLoopInteger;
  begin
    case AVector.Type_ of
     TSymbolicExpressionType.IntegerVector:
       begin
         tmpInt:=AVector.AsInteger;
         ChangeAndResize(TDataKind.dkInt32,tmpInt.VectorLength);

         // pp:=TProtected?
         TArray<Integer>(AData.Int32Data):=tmpInt.ToArray;
         //tmpInt.CopyTo(TArray<Integer>(AData.Int32Data),AData.Count);
       end;

     TSymbolicExpressionType.NumericVector:
       begin
         tmpFloat:=AVector.AsNumeric;
         ChangeAndResize(TDataKind.dkDouble,tmpFloat.VectorLength);

         // pp:=TProtected?
         TArray<Double>(AData.DoubleData):=tmpFloat.ToArray;
         //tmpFloat.CopyTo(TArray<Double>(AData.DoubleData),AData.Count);
       end;

     TSymbolicExpressionType.CharacterVector:
       begin
         tmpChar:=AVector.AsCharacter;
         ChangeAndResize(TDataKind.dkText,tmpChar.VectorLength);

         // pp:=TProtected?
         TArray<String>(AData.TextData):=tmpChar.ToArray;
         //tmpChar.CopyTo(TArray<String>(AData.TextData),AData.Count);
       end;

     TSymbolicExpressionType.LogicalVector:
       begin
         tmpLogical:=AVector.AsLogical;
         ChangeAndResize(TDataKind.dkBoolean,tmpLogical.VectorLength);

         for t:=0 to AData.Count-1 do
             AData.BooleanData[t]:=tmpLogical[t];
       end;

     TSymbolicExpressionType.List:
       begin

         tmpList:=AVector.AsList;
         tmpListVectors:=tmpList.ToArray;

         tmpNames:=AVector.GetAttribute('names');

         tmpNamesData:=nil;

         if tmpNames<>nil then
            if tmpNames.IsVector then
            begin
              tmpNamesData:=TDataItem.Create;
              GetVector(tmpNamesData,tmpNames);
            end;

         try
           AData.AsTable:=True;

           for t:=0 to tmpList.VectorLength-1 do
           begin
             tmpData:=TDataItem.Create;

             if tmpNamesData<>nil then
                tmpData.Name:=tmpNamesData.DataToString(t);

             GetVector(tmpData,tmpListVectors[t]);
             AData.Items.Add(tmpData);
           end;

         finally
           tmpNamesData.Free;
         end;

         //tmpNames:=tmpList.SetNames.ColumnNames;
         //for t:=0 to tmpFrame.ColumnCount-1 do
         //    AData.Items[t].Name:=tmpNames[t];

         if AData.Items.Count>0 then
            TDataAccess(AData).FCount:=AData.Items[0].Count;
       end;

     TSymbolicExpressionType.LanguageObject: ; // ??

    else
      raise EBIException.Create('Error: Cannot get R vector variable: '+
                   AName+' Unsupported type: '+IntToStr(Ord(AVector.Type_)) );
    end;
  end;

  procedure GetDataFrame(const AData:TDataItem; const AFrame:IDataFrame);

    function DataFromVector(const AVector:IDynamicVector):TDataItem; overload;
    begin
      result:=TDataItem.Create; // ???
    end;

  var tmpVectors : TArray<IDynamicVector>;
      tmpNames : TArray<String>;
      t : Integer;
  begin
    tmpVectors:=AFrame.ToArray;

    AData.AsTable:=True;

    for t:=0 to AFrame.ColumnCount-1 do
        AData.Items.Add(DataFromVector(tmpVectors[t]));

    tmpNames:=AFrame.ColumnNames;

    for t:=0 to AFrame.ColumnCount-1 do
        AData.Items[t].Name:=tmpNames[t];

    if AData.Items.Count>0 then
       TDataAccess(AData).FCount:=AData.Items[0].Count;
  end;

var tmp : ISymbolicExpression;
begin
  tmp:=R.GetSymbol(AName);

  if tmp.IsVector then
     GetVector(AData,tmp)
  else
  if tmp.IsDataFrame then
     GetDataFrame(AData,tmp.AsDataFrame);
end;

procedure TopaR.LoadPackage(const APackage: String);
var tmp : TStrings;
begin
  tmp:=TStringList.Create;
  try
    AddPackage(tmp,APackage);
    R.Evaluate(tmp.Text);
  finally
    tmp.Free;
  end;
end;

procedure TopaR.ParseOutput(const ADest: TDataItem);
begin
end;

procedure TopaR.ParseRawMap(const AMap, ADest: TDataItem);
begin
end;

procedure TopaR.Statement(const AStatement: String);
begin
  R.Evaluate(AStatement);
end;

function TopaR.Version: String;
begin
  result:=String(TRapi.DLLVersion);
end;

{ TStringsDevice }

procedure TStringsDevice.WriteConsole(output: string; length: integer;
  outputType: TConsoleOutputType);
begin
  if TBIREngine.Output<>nil then
     TBIREngine.Output.Add(output);
end;

initialization
  TBIREngine.Engine:=TopaR.Create;
end.
