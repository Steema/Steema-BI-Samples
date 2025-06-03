{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for Python language                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Plugins.Python;

interface

uses
  System.Classes, BI.Plugins.Python.Engine, BI.Algorithm.Model, BI.Arrays,
  BI.DataItem;

type
  TBIPython=class(TBIPlugin)
  private
    class var FEngine : TPythonEngine;
    class var FOutput : TStrings;

    class var PyNDArray_Type : PPyObject;

    class function GetEngine:TPythonEngine; static;
  public
    class property Engine:TPythonEngine read GetEngine;
    class property Output:TStrings read FOutput write FOutput;

    class function DataRowToVector(const AIndex:TInteger; const AData:TDataArray;
                      const UseMissing:Boolean=True):String; static;

    class function Evaluate(const AText:String):PPyObject; static;
    class function EvaluateAsString(const AText:String):String; static;

    class function Execute(const AScript:TStrings):Boolean; overload; static;
    class function Execute(const AText:String):Boolean; overload; static;

    class function FromData(const AName:String; const AData:TDataArray; const Index:TNativeIntArray=nil;
                      const UseMissing:Boolean=True):String; overload; static;

    class function FromData(const AName:String; const AData:TDataItem; const Index:TNativeIntArray=nil;
                      const UseMissing:Boolean=True):String; overload; static;

    // Pending:
    // class function FromData(...) : Int64;  <--- return pointer address

    class function GetData(const AName:String):TDataItem; static;
  end;

  // Base class for Scikit-learn algorithms
  TScikitSupervisedModel=class(TSupervisedModel)
  protected
    function BuildScript:TStrings; virtual; abstract;
    function Fit:Boolean;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}
  System.IOUtils, System.Types, System.SysUtils, BI.Persist;

{ TBIPython }

class function TBIPython.Execute(const AScript: TStrings): Boolean;
begin
  Engine.ExecStrings(AScript);
  result:=True;
end;

type
  TBIPythonIO=class(TPythonInputOutput)
  private
    procedure Display(const S:String);
  protected
    procedure SendData(const Data:AnsiString); override;
  end;

function TypeOfArray(const AName:String):TDataKind;
var T : String;
begin
  T:=TBIPython.EvaluateAsString(AName+'.dtype');

  if SameText(T,'int32') then
     result:=TDataKind.dkInt32
  else
  if SameText(T,'int64') then
     result:=TDataKind.dkInt64
  else
  if SameText(T,'float32') then
     result:=TDataKind.dkSingle
  else
  if SameText(T,'float64') then
     result:=TDataKind.dkDouble
  else
  begin
    T:=TBIPython.EvaluateAsString(AName+'.dtype.kind');

    if T='U' then
       result:=TDataKind.dkText
    else
       result:=TDataKind.dkUnknown;
  end;

  {$IFOPT C+}
  TBIPython.Engine.CheckError;
  {$ENDIF}
end;

function IsDateTime(const AItem:PPyObject):Boolean;
begin
  result:=TBIPython.Engine.PyTimeStruct_Check(AItem) or
          TBIPython.Engine.PyDate_Check(AItem) or
          TBIPython.Engine.PyDateTime_Check(AItem) or
          TBIPython.Engine.PyTime_Check(AItem);
end;

function TypeOfListItem(const AItem:PPyObject):TDataKind;
var tmp : PPyObject;
begin
  tmp:=TBIPython.Engine.PyList_GetItem(AItem,0);

  if TBIPython.Engine.PyFloat_Check(tmp) then
     result:=TDataKind.dkDouble
  else
  if TBIPython.Engine.PyBool_Check(tmp) then
     result:=TDataKind.dkBoolean
  else
  if TBIPython.Engine.PyLong_Check(tmp) then
     result:=TDataKind.dkInt64
  else
  if TBIPython.Engine.PyInt_Check(tmp) then
     result:=TDataKind.dkInt32
  else
  if TBIPython.Engine.PyUnicode_Check(tmp) or
     TBIPython.Engine.PyString_Check(tmp) then
     result:=TDataKind.dkText
  else
  if IsDateTime(tmp) then
     result:=TDataKind.dkDateTime
  else
     result:=TDataKind.dkUnknown;

  {$IFOPT C+}
  TBIPython.Engine.CheckError;
  {$ENDIF}
end;

type
  TDataAccess=class(TDataItem);

class function TBIPython.GetData(const AName: String): TDataItem;

  function AsLong(const AObject:PPyObject):Int64;
  begin
    if AObject=nil then
       result:=0
    else
    try
      if Engine.PyLong_Check(AObject) then
         result:=Engine.PyLong_AsLongLong(AObject)
      else
      if Engine.PyInt_Check(AObject) then
         result:=Engine.PyInt_AsLong(AObject)
      else
         result:=0;
    finally
      Engine.Py_XDECREF(AObject);
    end;
  end;

  function AsFloat(const AObject:PPyObject):Double;
  begin
    if AObject=nil then
       result:=0
    else
    try
      if Engine.PyFloat_Check(AObject) then
         result:=Engine.PyFloat_AsDouble(AObject)
      else
         result:=0;
    finally
      Engine.Py_XDECREF(AObject);
    end;
  end;

  function AsBoolean(const AObject:PPyObject):Boolean;
  begin
    if AObject=nil then
       result:=False
    else
    try
      if Engine.PyBool_Check(AObject) then
         result:=Engine.PyObject_IsTrue(AObject)=1
      else
         result:=False;
    finally
      Engine.Py_XDECREF(AObject);
    end;
  end;

  function ArraySize(const AArray:PPyObject):TInteger;
  begin
    result:=AsLong(Engine.PyObject_GetAttrString(AArray,'size'));
  end;

  function GetColumn(const AName:String):TDataItem;
  var P : Int64;
      NewName : String;
      t,
      tmpSize : TLoopInteger;
  begin
    result:=TDataItem.Create;

    TDataAccess(result).FKind:=TypeOfArray(AName);

    if result.Kind=TDataKind.dkText then
    begin
      tmpSize:=AsLong(Evaluate('len('+AName+')'));
      result.Resize(tmpSize);

      for t:=0 to tmpSize-1 do
          result.TextData[t]:=EvaluateAsString(AName+'['+IntToStr(t)+']');
    end
    else
    begin
      NewName:='tmp_data';

      TBIPython.Execute(NewName+'='+AName+'.__array_interface__[''data''][0]');
      P:=AsLong(Evaluate(NewName));

      case result.Kind of
          dkInt32: NativeInt(result.Int32Data):=P;
          dkInt64: NativeInt(result.Int64Data):=P;
         dkSingle: NativeInt(result.SingleData):=P;
         dkDouble: NativeInt(result.DoubleData):=P;
       dkExtended: NativeInt(result.ExtendedData):=P;
       dkDateTime: NativeInt(result.DateTimeData):=P;
        dkBoolean: NativeInt(result.BooleanData):=P;
      end;
    end;
  end;

  function IsNDArray(const AObject:PPyObject):Boolean;
  begin
    if PyNDArray_Type=nil then
       PyNDArray_Type:=Evaluate('ndarray');

    result:=Engine.PyObject_TypeCheck(AObject, PPyTypeObject(PyNDArray_Type));

    {$IFOPT C+}
    Engine.CheckError;
    {$ENDIF}
  end;

  function TableSize(const AList:PPyObject):TInteger;
  var t : Integer;
      tmpItem : PPyObject;
      tmpItems : Integer;
  begin
    result:=0;

    tmpItems:=Engine.PyList_Size(AList);

    if tmpItems>0 then
    begin
      for t:=0 to tmpItems-1 do
      begin
        tmpItem:=Engine.PyList_GetItem(AList,t);

        if Engine.PyList_Check(tmpItem) then
        begin
          if result=0 then
             result:=Engine.PyList_Size(tmpItem)
          else
          if result<>Engine.PyList_Size(tmpItem) then
             Exit(0);
        end
        else
        if IsNDArray(tmpItem) then
        begin
          if result=0 then
             result:=ArraySize(tmpItem)
          else
          if result<>ArraySize(tmpItem) then
             Exit(0)
        end
        else
          Exit(0);
      end;
    end;
  end;

  function NewListColumn(const AName:String; const AItem:PPyObject):TDataItem;
  var t : TLoopInteger;
      tmp : PPyObject;
  begin
    result:=TDataItem.Create;
    result.Name:=AName;
    TDataAccess(result).FKind:=TypeOfListItem(AItem);

    if result.Kind<>TDataKind.dkUnknown then
    begin
      result.Resize(Engine.PyList_Size(AItem));

      for t:=0 to result.Count-1 do
      begin
        tmp:=Engine.PyList_GetItem(AItem,t);

        if tmp=Engine.Py_None then
           result.Missing[t]:=True
        else
        case result.Kind of
         dkInt32: result.Int32Data[t]:=AsLong(tmp);
         dkInt64: result.Int64Data[t]:=AsLong(tmp);
        dkSingle: result.SingleData[t]:=AsFloat(tmp);
        dkDouble: result.DoubleData[t]:=AsFloat(tmp);
      dkExtended: result.ExtendedData[t]:=AsFloat(tmp);
          dkText: result.TextData[t]:=Engine.PyObjectAsString(tmp);
      dkDateTime: result.DateTimeData[t]:=Engine.PyObjectAsVariant(tmp);
       dkBoolean: result.BooleanData[t]:=AsBoolean(tmp);
        end;
      end;
    end;
  end;

  function FromList(const AList:PPyObject):TDataItem;
  var t : TLoopInteger;

      tmpSize,
      tmpTableSize : TInteger;

      tmpItem : PPyObject;
  begin
    tmpTableSize:=TableSize(AList);

    {$IFOPT C+}
    Engine.CheckError;
    {$ENDIF}

    result:=TDataItem.Create(tmpTableSize>0);
    result.Name:=AName;

    for t:=0 to Engine.PyList_Size(AList)-1 do
    begin
      tmpItem:=Engine.PyList_GetItem(AList,t);

      if Engine.PyList_Check(tmpItem) then
      begin
        tmpSize:=Engine.PyList_Size(tmpItem);

        if tmpSize>0 then
           result.Items.Add(NewListColumn('Item_'+IntToStr(t),tmpItem));
      end
      else
      if IsNDArray(tmpItem) then
         result.Items.Add(GetColumn(AName+'['+IntToStr(t)+']'));
    end;

    if result.AsTable then
       result.Resize(tmpTableSize);
  end;

  function FromArray:TDataItem;
  var Shape : PPyObject;
      t,
      tmpCount,
      Dims : Integer;
  begin
    result:=nil;

    Execute('import ctypes');
    Shape:=Evaluate(AName+'.shape');

    if Shape<>nil then
    try
      if Engine.PyTuple_Check(Shape) then
      begin
        Dims:=Engine.PyTuple_Size(Shape);

        if Dims>0 then
        begin
          tmpCount:=AsLong(Engine.PyTuple_GetItem(Shape,0));

          if Dims>1 then
          begin
            result:=TDataItem.Create(True);
            result.Name:=AName;

            for t:=0 to AsLong(Engine.PyTuple_GetItem(Shape,1))-1 do
                result.Items.Add(GetColumn(AName+'[:,'+IntToStr(t)+']'));
          end
          else
            result:=GetColumn(AName);

          result.Resize(tmpCount);
        end;
      end;
    finally
      Engine.Py_XDECREF(Shape);
    end;
  end;

var tmp : PPyObject;
begin
  result:=nil;

  tmp:=Evaluate(AName);

  if tmp<>nil then
  try
    {$IFOPT C+}
    Engine.CheckError;
    {$ENDIF}

    if Engine.PyList_Check(tmp) then
       result:=FromList(tmp)
    else
    if IsNDArray(tmp) then
       result:=FromArray;
  finally
    Engine.Py_XDECREF(tmp);

    {$IFOPT C+}
    Engine.CheckError;
    {$ENDIF}
  end;
end;

class function TBIPython.GetEngine: TPythonEngine;
var tmpDLL : String;
begin
  if FEngine=nil then
  begin
    FEngine:=TPythonEngine.Create(nil);
    FEngine.PyFlags:=[]; //pfDebug,pfInteractive,pfVerbose];
    FEngine.IO:=TBIPythonIO.Create(FEngine);

    tmpDLL:=TBIRegistry.ReadString('Plugins','Python');

    if tmpDLL<>'' then
    begin
      FEngine.DllPath:=TPath.GetDirectoryName(tmpDLL);
      FEngine.DllName:=TPath.GetFileName(tmpDLL);
    end;

    FEngine.LoadDll;
  end;

  result:=FEngine;
end;

class function TBIPython.Evaluate(const AText: String): PPyObject;
begin
  result:=Engine.EvalString(AnsiString(AText));
end;

class function TBIPython.EvaluateAsString(const AText: String): String;
begin
  result:=Engine.PyObjectAsString(Evaluate(AText));
end;

class function TBIPython.Execute(const AText: String): Boolean;
begin
  Engine.ExecString(AnsiString(AText));
  result:=True;
end;

class function TBIPython.FromData(const AName:String; const AData: TDataItem;
  const Index: TNativeIntArray; const UseMissing: Boolean): String;
var t : TLoopInteger;
    tmp : TDataArray;
begin
  result:='from numpy import *'+#13#10;

  if AData.AsTable then
     result:=result+FromData(AName,AData.Items.AsArray,Index,UseMissing)
  else
  if AData.Kind=TDataKind.dkUnknown then
  begin
    result:=result+AName+'=array([';

    for t:=0 to AData.Items.Count-1 do
    begin
      result:=result+'"'+AData.Items[t].Name+'"';

      if t<AData.Items.Count-1 then
         result:=result+',';
    end;

    result:=result+'])';
  end
  else
  begin
    // "tmp" is just for pre-XE8 :
    SetLength(tmp,1);
    tmp[0]:=AData;

    result:=result+FromData(AName,tmp,Index,UseMissing);
  end;
end;

class function TBIPython.DataRowToVector(const AIndex:TInteger;
              const AData:TDataArray; const UseMissing:Boolean=True):String;
var t : Integer;
begin
  result:='';

  for t:=0 to High(AData) do
  begin
    if AData[t].Missing[AIndex] then
       if UseMissing then
          result:=result+'None'
       else
       if AData[t].Kind=TDataKind.dkText then
          result:=result+'""'
       else
          result:=result+'0'

    else
    if AData[t].Kind=TDataKind.dkText then
       result:=result+'"'+AData[t].TextData[AIndex]+'"'
    else
       result:=result+AData[t].DataToString(AIndex);

    if t<High(AData) then
       result:=result+','
  end;
end;

class function TBIPython.FromData(const AName:String;
               const AData:TDataArray; const Index:TNativeIntArray=nil;
               const UseMissing:Boolean=True):String;

  function Vector(const AData:TDataItem):String;
  begin
    result:=DataToVector(Index,AData,UseMissing,'None');
  end;

  function ArrayTypeOf(const AData:TDataItem):String;
  begin
    case AData.Kind of
        dkInt32,
        dkInt64: result:='int';
       dkSingle,
       dkDouble,
     dkExtended: result:='float';
         dkText: result:='str';
      dkBoolean: result:='bool';
    else
      result:='';
    end;
  end;

  function NewArray(const AData:TDataItem):String;
  begin
    result:='=asarray(['+Vector(AData)+'], dtype='+ArrayTypeOf(AData)+')';
  end;

var t : Integer;
begin
  if High(AData)=0 then
     result:=AName+NewArray(AData[0])
  else
  if High(AData)>0 then
  begin
    result:='';

    for t:=0 to High(AData) do
        result:=result+'tmpItem_'+IntToStr(t)+NewArray(AData[t])+#13#10;

    result:=result+AName+'=[';

    for t:=0 to High(AData) do
    begin
      result:=result+'tmpItem_'+IntToStr(t);

      if t<High(AData) then
         result:=result+',';
    end;

    result:=result+']';
  end;
end;

{ TScikitSupervisedModel }

function TScikitSupervisedModel.Fit: Boolean;
var tmp : TStrings;
begin
  tmp:=BuildScript;
  try
    TBIPython.Execute(tmp);
    result:=True;
  finally
    tmp.Free;
  end;
end;

{ TBIPythonIO }

procedure TBIPythonIO.Display(const S:String);
begin
  if TBIPython.FOutput<>nil then
     TBIPython.FOutput.Add(S);
end;

procedure TBIPythonIO.SendData(const Data: AnsiString);
begin
  inherited;
  Display(String(Data));
end;

initialization
finalization
//  if TBIPython.FEngine<>nil then
//     TBIPython.FEngine.Py_XDECREF(PyNDArray_Type);

  TBIPython.FEngine.Free;
  TBIPython.FEngine:=nil;
end.
