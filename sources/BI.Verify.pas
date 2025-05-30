{*********************************************}
{  TeeBI Software Library                     }
{  Data Verification                          }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Verify;

interface

{
  TVerificator class to check the integrity of TDataItem structures.
  Any errors are added to AStrings parameter.

  (Work in progress)
}


uses
  System.Classes, BI.DataItem;

type
  TVerificator=class
  public
    Strings: TStrings;

    Constructor Create(const AStrings:TStrings);

    procedure Verify(const AData:TDataItem); overload;

    // Pending: Multi-colum master-detail
    //procedure Verify(const AData:TDataArray); overload;
  end;

implementation

uses
  System.SysUtils, BI.Arrays;

Constructor TVerificator.Create(const AStrings: TStrings);
begin
  inherited Create;
  Strings:=AStrings;
end;

type
  TDataAccess=class(TDataItem);

procedure TVerificator.Verify(const AData: TDataItem);

  function GetFail:TInt32Array;
  var L : TInteger;

    procedure Add(const Index:TInteger);
    begin
      SetLength(result,L+1);
      result[L]:=Index;
      Inc(L);
    end;

  var t : TLoopInteger;
  begin
    result:=nil;
    L:=0;

    if AData.Missing.Count>0 then
    begin
      for t:=0 to High(TDataAccess(AData).IMaster.Index) do
          if not AData.Missing[t] then
             if TDataAccess(AData).IMaster.Index[t]=-1 then
                Add(t);
    end
    else
      for t:=0 to High(TDataAccess(AData).IMaster.Index) do
          if TDataAccess(AData).IMaster.Index[t]=-1 then
             Add(t);
  end;

  function GetUnused:TInt32Array;
  var L : TInteger;

    procedure Add(const Index:TInteger);
    begin
      SetLength(result,L+1);
      result[L]:=Index;
      Inc(L);
    end;

  var t : TLoopInteger;
      Temp : TInt32Array;
  begin
    result:=nil;
    L:=0;

    {$IFDEF FPC}
    Temp:=nil;
    {$ENDIF}

    Temp.Resize(AData.Master.Count);

    if AData.Missing.Count>0 then
    begin
      for t:=0 to High(TDataAccess(AData).IMaster.Index) do
          if not AData.Missing[t] then
             if TDataAccess(AData).IMaster.Index[t]<>-1 then
                Inc(Temp[TDataAccess(AData).IMaster.Index[t]]);
    end
    else
      for t:=0 to High(TDataAccess(AData).IMaster.Index) do
          if TDataAccess(AData).IMaster.Index[t]<>-1 then
             Inc(Temp[TDataAccess(AData).IMaster.Index[t]]);

    for t:=0 to Temp.Count-1 do
        if Temp[t]=0 then
           Add(t);
  end;

var t : TLoopInteger;
    L : TInteger;
    Fail,
    Unused : TInt32Array;
    MissingPercent : Single;
begin
  if TDataAccess(AData).HasMaster then
  begin
    AData.Master.Load;
    AData.Master.Stats;

    if not AData.Master.Unique then
       Strings.Add('Not Unique: '+AData.Master.FullName);

    // Pending: Multi-item master-detail relationship
    if TDataAccess(AData).IMaster.Index=nil then
       AData.CreateMasterIndex;

    Fail:=GetFail;

    L:=Length(Fail);

    if L>0 then
    begin
      Strings.Add('Missing Master: '+AData.FullName+' -> '+AData.Master.Name+' : '+IntToStr(L));

      for t:=0 to L-1 do
          Strings.Add(' '+IntToStr(Fail[t])+' '+AData.DataToString(Fail[t]));
    end;

    Unused:=GetUnused;

    L:=Length(Unused);

    if L>0 then
       Strings.Add('Unused Master: '+AData.FullName+' -> '+AData.Master.Name+' : '+IntToStr(L));
  end;

  if (AData.Missing.Count>0) and (AData.Count>0) then
  begin
    MissingPercent:=(100*AData.Missing.Count/AData.Count);

    if MissingPercent<1 then
    begin
      Strings.Add('Missing: '+AData.FullName+' : '+IntToStr(AData.Missing.Count)+' ('+FormatFloat('0.##%',MissingPercent)+')');

      for t:=0 to AData.Count-1 do
          if AData.Missing[t] then
             Strings.Add(' '+IntToStr(t));
    end;
  end;

  if AData.AsTable then
  for t:=0 to AData.Items.Count-1 do
      if TDataAccess(AData.Items[t]).HasMaster then
         Verify(AData.Items[t]);
end;

end.
