unit BI.Tests.Workflow;

// Automated testing of all TBIWorkflow items

interface

uses
  BI.Data.Workflow, BI.Data;

type
 TDataProviderClass=class of TDataProvider;

  TBIWorkflowTests=record
  private
    procedure DoTest(const AClass:TDataProviderClass);

    procedure TestNewItem;
    procedure TestDeleteItems;
    procedure TestDuplicateItems;
    procedure TestRenameItem;
    procedure TestDataTranspose;
    procedure TestBIQuery;
    procedure TestDataShuffle;
    procedure TestSplitItem;
    procedure TestNormalizeItem;
    procedure TestDataDefinition;
    procedure TestDataRankItem;
    procedure TestDataGridifyItem;
    procedure TestMergeItem;
    procedure TestCloneItem;
    procedure TestCompareItem;
    procedure TestGroupByItem;
  public
    class procedure Test; static;
  end;

implementation

uses
  BI.Arrays, System.SysUtils, BI.Query, BI.Persist, BI.Algorithm;

function RandomText:String;
const
  Texts:Array[0..9] of String=('','a','!','Hello','Hello World',
                                'Some long text','áéíóú',
                                '直辖市','       ','.............');
begin
  result:=Texts[Random(Length(Texts))];
end;

function RandomDateTime:TDateTime;
begin
  result:=Now;
end;

procedure AddRandom(const AData:TDataItem; const AQuantity:Integer);
var t : TLoopInteger;
    tmpInt : Integer;
begin
  AData.Resize(AQuantity);

  for t:=0 to AQuantity-1 do
  begin
    tmpInt:=Random(100000)-50000;

    case AData.Kind of
        dkInt32: AData.Int32Data[t]:=tmpInt;
        dkInt64: AData.Int64Data[t]:=tmpInt;
       dkSingle: AData.SingleData[t]:=1.234*tmpInt;
       dkDouble: AData.DoubleData[t]:=1.234*tmpInt;
     dkExtended: AData.ExtendedData[t]:=1.234*tmpInt;
         dkText: AData.TextData[t]:=RandomText;
     dkDateTime: AData.DateTimeData[t]:=RandomDateTime;
      dkBoolean: AData.BooleanData[t]:=tmpInt>0;
    end;
  end;
end;

function RandomKind:TDataKind;
begin
  result:=TDataKind(Random(Ord(TDataKind.dkUnknown)));
end;

function SampleItem(const AKind:TDataKind; const AName:String=''):TDataItem;
var t : Integer;
begin
  result:=TDataItem.Create(AKind,AName);

  if AKind=TDataKind.dkUnknown then
     for t:=0 to Random(10) do
         result.Items.Add(SampleItem(RandomKind,'Item'+IntToStr(t)))
  else
     AddRandom(result,100);
end;

function SampleTable:TDataItem;
begin
  result:=SampleItem(TDataKind.dkUnknown);
  result.AsTable:=True;
end;

{ TBIWorkflowTests }

procedure TBIWorkflowTests.DoTest(const AClass: TDataProviderClass);

  procedure TestSample(const AProvider:TDataProvider; const ASample:TDataItem);
  var tmp : TDataItem;
  begin
    if AProvider is TDataProviderNeeds then
    begin
      if TDataProviderNeeds(AProvider).Needs.Count>0 then
      begin
        TDataProviderNeeds(AProvider).Needs[0].Data.Clear;
        TDataProviderNeeds(AProvider).Needs[0].Data.Add(ASample);
      end;

      TDataProviderNeeds(AProvider).Needs.Verify;
    end;

    tmp:=AProvider.NewData;
    try
      tmp.Load;
    finally
      tmp.Free;
    end;
  end;

  procedure TestKind(const AProvider:TDataProvider; const AKind:TDataKind);
  var tmp: TDataItem;
  begin
    tmp:=SampleItem(AKind);
    try
      TestSample(AProvider,tmp);
    finally
      tmp.Free;
    end;
  end;

  procedure TestTable(const AProvider:TDataProvider);
  var tmp: TDataItem;
  begin
    tmp:=SampleTable;
    try
      TestSample(AProvider,tmp);
    finally
      tmp.Free;
    end;
  end;

var tmp : TDataProvider;
begin
  tmp:=AClass.Create(nil);
  try
    TestKind(tmp,TDataKind.dkInt32);
    TestKind(tmp,TDataKind.dkInt64);
    TestKind(tmp,TDataKind.dkSingle);
    TestKind(tmp,TDataKind.dkDouble);
    TestKind(tmp,TDataKind.dkExtended);
    TestKind(tmp,TDataKind.dkText);
    TestKind(tmp,TDataKind.dkDateTime);
    TestKind(tmp,TDataKind.dkBoolean);
    TestKind(tmp,TDataKind.dkUnknown);

    TestTable(tmp);
  finally
    tmp.Free;
  end;
end;

class procedure TBIWorkflowTests.Test;
var tmp : TBIWorkflowTests;
begin
  tmp.TestNewItem;
  tmp.TestDeleteItems;
  tmp.TestDuplicateItems;
  tmp.TestRenameItem;
  tmp.TestDataTranspose;
  tmp.TestBIQuery;
  tmp.TestDataShuffle;
  tmp.TestSplitItem;
  tmp.TestNormalizeItem;
  tmp.TestDataDefinition;
  tmp.TestDataRankItem;
  tmp.TestDataGridifyItem;
  tmp.TestMergeItem;
  tmp.TestCloneItem;
  tmp.TestCompareItem;
  tmp.TestGroupByItem;
end;

procedure TBIWorkflowTests.TestBIQuery;
begin
  DoTest(TBIQuery);
end;

procedure TBIWorkflowTests.TestCloneItem;
begin
  DoTest(TCloneItem);
end;

procedure TBIWorkflowTests.TestCompareItem;
begin
//  DoTest(TCompareItem);
end;

procedure TBIWorkflowTests.TestDataDefinition;
begin
  DoTest(TDataDefinition);
end;

procedure TBIWorkflowTests.TestDataGridifyItem;
begin
 // DoTest(TDataGridifyItem);
end;

procedure TBIWorkflowTests.TestDataRankItem;
begin
//  DoTest(TDataRankItem);
end;

procedure TBIWorkflowTests.TestDataShuffle;
begin
  DoTest(TDataShuffle);
end;

procedure TBIWorkflowTests.TestDataTranspose;
begin
  DoTest(TDataTranspose);
end;

procedure TBIWorkflowTests.TestDeleteItems;
begin
  DoTest(TDeleteItems);
end;

procedure TBIWorkflowTests.TestDuplicateItems;
begin
  DoTest(TDuplicateItems);
end;

procedure TBIWorkflowTests.TestGroupByItem;
begin
  DoTest(TGroupByItem);
end;

procedure TBIWorkflowTests.TestMergeItem;
begin
//  DoTest(TMergeItem);
end;

procedure TBIWorkflowTests.TestNewItem;
begin
  DoTest(TNewItem);
end;

procedure TBIWorkflowTests.TestNormalizeItem;
begin
  DoTest(TNormalizeItem);
end;

procedure TBIWorkflowTests.TestRenameItem;
begin
  DoTest(TRenameItem);
end;

procedure TBIWorkflowTests.TestSplitItem;
begin
  DoTest(TSplitItem);
end;

end.
