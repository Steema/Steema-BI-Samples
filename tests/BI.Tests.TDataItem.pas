unit BI.Tests.TDataItem;

interface

uses
  BI.Arrays, BI.DataItem, DUnitX.TestFramework;

type
  [TestFixture]
  TDataItem_Test=class(TObject)
  strict private
    FData : TDataItem;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Append','8,1,2')]
    procedure TestAppend(Value1, Value2, _Result: Integer);
  end;

implementation

procedure TDataItem_Test.Setup;
begin
  FData:=TDataItem.Create(TDataKind.dkInt32);
end;

procedure TDataItem_Test.TearDown;
begin
  FData.Free;
end;

procedure TDataItem_Test.TestAppend(Value1, Value2, _Result: Integer);
var
  R: Integer;
begin
  FData.Int32Data.Append(Value1);
  FData.Int32Data.Append(Value2);

  R := FData.Int32Data.Count;
  Assert.AreEqual(R, _Result);
end;

initialization
  TDUnitX.RegisterTestFixture(TDataItem_Test);
end.
