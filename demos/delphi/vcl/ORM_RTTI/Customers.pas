unit Customers;

interface

{$M+}

// Sample data using a class and array

type
  TCustomer=class
  private
    FSales : Double;
  public
    //[PrimaryKey]
    ID : Integer;

    Name : String;

  published
    property Sales:Double read FSales write FSales;
  end;

  TCustomers=Array of TCustomer;

function CustomerArray:TCustomers;

implementation

var
 _Customers : TCustomers;

function _CustomerArray:TCustomers;
begin
  SetLength(result,2);

  result[0]:=TCustomer.Create;
  result[0].ID:=42;
  result[0].Name:='Acme Inc.';
  result[0].Sales:=1234;

  result[1]:=TCustomer.Create;
  result[1].ID:=60;
  result[1].Name:='FooBar Corp.';
  result[1].Sales:=5678;
end;

function CustomerArray:TCustomers;
begin
  if _Customers=nil then
     _Customers:=_CustomerArray;

  result:=_Customers;
end;

// Just to release the allocated memory
procedure ReleaseCustomers;
var tmp : TCustomer;
begin
  for tmp in _Customers do
      tmp.Free;
end;

initialization
finalization
  ReleaseCustomers;
end.
