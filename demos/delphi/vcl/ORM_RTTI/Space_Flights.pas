unit Space_Flights;

interface

uses
  System.Classes, System.Generics.Collections;

// Sample data using records

type
  TOperator=record
  private
    FSomeData : Int64;
  public
    Name : String;
    Country : String;

    property SomeData:Int64 read FSomeData;
  end;

  TPerson=record
  public
    Name : String;
  end;

  TFlight=record
  public
    Date : TDateTime;
    Name : String;
    Number : Integer;
    &Operator : TOperator;
    Remark : String;
    Crew : Array of TPerson;
    LaunchMassKg : Single;
  end;

  TFlights=Array of TFlight;

  TFlightItem=class(TCollectionItem)
  public
    Data : TFlight;
  end;

  TFlightCollection=class(TCollection)
  end;

function Flights:TFlights; // Example: Array of ...
function Vostok1:TFlight;  // Example: Simple record
function Apollo:TList<TFlight>;  // Example: TList<...>
function Viking:TFlightCollection;  // Example: Custom TCollection

implementation

uses
  SysUtils;

var
  _Apollo : TList<TFlight>;
  _Viking : TFlightCollection;

const
  OKB1:TOperator=(Name:'OKB-1'; Country:'Soviet Union');
  SovietSpaceProgram:TOperator=(Name:'Soviet Space Program'; Country:'Soviet Union');

function Flights:TFlights;
begin
  SetLength(result,4);

  result[0].Date:=EncodeDate(1957,10,4);
  result[0].Name:='Sputnik';
  result[0].Number:=1;
  result[0].&Operator:=OKB1;
  result[0].Remark:='First artificial satellite';
  result[0].Crew:=nil;
  result[0].LaunchMassKg:=83.6;

  result[1].Date:=EncodeDate(1959,1,2);
  result[1].Name:='Luna';
  result[1].Number:=1;
  result[1].&Operator:=OKB1;
  result[1].Remark:='First Lunar flyby, heliocentric orbit';
  result[1].Crew:=nil;
  result[1].LaunchMassKg:=361;

  result[2].Date:=EncodeDate(1959,9,12);
  result[2].Name:='Luna';
  result[2].Number:=2;
  result[2].&Operator:=OKB1;
  result[2].Remark:='Lunar impact';
  result[2].Crew:=nil;
  result[2].LaunchMassKg:=390.2;

  result[3].Date:=EncodeDate(1959,10,7);
  result[3].Name:='Luna';
  result[3].Number:=3;
  result[3].&Operator:=OKB1;
  result[3].Remark:='Pictures of the far side of the moon';
  result[3].Crew:=nil;
  result[3].LaunchMassKg:=278.5;
end;

const
  YuriGagarin:TPerson=(Name:'Yuri Gagarin');

function Vostok1:TFlight;
begin
  result.Date:=EncodeDate(1961,4,12);
  result.Name:='Vostok';
  result.Number:=1;
  result.&Operator:=SovietSpaceProgram;
  result.Remark:='First human in space';

  {$IF CompilerVersion>28}
  result.Crew:=[YuriGagarin];
  {$ELSE}
  SetLength(result.Crew,1);
  result.Crew[0]:=YuriGagarin;
  {$ENDIF}

  result.LaunchMassKg:=4725;
end;

const
  NASA:TOperator=(Name:'NASA'; Country:'USA');

function Apollo:TList<TFlight>;

  function AS201:TFlight;
  begin
    result.Date:=EncodeDate(1966,2,26);
    result.Name:='AS-201';
    result.Number:=0;
    result.&Operator:=NASA;
    result.Remark:='First flight of Saturn IB';
    result.Crew:=nil;
    result.LaunchMassKg:=15294;
  end;

  function AS202:TFlight;
  begin
    result.Date:=EncodeDate(1966,8,25);
    result.Name:='AS-202';
    result.Number:=0;
    result.&Operator:=NASA;
    result.Remark:='Suborbital flight';
    result.Crew:=nil;
    result.LaunchMassKg:=20091;
  end;

  function AS203:TFlight;
  begin
    result.Date:=EncodeDate(1966,7,5);
    result.Name:='AS-203';
    result.Number:=0;
    result.&Operator:=NASA;
    result.Remark:='Liquid hydrogen fuel behavior';
    result.Crew:=nil;
    result.LaunchMassKg:=0; // N/A
  end;

  function Apollo1:TFlight;
  begin
    result.Date:=EncodeDate(1967,2,21);
    result.Name:='Apollo';
    result.Number:=1;
    result.&Operator:=NASA;
    result.Remark:='Accident';
    result.Crew:=nil; //[GussGrissom,EdwardHWhite,RogerBChaffee];
    result.LaunchMassKg:=20000;
  end;

begin
  if _Apollo=nil then
  begin
    _Apollo:=TList<TFlight>.Create;
    _Apollo.Add(AS201);
    _Apollo.Add(AS203);
    _Apollo.Add(AS202);
    _Apollo.Add(Apollo1);
  end;

  result:=_Apollo;
end;

function Viking:TFlightCollection;  // Example: Custom TCollection
begin
  if _Viking=nil then
  begin
    _Viking:=TFlightCollection.Create(TFlightItem);

    with _Viking.Add as TFlightItem do
    begin
      Data.Date:=EncodeDate(1975,8,20);
      Data.Name:='Viking';
      Data.Number:=1;
      Data.&Operator:=NASA;
      Data.Remark:='Pictures from Mars';
      Data.Crew:=nil;
      Data.LaunchMassKg:=572;
    end;
  end;

  result:=_Viking;
end;

// Just to release the memory
initialization
finalization
  _Apollo.Free;
  _Viking.Free;
end.
