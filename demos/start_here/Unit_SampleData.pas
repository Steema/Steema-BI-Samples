unit Unit_SampleData;

interface

// Returns the filename containing the sample data for this demo
function SampleDataFile:String;

implementation

uses
  SysUtils;

// Search the file at the current directory or some levels up
// ie: \win32\debug  ->  \win32   -> \
function SampleDataFile:String;
const Name='products-10000.csv';
var Levels : Integer;
begin
  Levels:=0;

  result:=Name;

  repeat
    if FileExists(result) then
       Exit
    else
    begin
      Inc(Levels);
       result:='..\'+result;
    end;

  until Levels>5;

  raise Exception.Create('Error: Cannot locate sample file: '+Name);
end;

end.
