unit Import_File;

interface

uses
  BI.DataItem;

// Automatic data import based on AFileName extension.

// The appropiate import class is determined using the filename extension,
// and then used to import it.
function ImportFile(const AFileName:String):TDataItem;

// Special case for MongoDB JSON files, that aren't "Standard JSON"
function ImportMongoDB(const AFileName:String):TDataItem;

implementation

uses
  BI.DataSource, BI.JSON;

function ImportFile(const AFileName:String):TDataItem;
begin
  result:=TBIFileSource.FromFile(AFileName);
end;

function ImportMongoDB(const AFileName:String):TDataItem;
begin
  // MongoDB json format is not "standard json".
  // Each text row is a json on its own.

  result:=TBIJSON.FromFile(AFileName, TBIJSONFormat.&Array);
end;

end.
