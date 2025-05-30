{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit teebi;

interface

uses
  TeeBI_Lazarus, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TeeBI_Lazarus', @TeeBI_Lazarus.Register);
end;

initialization
  RegisterPackage('teebi', @Register);
end.
