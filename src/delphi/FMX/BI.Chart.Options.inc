// WARNING:
// This file is only necessary due to a bug in XE6 compiler.
// If these lines are moved to BI.VCL/FMX.Chart.pas unit, 
// the compiler generates a warning/error even if the first $IF
// evaluates to false.

// XE4,5,7 and up, do not have this bug.

{$WARN COMPARISON_FALSE OFF}

{$IF Declared(TeeVCLBuildVersionInteger)}
  {$DEFINE HASTAGOBJECT}
{$ENDIF}

{$IF Declared(TeeVCLBuildVersionInteger) and (TeeVCLBuildVersionInteger>=+160406)}
  {$DEFINE SERIESLABELSRESIZE}
{$ENDIF}

{$IF Declared(TeeVCLBuildVersionInteger) and (TeeVCLBuildVersionInteger>=+161005)}
  {$DEFINE CHARTLAYERS}
{$ENDIF}

{$WARN COMPARISON_FALSE ERROR}


