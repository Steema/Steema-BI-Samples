unit BI.VCL.Chart.Functions;

interface

uses
  BI.Data, VCLTee.TeEngine, BI.Algorithm;

type
  TDataFunction=class(TBaseAlgorithm)
  private
    FFunction : TTeeFunction;
    FFunctionClass : TTeeFunctionClass;
    FSource : TDataItem;
    FPeriod: Integer;

    procedure AddNeeds(const AFunction:TTeeFunction);
    procedure SetSource(const Value: TDataItem);
    procedure SetPeriod(const Value: Integer);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor CreateFunction(const AFunctionClass:TTeeFunctionClass); overload;
    Constructor CreateFunction(const AFunction:TTeeFunction); overload;

    procedure Calculate; override;

    property Period:Integer read FPeriod write SetPeriod;
    property Source:TDataItem read FSource write SetSource;
  end;

implementation
