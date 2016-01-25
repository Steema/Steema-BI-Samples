unit BI.Summary.Persist;

interface

uses
  System.Classes, BI.Persist, BI.Streams, BI.Summary;

type
  TSummaryPersist=class
  private
    class procedure Load(const Reader: TBIReader; const Summary:TSummary); static;
  public
    class function LoadFrom(const Reader: TBIReader): TSummary; overload; static;
    class function LoadFrom(const Stream: TStream): TSummary; overload; static;

    class procedure Save(const Stream: TStream; const Summary:TSummary); overload; static;
    class procedure Save(const Writer: TBIWriter; const Summary:TSummary); overload; static;
  end;

  TNamedSummary=record
    Name : String;
    Summary : TSummary;
  end;

  TSummaries=class
  public
    Items : Array of TNamedSummary;

    Destructor Destroy; override;

    procedure Add(const ASummary:TSummary);
    procedure Clear;
    function Count:Integer;

    procedure Load(const FileName:String); overload;
    procedure Load(const Stream:TStream); overload;
    procedure Load(const Reader:TBIReader); overload;

    procedure Save(const FileName:String); overload;
    procedure Save(const Stream:TStream); overload;
    procedure Save(const Writer:TBIWriter); overload;
  end;

implementation
