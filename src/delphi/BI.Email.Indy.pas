unit BI.Email.Indy;

interface

type
  TBIEmailIndy=class
  public
    Host,
    EmailFrom,
    Username,
    Password : String;

    procedure Send(const AUser,AEmail,ASubject,AContent:String);
    class function ValidAddress(const Email:String):Boolean; static;
  end;

implementation
