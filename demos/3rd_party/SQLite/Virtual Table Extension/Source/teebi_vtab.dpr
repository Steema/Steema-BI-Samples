library teebi_vtab;

{
   www.steema.com
   https://github.com/Steema/TeeBI
}

{$R *.res}

uses
  SysUtils, SynSqlite3;

type
  Pbi_tab_vtab = ^Tbi_tab_vtab;
  Tbi_tab_vtab = record
    base: TSQLite3VTab;
  end;

  Pbi_tab_cursor = ^Tbi_tab_cursor;
  Tbi_tab_cursor = record
    base: TSQLite3VTabCursor;
    Rowid: Int64;
  end;

// Connect
function bi_tabConnect(db: TSQLite3DB; pAux: Pointer; argc: Integer; argv: PPAnsiChar;
  var ppVtab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; cdecl;
var
  pNew: Pbi_tab_vtab;
  rc: Integer;
begin
  rc := sqlite3_declare_vtab(db, 'CREATE TABLE x(a,b)');
  if rc = SQLITE_OK then
  begin
    New(pNew);
    FillChar(pNew^, SizeOf(Tbi_tab_vtab), 0);
    ppVtab := @pNew.base;
  end;
  Result := rc;
end;

// Disconnect
function bi_tabDisconnect(pVtab: PSQLite3VTab): Integer; cdecl;
begin
  Dispose(Pbi_tab_vtab(pVtab));
  Result := SQLITE_OK;
end;

// Open
function bi_tabOpen(p: PSQLite3VTab; var ppCursor: TSqlite3VTabCursor): Integer; cdecl;
var
  pCur: Pbi_tab_cursor;
begin
  New(pCur);
  FillChar(pCur^, SizeOf(Tbi_tab_cursor), 0);
  ppCursor := @pCur.base;
  Result := SQLITE_OK;
end;

// Close
function bi_tabClose(var cur: TSqlite3VTabCursor): Integer; cdecl;
begin
  Dispose(Pbi_tab_cursor(cur));
  Result := SQLITE_OK;
end;

// Next
function bi_tabNext(cur: TSqlite3VTabCursor): Integer; cdecl;
begin
  Inc(Pbi_tab_cursor(cur)^.iRowid);
  Result := SQLITE_OK;
end;

// Column
function bi_tabColumn(var cur: TSqlite3VTabCursor; ctx: TSQLite3FunctionContext; i: Integer): Integer; cdecl;
begin
  case i of
      sqlite3_result_int(ctx, 1000 + Cur.Rowid);
  end;

  Result := SQLITE_OK;
end;

// Rowid
function bi_tabRowid(var cur: TSqlite3VTabCursor; var pRowid: Int64): Integer; cdecl;
begin
  pRowid := Pbi_tab_cursor(cur)^.iRowid;
  Result := SQLITE_OK;
end;

// Eof
function bi_tabEof(cur: TSqlite3VTabCursor): Integer; cdecl;
begin
  Result := Ord(Pbi_tab_cursor(cur)^.iRowid >= 10);
end;

// Filter
function bi_tabFilter(var pVtabCursor: TSqlite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
  argc: Integer; var argv: TSQLite3ValueArray): Integer; cdecl;
begin
  pVtabCursor.Rowid := 1;
  Result := SQLITE_OK;
end;

// BestIndex
function bi_tabBestIndex(var tab: TSQLite3VTab; var pIdxInfo: TSQLite3IndexInfo): Integer; cdecl;
begin
  pIdxInfo.estimatedCost := 10.0;
  pIdxInfo.estimatedRows := 10;
  Result := SQLITE_OK;
end;

var
  bi_tabModule: TSQLite3Module = (
    iVersion: 0;
    xCreate: nil;
    xConnect: @bi_tabConnect;
    xBestIndex: @bi_tabBestIndex;
    xDisconnect: @bi_tabDisconnect;
    xDestroy: nil;
    xOpen: @bi_tabOpen;
    xClose: @bi_tabClose;
    xFilter: @bi_tabFilter;
    xNext: @bi_tabNext;
    xEof: @bi_tabEof;
    xColumn: @bi_tabColumn;
    xRowid: @bi_tabRowid;
    xUpdate: nil;
    xBegin: nil;
    xSync: nil;
    xCommit: nil;
    xRollback: nil;
    xFindFunction: nil;
    xRename: nil;
    xSavepoint: nil;
    xRelease: nil;
    xRollbackTo: nil;
    //xShadowName: nil;
    //xIntegrity: nil
  );

// Export
function sqlite3_bi_tab_init(var db: TSQLite3DB; pzErrMsg: PPAnsiChar; const pApi: Psqlite3_api_routines): Integer; cdecl;
begin
  Result:=Sqlite3.create_module_v2(db,'bi_tab', bi_tabModule, nil, nil);
end;

exports
  sqlite3_bi_tab_init;

begin
end.
