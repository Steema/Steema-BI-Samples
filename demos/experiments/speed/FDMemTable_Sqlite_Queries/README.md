## Experimental test project

### Comparing the speed of FireDAC LocalSQL (SQLite) memory tables and TeeBI

This test project creates an in-memory table, adds 100.000 rows (no indexes) and performs a simple query 10 times:

```sql
select * from Persons where Name='Alex' 
```

Tested on an Intel i7 4770 cpu, compiled with Embarcadero Delphi Berlin 10.1, Win32 cpu release mode.

Trying to obtain the best speed for both FireDAC and TeeBI, current results are:


### FireDAC LocalSQL (SQLite)
Create: 1785 msec

Query 10 times: 1898 msec

### TeeBI (pure Pascal)
Create: 3 msec

Query 10 times: 282 msec

