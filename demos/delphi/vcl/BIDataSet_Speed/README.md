
This example compares the speed of a TBIDataset component with a FireDAC memory table component (TFDMemTable)

Both datasets are:

 - Created with a single "double" float field
 
 - Filled with a big quantity of records (10 million by default)
 
 - Traversed to calculate a total sum
 
 - Saved to a file.

### Benchmark:

Using RAD Studio Seattle Release mode, on a Windows 10 machine with an i7 4770 cpu, total time results:

32bit:
  *  FDMemTable : 27.5 seconds
  *  BIDataset  :  1.8 seconds

64bit:
  *  FDMemTable : 21.7 seconds
  *  BIDataset  :  1.4 seconds

