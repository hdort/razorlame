# Running Tests

This repository includes DUnit-style tests that can be executed with either Delphi or Free Pascal.

## Using Free Pascal

1. Ensure the FPC sources are installed (the Debian/Ubuntu package `fpc-source` provides them).
2. From the repository root run:

```bash
fpc tests/TestDetermineOutputPath.pas
./TestDetermineOutputPath
```

The test runner will output the results on the command line and return a non-zero exit code on failure.

## Using Delphi

Compile `tests/TestDetermineOutputPath.pas` with the DUnit units available in your Delphi installation and run the produced executable. The test project is self-contained and only depends on the standard DUnit units.
