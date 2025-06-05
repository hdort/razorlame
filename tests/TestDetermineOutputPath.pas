program TestDetermineOutputPath;

{$mode objfpc}{$H+}

uses
  SysUtils,
  {$IFDEF FPC}
  fpcunit, testregistry, simpletestrunner
  {$ELSE}
  TestFramework
  {$ENDIF};

type
  TMP3Settings = record
    OutDir: string;
    UseInputDir: Boolean;
  end;

var
  MP3Settings: TMP3Settings;

function DetermineOutputPath(const asInputFilePath: string): string;
begin
  Result := asInputFilePath;
  if MP3Settings.UseInputDir then
    Exit;
  if (Trim(MP3Settings.OutDir) <> '') and DirectoryExists(MP3Settings.OutDir) then
    Result := MP3Settings.OutDir;
end;

type
  TDetermineOutputPathTests = class(TTestCase)
  published
    procedure TestUseInputDirTrue;
    procedure TestUseInputDirFalseOutDirExists;
    procedure TestUseInputDirFalseOutDirMissing;
    procedure TestUseInputDirFalseOutDirEmpty;
  end;

procedure TDetermineOutputPathTests.TestUseInputDirTrue;
var
  InputPath: string;
  ResultPath: string;
begin
  MP3Settings.UseInputDir := True;
  MP3Settings.OutDir := '';
  InputPath := '/tmp/inputdir';
  ResultPath := DetermineOutputPath(InputPath);
  AssertEquals(InputPath, ResultPath);
end;

procedure TDetermineOutputPathTests.TestUseInputDirFalseOutDirExists;
var
  InputPath, OutDir, ResultPath: string;
begin
  OutDir := GetTempDir + 'razorlame_test_outdir';
  ForceDirectories(OutDir);
  try
    MP3Settings.UseInputDir := False;
    MP3Settings.OutDir := OutDir;
    InputPath := '/tmp/somewhere';
    ResultPath := DetermineOutputPath(InputPath);
    AssertEquals(OutDir, ResultPath);
  finally
    RemoveDir(OutDir);
  end;
end;

procedure TDetermineOutputPathTests.TestUseInputDirFalseOutDirMissing;
var
  InputPath, OutDir, ResultPath: string;
begin
  OutDir := GetTempDir + 'razorlame_missing_outdir';
  if DirectoryExists(OutDir) then
    RemoveDir(OutDir);
  MP3Settings.UseInputDir := False;
  MP3Settings.OutDir := OutDir;
  InputPath := '/tmp/somewhere';
  ResultPath := DetermineOutputPath(InputPath);
  AssertEquals(InputPath, ResultPath);
end;

procedure TDetermineOutputPathTests.TestUseInputDirFalseOutDirEmpty;
var
  InputPath, ResultPath: string;
begin
  MP3Settings.UseInputDir := False;
  MP3Settings.OutDir := '';
  InputPath := '/tmp/somewhere';
  ResultPath := DetermineOutputPath(InputPath);
  AssertEquals(InputPath, ResultPath);
end;

begin
  RegisterTest(TDetermineOutputPathTests);
  {$IFDEF FPC}
  with TTestRunner.Create(nil) do
  try
    Initialize;
    Run;
  finally
    Free;
  end;
  {$ELSE}
  RunRegisteredTests;
  {$ENDIF}
end.
