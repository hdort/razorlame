(* (c) Copyright 2000 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit UtilFuncs;

interface

uses Windows, Forms, Classes, Dialogs;

function MyMsgDlg(const asMsg, asCaption: string; aDlgType: TMsgDlgType;
  aButtons: TMsgDlgButtons; const Args: array of string): Integer;
function MyMessageBox(Msg, Caption: string; Flags: Integer): Integer;

function GetOptionString: string;
function ReadOptions: Boolean;
procedure WriteOptions;
procedure WriteLameOptions(const asFilename: string);
procedure ReadLameOptions(const asFilename: string);

type
  TShutdownFlag = (sfClose, sfShutdown, sfLogOff, sfRestart, sfHibernate, sfSuspend);

procedure ShutdownWindows(how: TShutdownFlag);

procedure StoreInIni(const asSection, asItem: string; const aiValue: Integer); overload;
procedure StoreInIni(const asSection, asItem, asValue: string); overload;
procedure StoreInIni(const asSection, asItem: string; const abValue: Boolean); overload;

procedure ReadSystemData;

//-- called without asIniFile, ExeFileName is used with .ini extension
procedure RepositionForm(const asSection: string; aForm: TForm; const asIniFile: string = '');
procedure RegisterFormPosition(const asSection: string; aForm: TForm; const asIniFile: string = '');

//- needed those when kicking RxLib
function FileDateTime(const FileName: string): TDateTime;
function GetFileSize(const FileName: string): Integer;

//-- other util funcs
function StrToFloatDef(const Value: string; const Default: Extended): Extended;
function GetSystemErrorMessage(const aiErrorCode: Integer): string;
function DeleteZeroByteFile(const FileName: string): Boolean;
function CreateFilterFromStringList(List: TStringList): string;
function IsExtensionAllowed(const asExt: string): Boolean;

procedure ParseM3UFile(const asFilename: string; var Strings: TStringList);
procedure ParsePLSFile(const asFilename: string; var Strings: TStringList);

//-- Cursor-Management
procedure PushCursor;
procedure PopCursor;
procedure ResetCursor;

//-- audio helper functions
function DetermineFileInfo(const asFile: string; var aiSize, aiBitrate: integer; var adtLength: TDateTime): boolean;
function ExtractWaveRate(const asFile: string): Integer;
function GetNumberFramesVBR(const asFile: string): Integer;

//-- only one instance management
procedure PassToOtherInstance(const handle: HWND);
function EnumFunc(Wnd: HWND; var pWnd: HWND): bool; stdcall;

implementation

uses Controls, StdCtrls, SysUtils, FileCtrl, IniFiles, Globals, ResStr, MPGTools;

function MyMsgDlg(const asMsg, asCaption: string; aDlgType: TMsgDlgType;
  aButtons: TMsgDlgButtons; const Args: array of string): Integer;
var
  i, liBeep, liButton: Integer;
begin
  with CreateMessageDialog(asMsg, aDlgType, aButtons) do
  try
    Caption := asCaption;
    liButton := 0;
    for i := 1 to ControlCount do
    begin
        //-- we're only looking for buttons
      if not (Controls[i - 1] is TButton) then Continue;
      with Controls[i - 1] as TButton do
      begin
          //-- increase the button count
        Inc(liButton);
          //-- if we've run out of captions for buttons,
          //-- break out of the for loop
        if (liButton - 1) > High(Args) then Break;
          //-- empty strings mean: keep default caption for that button
        if Args[liButton - 1] = '' then Continue;
        Caption := Args[liButton - 1];
      end;
    end;
      //-- determine what sound we should make
    liBeep := MB_OK;
    case aDlgType of
      mtConfirmation: liBeep := MB_ICONQUESTION;
      mtWarning: liBeep := MB_ICONEXCLAMATION;
      mtError: liBeep := MB_ICONHAND;
      mtInformation: liBeep := MB_OK;
    end;
    MessageBeep(liBeep);
      //-- now show the dialog
    Result := ShowModal;
  finally
    Free;
  end;
end;


procedure ShutdownWindows(how: TShutdownFlag);
//-- the following procedure is based on TWinReboot by
//-- Barry Brannan(barrylb@poboxes.com)
//-- shutdown flags added by Holger Dors, January 2001

  function SetPrivilege(asPrivilegeName: string; abEnable: Boolean): Boolean;
  var
    tpPrev, tp: TTokenPrivileges;
    MyToken: THandle;
    dwRetLen: DWord;
  begin
    Result := false;

    OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
      MyToken);

    tp.PrivilegeCount := 1;
    if LookupPrivilegeValue(nil, PChar(asPrivilegeName), tp.Privileges[0].LUID) then
    begin
      if abEnable then
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
      else
        tp.Privileges[0].Attributes := 0;

      dwRetLen := 0;
      Result := AdjustTokenPrivileges(MyToken, false, tp, SizeOf(tpPrev), tpPrev, dwRetLen);
    end;
    CloseHandle(MyToken);
  end;

begin
  SetPrivilege('SeShutdownPrivilege', true);
  case How of
    sfClose: Application.Terminate;
    sfShutdown: ExitWindowsEx(EWX_SHUTDOWN or EWX_POWEROFF, 0);
    sfLogOff: ExitWindowsEx(EWX_LOGOFF or EWX_POWEROFF, 0);
    sfRestart: ExitWindowsEx(EWX_REBOOT, 0);
    sfHibernate: SetSystemPowerState(false, false);
    sfSuspend: SetSystemPowerState(true, false);
  end;
  SetPrivilege('SeShutdownPrivilege', false);
end;

function MyMessageBox(Msg, Caption: string; Flags: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(Msg), PChar(Caption), Flags);
end;

function GetOptionString: string;
begin
  Result := '';
  with MP3Settings do
  begin
    if OnlyCustomOptions then
    begin
      Result := ' ' + Trim(CustomOptions);
      Exit;
    end;

    //-- set the desired bitrate
    Result := Result + ' -b ' + IntToStr(Bitrate);

    //-- set LAME Flags
    case LameMode of
      lmStereo: Result := Result + ' -m s';
      lmJointStereo: Result := Result + ' -m j';
      lmForcedJointStereo: Result := Result + ' -m f';
      lmMono: Result := Result + ' -m m';
    end;

    case LameOptimization of
      loSpeed: Result := Result + ' -f';
      loQuality: Result := Result + ' -h';
      loVoice: Result := Result + ' --voice';
    end;

    if IncludeCRC then Result := Result + ' -p';

    //-- set MP3 Flags
    if mfCopy in Flags then Result := Result + ' -o';
    if mfCopyright in Flags then Result := Result + ' -c';

    //-- set VBR stuff
    if VBREnabled then
    begin
      if VBRUseABR then
        Result := Result + ' --abr ' + IntToStr(VBRABRTargetBitrate)
      else
        Result := Result + ' -V ' + IntToStr(VBRQuality);
      Result := Result + ' -B ' + IntToStr(VBRMaxBitrate);
      if VBRStrictMin then Result := Result + ' -F';
      if VBRDisableTag then Result := Result + ' -t';
    end;

    //-- Resampling
    if ResampleFreq <> rfDefault then
    begin
      case ResampleFreq of
        rf16kHz: Result := Result + ' --resample 16';
        rf22kHz: Result := Result + ' --resample 22.05';
        rf24kHz: Result := Result + ' --resample 24';
        rf32kHz: Result := Result + ' --resample 32';
        rf44kHz: Result := Result + ' --resample 44.1';
        rf48kHz: Result := Result + ' --resample 48';
      end;
    end;

    //-- Highpass filter
    if HighpassEnabled then
    begin
      Result := Result + ' --highpass ' + StringReplace(FloatToStr(HighpassFreq), DecimalSeparator, '.', []);
      if HighWidthEnabled then
        Result := Result + ' --highpass-width ' + StringReplace(FloatToStr(HighpassWidth), DecimalSeparator, '.', []);
    end;

    //-- Lowpass filter
    if LowpassEnabled then
    begin
      Result := Result + ' --lowpass ' + StringReplace(FloatToStr(LowpassFreq), DecimalSeparator, '.', []);
      if LowWidthEnabled then
        Result := Result + ' --lowpass-width ' + StringReplace(FloatToStr(LowpassWidth), DecimalSeparator, '.', []);
    end;

    //-- Expert options
    if qLevel >= 0 then
      Result := Result + ' -q ' + IntToStr(qLevel);

    case ATHControl of
      athOnly: Result := Result + ' --athonly';
      athDisabled: Result := Result + ' --noath';
      athShort: Result := Result + ' --athshort';
    end;

    if DifferentBlockTypes then Result := Result + ' -d';
    if DisableFiltering then Result := Result + ' -k';
    if NoRes then Result := Result + ' --nores';
    if NoShort then Result := Result + ' --noshort';
    if ISO then Result := Result + ' --strictly-enforce-ISO';

    if Trim(CustomOptions) <> '' then
      Result := Result + ' ' + CustomOptions

  end;
end;

function ReadOptions: Boolean;
var
  MyIni: TIniFile;
begin
  //-- returns false if no INI-file is found
  Result := false;

  if not FileExists(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) then
  begin
    WriteOptions;
    Exit;
  end;

  Result := true;

  //-- read from INI
  MyIni := TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini'));
  try
    with MyIni, Global do
    begin
      //-- System
      LameEncoder := ReadString('System', 'Encoder', LameEncoder);
{$IFDEF USE_FAAC}
      FaacEncoder := ReadString('System', 'FaacEncoder', FaacEncoder);
{$ENDIF}

 //     DefaultEncoder := ReadString('System','DefaultEncoder',LameEncoder);
      DefaultEncoder := LameEncoder;

      ThreadPriority := TThreadPriority(ReadInteger('System', 'Priority', Ord(ThreadPriority)));
      ShutdownFlag := TShutdownFlag(ReadInteger('System', 'ShutdownFlag', Ord(ShutdownFlag)));
      ShowProgress := ReadBool('System', 'ShowProgress', true);
      AutoDelete := ReadBool('System', 'AutoDelete', true);

      ExcerptPosition := ReadInteger('System', 'ExcerptPosition', 1);
      ExcerptLength := ReadInteger('System', 'ExcerptLength', 10);
    end;
  finally
    MyIni.Free;
  end;
  ReadLameOptions(ChangeFileExt(LowerCase(Application.ExeName), '.ini'));
end;

procedure ReadLameOptions(const asFilename: string);
var
  MyIni: TIniFile;
  liCount, i: Integer;
begin
  MyIni := TIniFile.Create(asFilename);
  try
    with MyIni, MP3Settings do
    begin
      //-- General
      Description := ReadString('General', 'Description', Description);
      Bitrate := ReadInteger('General', 'Bitrate', Bitrate);
      OutDir := ReadString('General', 'OutDir', OutDir);
      UseInputDir := ReadBool('General', 'UseInputDir', UseInputDir);

      //-- Options
      LameMode := TLameMode(ReadInteger('Options', 'Mode', Ord(LameMode)));
      LameOptimization := TLameOptimization(ReadInteger('Options', 'Optimization', Ord(LameOptimization)));
      DeleteFileAfterProcessing := ReadBool('Options', 'Delete', DeleteFileAfterProcessing);
      IncludeCRC := ReadBool('Options', 'Crc', IncludeCRC);

      //-- Flags
      Flags := [];
      if ReadBool('Flags', 'Copy', false) then Include(Flags, mfCopy);
      if ReadBool('Flags', 'Copyright', false) then Include(Flags, mfCopyright);

      //-- Last Opened Dir
      Global.LastOpenDir := ReadString('System', 'LastOpenDir', '');

      //-- VBR
      VBREnabled := ReadBool('VBR', 'Enabled', VBREnabled);
      VBRQuality := ReadInteger('VBR', 'Quality', VBRQuality);
      VBRMaxBitrate := ReadInteger('VBR', 'MaxBitrate', VBRMaxBitrate);
      VBRDisableTag := ReadBool('VBR', 'DisableTag', VBRDisableTag);
      VBRStrictMin := ReadBool('VBR', 'StrictMin', VBRStrictMin);
      VBRUseABR := ReadBool('VBR', 'UseABR', VBRUseABR);
      VBRABRTargetBitrate := ReadInteger('VBR', 'ABRTargetBitrate', VBRABRTargetBitrate);

      //-- Expert options
      ATHControl := TATHControl(ReadInteger('Expert', 'ATHControl', Ord(ATHControl)));
      DifferentBlockTypes := ReadBool('Expert', 'DifferentBlockTypes', DifferentBlockTypes);
      DisableFiltering := ReadBool('Expert', 'DisableFiltering', DisableFiltering);
      NoRes := ReadBool('Expert', 'NoRes', NoRes);
      NoShort := ReadBool('Expert', 'NoShort', NoShort);
      ISO := ReadBool('Expert', 'ISO', ISO);
      CustomOptions := ReadString('Expert', 'CustomOptions', CustomOptions);
      OnlyCustomOptions := ReadBool('Expert', 'OnlyCustomOptions', OnlyCustomOptions);
      qLevel := ReadInteger('Expert', 'qLevel', qLevel);
      liCount := ReadInteger('Expert', 'MRUCount', 0);
      for i := 0 to liCount - 1 do
        CustomMRU.Add(ReadString('Expert', 'CustomMRU' + IntToStr(i), ''));

      //-- Audio Processing
      ResampleFreq := TResampleFreq(ReadInteger('Audio', 'Resampling', Ord(ResampleFreq)));
      HighpassEnabled := ReadBool('Audio', 'HighpassEnabled', HighpassEnabled);
      HighpassFreq := ReadFloat('Audio', 'HighpassFreq', HighpassFreq);
      HighWidthEnabled := ReadBool('Audio', 'HighWidthEnabled', HighWidthEnabled);
      HighpassWidth := ReadFloat('Audio', 'HighpassWidth', HighpassWidth);
      LowpassEnabled := ReadBool('Audio', 'LowpassEnabled', LowpassEnabled);
      LowpassFreq := ReadFloat('Audio', 'LowpassFreq', LowpassFreq);
      LowWidthEnabled := ReadBool('Audio', 'LowWidthEnabled', LowWidthEnabled);
      LowpassWidth := ReadFloat('Audio', 'LowpassWidth', LowpassWidth);
    end;
  finally
    MyIni.Free;
  end;
end;

procedure WriteOptions;
var
  MyIni: TIniFile;
begin
  MyIni := TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini'));
  try
    with MyIni, Global do
    begin
      //-- System
      WriteString('System', 'Encoder', LameEncoder);
{$IFDEF USE_FAAC}
      WriteString('System', 'FaacEncoder', FaacEncoder);
{$ENDIF}
    //  WriteString('System', 'DefaultEncoder', DefaultEncoder);
      DefaultEncoder := LameEncoder;

      WriteInteger('System', 'Priority', Ord(ThreadPriority));
      WriteInteger('System', 'ShutdownFlag', Ord(ShutdownFlag));
      WriteBool('System', 'ShowProgress', ShowProgress);
      WriteBool('System', 'AutoDelete', AutoDelete);

      WriteInteger('System', 'ExcerptLength', ExcerptLength);
      WriteInteger('System', 'ExcerptPosition', ExcerptPosition);
    end;
  finally
    MyIni.Free;
  end;
  WriteLameOptions(ChangeFileExt(LowerCase(Application.ExeName), '.ini'));
end;

procedure WriteLameOptions(const asFilename: string);
var
  MyIni: TIniFile;
  i: Integer;
begin
  MyIni := TIniFile.Create(asFilename);
  try
    with MyIni, MP3Settings do
    begin
      //-- General
      WriteString('General', 'Description', Description);
      WriteInteger('General', 'Bitrate', Bitrate);
      WriteString('General', 'OutDir', OutDir);
      WriteBool('General', 'UseInputDir', UseInputDir);

      //-- Options
      WriteInteger('Options', 'Mode', Ord(LameMode));
      WriteInteger('Options', 'Optimization', Ord(LameOptimization));
      WriteBool('Options', 'Crc', IncludeCRC);
      WriteBool('Options', 'Delete', DeleteFileAfterProcessing);

      //-- Flags
      WriteBool('Flags', 'Copy', mfCopy in Flags);
      WriteBool('Flags', 'Copyright', mfCopyright in Flags);

      //-- Last Opened Dir
      WriteString('System', 'LastOpenDir', Global.LastOpenDir);

      //-- VBR
      WriteBool('VBR', 'Enabled', VBREnabled);
      WriteInteger('VBR', 'Quality', VBRQuality);
      WriteInteger('VBR', 'MaxBitrate', VBRMaxBitrate);
      WriteBool('VBR', 'DisableTag', VBRDisableTag);
      WriteBool('VBR', 'StrictMin', VBRStrictMin);
      WriteBool('VBR', 'UseABR', VBRUseABR);
      WriteInteger('VBR', 'ABRTargetBitrate', VBRABRTargetBitrate);

      //-- Expert options
      WriteInteger('Expert', 'ATHControl', Ord(ATHControl));
      WriteBool('Expert', 'DifferentBlockTypes', DifferentBlockTypes);
      WriteBool('Expert', 'DisableFiltering', DisableFiltering);
      WriteBool('Expert', 'NoRes', NoRes);
      WriteBool('Expert', 'NoShort', NoShort);
      WriteBool('Expert', 'ISO', ISO);
      WriteString('Expert', 'CustomOptions', CustomOptions);
      WriteBool('Expert', 'OnlyCustomOptions', OnlyCustomOptions);
      WriteInteger('Expert', 'qLevel', qLevel);

      WriteInteger('Expert', 'MRUCount', CustomMRU.Count);
      for i := 0 to CustomMRU.Count - 1 do
        WriteString('Expert', 'CustomMRU' + IntToStr(i), CustomMRU[i]);

      //-- Audio Processing
      WriteInteger('Audio', 'Resampling', Ord(ResampleFreq));
      WriteBool('Audio', 'HighpassEnabled', HighpassEnabled);
      WriteFloat('Audio', 'HighpassFreq', HighpassFreq);
      WriteBool('Audio', 'HighWidthEnabled', HighWidthEnabled);
      WriteFloat('Audio', 'HighpassWidth', HighpassWidth);
      WriteBool('Audio', 'LowpassEnabled', LowpassEnabled);
      WriteFloat('Audio', 'LowpassFreq', LowpassFreq);
      WriteBool('Audio', 'LowWidthEnabled', LowWidthEnabled);
      WriteFloat('Audio', 'LowpassWidth', LowpassWidth);
    end;
  finally
    MyIni.Free;
  end;
end;

procedure StoreInIni(const asSection, asItem: string; const aiValue: Integer);
begin
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    WriteInteger(asSection, asItem, aiValue);
  finally
    Free;
  end;
end;

procedure StoreInIni(const asSection, asItem, asValue: string);
begin
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    WriteString(asSection, asItem, asValue);
  finally
    Free;
  end;
end;

procedure StoreInIni(const asSection, asItem: string; const abValue: Boolean);
begin
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    WriteBool(asSection, asItem, abValue);
  finally
    Free;
  end;
end;

procedure RepositionForm(const asSection: string; aForm: TForm; const asIniFile: string);
var
  lsIniFile: string;
  lbMaximized: Boolean;
  liLeft,
    liTop,
    liWidth,
    liHeight: Integer;
begin
  lsIniFile := asIniFile;
  //-- Empty Parameter -> ExeFile plus .INI
  if Trim(lsIniFile) = '' then
    lsIniFile := ChangeFileExt(LowerCase(Application.ExeName), '.ini');

  with TIniFile.Create(lsIniFile) do
  try
    lbMaximized := ReadBool(asSection, 'maximized', false);
    liLeft := ReadInteger(asSection, 'x', aForm.Left);
    liTop := ReadInteger(asSection, 'y', aForm.Top);
    liHeight := ReadInteger(asSection, 'height', aForm.Height);
    liWidth := ReadInteger(asSection, 'width', aForm.Width);

      //-- if wider or higher then screen, shrink form
    if liWidth > Screen.Width then liWidth := Screen.Width;
    if liHeight > Screen.Height then liHeight := Screen.Height;
      //-- move form so it will be fully visible
    if liLeft + liWidth > Screen.Width then liLeft := Screen.Width - liWidth;
    if liTop + liHeight > Screen.Height then liTop := Screen.Height - liHeight;
    if liLeft < 0 then liLeft := 0;
    if liTop < 0 then liTop := 0;
  finally
    Free;
  end;

  aForm.Left := liLeft;
  aForm.Top := liTop;
  aForm.Width := liWidth;
  aForm.Height := liHeight;

  if lbMaximized then
    aForm.WindowState := wsMaximized
  else
    aForm.WindowState := wsNormal;
end;

procedure RegisterFormPosition(const asSection: string; aForm: TForm; const asIniFile: string);
var
  lsIniFile: string;
begin
  lsIniFile := asIniFile;
  //-- Empty Parameter -> ExeFile plus .INI
  if Trim(lsIniFile) = '' then
    lsIniFile := ChangeFileExt(LowerCase(Application.ExeName), '.ini');

  with TIniFile.Create(lsIniFile) do
  try
    WriteInteger(asSection, 'x', aForm.Left);
    WriteInteger(asSection, 'y', AForm.Top);
    WriteInteger(asSection, 'height', aForm.Height);
    WriteInteger(asSection, 'width', aForm.Width);
    WriteBool(asSection, 'maximized', aForm.WindowState = wsMaximized);
  finally
    Free;
  end;
end;

function FileDateTime(const FileName: string): System.TDateTime;
begin
  Result := FileDateToDateTime(FileAge(FileName));
end;

function GetFileSize(const FileName: string): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := -1; //-- assume worst cas
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

function StrToFloatDef(const Value: string; const Default: Extended): Extended;
var
  Code: Integer;
begin
  Val(StringReplace(Value, DecimalSeparator, '.', []), Result, Code);
  if Code <> 0 then Result := Default;
end;

function GetSystemErrorMessage(const aiErrorCode: Integer): string;
var
  liLength: Integer;
  lsError: string;
begin
  SetLength(lsError, 250);
  liLength := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, aiErrorCode, 0,
    PChar(lsError), 250, nil);

  if liLength > 0 then
    SetLength(lsError, liLength)
  else
    lsError := 'Error during FormatMessage, Error Code: ' +
      IntToStr(GetLastError);
  Result := lsError;
end;

function DeleteZeroByteFile(const FileName: string): Boolean;
begin
  Result := false;
  //-- delete's FileName if it's size is 0!
  if FileExists(FileName) and (GetFileSize(FileName) = 0) then
    Result := DeleteFile(FileName);
end;

//-- this function takes a string and converts it.
//-- stuff like "#20" is mapped to a space
//-- this function is probably not very error friendly and could be improved,
//-- but as far as no one plays with the .dat file, everything should be fine.

function InfoMsgFormatToString(const Value: string): string;
var
  lsRemain, lsHex: string;
  liPos: integer;
begin
  Result := '';
  lsRemain := Value;

  liPos := Pos('#', lsRemain);
  while liPos > 0 do
  begin
    Result := Result + Copy(lsRemain, 1, liPos - 1);
    lsHex := '$' + Copy(lsRemain, liPos + 1, 2);
    Result := Result + Chr(StrToIntDef(lsHex, 32));
    Delete(lsRemain, 1, liPos + 2);
    liPos := Pos('#', lsRemain);
  end;

  Result := Result + lsRemain;
end;

procedure ReadSystemData;
var
  MyIni: TIniFile;
  i: Integer;
  lsValue: string;
const
  INFO_DEFAULT = '<none>';
begin
  MyIni := TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.dat'));
  try
    //-- read all info strings for the encoding
    Global.EncoderInfoStrings.Clear;
    i := 0;
    repeat
      Inc(i);
      lsValue := InfoMsgFormatToString(MyIni.ReadString('EncodeInfoMsgs', IntToStr(i), INFO_DEFAULT));
      if lsValue <> INFO_DEFAULT then
        Global.EncoderInfoStrings.Add(lsValue);
    until lsValue = INFO_DEFAULT;
    //calculate the Length of those strings
    SetLength(Global.EncoderInfoStringsLength, Global.EncoderInfoStrings.Count);
    for i := 0 to Global.EncoderInfoStrings.Count - 1 do
      Global.EncoderInfoStringsLength[i] := Length(Global.EncoderInfoStrings[i]);

    //-- read all info strings for the decoding
    Global.DecodeInfoStrings.Clear;
    i := 0;
    repeat
      Inc(i);
      lsValue := InfoMsgFormatToString(MyIni.ReadString('DecodeInfoMsgs', IntToStr(i), INFO_DEFAULT));
      if lsValue <> INFO_DEFAULT then
        Global.DecodeInfoStrings.Add(lsValue);
    until lsValue = INFO_DEFAULT;
    //calculate the Length of those strings
    SetLength(Global.DecodeInfoStringsLength, Global.DecodeInfoStrings.Count);
    for i := 0 to Global.DecodeInfoStrings.Count - 1 do
      Global.DecodeInfoStringsLength[i] := Length(Global.DecodeInfoStrings[i]);

    //-- read filter for open dialog
    Global.InputFileTypes.Clear;
    MyIni.ReadSectionValues('InputFileTypes', Global.InputFileTypes);
  finally
    MyIni.Free;
  end;
end;

function CreateFilterFromStringList(List: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
  begin
    Result := Result + List.Names[i] + '|'
      + List.Values[List.Names[i]] + '|';
  end;
  //-- always add the "all files" filter
  Result := Result + 'All Files|*.*';
end;

//-- check if the extension is allowed, i.e. if it is
//-- contained in the "Global.InputFileTypes" Stringlist

function IsExtensionAllowed(const asExt: string): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Global.InputFileTypes.Count - 1 do
  begin
    if Pos(LowerCase(asExt),
      LowerCase(Global.InputFileTypes.Values[Global.InputFileTypes.Names[i]])) > 0 then
    begin
      Result := true;
      Break;
    end;
  end;
end;

procedure ParseM3UFile(const asFilename: string; var Strings: TStringList);
var
  F: Textfile;
  lsLine: string;
begin
  AssignFile(F, asFilename);
  try
    FileMode := 0;
    Reset(F);
    while not EOF(F) do
    begin
      Readln(F, lsLine);
      if Trim(lsLine) = '' then Continue;
      if lsLine[1] = '#' then Continue;
      if Length(lsLine) < 2 then Continue;
      if lsLine[2] = ':' then
        Strings.Add(lsLine) // absolute path
      else
        if lsLine[1] = '\' then
        Strings.Add(ExtractFileDrive(asFilename) + lsLine)
      else
        Strings.Add(ExtractFileDir(asFilename) + '\' + lsLine);
    end;
  finally
    CloseFile(F);
  end;
end;


procedure ParsePLSFile(const asFilename: string; var Strings: TStringList);
var
  F: textfile;
  lsLine: string;
  liPos: Integer;
begin
  AssignFile(F, asFilename);
  try
    FileMode := 0;
    Reset(F);

    while not EOF(F) do
    begin
      Readln(F, lsLine);
      if not (Copy(lsLine, 1, 4) = 'File') then Continue;
      liPos := Pos('=', lsLine) + 1;
      if Length(lsLine) < liPos + 1 then Continue;
      if lsLine[liPos + 1] = ':' then
        Strings.Add(Copy(lsLine, liPos, Length(lsLine))) // absolute path
      else
        if lsLine[liPos] = '\' then
        Strings.Add(ExtractFileDrive(asFilename) + Copy(lsLine, liPos, Length(lsLine)))
      else
        Strings.Add(ExtractFileDir(asFilename) + '\' + Copy(lsLine, liPos, Length(lsLine)));
    end;
  finally
    CloseFile(F);
  end;
end;

//-----------------------------------------------
//-- Cursor-Management
//-----------------------------------------------
var
  CursorCount: Integer = 0;

procedure PushCursor;
begin
  Inc(CursorCount);
  Screen.Cursor := crHourglass;
end;

procedure PopCursor;
begin
  if CursorCount <= 0 then exit;
  Dec(CursorCount);
  if CursorCount = 0 then Screen.Cursor := crDefault;
end;

procedure ResetCursor;
begin
  CursorCount := 0;
  Screen.Cursor := crDefault;
end;

//-----------------------------------------------
//-- Audio helper functions
//-----------------------------------------------

function GetNumberFramesVBR(const asFile: string): Integer;
begin
  with TMpegAudio.Create do
  try
    FileName := asFile;
    if Mp3Type = mpVBR then
      Result := NumberOfFrames
    else
      Result := 0;
  finally
    Free;
  end;
end;

function ExtractWaveRate(const asFile: string): Integer;
var
  Datafile: file;
  Buffer: array[1..1024] of Byte;
  liNumRead: Integer;
begin
  //-- this function can be greatly expanded; e.g., it won't check for
  //-- a valid WAV header.
  Result := 0;
  AssignFile(Datafile, asFile);
  try
    FileMode := fmOpenRead;
    Reset(Datafile, 1);
    BlockRead(Datafile, Buffer, sizeof(Buffer), liNumRead);

    if liNumRead > 32 then
      Result := Buffer[29] + 256 * Buffer[30] + 256 * 256 * Buffer[31];
  finally
    CloseFile(Datafile);
  end;
end;

function DetermineFileInfo(const asFile: string; var aiSize, aiBitrate: integer; var adtLength: TDateTime): boolean;
var
  lsExt: string;
  liRate: integer;
begin
  //-- initialize results with worst-case values
  Result := false;
  aiSize := 0;
  adtLength := 0;
  aiBitrate := 0;

  lsExt := LowerCase(ExtractFileExt(asFile));

  //-- determine file size in bytes
  aiSize := GetFileSize(asFile);

  //-- determine file length & bitrate if we have appropriate means
  if (lsExt = '.wav') then
  begin
    liRate := ExtractWaveRate(asFile);
    if liRate > 0 then
      adtLength := (aiSize / liRate) / (60 * 60 * 24);
    if adtLength > 0 then
      //aiBitrate := Round((aiSize * 8) / (adtLength * 24 * 60 * 60 * 1000));
      aiBitrate := Round((aiSize * 8) / ((aiSize / liRate) * 1000))
  end;

  if (lsExt = '.mp3') or (lsExt = '.mp2') or (lsExt = '.mp1') then
  begin
    with TMpegAudio.Create do
    try
      Filename := asFile;
      adtLength := DurationExact / (60 * 60 * 24);
      if Bitrate = -1 then
        aiBitrate := Trunc((FileLength * -8) / (DurationExact * 1000)) //VBR
      else
        aiBitrate := Bitrate; //CBR
    finally
      Free;
    end;
  end;
end;

//-----------------------------------------------------------------
//-- helper functions for "only one instance" functionality
//-----------------------------------------------------------------

procedure PassToOtherInstance(const handle: HWND);
var
  liParamCount: Integer;
  i: Integer;
  lsCmdLine: string;
  at: atom;
begin
  liParamCount := System.ParamCount;
  for i := 1 to liParamCount do
    lsCmdLine := lsCmdLine + System.ParamStr(i);

  at := GlobalAddAtom(PChar(lsCmdLine));
  {Hopefully only _one_ (or zero) parameters}

  if SendMessage(handle, WM_PASSED_FROM_INSTANCE, liParamCount, DWORD(at)) < 0 then
  begin
    MyMessageBox(MSG_RL_IS_BUSY, MSG_INFO, MB_ICONEXCLAMATION or MB_OK);
  end;
end;


function EnumFunc(Wnd: HWND; var pWnd: HWND): bool; stdcall;
var
  ClassName, WindowText: array[0..30] of char;
begin
  Result := true; {true means _not_ our window}
  GetWindowText(wnd, WindowText, SizeOf(WindowText));
  GetClassName(wnd, ClassName, SizeOf(ClassName));
  if Copy(WindowText, 1, 11) = 'RazorLame 1' then
  begin
    if (Wnd <> Application.handle) and (ClassName = 'TFormMain') then
    begin
      pWnd := Wnd;
      result := false;
    end;
  end;
end;

end.

