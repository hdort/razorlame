(* (c) Copyright 2000-2005  -  Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit UtilFuncs;

interface

uses Forms, Controls, Classes, Dialogs;


function MyMsgDlg(const asMsg, asCaption: string; aDlgType: TMsgDlgType;
  aButtons: TMsgDlgButtons; const Args: array of string): Integer;
function MyMessageBox(Msg, Caption: string; Flags: Integer): Integer;

function GetOptionString: string;
function ReadOptions: Boolean;
procedure WriteOptions;
procedure WriteLameOptions(const asFilename: string);
procedure ReadLameOptions(const asFilename: string);

procedure FillResampleStrings(Strings: TStrings);

type
  TShutdownFlag = (sfClose, sfShutdown, sfLogOff, sfRestart, sfHibernate, sfSuspend);

procedure ShutdownWindows(how: TShutdownFlag);

procedure StoreInIni(const asSection, asItem: string; const aiValue: Integer); overload;
procedure StoreInIni(const asSection, asItem, asValue: string); overload;
procedure StoreInIni(const asSection, asItem: string; const abValue: Boolean); overload;

function IniReadBool(const asSection, asItem: string; const abDefault:Boolean): boolean;
function IniReadInteger(const asSection, asItem: string; const aiDefault: Integer): Integer;

procedure ReadSystemData;

//-- called without asIniFile, ExeFileName is used with .ini extension
function RepositionForm(const asSection: string; aForm: TForm; const asIniFile: string = ''): boolean;
procedure RegisterFormPosition(const asSection: string; aForm: TForm; const asIniFile: string = '');

//- needed those when kicking RxLib
function FileDateTime(const FileName: string): TDateTime;
function GetFileSize(const FileName: string): Int64;

//-- other util funcs
function StrToFloatDef(const Value: string; const Default: Extended): Extended;
function GetSystemErrorMessage(const aiErrorCode: Integer): string;
function DeleteZeroByteFile(const FileName: string): Boolean;
function CreateFilterFromStringList(List: TStringList): string;
function IsExtensionAllowed(const asExt: string): Boolean;
function FloatToInternalStr(const afValue: Extended): string;
function InternalStrToFloat(const asValue: string): Extended;

procedure FixDPI(f: TForm; DesignDPI: integer);
procedure SetFont(AWinControl: TWinControl; const DesignFont, SelectedFont: string);

//-- Cursor-Management
procedure PushCursor;
procedure PopCursor;
procedure ResetCursor;

implementation

uses Windows, StdCtrls, SysUtils, FileCtrl, IniFiles, Globals;

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

    //-- set alt-presets
    if UsePresets then
    begin
      case PresetMode of
        pmVBR: Result := Result + VBRPresets[PresetVBRItem];
        pmABR: if PresetCustomABR then
                 Result := Result + ' --alt-preset ' + IntToStr(PresetCustomABRValue)
               else Result := Result + ABRPresets[PresetABRItem];
        pmCBR: Result := Result + CBRPresets[PresetCBRItem];
      end;
      if Trim(CustomOptions) <> '' then
        Result := Result + ' ' + CustomOptions;

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
        rf8kHz: Result := Result + ' --resample 8';
        rf11kHz: Result := Result + ' --resample 11.025';
        rf12kHz: Result := Result + ' --resample 12';
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
      Encoder := ReadString('System', 'Encoder', Encoder);
      ThreadPriority := TThreadPriority(ReadInteger('System', 'Priority', Ord(ThreadPriority)));
      ShutdownFlag := TShutdownFlag(ReadInteger('System', 'ShutdownFlag', Ord(ShutdownFlag)));
      SelectedFont := ReadString('System', 'Font', DESIGN_FONT);
      XPMenu := ReadBool('System', 'XPMenu', XPMenu);
      LRColor := ReadInteger('System', 'LRColor', LRColor);
      MSColor := ReadInteger('System', 'MSColor', MSColor);
    end;
  finally
    MyIni.Free;
  end;
  ReadLameOptions(ChangeFileExt(LowerCase(Application.ExeName), '.ini'));
end;

procedure ReadLameOptions(const asFilename: string);
var
  MyIni: TIniFile;

begin
  MyIni := TIniFile.Create(asFilename);
  try
    with MyIni, MP3Settings do
    begin
      //-- Presets
      UsePresets := false;
      UsePresets := ReadBool('Presets', 'UsePresets', UsePresets);
      PresetMode := TPresetMode(ReadInteger('Presets', 'Mode', Ord(PresetMode)));

      PresetVBRItem := ReadInteger('Presets', 'VBRItem', PresetVBRItem);
      PresetABRItem := ReadInteger('Presets', 'ABRItem', PresetABRItem);
      PresetCBRItem := ReadInteger('Presets', 'CBRItem', PresetCBRItem);

      PresetCustomABR := ReadBool('Presets', 'PresetCustomABR', PresetCustomABR);
      PresetCustomABRValue := ReadInteger('Presets', 'CustomABRValue', PresetCustomABRValue);

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

      //-- Audio Processing
      ResampleFreq := TResampleFreq(ReadInteger('Audio', 'Resampling', Ord(ResampleFreq)));
      HighpassEnabled := ReadBool('Audio', 'HighpassEnabled', HighpassEnabled);
      HighpassFreq := InternalStrToFloat(ReadString('Audio', 'HighpassFreq', FloatToInternalStr(HighpassFreq)));
      HighWidthEnabled := ReadBool('Audio', 'HighWidthEnabled', HighWidthEnabled);
      HighpassWidth := InternalStrToFloat(ReadString('Audio', 'HighpassWidth', FloatToInternalStr(HighpassWidth)));
      LowpassEnabled := ReadBool('Audio', 'LowpassEnabled', LowpassEnabled);
      LowpassFreq := InternalStrToFloat(ReadString('Audio', 'LowpassFreq', FloatToInternalStr(LowpassFreq)));
      LowWidthEnabled := ReadBool('Audio', 'LowWidthEnabled', LowWidthEnabled);
      LowpassWidth := InternalStrToFloat(ReadString('Audio', 'LowpassWidth', FloatToInternalStr(LowpassWidth)));
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
      WriteString('System', 'Encoder', Encoder);
      WriteInteger('System', 'Priority', Ord(ThreadPriority));
      WriteInteger('System', 'ShutdownFlag', Ord(ShutdownFlag));
      WriteString('System', 'Font', SelectedFont);
      WriteBool('System', 'XPMenu', XPMenu);
      WriteInteger('System', 'LRColor', LRColor);
      WriteInteger('System', 'MSColor', MSColor);
    end;
  finally
    MyIni.Free;
  end;
  WriteLameOptions(ChangeFileExt(LowerCase(Application.ExeName), '.ini'));
end;

procedure WriteLameOptions(const asFilename: string);
var
  MyIni: TIniFile;

begin
  MyIni := TIniFile.Create(asFilename);
  try
    with MyIni, MP3Settings do
    begin
      //-- Presets
      WriteBool('Presets', 'UsePresets', UsePresets);
      WriteInteger('Presets', 'Mode', Ord(PresetMode));

      WriteInteger('Presets', 'VBRItem', PresetVBRItem);
      WriteInteger('Presets', 'ABRItem', PresetABRItem);
      WriteInteger('Presets', 'CBRItem', PresetCBRItem);

      WriteBool('Presets', 'PresetCustomABR', PresetCustomABR);
      WriteInteger('Presets', 'CustomABRValue', PresetCustomABRValue);

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

      //-- Audio Processing
      WriteInteger('Audio', 'Resampling', Ord(ResampleFreq));
      WriteBool('Audio', 'HighpassEnabled', HighpassEnabled);
      WriteString('Audio', 'HighpassFreq', FloatToInternalStr(HighpassFreq));
      WriteBool('Audio', 'HighWidthEnabled', HighWidthEnabled);
      WriteString('Audio', 'HighpassWidth', FloatToInternalStr(HighpassWidth));
      WriteBool('Audio', 'LowpassEnabled', LowpassEnabled);
      WriteString('Audio', 'LowpassFreq', FloatToInternalStr(LowpassFreq));
      WriteBool('Audio', 'LowWidthEnabled', LowWidthEnabled);
      WriteString('Audio', 'LowpassWidth', FloatToInternalStr(LowpassWidth));
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

function IniReadBool(const asSection, asItem: string; const abDefault: Boolean): boolean;
begin
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    Result := ReadBool(asSection, asItem, abDefault);
  finally
    Free;
  end;
end;

function IniReadInteger(const asSection, asItem: string; const aiDefault: Integer): Integer;
begin
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    Result := ReadInteger(asSection, asItem, aiDefault);
  finally
    Free;
  end;
end;

function RepositionForm(const asSection: string; aForm: TForm; const asIniFile: string): boolean;
var
  lsIniFile: string;
  lbMaximized: Boolean;
  liLeft,
    liTop,
    liWidth,
    liHeight: Integer;
begin
  Result := true;
  
  lsIniFile := asIniFile;
  //-- Empty Parameter -> ExeFile plus .INI
  if Trim(lsIniFile) = '' then
    lsIniFile := ChangeFileExt(LowerCase(Application.ExeName), '.ini');

  with TIniFile.Create(lsIniFile) do
  try
    //if not SectionExists(asSection) then Result := false;
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

function GetFileSize(const FileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := -1; //-- assume worst cas
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    //Result := SearchRec.Size;
    //Result := SearchRec.FindData.nFileSizeLow;
    Int64Rec(Result).Lo := SearchRec.FindData.nFileSizeLow;
    Int64Rec(Result).Hi := SearchRec.FindData.nFileSizeHigh;
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

function FloatToInternalStr(const afValue: Extended): string;
begin
  Result := StringReplace(FloatToStr(afValue), DecimalSeparator, '.', []);
end;

function InternalStrToFloat(const asValue: string): Extended;
begin
  Result := StrToFloat(StringReplace(asValue, '.', DecimalSeparator, []));
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

procedure FillResampleStrings(Strings: TStrings);
begin
  Strings.Clear;
  Strings.AddObject('default', TObject(Ord(rfDefault)));
  Strings.AddObject('8 kHz', TObject(Ord(rf8kHz)));
  Strings.AddObject('11.025 kHz', TObject(Ord(rf11kHz)));
  Strings.AddObject('12 kHz', TObject(Ord(rf12kHz)));
  Strings.AddObject('16 kHz', TObject(Ord(rf16kHz)));
  Strings.AddObject('22.05 kHz', TObject(Ord(rf22kHz)));
  Strings.AddObject('24 kHz', TObject(Ord(rf24kHz)));
  Strings.AddObject('32 kHz', TObject(Ord(rf32kHz)));
  Strings.AddObject('44.1 kHz', TObject(Ord(rf44kHz)));
  Strings.AddObject('48 kHz', TObject(Ord(rf48kHz)));
end;

procedure FixDPI(f: TForm; DesignDPI: integer);
(*
        Small/Large system font solution
        Have a TForm's properties Scaled and AutoScroll set to false
        and after creating your form call this procedure like:
                FixDPI (TForm1, 96) // If you designed in small fonts

        Author: Tomislav Kardas, Zagreb, Croatia, Europe
        Platform: Delphi 4 C/S
*)

//todo: adjust constraints (TControl property!) 

var
  ScaleM, ScaleD: integer;

  function Scale(x: longint): longint;
  begin
    result := (x * ScaleM + ScaleD div 2) div ScaleD;
  end;

  procedure FixControl(c: TControl);
  var
    x: integer;
    p: TWinControl;
    fixwidth, fixheight: boolean;
  begin
    //-- don't fix width for AutoSize Labels (added by hdo, 02.10.2001)
    fixwidth := true;
    if c is TCustomLabel then
      if TLabel(c).AutoSize then fixwidth := false;

    p := c.Parent;
    if c.Align <> alNone then
    begin
      // Fixing width
      if fixwidth then
        if ((c.Align = alLeft) and (akRight in c.Anchors)) or
          ((c.Align = alRight) and (akLeft in c.Anchors)) then
          c.Width := p.ClientWidth - Scale(p.ClientWidth - c.Width)
        else
          if (c.Align = alLeft) or (c.Align = alRight) then
            c.Width := Scale(c.Width);

      // Fixing height
      if ((c.Align = alTop) and (akBottom in c.Anchors)) or
        ((c.Align = alBottom) and (akTop in c.Anchors)) then
        c.Height := p.ClientHeight - Scale(p.ClientHeight - c.Height)
      else
        if (c.Align = alTop) or (c.Align = alBottom) then
          c.Height := Scale(c.Height);
    end
    else
    begin
      // Fixing width
      x := p.ClientWidth - c.Width - c.Left;
      if akLeft in c.Anchors then
      begin
        c.Left := Scale(c.Left);
        if fixwidth then
          if akRight in c.Anchors then
            c.Width := p.ClientWidth - c.Left - Scale(x)
          else
            c.Width := Scale(c.Width);
      end
      else
        if akRight in c.Anchors then
        begin
          if fixwidth then c.Width := Scale(c.Width);
          c.Left := p.ClientWidth - c.Width - Scale(x);
        end
        else
        begin
          c.Left := Scale(c.Left);
          if fixwidth then c.Width := Scale(c.Width);
        end;

      // Fixing height
      fixheight := true;
      if (c is TCustomEdit) and not (c is TCustomMemo) then
        fixheight := false;
      x := p.ClientHeight - c.Height - c.Top;
      if akTop in c.Anchors then
      begin
        c.Top := Scale(c.Top);
        if fixheight then
        begin
          if akBottom in c.Anchors then
            c.Height := p.ClientHeight - c.Top - Scale(x)
          else
            c.Height := Scale(c.Height);
        end;
      end
      else if akBottom in c.Anchors then
      begin
        if fixheight then
          c.Height := Scale(c.Height);
        c.Top := p.ClientHeight - c.Height - Scale(x);
      end
      else
      begin
        c.Top := Scale(c.Top);
        if fixheight then
          c.Height := Scale(c.Height);
      end;
    end;
  end;

  procedure FixControls(c: TWinControl);
  var
    i: integer;
  begin
    for i := 0 to c.ControlCount - 1 do
      if c.Controls[i].Owner = f then begin
        FixControl(c.Controls[i]);
        if c.Controls[i] is TWinControl then
          FixControls(TWinControl(c.Controls[i]));
      end;
  end;

begin
  if DesignDPI <> Screen.PixelsPerInch then begin
    ScaleM := Screen.PixelsPerInch;
    ScaleD := DesignDPI;
    f.ClientWidth := Scale(f.ClientWidth);
    f.ClientHeight := Scale(f.ClientHeight);
    FixControls(f);
  end;
end;

type
  TFontControl = class(TControl)
  public
    property Font;
  end;

procedure SetFont(AWinControl: TWinControl; const DesignFont, SelectedFont: string);
var
  i: integer;
begin
  if Screen.Fonts.IndexOf(SelectedFont) < 0 then exit; //-- Font not available
  if SelectedFont = DesignFont then exit; //-- same fonts!
  for i := 0 to AWinControl.ControlCount - 1 do
  begin
    with TFontControl(AWinControl.Controls[i]) do
     if Font.Name = DesignFont then Font.Name := SelectedFont;
    if AWinControl.Controls[i] is TWinControl then
      SetFont(TWinControl(AWinControl.Controls[i]), DesignFont, SelectedFont);
  end;
end;

end.

