(* (c) Copyright 2000,2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit globals;

{$I ConditionalDefines.inc}

interface

uses Windows, Classes, Graphics, ResStr, UtilFuncs;

const
  RL_VERSION = 'RazorLame 1.1.5';
  APP_TITLE = 'RazorLame';

  LAME_HOMEPAGE = 'http://www.mp3dev.org/';
  RAZORLAME_HOMEPAGE = 'http://www.dors.de/razorlame/';
  //RAZORLAME_FORUM = 'http://pub22.ezboard.com/brazorlame';
  RAZORLAME_FORUM = 'http://www.dors.de/razorlame/forum';

  TOOLBAR_SECTION = 'Toolbar';
  LAYOUT_ITEM = 'Layout';
  VISIBLE_ITEM = 'Visible';
  FILES_SECTION = 'Files';
  FORM_SECTION = 'Form';
  PROG_FORM_SECTION = 'ProgressForm';

  LAYOUT_ICONS = 1;
  LAYOUT_CAPTIONS = 2;
  LAYOUT_BOTH = 3;

  DESIGN_FONT = 'MS Sans Serif';

type

  TMp3Flag = (mfCopy, mfCopyright);
  TMp3Flags = set of TMp3Flag;
  TLameMode = (lmStereo, lmJointStereo, lmForcedJointStereo, lmMono, lmDefault);
  TLameOptimization = (loNone, loSpeed, loQuality);
  TResampleFreq = (rfDefault, rf16kHz, rf22kHz, rf24kHz, rf32kHz, rf44kHz, rf48kHz, rf8kHz, rf11kHz, rf12kHz);
  //'default', '16 kHz', '22.05 kHz', '24 kHz', '32 kHz', '44.1 kHz', '48 kHz', '8 kHz', '11.025 kHz', '12 kHz'
  TATHControl = (athDefault, athOnly, athDisabled, athShort);
  TThreadPriority = (tpIdle, tpNormal, tpHigh, tpRealtime);

  TMP3Settings = record
    Description: string;
    Bitrate: Integer;
    LameMode: TLameMode;
    LameOptimization: TLameOptimization;
    VBREnabled: Boolean;
    VBRQuality: Integer;
    VBRMaxBitrate: Integer;
    VBRDisableTag: Boolean;
    VBRStrictMin: Boolean;
    VBRUseABR: Boolean;
    VBRABRTargetBitrate: Integer;
    ResampleFreq: TResampleFreq;
    HighpassFreq: Double;
    HighpassEnabled: Boolean;
    HighpassWidth: Double;
    HighWidthEnabled: Boolean;
    LowpassFreq: Double;
    LowpassEnabled: Boolean;
    LowpassWidth: Double;
    LowWidthEnabled: Boolean;
    IncludeCRC: Boolean;
    Flags: TMp3Flags;
    ATHControl: TATHControl;
    DifferentBlockTypes: Boolean;
    DisableFiltering: Boolean;
    NoRes: Boolean;
    NoShort: Boolean;
    ISO: Boolean;
    CustomOptions: string;
    OnlyCustomOptions: Boolean;
    OutDir: string;
    DeleteFileAfterProcessing: Boolean;
    UseInputDir: Boolean;
    qLevel: Integer;
  end;

  TGlobalVars = record
    Encoder: string;
    FilesToEncode: Integer;
    FilesEncoded: Integer;
    FilesWithErrors: Integer;
    Log: TStringList;
    ErrorOccurred: Boolean;
    ErrorOccurredInBatch: Boolean;
    CurrentFileFullname: string;
    CurrentFile: string;
    FilePercent: Integer;
    BatchPercent: Integer;
    StopWhenDone: Boolean;
    LastOpenDir: string;
    StatusMessages: Boolean;
    VBRHistogram: Boolean;
    TotalSize: Int64;
    CurrentSize: Int64;
    LastSize: Int64;
    CurrentItemSize: Int64;
    BatchStart: TDateTime;
    FileStart: TDateTime;
    LastLine: string;
    InputFileTypes: TStringList;
    EncoderInfoStrings: TStringList;
    EncoderInfoStringsLength: array of Integer;
    DecodeInfoStrings: TStringList;
    DecodeInfoStringsLength: array of Integer;
    PercentPos: Integer;
    HistoMessages: Boolean;
    BRHistogram: TStringList;
    LastStatusUpdate: TDateTime;
    ThreadPriority: TThreadPriority;
    ShutdownFlag: TShutdownFlag;
    Overlap: string;
    SelectedFont: string;
    XPMenu: boolean;
    LRColor: TColor;
    MSColor: TColor;
  end;

  TFileItem = class(TObject)
    Filename: string;
    Path: string;
    Size: Int64;
    Date: TDateTime;
  end;

var
  Global: TGlobalVars;
  MP3Settings: TMP3Settings;

  gsaBitrates: array[1..18] of string
  = ('8', '16', '24', '32', '40', '48', '56', '64', '80', '96', '112', '128',
    '144', '160', '192', '224', '256', '320');

  gsaPriorities: array[0..5] of string;

implementation

procedure CreateGlobals;
begin
  Global.Log := TStringList.Create;
  Global.InputFileTypes := TStringList.Create;
  Global.EncoderInfoStrings := TStringList.Create;
  Global.DecodeInfoStrings := TStringList.Create;
  Global.BRHistogram := TStringList.Create;
end;

procedure FreeGlobals;
begin
  Global.Log.Free;
  Global.InputFileTypes.Free;
  Global.EncoderInfoStrings.Free;
  Global.DecodeInfoStrings.Free;
  Global.BRHistogram.Free;
end;

initialization
  CreateGlobals;

finalization
  FreeGlobals;

end.

