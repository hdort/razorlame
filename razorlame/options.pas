(* (c) Copyright 2000, 2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit options;

{$I ConditionalDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons;

type

  TFindFileCallback = function(const asCurrFName: string): Boolean of object;

  TFormOptions = class(TForm)
    Panel1: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControlOptions: TPageControl;
    TabSheetGeneral: TTabSheet;
    GroupBoxLame: TGroupBox;
    EditEncoder: TEdit;
    OpenDialogEncoder: TOpenDialog;
    SpeedButtonEncoder: TSpeedButton;
    GroupBoxPriority: TGroupBox;
    ComboBoxPriority: TComboBox;
    ComboBoxShutdownFlag: TComboBox;
    LabelShutdownWindows: TLabel;
    CheckBoxDelete: TCheckBox;
    TabSheetPreview: TTabSheet;
    GroupBoxExcerptLength: TGroupBox;
    EditExcerptLength: TEdit;
    LabelEncoderLame: TLabel;
    OpenDialogEncoderFaac: TOpenDialog;
    GroupBoxExcerptPosition: TGroupBox;
    RadioButtonStart: TRadioButton;
    RadioButtonMiddle: TRadioButton;
    RadioButtonEnd: TRadioButton;
    SpeedButton1: TSpeedButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    function KeepFindingLame(const asCurrFile: string): Boolean;
  public
    { Public declarations }
    //-- SetOptions sets up the dialog according to Globals
    procedure SetOptions;
    //-- GetOptions alters Globals with the new options as now set in the dialog
    procedure GetOptions;
  end;

implementation

uses Globals, ResStr, UtilFuncs;

{$R *.DFM}

function FindFileOnDrives(const FindFileName: string;
  var FullName: string;
  KeepFindingFile: TFindFileCallback): Boolean;
var
  DriveList: DWORD;
  DriveLetter: Char;

  function FindFileRecurse(const StartPath: string;
    var Name: string): Boolean;
  var
    SrchRec: TSearchRec;

  begin
    Result := False;

    if FindFirst(IncludeTrailingBackslash(StartPath) + '*.*',
      faDirectory, SrchRec) = 0 then
    begin
      { If the callback is defined, send the name of the file
        just compared to the callback.  If False is returned, then
        don't continue trying to find the file. }
      try
        repeat
          { If the user does not want to keep finding the file, then
            exit out. }
          if Assigned(KeepFindingFile) and
            not KeepFindingFile(SrchRec.Name) then Exit;

          { If this is a directory, then search it by recursively calling
            this routine. }
          if SrchRec.Attr and faDirectory > 0 then
          begin
            { Skip . and .., look for the file }
            Result := (SrchRec.Name <> '.') and (SrchRec.Name <> '..') and
              FindFileRecurse(IncludeTrailingBackslash(StartPath) + SrchRec.Name, Name);

            { Result is True if we found a match.  }
            if Result then Exit;
          end
          else
            { See if we've found the file }
            Result := CompareText(ExtractFileName(SrchRec.Name), FindFileName) = 0;

          { Keep looking?  FindNext() returns non-zero when there is nothing else to process. }
          if not Result and (FindNext(SrchRec) <> 0) then Exit;
        until Result;

        { Set Name to the filename found if we found the file }
        if Result then
          Name := IncludeTrailingBackslash(StartPath) + SrchRec.Name;
      finally
        FindClose(SrchRec);
      end;
    end;
  end;

begin
  Result := False;

  { Find LAME.EXE somewhere on any of the harddrives.  Don't try floppy drives.
    This calls itself recursively -- should not be problem, as there can
    only be so many paths deep (not enough to blow any stack, etc.).

    This also stops on the first file found.  This could be modified to create
    a list of names and then let the user choose which one to use...??? }
  DriveList := GetLogicalDrives;

  { DriveList is a bitmap, where the the least significant bit is drive A,
    bit 1 is drive B, etc.  Skip drives A and B. }
  DriveList := DriveList shr 2;
  DriveLetter := 'C';
  while DriveList <> 0 do
  begin
    { See if the next drive is available }
    if DriveList and 1 > 0 then
    begin
      { Traverse this next drive letter to find the file.
        If found, then we're done.  }
      Result := FindFileRecurse(DriveLetter + ':\', FullName);

      { Found a match }
      if Result then Exit;
    end;

    { On to the next drive }
    DriveList := DriveList shr 1;
    Inc(DriveLetter);
  end;
end;

{ TFormOptions }

procedure TFormOptions.SetOptions;
begin
  //-- Set chosen Options in the dialog
  CheckBoxDelete.Checked := Global.AutoDelete;
  //-- Encoder
  EditEncoder.Text := Global.LameEncoder;
  // EditEncoderFaac.Text := Global.FaacEncoder;
  //-- Priority
  ComboBoxPriority.ItemIndex := Ord(Global.ThreadPriority);
  //-- Shutdown flag
  ComboBoxShutdownFlag.ItemIndex := Ord(Global.ShutdownFlag);
  //-- Excerpt stuff
  case Global.ExcerptPosition of
    0: RadioButtonStart.Checked := true;
    1: RadioButtonMiddle.Checked := true;
    2: RadioButtonEnd.Checked := true;
  end;
  EditExcerptLength.Text := IntToStr(Global.ExcerptLength);
end;

procedure TFormOptions.GetOptions;
begin
  //-- Get chosen Options FROM the dialog
  Global.AutoDelete := CheckBoxDelete.Checked;

  //-- Encoder stuff
  if Global.LameEncoder <> EditEncoder.Text then
  begin
    if Global.LameEncoder = Global.DefaultEncoder then
      Global.DefaultEncoder := EditEncoder.Text;
    Global.LameEncoder := EditEncoder.Text;
  end;
  //  Global.FaacEncoder := EditEncoderFaac.Text;

  //-- Priority
  Global.ThreadPriority := TThreadPriority(ComboBoxPriority.ItemIndex);
  //-- Shutdown flag
  Global.ShutdownFlag := TShutdownFlag(ComboBoxShutdownFlag.ItemIndex);

  //-- Excerpt stuff
  if RadioButtonStart.Checked then
    Global.ExcerptPosition := 0
  else
    if RadioButtonMiddle.Checked then
    Global.ExcerptPosition := 1
  else
    Global.ExcerptPosition := 2;

  Global.ExcerptLength := StrToIntDef(EditExcerptLength.Text, 10);

  if Global.ExcerptLength > 60 then Global.ExcerptLength := 60;
  if Global.ExcerptLength < 1 then Global.ExcerptLength := 1;
end;

procedure TFormOptions.ButtonOKClick(Sender: TObject);
begin
  //-- Ask user if he's sure when Encoder does not read "lame.exe"
  if (EditEncoder.Text <> Global.LameEncoder)
    and (LowerCase(ExtractFilename(EditEncoder.Text)) <> 'lame.exe') then
    if MyMessageBox(MSG_DIFFERENTENCODER, MSG_QUESTION, MB_YESNOCANCEL
      or MB_ICONQUESTION) <> ID_YES then ModalResult := mrNone;
end;

procedure TFormOptions.SpeedButtonClick(Sender: TObject);
begin
  if Trim(EditEncoder.Text) <> '' then
    OpenDialogEncoder.FileName := EditEncoder.Text;
  if OpenDialogEncoder.Execute then
    EditEncoder.Text := OpenDialogEncoder.Filename;
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  PageControlOptions.ActivePageIndex := 0;
  SetWindowLong(EditExcerptLength.handle, GWL_STYLE,
    GetWindowLong(EditExcerptLength.handle, GWL_STYLE) or ES_NUMBER);
end;

function TFormOptions.KeepFindingLame(const asCurrFile: string): Boolean;
begin
  {  Callback method to the FindFileOnDrives() function.
     Return False to stop finding the file (user presses ESC, or clicks a Cancel button, etc.)
     or True to keep looking.   The value in CurrFile contains the current file name
     that is being compared -- could be used to show progress in a label, etc. }
  Result := True;
end;

procedure TFormOptions.SpeedButton1Click(Sender: TObject);
var
  lsLameFName: string;
begin
  { Find LAME.EXE by scanning all drives and subfolders.
    This was implemented by the user double-clicking on the edit box, but
    maybe should be an event handler to a new button...  Also, maybe check
    for LAME.EXE to be in the current directory first...???  }
  PushCursor;
  try
    if FindFileOnDrives('LAME.EXE', lsLameFName, KeepFindingLame) then
      EditEncoder.Text := lsLameFName;
  finally
    PopCursor;
  end;
end;

end.

