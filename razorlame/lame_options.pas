(* (c) Copyright 2000, 2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit lame_options;

{$I ConditionalDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons, BrowseDr, ActnList, Dialogs;

type
  TFormLameOptions = class(TForm)
    Panel1: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControlOptions: TPageControl;
    TabSheetGeneral: TTabSheet;
    TabSheetAdvanced: TTabSheet;
    TabSheetExpert: TTabSheet;
    TabSheetAudioProcessing: TTabSheet;
    GroupBoxBitrate: TGroupBox;
    LabelQualityLow: TLabel;
    LabelQualityHigh: TLabel;
    LabelBitrate: TLabel;
    LabelFileSmall: TLabel;
    LabelFileBig: TLabel;
    TrackBarBitrate: TTrackBar;
    GroupBoxOutputDir: TGroupBox;
    EditOutputDir: TEdit;
    GroupBoxOptions: TGroupBox;
    CheckBoxCRC: TCheckBox;
    GroupBoxFlags: TGroupBox;
    CheckBoxCopy: TCheckBox;
    CheckBoxCopyright: TCheckBox;
    TabSheetVBR: TTabSheet;
    CheckBoxVBR: TCheckBox;
    GroupBoxMode: TGroupBox;
    ComboBoxMode: TComboBox;
    GroupBoxOptimization: TGroupBox;
    ComboBoxOptimization: TComboBox;
    GroupBoxVBRMaxBitrate: TGroupBox;
    TrackBarVbrMaxBitrate: TTrackBar;
    LabelVbrMaxBitrate: TLabel;
    GroupBoxVBRQuality: TGroupBox;
    SpinEditVBRQuality: TSpinEdit;
    LabelVbrHelp: TLabel;
    GroupBoxResample: TGroupBox;
    ComboBoxResample: TComboBox;
    CheckBoxVBRDisableTag: TCheckBox;
    GroupBoxHighpass: TGroupBox;
    CheckBoxHighpassFreq: TCheckBox;
    CheckBoxHighpassWidth: TCheckBox;
    GroupBoxLowpass: TGroupBox;
    CheckBoxLowpassFreq: TCheckBox;
    CheckBoxLowpassWidth: TCheckBox;
    SpeedButtonOutputDir: TSpeedButton;
    dfsBrowseDirectoryDlgOutputDir: TdfsBrowseDirectoryDlg;
    EditHighpassFreq: TEdit;
    EditHighpassWidth: TEdit;
    EditLowpassFreq: TEdit;
    EditLowpassWidth: TEdit;
    ComboBoxATH: TComboBox;
    LabelATH: TLabel;
    CheckBoxDifferentBlockTypes: TCheckBox;
    CheckBoxDisableFiltering: TCheckBox;
    CheckBoxNoRes: TCheckBox;
    CheckBoxNoShort: TCheckBox;
    CheckBoxISO: TCheckBox;
    CheckBoxVBRStrictMin: TCheckBox;
    LabelCustomOptions: TLabel;
    CheckBoxOnlyCustomOptions: TCheckBox;
    GroupBoxFiling: TGroupBox;
    CheckBoxDeleteFileAfterProcessing: TCheckBox;
    CheckBoxVBRUseABR: TCheckBox;
    LabelABRTarget: TLabel;
    SpinEditVBRABRTargetBitrate: TSpinEdit;
    RadioButtonInputDir: TRadioButton;
    RadioButtonOutputDir: TRadioButton;
    PanelLameOptions: TPanel;
    EditLameOptions: TEdit;
    LabelLameOptions: TLabel;
    ComboBoxqLevel: TComboBox;
    LabelqLevel: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    EditDescription: TEdit;
    ButtonSaveOptions: TButton;
    ButtonLoadOptions: TButton;
    OpenDialogLameOptions: TOpenDialog;
    SaveDialogLameOptions: TSaveDialog;
    ComboBoxCustomOptions: TComboBox;
    procedure TrackBarBitrateChange(Sender: TObject);
    procedure TrackBarVbrMaxBitrateChange(Sender: TObject);
    procedure CheckBoxVBRClick(Sender: TObject);
    procedure CheckBoxHighpassFreqClick(Sender: TObject);
    procedure CheckBoxLowpassFreqClick(Sender: TObject);
    procedure CheckBoxHighpassWidthClick(Sender: TObject);
    procedure CheckBoxLowpassWidthClick(Sender: TObject);
    procedure ButtonShowOptionsClick(Sender: TObject);
    procedure SpeedButtonOutputDirClick(Sender: TObject);
    procedure FloatExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxVBRUseABRClick(Sender: TObject);
    procedure RadioButtonDirClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSaveOptionsClick(Sender: TObject);
    procedure ButtonLoadOptionsClick(Sender: TObject);
  private
    { Private declarations }
    FOldOnIdle: TIdleEvent;
    procedure MyOnIdle(Sender: TObject; var Done: Boolean);
    procedure UpdateMRU;
    procedure FillComboFromMRU;
  public
    { Public declarations }
    //-- SetOptions sets up the dialog according to Globals
    procedure SetOptions;
    //-- GetOptions alters Globals with the new options as now set in the dialog
    procedure GetOptions;
    //-- Shows the current command line options
    procedure ShowLameOptions;
  end;

implementation

uses Globals, ResStr, UtilFuncs;

{$R *.DFM}

procedure TFormLameOptions.FillComboFromMRU;
begin
  ComboBoxCustomOptions.Items.AddStrings(MP3Settings.CustomMRU)
end;

procedure TFormLameOptions.UpdateMRU;
var
  i, liCount: Integer;
begin
  liCount := 0;
  with MP3Settings do
  begin
    CustomMRU.Clear;

    if ComboBoxCustomOptions.Items.IndexOf(ComboBoxCustomOptions.Text) = -1 then
    begin
      CustomMRU.Add(ComboBoxCustomOptions.Text);
      liCount := 1;
    end;

    for i := 0 to ComboBoxCustomOptions.Items.Count - 1 do
    begin
      CustomMRU.Add(ComboBoxCustomOptions.Items[i]);
      Inc(liCount);
      if liCount > 4 then exit;
    end;
  end;
end;

procedure TFormLameOptions.SetOptions;
var
  i: Integer;
begin
  //-- Set chosen Options in the dialog
  with MP3Settings do
  begin
    //-- Description
    EditDescription.Text := Description;

    //-- Options
    if IncludeCRC then CheckBoxCRC.Checked := true;

    //-- Flags
    if mfCopy in Flags then CheckBoxCopy.Checked := true;
    if mfCopyright in Flags then CheckBoxCopyright.Checked := true;

    //-- Filing
    CheckBoxDeleteFileAfterProcessing.Checked := DeleteFileAfterProcessing;

    //-- Bitrate
    with TrackBarBitrate do
    begin
      //-- Set a default
      Position := 8;
      //-- set the chosen bitrate
      for i := Low(gsaBitrates) to High(gsaBitrates) do
        if gsaBitrates[i] = IntToStr(Bitrate) then Position := i;
    end;

    //-- Mode
    ComboBoxMode.ItemIndex := Ord(LameMode);

    //-- Optimization
    ComboBoxOptimization.ItemIndex := Ord(LameOptimization);

    //-- VBR
    CheckBoxVBR.Checked := VBREnabled;
    SpinEditVBRQuality.Value := VBRQuality;
    with TrackBarVbrMaxBitrate do
    begin
      //-- Set a default
      Position := High(gsaBitrates);
      //-- set the chosen bitrate
      for i := Low(gsaBitrates) to High(gsaBitrates) do
        if gsaBitrates[i] = IntToStr(VBRMaxBitrate) then Position := i;
    end;
    CheckBoxVBRDisableTag.Checked := VBRDisableTag;
    CheckBoxVBRStrictMin.Checked := VBRStrictMin;
    CheckBoxVBRUseABR.Checked := VBRUseABR;
    SpinEditVBRABRTargetBitrate.Enabled := CheckBoxVBRUseABR.Checked;
    SpinEditVBRABRTargetBitrate.Value := VBRABRTargetBitrate;

    //-- Output Directory
    RadioButtonInputDir.Checked := UseInputDir;
    RadioButtonOutputDir.Checked := not UseInputDir;
    EditOutputDir.Text := OutDir;
    if OutDir = '' then RadioButtonInputDir.Checked := true;
    EditOutputDir.Enabled := not RadioButtonInputDir.Checked;
    SpeedButtonOutputDir.Enabled := EditOutputDir.Enabled;

    //-- Expert options
    ComboBoxATH.ItemIndex := Ord(ATHControl);
    CheckBoxDifferentBlockTypes.Checked := DifferentBlockTypes;
    CheckBoxDisableFiltering.Checked := DisableFiltering;
    CheckBoxNoRes.Checked := NoRes;
    CheckBoxNoShort.Checked := NoShort;
    CheckBoxISO.Checked := ISO;
    ComboBoxCustomOptions.Text := CustomOptions;
    CheckBoxOnlyCustomOptions.Checked := OnlyCustomOptions;
    ComboBoxqLevel.ItemIndex := qLevel + 1;
    FillComboFromMRU;

    //-- Audio Processing
    ComboBoxResample.ItemIndex := Ord(ResampleFreq);

    CheckBoxHighpassFreq.Checked := HighpassEnabled;
    EditHighpassFreq.Text := FloatToStr(HighpassFreq); //FloatToStrF(HighpassFreq, ffFixed, 4,2);
    CheckBoxHighpassWidth.Checked := HighWidthEnabled;
    EditHighpassWidth.Text := FloatToStr(HighpassWidth);

    CheckBoxLowpassFreq.Checked := LowpassEnabled;
    EditLowpassFreq.Text := FloatToStr(LowpassFreq);
    CheckBoxLowpassWidth.Checked := LowWidthEnabled;
    EditLowpassWidth.Text := FloatToStr(LowpassWidth);
  end;
end;

procedure TFormLameOptions.GetOptions;
begin
  //-- Get chosen Options FROM the dialog
  with MP3Settings do
  begin
    //-- Description
    Description := EditDescription.Text;

    //-- Flags
    if CheckBoxCRC.Checked then
      IncludeCRC := true
    else
      IncludeCRC := false;

    //-- Flags
    Flags := [];
    if CheckBoxCopy.Checked then Include(Flags, mfCopy);
    if CheckBoxCopyright.Checked then Include(Flags, mfCopyright);

    //-- Output Directory
    if Trim(EditOutputDir.Text) <> '' then
      OutDir := IncludeTrailingBackslash(Trim(EditOutputDir.Text))
    else
      OutDir := '';
    UseInputDir := RadioButtonInputDir.Checked;

    //-- Filing
    DeleteFileAfterProcessing := CheckBoxDeleteFileAfterProcessing.Checked;

    //-- Bitrate
    Bitrate := StrToInt(gsaBitrates[TrackBarBitrate.Position]);

    //-- Mode
    LameMode := TLameMode(ComboBoxMode.ItemIndex);

    //-- Optimization
    LameOptimization := TLameOptimization(ComboBoxOptimization.ItemIndex);

    //-- VBR
    VBREnabled := CheckBoxVBR.Checked;
    VBRQuality := SpinEditVBRQuality.Value;
    VBRMaxBitrate := StrToInt(gsaBitrates[TrackBarVbrMaxBitrate.Position]);
    VBRDisableTag := CheckBoxVBRDisableTag.Checked;
    VBRStrictMin := CheckBoxVBRStrictMin.Checked;
    VBRUseABR := CheckBoxVBRUseABR.Checked;
    VBRABRTargetBitrate := SpinEditVBRABRTargetBitrate.Value;

    //-- Expert options
    ATHControl := TATHControl(ComboBoxATH.ItemIndex);
    DifferentBlockTypes := CheckBoxDifferentBlockTypes.Checked;
    DisableFiltering := CheckBoxDisableFiltering.Checked;
    NoRes := CheckBoxNoRes.Checked;
    NoShort := CheckBoxNoShort.Checked;
    ISO := CheckBoxISO.Checked;
    CustomOptions := ComboBoxCustomOptions.Text;
    OnlyCustomOptions := CheckBoxOnlyCustomOptions.Checked;
    qLevel := ComboBoxqLevel.ItemIndex - 1;
    UpdateMRU;

    //-- Audio Processing
    ResampleFreq := TResampleFreq(ComboBoxResample.ItemIndex);

    HighpassEnabled := CheckBoxHighpassFreq.Checked;
    HighpassFreq := StrToFloatDef(EditHighpassFreq.Text, 0);
    HighWidthEnabled := CheckBoxHighpassWidth.Checked;
    HighpassWidth := StrToFloatDef(EditHighpassWidth.Text, 0);

    LowpassEnabled := CheckBoxLowpassFreq.Checked;
    LowpassFreq := StrToFloatDef(EditLowpassFreq.Text, 0);
    LowWidthEnabled := CheckBoxLowpassWidth.Checked;
    LowpassWidth := StrToFloatDef(EditLowpassWidth.Text, 0);
  end;
end;

procedure TFormLameOptions.TrackBarBitrateChange(Sender: TObject);
begin
  LabelBitrate.Caption := Format(LABEL_CURRENT_BITRATE,
    [gsaBitrates[TrackBarBitrate.Position]]);
end;

procedure TFormLameOptions.TrackBarVbrMaxBitrateChange(Sender: TObject);
begin
  LabelVbrMaxBitrate.Caption := Format(LABEL_VBR_MAX_BITRATE,
    [gsaBitrates[TrackBarVbrMaxBitrate.Position]]);
end;

procedure TFormLameOptions.CheckBoxVBRClick(Sender: TObject);
begin
  if CheckBoxVBR.Checked then
  begin
    GroupBoxVBRMaxBitrate.Enabled := true;
    GroupBoxVBRMaxBitrate.Font.Color := clWindowText;
    GroupBoxVBRQuality.Enabled := true;
    GroupBoxVBRQuality.Font.Color := clWindowText;
    LabelVbrHelp.Enabled := true;
    SpinEditVBRQuality.Enabled := true;
    LabelVbrMaxBitrate.Enabled := true;
    TrackBarVbrMaxBitrate.Enabled := true;
    CheckBoxVBRDisableTag.Enabled := true;
    CheckBoxVBRStrictMin.Enabled := true;
    CheckBoxVBRUseABR.Enabled := true;
    LabelABRTarget.Enabled := CheckBoxVBRUseABR.Checked;
    SpinEditVBRABRTargetBitrate.Enabled := CheckBoxVBRUseABR.Checked;
  end
  else
  begin
    GroupBoxVBRMaxBitrate.Enabled := false;
    GroupBoxVBRMaxBitrate.Font.Color := clGrayText;
    GroupBoxVBRQuality.Enabled := false;
    GroupBoxVBRQuality.Font.Color := clGrayText;
    LabelVbrHelp.Enabled := false;
    SpinEditVBRQuality.Enabled := false;
    LabelVbrMaxBitrate.Enabled := false;
    TrackBarVbrMaxBitrate.Enabled := false;
    CheckBoxVBRDisableTag.Enabled := false;
    CheckBoxVBRStrictMin.Enabled := false;
    CheckBoxVBRUseABR.Enabled := false;
    LabelABRTarget.Enabled := false;
    SpinEditVBRABRTargetBitrate.Enabled := false;
  end;
end;

procedure TFormLameOptions.CheckBoxHighpassFreqClick(Sender: TObject);
begin
  EditHighpassFreq.Enabled := CheckBoxHighpassFreq.Checked;
  CheckBoxHighpassWidth.Enabled := CheckBoxHighpassFreq.Checked;
  EditHighpassWidth.Enabled := CheckBoxHighpassWidth.Enabled and CheckBoxHighpassWidth.Checked;
end;

procedure TFormLameOptions.CheckBoxLowpassFreqClick(Sender: TObject);
begin
  EditLowpassFreq.Enabled := CheckBoxLowpassFreq.Checked;
  CheckBoxLowpassWidth.Enabled := CheckBoxLowpassFreq.Checked;
  EditLowpassWidth.Enabled := CheckBoxLowpassWidth.Enabled and CheckBoxLowpassWidth.Checked;
end;

procedure TFormLameOptions.CheckBoxHighpassWidthClick(Sender: TObject);
begin
  EditHighpassWidth.Enabled := CheckBoxHighpassWidth.Checked;
end;

procedure TFormLameOptions.CheckBoxLowpassWidthClick(Sender: TObject);
begin
  EditLowpassWidth.Enabled := CheckBoxLowpassWidth.Checked;
end;

procedure TFormLameOptions.ButtonShowOptionsClick(Sender: TObject);
begin
  ShowLameOptions;
end;

procedure TFormLameOptions.SpeedButtonOutputDirClick(Sender: TObject);
begin
  if Trim(EditOutputDir.Text) <> '' then
    dfsBrowseDirectoryDlgOutputDir.Selection := EditOutputDir.Text;
  if dfsBrowseDirectoryDlgOutputDir.Execute then
    EditOutputDir.Text := IncludeTrailingBackslash(dfsBrowseDirectoryDlgOutputDir.Selection);
end;

procedure TFormLameOptions.FloatExit(Sender: TObject);
begin
  if StrToFloatDef(TEdit(Sender).Text, -1) < 0 then
  begin
    MyMessageBox(Format('Sorry, but "%s" is not a valid value here!'#13#10#13#10 +
      'Try something like "%s" instead!', [TEdit(Sender).Text, FloatToStr(22.05)]),
      'Invalid value', MB_ICONINFORMATION or MB_OK);
    TEdit(Sender).SetFocus;
  end;
end;

procedure TFormLameOptions.FormCreate(Sender: TObject);
begin
  PageControlOptions.ActivePageIndex := 0;
  TrackBarBitrate.Max := High(gsaBitrates);
  TrackBarVbrMaxBitrate.Max := High(gsaBitrates);
  FOldOnIdle := Application.OnIdle;
  Application.OnIdle := MyOnIdle;
end;

procedure TFormLameOptions.CheckBoxVBRUseABRClick(Sender: TObject);
begin
  LabelABRTarget.Enabled := CheckBoxVBRUseABR.Checked;
  SpinEditVBRABRTargetBitrate.Enabled := CheckBoxVBRUseABR.Checked;
  GroupBoxVBRQuality.Enabled := not CheckBoxVBRUseABR.Checked;
  SpinEditVBRQuality.Enabled := GroupBoxVBRQuality.Enabled;
  if GroupBoxVBRQuality.Enabled then
    GroupBoxVBRQuality.Font.Color := clWindowText
  else
    GroupBoxVBRQuality.Font.Color := clGrayText;
end;

procedure TFormLameOptions.RadioButtonDirClick(Sender: TObject);
begin
  EditOutputDir.Enabled := not RadioButtonInputDir.Checked;
  SpeedButtonOutputDir.Enabled := EditOutputDir.Enabled;
end;

procedure TFormLameOptions.ShowLameOptions;
var
  TempSettings: TMP3Settings;
begin
  //-- save current state
  TempSettings := MP3Settings;
  //-- determine the currently settings
  GetOptions;
  //-- and display those Options
  EditLameOptions.Text := GetOptionString;
  //-- now set back the options, so user can still cancel dialog!
  MP3Settings := TempSettings;
end;

procedure TFormLameOptions.MyOnIdle(Sender: TObject; var Done: Boolean);
begin
  ShowLameOptions;
  if Assigned(FOldOnIdle) then
    FOldOnIdle(Sender, Done);
end;

procedure TFormLameOptions.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := FOldOnIdle;
end;

procedure TFormLameOptions.ButtonSaveOptionsClick(Sender: TObject);
var
  TempSettings: TMP3Settings;
begin
  with SaveDialogLameOptions do
    if Execute then
    begin
      //-- save current state
      TempSettings := MP3Settings;
      //-- determine the currently settings
      GetOptions;
      //-- and write those Options
      WriteLameOptions(FileName);
      //-- now set back the options, so user can still cancel dialog!
      MP3Settings := TempSettings;
    end;
end;

procedure TFormLameOptions.ButtonLoadOptionsClick(Sender: TObject);
begin
  with OpenDialogLameOptions do
    if Execute then
    begin
      //-- read those Options
      ReadLameOptions(FileName);
      //-- and set them in the dialog
      SetOptions;
    end;
end;

end.

