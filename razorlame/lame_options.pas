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
    EditCustomOptions: TEdit;
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
    LabelDescription: TLabel;
    EditDescription: TEdit;
    ButtonSaveOptions: TButton;
    ButtonLoadOptions: TButton;
    SaveDialogLameOptions: TSaveDialog;
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
    procedure CheckBoxOnlyCustomOptionsClick(Sender: TObject);
  private
    { Private declarations }
    FOldOnIdle: TIdleEvent;
    procedure MyOnIdle(Sender: TObject; var Done: Boolean);
    procedure SetCustomOnly(Value: boolean);    
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
    CheckBoxCRC.Checked := IncludeCRC;

    //-- Flags
    CheckBoxCopy.Checked := (mfCopy in Flags);
    CheckBoxCopyright.Checked := (mfCopyright in Flags);

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
    LabelABRTarget.Enabled := CheckBoxVBRUseABR.Checked and VBREnabled;
    SpinEditVBRABRTargetBitrate.Enabled := CheckBoxVBRUseABR.Checked and VBREnabled;
    SpinEditVBRABRTargetBitrate.Value := VBRABRTargetBitrate;

    //-- Output Directory
    RadioButtonInputDir.Checked := UseInputDir;
    RadioButtonOutputDir.Checked := not UseInputDir;
    EditOutputDir.Text := OutDir;
    EditOutputDir.Enabled := not RadioButtonInputDir.Checked;
    SpeedButtonOutputDir.Enabled := EditOutputDir.Enabled;

    //-- Expert options
    ComboBoxATH.ItemIndex := Ord(ATHControl);
    CheckBoxDifferentBlockTypes.Checked := DifferentBlockTypes;
    CheckBoxDisableFiltering.Checked := DisableFiltering;
    CheckBoxNoRes.Checked := NoRes;
    CheckBoxNoShort.Checked := NoShort;
    CheckBoxISO.Checked := ISO;
    EditCustomOptions.Text := CustomOptions;
    CheckBoxOnlyCustomOptions.Checked := OnlyCustomOptions;
    ComboBoxqLevel.ItemIndex := qLevel + 1;

    //-- if CheckBoxOnlyCustomOptions is checked, make that tab first!
    if CheckBoxOnlyCustomOptions.checked then PageControlOptions.ActivePageIndex := 3;

    //-- Audio Processing
    ComboBoxResample.Items.IndexOfObject(TObject(Ord(ResampleFreq)));
    ComboBoxResample.ItemIndex := ComboBoxResample.Items.IndexOfObject(TObject(Ord(ResampleFreq)));
    //ComboBoxResample.ItemIndex := Ord(ResampleFreq);

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
    CustomOptions := EditCustomOptions.Text;
    OnlyCustomOptions := CheckBoxOnlyCustomOptions.Checked;
    qLevel := ComboBoxqLevel.ItemIndex - 1;

    //-- Audio Processing
    if ComboBoxResample.ItemIndex >= 0 then
      ResampleFreq := TResampleFreq(Integer(ComboBoxResample.Items.Objects[ComboBoxResample.ItemIndex]));

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
    LabelVbrHelp.Enabled := true;
    LabelVbrMaxBitrate.Enabled := true;
    TrackBarVbrMaxBitrate.Enabled := true;
    CheckBoxVBRDisableTag.Enabled := true;
    CheckBoxVBRStrictMin.Enabled := true;
    CheckBoxVBRUseABR.Enabled := true;
    LabelABRTarget.Enabled := CheckBoxVBRUseABR.Checked;
    SpinEditVBRABRTargetBitrate.Enabled := CheckBoxVBRUseABR.Checked;
    GroupBoxVBRQuality.Enabled := not CheckBoxVBRUseABR.Checked;
    SpinEditVBRQuality.Enabled := not CheckBoxVBRUseABR.Checked;
    GroupBoxVBRMaxBitrate.Font.Color := clWindowText;
    if not CheckBoxVBRUseABR.Checked then
      GroupBoxVBRQuality.Font.Color := clWindowText
    else
      GroupBoxVBRQuality.Font.Color := clGrayText;
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
  //-- Set selected font
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  FixDPI(self, 96);
  PageControlOptions.ActivePageIndex := 0;
  TrackBarBitrate.Max := High(gsaBitrates);
  TrackBarVbrMaxBitrate.Max := High(gsaBitrates);
  FOldOnIdle := Application.OnIdle;
  Application.OnIdle := MyOnIdle;
  FillResampleStrings(ComboBoxResample.Items);
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
  with TSaveDialog.Create(Self) do
  begin
    try
      DefaultExt := 'rlo';
      Filter := 'RazorLame Lame Options|*.rlo|All files|*.*';
      Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
      InitialDir := ExtractFilePath(Application.ExeName); //-- always start in EXE-Dir
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
    finally
      Free;
    end;
  end;
end;

procedure TFormLameOptions.ButtonLoadOptionsClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  begin
    try
      DefaultExt := 'rlo';
      Filter := 'RazorLame Lame Options|*.rlo|All files|*.*';
      //Options := [ofFileMustExist, ofEnableSizing];
      Options := [ofFileMustExist, ofHideReadOnly, ofEnableSizing];
      InitialDir := ExtractFilePath(Application.ExeName); //-- always start in EXE-Dir
      if Execute then
      begin
        //-- read those Options
        ReadLameOptions(FileName);
        //-- and set them in the dialog
        SetOptions;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormLameOptions.CheckBoxOnlyCustomOptionsClick(Sender: TObject);
begin
  SetCustomOnly(not CheckBoxOnlyCustomOptions.Checked);
end;

procedure TFormLameOptions.SetCustomOnly(Value: boolean);
var
  MyColor: TColor;
begin
  CheckBoxCRC.Enabled := Value;
  CheckBoxCopy.Enabled := Value;
  CheckBoxCopyright.Enabled := Value;
  CheckBoxDifferentBlockTypes.Enabled := Value;
  CheckBoxDisableFiltering.Enabled := Value;
  CheckBoxHighpassFreq.Enabled := Value;
  CheckBoxHighpassWidth.Enabled := Value;
  CheckBoxISO.Enabled := Value;
  CheckBoxLowpassFreq.Enabled := Value;
  CheckBoxLowpassWidth.Enabled := Value;
  CheckBoxNoRes.Enabled := Value;
  CheckBoxNoShort.Enabled := Value;
  CheckBoxVBR.Enabled := Value;
  CheckBoxVBRDisableTag.Enabled := Value;
  CheckBoxVBRStrictMin.Enabled := Value;
  CheckBoxVBRUseABR.Enabled := Value;
  ComboBoxATH.Enabled := Value;
  ComboBoxMode.Enabled := Value;
  ComboBoxOptimization.Enabled := Value;
  ComboBoxResample.Enabled := Value;
  ComboBoxqLevel.Enabled := Value;
  EditHighpassFreq.Enabled := Value;
  EditHighpassWidth.Enabled := Value;
  EditLowpassFreq.Enabled := Value;
  EditLowpassWidth.Enabled := Value;
  GroupBoxBitrate.Enabled := Value;
  GroupBoxFlags.Enabled := Value;
  GroupBoxHighpass.Enabled := Value;
  GroupBoxLowpass.Enabled := Value;
  GroupBoxMode.Enabled := Value;
  GroupBoxOptimization.Enabled := Value;
  GroupBoxOptions.Enabled := Value;
  GroupBoxResample.Enabled := Value;
  GroupBoxVBRMaxBitrate.Enabled := Value;
  GroupBoxVBRQuality.Enabled := Value;
  LabelABRTarget.Enabled := Value;
  LabelATH.Enabled := Value;
  LabelBitrate.Enabled := Value;
  LabelFileBig.Enabled := Value;
  LabelFileSmall.Enabled := Value;
  LabelQualityHigh.Enabled := Value;
  LabelQualityLow.Enabled := Value;
  LabelVbrHelp.Enabled := Value;
  LabelVbrMaxBitrate.Enabled := Value;
  LabelqLevel.Enabled := Value;
  SpinEditVBRABRTargetBitrate.Enabled := Value;
  SpinEditVBRQuality.Enabled := Value;
  TrackBarBitrate.Enabled := Value;
  TrackBarVbrMaxBitrate.Enabled := Value;

  //-- adjust colors of GroupBoxes
  if Value then
    MyColor := clWindowText
  else
    MyColor := clGrayText;

  GroupBoxBitrate.Font.Color := MyColor;
  GroupBoxFlags.Font.Color := MyColor;
  GroupBoxHighpass.Font.Color := MyColor;
  GroupBoxLowpass.Font.Color := MyColor;
  GroupBoxMode.Font.Color := MyColor;
  GroupBoxOptimization.Font.Color := MyColor;
  GroupBoxOptions.Font.Color := MyColor;
  GroupBoxResample.Font.Color := MyColor;
  GroupBoxVBRMaxBitrate.Font.Color := MyColor;
  GroupBoxVBRQuality.Font.Color := MyColor;

  //-- Set BOLD TEXT if OnlyUseCustomOptions is checked
  if not Value then
    CheckBoxOnlyCustomOptions.Font.Style := CheckBoxOnlyCustomOptions.Font.Style + [fsBold]
  else
    CheckBoxOnlyCustomOptions.Font.Style := CheckBoxOnlyCustomOptions.Font.Style - [fsBold];
end;

end.

