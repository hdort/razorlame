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
    Label1: TLabel;
    GroupBoxUI: TGroupBox;
    LabelFont: TLabel;
    ComboBoxFonts: TComboBox;
    CheckBoxXPMenu: TCheckBox;
    LabelRestartRequired: TLabel;
    GroupBoxHistogram: TGroupBox;
    LabelColorLRStereo: TLabel;
    Label2: TLabel;
    ShapeLRColor: TShape;
    ShapeMSColor: TShape;
    ButtonPickLRStereoColor: TButton;
    ButtonPickMSStereoColor: TButton;
    ColorDialog: TColorDialog;
    procedure ButtonOKClick(Sender: TObject);
    procedure SpeedButtonEncoderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonPickLRStereoColorClick(Sender: TObject);
    procedure ButtonPickMSStereoColorClick(Sender: TObject);
  private
    { Private declarations }
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

procedure TFormOptions.SetOptions;
begin
  //-- Set chosen Options in the dialog
  //-- Encoder
  EditEncoder.Text := Global.Encoder;
  //-- Priority
  ComboBoxPriority.ItemIndex := Ord(Global.ThreadPriority);
  //-- Shutdown flag
  ComboBoxShutdownFlag.ItemIndex := Ord(Global.ShutdownFlag);
  //-- Font
  ComboBoxFonts.ItemIndex := ComboBoxFonts.Items.IndexOf(Global.SelectedFont);
  //-- XP Menu
  CheckBoxXPMenu.Checked := Global.XPMenu;
  //-- Histogram colors
  ShapeLRColor.Brush.Color := Global.LRColor;
  ShapeMSColor.Brush.Color := Global.MSColor;
end;

procedure TFormOptions.GetOptions;
begin
  //-- Get chosen Options FROM the dialog
  Global.Encoder := EditEncoder.Text;
  //-- Priority
  Global.ThreadPriority := TThreadPriority(ComboBoxPriority.ItemIndex);
  //-- Shutdown flag
  Global.ShutdownFlag := TShutdownFlag(ComboBoxShutdownFlag.ItemIndex);
  //-- Font
  Global.SelectedFont := ComboBoxFonts.Items[ComboBoxFonts.ItemIndex];
  //-- XP Menu
  Global.XPMenu := CheckBoxXPMenu.Checked;
  //-- Histogram colors
  Global.LRColor := ShapeLRColor.Brush.Color;
  Global.MSColor := ShapeMSColor.Brush.Color;
end;

procedure TFormOptions.ButtonOKClick(Sender: TObject);
begin
  //-- Ask user if he's sure when Encoder does not read "lame.exe"
  if (EditEncoder.Text <> Global.Encoder)
    and (LowerCase(ExtractFilename(EditEncoder.Text)) <> 'lame.exe') then
    if MyMessageBox(MSG_DIFFERENTENCODER, MSG_QUESTION, MB_YESNOCANCEL
      or MB_ICONQUESTION) <> ID_YES then ModalResult := mrNone;
end;

procedure TFormOptions.SpeedButtonEncoderClick(Sender: TObject);
begin
  if Trim(EditEncoder.Text) <> '' then
    OpenDialogEncoder.FileName := EditEncoder.Text;
  if OpenDialogEncoder.Execute then
    EditEncoder.Text := OpenDialogEncoder.Filename;
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  FixDPI(self, 96);
  PageControlOptions.ActivePageIndex := 0;
  ComboBoxFonts.Items.Assign(Screen.Fonts);
end;

procedure TFormOptions.ButtonPickLRStereoColorClick(Sender: TObject);
begin
  ColorDialog.Color := ShapeLRColor.Brush.Color;
  if ColorDialog.Execute then ShapeLRColor.Brush.Color := ColorDialog.Color;
end;

procedure TFormOptions.ButtonPickMSStereoColorClick(Sender: TObject);
begin
  ColorDialog.Color := ShapeMSColor.Brush.Color;
  if ColorDialog.Execute then ShapeMSColor.Brush.Color := ColorDialog.Color;
end;

end.

