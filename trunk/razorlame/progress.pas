(* (c) Copyright 2000-2005  -  Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit progress;

{$I ConditionalDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TFormProgress = class(TForm)
    PanelButtons: TPanel;
    ButtonCancel: TButton;
    ButtonTray: TButton;
    PanelTop: TPanel;
    PanelStatus: TPanel;
    LabelStatus: TLabel;
    LabelStatusRemaining: TLabel;
    LabelBatchRemaining: TLabel;
    LabelBatch: TLabel;
    LabelWorking: TLabel;
    LabelOutput: TLabel;
    LabelStopWhenDone: TLabel;
    LabelBatchElapsed: TLabel;
    LabelBatchEstimated: TLabel;
    LabelRemainingLabel: TLabel;
    LabelElapsedLabel: TLabel;
    LabelestimatedLabel: TLabel;
    LabelStatusRemainingLabel: TLabel;
    ProgressBarStatus: TProgressBar;
    ProgressBarBatch: TProgressBar;
    CheckBoxShutdown: TCheckBox;
    ButtonHistogram: TButton;
    PanelHistogram: TPanel;
    BevelHistogram: TBevel;
    ImageHistogram: TImage;
    Splitter: TSplitter;
    CheckBoxDelSource: TCheckBox;
    LabelVersion: TLabel;
    BevelBottomLine: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonTrayClick(Sender: TObject);
    procedure ButtonHistogramClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure CheckBoxDelSourceClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure ResetFormControls;
    procedure WMSysCommand(var Message: TMessage); message WM_SYSCOMMAND;
  public
    { Public declarations }
    ExternalClosed: Boolean;
    procedure DrawHistogram;
  end;

var
  FormProgress: TFormProgress;

implementation

{$R *.DFM}

uses UtilFuncs, Globals, ResStr, Main, About;

procedure TFormProgress.ResetFormControls;
begin
  //todo: re-positoning won't work yet...
  //todo: resizing won't work properly as well. Idea: safe state with/without histogram seperately?
  if not RepositionForm(PROG_FORM_SECTION, Self) then
    Self.Position := poOwnerFormCenter;

  if not IniReadBool(PROG_FORM_SECTION, 'ShowHistogram', false) then
  begin
    PanelHistogram.Hide;
    ImageHistogram.Hide;
    Splitter.Hide;
    ButtonHistogram.Enabled := false;
    Width := Width - BevelHistogram.Width - 8 - Splitter.Width;
  end
  else
  begin
    ButtonHistogram.Caption := 'Hide &histogram';
    PanelHistogram.Width := IniReadInteger(PROG_FORM_SECTION, 'PanelHistogramWidth', PanelHistogram.Width);
    //PanelHistogram.Width := ClientWidth - PanelStatus.Constraints.MinWidth;
  end;

  ProgressBarStatus.Position := 0;
  ProgressBarBatch.Position := 0;
  LabelStatus.Caption := ID_STATUS + '0%';
  LabelBatch.Caption := ID_BATCH + '0%';
  LabelStatusRemaining.Caption := '';
  LabelBatchRemaining.Caption := '';
  LabelBatchElapsed.Caption := '';
  LabelBatchEstimated.Caption := '';
  LabelWorking.Caption := Format(ID_WORKING, [0, 0, '']);
  LabelOutput.Caption := ID_OUTPUT;
  LabelVersion.Caption := '';
  LabelStopWhenDone.Visible := false;
  CheckBoxDelSource.Checked := MP3Settings.DeleteFileAfterProcessing;
end;

procedure TFormProgress.FormCreate(Sender: TObject);
var
  liLeft, liWidth: integer;
begin
  //-- Set selected font
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  //-- do some init stuff for controls on the form
  FixDPI(self, 96);
  //-- adjust the middle labels for fonts other than 96 dpi
  if Screen.PixelsPerInch <> 96 then
  begin
    liLeft := LabelStatusRemainingLabel.Left + LabelStatusRemainingLabel.Width + 4;
    liWidth := LabelStatus.Left - 8 - liLeft;
    LabelStatusRemaining.Left := liLeft;
    LabelBatchRemaining.Left := liLeft;
    LabelBatchElapsed.Left := liLeft;
    LabelBatchEstimated.Left := liLeft;
    LabelStatusRemaining.Width := liWidth;
    LabelBatchRemaining.Width := liWidth;
    LabelBatchElapsed.Width := liWidth;
    LabelBatchEstimated.Width := liWidth;
    //-- constraints
    PanelStatus.Constraints.MinHeight := (PanelStatus.Constraints.MinHeight * Screen.PixelsPerInch + 96 div 2) div 96;
    PanelStatus.Constraints.MinWidth := (PanelStatus.Constraints.MinWidth * Screen.PixelsPerInch + 96 div 2) div 96;
    PanelHistogram.Constraints.MinWidth := (PanelHistogram.Constraints.MinWidth * Screen.PixelsPerInch + 96 div 2) div 96;
  end;

  ProgressBarStatus.Max := 1000;
  ProgressBarBatch.Max := 1000;
  ExternalClosed := false;
end;

procedure TFormProgress.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  liResult: Integer;
begin
  CanClose := true;
  if not ExternalClosed then
  begin
    liResult := MyMsgDlg(MSG_ABORTENCODING, MSG_QUESTION, mtConfirmation,
      mbYesNoCancel, [BTN_NOW, BTN_DONE]);

    CanClose := (liResult = ID_YES);
    Global.StopWhenDone := (liResult = ID_NO);

    LabelStopWhenDone.Visible := Global.StopWhenDone;

    //-- if batch was through by the time the user answered
    //-- the messagebox, close the form anyway,
    //-- no matter what the user answered to the question
    if ExternalClosed then
    begin
      CanClose := true;
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TFormProgress.ButtonTrayClick(Sender: TObject);
begin
  Self.Hide;
  FormMain.Hide;
  ShowWindow(Application.Handle, SW_HIDE);
  FormMain.TrayIcon.Active := true;
end;

procedure TFormProgress.WMSysCommand(var Message: TMessage);
begin
  if Message.wParam = SC_MINIMIZE then
    ShowWindow(Application.Handle, SW_MINIMIZE)
  else
    inherited;
end;


procedure TFormProgress.ButtonHistogramClick(Sender: TObject);
var
  liWidth: integer;
begin
  if ImageHistogram.Visible then
  begin
    //-- hide histogram
    ButtonHistogram.Caption := 'Show &histogram';
    liWidth := PanelStatus.Width;
    ImageHistogram.Hide;
    PanelHistogram.Hide;
    Splitter.Hide;
    ClientWidth := liWidth;
  end
  else
  begin
    //-- show histogram
    ButtonHistogram.Caption := 'Hide &histogram';
    ClientWidth := Width + BevelHistogram.Width + 4 + Splitter.Width;
    ImageHistogram.Show;
    PanelHistogram.Show;
    Splitter.Show;
    Splitter.Align := alRight;
  end;
end;

(*
procedure TFormProgress.DrawHistogram;
var
  liMax, liHeight, liBar, i, liValue, liTextHeight: Integer;
  lfWidth: Double;
  lsValue: string;
  MyBitmap: TBitmap;

  procedure DrawBar;
  var
    x, y, liTextWidth: Integer;
  begin
    x := Round(lfWidth * (liBar * 2));
    y := liHeight - Round(liHeight * (liValue / liMax));
    MyBitmap.Canvas.Brush.Color := clHighlight;
    MyBitmap.Canvas.FillRect(Rect(x, y, x + Round(lfWidth), liHeight));
    MyBitmap.Canvas.Brush.Color := clBtnFace;
    liTextWidth := MyBitmap.Canvas.TextWidth(Trim(lsValue));
    MyBitmap.Canvas.TextOut(x + Round(lfWidth / 2) - (liTextWidth div 2), y - liTextHeight, Trim(lsValue));
  end;

begin
  MyBitmap := TBitmap.Create;
  try
    //-- Draw the bitrate histogram
    MyBitmap.Width := ImageHistogram.Width;
    MyBitmap.Height := ImageHistogram.Height;
    MyBitmap.Canvas.Pen.Color := clBtnText;
    MyBitmap.Canvas.Brush.Color := clBtnFace;
    MyBitmap.Canvas.Brush.Style := bsSolid;
    MyBitmap.Canvas.FillRect(Rect(0, 0, MyBitmap.Width, MyBitmap.Height));

    liMax := 80;
    liTextHeight := MyBitmap.Canvas.TextHeight('Ag');
    liHeight := MyBitmap.Height;
    lfWidth := MyBitmap.Width / ((2 * Global.BRHistogram.Count) - 1);

    for liBar := 0 to Global.BRHistogram.Count - 1 do
    begin
      lsValue := Copy(Global.BRHistogram[liBar], 1, 3);
      liValue := 0;
      for i := 1 to Length(Global.BRHistogram[liBar]) do
        if (Global.BRHistogram[liBar][i] = '*') or (Global.BRHistogram[liBar][i] = '%') then Inc(liValue);
      DrawBar;
    end;

    ImageHistogram.Picture.Bitmap.Width := MyBitmap.Width;
    ImageHistogram.Picture.Bitmap.Height := MyBitmap.Height;
    ImageHistogram.Canvas.Draw(0, 0, MyBitmap);
    //ImageHistogram.Canvas.CopyRect(Rect(0, 0, MyBitmap.Width, MyBitmap.Height), MyBitmap.Canvas, Rect(0, 0, MyBitmap.Width, MyBitmap.Height));
  finally
    MyBitmap.Free;
  end;
end;
*)

procedure TFormProgress.FormResize(Sender: TObject);
begin
  DrawHistogram;
end;

procedure TFormProgress.SplitterMoved(Sender: TObject);
begin
  DrawHistogram;
end;

procedure TFormProgress.CheckBoxDelSourceClick(Sender: TObject);
begin
  MP3Settings.DeleteFileAfterProcessing := CheckBoxDelSource.Checked;
end;

procedure TFormProgress.FormDestroy(Sender: TObject);
begin
  try
    RegisterFormPosition(PROG_FORM_SECTION, Self);
    StoreInIni(PROG_FORM_SECTION, 'ShowHistogram', ImageHistogram.Visible);
    StoreInIni(PROG_FORM_SECTION, 'PanelHistogramWidth', PanelHistogram.Width);
  except
    on E: Exception do
    begin
      Application.ShowException(E); //-- no matter what happens, close this form
      exit;
    end;
  end;
end;


procedure TFormProgress.DrawHistogram;
var
  liMax, liHeight, liBar, i, liSSValue, liMSValue, liTextHeight: Integer;
  lfWidth: Double;
  lsValue: string;
  MyBitmap: TBitmap;
  lbLastBar: Boolean;

  procedure DrawBar;
  var
    x, y, x2, y2, liTextWidth: Integer;
  begin
    //-- claculate upper left point of bar
    x := Round(lfWidth * liBar);
    y := liHeight - Round(liHeight * ((liMSValue + liSSValue) / liMax));
    y2 := y;

    if lbLastBar then
      x2 := MyBitmap.Width
    else
      x2 := Round(lfWidth * (liBar + 1)) + 1;

    //-- draw stereo types if available
    if (liMSValue > 0) and (liSSValue > 0) then
    begin
      MyBitmap.Canvas.Brush.Color := Global.MSColor; //$00184302 //$007BC7EF
      y2 := liHeight - Round(liHeight * ((liMSValue) / liMax));
      MyBitmap.Canvas.FillRect(Rect(x, y, x2, y2));
    end;

    MyBitmap.Canvas.Brush.Color := Global.LRColor; //$00E4F6FE;

    //-- draw the bar
    MyBitmap.Canvas.FillRect(Rect(x, y2, x2, liHeight));
    //-- draw a black border around it
    MyBitmap.Canvas.Brush.Color := clBlack;
    MyBitmap.Canvas.FrameRect(Rect(x, y, x2, liHeight));

    //-- print the bitrate centered at the bottom
    MyBitmap.Canvas.Brush.Color := clBtnFace;
    liTextWidth := MyBitmap.Canvas.TextWidth(Trim(lsValue));
    //MyBitmap.Canvas.TextOut(x + Round(lfWidth / 2) - (liTextWidth div 2), y - liTextHeight, Trim(lsValue));
    MyBitmap.Canvas.TextOut(x + Round(lfWidth / 2) - (liTextWidth div 2), liHeight + 4, Trim(lsValue));
  end;

begin
  if Global.BRHistogram.Count = 0 then exit;
  MyBitmap := TBitmap.Create;
  try
    //-- Draw the bitrate histogram ------
    //-- initialize bitmap
    MyBitmap.Width := ImageHistogram.Width;
    MyBitmap.Height := ImageHistogram.Height;
    MyBitmap.Canvas.Pen.Color := clBtnText;
    MyBitmap.Canvas.Brush.Color := clBtnFace;
    MyBitmap.Canvas.Brush.Style := bsSolid;
    MyBitmap.Canvas.FillRect(Rect(0, 0, MyBitmap.Width, MyBitmap.Height));

    //-- get the starting values
    liMax := 67;
    liTextHeight := MyBitmap.Canvas.TextHeight('Ag');
    liHeight := MyBitmap.Height;
    lfWidth := MyBitmap.Width / (Global.BRHistogram.Count {- 1});

    //-- draw zero x axis
    liHeight := liHeight - 4 - liTextHeight - 4;
    MyBitmap.Canvas.Pen.Color := clBlack;
    MyBitmap.Canvas.MoveTo(0, liHeight);
    MyBitmap.Canvas.LineTo(MyBitmap.Width, liHeight);

    //-- draw each bar
    for liBar := 0 to Global.BRHistogram.Count - 1 do
    begin
      lsValue := Copy(Global.BRHistogram[liBar], 1, 3);
      liSSValue := 0;
      liMSValue := 0;
      lbLastBar := liBar = Global.BRHistogram.Count - 1;
      for i := 1 to Length(Global.BRHistogram[liBar]) do
      begin
        if (Global.BRHistogram[liBar][i] = '*') then Inc(liSSValue);
        if (Global.BRHistogram[liBar][i] = '%') then Inc(liMSValue);
      end;
      DrawBar;
    end;

    //-- update the image of the form
    ImageHistogram.Picture.Bitmap.Width := MyBitmap.Width;
    ImageHistogram.Picture.Bitmap.Height := MyBitmap.Height;
    ImageHistogram.Canvas.Draw(0, 0, MyBitmap);
    //MyBitmap.SaveToFile(Format('%.3d.bmp', [FDebugCount]));
    //Global.BRHistogram.SaveToFile(Format('%.3d.txt', [FDebugCount]));
    //Inc(FDebugCount);
    //ImageHistogram.Canvas.CopyRect(Rect(0, 0, MyBitmap.Width, MyBitmap.Height), MyBitmap.Canvas, Rect(0, 0, MyBitmap.Width, MyBitmap.Height));
  finally
    MyBitmap.Free;
  end;
end;

procedure TFormProgress.FormShow(Sender: TObject);
begin
  ResetFormControls;
end;

end.

