(* (c) Copyright 2000,2001 - Holger Dors
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
    LabelVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonTrayClick(Sender: TObject);
    procedure ButtonHistogramClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
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
  PanelHistogram.Hide;
  ImageHistogram.Hide;
  Splitter.Hide;
  ButtonHistogram.Enabled := false;
  Width := Width - BevelHistogram.Width - 8 - Splitter.Width;
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
  LabelStopWhenDone.Visible := false;
  LabelVersion.Caption := '';
end;

procedure TFormProgress.FormCreate(Sender: TObject);
begin
  //-- do some init stuff for controls on the form
  ProgressBarStatus.Max := 1000;
  ProgressBarBatch.Max := 1000;
  ResetFormControls;
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

procedure TFormProgress.FormResize(Sender: TObject);
begin
  DrawHistogram;
end;

procedure TFormProgress.SplitterMoved(Sender: TObject);
begin
  DrawHistogram;
end;

end.

