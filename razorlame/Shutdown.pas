(* (c) Copyright 2000 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

Unit Shutdown;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

Type
  TFormShutdown = Class(TForm)
    PanelButton: TPanel;
    ButtonCancel: TButton;
    PanelLabel: TPanel;
    LabelShutdownDescription: TLabel;
    LabelWarning: TLabel;
    LabelTimeOut: TLabel;
    TimerShutdown: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure TimerShutdownTimer(Sender: TObject);
  Private
    { Private declarations }
    FiTimeLeft: Integer;
  Public
    { Public declarations }
  End;

Var
  FormShutdown: TFormShutdown;

Implementation

uses UtilFuncs, ResStr, Globals;

{$R *.DFM}

Procedure TFormShutdown.FormCreate(Sender: TObject);
Begin
  //-- Set selected font
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  FixDPI(self, 96);
  FiTimeLeft := 30;
  LabelTimeOut.Caption := Format(LABEL_TIMEOUT, [FiTimeLeft]);
  TimerShutdown.enabled := true;
End;

Procedure TFormShutdown.TimerShutdownTimer(Sender: TObject);
Begin
  Dec(FiTimeLeft);
  If FiTimeLeft < 0 Then
    ModalResult := mrOK //-- Shutdown!
  Else
    LabelTimeOut.Caption := Format(LABEL_TIMEOUT, [FiTimeLeft]);
End;

End.

