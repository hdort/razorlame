(* (c) Copyright 2000,2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit Log;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormLog = class(TForm)
    PanelBottom: TPanel;
    ButtonOK: TButton;
    MemoLog: TMemo;
    ButtonDelete: TButton;
    procedure PanelBottomResize(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLog: TFormLog;

implementation

{$R *.DFM}

uses Globals, UtilFuncs;

procedure TFormLog.PanelBottomResize(Sender: TObject);
var
  liWidth, liSpace: Integer;
begin
  liWidth := PanelBottom.ClientWidth div 2;
  liSpace := (ButtonDelete.Left - (ButtonOK.Left + ButtonOK.Width)) div 2;
  ButtonOK.Left := liWidth - ButtonOK.Width - liSpace;
  ButtonDelete.Left := liWidth + liSpace;
end;

procedure TFormLog.ButtonDeleteClick(Sender: TObject);
begin
  if MyMsgDlg('Really delete log file?', 'Delete log?', mtConfirmation,
    [mbYes, mbNo, mbCancel], []) = mrYes then
  begin
    if DeleteFile(ExtractFilePath(Application.Exename) + LOG_FILENAME) then
    begin
      Global.Log.Clear;
      MemoLog.Lines.Clear;
    end;
  end;
end;

end.

