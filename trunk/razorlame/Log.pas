(* (c) Copyright 2000-2005  -  Holger Dors
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
    ButtonSave: TButton;
    SaveDialog1: TSaveDialog;
    procedure PanelBottomResize(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure PositionButtons;
  public
    { Public declarations }
  end;

var
  FormLog: TFormLog;

implementation

{$R *.DFM}

uses Globals, UtilFuncs;

procedure TFormLog.PanelBottomResize(Sender: TObject);
begin
  PositionButtons;
end;

procedure TFormLog.ButtonDeleteClick(Sender: TObject);
begin
  if MyMsgDlg('Really delete log file?', 'Delete log?', mtConfirmation,
    [mbYes, mbNo, mbCancel], []) = mrYes then
  begin
    if DeleteFile(ChangeFileExt(Application.Exename, '.log')) then
    begin
      Global.Log.Clear;
      MemoLog.Lines.Clear;
      ButtonSave.Enabled := false;
    end;
  end;
end;

procedure TFormLog.ButtonSaveClick(Sender: TObject);
begin
  with SaveDialog1 do
  begin
    if Execute then
      Global.Log.SaveToFile(Filename);
  end;
end;

procedure TFormLog.FormCreate(Sender: TObject);
begin
  //-- Set selected font
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  FixDPI(self, 96);
  PositionButtons;
end;

procedure TFormLog.PositionButtons;
var
  liWidth, liSpace: Integer;
begin
  liWidth := PanelBottom.ClientWidth div 2;
  liSpace := ((ButtonSave.Left + ButtonSave.Width) - ButtonOK.Left) div 2;
  ButtonOK.Left := liWidth - liSpace;
  ButtonDelete.Left := ButtonOK.Left + ButtonOK.Width + 8;
  ButtonSave.Left := ButtonDelete.Left + ButtonDelete.Width + 8;
end;

end.

