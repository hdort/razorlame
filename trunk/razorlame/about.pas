(* (c) Copyright 2000,2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit about;

{$I ConditionalDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormAbout = class(TForm)
    ButtonOK: TButton;
    LabelVersion: TLabel;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses Globals, UtilFuncs;

{$R *.DFM}

function GetFileVersion(const sFileName: string): string;
var
  n, Len: Cardinal;
  Value, Buf: PChar;
begin
  Result := '';
  n := GetFileVersionInfoSize(PChar(sFileName), n);
  if (n > 0) then
  begin
    Buf := AllocMem(n);
    try
      Windows.GetFileVersionInfo(PChar(sFileName), 0, n, Buf);
      //if VerQueryValue(Buf, '\StringFileInfo\080904E4\FileVersion',
      if VerQueryValue(Buf, '\StringFileInfo\040904E4\FileVersion',
        Pointer(Value), Len) then
        Result := Value;
    finally
      FreeMem(Buf, n);
    end;
  end;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  //LabelVersion.Caption := GetFileVersion(Application.ExeName);
  with LabelVersion do
  begin
    Caption := 'RazorLame V ' + GetFileVersion(Application.ExeName);
{$IFDEF BETAVER}
    Caption := Caption + ' BETA!';
{$ENDIF}
{$IFDEF DEBUG}
    Caption := Caption + ' DEBUG!!';
{$ENDIF}
  end;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  //-- Set selected font
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  FixDPI(self, 96);
end;

end.

