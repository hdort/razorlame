(* (c) Copyright 2000,2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

program RazorLame;

uses
  Forms,
  registry,
  windows,
  ResStr in 'ResStr.pas',
  UtilFuncs in 'UtilFuncs.pas',
  globals in 'globals.pas',
  lame_options in 'lame_options.pas' {FormLameOptions},
  progress in 'progress.pas' {FormProgress},
  about in 'about.pas' {FormAbout},
  Log in 'Log.pas' {FormLog},
  Shutdown in 'Shutdown.pas' {FormShutdown},
  Redirect in 'Redirect.pas',
  options in 'options.pas' {FormOptions},
  WAVTools in 'WAVTools.pas',
  Main in 'Main.pas' {FormMain};

{$R *.RES}
{$R FVCSVER.RES}

var
  Handle: HWND = 0;

begin
  Application.Initialize;
  Application.Title := 'RazorLame';
  if not EnumWindows(@EnumFunc, LongInt(@Handle)) then
    PassToOtherInstance(Handle)
  else
  begin
    Application.CreateForm(TFormMain, FormMain);
    Application.Run;
  end;
end.

