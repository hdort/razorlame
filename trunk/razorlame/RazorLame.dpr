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
  Main in 'Main.pas' {FormMain},
  ResStr in 'ResStr.pas',
  UtilFuncs in 'UtilFuncs.pas',
  globals in 'globals.pas',
  lame_options in 'lame_options.pas' {FormLameOptions},
  progress in 'progress.pas' {FormProgress},
  about in 'about.pas' {FormAbout},
  Log in 'Log.pas' {FormLog},
  Shutdown in 'Shutdown.pas' {FormShutdown},
  Redirect in 'Redirect.pas',
  options in 'options.pas' {FormOptions};

{$R *.RES}
{$R FVCSVER.RES}

var
  handle: HWND;

begin
  handle := 0;

  Application.Initialize;
  Application.Title := 'RazorLame';

  if not EnumWindows(@EnumFunc, longint(@handle)) then
    PassToOtherInstance(handle)
  else
  begin
    Application.CreateForm(TFormMain, FormMain);
    Application.Run;
  end;
end.

