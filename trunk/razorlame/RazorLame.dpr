(* (c) Copyright 2000 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

program RazorLame;

{$R 'FVCSVer.res' 'FVCSVer.rc'}
{$R 'XPThemes.res' 'XPThemes.rc'}

(*

Note: For XP Themes support, the following "fix" in ComCtrls.pas is
necessary:

----- cut -----
Unit: ComCtrls
Method: TCustomListView.UpdateColumn
Old:
if FImageIndex <> -1 then
fmt := fmt or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES;
New:
if FImageIndex <> -1 then
fmt := fmt or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES
else
mask := mask and not (LVCF_IMAGE);
----- cut -----

This seems to be valid for Delphi 5 and Delphi 6, not sure for Delphi 6sp1.

I've also "fixed" menus.pas a bit; I added the following line

----- cut -----
//--hdo: special check for WinXP (One line added)
if (Win32MajorVersion > 4) and (Win32MinorVersion > 0) and (Win32Platform = VER_PLATFORM_WIN32_NT) and TopLevel then Brush.Color := clBtnFace;
MenuItem.AdvancedDrawItem(ACanvas, ARect, State, TopLevel);
----- cut -----

in procedure DrawMenuItem just before that last line. This isn't perfect,
but works for most settings.

Again, this seems to be unnecessary with D6sp1.

*)

uses
  Forms,
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

begin
  Application.Initialize;
  Application.Title := 'RazorLame';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
