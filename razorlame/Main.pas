(* (c) Copyright 2000-2005  -  Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit Main;

{$I ConditionalDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ToolWin, Menus,
  EnhListView, ImgList, ActnList, Redirect, TrayIcon, SystemImageList,
  XPMenu, BrowseDr;

const
  WM_ITEM_DELETED = WM_USER + 1;
  WM_ENCODE_NEXT_FILE = WM_USER + 2;
  WM_DECODE_NEXT_FILE = WM_USER + 3;
  WM_AFTER_CREATE = WM_USER + 4;

type
  TProcessOperation = (poEncode, poDecode);

  TFormMain = class(TForm)
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemChooseFiles: TMenuItem;
    N1: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemActions: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemClearList: TMenuItem;
    N2: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuItemEncode: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemContents: TMenuItem;
    MenuItemKeywordSearch: TMenuItem;
    N3: TMenuItem;
    MenuItemAboutRazorLame: TMenuItem;
    MenuItemLameHomepage: TMenuItem;
    MenuItemRazorLameHomepage: TMenuItem;
    N4: TMenuItem;
    PanelListView: TPanel;
    ListViewFiles: TdfsEnhListView;
    PopupMenuListView: TPopupMenu;
    MenuItemPopUpDelete: TMenuItem;
    MenuItemPopUpClearList: TMenuItem;
    N5: TMenuItem;
    MenuItemPopUpAddFiles: TMenuItem;
    N6: TMenuItem;
    MenuItemPopUpEncode: TMenuItem;
    MenuItemPopUpOptions: TMenuItem;
    PopupMenuTray: TPopupMenu;
    MenuItemTrayShow: TMenuItem;
    MenuItemTrayExit: TMenuItem;
    Images: TImageList;
    ActionList: TActionList;
    ActionAddFiles: TAction;
    ActionEncode: TAction;
    ActionClearList: TAction;
    ActionRemove: TAction;
    ActionOptions: TAction;
    ActionInfo: TAction;
    PopupMenuToolbar: TPopupMenu;
    MenuItemPopupShowToolbar: TMenuItem;
    MenuItemPopupDisplay: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemLastLog: TMenuItem;
    ActionToolbar: TAction;
    ActionToolbarCaptions: TAction;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonChooseFiles: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonEncode: TToolButton;
    ToolButton5: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonClearList: TToolButton;
    ToolButtonOptions: TToolButton;
    ToolButtonAbout: TToolButton;
    ActionViewLog: TAction;
    ActionToolbarIcons: TAction;
    MenuItemPopupIcons: TMenuItem;
    MenuItemPopupCaptions: TMenuItem;
    MenuItemPopupBoth: TMenuItem;
    ActionToolbarBoth: TAction;
    MenuItemViewToolbarDisplay: TMenuItem;
    MenuItemToolbar: TMenuItem;
    MenuItemShowToolbar: TMenuItem;
    MenuItemIcons: TMenuItem;
    MenuItemCaptions: TMenuItem;
    MenuItemBoth: TMenuItem;
    TrayIcon: TTrayIcon;
    ActionDecode: TAction;
    Decode1: TMenuItem;
    Decode2: TMenuItem;
    N7: TMenuItem;
    ToolButtonDecode: TToolButton;
    MailAuthor1: TMenuItem;
    N8: TMenuItem;
    RazorLameWebForum1: TMenuItem;
    Documentation1: TMenuItem;
    dfsSystemImageList: TdfsSystemImageList;
    ActionLameOptions: TAction;
    LAMEOptions1: TMenuItem;
    XPMenu: TXPMenu;
    ActionAddFolder: TAction;
    dfsBrowseDirectoryDlgAddFolder: TdfsBrowseDirectoryDlg;
    MenuItemAddFolder: TMenuItem;
    MenuItemPopupAddFolder: TMenuItem;
    MenuItemDonate: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListViewFilesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MenuItemLameHomepageClick(Sender: TObject);
    procedure MenuItemRazorLameHomepageClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure MenuItemTrayExitClick(Sender: TObject);
    procedure PopupMenuTrayPopup(Sender: TObject);
    procedure ActionAddFilesExecute(Sender: TObject);
    procedure ActionEncodeExecute(Sender: TObject);
    procedure ActionClearListExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionInfoExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure ActionToolbarCaptionsExecute(Sender: TObject);
    procedure ActionToolbarExecute(Sender: TObject);
    procedure ActionViewLogExecute(Sender: TObject);
    procedure ListViewFilesDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ListViewFilesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListViewFilesSortFinished(Sender: TObject;
      SortColumn: Integer; Ascending: Boolean);
    procedure ActionToolbarIconsExecute(Sender: TObject);
    procedure ActionToolbarBothExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListViewFilesDeletion(Sender: TObject; Item: TListItem);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewFilesSortItems(Sender: TObject; Item1,
      Item2: TListItem; SortColumn: Integer; var SortAs: TSortAs;
      var CompResult: Integer);
    procedure ActionDecodeExecute(Sender: TObject);
    procedure MailAuthor1Click(Sender: TObject);
    procedure RazorLameWebForum1Click(Sender: TObject);
    procedure Documentation1Click(Sender: TObject);
    procedure ActionLameOptionsExecute(Sender: TObject);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure ActionAddFolderExecute(Sender: TObject);
    procedure MenuItemDonateClick(Sender: TObject);
  private
    { Private declarations }
    Redirector: TRedirector;
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    procedure OnEncodeTerminated(Sender: TObject);
    procedure OnDecodeTerminated(Sender: TObject);
    procedure OnEncodeData(Sender: TRedirector; Buffer: Pointer; Size: Integer);
    procedure OnDecodeData(Sender: TRedirector; Buffer: Pointer; Size: Integer);
    procedure AddFilesToList(Files: TStrings);
    procedure AddFileToList(asFile: string; abSaveFileList: Boolean = true);
    procedure AddFolderToList(asFolder: string);
    procedure UpdateStatusbar;
    procedure RemoveFileFromList(asFile: string);
    procedure MoveFileToListBottom(asFile: string);
    procedure SaveFileList;
    procedure LoadFileList;
    procedure ItemDeleted(var Message: TMessage); message WM_ITEM_DELETED;
    procedure WmEncodeNextFile(var Message: TMessage); message WM_ENCODE_NEXT_FILE;
    procedure WmDecodeNextFile(var Message: TMessage); message WM_DECODE_NEXT_FILE;
    procedure WmDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure WmAfterCreate(var Message: TMessage); message WM_AFTER_CREATE;
    procedure Initialize;
    function EncoderFound: Boolean;
    function CheckExistingFiles(const asExtension: string): Boolean;
    procedure ProcessFiles(aOperation: TProcessOperation);
    function ProcessNextFile(const aOperation: TProcessOperation): Boolean;
    function CheckOutputDiffersInput(const asExtension: string): Boolean;
    function DetermineOutputPath(const asInputFilePath: string): string;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses ShellApi, ResStr, Globals, UtilFuncs, Options, Lame_Options,
  Progress, About, Log, Shutdown, IniFiles, FileCtrl, Graphics;

var
  ProgressForm: TFormProgress;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.Title := APP_TITLE;
  Application.OnMinimize := AppMinimize;
  Application.OnRestore := AppRestore;
  //-- hints should appear for 10 seconds!
  //-- we do this so the hints in the option dialog can be read!
  Application.HintHidePause := 10 * 1000;

  //-- scale Constraints for fonts <> 96 dpi
  if Screen.PixelsPerInch <> 96 then
  begin
    Constraints.MinHeight := (Constraints.MinHeight * Screen.PixelsPerInch + 96 div 2) div 96;
    Constraints.MinWidth := (Constraints.MinWidth * Screen.PixelsPerInch + 96 div 2) div 96;
  end;

  ProgressForm := nil;

  //-- create controls
  Redirector := TRedirector.Create;

  //-- init controls
  ListViewFiles.FullDrag := true; //-- allow moving of columns {todo: this screws up the headings!?}
  {todo: check how to store that stuff in an ini, and not in the registry!}
  //ListViewFiles.SaveSettings.RegistryKey := ChangeFileExt(LowerCase(Application.ExeName), '.ini');
  //ListViewFiles.LoadSettings;

  //-- set defaults
  with Global do
  begin
    Encoder := ExtractFilePath(Application.ExeName) + 'Lame.exe';
    FilesToEncode := 0;
    LastOpenDir := '';
    ThreadPriority := tpIdle;
    ShutdownFlag := sfShutdown;
    SelectedFont := 'Tahoma';
    XPMenu := true;
    LRColor := $007BC7EF;
    MSColor := $00E4F6FE;
    {TODO: check if we should initialize more global stuff}
  end;

  with MP3Settings do
  begin
    Bitrate := 128;
    OutDir := '';
    UseInputDir := true;
    LameMode := lmJointStereo;
    LameOptimization := loNone;
    IncludeCRC := false;
    Flags := [];
    VBREnabled := false;
    VBRQuality := 4;
    VBRMaxBitrate := 320;
    VBRDisableTag := false;
    VBRStrictMin := false;
    VBRUseABR := false;
    VBRABRTargetBitrate := 128;
    ResampleFreq := rfDefault;
    HighpassFreq := 0;
    HighpassEnabled := false;
    HighpassWidth := 0;
    HighWidthEnabled := false;
    LowpassFreq := 0;
    LowpassEnabled := false;
    LowpassWidth := 0;
    LowWidthEnabled := false;
    DeleteFileAfterProcessing := false;
    ATHControl := athDefault;
    DifferentBlockTypes := false;
    DisableFiltering := false;
    NoRes := false;
    NoShort := false;
    ISO := false;
    CustomOptions := '';
    OnlyCustomOptions := false;
    qLevel := -1;
    Description := 'No Description';
    UsePresets := true;
    PresetMode := pmVBR;
    PresetVBRItem := 1;
    PresetABRItem := 3;
    PresetCBRItem := 3;
    PresetCustomABR := false;
    PresetCustomABRValue := 144;

    //todo: ID3 stuff
  end;

  //-- read options from ini file
  if not ReadOptions then
  begin
    //-- if we ever want to do something on the first time RazorLame
    //-- is started, here would be the right place!
  end;

  //-- if existent, read last logfile
  if FileExists(ChangeFileExt(Application.Exename, '.log')) then
  begin
    try
      Global.Log.LoadFromFile(ChangeFileExt(Application.Exename, '.log'));
    except
      Global.Log.Clear;
    end;
  end;

  //-- read "system" data. Currently, this includes all the
  //-- "informational" messages for Lame, and the allowed
  //-- input files for Lame.
  if not FileExists(ChangeFileExt(LowerCase(Application.ExeName), '.dat')) then
  begin
    MyMessageBox(Format(MSG_NO_DAT_FILE, [ChangeFileExt(LowerCase(Application.ExeName), '.dat')]),
    MSG_ERROR, MB_ICONEXCLAMATION or MB_OK);
    Application.Terminate;
    exit;
  end;
  ReadSystemData;

  //-- set the filter for the open Dialog
  OpenDialog.Filter := CreateFilterFromStringList(Global.InputFileTypes);

  //-- now initialize the rest
  Initialize;

  //-- everything else that takes some time should take place
  //-- in the WmAfterCreate event!
  PostMessage(Handle, WM_AFTER_CREATE, 0, 0);
  mainmenu.images:=images;  //by Mr. C
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Redirector.Running then Redirector.Terminate(0); {TODO: Check if 0 is correct code!}
end;

procedure TFormMain.AddFileToList(asFile: string; abSaveFileList: Boolean = true);
var
  lsCaption, lsPath, lsType: string;
  MyItem: TListItem;
  NewFileItem: TFileItem;
  liImageIndex: integer;
begin
  //-- don't add files that don't exist!
  if not FileExists(asFile) then Exit;

  //-- if file extension isn't a supported audio
  //-- type, ignore it!
  if not IsExtensionAllowed(ExtractFileExt(asFile)) then Exit;

  //-- seperate filename and path
  lsCaption := ExtractFilename(asFile);
  lsPath := ExtractFilePath(asFile);

  //-- check if this file isn't already in the list
  //-- this takes the most time when adding many files! (ca. 66% time is spent here for 350 files)
  MyItem := ListViewFiles.FindCaption(0, lsCaption, false, true, false);
  while Assigned(MyItem) do
  begin
    if MyItem.SubItems[0] = lsPath then Exit; //-- File already in list!
    //-- continue with the search!
    MyItem := ListViewFiles.FindCaption(MyItem.Index, lsCaption, false, false, false);
  end;

  NewFileItem := TFileItem.Create;
  NewFileItem.Filename := lsCaption;
  NewFileItem.Path := lsPath;
  NewFileItem.Size := GetFileSize(asFile);
  NewFileItem.Date := FileDateTime(asFile);

  with ListViewFiles.Items.Add do
  begin
    //-- Add the file
    Caption := NewFileItem.Filename;
    //-- Add Icon
    liImageIndex := dfsSystemImageList.GetFileInformation(asFile, false, false, [], lsType);
    ImageIndex := liImageIndex;
    //-- if no type is returned, just give the extionsion of the file as type
    if lsType = '' then lsType := AnsiUpperCase(Copy(ExtractFileExt(asFile), 2, MaxInt)) + ' file';
    SubItems.Add(NewFileItem.Path);
    SubItems.Add(Format('%.0n', [NewFileItem.Size * 1.0]));
    SubItems.Add(FormatDateTime('ddddd t', NewFileItem.Date));
    SubItems.Add(lsType);
    Data := Pointer(NewFileItem);
  end;

  //-- sort order is now probably wrong, so hide the arrows
  ListViewFiles.ShowSortArrows := false;

  //-- Save the new list if wanted
  if abSaveFileList then SaveFileList;
end;

procedure TFormMain.RemoveFileFromList(asFile: string);
var
  lsCaption, lsPath: string;
  MyItem: TListItem;
begin
  lsCaption := ExtractFilename(asFile);
  lsPath := ExtractFilePath(asFile);

  //-- search for this file
  MyItem := ListViewFiles.FindCaption(0, lsCaption, false, true, false);
  while Assigned(MyItem) do
  begin
    if MyItem.SubItems[0] = lsPath then Break; //-- File found!
    //-- continue with the search!
    MyItem := ListViewFiles.FindCaption(MyItem.Index, lsCaption, false, false, false);
  end;

  if Assigned(MyItem) then
  begin
    //-- Found an item, remove this from the list
    ListViewFiles.Items.Delete(ListViewFiles.Items.IndexOf(MyItem));
    //-- destroy the data
    if Assigned(MyItem.Data) then TFileItem(MyItem.Data).Free;
    //-- now update status line
    UpdateStatusbar;
    //-- save this changed list!
    SaveFileList;
  end;
end;

procedure TFormMain.MoveFileToListBottom(asFile: string);
var
  lsCaption, lsPath: string;
  MyItem: TListItem;
begin
  lsCaption := ExtractFilename(asFile);
  lsPath := ExtractFilePath(asFile);

  //-- search for this file
  MyItem := ListViewFiles.FindCaption(0, lsCaption, false, true, false);
  while Assigned(MyItem) do
  begin
    if MyItem.SubItems[0] = lsPath then Break; //-- File found!
    //-- continue with the search!
    MyItem := ListViewFiles.FindCaption(MyItem.Index, lsCaption, false, false, false);
  end;

  if Assigned(MyItem) then
  begin
    //-- Found an item, move this to the bottom of the list
    with ListViewFiles.Items do
    begin
      BeginUpdate; {todo: call ListViewFiles.BeginUpdate}
      try
        with Insert(ListViewFiles.Items.Count) do
          Assign(MyItem);
        Delete(IndexOf(MyItem));
      finally
        EndUpdate;
      end;
    end;
    //-- save this changed list!
    SaveFileList;
  end;
end;

procedure TFormMain.ListViewFilesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateStatusbar;
end;

procedure TFormMain.MenuItemLameHomepageClick(Sender: TObject);
begin
  ShellExecute(HInstance, nil, PChar(LAME_HOMEPAGE), nil, nil, SW_SHOW);
end;

procedure TFormMain.MenuItemRazorLameHomepageClick(Sender: TObject);
begin
  ShellExecute(HInstance, nil, PChar(RAZORLAME_HOMEPAGE), nil, nil, SW_SHOW);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.UpdateStatusbar;
begin
  case ListviewFiles.Items.Count of
    0: StatusBar.Panels[0].Text := STATUS_NOFILES;
    1: StatusBar.Panels[0].Text := STATUS_ONEFILE;
  else
    StatusBar.Panels[0].Text := Format(STATUS_FILES, [ListviewFiles.Items.Count]);
  end;
  StatusBar.Panels[1].Text := 'Current LAME options: ' + GetOptionString;
end;

procedure TFormMain.TrayIconDblClick(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_SHOW);
  Show;
  if Assigned(ProgressForm) then ProgressForm.Show;
  TrayIcon.Active := false;
end;

procedure TFormMain.MenuItemTrayExitClick(Sender: TObject);
begin
  if Assigned(ProgressForm) then
    //-- Stop encoding
    ProgressForm.Close
  else
    //-- exit RazorLame
    Close;
end;

procedure TFormMain.PopupMenuTrayPopup(Sender: TObject);
begin
  if Assigned(ProgressForm) then
    MenuItemTrayExit.Caption := TRAY_MENU_STOPENCODING
  else
    MenuItemTrayExit.Caption := TRAY_MENU_EXIT;
end;

procedure TFormMain.ActionAddFilesExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    InitialDir := Global.LastOpenDir;
    if Execute then
    begin
      PushCursor;
      try
        AddFilesToList(Files);
        Global.LastOpenDir := ExtractFilePath(Files.Strings[0]);
        //-- only keep Filename in memory if just one file was selected,
        //-- otherwise it's irritating users
        if Files.Count > 1 then Filename := '';
        //-- save this selected folder in the INI
        StoreInIni('System', 'LastOpenDir', Global.LastOpenDir);
      finally
        PopCursor;
      end;
    end;
  end;
end;

function TFormMain.EncoderFound: Boolean;
begin
  //-- to better handle relativ path to the encoder, change into Applications folder
  if Copy(Global.Encoder, 1, 2) = '..' then
    ChDir(ExtractFilePath(Application.ExeName));

  //-- if encoder has no path, assume it's in the App folder and change into that folder
  if ExtractFilePath(Global.Encoder) = '' then
    ChDir(ExtractFilePath(Application.ExeName));

  Result := FileExists(Global.Encoder);
  if not Result then
  begin
    MyMessageBox(Format(MSG_NOENCODER, [Global.Encoder]),
      MSG_HINT, MB_ICONEXCLAMATION or MB_OK);
    ActionOptions.Execute;
  end;
end;

function TFormMain.CheckExistingFiles(const asExtension: string): Boolean;
var
  i: Integer;
  lsFile: string;
begin
  Result := false; //-- assume worst-case
  for i := 0 to ListViewFiles.Items.Count - 1 do
  begin
    lsFile := DetermineOutputPath(ListViewFiles.Items[i].SubItems[0])
      + ChangeFileExt(ListViewFiles.Items[i].Caption, asExtension);

    //-- does this file already exist?
    if FileExists(lsFile) then
    begin
      case MyMsgDlg(Format('File "%s" already exists.'#13#10#13#10'Are you sure you want to overwrite it?',
        [lsFile]), 'Overwrite file?', mtConfirmation,
        [mbYes, mbNo, mbYesToAll, mbCancel], []) of
        mrYes: Continue;
        mrYesToAll:
          if MyMessageBox('Are you sure to overwrite all existing files?',
            'Overwrite files?', MB_ICONQUESTION or MB_YESNOCANCEL or MB_DEFBUTTON2)
            = ID_YES then
            Break
          else
            Exit;
      else
        Exit;
      end;
    end;
  end;
  //-- once we're here, everything's ok!
  Result := true;
end;

procedure TFormMain.ActionEncodeExecute(Sender: TObject);
begin
  //-- No Encoder, no encode!
  if not EncoderFound then Exit;

  //-- No files, no encode!
  if ListViewFiles.Items.Count = 0 then
  begin
    MyMessageBox(MSG_NOFILES, MSG_HINT, MB_ICONEXCLAMATION or MB_OK);
    Exit;
  end;

  //-- Mantis #89: check if Output Folder exists
  if not MP3Settings.UseInputDir and not DirectoryExists(MP3Settings.OutDir) then
  begin
    case MyMessageBox(Format(MSG_OUTDIR_DOES_NOT_EXIST, [MP3Settings.OutDir]),
     MSG_QUESTION, MB_ICONEXCLAMATION or MB_YESNOCANCEL) of
      ID_YES:
        if not ForceDirectories(MP3Settings.OutDir) then
        begin
          MyMessageBox(Format(MSG_COULD_NOT_CREATE_OUTDIR, [MP3Settings.OutDir]),
            MSG_ERROR, MB_ICONEXCLAMATION or MB_OK);
          exit;
        end;

      ID_CANCEL: exit;
    end;
  end;

  //-- check if input <> output files
  if not CheckOutputDiffersInput('.mp3') then Exit;

  //-- check if any mp3 files already exist
  if not CheckExistingFiles('.mp3') then Exit;

  //-- now process the files!
  ProcessFiles(poEncode);
end;

procedure freelistviewdataitems; //-- by Mr. C
var i:integer;
begin
with formmain.ListViewFiles do begin
   for i:=0 to Items.Count-1 do begin
       if assigned(tfileitem(items[i].data)) then
          tfileitem(items[i].data).Free;
       end;
   end;
end;

procedure TFormMain.ActionClearListExecute(Sender: TObject);
//-- Delete all file from the ListView
begin
  ListViewFiles.Items.BeginUpdate;
  try
    freelistviewdataitems; //by Mr. C
    ListViewFiles.Items.Clear;
  finally
    ListViewFiles.Items.EndUpdate;
  end;
  UpdateStatusBar;
end;

procedure TFormMain.ActionRemoveExecute(Sender: TObject);
//-- Delete all selected files from the ListView
var
  i: Integer;
begin
  with ListViewFiles do
    if SelCount > 0 then
    begin
      Items.BeginUpdate;
      try
        for i := Items.Count - 1 downto 0 do begin
          if Items[i].Selected then begin
             if assigned(tfileitem(items[i].data)) then //by Mr. C
                tfileitem(items[i].data).Free;
             Items.Delete(i);                
             end;
          end;
      finally
        Items.EndUpdate;
      end;
    end;
end;

procedure TFormMain.ActionOptionsExecute(Sender: TObject);
//-- Show Options-Dialog, centered within MainForm;
//-- if uses chooses OK, set new options and store them
//-- in the configuration file
begin
  with TFormOptions.Create(Self) do
  try
    Top := Self.Top + (Self.Height - Height) div 2;
    Left := Self.Left + (Self.Width - Width) div 2;
    SetOptions;
    if ShowModal = mrOK then
    begin
      GetOptions;
      WriteOptions;
    end;
  finally
    Free;
  end;
end;

procedure TFormMain.ActionInfoExecute(Sender: TObject);
//-- Show About-Dialog, centered within MainForm
begin
  with TFormAbout.Create(Self) do
  try
    Top := Self.Top + (Self.Height - Height) div 2;
    Left := Self.Left + (Self.Width - Width) div 2;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  ActionClearList.Enabled := ListViewFiles.Items.Count > 0;
  ActionEncode.Enabled := ListViewFiles.Items.Count > 0;
  ActionDecode.Enabled := ListViewFiles.Items.Count > 0;
  ActionRemove.Enabled := ListViewFiles.SelCount > 0;

  MenuItemViewToolbarDisplay.Enabled := ActionToolbar.Checked;

  ActionToolbarBoth.Checked := Toolbar.ShowCaptions and Assigned(Toolbar.Images);
  ActionToolbarIcons.Checked := not Toolbar.ShowCaptions and Assigned(Toolbar.Images);
  ActionToolbarCaptions.Checked := Toolbar.ShowCaptions and not Assigned(Toolbar.Images);

  ActionViewLog.Enabled := Global.Log.Count > 0;
  Handled := true;
end;

procedure TFormMain.ActionToolbarIconsExecute(Sender: TObject);
//-- Show only icons on toolbar
begin
  with Sender as TAction do
  begin
    //-- set this item checked
    Checked := true;

    //-- set icons only
    with Toolbar do
    begin
      Images := Self.Images;
      //DisabledImages := Self.DisabledImages;
      ShowCaptions := false;
      ButtonWidth := 24;
      ButtonHeight := 24;
    end;

    //-- store setting in INI-File
    StoreInIni(TOOLBAR_SECTION, LAYOUT_ITEM, LAYOUT_ICONS);
  end;
end;

procedure TFormMain.ActionToolbarCaptionsExecute(Sender: TObject);
//-- Show only captions on toolbar
begin
  with Sender as TAction do
  begin
    //-- set this item checked
    Checked := true;

    //-- Show only captions
    with Toolbar do
    begin
      ShowCaptions := true;
      Images := nil;
      DisabledImages := nil;
    end;

    //-- store setting in INI-File
    StoreInIni(TOOLBAR_SECTION, LAYOUT_ITEM, LAYOUT_CAPTIONS);
  end;
end;

procedure TFormMain.ActionToolbarBothExecute(Sender: TObject);
//-- Show icons and captions on toolbar
begin
  with Sender as TAction do
  begin
    //-- set this item checked
    Checked := true;

    //-- Show both captions and icons
    Toolbar.ShowCaptions := true;
    Toolbar.Images := Images;
    //Toolbar.DisabledImages := DisabledImages;

    //-- store setting in INI-File
    StoreInIni(TOOLBAR_SECTION, LAYOUT_ITEM, LAYOUT_BOTH);
  end;
end;

procedure TFormMain.ActionToolbarExecute(Sender: TObject);
//-- show or hide toolbar
begin
  with Sender as TAction do
  begin
    //-- toggle mode
    Checked := not Checked;

    //-- now do whatever the user wants
    Toolbar.Visible := Checked;

    //-- store setting in INI-File
    StoreInIni(TOOLBAR_SECTION, VISIBLE_ITEM, Toolbar.Visible);
  end;
end;

procedure TFormMain.ActionViewLogExecute(Sender: TObject);
//-- Show Form with Log of last run, centered within MainForm
begin
  with TFormLog.Create(Self) do
  try
    begin
      Top := Self.Top + (Self.Height - Height) div 2;
      Left := Self.Left + (Self.Width - Width) div 2;
      MemoLog.Lines.Assign(Global.Log);
      ShowModal;
    end;
  finally
    Free;
  end;
end;

procedure TFormMain.ListViewFilesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  liOffset: Integer;
begin
  with ListViewFiles do
  begin
    if Assigned(DropTarget) then
    begin
      BeginUpdate;
      try
        //-- insert Item before DropTarget, only
        //-- if the item is dragged after the last item
        //-- this item should be put at the end of the list
        liOffset := 0;
        if GetItemAt(X, Y) = nil then liOffset := 1;
        with Items.Insert(Items.IndexOf(DropTarget) + liOffset) do
          Assign(ItemFocused);
        //-- delete FocusedItem
        Items.Delete(Items.IndexOf(ItemFocused));
        //-- Workaround for whatever; else the listview will be drawn with checkboxes...
        Checkboxes := false;
        //-- Show No Sort Arrow, List is unsorted now
        ShowSortArrows := false;
      finally
        EndUpdate;
      end;
    end;
  end;
  UpdateStatusbar;
end;

procedure TFormMain.ListViewFilesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  //-- only allow dragging withing listview
  if (Source = ListViewFiles) then Accept := true;
end;

procedure TFormMain.ListViewFilesSortFinished(Sender: TObject;
  SortColumn: Integer; Ascending: Boolean);
begin
  //-- During Drag'n'drop the sort order can be destroyed,
  //-- thus the sort arrows are hidden. Now, show the arrow
  //-- as the sort order is now correct
  ListViewFiles.ShowSortArrows := true;
end;

procedure TFormMain.SaveFileList;
//-- Save current list of files in LST file
var
  i: Integer;
  MyFilelist: TStringList;
begin
  //-- Save the FileList
  MyFileList := TStringList.Create;
  try
    //-- fill the string list
    for i := 0 to ListViewFiles.Items.Count - 1 do
      with TFileItem(ListViewFiles.Items[i].Data) do
        MyFileList.Add(Path + Filename);
    //-- save the string list
    MyFileList.SaveToFile(ChangeFileExt(Application.ExeName, '.lst'));
  finally
    MyFileList.Free;
  end;
  //-- erase the old ini section; compatability stuff, see comment under "LoadFileList"
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    //-- First erase the entire section
    EraseSection(FILES_SECTION);
  finally
    Free;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    RegisterFormPosition(FORM_SECTION, Self);
  except
    on E: Exception do
      Application.ShowException(E); //-- no matter what happens, exit RL!
  end;
end;

procedure TFormMain.ListViewFilesDeletion(Sender: TObject;
  Item: TListItem);
begin
  //-- _After_ Deletion we have to update the status bar
  //-- and save the FileList!
  PostMessage(Handle, WM_ITEM_DELETED, 0, 0);
end;

procedure TFormMain.ItemDeleted(var Message: TMessage);
begin
  UpdateStatusbar;
  //-- save this changed list!
  SaveFileList;
end;

procedure TFormMain.WmDropFiles(var Message: TWMDropFiles);
var
  i, NumFiles: Longint;
  Buffer: array[0..255] of Char;
begin
  PushCursor;
  try
    //-- How many files are being dropped?
    NumFiles := DragQueryFile(Message.Drop, UINT(-1), nil, 0);
    //-- Process the dropped files
    for i := 0 to (NumFiles - 1) do
    begin
      ListViewFiles.Items.BeginUpdate;
      try
        DragQueryFile(Message.Drop, i, @Buffer, SizeOf(Buffer));
        if DirectoryExists(Buffer) then
          AddFolderToList(Buffer)
        else
          AddFileToList(Buffer, false);
      finally
        ListViewFiles.Items.EndUpdate;
      end;
    end;
  finally
    SaveFileList;
    UpdateStatusbar;
    PopCursor;
  end;
end;

procedure TFormMain.AddFolderToList(asFolder: string);
var
  F: TSearchRec;
  lsFolder: string;
begin
  PushCursor;
  try
    lsFolder := IncludeTrailingBackslash(asFolder);
    StatusBar.Panels[0].Text := Format('Scanning %s ...', [lsFolder]);
    StatusBar.Update;

    if FindFirst(lsFolder + '*.*', faAnyFile, F) = 0 then
    try
      repeat
        if (F.Name = '.') or (F.Name = '..') then Continue;
        if DirectoryExists(lsFolder + F.Name) then
          AddFolderToList(lsFolder + F.Name)
        else
          AddFileToList(lsFolder + F.Name, false);
      until FindNext(F) > 0;
    finally
      FindClose(F);
    end;
  finally
    PopCursor;
  end;
end;

procedure TFormMain.LoadFileList;
var
  MyFileList: TStringList;
  i: Integer;
begin
  {todo: why is this stuff sitting in LoadFileList??}
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    case ReadInteger(TOOLBAR_SECTION, LAYOUT_ITEM, LAYOUT_BOTH) of
      1: ActionToolbarIcons.Execute;
      2: ActionToolbarCaptions.Execute;
      3: ActionToolbarBoth.Execute;
    end;
    ActionToolbar.Checked := ReadBool(TOOLBAR_SECTION, VISIBLE_ITEM, true);
    Toolbar.Visible := ActionToolbar.Checked;
    Toolbar.Update;

    //-- that's the "old" code for loading the files
    //-- they were originally stored in the INI file
    //-- when some users were hitting the 64 KB border, I decided
    //-- to store the list seperatly in a *.lst file
    //-- this code is here only to allow a smooth "migration"
    //-- files aren't stored in the ini file any more
    //-- so this block could be simplyfied in a later version

    //-- Load File list into ListView
    StatusBar.Panels[0].Text := 'Loading files...';
    StatusBar.Update;
    ListViewFiles.Items.BeginUpdate;
    try
      MyFileList := TStringList.Create;
      try
        ReadSectionValues(FILES_SECTION, MyFileList);
        for i := 1 to MyFileList.Count do
          AddFileToList(MyFileList.Values[IntToStr(i)], false);
        //-- now load the files from the lst file
        if FileExists(ChangeFileExt(Application.ExeName, '.lst')) then
        begin
          MyFileList.Clear;
          MyFileList.LoadFromFile(ChangeFileExt(Application.ExeName, '.lst'));
          for i := 0 to MyFileList.Count - 1 do
            AddFileToList(MyFileList[i], false);
        end;
      finally
        MyFileList.Free;
        UpdateStatusbar;
      end;
    finally
      ListViewFiles.Items.EndUpdate;
    end;
  finally
    Free;
  end;
end;

procedure TFormMain.Initialize;
begin
  {Let Windows know we accept dropped files}
  DragAcceptFiles(Self.Handle, true);

  //-- initialize language-dependend stuff
  gsaPriorities[0] := IDLE_PRIORITY;
  gsaPriorities[1] := LOWEST_PRIORITY;
  gsaPriorities[2] := LOWER_PRIORITY;
  gsaPriorities[3] := NORMAL_PRIORITY;
  gsaPriorities[4] := HIGHER_PRIORITY;
  gsaPriorities[5] := HIGHEST_PRIORITY;

  ListViewFiles.RowSelect := true;

  Caption := RL_VERSION;
{$IFDEF BETAVER}
  Caption := Caption + ' BETA VERSION!';
{$ENDIF}
{$IFDEF DEBUG}
  Caption := Caption + ' DEBUG VERSION!!';
{$ENDIF}
  //StatusBar.Panels[0].Text := RL_VERSION;
  UpdateStatusBar;

  RepositionForm(FORM_SECTION, Self);

  //-- Load Toolbar-Seetings
  with TIniFile.Create(ChangeFileExt(LowerCase(Application.ExeName), '.ini')) do
  try
    case ReadInteger(TOOLBAR_SECTION, LAYOUT_ITEM, LAYOUT_BOTH) of
      1: ActionToolbarIcons.Execute;
      2: ActionToolbarCaptions.Execute;
      3: ActionToolbarBoth.Execute;
    end;
    ActionToolbar.Checked := ReadBool(TOOLBAR_SECTION, VISIBLE_ITEM, true);
    Toolbar.Visible := ActionToolbar.Checked;
  finally
    Free;
  end;

  //-- Set GUI stuff
  SetFont(Self, DESIGN_FONT, Global.SelectedFont);
  XPMenu.active := Global.XPMenu;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //-- destroy controls
  Redirector.Free;
  if listviewfiles.items.count<>0 then
     freelistviewdataitems; // zo zijn alle references dan ook verwijderd!
end;

procedure TFormMain.OnEncodeData(Sender: TRedirector; Buffer: Pointer;
  Size: Integer);
var
  i, j, liPercent: Integer;
  lbFound: Boolean;
  lsPercent: string;
  lpcLine: PChar;
  lsBuffer, Line, lsLastStatus: string;
  MyStringList: TStringList;
  ldtElapsedTime, ldtEstimatedTime, ldtDiff: TDateTime;
begin
  lpcLine := StrAlloc(Size + 1);
  try
    StrLCopy(lpcLine, Buffer, Size);
    lpcLine[Size] := #0;
    lsBuffer := string(lpcLine);
  finally
    StrDispose(lpcLine);
  end;

  //-- check for overlapping strings, using CR LF as indicator
  lsBuffer := AdjustLinebreaks(lsBuffer);
  if Global.Overlap <> '' then
    lsBuffer := Global.Overlap + lsBuffer;
  Global.Overlap := '';

  //-- determine overlap, except for last go!
  if not Redirector.LastData then
  begin
    j := 0;
    i := Pos(#13#10, lsBuffer);
    while i > 0 do
    begin
      Inc(j, i);
      Inc(j, 2);
      i := Pos(#13#10, Copy(lsBuffer, j, MAXINT));
    end;

    Global.Overlap := Copy(lsBuffer, j - 1, MAXINT);
    Delete(lsBuffer, j - 1, MAXINT);
  end;

  MyStringList := TStringList.Create;
  try
    MyStringList.Text := lsBuffer;

    for i := 0 to MyStringList.Count - 1 do
    begin
      Line := MyStringList.Strings[i];

{$IFDEF DETAIL_DEBUG}
      Global.Log.Add(Line);
{$ENDIF}

      //-- ignore blank lines
      if Trim(Line) = '' then Continue;

      //-- if no ProgressForm exists, simply log everthing
      if not Assigned(ProgressForm) then
      begin
        Global.Log.Add(Line);
        Continue;
      end;

      //-- Lame Version
      if Copy(Line, 1, 4) = 'LAME' then
        ProgressForm.LabelVersion.Caption := Line;

      //-- we have a special case when vbr is used:
      //-- perhaps we get a histogram!
      if Copy(Line, 1, 30) = '----- bitrate statistics -----' then
      begin
        lsLastStatus := Global.LastLine;
        Global.VBRHistogram := true;
        Global.Log.Add(Line);
        Continue;
      end;

      //-- save Histograms
      if Global.VBRHistogram then
      begin
        Global.Log.Add(Line);
        if Copy(Line, 1, 8) = 'average:' then
          Global.LastLine := lsLastStatus;
        Continue;
      end;

      //-- if StatusMessages is set, we should get only status messages
      if Global.StatusMessages then
      begin
        //-- another special case: 3.87 and up provide the bitrate histogram
        //-- with each progress update!
        if (Copy(Line, 5, 1) = '[') then
        begin
          if not Global.HistoMessages then
          begin
            //-- Start Histogram!
            Global.HistoMessages := true;
            Global.BRHistogram.Clear;
          end;
          Global.BRHistogram.Add(Line);
          Continue;
        end;

        if Global.HistoMessages then
        begin
          //-- End Histogram!
          Global.HistoMessages := false;
          ProgressForm.ButtonHistogram.Enabled := true;
          if ProgressForm.ImageHistogram.visible then
            ProgressForm.DrawHistogram;
        end;

        if Copy(Line, 1, 8) = 'average:' then
        begin
          Global.Log.Add(Line);
          Continue;
        end;

        assert(Pos('average', Line) = 0, Line);

        {update progress only once per second!  if in tray, only once per 10 seconds!!}
        ldtDiff := Now - Global.LastStatusUpdate;
        //-- we have to make sure that we'll get the very last messages of LAME, as there are infos there we'd like to show!
        if not Redirector.LastData and Global.StatusMessages
          and (ldtDiff < (1 / (24 * 60 * 60))) then
        begin
          exit;
        end;

        if not Redirector.LastData and Global.StatusMessages
          and TrayIcon.Active and (ldtDiff < 10 / (24 * 60 * 60)) then
          exit;

        Global.LastStatusUpdate := Now;

        //-- Main task: update progress dialog
        //-- Update display only every 2 seconds!
        //if (((Now - idtStatusStart) * 24 * 60 * 60) < 0.5) then Continue;
        //idtStatusStart := now;

        //--          1         2         3         4         5         6         7
        //-- 123456789012345678901234567890123456789012345678901234567890123456789012345
        //-- pre 3.87 format:
        //--     Frame          |  CPU/estimated  |  time/estimated | play/CPU |   ETA
        //--     50/   235( 21%)| 0:00:01/ 0:00:03| 0:00:00/ 0:00:00|    2.1412| 0:00:00
        //-- 3.87 format:
        //--     Frame          |  CPU time/estim | REAL time/estim | play/CPU |    ETA
        //--   4410/4410  (100%)|    1:05/    1:05|    1:06/    1:06|   1.7625x|    0:00

        ldtElapsedTime := Now - Global.BatchStart;
        ProgressForm.LabelBatchElapsed.Caption := FormatDateTime('h:nn:ss', ldtElapsedTime);

        //lsPercent := Trim(Copy(Line, 15, 3));
        if Global.PercentPos = 0 then
          Global.PercentPos := Pos('%', Line) - 3;

        lsPercent := Trim(Copy(Line, Global.PercentPos, 3));
        if Copy(lsPercent, 1, 1) = '(' then Delete(lsPercent, 1, 1);

        liPercent := StrToIntDef(lsPercent, 0);
        if (liPercent > 0) and (liPercent <= 100) then
        begin
          ProgressForm.LabelStatus.Caption := ID_STATUS + lsPercent + '%';
          Global.FilePercent := liPercent;
          ProgressForm.ProgressBarStatus.Position
            := Round((Global.FilePercent * ProgressForm.ProgressBarStatus.Max) / 100.0);
        end;

        ProgressForm.LabelStatusRemaining.Caption := Trim(Copy(Line, 68, 9));

        //-- Calculate Batch Percent
        Global.CurrentSize := Global.LastSize + ((Global.CurrentItemSize * Global.FilePercent) div 100);
        Global.BatchPercent := (100 * Global.CurrentSize) div Global.TotalSize;
        ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
        ProgressForm.ProgressBarBatch.Position
          := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);

        if Global.CurrentSize > 0 then
        begin
          ldtEstimatedTime := (ldtElapsedTime * Global.TotalSize) / Global.CurrentSize;
          ProgressForm.LabelBatchEstimated.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime);
          ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime - ldtElapsedTime);
        end;

         //-- todo: multi-line hint?
        with Global do
          TrayIcon.ToolTip := Format(TRAY_HINT, [CurrentFile, FilePercent, BatchPercent]);
        Application.Title := '[' + IntToStr(Global.BatchPercent) + '%] ' + APP_TITLE;

        //-- save the line for summary!
        Global.LastLine := Line;

        //-- and done!
        Continue;
      end;

      //-- if we encounter the first 'Frame' line only status messages follow
      if Copy(Line, 1, 19) = '    Frame          ' then
      begin
        Global.StatusMessages := true;
        Continue;
      end;

      //-- write log file. (Sits here as we don't want to add status-lines!
      Global.Log.Add(Line);

      //-- Check for output format
      if Copy(Line, 1, 12) = 'Encoding as ' then
      begin
        ProgressForm.LabelOutput.Caption := ID_OUTPUT + Trim(Copy(Line, 12, Length(Line)));
        Continue;
      end;

      if Copy(Line, 1, 9) = 'Encoding ' then
      begin
        ProgressForm.LabelWorking.Caption := Format(ID_WORKING, [Global.FilesEncoded + Global.FilesWithErrors + 1, Global.FilesToEncode, Global.CurrentFile]);
        //-- Status = 0%
        ProgressForm.LabelStatus.Caption := ID_STATUS + '0%';
        ProgressForm.ProgressBarStatus.Position := 0;
        ProgressForm.LabelStatusRemaining.Caption := '?';
        Continue;
      end;

      //-- ignore informational messages
      lbFound := false;
      for j := 0 to Global.EncoderInfoStrings.Count - 1 do
        if Copy(Line, 1, Global.EncoderInfoStringsLength[j]) =
          Global.EncoderInfoStrings[j] then
        begin
          lbFound := true;
          Break;
        end;
      if lbFound then Continue;

      //-- everything we haven't catched yet could be an error!
      Global.ErrorOccurred := true;
{$IFDEF DEBUG}
      Global.Log.Add('^^^ Error Line! ^^^');
{$ENDIF}
    end;
  finally
    MyStringList.Free;
  end;
end;

procedure TFormMain.OnEncodeTerminated(Sender: TObject);
var
  lsFile, lsSamples, lsTime, lsCPU, lsSpeed: string;
  ldtElapsedTime, ldtEstimatedTime: TDateTime;
begin

  if Assigned(ProgressForm) then
  begin
    //--  if no error occurred and StatusMessages were expected
    //-- and we are in OnTerminate, it looks like all went well!
    if not Global.ErrorOccurred and Global.StatusMessages then
    begin
      assert(Global.Overlap = '', Global.Overlap);
      if MP3Settings.DeleteFileAfterProcessing then
        DeleteFile(Global.CurrentFileFullname);
      RemoveFileFromList(Global.CurrentFileFullname);
      //-- add a nice summary line! (if we have one!)
      lsSamples := Trim(Copy(Global.LastLine, 8, 6));
      if StrToIntDef(lsSamples, -1) > 0 then
      begin
        lsCPU := Trim(Copy(Global.LastLine, 21, 8));
        lsTime := Trim(Copy(Global.LastLine, 39, 8));
        lsSpeed := Trim(Copy(Global.LastLine, 57, 10));
        if Pos('x', lsSpeed) > 0 then
          Global.Log.Add(Format('Encoded %s samples in %s (CPU: %s) (%s)',
            [lsSamples, lsTime, lsCPU, lsSpeed]))
        else
          Global.Log.Add(Format('Encoded %s samples in %s (CPU: %s) (%s x)',
            [lsSamples, lsTime, lsCPU, lsSpeed]));
      end;

      //-- print out last BR histogram
      if Global.BRHistogram.Count > 0 then
      begin
        Global.Log.AddStrings(Global.BRHistogram);
      end;

      //-- we have to check if this was the last file in the batch,
      //-- because than we have to set the batch to full as well!
      Inc(Global.FilesEncoded);
      if Global.FilesToEncode = Global.FilesEncoded then
      begin
        ProgressForm.LabelBatch.Caption := ID_BATCH + '100%';
        ProgressForm.ProgressBarBatch.Position := 1000;
        ProgressForm.LabelBatchRemaining.Caption := LABEL_BATCHDONE;
        ProgressForm.Caption := LABEL_ENCODINGFINISHED;
      end;
    end;

    if Global.ErrorOccurred then
    begin
      Global.ErrorOccurredInBatch := true;
      Inc(Global.FilesWithErrors);
      MoveFileToListBottom(Global.CurrentFileFullname);
      //-- Delete 0 byte files after an error occurred
      lsFile := DetermineOutputPath(ExtractFilePath(Global.CurrentFileFullname))
        + ChangeFileExt(ExtractFilename(Global.CurrentFileFullname), '.mp3');
      DeleteZeroByteFile(lsFile);

      //-- add a note about the error
      Global.Log.Add(Format('RazorLame encountered an unknown message from LAME while trying to encode "%s"!',
        [Global.CurrentFileFullname]));
    end;

    //-- should we stop with the last file?
    //-- or are we through with the batch?
    if Global.StopWhenDone
      or (Global.FilesEncoded + Global.FilesWithErrors = Global.FilesToEncode) then
    begin
      ProgressForm.ExternalClosed := true;
      ProgressForm.ModalResult := mrOK;
      Global.Log.Add('');
      if Global.FilesEncoded = 1 then
        Global.Log.Add(Format('Encoded one file in %s',
          [FormatDateTime('h:nn:ss', Now - Global.BatchStart)]))
      else
        Global.Log.Add(Format('Encoded %d files in %s',
          [Global.FilesEncoded, FormatDateTime('h:nn:ss', Now - Global.BatchStart)]));
      if Global.FilesWithErrors > 0 then
      begin
        if Global.FilesWithErrors = 1 then
          Global.Log.Add('There was an unexpected LAME message for one file, please check log for error messages.')
        else
          Global.Log.Add(Format('There were unexpected LAME messages for %d files, please check log for error messages.',
            [Global.FilesWithErrors]));
      end;
      TrayIcon.ToolTip := TRAY_ENCODING_DONE;
      Application.Title := APP_TITLE;
      Exit;
    end;

    //-- Update Batch Information
    Global.CurrentSize := Global.LastSize + Global.CurrentItemSize;
    Global.LastSize := Global.CurrentSize;

    Global.BatchPercent := (100 * Global.CurrentSize) div Global.TotalSize;
    ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
    ProgressForm.ProgressBarBatch.Position
      := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);

    if Global.CurrentSize > 0 then
    begin
      ldtElapsedTime := Now - Global.BatchStart;
      ldtEstimatedTime := (ldtElapsedTime * Global.TotalSize) / Global.CurrentSize;
      ProgressForm.LabelBatchEstimated.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime);
      ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime - ldtElapsedTime);
    end;

    //-- Check if there's more work to do?
    if ListViewFiles.Items.Count > 0 then
    begin
      Global.Log.Add('');
      PostMessage(Handle, WM_ENCODE_NEXT_FILE, 0, 0);
      Exit;
    end;
  end;
end;

procedure TFormMain.WmEncodeNextFile(var Message: TMessage);
begin
  ProcessNextFile(poEncode);
end;

procedure TFormMain.AppMinimize(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_MINIMIZE);
end;

procedure TFormMain.AppRestore(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_RESTORE);
end;

procedure TFormMain.ListViewFilesSortItems(Sender: TObject; Item1,
  Item2: TListItem; SortColumn: Integer; var SortAs: TSortAs;
  var CompResult: Integer);
var
  Str1, Str2: string;
begin
  case SortColumn of
    -1: SortAs := saString;
    0:
      begin
        Str1 := Item1.SubItems[SortColumn];
        Str2 := Item2.SubItems[SortColumn];
        CompResult := AnsiCompareStr(Str1, Str2);
        if CompResult = 0 then
        begin
          //-- second criteria: use filename!
          Str1 := Item1.Caption;
          Str2 := Item2.Caption;
          CompResult := AnsiCompareStr(Str1, Str2);
        end;
      end;
    1: SortAs := saNumeric;
    2: SortAs := saDateTime;
    else
       sortas:=sastring;  // added for sorting of filetype (mpx/wave) (c) 2003 Mr. C
  end;

end;

procedure TFormMain.ProcessFiles(aOperation: TProcessOperation);
var
  i, liModalResult: Integer;

  function ThreadPriorityToPriorityClass(Value: TThreadPriority): TPriorityClass;
  begin
    Result := pcIdle;
    case Value of
      tpIdle: Result := pcIdle;
      tpNormal: Result := pcNormal;
      tpHigh: Result := pcHigh;
      tpRealtime: Result := pcRealtime;
    end;
  end;

begin
  //-- initialize TRedirector
  Redirector.InitialPriority := ThreadPriorityToPriorityClass(Global.ThreadPriority);
  Redirector.DefaultErrorMode := true;

  if aOperation = poEncode then
  begin
    Redirector.OnData := OnEncodeData;
    //Redirector.OnErrorData := MyOnError;
    Redirector.OnErrorData := OnEncodeData;
    Redirector.OnTerminated := OnEncodeTerminated;
    Redirector.KillOnDestroy := true;
    //Redirector.StartSuspended := false;
  end
  else
  begin
    Redirector.OnData := OnDecodeData;
    //Redirector.OnErrorData := MyOnError;
    Redirector.OnErrorData := OnDecodeData;
    Redirector.OnTerminated := OnDecodeTerminated;
    Redirector.KillOnDestroy := true;
    //Redirector.StartSuspended := false;
  end;

  TrayIcon.ToolTip := TRAY_ENCODING_START;
  ProgressForm := TFormProgress.Create(Self);
  try
    ProgressForm.Top := Self.Top + (Self.Height - ProgressForm.Height) div 2;
    ProgressForm.Left := Self.Left + (Self.Width - ProgressForm.Width) div 2;
    if aOperation = poDecode then
    begin
      ProgressForm.Caption := ID_DEC_IN_PROGRESS;
      ProgressForm.LabelStopWhenDone.Caption := ID_STOP_DEC;
    end
    else
    begin
      ProgressForm.Caption := ID_ENC_IN_PROGRESS;
      ProgressForm.LabelStopWhenDone.Caption := ID_STOP_ENC;
    end;
    Global.ErrorOccurredInBatch := false;
    Global.StopWhenDone := false;
    Global.Log.Clear;
    Global.FilesToEncode := ListViewFiles.Items.Count;
    Global.FilesEncoded := 0;
    Global.FilesWithErrors := 0;
    //-- How many bytes are in the batch?
    Global.TotalSize := 0;
    for i := 0 to ListViewFiles.Items.Count - 1 do
      Inc(Global.TotalSize, TFileItem(ListViewFiles.Items[i].Data).Size);
    Global.CurrentItemSize := 0;
    Global.LastSize := 0;
    Global.CurrentSize := 0;
    Global.BatchPercent := 0;
    Global.BatchStart := Now;
    ProcessNextFile(aOperation);
    liModalResult := ProgressForm.ShowModal;
    Application.Title := APP_TITLE;
    if Redirector.Running then Redirector.Terminate(0); {TODO: check if 0 is correct}
    if Global.ErrorOccurredInBatch then
    begin
      //-- There has been an error!
      if MyMessageBox(MSG_ANERROROCCURRED, 'Oops, something wasn''t as expected!',
        MB_ICONEXCLAMATION or MB_YESNOCANCEL) = ID_YES then
      begin
        with TFormLog.Create(Self) do
        try
          begin
            Top := Self.Top + (Self.Height - Height) div 2;
            Left := Self.Left + (Self.Width - Width) div 2;
            MemoLog.Lines.Assign(Global.Log);
            ShowModal;
              //-- Save the log
            Global.Log.SaveToFile(ChangeFileExt(Application.Exename, '.log'));
          end;
        finally
          Free;
        end;
      end;
    end
    else
    begin
      //-- we're through, and we didn't have an error,
      //-- now save the log-file
      Global.Log.SaveToFile(ChangeFileExt(Application.Exename, '.log'));
      //-- perhaps we should shutdown windows?
      if ProgressForm.CheckBoxShutdown.Checked and (liModalResult <> mrCancel) then
      begin
        with TFormShutdown.Create(Self) do
        try
          Top := Self.Top + (Self.Height - Height) div 2;
          Left := Self.Left + (Self.Width - Width) div 2;
          if ShowModal = mrOK then
            ShutdownWindows(Global.ShutdownFlag);
        finally
          Free;
        end;
      end;
    end;
  finally
    ProgressForm.Free;
    ProgressForm := nil;
  end;
end;

procedure TFormMain.ActionDecodeExecute(Sender: TObject);
begin
  //-- No Encoder, no decode!
  if not EncoderFound then Exit;

  //-- No files, no decode!
  if ListViewFiles.Items.Count = 0 then
  begin
    MyMessageBox('There are no files to decode!', MSG_HINT,
      MB_ICONEXCLAMATION or MB_OK);
    Exit;
  end;

  //-- check if input <> output files
  if not CheckOutputDiffersInput('.wav') then Exit;

  //-- check if any wav files already exist
  if not CheckExistingFiles('.wav') then Exit;

  //-- now process the files!
  ProcessFiles(poDecode);
end;

function TFormMain.ProcessNextFile(const aOperation: TProcessOperation): Boolean;
var
  lsCmd: string;
begin
  Result := false;
  if ListViewFiles.Items.Count > 0 then
  begin
    //-- to better handle relativ path to the encoder, change into Applications folder
    if Copy(Global.Encoder, 1, 2) = '..' then
      ChDir(ExtractFilePath(Application.ExeName));

    //-- if encoder has no path, assume it's in the App folder and change into that folder
    if ExtractFilePath(Global.Encoder) = '' then
      ChDir(ExtractFilePath(Application.ExeName));

    //-- construct command line
    if aOperation = poEncode then
    begin
      //-- Mantis #36: Better support for LameAPE
      if (ExtractFileExt(ListViewFiles.Items[0].SubItems[0]) = '.ape')
        and (Pos('--apeinput', GetOptionString) = 0) then
          lsCmd := ExpandFileName(Global.Encoder) + GetOptionString
            + '--apeinput'
            + ' "' + ListViewFiles.Items[0].SubItems[0]
            + ListViewFiles.Items[0].Caption + '" "'
      else
        lsCmd := ExpandFileName(Global.Encoder) + GetOptionString
          + ' "' + ListViewFiles.Items[0].SubItems[0]
          + ListViewFiles.Items[0].Caption + '" "';
    end
    else
      lsCmd := ExpandFileName(Global.Encoder) + ' --decode'
        + ' "' + ListViewFiles.Items[0].SubItems[0]
        + ListViewFiles.Items[0].Caption + '" "';

    lsCmd := lsCmd + DetermineOutputPath(ListViewFiles.Items[0].SubItems[0]);

    if aOperation = poEncode then
      lsCmd := lsCmd + ChangeFileExt(ListViewFiles.Items[0].Caption, '.mp3') + '"'
    else
      lsCmd := lsCmd + ChangeFileExt(ListViewFiles.Items[0].Caption, '.wav') + '"';
    Redirector.CommandLine := lsCmd;
    Global.StatusMessages := false;
    Global.VBRHistogram := false;
    Global.HistoMessages := false;
    Global.BRHistogram.Clear;
    Global.ErrorOccurred := false;
    Global.Log.Add('Command: ' + lsCmd);
    Global.FileStart := Now;
    Global.FilePercent := 0;
    Global.LastStatusUpdate := 0;
    Global.Overlap := '';
    Redirector.Execute;
    Global.CurrentFileFullname := ListViewFiles.Items[0].SubItems[0] + ListViewFiles.Items[0].Caption;
    Global.CurrentFile := ListViewFiles.Items[0].Caption;
    Global.CurrentItemSize := TFileItem(ListViewFiles.Items[0].Data).Size;

    //THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_IDLE
{    if not SetThreadPriority(Redirector.ThreadID, THREAD_PRIORITY_IDLE)
      then GetSystemErrorMessage(GetLastError);}

    Result := true;
  end;
end;

procedure TFormMain.WmDecodeNextFile(var Message: TMessage);
begin
  ProcessNextFile(poDecode);
end;

procedure TFormMain.OnDecodeData(Sender: TRedirector; Buffer: Pointer;
  Size: Integer);
var
  i, j, liPos1, liPos2, liFrameNo, liFrameTotal, liPercent: Integer;
  lbFound: Boolean;
  lsPercent, lsFrameNo, lsFrameTotal: string;
  lpcLine: PChar;
  lsBuffer, Line: string;
  MyStringList: TStringList;
  ldtDiff, ldtElapsedTime, ldtEstimatedTime: TDateTime;
begin
  {update once per second only! in in tray, update only once every 10 seconds!!}
  ldtDiff := Now - Global.LastStatusUpdate;
  if Global.StatusMessages and (ldtDiff < (1 / (24 * 60 * 60))) then
  begin
{$IFDEF DETAIL_DEBUG}
    Global.Log.Add('Exiting at Time Threshold 1');
{$ENDIF}
    exit;
  end;

  if Global.StatusMessages and TrayIcon.Active and (ldtDiff < 10 / (24 * 60 * 60)) then
    exit;
  Global.LastStatusUpdate := Now;

  lpcLine := StrAlloc(Size + 1);
  try
    StrLCopy(lpcLine, Buffer, Size);
    lpcLine[Size] := #0;
    lsBuffer := string(lpcLine);
  finally
    StrDispose(lpcLine);
  end;

  //-- check for overlapping strings, using CR LF as indicator
  lsBuffer := AdjustLinebreaks(lsBuffer);
  if Global.Overlap <> '' then
    lsBuffer := Global.Overlap + lsBuffer;
  Global.Overlap := '';

  j := 0;
  i := Pos(#13#10, lsBuffer);
  while i > 0 do
  begin
    Inc(j, i);
    Inc(j, 2);
    i := Pos(#13#10, Copy(lsBuffer, j, MAXINT));
  end;

  Global.Overlap := Copy(lsBuffer, j - 1, MAXINT);
  Delete(lsBuffer, j - 1, MAXINT);

  MyStringList := TStringList.Create;
  try
    MyStringList.Text := lsBuffer;

    for i := 0 to MyStringList.Count - 1 do
    begin
      Line := MyStringList.Strings[i];

      //Global.Log.Add(Line);

      //-- ignore blank lines
      if Trim(Line) = '' then Continue;

      //-- if no ProgressForm exists, simply log everthing
      if not Assigned(ProgressForm) then
      begin
        Global.Log.Add(Line);
        Continue;
      end;

      //-- if StatusMessages is set, we should get only status messages
      if Global.StatusMessages then
      begin
        //--          1         2         3         4         5         6         7
        //-- 123456789012345678901234567890123456789012345678901234567890123456789012345
        //-- Frame# 5 [ 3674]  128kbs
        //-- Frame#     1/9292     1 kbps (starting with 3.86 beta!)
        ldtElapsedTime := Now - Global.BatchStart;
        ProgressForm.LabelBatchElapsed.Caption := FormatDateTime('h:nn:ss', ldtElapsedTime);

        //-- Calculate File Percent
        //-- check what version!
        liPos1 := Pos('[', Line);
        if liPos1 > 0 then
        begin //-- pre 3.86
          liPos2 := Pos(']', Line);
          lsFrameNo := Trim(Copy(Line, 7, liPos1 - 7));
          lsFrameTotal := Trim(Copy(Line, liPos1 + 1, liPos2 - liPos1 - 1));
        end
        else
        begin //-- post 3.86
          liPos1 := Pos('/', Line);
          lsFrameNo := Trim(Copy(Line, 7, liPos1 - 7));
          lsFrameTotal := Trim(Copy(Line, liPos1 + 1, 7));
        end;

        liFrameNo := StrToIntDef(lsFrameNo, 0);
        liFrameTotal := StrToIntDef(lsFrameTotal, 0);

        if liFrameTotal > 0 then
        begin
          liPercent := liFrameNo * 100 div liFrameTotal;
          if (liPercent > 0) and (liPercent <= 100) then
          begin
            ProgressForm.LabelStatusRemaining.Caption := FormatDateTime('h:nn:ss',
              (Now - Global.FileStart) - ((Now - Global.FileStart) * Global.CurrentItemSize) / (Global.CurrentItemSize * liPercent div 100));
            Global.FilePercent := liPercent
          end;
        end;
        {We do get some corrupted lines now and then; I believe this
        comes from the buffer "wrapping" over to the next portion to
        be read. Could probably be handled by checking for a proper
        CR/LF at the end of each buffer, the rest shoudl be prepended
        to the next buffer. Not sure if I bother enough to really do this...}
        {
        else
        begin
          Global.ErrorOccurred := true;
          Global.Log.Add(Line);
        end;}

        lsPercent := IntToStr(Global.FilePercent);
        ProgressForm.LabelStatus.Caption := ID_STATUS + lsPercent + '%';
        ProgressForm.ProgressBarStatus.Position
          := Round((Global.FilePercent * ProgressForm.ProgressBarStatus.Max) / 100.0);

        //-- Calculate Batch Percent
        Global.CurrentSize := Global.LastSize + ((Global.CurrentItemSize * Global.FilePercent) div 100);
        Global.BatchPercent := (100 * Global.CurrentSize) div Global.TotalSize;
        ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
        ProgressForm.ProgressBarBatch.Position
          := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);
        if Global.CurrentSize > 0 then
        begin
          ldtEstimatedTime := (ldtElapsedTime * Global.TotalSize) / Global.CurrentSize;
          ProgressForm.LabelBatchEstimated.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime);
          ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime - ldtElapsedTime);
        end;

         //-- todo: multi-line hint?
        with Global do
          TrayIcon.ToolTip := Format(TRAY_HINT, [CurrentFile, FilePercent, BatchPercent]);
        Application.Title := '[' + IntToStr(Global.BatchPercent) + '%] ' + APP_TITLE;

        //-- save the line for summary!
        Global.LastLine := Line;

        //-- and done!
        Continue;
      end;

      //-- if we encounter the first 'Frame#' line only status messages follow
      if Copy(Line, 1, 7) = 'Frame# ' then
      begin
        Global.StatusMessages := true;
        Continue;
      end;

      //-- write log file. (Sits here as we don't want to add status-lines!
      Global.Log.Add(Line);

      //-- Check for output format
      if Copy(Line, 1, 8) = 'output: ' then
      begin
        ProgressForm.LabelOutput.Caption := ID_OUTPUT + '.WAV';
        Continue;
      end;

      if Copy(Line, 1, 7) = 'input: ' then
      begin
        ProgressForm.LabelWorking.Caption := Format(ID_WORKING, [Global.FilesEncoded + Global.FilesWithErrors + 1, Global.FilesToEncode, Global.CurrentFile]);
        //-- Status = 0%
        ProgressForm.LabelStatus.Caption := ID_STATUS + '0%';
        ProgressForm.ProgressBarStatus.Position := 0;
        ProgressForm.LabelStatusRemaining.Caption := '?';
        Continue;
      end;

      //-- ignore informational messages
      lbFound := false;
      for j := 0 to Global.DecodeInfoStrings.Count - 1 do
        if Copy(Line, 1, Global.DecodeInfoStringsLength[j]) =
          Global.DecodeInfoStrings[j] then
        begin
          lbFound := true;
          Break;
        end;
      if lbFound then Continue;

      //-- everything we haven't catched yet could be an error!
      Global.ErrorOccurred := true;
    end;
  finally
    MyStringList.Free;
  end;
end;

procedure TFormMain.OnDecodeTerminated(Sender: TObject);
var
  lsFile: string;
begin

  if Assigned(ProgressForm) then
  begin
    //--  if no error occurred and StatusMessages were expected
    //-- and we are in OnTerminate, it looks like all went well!
    if not Global.ErrorOccurred and Global.StatusMessages then
    begin
      if MP3Settings.DeleteFileAfterProcessing then
        DeleteFile(Global.CurrentFileFullname);
      RemoveFileFromList(Global.CurrentFileFullname);
      //-- we have to check if this was the last file in the batch,
      //-- because than we have to set the batch to full as well!
      Inc(Global.FilesEncoded);
      if Global.FilesToEncode = Global.FilesEncoded then
      begin
        ProgressForm.LabelBatch.Caption := ID_BATCH + '100%';
        ProgressForm.ProgressBarBatch.Position := 1000;
        ProgressForm.LabelBatchRemaining.Caption := LABEL_BATCHDONE;
        ProgressForm.Caption := LABEL_ENCODINGFINISHED;
      end;
    end;

    if Global.ErrorOccurred then
    begin
      Global.ErrorOccurredInBatch := true;
      Inc(Global.FilesWithErrors);
      MoveFileToListBottom(Global.CurrentFileFullname);
      //-- Delete 0 byte files after an error occurred
      lsFile := DetermineOutputPath(ExtractFilePath(Global.CurrentFileFullname))
        + ChangeFileExt(ExtractFilename(Global.CurrentFileFullname), '.wav');
      DeleteZeroByteFile(lsFile);

      //-- add a note about the error
      Global.Log.Add(Format('RazorLame encountered an unknown message from LAME while trying to decode "%s"!',
        [Global.CurrentFileFullname]));
    end;

    //-- should we stop with the last file?
    //-- or are we through with the batch?
    if Global.StopWhenDone
      or (Global.FilesEncoded + Global.FilesWithErrors = Global.FilesToEncode) then
    begin
      ProgressForm.ExternalClosed := true;
      ProgressForm.ModalResult := mrOK;
      Global.Log.Add('');
      if Global.FilesEncoded = 1 then
        Global.Log.Add(Format('Decoded one file in %s',
          [FormatDateTime('h:nn:ss', Now - Global.BatchStart)]))
      else
        Global.Log.Add(Format('Decoded %d files in %s',
          [Global.FilesEncoded, FormatDateTime('h:nn:ss', Now - Global.BatchStart)]));
      if Global.FilesWithErrors > 0 then
      begin
        if Global.FilesWithErrors = 1 then
          Global.Log.Add('There was an unexpected LAME message for one file, please check log for error messages.')
        else
          Global.Log.Add(Format('There were unexpected LAME messages for %d files, please check log for error messages.',
            [Global.FilesWithErrors]));
      end;
      TrayIcon.ToolTip := 'Finished decoding';
      Application.Title := APP_TITLE;
      Exit;
    end;

    //-- Update Batch Information
    Global.CurrentSize := Global.LastSize + Global.CurrentItemSize;
    Global.LastSize := Global.CurrentSize;

    Global.BatchPercent := (100 * Global.CurrentSize) div Global.TotalSize;
    ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
    ProgressForm.ProgressBarBatch.Position
      := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);

    if Global.CurrentSize > 0 then
      ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss',
        (Now - Global.BatchStart) - ((Now - Global.BatchStart) * Global.TotalSize) / Global.CurrentSize);

    //-- Check if there's more work to do?
    if ListViewFiles.Items.Count > 0 then
    begin
      Global.Log.Add('');
      PostMessage(Handle, WM_DECODE_NEXT_FILE, 0, 0);
      Exit;
    end;
  end;
end;

procedure TFormMain.MailAuthor1Click(Sender: TObject);
begin
  ShellExecute(HInstance, nil,
    PChar('mailto:avcsuite@fastmail.fm?subject='
    + StringReplace(RL_VERSION, ' ', '%20', [rfReplaceAll])),
    nil, nil, SW_SHOW);
end;

function TFormMain.CheckOutputDiffersInput(const asExtension: string): Boolean;
var
  i: Integer;
  lsFile: string;
begin
  Result := false; //-- assume worst-case
  for i := 0 to ListViewFiles.Items.Count - 1 do
  begin
    lsFile := DetermineOutputPath(ListViewFiles.Items[i].SubItems[0])
      + ChangeFileExt(ListViewFiles.Items[i].Caption, asExtension);

    //-- is input = output file?
    if lsFile = ListViewFiles.Items[i].SubItems[0] + ListViewFiles.Items[i].Caption then
    begin
      MyMsgDlg('Input file and output file must be different!'#13#10#13#10 +
        Format('Please correct this for "%s".', [lsFile]), 'Information', mtInformation, [mbOK], []);
      Exit;
    end;
  end;
  //-- once we're here, everything's ok!
  Result := true;
end;

procedure TFormMain.RazorLameWebForum1Click(Sender: TObject);
begin
  ShellExecute(HInstance, nil, PChar(RAZORLAME_FORUM), nil, nil, SW_SHOW);
end;

procedure TFormMain.Documentation1Click(Sender: TObject);
begin
  ShellExecute(HInstance, nil, pchar(extractfilepath(application.exename)+'RazorLame.html'), nil, nil, SW_SHOW); //by Mr. C
end;

procedure TFormMain.AddFilesToList(Files: TStrings);
var
  i: Integer;
begin
  ListViewFiles.Items.BeginUpdate;
  try
    for i := 0 to Files.Count - 1 do
      AddFileToList(Files[i], false);
    SaveFileList;
  finally
    ListViewFiles.Items.EndUpdate;
  end;
end;

procedure TFormMain.WmAfterCreate(var Message: TMessage);
begin
  //-- show the main form NOW!
  Show;
  //-- load file list
  LoadFileList;
end;

procedure TFormMain.ActionLameOptionsExecute(Sender: TObject);
begin
  with TFormLameOptions.Create(Self) do
  try
    Top := Self.Top + (Self.Height - Height) div 2;
    Left := Self.Left + (Self.Width - Width) div 2;
    SetOptions;
    if ShowModal = mrOK then
    begin
      GetOptions;
      WriteOptions;
      UpdateStatusbar;
    end;
  finally
    Free;
  end;
end;

function TFormMain.DetermineOutputPath(const asInputFilePath: string): string;
begin
  Result := asInputFilePath;
  if MP3Settings.UseInputDir then exit;
  if (Trim(MP3Settings.OutDir) <> '') and DirectoryExists(MP3Settings.OutDir) then
    Result := MP3Settings.OutDir
end;

procedure TFormMain.ListViewFilesDblClick(Sender: TObject);
begin
  //-- execute that file! If we have one!
  if assigned(ListViewFiles.Selected) then
     // ShellExecute(HInstance, nil, PChar(ListViewFiles.ItemFocused.SubItems[0]+listviewfiles.ItemFocused.caption), nil, nil, SW_SHOW); // modified data by Mr. C, prevents leaks!!!!!
     ShellExecute(HInstance, nil, PChar(IncludeTrailingBackslash(TFileItem(ListViewFiles.ItemFocused.Data).Path) + TFileItem(ListViewFiles.Selected.Data).Filename), nil, nil, SW_SHOW);
end;

procedure TFormMain.ActionAddFolderExecute(Sender: TObject);
begin
  if dfsBrowseDirectoryDlgAddFolder.Execute then
    AddFolderToList(dfsBrowseDirectoryDlgAddFolder.Selection);
end;

procedure TFormMain.MenuItemDonateClick(Sender: TObject);
begin
  ShellExecute(HInstance, nil, PChar(RAZORLAME_DONATE), nil, nil, SW_SHOW);
end;

end.

