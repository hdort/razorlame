(* (c) Copyright 2000,2001 - Holger Dors
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
  EnhListView, ImgList, ActnList, TrayIcon, SystemImageList, Globals, WAVTools, Redirect;

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
    MenuItemPopUpChooseFiles: TMenuItem;
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
    ActionDecode: TAction;
    Decode1: TMenuItem;
    Decode2: TMenuItem;
    N7: TMenuItem;
    ToolButtonDecode: TToolButton;
    MailAuthor1: TMenuItem;
    N8: TMenuItem;
    RazorLameWebForum1: TMenuItem;
    Documentation1: TMenuItem;
    dfsSystemImageList1: TdfsSystemImageList;
    ActionLameOptions: TAction;
    LAMEOptions1: TMenuItem;
    ToolButtonShowProgress: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonPreview: TToolButton;
    ActionPreview: TAction;
    ToolButtonRequeue: TToolButton;
    ActionRequeue: TAction;
    Preview1: TMenuItem;
    N9: TMenuItem;
    PopupMenuEncoder: TPopupMenu;
    Lame1: TMenuItem;
    ActionEncodeLame: TAction;
    ActionEncodeFaac: TAction;
    EncodewithFAAC1: TMenuItem;
    Preview2: TMenuItem;
    Requeue1: TMenuItem;
    ActionSelectAll: TAction;
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
    procedure SuspendLame(suspend: boolean);
    procedure ToolButtonShowProgressClick(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionRequeueExecute(Sender: TObject);
    procedure ProcessLineLAMEEncode(const Line: string);
    procedure ProcessLineFAACEncode(const Line: string);
    procedure ProcessLineLAMEDecode(const Line: string);
    procedure CommonProcessData(const OnData: TDataEvent; Buffer: Pointer;
      Size: Integer);
    procedure ActionEncodeLameExecute(Sender: TObject);
    procedure ActionEncodeFaacExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
  private
    { Private declarations }
    Redirector: TRedirector;
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    procedure OnEncodeTerminated(Sender: TObject);
    procedure OnDecodeTerminated(Sender: TObject);
    procedure AddFilesToList(Files: TStrings);
    procedure AddFileToList(const asFilename: string; abSaveFileList: Boolean = true);
    procedure AddFolderToList(asFolder: string);
    procedure AddPlaylistToList(const asFilename: string; abSaveFileList: Boolean);
    procedure UpdateStatusbar;
    procedure SaveFileList;
    procedure LoadFileList;
    procedure ItemDeleted(var Message: TMessage); message WM_ITEM_DELETED;
    procedure WmEncodeNextFile(var Message: TMessage); message WM_ENCODE_NEXT_FILE;
    procedure WmDecodeNextFile(var Message: TMessage); message WM_DECODE_NEXT_FILE;
    procedure WmDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure WmAfterCreate(var Message: TMessage); message WM_AFTER_CREATE;
    procedure WmPassedFromInstance(var Message: TMessage); message WM_PASSED_FROM_INSTANCE;
    procedure WmProgressClosing(var Message: TMessage); message WM_PROGRESS_CLOSING;
    procedure Initialize;
    procedure DisplayCurrentFileStatus(const asMsg: string);
    function EncoderFound: Boolean;
    function CheckExistingFiles(const asExtension: string): Boolean;
    procedure ProcessFiles(aOperation: TProcessOperation);
    function ProcessNextFile(const aOperation: TProcessOperation): Boolean;
    function ProcessFileByInfo(const lsCmd, lsFilename, lsFileFolder: string; const Size: Int64): Boolean;
    function CheckOutputDiffersInput(const asExtension: string): Boolean;
    function GetNextAvailableItemIndex: Integer;
    function GetNumberFilesRemaining: Integer;
    function GetTotalSize: Integer;
    function GetLastSize: Integer;
    function GetRemainingSize: Int64;
    function GetItemStatus(const Item: TListItem): string;
    procedure SetupRedirector(aOperation: TProcessOperation);
    procedure FinishPreview;
    procedure CalculateBatchPercent;
    procedure SetupGlobals;
    function DetermineOutputPath(const asInputFilePath: string): string;
  public
    { Public declarations }
    TrayIcon: TTrayIcon;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses ShellApi, ResStr, UtilFuncs, Options, Lame_Options,
  Progress, About, Log, Shutdown, IniFiles, FileCtrl, Graphics;

var
  ProgressForm: TFormProgress;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnMinimize := AppMinimize;
  Application.OnRestore := AppRestore;
  //-- hints should appear for 10 seconds!
  //-- we do this so the hints in the option dialog can be read!
  Application.HintHidePause := 10 * 1000;

  ProgressForm := nil;

  //-- create controls
  Redirector := TRedirector.Create;
  TrayIcon := TTrayIcon.Create(Self);
  with TrayIcon do
  begin
    //Name := 'TrayIcon';
    Active := False;
    Animate := False;
    ShowDesigning := False;
    MinimizeApp := False;
    OnDblClick := TrayIconDblClick;
    PopupMenu := PopupMenuTray;
  end;

  //-- init controls
  ListViewFiles.FullDrag := true; //-- allow moving of columns {todo: this screws up the headings!?}
  {todo: check how to store that stuff in an ini, and not in the registry!}
  //ListViewFiles.SaveSettings.RegistryKey := ChangeFileExt(LowerCase(Application.ExeName), '.ini');
  //ListViewFiles.LoadSettings;

  //-- set defaults
  with Global do
  begin
    LameEncoder := ExtractFilePath(Application.ExeName) + 'Lame.exe';
    FaacEncoder := '';
    DefaultEncoder := LameEncoder;
    AutoDelete := true;
    LastOpenDir := '';
    ThreadPriority := tpIdle;
    ShutdownFlag := sfShutdown;
    {TODO: check if we should initialize more global stuff}
  end;

  with MP3Settings do
  begin
    Bitrate := 128;
    OutDir := '';
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
  end;

  //-- read options from ini file
  if not ReadOptions then
  begin
    //-- if we ever want to do something on the first time RazorLame
    //-- is started, here would be the right place!
  end;

  //-- if existent, read last logfile
  if FileExists(ExtractFilePath(Application.Exename) + LOG_FILENAME) then
  begin
    try
      Global.Log.LoadFromFile(LOG_FILENAME);
    except
      Global.Log.Clear;
    end;
  end;

  //-- read "system" data. Currently, this includes all the
  //-- "informational" messages for Lame, and the allowed
  //-- input files for Lame.
  ReadSystemData;

  //-- set the filter for the open Dialog
  OpenDialog.Filter := CreateFilterFromStringList(Global.InputFileTypes);

  //-- now initialize the rest
  Initialize;

  //-- everything else that takes some time should take place
  //-- in the WmAfterCreate event!
  PostMessage(Handle, WM_AFTER_CREATE, 0, 0);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //todo: ask if to stop encoding! (alternative: minimize to tray if working!)
  if Redirector.Running then Redirector.Terminate(0); {TODO: Check if 0 is correct code!}
end;

procedure TFormMain.AddFileToList(const asFilename: string; abSaveFileList: Boolean = true);
var
  lsCaption, lsPath, lsType, lsExt: string;
  MyItem: TListItem;
  NewFileItem: TFileItem;
  ldtLen: TDateTime;
  liSize, liBitrate: integer;
begin
  //-- don't add files that don't exist!
  if not FileExists(asFilename) then Exit;

  //-- Determine File-Extension
  lsExt := LowerCase(ExtractFileExt(asFilename));

  //-- Is this a playlist?
  if (lsExt = '.m3u') or (lsExt = '.pls') then
  begin
    AddPlaylistToList(asFilename, abSaveFileList);
    exit;
  end;

  //-- if file extension isn't a supported audio
  //-- type, ignore it!
  if not IsExtensionAllowed(lsExt) then Exit;

  //-- seperate filename and path
  lsCaption := ExtractFilename(asFilename);
  lsPath := ExtractFilePath(asFilename);

  //-- check if this file isn't already in the list
  //-- this takes the most time when adding many files! (ca. 66% time is spent here for 350 files)
  MyItem := ListViewFiles.FindCaption(0, lsCaption, false, true, false);
  while Assigned(MyItem) do
  begin
    if MyItem.SubItems[0] = lsPath then Exit; //-- File already in list!
    //-- continue with the search!
    MyItem := ListViewFiles.FindCaption(MyItem.Index, lsCaption, false, false, false);
  end;

  //-- determine file information
  DetermineFileInfo(asFilename, liSize, liBitrate, ldtLen);

  //-- create and initialize new item
  NewFileItem := TFileItem.Create;
  NewFileItem.Filename := lsCaption;
  NewFileItem.Path := lsPath;
  NewFileItem.Size := liSize;
  NewFileItem.Date := FileDateTime(asFilename);
  NewFileItem.Length := ldtLen;
  NewFileItem.Bitrate := liBitrate;

  with ListViewFiles.Items.Add do
  begin
    //-- Add the file
    Caption := NewFileItem.Filename;
    //-- Add Icon
    ImageIndex := dfsSystemImageList1.GetFileInformation(asFilename, false, false, [], lsType);
    //-- if no type is returned, just give the extionsion of the file as type
    if lsType = '' then lsType := AnsiUpperCase(Copy(ExtractFileExt(asFilename), 2, MaxInt)) + ' file';
    SubItems.Add(NewFileItem.Path);
    SubItems.Add(Format('%.0n', [NewFileItem.Size * 1.0]));
    if NewFileItem.Bitrate > 0 then
      SubItems.Add(IntToStr(NewFileItem.Bitrate))
    else
      if NewFileItem.Bitrate < 0 then
      SubItems.Add(IntToStr(-1 * NewFileItem.Bitrate) + ' (average)')
    else
      SubItems.Add('?');

    if NewFileItem.Length > 1 / 24 then //1/24th of a day....
      SubItems.Add(FormatDateTime('h:nn:ss', NewFileItem.Length))
    else
      if NewFileItem.Length > 10 / (24 * 60) then //10 minutes
      SubItems.Add(FormatDateTime('nn:ss', NewFileItem.Length))
    else
      if NewFileItem.Length > 1 / (24 * 60 * 60) then //1 second
      SubItems.Add(FormatDateTime('n:ss', NewFileItem.Length))
    else
      SubItems.Add(FormatDateTime('n:ss.z', NewFileItem.Length));

    SubItems.Add(FormatDateTime('ddddd t', NewFileItem.Date));
    SubItems.Add(lsType);
    SubItems.Add(ID_WAITING);
    Data := Pointer(NewFileItem);
  end;

  //-- sort order is now probably wrong, so hide the arrows
  ListViewFiles.ShowSortArrows := false;

  //-- Save the new list if wanted
  if abSaveFileList then SaveFileList;
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
    0: StatusBar.Panels[1].Text := STATUS_NOFILES;
    1: StatusBar.Panels[1].Text := STATUS_ONEFILE;
    else
      StatusBar.Panels[1].Text := Format(STATUS_FILES, [ListviewFiles.Items.Count]);
  end;
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
var
  lsMsg: string;
begin
  Result := FileExists(Global.DefaultEncoder);
  if not Result then
  begin
    if Global.DefaultEncoder = Global.LameEncoder then
      lsMsg := MSG_NOENCODERLAME
    else
      lsMsg := MSG_NOENCODERFAAC;

    MyMessageBox(Format(lsMsg, [Global.DefaultEncoder]),
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
    if ListViewFiles.Items[i].Subitems[6] <> ID_WAITING then COntinue;

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

  //-- check if input <> output files
  if Global.DefaultEncoder = Global.FaacEncoder then
  begin
    if not CheckOutputDiffersInput('.aac') then Exit;
  end
  else
    if not CheckOutputDiffersInput('.mp3') then
    Exit;

  //-- check if any mp3 files already exist
  if Global.DefaultEncoder = Global.FaacEncoder then
  begin
    if not CheckExistingFiles('.aac') then Exit;
  end
  else
    if not CheckExistingFiles('.mp3') then
    Exit;

  //-- now process the files!
  ProcessFiles(poEncode);
end;

procedure TFormMain.ActionClearListExecute(Sender: TObject);
//-- Delete all file from the ListView
begin
  ListViewFiles.Items.BeginUpdate;
  try
    ListViewFiles.Items.Clear;
  finally
    ListViewFiles.Items.EndUpdate;
  end;
  UpdateStatusBar;
end;

procedure TFormMain.ActionRequeueExecute(Sender: TObject);
var
  i: Integer;
begin
  with ListViewFiles do
    if SelCount > 0 then
    begin
      Items.BeginUpdate;
      try
        for i := Items.Count - 1 downto 0 do
          if Items[i].Selected then Items[i].SubItems[6] := ID_WAITING; {todo: remove hardcoded index of status!}
      finally
        Items.EndUpdate;
      end;
    end;
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
        for i := Items.Count - 1 downto 0 do
          if Items[i].Selected then Items.Delete(i);
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
  ActionClearList.Enabled := (ListViewFiles.Items.Count > 0) and not Redirector.Running;
  ActionEncode.Enabled := (GetNumberFilesRemaining > 0) and not Redirector.Running;
  ActionDecode.Enabled := (GetNumberFilesRemaining > 0) and not Redirector.Running;
  ActionPreview.Enabled := (not Redirector.Running) and (ListViewFiles.SelCount > 0);
  ActionRequeue.Enabled := ListViewFiles.SelCount > 0;
  ActionRemove.Enabled := ListViewFiles.SelCount > 0;

  ActionLameOptions.Enabled := (not Redirector.Running);

  MenuItemViewToolbarDisplay.Enabled := ActionToolbar.Checked;

  ActionToolbarBoth.Checked := Toolbar.ShowCaptions and Assigned(Toolbar.Images);
  ActionToolbarIcons.Checked := not Toolbar.ShowCaptions and Assigned(Toolbar.Images);
  ActionToolbarCaptions.Checked := Toolbar.ShowCaptions and not Assigned(Toolbar.Images);

  ActionViewLog.Enabled := Global.Log.Count > 0;
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
      begin
        if GetItemStatus(ListViewFiles.Items[i]) = ID_WAITING then
        begin
          MyFileList.Add(Path + Filename);
        end;
      end;
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
  RegisterFormPosition(FORM_SECTION, Self);
  WriteOptions;
end;

procedure TFormMain.ListViewFilesDeletion(Sender: TObject;
  Item: TListItem);
var
  lsStatus: string;
begin
  {todo: move status stuff into TFileItem!}
  lsStatus := GetItemStatus(Item);

  //-- Maybe file is in progress: then we should cancel it.
  if (Pos('%', lsStatus) <> 0) then
    ProgressForm.Close;

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

  StatusBar.Panels[0].Text := 'Ready';
  StatusBar.Update;
end;

procedure TFormMain.AddPlaylistToList(const asFilename: string; abSaveFileList: Boolean);
var
  Strings: TStringList;
  lsExt: string;
  i: Integer;
begin
  Strings := TStringList.Create;
  try
    lsExt := LowerCase(ExtractFileExt(asFilename));
    if lsExt = '.m3u' then
      ParseM3UFile(asFilename, Strings)
    else
      if lsExt = '.pls' then
      ParsePLSFile(asFilename, Strings);

    for i := 0 to Strings.Count - 1 do
      AddFileToList(Strings[i], abSaveFileList);
  finally
    Strings.Free;
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
end;

procedure TFormMain.ProcessLineFAACEncode(const Line: string);
var
  lsPercent: string;
  liPercent, per: Integer;
begin
  ProgressForm.LabelWorking.Caption := 'File: "' + Global.CurrentFile + '"';

  if Copy(Line, 1, 7) = 'Encoder' then
  begin
    ProgressForm.LabelVersion.Caption := 'FAAC ' + Line;
    Exit;
  end;

  if Copy(Line, 1, 7) = 'Bitrate' then
  begin
    ProgressForm.LabelOutput.Caption := ID_OUTPUT + Line;
    Exit;
  end;

  per := Pos('%', Line);
  if per <> 0 then
  begin
    lsPercent := Copy(Line, 1, per - 1);
    if lsPercent = '' then
      Exit;

    liPercent := Trunc(StrToFloat(lsPercent));
    if (liPercent > 0) and (liPercent <= 100) then
    begin
      ProgressForm.LabelStatus.Caption := ID_STATUS + lsPercent + '%';
      Global.FilePercent := liPercent;
      DisplayCurrentFileStatus(IntToStr(liPercent) + '%');
      ProgressForm.ProgressBarStatus.Position
        := Round((Global.FilePercent * ProgressForm.ProgressBarStatus.Max) / 100.0);
    end;
  end;

  CalculateBatchPercent;
end;

procedure TFormMain.CalculateBatchPercent;
var
  ldtElapsedTime, ldtEstimatedTime: TDateTime;
  liCurrentSize: Int64;
begin
  ldtElapsedTime := Now - Global.BatchStart;
  ProgressForm.LabelBatchElapsed.Caption := FormatDateTime('h:nn:ss', ldtElapsedTime);

  liCurrentSize := GetLastSize + ((Global.CurrentItemSize * Global.FilePercent) div 100);
  Global.BatchPercent := (100 * liCurrentSize) div GetTotalSize;
  ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
  ProgressForm.ProgressBarBatch.Position
    := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);

  if liCurrentSize > 0 then
  begin
    ldtEstimatedTime := (ldtElapsedTime * GetTotalSize) / liCurrentSize;
    ProgressForm.LabelBatchEstimated.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime);
    ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime - ldtElapsedTime);
  end;
end;

procedure TFormMain.ProcessLineLAMEEncode(const Line: string);
var
  lsLastStatus: string;
  lsPercent: string;
  liPercent, j: Integer;
  lbFound: boolean;
begin
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
    Exit;
  end;

  if Global.VBRHistogram then
  begin
    Global.Log.Add(Line);
    if Copy(Line, 1, 8) = 'average:' then
      Global.LastLine := lsLastStatus;
    Exit;
  end;

  //-- save Histograms
  if Global.HistoMessages then
  begin

    Global.BRHistogram.Add(Line);
    if Copy(Line, 1, 5) = '320 [' then {todo: histograms don't necessarily need to end with 320!}
    begin
      //-- End Histogram!
      Global.HistoMessages := false;
      ProgressForm.ButtonHistogram.Enabled := true;
      if ProgressForm.ImageHistogram.visible then
        ProgressForm.DrawHistogram;
    end;
    Exit;
  end;

   //-- if StatusMessages is set, we should get only status messages
  if Global.StatusMessages then
  begin
    //-- another special case: 3.87 and up provide the bitrate histogram
    //-- with each progress update!
    if Copy(Line, 1, 5) = ' 32 [' then {todo: histograms don't necessarily need to start with 32!}
    begin
      //-- Start Histogram!
      Global.HistoMessages := true;
      Global.BRHistogram.Clear;
      Global.BRHistogram.Add(Line);
      Exit;
    end;

    if Copy(Line, 1, 8) = 'average:' then
    begin
      Global.Log.Add(Line);
      Exit;
    end;

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
      DisplayCurrentFileStatus(IntToStr(liPercent) + '%');
      ProgressForm.ProgressBarStatus.Position
        := Round((Global.FilePercent * ProgressForm.ProgressBarStatus.Max) / 100.0);
    end;

    if Global.PreviewMode then
    begin
      StatusBar.Panels[0].Text :=
        'Preparing preview: ' + IntToStr(liPercent) + '%';
      UpdateStatusBar;
    end;

    ProgressForm.LabelStatusRemaining.Caption := Trim(Copy(Line, 68, 9));


    //-- Calculate Batch Percent
    CalculateBatchPercent;

     //-- todo: multi-line hint?
    with Global do
      TrayIcon.ToolTip := Format(TRAY_HINT, [CurrentFile, FilePercent, BatchPercent]);

    //-- save the line for summary!
    Global.LastLine := Line;

    //-- and done!
    Exit;
  end;

  //-- if we encounter the first 'Frame' line only status messages follow
  if Copy(Line, 1, 19) = '    Frame          ' then
  begin
    Global.StatusMessages := true;
    Exit;
  end;

  //-- write log file. (Sits here as we don't want to add status-lines!
  Global.Log.Add(Line);

  //-- Check for output format
  if Copy(Line, 1, 12) = 'Encoding as ' then
  begin
    ProgressForm.LabelOutput.Caption := ID_OUTPUT + Trim(Copy(Line, 12, Length(Line)));
    Exit;
  end;

  if Copy(Line, 1, 9) = 'Encoding ' then
  begin
    ProgressForm.LabelWorking.Caption := 'File: "' + Global.CurrentFile + '"';
    //-- Status = 0%
    ProgressForm.LabelStatus.Caption := ID_STATUS + '0%';
    ProgressForm.ProgressBarStatus.Position := 0;
    ProgressForm.LabelStatusRemaining.Caption := '?';
    Exit;
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
  if lbFound then Exit;

  //-- everything we haven't catched yet could be an error!
  Global.ErrorOccurred := true;
{$IFDEF DEBUG}
  Global.Log.Add('^^^ Error Line! ^^^');
{$ENDIF}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //-- destroy controls
  Redirector.Free;
end;

procedure TFormMain.ProcessLineLAMEDecode(const Line: string);
var
  j, liPos1, liPos2, liFrameNo, liFrameTotal, liPercent: Integer;
  lbFound: Boolean;
  lsPercent, lsFrameNo, lsFrameTotal: string;
begin
  //-- if StatusMessages is set, we should get only status messages
  if Global.StatusMessages then
  begin
    //--          1         2         3         4         5         6         7
    //-- 123456789012345678901234567890123456789012345678901234567890123456789012345
    //-- Frame# 5 [ 3674]  128kbs
    //-- Frame#     1/9292     1 kbps (starting with 3.86 beta!)

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

    if Global.CurrentItemFrames = 0 then
      liFrameTotal := StrToIntDef(lsFrameTotal, 0)
    else
      liFrameTotal := Global.CurrentItemFrames;

    if liFrameTotal > 0 then
    begin
      liPercent := liFrameNo * 100 div liFrameTotal;
      if (liPercent > 0) and (liPercent <= 100) then
      begin
        ProgressForm.LabelStatusRemaining.Caption := FormatDateTime('h:nn:ss',
          (Now - Global.FileStart) - ((Now - Global.FileStart) * Global.CurrentItemSize) / (Global.CurrentItemSize * liPercent div 100));
        Global.FilePercent := liPercent;

        lsPercent := IntToStr(Global.FilePercent);
        ProgressForm.LabelStatus.Caption := ID_STATUS + lsPercent + '%';
        ProgressForm.ProgressBarStatus.Position
          := Round((Global.FilePercent * ProgressForm.ProgressBarStatus.Max) / 100.0);

        DisplayCurrentFileStatus(IntToStr(liPercent) + '%');
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

    //-- Calculate Batch Percent
    CalculateBatchPercent;

     //-- todo: multi-line hint?
    with Global do
      TrayIcon.ToolTip := Format(TRAY_HINT, [CurrentFile, FilePercent, BatchPercent]);

    //-- save the line for summary!
    Global.LastLine := Line;

    //-- and done!
    Exit;
  end;

  //-- if we encounter the first 'Frame#' line only status messages follow
  if Copy(Line, 1, 7) = 'Frame# ' then
  begin
    Global.StatusMessages := true;
    Exit;
  end;

  //-- write log file. (Sits here as we don't want to add status-lines!
  Global.Log.Add(Line);

  //-- Check for output format
  if Copy(Line, 1, 8) = 'output: ' then
  begin
    ProgressForm.LabelOutput.Caption := ID_OUTPUT + '.WAV';
    Exit;
  end;

  if Copy(Line, 1, 7) = 'input: ' then
  begin
    ProgressForm.LabelWorking.Caption := 'File: "' + Global.CurrentFile + '"';
    StatusBar.Panels[0].Text := IntToStr(Global.FilesEncoded) + ' decoded, '
      + IntToStr(GetNumberFilesRemaining) + ' remaining';
    UpdateStatusBar;

    //-- Status = 0%
    ProgressForm.LabelStatus.Caption := ID_STATUS + '0%';
    ProgressForm.ProgressBarStatus.Position := 0;
    ProgressForm.LabelStatusRemaining.Caption := '?';
    Exit;
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
  if lbFound then Exit;

  //-- everything we haven't catched yet could be an error!
  Global.ErrorOccurred := true;
{$IFDEF DEBUG}
  Global.Log.Add('^^^ Error Line! ^^^');
{$ENDIF}
end;

procedure TFormMain.CommonProcessData(const OnData: TDataEvent; Buffer: Pointer;
  Size: Integer);
var
  i: Integer;
  ldtDiff: TDateTime;
  lpcLine: PChar;
  lsBuffer, Line: string;
  MyStringList: TStringList;
begin
  if not Assigned(ProgressForm) then exit;

  if not Global.PreviewMode then
  begin
   //-- update status bar
    StatusBar.Panels[0].Text := IntToStr(Global.FilesEncoded) + ' processed, '
      + IntToStr(GetNumberFilesRemaining + 1) + ' remaining';
    StatusBar.Update;
  end;

  if ProgressForm.SkipRequest then
  begin
    //-- kill LAME process, but not RedirectorThread
    Redirector.Terminate(0);
    exit;
  end;

  {update progress only once per second!  if in tray, only once per 10 seconds!!}
  ldtDiff := Now - Global.LastStatusUpdate;
  //-- we have to make sure that we'll get the very last messages of LAME, as there are infos there we'd like to show!
  if not Redirector.LastData and Global.StatusMessages and (ldtDiff < (1 / (24 * 60 * 60))) then  
  begin
{$IFDEF DETAIL_DEBUG}
    Global.Log.Add('Exiting at Time Threshold 1');
{$ENDIF}
    exit;
  end;

  if not Redirector.LastData and Global.StatusMessages and TrayIcon.Active and (ldtDiff < 10 / (24 * 60 * 60)) then
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
      if Assigned(OnData) then OnData(Line);
    end;
  finally
    MyStringList.Free;
  end;
end;

procedure TFormMain.OnEncodeTerminated(Sender: TObject);
var
  lsFile, lsSamples, lsTime, lsCPU, lsSpeed: string;
  ldtElapsedTime, ldtEstimatedTime: TDateTime;
  liCurrentSize: Int64;
begin
  if Assigned(ProgressForm) then
  begin
    //--  if no error occurred and StatusMessages were expected
    //-- and we are in OnTerminate, it looks like all went well!
    if not Global.ErrorOccurred and Global.StatusMessages then
    begin
      if MP3Settings.DeleteFileAfterProcessing then
        DeleteFile(Global.CurrentFileFullname);

      DisplayCurrentFileStatus(ID_DONE);

      //-- print out last BR histogram
      if Global.BRHistogram.Count > 0 then
      begin
        Global.Log.AddStrings(Global.BRHistogram);
      end;

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

      //-- we have to check if this was the last file in the batch,
      //-- because than we have to set the batch to full as well!
      Inc(Global.FilesEncoded);
      if GetNumberFilesRemaining = 0 then
      begin
        ProgressForm.LabelBatch.Caption := ID_BATCH + '100%';
        ProgressForm.ProgressBarBatch.Position := 1000;
        ProgressForm.LabelBatchRemaining.Caption := LABEL_BATCHDONE;
        ProgressForm.Caption := LABEL_ENCODINGFINISHED;
        StatusBar.Panels[0].Text := 'Ready';
      end;
    end;

    if ProgressForm.SkipRequest then
    begin
      ProgressForm.SkipRequest := false;
      ProgressForm.ModalResult := mrOk;
      DisplayCurrentFileStatus(ID_CANCELLED);
      if Global.AutoDelete then
        DeleteFile(Global.CurrentOutputFile);
    end;

    if Global.ErrorOccurred then
    begin
      DisplayCurrentFileStatus(ID_FAILED);
      Global.ErrorOccurredInBatch := true;
      Inc(Global.FilesWithErrors);
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
      or (GetNumberFilesRemaining = 0) or Global.PreviewMode then
    begin

      if (ProgressForm.ModalResult = mrOk) then
      begin
        ProgressForm.ExternalClosed := true;
        ProgressForm.Close;
      end;

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
          Global.Log.Add('One file couldn''t be encoded due to errors, probably wrong format.')
        else
          Global.Log.Add(Format('%d files couldn''t be encoded due to errors, probably wrong format.',
            [Global.FilesWithErrors]));
      end;
      TrayIcon.ToolTip := TRAY_ENCODING_DONE;
      Exit;
    end;

    //-- Update Batch Information
    Global.LastSize := Global.LastSize + Global.CurrentItemSize;
    liCurrentSize := GetLastSize; //already changed status of current file...

    Global.BatchPercent := (100 * liCurrentSize) div GetTotalSize;
    ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
    ProgressForm.ProgressBarBatch.Position
      := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);

    if liCurrentSize > 0 then
    begin
      ldtElapsedTime := Now - Global.BatchStart;
      ldtEstimatedTime := (ldtElapsedTime * GetTotalSize) / liCurrentSize;
      ProgressForm.LabelBatchEstimated.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime);
      ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss', ldtEstimatedTime - ldtElapsedTime);
    end;

    //-- Check if there's more work to do?
    if GetNumberFilesRemaining > 0 then
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
    2:
      begin
        CompResult := TFileItem(Item1.Data).Bitrate - TFileItem(Item2.Data).Bitrate;
      end;
    3: SortAs := saDateTime;
    4: SortAs := saDateTime;
    else
      SortAs := saString;
  end;

end;

procedure TFormMain.SetupRedirector(aOperation: TProcessOperation);

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
    Redirector.OnCommonData := CommonProcessData;

    if Global.DefaultEncoder = Global.LameEncoder then
      Redirector.OnData := ProcessLineLAMEEncode
    else
      Redirector.OnData := ProcessLineFAACEncode;

    Redirector.OnErrorData := Redirector.OnData;
    Redirector.OnTerminated := OnEncodeTerminated;
    Redirector.KillOnDestroy := true;
  end
  else
  begin
    Redirector.OnCommonData := CommonProcessData;
    Redirector.OnData := ProcessLineLAMEDecode;
    Redirector.OnErrorData := ProcessLineLAMEDecode;
    Redirector.OnTerminated := OnDecodeTerminated;
    Redirector.KillOnDestroy := true;
  end;
end;

procedure TFormMain.ProcessFiles(aOperation: TProcessOperation);
begin
  SetupRedirector(aOperation);

  TrayIcon.ToolTip := TRAY_ENCODING_START;
  ProgressForm := TFormProgress.Create(Self);
  ProgressForm.ModalResult := mrOk; //-- signal that everything is fine...

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

  SetupGlobals;

  Global.PreviewMode := False;

  if Global.ShowProgress then
    ProgressForm.Show
  else
    ProgressForm.Hide;

  ProcessNextFile(aOperation);
end;

procedure TFormMain.SetupGlobals;
begin
  Global.ErrorOccurredInBatch := false;
  Global.StopWhenDone := false;
  Global.Log.Clear;
  Global.FilesEncoded := 0;
  Global.FilesWithErrors := 0;
    //-- How many bytes are in the batch?
  Global.LastSize := 0;
  Global.CurrentItemSize := 0;
  Global.BatchPercent := 0;
  Global.BatchStart := Now;
  Global.ExcerptLength := 1;
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
  k: Integer;
begin
  Result := false;
  k := GetNextAvailableItemIndex;

  if k <> -1 then
  begin
    //-- construct command line
    if aOperation = poEncode then
    begin
      if Global.DefaultEncoder = Global.LameEncoder then
        lsCmd := Global.LameEncoder + GetOptionString
          + ' "' + ListViewFiles.Items[k].SubItems[0]
          + ListViewFiles.Items[k].Caption + '" "'
      else
        lsCmd := Global.FaacEncoder + ' -b128 "' + ListViewFiles.Items[k].SubItems[0]
          + ListViewFiles.Items[k].Caption + '" -o"';
    end
    else
      lsCmd := Global.LameEncoder + ' --decode'
        + ' "' + ListViewFiles.Items[k].SubItems[0]
        + ListViewFiles.Items[k].Caption + '" "';

    Global.CurrentOutputFile := DetermineOutputPath(ListViewFiles.Items[k].SubItems[0]);
    if aOperation = poEncode then
    begin
      if Global.DefaultEncoder = Global.FAACEncoder then
        Global.CurrentOutputFile := Copy(Global.CurrentOutputFile, 1, Length(Global.CurrentOutputFile) - 1)
      else
        Global.CurrentOutputFile := Global.CurrentOutputFile + ChangeFileExt(ListViewFiles.Items[k].Caption, '.mp3')
    end
    else
    begin //decode

      Global.CurrentItemFrames := GetNumberFramesVBR(ListViewFiles.Items[k].SubItems[0]
        + ListViewFiles.Items[k].Caption);
      Global.CurrentOutputFile := Global.CurrentOutputFile + ChangeFileExt(ListViewFiles.Items[k].Caption, '.wav');
    end;
    lsCmd := lsCmd + Global.CurrentOutputFile + '"';

    if (aOperation = poEncode) and (Global.DefaultEncoder = Global.FAACEncoder) then
      Global.CurrentOutputFile := Global.CurrentOutputFile + '\' + ChangeFileExt(ListViewFiles.Items[k].Caption, '.aac');

    Result := ProcessFileByInfo(lsCmd,
      ListViewFiles.Items[k].Caption,
      ListViewFiles.Items[k].SubItems[0],
      TFileItem(ListViewFiles.Items[k].Data).Size);

    //THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_IDLE
{    if not SetThreadPriority(Redirector.ThreadID, THREAD_PRIORITY_IDLE)
      then GetSystemErrorMessage(GetLastError);}
  end;
end;

function TFormMain.ProcessFileByInfo(const lsCmd, lsFilename, lsFileFolder: string; const Size: Int64): Boolean;
begin
  //-- reset some global file specific stuff
  with Global do
  begin
    StatusMessages := false;
    VBRHistogram := false;
    HistoMessages := false;
    BRHistogram.Clear;
    ErrorOccurred := false;
    Log.Add('Command: ' + lsCmd);
    FileStart := Now;
    FilePercent := 0;
    LastStatusUpdate := 0;
  end;

  Redirector.CommandLine := lsCmd;
  Redirector.Execute;

  Global.CurrentFileFullname := lsFileFolder + lsFilename;
  Global.CurrentFile := lsFilename;
  Global.CurrentItemSize := Size;

  DisplayCurrentFileStatus(ID_CONNECTING);

  Result := true;
end;


procedure TFormMain.WmDecodeNextFile(var Message: TMessage);
begin
  ProcessNextFile(poDecode);
end;

procedure TFormMain.OnDecodeTerminated(Sender: TObject);
var
  lsFile: string;
  liCurrentSize: Int64;
begin

  if Assigned(ProgressForm) then
  begin
    //--  if no error occurred and StatusMessages were expected
    //-- and we are in OnTerminate, it looks like all went well!
    if not Global.ErrorOccurred and Global.StatusMessages then
    begin
      if MP3Settings.DeleteFileAfterProcessing then
        DeleteFile(Global.CurrentFileFullname);

      DisplayCurrentFileStatus(ID_DONE);
      //-- we have to check if this was the last file in the batch,
      //-- because than we have to set the batch to full as well!
      Inc(Global.FilesEncoded);
      if GetNumberFilesRemaining = 0 then
      begin
        ProgressForm.LabelBatch.Caption := ID_BATCH + '100%';
        ProgressForm.ProgressBarBatch.Position := 1000;
        ProgressForm.LabelBatchRemaining.Caption := LABEL_BATCHDONE;
        ProgressForm.Caption := LABEL_ENCODINGFINISHED;
        StatusBar.Panels[0].Text := 'Ready';
      end;
    end;

    if ProgressForm.SkipRequest then
    begin
      ProgressForm.SkipRequest := false;
      ProgressForm.ModalResult := mrOk;
      DisplayCurrentFileStatus(ID_CANCELLED);
      if Global.AutoDelete then
        DeleteFile(Global.CurrentOutputFile);
    end;

    if Global.ErrorOccurred then
    begin
      DisplayCurrentFileStatus(ID_FAILED);
      Global.ErrorOccurredInBatch := true;
      Inc(Global.FilesWithErrors);
      //-- Delete 0 byte files after an error occurred
      lsFile := DetermineOutputPath(ExtractFilePath(Global.CurrentFileFullname))
        + ChangeFileExt(ExtractFilename(Global.CurrentFileFullname), '.wav');
      DeleteZeroByteFile(lsFile);

      //-- add a note about the error
      Global.Log.Add(Format('An error occurred while trying to decode "%s"!',
        [Global.CurrentFileFullname]));
    end;

    //-- should we stop with the last file?
    //-- or are we through with the batch?
    if Global.StopWhenDone
      or (GetNumberFilesRemaining = 0) then
    begin

      if ProgressForm.ModalResult = mrOk then
      begin
        ProgressForm.ExternalClosed := true;
        ProgressForm.Close;
      end;

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
          Global.Log.Add('One file couldn''t be decoded due to errors, probably wrong format.')
        else
          Global.Log.Add(Format('%d files couldn''t be decoded due to errors, probably wrong format.',
            [Global.FilesWithErrors]));
      end;
      TrayIcon.ToolTip := 'Finished decoding';
      Exit;
    end;

    //-- Update Batch Information
    Global.LastSize := Global.LastSize + Global.CurrentItemSize;
    liCurrentSize := GetLastSize;

    Global.BatchPercent := (100 * liCurrentSize) div GetTotalSize;
    ProgressForm.LabelBatch.Caption := ID_BATCH + IntToStr(Global.BatchPercent) + '%';
    ProgressForm.ProgressBarBatch.Position
      := Round((Global.BatchPercent * ProgressForm.ProgressBarBatch.Max) / 100.0);

    if liCurrentSize > 0 then
      ProgressForm.LabelBatchRemaining.Caption := FormatDateTime('h:nn:ss',
        (Now - Global.BatchStart) - ((Now - Global.BatchStart) * GetTotalSize) / liCurrentSize);

    //-- Check if there's more work to do?
    if GetNumberFilesRemaining > 0 then
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
    PChar('mailto:Razor.Blade@gmx.net?subject='
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
    if ListViewFiles.Items[i].Subitems[6] <> ID_WAITING then Continue;

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
  ShellExecute(HInstance, nil, 'RazorLame.html', nil, nil, SW_SHOW);
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
  //-- if we have a command line, process it!
  if ParamCount > 0 then
    PassToOtherInstance(handle);

  if Global.ShowProgress then
    ToolButtonShowProgress.Down := true;

  StatusBar.Panels[0].Text := 'Ready';
  StatusBar.Panels[2].Text := 'LAME Options: ' + GetOptionString;
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
    end;
  finally
    Free;
  end;

  StatusBar.Panels[2].Text := 'LAME Options: ' + GetOptionString;
end;


//called when Progress dialog closes.
//lbNoCancel is false precisley when the user
//pressed the cancel button.

procedure TFormMain.WmProgressClosing(var Message: TMessage);
var
  lbNoCancel: boolean;
  i: Integer;
begin
  if Global.PreviewMode then
    FinishPreview;

  lbNoCancel := (Message.WParam = 1);

  if not lbNoCancel then DisplayCurrentFileStatus(ID_WAITING);

  StatusBar.Panels[0].Text := 'Ready';
  StatusBar.Update;

  try
    if Redirector.Running then Redirector.Terminate(0);

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
            Global.Log.SaveToFile(ExtractFilePath(Application.Exename) + LOG_FILENAME);
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
      Global.Log.SaveToFile(ExtractFilePath(Application.Exename) + LOG_FILENAME);
      //-- perhaps we should shutdown windows?
      if ProgressForm.CheckBoxShutdown.Checked and lbNoCancel then
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

    if not lbNoCancel then
    begin
 //      if Redirector.Running then
 //      Application.MessageBox(PChar('running'),'Error',MB_OK);
      for i := 0 to ListViewFiles.Items.Count - 1 do
      begin
        if (GetItemStatus(ListViewFiles.Items[i]) = ID_WAITING)
          or (Pos('%', GetItemStatus(ListViewFiles.Items[i])) <> 0) then
          ListViewFiles.Items[i].Subitems[6] := ID_CANCELLED;
      end;
      if Global.AutoDelete then
        DeleteFile(Global.CurrentOutputFile);
    end;

  finally
    ProgressForm.Free;
    ProgressForm := nil;
  end;

  SaveFileList;
end;


procedure TFormMain.DisplayCurrentFileStatus(const asMsg: string);
var
  lsCaption, lsPath, lsFile: string;
  MyItem: TListItem;
begin
  lsFile := Global.CurrentFileFullName;
  lsCaption := ExtractFilename(lsFile);
  lsPath := ExtractFilePath(lsFile);

  //-- search for this file
  MyItem := ListViewFiles.FindCaption(0, lsCaption, false, true, false);
  while Assigned(MyItem) do
  begin
    if MyItem.SubItems[0] = lsPath then
    begin
      Break; //-- File found!
    end;
    //-- continue with the search!
    MyItem := ListViewFiles.FindCaption(MyItem.Index, lsCaption, false, false, false);
  end;

  if Assigned(MyItem) then
    MyItem.SubItems[6] := asMsg;
end;


procedure TFormMain.WmPassedFromInstance(var Message: TMessage);
var
  lsCmdline: string;
  liLength: Integer;
  at: atom;
  Buffer: array[1..1024] of char;
begin
  Application.Restore;
  Application.BringToFront;

  at := atom(Message.LParam);

  liLength := GlobalGetAtomName(at, @Buffer, 1024);
  lsCmdLine := Buffer;
  SetLength(lsCmdline, liLength);
  GlobalDeleteAtom(at);

  PushCursor;
  try
    ListViewFiles.Items.BeginUpdate;
    try
      if DirectoryExists(lsCmdline) then
        AddFolderToList(lsCmdline)
      else
        AddFileToList(lsCmdLine, false);
    finally
      ListViewFiles.Items.EndUpdate;
    end;
  finally
    SaveFileList;
    UpdateStatusbar;
    PopCursor;
  end;
  Message.Result := 0;
end;

procedure TFormMain.SuspendLame(suspend: boolean);
begin
  Redirector.Suspend(suspend);
end;

function TFormMain.GetNextAvailableItemIndex: Integer;
var
  i: Integer;
begin
  //-- return value if _nothing_ is waiting....
  Result := -1;
  for i := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if GetItemStatus(ListViewFiles.Items[i]) = ID_WAITING then
    begin
      Result := I;
      exit;
    end;
  end;
end;


function TFormMain.GetNumberFilesRemaining: integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ListViewFiles.Items.Count - 1 do
    if GetItemStatus(ListViewFiles.Items[I]) = ID_WAITING then
      Inc(Result);
end;

function TFormMain.GetRemainingSize: Int64;
var
  Item: TListItem;
  i: Integer;
begin
  Result := 0;
  for i := 0 to ListViewFiles.Items.Count - 1 do
  begin
    Item := ListViewFiles.Items[i];
    if GetItemStatus(Item) = ID_WAITING then
      Inc(Result, GetFileSize(Item.Subitems[0] + Item.Caption)); //-- todo: get file size from TFileItem
  end;
end;

//use this: just in case we change the column ordering...

function TFormMain.GetItemStatus(const item: TListItem): string;
begin
  Result := item.SubItems[6];
end;

function TFormMain.GetTotalSize: Integer;
begin
  Result := Global.LastSize + Global.CurrentItemSize + GetRemainingSize;
end;

function TFormMain.GetLastSize: Integer;
begin
  Result := Global.LastSize;
end;

procedure TFormMain.ToolButtonShowProgressClick(Sender: TObject);
begin
  Global.ShowProgress := ToolButtonShowProgress.Down;

  if Assigned(ProgressForm) then
    ProgressForm.Visible := Global.ShowProgress;
end;

procedure TFormMain.FinishPreview;
begin
  StatusBar.Panels[0].Text := 'Ready';

  ShellExecute(GetActiveWindow, 'open', PChar(Global.CurrentOutputFile), nil, nil, SW_SHOW);
  if Application.MessageBox('Click ''Yes'' to delete the temporary files (after you''ve finished listening)...'#13#10 +
    '                           or ''No'' to leave the files alone.',
    'Delete temp files?', MB_YESNO) = IDYES then
  begin
    if not DeleteFile(Global.CurrentFileFullName) or
      not DeleteFile(ChangeFileExt(Global.CurrentFileFullName, '.mp3')) then
      Application.MessageBox('Sorry - the temporary files couldn''t be deleted.'#13#10 +
        'One was probably in use, or already deleted...', 'Problem', MB_OK);
  end;
  Global.PreviewMode := false;
end;

procedure TFormMain.ActionPreviewExecute(Sender: TObject);
var
  wav: TWAVFile;
  lsExcerptPath, lsOrigPath, lsCmd: string;
  Item: TListItem;
  lfStart, lfStop: real;
begin
   //-- No Encoder, no encode!
  if not EncoderFound then Exit;

  lfStart := 0;
  lfStop := 0;

  if ListViewFiles.SelCount = 0 then
  begin
    Application.MessageBox('Please select a file to preview...', 'Message', MB_OK);
    Exit;
  end;

  Item := ListViewFiles.Selected;

  Global.PreviewMode := true;

  lsExcerptPath := Item.SubItems[0] + 'RLTemp_' + Item.Caption;
  lsOrigPath := Item.SubItems[0] + Item.Caption;

  wav := TWavFile.Create(lsOrigPath);
  try
    case Global.ExcerptPosition of
      0:
        begin
          lfStart := 0;
          lfStop := Global.ExcerptLength;
        end;
      1:
        begin
          lfStart := wav.Duration / 2 - Global.ExcerptLength / 2;
          lfStop := wav.Duration / 2 + Global.ExcerptLength / 2;
        end;
      2:
        begin
          lfStart := wav.Duration - Global.ExcerptLength;
          lfStop := wav.Duration;
        end;
    end;
    wav.SaveExcerpt(lsExcerptPath, lfStart, lfStop);
  finally
    wav.free;
  end;

  SetupGlobals;
  SetupRedirector(poEncode);
  ProgressForm := TFormProgress.Create(Self);
  ProgressForm.ModalResult := mrOk;

  if Global.DefaultEncoder = Global.LameEncoder then
    lsCmd := Global.LameEncoder + GetOptionString
      + ' "' + lsExcerptPath + '" "' + ChangeFileExt(lsExcerptPath, '.mp3') + '"'
  else
    lsCmd := Global.FaacEncoder + GetOptionString
      + ' "' + lsExcerptPath + '" -o"' + ChangeFileExt(lsExcerptPath, '.mp3') + '"';

  Global.CurrentOutputFile := ChangeFileExt(lsExcerptPath, '.mp3');

  ProcessFileByInfo(lsCmd, ExtractFilename(lsExcerptPath), ExtractFileDir(lsExcerptPath) + '\', GetFileSize(lsExcerptPath));
end;

procedure TFormMain.ActionEncodeLameExecute(Sender: TObject);
begin
  Global.DefaultEncoder := Global.LameEncoder;
  ActionEncodeExecute(Self);
end;

procedure TFormMain.ActionEncodeFaacExecute(Sender: TObject);
begin
  Global.DefaultEncoder := Global.FaacEncoder;
  ActionEncodeExecute(Self);
end;

procedure TFormMain.ActionSelectAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListViewFiles.Items.Count - 1 do
    ListViewFiles.Items[i].Selected := true;

  ListViewFiles.Items[ListViewFiles.Items.Count - 1].Focused := true;
end;

function TFormMain.DetermineOutputPath(const asInputFilePath: string): string;
begin
  Result := asInputFilePath;
  if MP3Settings.UseInputDir then exit;
  if (Trim(MP3Settings.OutDir) <> '') and DirectoryExists(MP3Settings.OutDir) then
    Result := MP3Settings.OutDir
end;

end.

