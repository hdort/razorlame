(* (c) Copyright 2000 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

unit ResStr;

interface

resourcestring
  ID_ENCODE = 'Encode';
  ID_CANCEL = 'Cancel';
  ID_STATUS = 'Status=';
  ID_BATCH = 'Batch=';
  ID_WORKING = 'Working on file %d of %d: "%s"';
  ID_OUTPUT = 'Output format: ';
  ID_ENC_IN_PROGRESS = 'Encoding in progress';
  ID_DEC_IN_PROGRESS = 'Decoding in progress';
  ID_STOP_ENC = 'Stopping encoding when done with current file...';
  ID_STOP_DEC = 'Stopping decoding when done with current file...';

  MSG_NOFILES = 'There are no files to encode!';
  MSG_NOENCODERLAME = 'The file "%s" couldn''t be found.'#13#10'Please specify in the following options dialog the correct location of LAME.';
{.$IFDEF USE_FAAC}
  MSG_NOENCODERFAAC = 'The file "%s" couldn''t be found.'#13#10'Please specify in the following options dialog the correct location of FAAC.';
{.$ENDIF}
  MSG_ANERROROCCURRED = 'At least one error occurred. Do you want to view the log?';
  MSG_ABORTENCODING = 'Do you want to stop encoding now or stop encoding when the currently processed file is done?';
  MSG_DIFFERENTENCODER = 'The file you''ve selected isn''t named "Lame.exe".'#13#10'Are you sure that you''ve selected the right file?';
  MSG_RL_IS_BUSY = 'RazorLame is currently busy. No files can be added currently, sorry.';

  BTN_NOW = 'Stop now';
  BTN_DONE = 'When done';

  MSG_HINT = 'Hint';
  MSG_ERROR = 'Error';
  MSG_QUESTION = 'Question';
  MSG_INFO = 'Information';

  STATUS_NOFILES = 'No files';
  STATUS_ONEFILE = 'One file';
  STATUS_FILES = '%d files';

  TRAY_ENCODING_DONE = 'Finished encoding';
  TRAY_ENCODING_START = 'Starting...';
  TRAY_MENU_STOPENCODING = 'Stop encoding';
  TRAY_MENU_EXIT = 'Exit RazorLame';
  TRAY_HINT = '%s: %d%%, Batch: %d%%';

  LOG_MSG_TIMEOUT = 'Encoder timed out.';
  LOG_ERROR = 'Error: ';

  LABEL_COMPLETED = 'Completed in ';
  LABEL_BATCHDONE = 'Batch done';
  LABEL_ENCODINGFINISHED = 'Encoding finished';

  LABEL_CURRENT_BITRATE = 'Current bitrate: %s kbit';
  //LABEL_VBR_MAX_BITRATE = 'Maximum VBR Bitrate: %s kbit';
  LABEL_VBR_MAX_BITRATE = 'Current bitrate: %s kbit';

  IDLE_PRIORITY = 'Idle';
  LOWEST_PRIORITY = 'Lowest';
  LOWER_PRIORITY = 'Lower';
  NORMAL_PRIORITY = 'Normal';
  HIGHER_PRIORITY = 'Higher';
  HIGHEST_PRIORITY = 'Highest';

  LABEL_TIMEOUT = 'Time left: %d';

  ID_WAITING = 'Queued';
  ID_DONE = 'Done';
  ID_FAILED = 'Failed';
  ID_CANCELLED = 'Cancelled';
  ID_CONNECTING = 'Starting';

implementation

end.

