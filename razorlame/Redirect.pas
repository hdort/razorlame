unit Redirect;
{
  I've no idea who originally created this unit.
  I've taken what I've found and have done some small enhancements to it so
  it fits my needs.

  Holger Dors, holger@dors.de, March 2000

  2000-03-18: added some comments, checked pipes a last time after process
              finishes to "clean up" properly. (Holger Dors)
  2000-06-25: InitialPriority was never really used! (Holger Dors)
  2001-03-03: Added "LastData" property (Holger Dors)
  2001-09-01: make sure that pipes are read until they are empty after process 
              has finished (Holger Dors)

  to do: check if the last two changes are really necessary!
  Those were done in order to get the "sometimes missing average problem"
  solved, however, I think I've solved this now with a change in main.pas,
  so perhaps those last changes aren't really necessary at all!!

  2005-02-27: added "sleep(10)" in TRedirectorThread.Execute thread. Should
              speed up processing when called with anything other than
              SW_SHOWNORMAL.
              Thanks to Jos Wolters for pointing this out!

}

interface

uses
  Windows, SysUtils, Classes;

type
  TRedirector = class;
  TPriorityClass = (pcDefault, pcIdle, pcNormal, pcHigh, pcRealtime);
  TDataEvent = procedure(Sender: TRedirector; Buffer: Pointer; Size: Integer) of object;



  TPipeError = record
    hRead: DWORD;
    hWrite: DWORD;
  end;

  TRedirector = class{$IFDEF COMPONENT}(TComponent){$ENDIF}
  private
    FAvailable: Integer;
    procedure ReadStdOutput;
    procedure ReadStdError;
    procedure ProcessTerminated;
  protected
    FProcessInfo: TProcessInformation;
    FExitCode: Integer;
    FExecutable: string;
    FCommandline: string;
    FDefaultErrorMode: Boolean;
    FStartSuspended: Boolean;
    FKillOnDestroy: Boolean;
    FDirectory: string;
    FEnvironment: Pointer;
    FInitialPriority: TPriorityClass;
    FPipeInput: TPipeError;
    FPipeOutput: TPipeError;
    FPipeError: TPipeError;
    FThread: TThread;
    FOnData: TDataEvent;

    FOnErrorData: TDataEvent;
    FOnTerminated: TNotifyEvent;
    FShowWindow: Integer;
    FLastData: Boolean;
    procedure Error(msg: string);
    procedure WinError(msg: string);
    procedure CreatePipes;
    procedure ClosePipes;
    function GetRunning: Boolean;
    function GetExitCode: Integer;
    function GetProcessID: Integer;
    function GetThreadID: Integer;
    function GetProcessHandle: Integer;
    procedure SetShowWindow(Value: Integer);
    function GetThreadHandle: Integer;
    procedure SetExecutable(Value: string);
    function GetCommandLine: string;
    procedure SetCommandLine(Value: string);
    procedure SetDefaultErrorMode(Value: Boolean);
    procedure SetStartSuspended(Value: Boolean);
    procedure SetInitialPriority(Value: TPriorityClass);
    procedure SetDirectory(Value: string);
    procedure SetEnvironment(Value: Pointer);
    property ProcessHandle: Integer read GetProcessHandle;
    property ThreadHandle: Integer read GetThreadHandle;
  public
    destructor Destroy; override;
    procedure Terminate(dwExitCode: Integer);
    procedure Execute;

    procedure SendData(Buffer: Pointer; BufferSize: Integer);
    procedure SendText(s: string);
    property Running: Boolean read GetRunning;
    property ExitCode: Integer read GetExitCode;
    property ProcessID: Integer read GetProcessID;
    property ThreadID: Integer read GetThreadID;
    property Environment: Pointer read FEnvironment write SetEnvironment;
    property LastData: boolean read FLastData;
  published
    property KillOnDestroy: Boolean read FKillOnDestroy write FKillOnDestroy;
    property Executable: string read FExecutable write SetExecutable;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property ShowWindow: Integer read FShowWindow write SetShowWindow default SW_SHOWDEFAULT;
    property DefaultErrorMode: Boolean read FDefaultErrorMode write SetDefaultErrorMode;
    property StartSuspended: Boolean read FStartSuspended write SetStartSuspended;
    property InitialPriority: TPriorityClass read FInitialPriority write SetInitialPriority;
    property Directory: string read FDirectory write SetDirectory;
    property OnData: TDataEvent read FOnData write FOnData;

    property OnErrorData: TDataEvent read FOnErrorData write FOnErrorData;
    property OnTerminated: TNotifyEvent read FOnTerminated write FOnTerminated;
  end;

implementation

const
  DUPLICATE_CLOSE_SOURCE = 1;
  DUPLICATE_SAME_ACCESS = 2;

type
  TRedirectorThread = class(TThread)
  protected
    FRedirector: TRedirector;
    procedure Execute; override;
    constructor Create(ARedirector: TRedirector);
  end;

////////////////////////////////////////////////////////////////////////////////
// Misc. internal methods
////////////////////////////////////////////////////////////////////////////////

procedure TRedirector.Error(msg: string);
begin
  raise Exception.Create(msg);
  TerminateProcess(ProcessHandle, 0)
end;

procedure TRedirector.WinError(msg: string);
begin
  Error(msg + IntToStr(GetLastError));
end;

procedure TRedirector.CreatePipes;
var
  SecAttr: TSecurityAttributes;
const
  PIPE_SIZE = 0; //--was: 1024;
begin
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle := true;

  with FPipeInput do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, PIPE_SIZE)
      then WinError('Error on STDIN pipe creation : ');
    if not DuplicateHandle(GetCurrentProcess, hRead, GetCurrentProcess,
      @hRead, 0, true, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS)
      then WinError('Error on STDIN pipe duplication : ');
  end;
  with FPipeOutput do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, PIPE_SIZE)
      then WinError('Error on STDOUT pipe creation : ');
    if not DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess,
      @hWrite, 0, true, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS)
      then WinError('Error on STDOUT pipe duplication : ');
  end;
  with FPipeError do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, PIPE_SIZE)
      then WinError('Error on STDERR pipe creation : ');
    if not DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess,
      @hWrite, 0, true, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS)
      then WinError('Error on STDERR pipe duplication : ');
  end;
end;

procedure TRedirector.ClosePipes;
begin
  with FPipeInput do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
  with FPipeOutput do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
  with FPipeError do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Property implementations
////////////////////////////////////////////////////////////////////////////////

function TRedirector.GetRunning: Boolean;
begin
  Result := ProcessHandle <> 0;
end;

function TRedirector.GetExitCode: Integer;
begin
  if Running then
    Result := STILL_ACTIVE
  else
    Result := FExitCode;
end;

function TRedirector.GetProcessID: Integer;
begin
  Result := FProcessInfo.dwProcessID;
end;

function TRedirector.GetThreadID: Integer;
begin
  Result := FProcessInfo.dwThreadID;
end;

function TRedirector.GetProcessHandle: Integer;
begin
  Result := FProcessInfo.hProcess;
end;

function TRedirector.GetThreadHandle: Integer;
begin
  Result := FProcessInfo.hThread;
end;

procedure TRedirector.SetExecutable(Value: string);
begin
  if (ANSICompareText(Value, Executable) = 0) or not Running
    then
    FExecutable := Value
  else if Running
    then
    Error('Cannot change Executable while process is active');
end;

procedure TRedirector.SetCommandLine(Value: string);
begin
  if (ANSICompareText(Value, Commandline) = 0) or not Running
    then
    FCommandline := Value
  else if Running
    then
    Error('Cannot change Commandline while process is active');
end;

function TRedirector.GetCommandLine: string;
begin
  Result := FExecutable;
  if Result = ''
    then
    Result := FCommandline
  else
    Result := FExecutable + ' ' + FCommandline;
end;
{
procedure TRedirector.XSetCommandLine (value : string);
var
  n1,
  n2      : integer;
begin
  if (ANSICompareText (value, CommandLine) = 0) or (not Running) then begin
    n1 := Length(value)+1;
    n2 := Pos(' ', value);
    if (n2>0) and (n2<n1) then n1 := n2;
    n2 := Pos('-', value);
    if (n2>0) and (n2<n1) then n1 := n2;
    n2 := Pos('/', value);
    if (n2>0) and (n2<n1) then n1 := n2;
    FExecutable := Copy (value, 1, n1-1);
    //FCommandline := Copy (value, 1, n1-1);
   // FParameters := Trim(Copy (value, Length(FExecutable)+1, Length(value)));
    FCommandline := Trim(Copy (value, Length(FExecutable)+1, Length(value)));
  end else if Running then Error('Cannot change CommandLine while process is active');
end;
}

procedure TRedirector.SetDefaultErrorMode(Value: Boolean);
begin
  if (Value = DefaultErrorMode) or not Running then
    FDefaultErrorMode := Value
  else if Running then

    Error('Cannot change DefaultErrorMode while process is active');
end;

procedure TRedirector.SetStartSuspended(Value: Boolean);
begin
  if (Value = DefaultErrorMode) or not Running then
    FStartSuspended := Value
  else if Running then

    Error('Cannot change StartSuspended while process is active');
end;

procedure TRedirector.SetInitialPriority(Value: TPriorityClass);
begin
  if (Value <> InitialPriority) and not Running then
    FInitialPriority := Value
  else if Running then

    Error('Cannot change InititalPriority while process is active');
end;

procedure TRedirector.SetDirectory(Value: string);
begin
  if (ANSICompareText(Value, Directory) = 0) or (not Running) then
    FDirectory := Value
  else if Running then

    Error('Cannot change Directory while process is active');
end;

procedure TRedirector.SetEnvironment(Value: Pointer);
begin
  if (Value = Environment) or not Running then
    FEnvironment := Value
  else if Running then

    Error('Cannot change Environment while process is active');
end;

procedure TRedirector.SetShowWindow(Value: Integer);
begin
  if (Value = ShowWindow) or not Running then
    FShowWindow := Value
  else if Running then

    Error('Cannot change ShowWindow while process is active');
end;

procedure TRedirector.ReadStdOutput;
var
  BytesRead: DWord;
  Buffer: Pointer;
begin
  GetMem(Buffer, FAvailable);
  try
    if not ReadFile(FPipeOutput.hRead, Buffer^, FAvailable, BytesRead, nil) then
    begin
      FThread.Terminate;
      WinError('Error reading STDOUT pipe : ');
    end;
    if Assigned(FOnData) then
      FOnData(Self, Buffer, BytesRead);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TRedirector.ReadStdError;
var
  BytesRead: DWord;
  Buffer: Pointer;
begin
  GetMem(Buffer, FAvailable);
  try
    if not ReadFile(FPipeError.hRead, Buffer^, FAvailable, BytesRead, nil) then
    begin
      FThread.Terminate;
      WinError('Error reading STDERR pipe : ');
    end;
    if Assigned(FOnErrorData) then
      FOnErrorData(Self, Buffer, BytesRead);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TRedirector.ProcessTerminated;
begin
  FThread.Terminate;
  ClosePipes;
  CloseHandle(FProcessInfo.hProcess);
  CloseHandle(FProcessInfo.hThread);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  if Assigned(FOnTerminated) then FOnTerminated(Self);  //-- moved from 2nd line to last line! (hdo, 2001-09-02)
end;

////////////////////////////////////////////////////////////////////////////////
// Public methods
////////////////////////////////////////////////////////////////////////////////

procedure TRedirector.Terminate(dwExitCode: Integer);
begin
  if Running
    then
    TerminateProcess(ProcessHandle, dwExitCode)
  else
    Error('Cannot Terminate an inactive process');
end;

procedure TRedirector.Execute;
var
  StartupInfo: TStartupInfo;
  szExecutable, szCommandline, szDirectory: PChar;
  liPriorityClass: Integer;
begin
  if Running then Error('Process is already active');
  if Trim(CommandLine) = '' then Error('No commandline to run');
  FLastData := false;
  try
    CreatePipes;

    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);

    StartupInfo.wShowWindow := FShowWindow;
    StartupInfo.hStdInput := FPipeInput.hRead;
    StartupInfo.hStdOutput := FPipeOutput.hWrite;
    StartupInfo.hStdError := FPipeError.hWrite;
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

    if Trim(Executable) = '' then
      szExecutable := nil
    else
      szExecutable := PChar(FExecutable);
    if Trim(Commandline) = '' then
      szCommandline := nil
    else
      szCommandline := PChar(FCommandline);
    if Trim(Directory) = '' then
      szDirectory := nil
    else
      szDirectory := PChar(FDirectory);

    liPriorityClass := 0;
    case FInitialPriority of
      pcIdle: liPriorityClass := IDLE_PRIORITY_CLASS;
      pcNormal: liPriorityClass := NORMAL_PRIORITY_CLASS;
      pcHigh: liPriorityClass := HIGH_PRIORITY_CLASS;
      pcRealtime: liPriorityClass := REALTIME_PRIORITY_CLASS;
    end;

    if CreateProcess(szExecutable, szCommandline, nil, nil, true,
      (CREATE_DEFAULT_ERROR_MODE and Integer(FDefaultErrorMode))
      or (CREATE_SUSPENDED and Integer(FStartSuspended) or liPriorityClass),
      Environment, szDirectory, StartupInfo, FProcessInfo)
      then
    begin
      FThread := TRedirectorThread.Create(Self);
    end
    else
      WinError('Error creating process : ');
  except
    on Exception do
    begin
      ClosePipes;
      CloseHandle(FProcessInfo.hProcess);
      CloseHandle(FProcessInfo.hThread);
      FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
      raise;
    end;
  end;
end;

procedure TRedirector.SendData(Buffer: Pointer; BufferSize: Integer);
var
  BytesWritten: DWord;
begin
  if not Running then Error('Can''t send data to an inactive process');
  if not WriteFile(FPipeInput.hWrite, Buffer^, BufferSize, BytesWritten, nil)
    then WinError('Error writing to STDIN pipe : ');
end;

procedure TRedirector.SendText(s: string);
begin
  SendData(PChar(s), Length(s));
end;

destructor TRedirector.Destroy;
begin
  if Running and KillOnDestroy then
  begin
    FOnTerminated := nil;
    FThread.Terminate;
    Terminate(0);
  end;
  inherited Destroy;
end;

constructor TRedirectorThread.Create(ARedirector: TRedirector);
begin
  FRedirector := ARedirector;
  inherited Create(false);
end;

procedure TRedirectorThread.Execute;
var
  Idle: Boolean;
begin
  FreeOnTerminate := true;
  repeat
    Idle := true;
    Sleep(10);

    //-- check for StdOutout-Pipe
    if PeekNamedPipe(FRedirector.FPipeOutput.hRead, nil, 0, nil,
      @FRedirector.FAvailable, nil) and (FRedirector.FAvailable > 0) then
    begin
      Synchronize(FRedirector.ReadStdOutput);
      Idle := false;
    end;

    //-- check for StdErr-Pipe
    if PeekNamedPipe(FRedirector.FPipeError.hRead, nil, 0, nil,
      @FRedirector.FAvailable, nil) and (FRedirector.FAvailable > 0) then
    begin
      Synchronize(FRedirector.ReadStdError);
      Idle := false;
    end;

    //-- check if process terminated
    if Idle and (WaitForSingleObject(FRedirector.ProcessHandle,
      100) = WAIT_OBJECT_0) then
    begin
      FRedirector.FLastData := true;

      //-- process has finished, read pipes until they are empty
      repeat
        Idle := true;

        //-- check for StdOutout-Pipe a last time
        if PeekNamedPipe(FRedirector.FPipeOutput.hRead, nil, 0, nil,
          @FRedirector.FAvailable, nil) and (FRedirector.FAvailable > 0) then
        begin
          Synchronize(FRedirector.ReadStdOutput);
          Idle := false;
        end;

        //-- check for StdErr-Pipe a last time
        if PeekNamedPipe(FRedirector.FPipeError.hRead, nil, 0, nil,
          @FRedirector.FAvailable, nil) and (FRedirector.FAvailable > 0) then
        begin
          Synchronize(FRedirector.ReadStdError);
          Idle := false;
        end;

      until Idle;

      //-- self-destruct
      if not Terminated then Synchronize(FRedirector.ProcessTerminated);
    end;

  until Terminated and Idle;
end;

end.
