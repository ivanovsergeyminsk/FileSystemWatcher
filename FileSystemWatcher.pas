unit FileSystemWatcher;

interface

uses
  System.SysUtils,
  System.Threading,
  Winapi.Windows;

type
  WatcherChangeType =
    (
      Created = 1,
      Deleted = 2,
      Changed = 4,
      Renamed = 8,
      All     = 15
     );

  TNotifyFilter =
    (
      FileName,
      DirectoryName,
      Attributes,
      Size,
      LastWrite,
      LastAccess,
      CreationTime,
      Security
    );

  NotifyFilters = set of TNotifyFilter;

  TNotifyFileSystemWatcher = procedure(Sender: TObject; ChangeType: WatcherChangeType; Directory, Name: string) of object;
  TNotifyFileSystemRenamed = procedure(Sender: TObject; ChangeType: WatcherChangeType; Directory, Name, OldName: string) of object;

  TFileSystemWatcher = class
  private type
  FILE_NOTIFY_INFORMATION = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array [0 .. 0] of WCHAR;
  end;
  private
    FPath: string;
    FFilter: string;

    FWatcherTask: ITask;
    FTermEvent: THandle;
  private
    FOnChanged: TNotifyFileSystemWatcher;
    FOnDeleted: TNotifyFileSystemWatcher;
    FOnCreated: TNotifyFileSystemWatcher;
    FOnRenamed: TNotifyFileSystemRenamed;
    FNotifyFilter: NotifyFilters;
    FIncludeSubDirectories: boolean;
    procedure SetOnChanged(const Value: TNotifyFileSystemWatcher);
    procedure SetOnCreated(const Value: TNotifyFileSystemWatcher);
    procedure SetOnDeleted(const Value: TNotifyFileSystemWatcher);
    procedure SetOnRenamed(const Value: TNotifyFileSystemRenamed);
    procedure SetPath(const Value: string);
    procedure SetNotifyFilter(const Value: NotifyFilters);
    procedure SetIncludeSubDirectories(const Value: boolean);

    function GetWatcherTask(APath: string; AFilter: string;
                            AIncludeSubDirectories: boolean;
                            ANotifyFilter: NotifyFilters): ITask;
    function GetNotifyFilterAsDWORD(ANotifyFilter: NotifyFilters): DWORD;
    procedure ParseChanges(var buffer: array of byte);
  protected
    procedure DoOnDeleted(ChangeType: WatcherChangeType; Directory, Name: string); virtual;
    procedure DoOnCreated(ChangeType: WatcherChangeType; Directory, Name: string); virtual;
    procedure DoOnChanged(ChangeType: WatcherChangeType; Directory, Name: string); virtual;
    procedure DoOnRenamed(ChangeType: WatcherChangeType; Directory, Name, OldName: string); virtual;
  public
    property OnDeleted: TNotifyFileSystemWatcher read FOnDeleted write SetOnDeleted;
    property OnCreated: TNotifyFileSystemWatcher read FOnCreated write SetOnCreated;
    property OnChanged: TNotifyFileSystemWatcher read FOnChanged write SetOnChanged;
    property OnRenamed: TNotifyFileSystemRenamed read FOnRenamed write SetOnRenamed;

    property Path: string read FPath write SetPath;
    property IncludeSubDirectories: boolean read FIncludeSubDirectories write SetIncludeSubDirectories;
    property Filter: string read FFilter write FFilter;
    property NotifyFilter: NotifyFilters read FNotifyFilter write SetNotifyFilter;

    constructor Create; overload;
    constructor Create(APath: string); overload;
    constructor Create(APath, AFilter: string); overload;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation
uses
  System.Classes,
  System.IOUtils,
  System.Masks;

const
 NOTIFY_FILTER_DEFAULT = [
                          FileName,
                          DirectoryName,
                          Size,
                          LastWrite,
                          CreationTime
                         ];

{ TFileSystemWatcher }

constructor TFileSystemWatcher.Create;
begin
  FNotifyFilter := NOTIFY_FILTER_DEFAULT;
end;

constructor TFileSystemWatcher.Create(APath: string);
begin
  if APath.IsEmpty then
    raise EArgumentNilException.Create('APath');

  FPath := APath;
  FNotifyFilter := NOTIFY_FILTER_DEFAULT;
end;

constructor TFileSystemWatcher.Create(APath, AFilter: string);
begin
  if APath.IsEmpty then
    raise EArgumentNilException.Create('APath');

  if AFilter.IsEmpty then
    raise EArgumentNilException.Create('AFilter');

  FPath   := APath;
  FFilter := AFilter;
  FNotifyFilter := NOTIFY_FILTER_DEFAULT;
end;

destructor TFileSystemWatcher.Destroy;
begin
  self.Stop;
  inherited Destroy;
end;

procedure TFileSystemWatcher.DoOnChanged(ChangeType: WatcherChangeType;
  Directory, Name: string);
begin
  if assigned(FOnChanged) then
    FOnChanged(self, ChangeType, Directory, Name);
end;

procedure TFileSystemWatcher.DoOnCreated(ChangeType: WatcherChangeType;
  Directory, Name: string);
begin
  if assigned(FOnCreated) then
    FOnCreated(self, ChangeType, Directory, Name);
end;

procedure TFileSystemWatcher.DoOnDeleted(ChangeType: WatcherChangeType;
  Directory, Name: string);
begin
  if assigned(FOnDeleted) then
    FOnDeleted(self, ChangeType, Directory, Name);
end;

procedure TFileSystemWatcher.DoOnRenamed(ChangeType: WatcherChangeType;
  Directory, Name, OldName: string);
begin
  if assigned(FOnRenamed) then
    FOnRenamed(self, ChangeType, Directory, Name, OldName);
end;

function TFileSystemWatcher.GetNotifyFilterAsDWORD(
  ANotifyFilter: NotifyFilters): DWORD;
var
  vResult: DWORD;
begin
  vResult := 0;

  if TNotifyFilter.FileName in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_FILE_NAME;

  if TNotifyFilter.DirectoryName in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_DIR_NAME;

  if TNotifyFilter.Attributes in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_ATTRIBUTES;

  if TNotifyFilter.Size in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_SIZE;

  if TNotifyFilter.LastWrite in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_LAST_WRITE;

  if TNotifyFilter.LastAccess in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_LAST_ACCESS;

  if TNotifyFilter.CreationTime in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_CREATION;

  if TNotifyFilter.Security in ANotifyFilter then
    vResult := vResult or FILE_NOTIFY_CHANGE_SECURITY;

  result := vResult;
end;

function TFileSystemWatcher.GetWatcherTask(APath, AFilter: string;
  AIncludeSubDirectories: boolean; ANotifyFilter: NotifyFilters): ITask;
begin
  result := TTask.Create(
            procedure
            var
              WaitHandles: array [0 .. 1] of THandle;
              HandleChange: THandle;
              FOverlapped: TOverlapped;
              Buf: array[0..65535] of byte;
              vNotifyFilter: DWORD;
            begin
              {хэндл события завершения потока}
              FTermEvent := CreateEvent(nil, True, False, nil);

              FillChar(FOverlapped, SizeOf(TOverlapped), 0);

              FOverlapped.hEvent := CreateEvent(nil, True, False, nil);

              HandleChange := CreateFile(PWideChar(APath), GENERIC_READ,
                FILE_SHARE_READ or FILE_SHARE_DELETE or FILE_SHARE_WRITE,
                nil,
                OPEN_EXISTING,
                FILE_FLAG_BACKUP_SEMANTICS or
                FILE_FLAG_OVERLAPPED, 0);
              {проверяем, что хэндл успешно получен}
//              Win32Check(HandleChange, INVALID_HANDLE_VALUE);

              WaitHandles[0] := FTermEvent;
              WaitHandles[1] := FOverlapped.hEvent;
              vNotifyFilter := GetNotifyFilterAsDWORD(ANotifyFilter);

              try
                while true do
                begin
                  ReadDirectoryChanges(HandleChange, @Buf, Sizeof(Buf), AIncludeSubDirectories,
                    vNotifyFilter,
                    nil, @FOverlapped, nil);

                  if WaitForMultipleObjects(2, @WaitHandles, False, INFINITE)
                    = WAIT_OBJECT_0 then
                    Break;
                  ParseChanges(Buf);
                end;
              finally
                CloseHandle(HandleChange);
                CloseHandle(FTermEvent);
                CloseHandle(FOverlapped.hEvent);
                FTermEvent := 0;
              end;
            end
            );
end;

procedure TFileSystemWatcher.SetIncludeSubDirectories(const Value: boolean);
var
  isStarted: boolean;
begin
  if FWatcherTask <> nil then begin
    self.Stop;
    isStarted := true;
  end else
    isStarted := false;

  FIncludeSubDirectories := Value;

  if isStarted then
    self.Start;
end;

procedure TFileSystemWatcher.SetNotifyFilter(const Value: NotifyFilters);
var
  isStarted: boolean;
begin
  if FWatcherTask <> nil then begin
    self.Stop;
    isStarted := true;
  end else
    isStarted := false;

  FNotifyFilter := Value;

  if isStarted then
    self.Start;
end;

procedure TFileSystemWatcher.SetOnChanged(
  const Value: TNotifyFileSystemWatcher);
begin
  FOnChanged := Value;
end;

procedure TFileSystemWatcher.SetOnCreated(
  const Value: TNotifyFileSystemWatcher);
begin
  FOnCreated := Value;
end;

procedure TFileSystemWatcher.SetOnDeleted(
  const Value: TNotifyFileSystemWatcher);
begin
  FOnDeleted := Value;
end;

procedure TFileSystemWatcher.SetOnRenamed(
  const Value: TNotifyFileSystemRenamed);
begin
  FOnRenamed := Value;
end;

procedure TFileSystemWatcher.SetPath(const Value: string);
var
  isStarted: boolean;
begin
  if FWatcherTask <> nil then begin
    self.Stop;
    isStarted := true;
  end else
    isStarted := false;

  FPath := Value;

  if isStarted then
    self.Start;
end;

procedure TFileSystemWatcher.Start;
begin
  if not TDirectory.Exists(FPath) then
    raise EArgumentException.Create('Path not exists.');

  if FWatcherTask <> nil then
    exit;


  FWatcherTask := GetWatcherTask(FPath,
                                 FFilter,
                                 FIncludeSubDirectories,
                                 FNotifyFilter);

  FWatcherTask.Start;
end;

procedure TFileSystemWatcher.Stop;
begin
  if FWatcherTask = nil then
    exit;

  SetEvent(FTermEvent);

  FWatcherTask := nil;
end;

procedure TFileSystemWatcher.ParseChanges(var buffer: array of byte);
var
  fni: ^FILE_NOTIFY_INFORMATION;
  ws: WideString;
  vOldName, VNewName: string;
begin
  fni := @buffer;
  while True do begin
    SetLength(ws, fni^.FileNameLength div SizeOf(WideChar));
    Move(fni^.FileName, ws[1], fni^.FileNameLength);

    case fni^.Action of
      FILE_ACTION_ADDED:      TThread.Synchronize(nil,
                              procedure
                              var
                                vFileName: string;
                              begin
                                vFileName := ExtractFileName(ws);
                                if MatchesMask(vFileName, FFilter) or FFilter.IsEmpty then

                                self.DoOnCreated(WatcherChangeType.Created,
                                                 ExtractFilePath(ws),
                                                 vFileName);
                              end);
      FILE_ACTION_REMOVED:    TThread.Synchronize(nil,
                              procedure
                              var
                                vFileName: string;
                              begin
                                vFileName := ExtractFileName(ws);
                                if MatchesMask(vFileName, FFilter) or FFilter.IsEmpty then
                                self.DoOnDeleted(WatcherChangeType.Changed,
                                                 ExtractFilePath(ws),
                                                 vFileName);
                              end);
      FILE_ACTION_MODIFIED:   TThread.Synchronize(nil,
                              procedure
                              var
                                vFileName: string;
                              begin
                                vFileName := ExtractFileName(ws);
                                if MatchesMask(vFileName, FFilter) or FFilter.IsEmpty then
                                self.DoOnChanged(WatcherChangeType.Changed,
                                                 ExtractFilePath(ws),
                                                 vFileName);
                              end);
      FILE_ACTION_RENAMED_OLD_NAME,
      FILE_ACTION_RENAMED_NEW_NAME: begin
                                      case fni^.Action of
                                        FILE_ACTION_RENAMED_OLD_NAME: vOldName := ExtractFileName(ws);
                                        FILE_ACTION_RENAMED_NEW_NAME: vNewName := ExtractFileName(ws);
                                      end;

                                       if (not vOldName.IsEmpty) and (not vNewName.IsEmpty) then begin
                                          TThread.Synchronize(nil,
                                           procedure
                                           begin
                                            if MatchesMask(vOldName, FFilter) or
                                               MatchesMask(vNewName, FFilter) or
                                               FFilter.IsEmpty then
                                            self.DoOnRenamed(WatcherChangeType.Renamed,
                                                             ExtractFilePath(ws),
                                                             vNewName,
                                                             vOldName);
                                           end);

                                          vNewName := string.Empty;
                                          vOldName := string.Empty;
                                       end;
                                     end;
    end;

    if fni^.NextEntryOffset > 0 then
      fni := pointer(Cardinal(fni) + fni^.NextEntryOffset)
    else
      Break;
  end;
end;

end.
