unit psSQLEngine;
//42803
{$M+}{$H+}{$mode delphi}

{$include phone_sy.inc}

interface

uses
  Forms, SysUtils, Classes, DateUtils, Graphics,
  MsgBox,
  mncConnections,
  psSqlUtils, Dialogs,
  mncCSV, mncSQL, mncDB,
  mnXMLRttiProfile, mnXMLRttiStdClasses,
  mncFirebird,
  mncPostgre,
  mncMySQL,
  mncSQLite,
  UniDates;

type

  { TSQLConnection }

  TpsSQLConnection = TmncSQLConnection;

  TpsSQLSession = TmncSQLSession;

  TpsCMD = TmncSQLCommand;

  TSaveInTableInfo = record
    Name: string;
    Clear: Boolean;
    Remove: Boolean;
    Selected: Boolean;
  end;

  TDataGridKind = (dgkNone, dgkData);

  TCSVIE = record
    CSVOptions: TmncCSVOptions;
    Archives: Boolean;
    Clear: Boolean;
    CalcTotals: Boolean;
    Name: string;
    Sig: string;
    Count: Integer;
    Skip: Integer;
    SkipDigit: Integer;
    NotStarted: string;
    Started: string;
    Limit: Integer;
    Active: Boolean;
    Flexible: Boolean;
    FileName: string;
    FieldByNumber: Boolean;
    CallerField: string;
    CalleeField: string;
    DurationField: string;
    TimeSeparator: Char;
    ExternalDatabase: Boolean;
    Host: string;
    Database: string;
    UserName: string;
    Password: string;
    DataTable: string;
    Tables: string;
    DateField: string;
    Where: string;
    FromDate: TDateTime;
    ToDate: TDateTime;
    MultiTables: Boolean;
  end;

  TpsProfile = class(TCollectionItem)
  public
    Info: TCSVIE;
  published
    property Name: string read Info.Name write Info.Name;
    property Sig: string read Info.Sig write Info.Sig;
    property Count: Integer read Info.Count write Info.Count;

    property QuoteChar: Char read Info.CSVOptions.QuoteChar write Info.CSVOptions.QuoteChar;
    property ANSIContents: Boolean read Info.CSVOptions.ANSIContents write Info.CSVOptions.ANSIContents;
    property EndOfLine: string read Info.CSVOptions.EndOfLine write Info.CSVOptions.EndOfLine;
    property DelimiterChar: Char read Info.CSVOptions.DelimiterChar write Info.CSVOptions.DelimiterChar;
    property EscapeChar: Char read Info.CSVOptions.EscapeChar write Info.CSVOptions.EscapeChar;

    property Skip: Integer read Info.Skip write Info.Skip;
    property SkipDigit: Integer read Info.SkipDigit write Info.SkipDigit;
    property Limit: Integer read Info.Limit write Info.Limit;
    property Active: Boolean read Info.Active write Info.Active;
    property FileName: string read Info.FileName write Info.FileName;
    property FieldByNumber: Boolean read Info.FieldByNumber write Info.FieldByNumber;
    property CallerField: string read Info.CallerField write Info.CallerField;
    property CalleeField: string read Info.CalleeField write Info.CalleeField;
    property DurationField: string read Info.DurationField write Info.DurationField;
    property TimeSeparator: Char read Info.TimeSeparator write Info.TimeSeparator;
    property Host: string read Info.Host write Info.Host;
    property Database: string read Info.Database write Info.Database;
    property UserName: string read Info.UserName write Info.UserName;
    property Password: string read Info.Password write Info.Password;
    property DataTable: string read Info.DataTable write Info.DataTable;
    property DateField: string read Info.DateField write Info.DateField;
    property MultiTables: Boolean read Info.MultiTables write Info.MultiTables;
    property Tables: string read Info.Tables write Info.Tables;
    property Where: string read Info.Where write Info.Where;
    property Flexible: Boolean read Info.Flexible write Info.Flexible;
  end;

  { TpsProfile }

  TpsProfiles = class(TCollection)
  private
    function GetItem(Index: Integer): TpsProfile;
    procedure SetItem(Index: Integer; AValue: TpsProfile);
  public
    property Items[Index: Integer]: TpsProfile read GetItem write SetItem; default;
  published
  end;

  { TpsOptions }

  TpsOptions = class(TmnXMLProfile)
  private
    FInteractiveGrid: Boolean;
    FProfiles: TpsProfiles;
    FRecentFolder: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property RecentFolder: string read FRecentFolder write FRecentFolder;
    property Profiles: TpsProfiles read FProfiles;
    property InteractiveGrid: Boolean read FInteractiveGrid write FInteractiveGrid default True;
  end;

  { TpsSQLEngine }

  TclctEnumRequestsFlag = (erqAll, erqNew);

  TpsSQLEngine = class(TObject)
  private
    FWorkPath: String;
    FActiveCount:Integer;
    FSetting: TpsOptions;
    FSettingLoaded :Boolean;
    FConnection: TpsSQLConnection;
    FSession: TpsSQLSession;

    FHost: string;
    FUsername: string;
    FPassword: string;

    function GetActive: Boolean;
  public
    Tables: TStringList;
    FieldNames: TStringList;
    FieldWidths: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure EnumTables(vTables: TStringList);
    function CreateCMD: TpsCMD;
    procedure ExecuteFile(SQLFile: string);
    procedure Execute(SQL: TStringList);
    procedure Execute(SQL: string);
    procedure LoadSetting;
    procedure SaveSetting;
    procedure Open(Options: TpsOptions); overload;
    procedure Open; overload;
    procedure Close;
    property Active:Boolean read GetActive;
    property Connection: TpsSQLConnection read FConnection;
    property Session: TpsSQLSession read FSession;
    property Options: TpsOptions read FSetting;
    property WorkPath: String read FWorkPath write FWorkPath;
  end;

function DurationToString(vDuration: Integer; TimeSeparator: string): string;
function StringToDuration(S: String; TimeSeparator: Char): Integer;
function TickToString(ms: Integer): string;

function GetWorkPath: String;

var
  Engine: TpsSQLEngine = nil;

implementation

uses
  mnUtils;

function GetWorkPath: String;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ExtractFilePath(Application.ExeName) + 'data'));
end;

{ TpsProfiles }

function TpsProfiles.GetItem(Index: Integer): TpsProfile;
begin
  Result := inherited Items[Index] as TpsProfile
end;

procedure TpsProfiles.SetItem(Index: Integer; AValue: TpsProfile);
begin
  inherited Items[Index] := AValue;
end;

procedure TpsSQLEngine.EnumTables(vTables: TStringList);
var
  CMD: TpsCMD;
begin
  CMD := CreateCMD;
  try
    CMD.SQL.Text := 'SELECT * FROM sqlite_master WHERE type="table" and name like "tbl%" order by name';
    CMD.Execute;
    while not CMD.Done do
    begin
      vTables.Add(CMD.Fields['name'].AsAnsiString);
      CMD.Next;
    end
  finally
    FreeAndNil(CMD);
  end
end;

function DurationToString(vDuration: Integer; TimeSeparator: string): string;
var
  h, m, s: integer;
  g: Boolean;
  function LeadToRight(const vStr: string; Count: integer; vChar: Char): string;
  var
    l: integer;
  begin
    l := Length(vStr);
    if l < Count then
    begin
      Result := StringOfChar(vChar, Count - l) + vStr;
    end
    else
      Result := vStr;
  end;
begin
  Result := '';
  g := vDuration < 0;
  vDuration := abs(vDuration);
  h := trunc(vDuration) div 3600;
  vDuration := (vDuration  - (h *  3600));
  m := vDuration div 60;
  s := (vDuration  - (m *  60));

  if h > 0 then
    Result := LeadToRight(IntToStr(h), 2, '0');

  if m > 0 then
  begin
    if Result <> '' then
      Result := Result + TimeSeparator;
    Result := Result + LeadToRight(IntToStr(m), 2, '0');
  end;

  if s > 0 then
  begin
    if Result <> '' then
      Result := Result + TimeSeparator;
    Result := Result + LeadToRight(IntToStr(s), 2, '0');
  end;
  if g then
    Result := '-' + Result;
end;

function StringToDuration(S: String; TimeSeparator: Char): Integer;
var
  Strings: TStringList;
  i: Integer;
  d: Integer;
begin
  //10:20:15
  //20:15
  //15
  Strings := TStringList.Create;
  mnUtils.StrToStrings(S, Strings, [TimeSeparator]);
  d := 1;
  Result := 0;
  i := Strings.Count - 1;
  while i>=0 do
  begin
    Result := Result + StrToInt(Strings[i]) * d;
    d := d * 60;
    dec(i);
  end;
end;

function TickToString(ms: Integer): string;
var
  h, m, s: integer;
  g: Boolean;
  function LeadToRight(const vStr: string; Count: integer; vChar: Char): string;
  var
    l: integer;
  begin
    l := Length(vStr);
    if l < Count then
    begin
      Result := StringOfChar(vChar, Count - l) + vStr;
    end
    else
      Result := vStr;
  end;
begin
  Result := '';
  g := ms < 0;

  ms := abs(ms);

  s := ms div 1000;
  ms := (ms - (s *  1000));

  m := s div 60;
  s := (s - (m *  60));

  h := m div 60;
  m := (m - (h *  60));

  if h > 0 then
    Result := LeadToRight(IntToStr(h), 2, '0');

  if m > 0 then
  begin
    if Result <> '' then
      Result := Result + GetFormatSettings.TimeSeparator;
    Result := Result + LeadToRight(IntToStr(m), 2, '0');
  end;

  if s > 0 then
  begin
    if Result <> '' then
      Result := Result + GetFormatSettings.TimeSeparator;
    Result := Result + LeadToRight(IntToStr(s), 2, '0');
  end;

  if Result <> '' then
    Result := Result + GetFormatSettings.TimeSeparator;
  Result := Result + LeadToRight(IntToStr(ms), 2, '0');

  if g then
    Result := '-' + Result;
end;

procedure TpsSQLEngine.Open(Options: TpsOptions);
var
  s:string;
begin
  if FActiveCount = 0 then
  begin
    if not FConnection.Connected then
    begin
      if Connection.IsModel('SQLite') then
      begin
        FConnection.Resource := WorkPath + 'data.sqlite';
        FConnection.AutoCreate := True;
        (FConnection as TmncSQLiteConnection).Exclusive := True;
        //(FConnection as TmncSQLiteConnection).JournalMode := jrmMemory;
        (FConnection as TmncSQLiteConnection).JournalMode := jrmTruncate;
        (FConnection as TmncSQLiteConnection).TempStore := tmpFile;
        s := FConnection.Resource;
        if not FileExists(s) then
        begin
          FConnection.Connect;
          FConnection.Execute('PRAGMA page_size = 4096');
          FConnection.Execute('PRAGMA cache_size=10000');
          Session.Start;
          //CreateMetaData;
        end
        else
        begin
          FConnection.Connect;
          FConnection.Execute('PRAGMA page_size = 4096');
          FConnection.Execute('PRAGMA cache_size=10000');
        end;
      end
      else if Connection.IsModel('MySQL') then
      begin
        FConnection.Connect;
        (Connection as TmncMySQLConnection).SetStorageEngine('MYISAM');
        FConnection.Resource := 'phone_sy';
        if not (FConnection as TmncMySQLConnection).SelectDatabase('phone_sy', false) then
        begin
          (FConnection as TmncMySQLConnection).CreateDatabase('phone_sy', true);
          (FConnection as TmncMySQLConnection).SelectDatabase('phone_sy');
          FSession.Start;
          ///CreateMetaData;
        end;
      end
      else if Connection.IsModel('PostgreSQL') then
      begin
        FConnection.Resource := 'phone_sy';
        try
          FConnection.CreateDatabase(FConnection.Resource, false);
          //CreateMetaData;
        except
        end;
        FConnection.Connect;
      end;
      if not FSession.Active then
      begin
        FSession.Start;
        Tables.Clear;
        EnumTables(Tables);
      end;
    end;
  end;
  Inc(FActiveCount);
end;

procedure TpsSQLEngine.Close;
begin
  if Active then
  begin
    Dec(FActiveCount);
    if (FActiveCount = 0) and (FConnection <> nil) then
    begin
      FSession.Stop;
      FConnection.Disconnect;
    end;
  end;
end;

procedure TpsSQLEngine.Open;
begin
  if not FSettingLoaded then
    LoadSetting;
  Open(FSetting);
end;

constructor TpsSQLEngine.Create;
begin
  inherited;
  Tables := TStringList.Create;
  FieldNames := TStringList.Create;

{  FieldNames.Values['PhoneNum'] := 'Phone';
  FieldNames.Values['FName'] := 'Name';
  FieldNames.Values['LName'] := 'Family';
  FieldNames.Values['FATHR'] := 'Father';
  FieldNames.Values['ADDRSS'] := 'Address';
}
  FieldNames.Values['PhoneNum'] := 'الهاتف';
  FieldNames.Values['FName'] := 'الاسم';
  FieldNames.Values['LName'] := 'العائلة';
  FieldNames.Values['FATHR'] := 'الأب';
  FieldNames.Values['ADDRSS'] := 'العنوان';

  FieldWidths := TStringList.Create;
  FieldWidths.Values['PhoneNum'] := '100';
  FieldWidths.Values['FName'] := '200';
  FieldWidths.Values['LName'] := '100';
  FieldWidths.Values['FATHR'] := '100';
  FieldWidths.Values['ADDRSS'] := '300';


  if Engine <> nil then
    raise Exception.Create('Engine already created');

  FSetting := TpsOptions.Create;

  DefaultSQLEngine := sqleSQLite;
  DefaultSQLQuoted := False;

  FConnection := (Engines.CreateByName('sqlite') as TmncSQLConnection);
  FSession := Connection.CreateSession;
  FConnection.Host := FHost;
  FConnection.UserName := FUserName;
  FConnection.Password := FPassword;
end;

function TpsSQLEngine.CreateCMD: TpsCMD;
begin
  if not Active then
    raise Exception.Create('Database not opened');
  Result := Session.CreateCommand;
end;

procedure TpsSQLEngine.ExecuteFile(SQLFile: string);
var
  aStrings: TStringList;
begin
  aStrings := TStringList.Create;
  try
    aStrings.LoadFromFile(SQLFile);
    Execute(aStrings);
  finally
    aStrings.Free;
  end;
end;

procedure TpsSQLEngine.Execute(SQL: string);
var
  aStrings: TStringList;
begin
  aStrings := TStringList.Create;
  try
    aStrings.Text := SQL;
    Execute(aStrings);
  finally
    aStrings.Free;
  end;
end;

procedure TpsSQLEngine.Execute(SQL: TStringList);
var
  CMD: TpsCMD;
  i: Integer;
begin
  CMD := CreateCMD;
  try
    for i := 0 to SQL.Count -1 do
    begin
      if SQL[i] = '^' then
      begin
        if CMD.SQL.Count > 0 then
          CMD.Execute;
        CMD.SQL.Clear;
      end
      else
      begin
        if trim(SQL[i]) <> '' then
          CMD.SQL.Add(SQL[i]);
      end;
    end;
    if CMD.SQL.Count > 0 then
      CMD.Execute;
    CMD.Session.CommitRetaining;
  finally
    CMD.Free;
  end;
end;

destructor TpsSQLEngine.Destroy;
begin
  FreeAndNil(FSession);
  FreeAndNil(FConnection);
  FreeAndNil(FSetting);
  FreeAndNil(Tables);
  FreeAndNil(FieldNames);
  FreeAndNil(FieldWidths);
  inherited;
end;

function TpsSQLEngine.GetActive: Boolean;
begin
  Result := Connection.Connected;
end;

procedure TpsSQLEngine.LoadSetting;
begin
  WorkPath := GetWorkPath;
  Options.SafeLoadFromFile(WorkPath + 'config.xml');
  FSettingLoaded := True;
end;

procedure TpsSQLEngine.SaveSetting;
begin
  Options.SaveToFile(WorkPath + 'config.xml');
end;

{ TpsOptions }

constructor TpsOptions.Create;
begin
  inherited;
  FProfiles := TpsProfiles.Create(TpsProfile);
  FInteractiveGrid := True;
end;

destructor TpsOptions.Destroy;
begin
  FreeAndNil(FProfiles);
  inherited Destroy;
end;

end.

