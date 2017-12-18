unit MainForms;
{$codepage utf8}
{$mode delphi}{$H+}
{$include phone_sy.inc}

interface

uses
  windows, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType, FileUtil, strutils,
  ExtCtrls, StdCtrls, Menus, SynEdit, SynHighlighterSQL, Variants, mnStreams,
  MsgBox, mncCSVExchanges, mncCSV, mnSynHighlighterStdSQL,
  CSVOptionsForms, OptionsForms, psSQLEngine,
  psSqlUtils, sqldb;

type

  TControlRec = record
    Edit: TEdit;
    SQLName: string;
    Visible: string;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    ArcCountLbl: TLabel;
    Button1: TButton;
    ExportBtn: TButton;
    FindBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    NameEdit: TEdit;
    Label1: TLabel;
    LastIDlbl: TLabel;
    NumberEdit: TEdit;
    FatherEdit: TEdit;
    FamilyEdit: TEdit;
    AddressEdit: TEdit;
    TestBtn: TButton;
    ExitBtn: TButton;
    ExportSaveDialog: TSaveDialog;
    CountLbl: TLabel;
    ImportBtn3: TButton;
    ImportBtn4: TButton;
    ImportBtn7: TButton;
    ImportBtn8: TButton;
    ImportOpenDialog: TOpenDialog;
    MainPnl: TPanel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    ReloadMnu: TMenuItem;
    SQLQuery1: TSQLQuery;
    ToolsMnu: TMenuItem;
    OpenDialog: TOpenDialog;
    OptionsBtn: TButton;
    PopupMenu1: TPopupMenu;
    SaveDialog: TSaveDialog;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter2: TSplitter;

    procedure Button1Click(Sender: TObject);

    procedure FindBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem8Click(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
  private
    FindFields: array of TControlRec;
  public
    function GetFindWhere(Where: string): string;
    function GetFindSQL: string;
    function GetFindSQLBy(FieldName, Value: string): string;

    procedure ExecuteScript; overload;
    procedure ExecuteScript(DataKind: TDataGridKind; SQL: string); overload;
    procedure ExecuteScript(DataKind: TDataGridKind; SQL: TStringList; vList: TStringList = nil); overload;
    procedure ExportData(aExportStream: TFileStream; SQLCMD: TpsCMD);

    procedure OpenData;

    procedure CatchErr(Sender: TObject; e: exception);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  mncConnections, DataForm, LConvEncoding;

{ TMainForm }

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := CatchErr;
  Engine := TpsSQLEngine.Create;
  Engine.LoadSetting;
  OpenData;
  Caption := Caption + ' : '+Engine.Connection.Model.Name;
end;

procedure TMainForm.ExportBtnClick(Sender: TObject);
var
  aCMD: TpsCMD;
  aExport: TmncCSVExport;
  aStream : TFileStream;
  aCSVOptions: TmncCSVOptions;
begin
  aExport := TmncCSVExport.Create;
  try
    ExportSaveDialog.FileName := '*.csv';
    ExportSaveDialog.DefaultExt := 'csv';
    ExportSaveDialog.Filter := '*.csv';
    FillMemory(@aCSVOptions, SizeOf(aCSVOptions), 0);
    aCSVOptions.DelimiterChar := ';';
    aCSVOptions.HeaderLine := hdrNormal;
    if ExportSaveDialog.Execute and ShowCSVOptions('Export CSV', aCSVOptions) then
    begin
      aExport.CSVOptions := aCSVOptions;
      aStream := TFileStream.Create(ExportSaveDialog.FileName, fmCreate);
      aCMD := Engine.CreateCMD;
      try
        aCMD.SQL.Text := GetFindSQL;
        aExport.Command := aCMD;
        aExport.Stream := aStream;
        Screen.Cursor := crHourGlass;
        aExport.Execute;
        Screen.Cursor := crDefault;
        ShowMessage('Export count: '+IntToStr(aExport.Count));
        Engine.Session.CommitRetaining;
      finally
        aStream.Free;
        aCMD.Free;
      end;
    end;
  finally
    aExport.Free;
  end;
end;

function TMainForm.GetFindWhere(Where: string): string;
var
  i: integer;
begin
  Result := '';
  if Where <> '' then
  begin
    for i := 0 to Engine.Tables.Count - 1 do
    begin
      if i > 0 then
        Result := Result + #13'union all '#13;
      Result := Result + 'select PhoneNum, FName, LName, FATHR, ADDRSS from ' + Engine.Tables[i] + ' where ' + Where;
    end;
  end;
end;

function TMainForm.GetFindSQL: string;
var
  i: integer;
  f: string;
  s, Where: utf8string;
  rb: RawByteString;
begin
  Result := '';
  Where := '';
  for i := 0 to length(FindFields) -1 do
  begin
    if FindFields[i].Edit.Text <> '' then
    begin
      if Where <> '' then
        Where := Where + ' and ';

      f := trim(FindFields[i].Edit.Text);
      f := StringReplace(f, '*', '%', [rfReplaceAll]);
      if LeftStr(f, 1) = '%' then
        f := MidStr(f, 2, MaxInt);
      if RightStr(f, 1) = '%' then
        f := MidStr(f, 1, Length(f) - 1);
      s := f;
      rb := CP1252ToUTF8(UTF8ToCP1256(s, false));
      s := rb;
      Where := Where + FindFields[i].SQLName + ' like "%' + s + '%"';
    end
  end;
  Result := GetFindWhere(Where);
end;

function TMainForm.GetFindSQLBy(FieldName, Value: string): string;
var
  Where: utf8string;
begin
  Result := '';
  Where := FieldName + ' like "%' + ConvertToLatin(Value) + '%"';
  Result := GetFindWhere(Where);
end;

procedure TMainForm.OpenData;
begin
  if not Engine.Active then
  begin
    Engine.Open;
  end;
end;

procedure TMainForm.MenuItem8Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(Engine);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(FindFields) -1 do
  begin
    FindFields[i].Edit.Text := '';
  end
end;

procedure TMainForm.FindBtnClick(Sender: TObject);
begin
  ExecuteScript(dgkData, GetFindSQL);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ExecuteScript;
end;

constructor TMainForm.Create(TheOwner: TComponent);
  procedure AddEdit(ASQLName: string; AEdit: TEdit);
  var
    c: integer;
  begin
    c := Length(FindFields);
    SetLength(FindFields, c + 1);
    FindFields[c].Edit := AEdit;
    FindFields[c].SQLName := ASQLName;
  end;
begin
  inherited Create(TheOwner);
  AddEdit('PhoneNum', NumberEdit);
  AddEdit('FName', NameEdit);
  AddEdit('FATHR', FatherEdit);
  AddEdit('LName', FamilyEdit);
  AddEdit('ADDRSS',AddressEdit);
end;

destructor TMainForm.Destroy;
begin
  inherited Destroy;
end;

procedure TMainForm.OptionsBtnClick(Sender: TObject);
begin
  if ShowOptions(Engine.Options) then
    Engine.SaveSetting;
end;

procedure TMainForm.ExecuteScript;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := GetFindSQL;
    ExecuteScript(dgkData, Lines);
  finally
    Lines.Free;
  end;
end;

procedure TMainForm.ExecuteScript(DataKind: TDataGridKind; SQL: TStringList; vList: TStringList = nil);
var
  aForm: TGridDataForm;
begin
  if DataKind > dgkNone then
  begin
    aForm := TGridDataForm.Create(Application);
    aForm.Show;
    Application.ProcessMessages;
    aForm.DataKind := DataKind;
  end
  else
    aForm := nil;
  ExecuteScriptData(aForm, SQL, vList);
end;

procedure TMainForm.ExecuteScript(DataKind: TDataGridKind; SQL: string);
var
  Lines:TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := SQL;
    ExecuteScript(DataKind, Lines);
  finally
    Lines.Free;
  end;
end;

procedure TMainForm.ExportData(aExportStream: TFileStream; SQLCMD: TpsCMD);
var
  aExport: TmncCSVExport;
begin
  aExport := TmncCSVExport.Create;
  try
    aExport.Command := SQLCMD;
    aExport.HeaderLine := hdrNone;
    aExport.EndOfLine := sWinEndOfLine;
    aExport.Stream := aExportStream;
    aExport.Execute;
  finally
    aExport.Free;
  end;
end;

procedure TMainForm.CatchErr(Sender: TObject; e: exception);
begin
  MsgBox.Msg.Error(e.Message);
end;
{
Engine.Session.Commit;
Engine.Connection.Vacuum;
Engine.Session.Start;
}
end.
