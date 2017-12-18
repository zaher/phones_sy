unit DataForm;

{$mode objfpc}
{$H+}
{$include phone_sy.inc}

interface

uses
  Windows, Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, mncSQLite, strutils, Clipbrd, psSqlUtils, MsgBox, LazUTF8, LConvEncoding,
  SynEdit,
  mncConnections, mncCSV, mnStreams, StdCtrls, Grids, Menus,
  psSQLEngine, MainForms, ParamsForms, CSVOptionsForms;

type

  { TGridDataForm }

  TGridDataForm = class(TForm)
    DataGrid: TStringGrid;
    ExportSaveDialog: TSaveDialog;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    ImportOpenDialog: TOpenDialog;
    MenuItem1: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    SearchNumber: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem26: TMenuItem;
    SaveGrisBtn1: TButton;
    MenuItem3: TMenuItem;
    Panel2: TPanel;
    DataMenu: TPopupMenu;
    LogEdit: TSynEdit;
    StopBtn: TButton;
    StopBtn2: TButton;
    StopBtn3: TButton;
    StopBtn4: TButton;
    StopBtn6: TButton;
    SaveGrisBtn: TButton;

      procedure DataGridCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: Integer; var Result: integer);
      procedure DataGridDblClick(Sender: TObject);

      procedure DataGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
      procedure DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure DataGridPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormCreate(Sender: TObject);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure MenuItem17Click(Sender: TObject);
      procedure MenuItem18Click(Sender: TObject);
      procedure MenuItem19Click(Sender: TObject);
      procedure MenuItem20Click(Sender: TObject);
      procedure MenuItem22Click(Sender: TObject);
      procedure MenuItem23Click(Sender: TObject);
      procedure MenuItem26Click(Sender: TObject);
      procedure MenuItem3Click(Sender: TObject);
      procedure SaveGrisBtn1Click(Sender: TObject);
      procedure SearchNumberClick(Sender: TObject);
      procedure StopBtn2Click(Sender: TObject);
      procedure StopBtn3Click(Sender: TObject);
      procedure StopBtn4Click(Sender: TObject);
      procedure SaveGrisBtnClick(Sender: TObject);
      procedure StopBtn6Click(Sender: TObject);
      procedure StopBtn7Click(Sender: TObject);
      procedure StopBtnClick(Sender: TObject);
  private
    FCancel: Boolean;
    FDataKind: TDataGridKind;
    FSQL: TStringList;
    FList: TStringList;
    IsNumbers: array of boolean;
    FFields: TStringList;

    function GetIsNumber(Col:integer): Boolean;
    procedure SetDataKind(AValue: TDataGridKind);
  public
    procedure ClearGrid;
    procedure ExportGrid;
    procedure ImportGrid;

    procedure FillGrid(SQLCMD: TmncCommand; Title: String; Append: Boolean = False);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(s: string);
    property DataKind: TDataGridKind read FDataKind write SetDataKind;
  end;

var
  GridDataForm: TGridDataForm;

procedure ExecuteScriptData(Form: TGridDataForm; SQL: TStringList; vList: TStringList = nil);

implementation

{$R *.lfm}

{ TGridDataForm }

procedure TGridDataForm.DataGridDblClick(Sender: TObject);
begin
end;

procedure TGridDataForm.DataGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  DataGrid.Canvas.Font.Assign(DataGrid.Font);
  //aState := aState + [gdSelected];
  if (aRow < DataGrid.FixedRows) or (aCol < DataGrid.FixedCols) then
  begin
    DataGrid.Canvas.Brush.Color := DataGrid.FixedColor;
    DataGrid.Canvas.FillRect(aRect);
  end
  else if ((aRow = DataGrid.Row) and (aCol = DataGrid.Col)) or
          ((aRow >= DataGrid.Selection.Top) and (aRow <= DataGrid.Selection.Bottom) and (aCol >= DataGrid.Selection.Left) and (aCol <= DataGrid.Selection.Right))
    then
  begin
    DataGrid.Canvas.Brush.Color := $00D8A276;
    DataGrid.Canvas.FillRect(aRect);
    //DataGrid.Canvas.Font.Color := clWhite;
  end
  else if (aRow = DataGrid.Row) then
  begin
    DataGrid.Canvas.Brush.Color := $00E0C6A3;
    DataGrid.Canvas.FillRect(aRect);
  end;
  DataGrid.DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TGridDataForm.DataGridCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: Integer; var Result: integer);
var
  f1, f2: double;
begin
  with (Sender as TStringGrid) do
  begin
    if GetIsNumber(ACol) then
    begin
      f1 := StrToFloatDef(Cells[ACol, ARow], 0);
      f2 := StrToFloatDef(Cells[BCol, BRow], 0);
      if f1 > f2 then
        Result := 1
      else if f1 < f2 then
        Result := -1
      else
        Result := 0;
    end
    else
      Result := UTF8CompareText(Cells[ACol,ARow], Cells[BCol,BRow]);
    if SortOrder=soDescending then
      result:=-result;
  end;
end;

procedure TGridDataForm.DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_C) and (Shift = [ssCtrl]) then
    DataGrid.CopyToClipboard(True)
  else if (Key = VK_RETURN) and (Shift=[]) then;

end;

procedure TGridDataForm.DataGridPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin

end;

procedure TGridDataForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FCancel := True;
end;

procedure TGridDataForm.FormCreate(Sender: TObject);
begin
  DataGrid.FocusColor := clBlack;
  //DataGrid.SelectedColor := clBlue;
  ClearGrid;
end;

procedure TGridDataForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    FCancel := True;
end;

procedure TGridDataForm.MenuItem17Click(Sender: TObject);
begin
  DataGrid.SortOrder := soDescending; // or soAscending
  DataGrid.SortColRow(true, DataGrid.Col);
end;

procedure TGridDataForm.MenuItem18Click(Sender: TObject);
begin
  DataGrid.SortOrder := soAscending;
  DataGrid.SortColRow(true, DataGrid.Col);
end;

procedure TGridDataForm.MenuItem19Click(Sender: TObject);
begin
  DataGrid.CopyToClipboard(True);
end;

procedure TGridDataForm.MenuItem20Click(Sender: TObject);
begin
  if not Msg.No('Do you want to drop table'+ DataGrid.Cells[DataGrid.Col, DataGrid.Row]) then
    Engine.Execute('drop table if exists ' + DataGrid.Cells[DataGrid.Col, DataGrid.Row]);
end;

procedure TGridDataForm.MenuItem22Click(Sender: TObject);
begin
  if not Msg.No('Do you want to empty table'+ DataGrid.Cells[DataGrid.Col, DataGrid.Row]) then
    Engine.Execute('delete from ' + DataGrid.Cells[DataGrid.Col, DataGrid.Row]);
end;

procedure RemoveRows(Grid: TStringGrid; RowIndex, vCount: Integer);
var
  i: Integer;
begin
  with Grid do
  begin
    BeginUpdate;
    try
      for i := RowIndex to RowCount - vCount - 1 do
        Rows[i] := Rows[i + vCount];
      RowCount := RowCount - vCount;
    finally
      EndUpdate;
    end;
  end;
end;

procedure RemoveCols(Grid: TStringGrid; ColIndex, vCount: Integer);
var
  i: Integer;
begin
  with Grid do
  begin
    BeginUpdate;
    try
      for i := ColIndex to ColCount - vCount - 1 do
        Cols[i] := Cols[i + vCount];
      ColCount := ColCount - vCount;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TGridDataForm.MenuItem23Click(Sender: TObject);
begin
  //DataGrid.DeleteRow(DataGrid.Row);
  RemoveRows(DataGrid, DataGrid.Selection.Top,DataGrid.Selection.Bottom - DataGrid.Selection.Top + 1);
end;

procedure TGridDataForm.MenuItem26Click(Sender: TObject);
begin
  //DataGrid.DeleteCol(DataGrid.Col);
  RemoveCols(DataGrid, DataGrid.Selection.Left,DataGrid.Selection.Right - DataGrid.Selection.Left + 1);
end;

procedure TGridDataForm.MenuItem3Click(Sender: TObject);
begin
  DataGrid.CopyToClipboard(True);
end;

procedure TGridDataForm.SaveGrisBtn1Click(Sender: TObject);
begin
  ImportGrid;
end;

procedure TGridDataForm.SearchNumberClick(Sender: TObject);
begin
  MainForm.ExecuteScript(dgkData, MainForm.GetFindSQLBy(FFields[DataGrid.Col - 1], DataGrid.Cells[DataGrid.Col, DataGrid.Row]));
end;

procedure TGridDataForm.StopBtn2Click(Sender: TObject);
begin
  ClearGrid;
end;

procedure TGridDataForm.StopBtn3Click(Sender: TObject);
begin
  Close;
end;

procedure TGridDataForm.StopBtn4Click(Sender: TObject);
begin
  Clipboard.AsText := FSQL.Text;
end;

procedure TGridDataForm.SaveGrisBtnClick(Sender: TObject);
begin
  ExportGrid;
  Msg.Show('Exported DONE');
end;

procedure TGridDataForm.StopBtn6Click(Sender: TObject);
begin
  ClearGrid;
  ExecuteScriptData(Self, FSQL, FList);
end;

procedure TGridDataForm.StopBtn7Click(Sender: TObject);
begin

end;

procedure TGridDataForm.StopBtnClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TGridDataForm.ClearGrid;
begin
  IsNumbers := nil;
  DataGrid.ColWidths[0] := 20;
  DataGrid.Row := 1;
  DataGrid.Col := 1;
  DataGrid.FixedCols := 1;
  DataGrid.FixedRows := 1;
  DataGrid.ColCount := 1;
  DataGrid.RowCount := 1;
  DataGrid.Cells[0, 0] := '';
end;

procedure TGridDataForm.FillGrid(SQLCMD: TmncCommand; Title: String; Append: Boolean);

  function GetTextWidth(Text: String): Integer;
  begin
    DataGrid.Canvas.Font := DataGrid.Font;
    Result := DataGrid.Canvas.TextWidth(Text);
  end;

  function GetCharWidth: Integer;
  begin
    Result := (GetTextWidth('Wi') div 2);
  end;

var
  i, c, w: Integer;
  s: String;
  b: Boolean;
  str: string;
  startCol: integer;
  cols: Integer;
  max: array of integer;
var
  Steps: Integer;
  bb: Boolean;
  rb: rawbytestring;
  ansi: ansistring;
begin
  StopBtn.Enabled := True;
  Steps := 100;
  if not Engine.Options.InteractiveGrid then
    DataGrid.BeginUpdate;
  try
    IsNumbers := nil;
    if Title = '' then
      Caption := 'Data'
    else
      Caption := 'Data: ' + Title;

    FetchedLbl.Caption := 'Fetched:';
    max := nil;
    FCancel := False;

    cols := SQLCMD.Columns.Count;
    setLength(max, cols + 1);
    setLength(IsNumbers, cols + 1);

    if Append then
    begin
      startCol := 0;
      DataGrid.Col := startCol;
      c := DataGrid.RowCount;
      for i := 0 to cols - 1 do
      begin
        s := Engine.FieldNames.Values[SQLCMD.Columns[i].Name];
        if s = '' then
          s := SQLCMD.Columns[i].Name;
        max[i + 1] := length(s);
        IsNumbers[i] := SQLCMD.Columns[i].IsNumber;
      end;
    end
    else
    begin
      startCol := DataGrid.ColCount - 1;
      DataGrid.ColCount := startCol + cols + 1; //1 for fixed col
      DataGrid.Col := startCol;

      for i := 0 to cols - 1 do
      begin
        FFields.Add(SQLCMD.Columns[i].Name);
        s := Engine.FieldNames.Values[SQLCMD.Columns[i].Name];
        if s = '' then
          s := SQLCMD.Columns[i].Name;
        max[i + 1] := length(s);
        DataGrid.Cells[startCol + i + 1, 0] := s;
        DataGrid.ColWidths[startCol + i + 1] := StrToIntDef(Engine.FieldWidths.Values[SQLCMD.Columns[i].Name], 100);
        b := SQLCMD.Columns[i].IsNumber;
        IsNumbers[i] := b;
      end;
      c := 1;
    end;
    if Engine.Options.InteractiveGrid then
      Application.ProcessMessages;

    while not SQLCMD.Done do
    begin
      if DataGrid.RowCount <= (c + 1) then
      begin
        if (c >= Steps) then
          DataGrid.RowCount := c + Steps
        else
          DataGrid.RowCount := c + 1;
      end;
      DataGrid.Cells[0, c] := IntToStr(c);
      for i := 0 to cols - 1 do
      begin
        if i < SQLCMD.Fields.Count then
        begin
          str := SQLCMD.Fields.Items[i].AsString;
          str := CP1256ToUTF8(UTF8ToCP1252(str));
          if length(str) > max[i + 1] then
            max[i + 1] := length(str);
          DataGrid.Cells[startCol + i + 1, c] := str;
        end;
      end;
      Inc(c);
      //before 100 rows will see the grid row by row filled, cheeting the eyes of user
      if (c < Steps) or (Frac(c / Steps) = 0) then
      begin
        if Engine.Options.InteractiveGrid then
        begin
          FetchCountLbl.Caption := IntToStr(c - 1);
        end;
        Application.ProcessMessages;
        {if c > 100000 then
          steps := 100000
        else
        if c > 10000 then
          steps := 10000
        else
        if c > 2500 then
          steps := 1000
        else }if c > 500 then
          steps := 500;
      end;
      if FCancel then
        break;
      SQLCMD.Next;
    end;

    DataGrid.RowCount := c;
    FetchCountLbl.Caption := IntToStr(c - 1);
  finally
    if not Engine.Options.InteractiveGrid then
      DataGrid.EndUpdate(True);
    StopBtn.Enabled := False;
  end;
end;

constructor TGridDataForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FList:=TStringList.Create;
  FSQL := TStringList.Create;
  FFields := TStringList.Create;
  LogEdit.Clear;
end;

destructor TGridDataForm.Destroy;
begin
  FList.Free;
  FSQL.Free;
  FFields.Free;
  inherited Destroy;
end;

procedure TGridDataForm.Log(s: string);
begin
  LogEdit.Lines.Add(s);
  LogEdit.CaretY := LogEdit.Lines.Count;
end;

procedure TGridDataForm.ExportGrid;
var
  aFile: TFileStream;
  r, c:Integer;
  s: string;
begin
  if ExportSaveDialog.Execute then
  begin
    aFile := TFileStream.Create(ExportSaveDialog.FileName, fmCreate or fmOpenWrite);
    try
      for r := 0 to DataGrid.RowCount -1 do
      begin
        s := '';
        for c := 1 to DataGrid.ColCount -1 do
        begin
          if c>1 then
            s := s + ';';
          s := s + DataGrid.Cells[c, r];
        end;

        s := s + sWinEndOfLine;
        aFile.WriteBuffer(Pointer(S)^, length(s));
      end;
    finally
      aFile.Free;
    end;
  end;
end;

procedure TGridDataForm.ImportGrid;
var
  aFile: TFileStream;
  csvCnn: TmncCSVConnection;
  csvSes: TmncCSVSession;
  csvCMD: TmncCSVCommand;
  aCSVOptions: TmncCSVOptions;
begin
  if ImportOpenDialog.Execute then
  begin
    csvCnn := TmncCSVConnection.Create;
    csvSes := TmncCSVSession.Create(csvCnn);
    try
      aCSVOptions.HeaderLine := hdrNormal;
      aCSVOptions.DelimiterChar := ',';
      aCSVOptions.EndOfLine := sUnixEndOfLine;
      if ShowCSVOptions('Export CSV', aCSVOptions) then
      begin
        csvSes.CSVOptions := aCSVOptions;
        csvCnn.Connect;
        csvSes.Start;
        aFile := TFileStream.Create(ImportOpenDialog.FileName, fmOpenRead);
        csvCMD := TmncCSVCommand.Create(csvSes, aFile, csvmRead);
        csvCMD.EmptyLine := elSkip;
        try
          csvCMD.Execute;
          FillGrid(csvCMD, 'File: ' + ImportOpenDialog.FileName);
        finally
          aFile.Free;
          csvCMD.Free;
        end;
      end;
    finally
      csvSes.Free;
      csvCnn.Free;
    end;
  end;
end;

function TGridDataForm.GetIsNumber(Col: integer): Boolean;
begin
  if Col < Length(IsNumbers) then
    Result := IsNumbers[Col - 1]
  else
    Result := false;
end;

procedure TGridDataForm.SetDataKind(AValue: TDataGridKind);
begin
  if FDataKind =AValue then Exit;
  FDataKind :=AValue;
  case DataKind of
    dgkData: DataGrid.PopupMenu := DataMenu;
    else
      DataGrid.PopupMenu := nil;
  end;
end;

procedure ExecuteScriptData(Form: TGridDataForm; SQL: TStringList; vList: TStringList = nil);
  procedure Execute(SQLCMD: TpsCMD; SQL: TStringList; Title: string; vList: TStringList = nil);
  var
    i: Integer;
    t: Cardinal;
  begin
    SQLCMD.SQL.Assign(SQL);
    SQLCMD.Prepare;
    if (vList <> nil) and (SQLCMD.Params.Count = 1) then
    begin
      for i := 0 to vList.Count -1 do
      begin
        SQLCMD.Params.Items[0].AsAnsiString := vList[i];
        t := GetTickCount64;
        SQLCMD.Execute;
        t := GetTickCount64 - t;
        if Form <> nil then
          Form.Log(FormatDateTime('hh:mm:ss', now) +' MS='+ IntToStr(t) + ' Time= '+ TickToString(t) + ': ' + Copy(SQLCMD.SQL.Text, 1, 20)+'...');
        if Form <> nil then
          Form.FillGrid(SQLCMD, Title, i > 0);
      end;
    end
    else
    begin
      if (SQLCMD.Params.Count > 0) then
      begin
        if not ShowSQLParams(SQLCMD) then
        begin
          SQLCMD.Active := False;
          exit;
        end
      end;
      t := GetTickCount64;
      SQLCMD.Execute;
      t := GetTickCount64 - t;
      if Form <> nil then
        Form.Log(FormatDateTime('hh:mm:ss', now) +' MS='+ IntToStr(t) + ' Time= '+ TickToString(t) + ': ' + Copy(SQLCMD.SQL.Text, 1, 20)+'...');
      if Form <> nil then
        Form.FillGrid(SQLCMD, Title);
    end;
  end;
var
  aStrings: TStringList;
  i: Integer;
  SQLCMD: TpsCMD;
  aTitle: string;
  line: string;
begin
  if (Form <> nil) then
  begin
    if (SQL <> nil) and (SQL <> Form.FSQL) then
      Form.FSQL.Assign(SQL);
    if (vList <> nil) and (vList <> Form.FList) then
      Form.FList.Assign(vList);
  end;
  SQLCMD := Engine.CreateCMD;
  aStrings := TStringList.Create;
  try
    try
      for i := 0 to SQL.Count - 1 do
      begin
        line := Trim(SQL[i]);
        if SameText('-- title=', LeftStr(line,  length('-- title='))) then
        begin
          aTitle := MidStr(line, Length('-- title=') + 1, MaxInt);
        end
        else if SameText(line, '^exit') then
        begin
          break
        end
        else
        if line = '^' then
        begin
          Execute(SQLCMD, aStrings, aTitle, vList);
          aStrings.Clear;
        end
        else
          aStrings.Add(SQL[i]);
      end;
      if aStrings.Count > 0 then
        Execute(SQLCMD, aStrings, aTitle, vList);
      if SQLCMD.Active then
        SQLCMD.Close;
      SQLCMD.Session.Commit(true);
    except
      on E: Exception do
      begin
        SQLCMD.Session.RollbackRetaining;
        raise;
      end
      else
        raise;
    end;
    Engine.Session.CommitRetaining;
  finally
    aStrings.Free;
    SQLCMD.Free;
  end;
end;

end.

