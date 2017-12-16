unit OptionsForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles, psSQLEngine;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    InteractiveGridChk: TCheckBox;
    CloseBtn: TButton;
    OkBtn: TButton;
    procedure FormCreate(Sender: TObject);
  private
    FOptions: TpsOptions;
  public
    procedure Apply;
    procedure Retrive;
  end;

function ShowOptions(AOptions: TpsOptions): Boolean;

implementation

{$R *.lfm}

function ShowOptions(AOptions: TpsOptions): Boolean;
begin
  with TOptionsForm.Create(Application) do
  begin
    FOptions := AOptions;
    Retrive;
    Result := ShowModal = mrOk;
    if Result then
      Apply;
  end;
end;

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
end;

procedure TOptionsForm.Apply;
var
  ini: TiniFile;
begin
  FOptions.InteractiveGrid := InteractiveGridChk.Checked;
  ini := TiniFile.Create(Application.Location + 'config.ini');
  try
  finally
    ini.Free;
  end;
end;

procedure TOptionsForm.Retrive;
var
  ini: TiniFile;
begin
  InteractiveGridChk.Checked := FOptions.InteractiveGrid;
  if FileExists(Application.Location + 'config.ini') then
  begin
    ini := TiniFile.Create(Application.Location + 'config.ini');
    try
    finally
      ini.Free;
    end;
  end;
end;

end.

