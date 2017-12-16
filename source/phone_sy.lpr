program phone_sy;
{-----------------------------------------------------------------------------
 Author:    zaher dirkey
 Purpose:
 History:
-----------------------------------------------------------------------------}

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, Classes, MsgBox, GUIMsgBox, MainForms, OptionsForms,
  DataForm, psSQLEngine,
  psSqlUtils;

{$R *.res}

begin
  Application.Initialize;
  Application.BidiMode := bdLeftToRight;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

