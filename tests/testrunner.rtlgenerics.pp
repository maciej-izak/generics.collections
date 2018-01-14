{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }

program testrunner.rtlgenerics;

{$mode objfpc}{$H+}

uses
  consoletestrunner,
  tests.generics.dictionaries;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'RTL-Generics unit tests';
  Application.Run;
  Application.Free;
{$IFDEF WAIT_FOR_ENTER}
  WriteLn('Press enter...');
  ReadLn;
{$ENDIF}
end.
