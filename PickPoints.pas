program Digitizer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufmmain, UPoints
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Digitizer';
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

