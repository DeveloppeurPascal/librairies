program ShowDeviceNameSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fShowDeviceName in 'fShowDeviceName.pas' {Form15},
  uGetDeviceName in '..\..\src\uGetDeviceName.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm15, Form15);
  Application.Run;
end.
