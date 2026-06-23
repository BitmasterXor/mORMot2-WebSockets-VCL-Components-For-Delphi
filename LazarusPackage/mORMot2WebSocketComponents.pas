{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mORMot2WebSocketComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  mORMot2.WebSocket.Client, mORMot2.WebSocket.Server, 
  mORMot2.WebSocket.Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mORMot2.WebSocket.Register', 
    @mORMot2.WebSocket.Register.Register);
end;

initialization
  RegisterPackage('mORMot2WebSocketComponents', @Register);
end.
