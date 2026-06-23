{$mode delphi}
unit mORMot2.WebSocket.Register;

interface

procedure Register;

implementation

uses
  Classes,
  LResources,
  PropEdits,
  mORMot2.WebSocket.Client,
  mORMot2.WebSocket.Server;

type
  TWebSocketHostPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TWebSocketProtocolPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TWebSocketHostPropertyEditor }

function TWebSocketHostPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TWebSocketHostPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('localhost');
  Proc('127.0.0.1');
  Proc('0.0.0.0');
end;

{ TWebSocketProtocolPropertyEditor }

function TWebSocketProtocolPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TWebSocketProtocolPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('chat');
  Proc('synopsejson');
  Proc('synopsebin');
  Proc('echo');
  Proc('custom');
end;

procedure Register;
begin
  RegisterComponents('mORMot2 WebSocket', [
    TmORMot2WebSocketClient,
    TmORMot2WebSocketServer
  ]);

  RegisterPropertyEditor(TypeInfo(string), TmORMot2WebSocketClient,
    'Host', TWebSocketHostPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TmORMot2WebSocketServer,
    'WebSocketsURI', TWebSocketProtocolPropertyEditor);
end;

initialization
  {$I mORMot2WebSocketComponents.lrs}

end.
