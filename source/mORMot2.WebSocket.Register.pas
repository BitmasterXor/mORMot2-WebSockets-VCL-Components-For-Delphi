unit mORMot2.WebSocket.Register;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  DesignEditors,
  mORMot2.WebSocket.Client,
  mORMot2.WebSocket.Server;

/// Custom property editor for Host property
type
  TWebSocketHostPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

/// Custom property editor for Protocol Name property
type
  TWebSocketProtocolPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
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

function TWebSocketHostPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TWebSocketHostPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
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

function TWebSocketProtocolPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TWebSocketProtocolPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure Register;
begin
  // Register components on the component palette
  RegisterComponents('mORMot2 WebSocket', [
    TmORMot2WebSocketClient,
    TmORMot2WebSocketServer
  ]);

  // Register property editors for enhanced design-time experience
  RegisterPropertyEditor(TypeInfo(string), TmORMot2WebSocketClient, 'Host', TWebSocketHostPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TmORMot2WebSocketClient, 'ProtocolName', TWebSocketProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TmORMot2WebSocketServer, 'BindInterface', TWebSocketHostPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TmORMot2WebSocketServer, 'ProtocolName', TWebSocketProtocolPropertyEditor);
end;

end.
