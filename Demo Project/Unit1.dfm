object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'mORMot2 WebSocket Components Demo v2.0.5'
  ClientHeight = 700
  ClientWidth = 1200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1200
    700)
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 1184
    Height = 684
    ActivePage = TabServer
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabServer: TTabSheet
      Caption = 'Server'
      object GroupBoxServer: TGroupBox
        Left = 3
        Top = 3
        Width = 570
        Height = 650
        Caption = ' WebSocket Server '
        TabOrder = 0
        object LabelServerPort: TLabel
          Left = 16
          Top = 24
          Width = 24
          Height = 13
          Caption = 'Port:'
        end
        object LabelServerStatus: TLabel
          Left = 16
          Top = 56
          Width = 90
          Height = 13
          Caption = 'Status: Stopped'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelClientCount: TLabel
          Left = 16
          Top = 75
          Width = 100
          Height = 13
          Caption = 'Connected Clients: 0'
        end
        object EditServerPort: TEdit
          Left = 56
          Top = 21
          Width = 80
          Height = 21
          TabOrder = 0
          Text = '8080'
        end
        object ButtonStartServer: TButton
          Left = 152
          Top = 19
          Width = 80
          Height = 25
          Caption = 'Start'
          TabOrder = 1
          OnClick = ButtonStartServerClick
        end
        object ButtonStopServer: TButton
          Left = 240
          Top = 19
          Width = 80
          Height = 25
          Caption = 'Stop'
          Enabled = False
          TabOrder = 2
          OnClick = ButtonStopServerClick
        end
        object MemoServerLog: TMemo
          Left = 16
          Top = 104
          Width = 540
          Height = 280
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 3
        end
        object ListBoxClients: TListBox
          Left = 16
          Top = 400
          Width = 240
          Height = 120
          ItemHeight = 13
          TabOrder = 4
        end
        object EditServerMessage: TEdit
          Left = 16
          Top = 530
          Width = 240
          Height = 21
          TabOrder = 5
          Text = 'Hello from server!'
        end
        object ButtonSendToSelected: TButton
          Left = 272
          Top = 400
          Width = 120
          Height = 25
          Caption = 'Send to Selected'
          TabOrder = 6
          OnClick = ButtonSendToSelectedClick
        end
        object ButtonBroadcast: TButton
          Left = 272
          Top = 431
          Width = 120
          Height = 25
          Caption = 'Broadcast to All'
          TabOrder = 7
          OnClick = ButtonBroadcastClick
        end
      end
      object GroupBoxStats: TGroupBox
        Left = 579
        Top = 3
        Width = 290
        Height = 200
        Caption = ' Server Statistics '
        TabOrder = 1
        object LabelTotalConnections: TLabel
          Left = 16
          Top = 24
          Width = 128
          Height = 13
          Caption = 'Total Connections Made: 0'
        end
        object LabelBytesReceived: TLabel
          Left = 16
          Top = 48
          Width = 87
          Height = 13
          Caption = 'Bytes Received: 0'
        end
        object LabelBytesSent: TLabel
          Left = 16
          Top = 72
          Width = 65
          Height = 13
          Caption = 'Bytes Sent: 0'
        end
        object LabelMessagesReceived: TLabel
          Left = 16
          Top = 96
          Width = 107
          Height = 13
          Caption = 'Messages Received: 0'
        end
        object LabelMessagesSent: TLabel
          Left = 16
          Top = 120
          Width = 85
          Height = 13
          Caption = 'Messages Sent: 0'
        end
      end
    end
    object TabClient: TTabSheet
      Caption = 'Client'
      ImageIndex = 1
      object GroupBoxClient: TGroupBox
        Left = 3
        Top = 3
        Width = 570
        Height = 650
        Caption = ' WebSocket Client '
        TabOrder = 0
        object LabelClientPort: TLabel
          Left = 280
          Top = 24
          Width = 24
          Height = 13
          Caption = 'Port:'
        end
        object LabelClientStatus: TLabel
          Left = 16
          Top = 80
          Width = 119
          Height = 13
          Caption = 'Status: Disconnected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ImageReceived: TImage
          Left = 16
          Top = 360
          Width = 540
          Height = 280
          Center = True
          Proportional = True
          Stretch = True
        end
        object EditServerIP: TEdit
          Left = 16
          Top = 21
          Width = 120
          Height = 21
          TabOrder = 0
          Text = 'localhost'
        end
        object EditClientPort: TEdit
          Left = 312
          Top = 21
          Width = 80
          Height = 21
          TabOrder = 1
          Text = '8080'
        end
        object ButtonConnect: TButton
          Left = 16
          Top = 48
          Width = 80
          Height = 25
          Caption = 'Connect'
          TabOrder = 2
          OnClick = ButtonConnectClick
        end
        object ButtonDisconnect: TButton
          Left = 104
          Top = 48
          Width = 80
          Height = 25
          Caption = 'Disconnect'
          Enabled = False
          TabOrder = 3
          OnClick = ButtonDisconnectClick
        end
        object MemoClientLog: TMemo
          Left = 16
          Top = 104
          Width = 540
          Height = 200
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 4
        end
        object EditClientMessage: TEdit
          Left = 16
          Top = 320
          Width = 300
          Height = 21
          TabOrder = 5
          Text = 'Hello from client!'
        end
        object ButtonSendText: TButton
          Left = 328
          Top = 318
          Width = 100
          Height = 25
          Caption = 'Send Text'
          Enabled = False
          TabOrder = 6
          OnClick = ButtonSendTextClick
        end
        object ButtonSendScreenshot: TButton
          Left = 440
          Top = 318
          Width = 116
          Height = 25
          Caption = 'Send Screenshot'
          Enabled = False
          TabOrder = 7
          OnClick = ButtonSendScreenshotClick
        end
        object CheckBox1: TCheckBox
          Left = 280
          Top = 48
          Width = 276
          Height = 17
          Caption = 'Auto Client Re-Connect'
          Checked = True
          State = cbChecked
          TabOrder = 8
          OnClick = CheckBox1Click
        end
      end
    end
    object TabSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 2
      object GroupBoxEncryption: TGroupBox
        Left = 3
        Top = 3
        Width = 870
        Height = 300
        Caption = ' Encryption Settings '
        TabOrder = 0
        object LabelClientMode: TLabel
          Left = 344
          Top = 78
          Width = 100
          Height = 13
          Caption = 'Client Encrypt Mode:'
        end
        object LabelClientKey: TLabel
          Left = 392
          Top = 50
          Width = 52
          Height = 13
          Caption = 'Client Key:'
        end
        object LabelServerKey: TLabel
          Left = 16
          Top = 47
          Width = 57
          Height = 13
          Caption = 'Server Key:'
        end
        object Label1: TLabel
          Left = 43
          Top = 79
          Width = 30
          Height = 13
          Caption = 'Mode:'
        end
        object CheckBoxServerEncryption: TCheckBox
          Left = 16
          Top = 24
          Width = 150
          Height = 17
          Caption = 'Enable Server Encryption'
          TabOrder = 0
          OnClick = CheckBoxServerEncryptionClick
        end
        object CheckBoxClientEncryption: TCheckBox
          Left = 450
          Top = 24
          Width = 150
          Height = 17
          Caption = 'Enable Client Encryption'
          TabOrder = 1
          OnClick = CheckBoxClientEncryptionClick
        end
        object ComboBoxServerKeySize: TComboBox
          Left = 79
          Top = 47
          Width = 150
          Height = 21
          Style = csDropDownList
          TabOrder = 2
        end
        object ComboBoxServerMode: TComboBox
          Left = 79
          Top = 76
          Width = 150
          Height = 21
          Style = csDropDownList
          TabOrder = 3
        end
        object EditServerKey: TEdit
          Left = 16
          Top = 103
          Width = 213
          Height = 21
          TabOrder = 4
          Text = 'MySecretKey123!'
        end
        object ComboBoxClientKeySize: TComboBox
          Left = 450
          Top = 47
          Width = 150
          Height = 21
          Style = csDropDownList
          TabOrder = 5
        end
        object ComboBoxClientMode: TComboBox
          Left = 450
          Top = 75
          Width = 150
          Height = 21
          Style = csDropDownList
          TabOrder = 6
        end
        object EditClientKey: TEdit
          Left = 344
          Top = 102
          Width = 256
          Height = 21
          TabOrder = 7
          Text = 'MySecretKey123!'
        end
      end
    end
  end
  object Server: TmORMot2WebSocketServer
    Name = 'mMServer1'
    WebSocketsURI = 'websocket'
    Description = 'mORMot2 WebSocket Server Component with COMPLETE FEATURE SET'
    MaxConnections = 200
    Version = '2.0.5'
    OnClientConnected = ServerClientConnected
    OnClientDisconnected = ServerClientDisconnected
    OnError = ServerError
    OnDataReceived = ServerDataReceived
    OnDataSent = ServerDataSent
    OnHandleCommand = ServerHandleCommand
    OnServerStateChange = ServerServerStateChange
    Left = 160
    Top = 216
  end
  object Client: TmORMot2WebSocketClient
    Name = ''
    Host = 'localhost'
    URI = '/'
    Version = '2.0.4'
    OnConnect = ClientConnect
    OnDataReceived = ClientDataReceived
    OnDataSent = ClientDataSent
    OnDisconnect = ClientDisconnect
    OnError = ClientError
    OnHandleCommand = ClientHandleCommand
    OnStateChange = ClientStateChange
    OnReconnecting = ClientReconnecting
    OnReconnectFailed = ClientReconnectFailed
    Left = 288
    Top = 216
  end
  object TimerStats: TTimer
    OnTimer = TimerStatsTimer
    Left = 224
    Top = 216
  end
end
