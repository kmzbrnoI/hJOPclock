object F_Main: TF_Main
  Left = 0
  Top = 0
  Caption = 'hJOPclock'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object P_Time: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 337
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = P_TimeResize
    ExplicitLeft = 10
    ExplicitTop = 10
    ExplicitWidth = 607
    ExplicitHeight = 311
    object L_Time: TLabel
      Left = 16
      Top = 72
      Width = 580
      Height = 164
      Align = alCustom
      Alignment = taCenter
      Caption = 'Odpojeno'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -136
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object AL_Main: TActionList
    Left = 584
    Top = 280
    object A_Seconds: TAction
      Caption = 'Sekundy'
      ShortCut = 16467
      OnExecute = A_SecondsExecute
    end
  end
end
