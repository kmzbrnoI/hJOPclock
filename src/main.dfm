object F_Main: TF_Main
  Left = 0
  Top = 0
  Caption = 'hJOPclock'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = P_TimeResize
  PixelsPerInch = 96
  TextHeight = 13
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
    Transparent = True
  end
  object AL_Main: TActionList
    Left = 584
    Top = 280
    object A_Seconds: TAction
      Caption = 'Sekundy'
      ShortCut = 16467
      OnExecute = A_SecondsExecute
    end
    object A_Help: TAction
      Caption = 'A_Help'
      ShortCut = 112
      OnExecute = A_HelpExecute
    end
    object A_Maximize: TAction
      Caption = 'A_Maximize'
      ShortCut = 16454
      OnExecute = A_MaximizeExecute
    end
  end
end
