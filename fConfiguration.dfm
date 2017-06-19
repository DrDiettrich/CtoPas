object ConfigViewer: TConfigViewer
  Left = 602
  Top = 117
  Width = 463
  Height = 408
  Caption = 'Configuration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbConfig: TMemo
    Left = 0
    Top = 53
    Width = 455
    Height = 328
    Align = alClient
    PopupMenu = PopupMenu1
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = lbConfigChange
  end
  object lbTabs: TTabControl
    Left = 0
    Top = 0
    Width = 455
    Height = 53
    Align = alTop
    RaggedRight = True
    TabOrder = 1
    Tabs.Strings = (
      'User'
      'Main')
    TabIndex = 0
    OnChange = lbTabsChange
    object Label1: TLabel
      Left = 8
      Top = 28
      Width = 25
      Height = 13
      AutoSize = False
      Caption = 'File'
    end
    object edFile: TEdit
      Left = 32
      Top = 24
      Width = 417
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 216
    Top = 220
    object Save1: TMenuItem
      Caption = '&Save'
      OnClick = Save1Click
    end
    object Undo1: TMenuItem
      Caption = '&Undo'
      OnClick = Undo1Click
    end
  end
end
