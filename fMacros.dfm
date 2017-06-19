object MacroChecker: TMacroChecker
  Left = 516
  Top = 113
  Width = 421
  Height = 453
  Caption = 'Macro Checker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edMac: TMemo
    Left = 0
    Top = 321
    Width = 413
    Height = 105
    Align = alBottom
    Lines.Strings = (
      'edMac')
    TabOrder = 0
  end
  object pnlConst: TGroupBox
    Left = 0
    Top = 33
    Width = 129
    Height = 288
    Align = alLeft
    Caption = 'constants'
    TabOrder = 1
  end
  object pnlProc: TGroupBox
    Left = 264
    Top = 33
    Width = 149
    Height = 288
    Align = alRight
    Caption = 'procedures'
    TabOrder = 2
  end
  object pnlUnk: TGroupBox
    Left = 129
    Top = 33
    Width = 135
    Height = 288
    Align = alClient
    Caption = 'unclassified'
    TabOrder = 3
    object lbMac: TListBox
      Left = 2
      Top = 15
      Width = 131
      Height = 271
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbMacClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 413
    Height = 33
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 4
  end
end
