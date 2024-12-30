-- | TODO: Write module documentation
--
-- A complete list of the scripts and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/ISO_15924
--
module Ogma.Internal.Script.UnicodeRange
  ( UnicodeRange (..)
  , scriptUnicodeRange
  ) where

import Data.Char (chr)

import Ogma.Internal.Script.Script (Script (..))

newtype UnicodeRange = UnicodeRange [Char]
  deriving newtype (Eq, Show)

scriptUnicodeRange :: Script -> Maybe UnicodeRange
scriptUnicodeRange script =
  case script of
    AdlamScript ->
      Just $ UnicodeRange [ chr 0x1E900..chr 0x1E95F ]

    AfakaScript ->
      Nothing

    AhomScript ->
      Just $ UnicodeRange [ chr 0x11700..chr 0x1173F ]

    AnatolianHieroglyphs ->
      Just $ UnicodeRange [ chr 0x14400..chr 0x1467F ]

    ArabicScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0600..chr 0x06FF ]
          , [ chr 0x0750..chr 0x077F ]
          , [ chr 0x08A0..chr 0x08FF ]
          , [ chr 0x0870..chr 0x089F ]
          , [ chr 0x10EC0..chr 0x10EFF ]
          , [ chr 0xFB50..chr 0xFDFF ]
          , [ chr 0xFE70..chr 0xFEFF ]
          , [ chr 0x1EE00..chr 0x1EEFF ]
          , [ chr 0x1EC70..chr 0x1ECBF ]
          , [ chr 0x1ED00..chr 0x1ED4F ]
          , [ chr 0x10E60..chr 0x10E7F ]
          ]

    Arabic_Nastaliq ->
      Nothing

    ArmenianScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0530..chr 0x058F ]
          , [ chr 0xFB00..chr 0xFB17 ]
          ]

    AvestanScript ->
      Just $ UnicodeRange [ chr 0x10B00..chr 0x10B3F ]

    BalineseScript ->
      Just $ UnicodeRange [ chr 0x1B00..chr 0x1B7F ]

    BamumScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0xA6A0..chr 0xA6FF ]
          , [ chr 0x16800..chr 0x16A3F ]
          ]

    BassaVahScript ->
      Just $ UnicodeRange [ chr 0x16AD0..chr 0x16AFF ]

    BatakScript ->
      Just $ UnicodeRange [ chr 0x1BC0..chr 0x1BFF ]

    BengaliScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0980..chr 0x09FF ]
          , [ chr 0x011480..chr 0x0114DF ]
          ]

    BhaiksukiScript ->
      Just $ UnicodeRange [ chr 0x11C00..chr 0x11C6F ]

    BlissymbolsScript ->
      Nothing

    BookPahlaviScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10B60..chr 0x10B7F ]
          , [ chr 0x10B40..chr 0x10B5F ]
          , [ chr 0x10B80..chr 0x10BAF ]
          ]

    BopomofoScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x3100..chr 0x312F ]
          , [ chr 0x31A0..chr 0x31BF ]
          ]

    BrahmiScript ->
      Just $ UnicodeRange [ chr 0x11000..chr 0x1107F ]

    BrailleScript ->
      Just $ UnicodeRange [ chr 0x2800..chr 0x28FF ]

    BugineseScript ->
      Just $ UnicodeRange [ chr 0x1A00..chr 0x1A1F ]

    BuhidScript ->
      Just $ UnicodeRange [ chr 0x1740..chr 0x175F ]

    CarianScript ->
      Just $ UnicodeRange [ chr 0x102A0..chr 0x102DF ]

    CaucasianAlbanianScript ->
      Just $ UnicodeRange [ chr 0x10530..chr 0x1056F ]

    ChakmaScript ->
      Nothing

    ChamScript ->
      Just $ UnicodeRange [ chr 0xAA00..chr 0xAA5F ]

    CherokeeScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x13A0..chr 0x13FF ]
          , [ chr 0xAB70..chr 0xABBF ]
          ]

    ChisoiScript ->
      Nothing

    ChorasmianScript ->
      Nothing

    CirthScript ->
      Nothing

    CodeForInheritedScript ->
      Nothing

    CodeForUncodedScript ->
      Nothing

    CodeForUndeterminedScript ->
      Nothing

    CodeForUnwrittenDocuments ->
      Nothing

    CopticScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x2C80..chr 0x2CFF ]
          , [ chr 0x0370..chr 0x03FF ]
          , [ chr 0x102E0..chr 0x102FF ]
          ]

    CuneiformScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x12000..chr 0x123FF ]
          , [ chr 0x12400..chr 0x1247F ]
          ]

    CypriotSyllabary ->
      Just $ UnicodeRange [ chr 0x10800..chr 0x1083F ]

    CyproMinoanScript ->
      Just $ UnicodeRange [ chr 0x12F90..chr 0x12FFF ]

    CyrillicScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0400..chr 0x04FF ]
          , [ chr 0x0500..chr 0x052F ]
          , [ chr 0x2DE0..chr 0x2DFF ]
          , [ chr 0xA640..chr 0xA69F ]
          , [ chr 0x1C80..chr 0x1C8F ]
          , [ chr 0x1E030..chr 0x1E08F ]
          ]

    Cyrillic_OldChurchSlavonic ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0400..chr 0x04FF ]
          , [ chr 0x0500..chr 0x052F ]
          , [ chr 0x2DE0..chr 0x2DFF ]
          , [ chr 0xA640..chr 0xA69F ]
          , [ chr 0x1C80..chr 0x1C8F ]
          ]

    DeseretScript ->
      Just $ UnicodeRange [ chr 0x10400..chr 0x1044F ]

    DevanagariScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0900..chr 0x097F ]
          , [ chr 0xA8E0..chr 0xA8FF ]
          , [ chr 0x11B00..chr 0xB5F ]
          , [ chr 0x1CD0..chr 0x1CFF ]
          ]

    DivesAkuruScript ->
      Just $ UnicodeRange [ chr 0x11900..chr 0x1195F ]

    DograScript ->
      Just $ UnicodeRange [ chr 0x11800..chr 0x1184F ]

    DuployanShorthand ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1BC00..chr 0x1BC9F ]
          , [ chr 0x1BCA0..chr 0x1BCAF ]
          ]

    EgyptianDemoticScript ->
      Nothing

    EgyptianHieraticScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x13000..chr 0x1342F ]
          , [ chr 0x13460..chr 0x143FF ]
          , [ chr 0x13430..chr 0x1345F ]
          ]

    EgyptianHieroglyphs ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x13000..chr 0x1342F ]
          , [ chr 0x13460..chr 0x143FF ]
          , [ chr 0x13430..chr 0x1345F ]
          ]

    ElbasanScript ->
      Just $ UnicodeRange [ chr 0x10500..chr 0x1052F ]

    ElymaicScript ->
      Just $ UnicodeRange [ chr 0x10FE0..chr 0x10FFF ]

    EthiopicScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1200..chr 0x137F ]
          , [ chr 0x1380..chr 0x139F ]
          , [ chr 0x2D80..chr 0x2DDF ]
          , [ chr 0xAB00..chr 0xAB2F ]
          , [ chr 0x1E7E0..chr 0x1E7FF ]
          ]

    GarayScript ->
      Just $ UnicodeRange [ chr 0x10D40..chr 0x10D8F ]

    GeorgianScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10A0..chr 0x10FF ]
          , [ chr 0x2D00..chr 0x2D2F ]
          , [ chr 0x1C90..chr 0x1CBF ]
          ]

    GlagoliticScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x2C00..chr 0x2C5F ]
          , [ chr 0x1E000..chr 0x1E02F ]
          ]

    GothicScript ->
      Just $ UnicodeRange [ chr 0x10330..chr 0x1034F ]

    GranthaScript ->
      Just $ UnicodeRange [ chr 0x11300..chr 0x1137F ]

    GreekScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0370..chr 0x03FF ]
          , [ chr 0x1F00..chr 0x1FFF ]
          ]

    GujaratiScript ->
      Just $ UnicodeRange [ chr 0x0A80..chr 0x0AFF ]

    GunjalaGondiScript ->
      Just $ UnicodeRange [ chr 0x11D60..chr 0x11DAF ]

    GurmukhiScript ->
      Just $ UnicodeRange [ chr 0x0A00..chr 0x0A7F ]

    GurungKhemaScript ->
      Just $ UnicodeRange [ chr 0x16100..chr 0x1613F ]

    HanScript ->
      Just $ UnicodeRange [ chr 0x4E00..chr 0x9FFF ]

    HanWithBopomofoScript ->
      Just $ UnicodeRange [ chr 0x4E00..chr 0x9FFF ]

    Han_Simplified ->
      Nothing

    Han_Traditional ->
      Nothing

    HangulScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0xAC00..chr 0xD7AF ]
          , [ chr 0x1100..chr 0x11FF ]
          , [ chr 0x3130..chr 0x318F ]
          , [ chr 0xA960..chr 0xA97F ]
          , [ chr 0xD7B0..chr 0xD7FF ]
          ]

    HanifiRohingyaScript ->
      Just $ UnicodeRange [ chr 0x10D00..chr 0x10D3F ]

    HanunooScript ->
      Just $ UnicodeRange [ chr 0x1720..chr 0x173F ]

    HatranScript ->
      Nothing

    HebrewScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0590..chr 0x05FF ]
          , [ chr 0xFB1D..chr 0xFB4F ]
          ]

    HiraganaScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x3040..chr 0x309F ]
          , [ chr 0x1AFF0..chr 0x1AFFF ]
          , [ chr 0x1B000..chr 0x1B0FF ]
          , [ chr 0x1B100..chr 0x1B12F ]
          , [ chr 0x1B130..chr 0x1B16F ]
          ]

    ImperialAramaicScript ->
      Just $ UnicodeRange [ chr 0x10840..chr 0x1085F ]

    IndusScript ->
      Nothing

    InscriptionalPahlaviScript ->
      Just $ UnicodeRange [ chr 0x10B60..chr 0x10B7F ]

    InscriptionalParthianScript ->
      Just $ UnicodeRange [ chr 0x10B40..chr 0x10B5F ]

    JamoScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0xAC00..chr 0xD7AF ]
          , [ chr 0x1100..chr 0x11FF ]
          , [ chr 0x3130..chr 0x318F ]
          , [ chr 0xA960..chr 0xA97F ]
          , [ chr 0xD7B0..chr 0xD7FF ]
          ]

    JapaneseScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x4E00..chr 0x9FBF ]
          , [ chr 0x3040..chr 0x309F ]
          , [ chr 0x30A0..chr 0x30FF ]
          ]

    JapaneseSyllabaries ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x3040..chr 0x309F ]
          , [ chr 0x30A0..chr 0x30FF ]
          ]

    JavaneseScript ->
      Just $ UnicodeRange [ chr 0xA980..chr 0xA9DF ]

    JurchenScript ->
      Nothing

    KaithiScript ->
      Just $ UnicodeRange [ chr 0x11080..chr 0x110CF ]

    KannadaScript ->
      Just $ UnicodeRange [ chr 0x0C80..chr 0x0CFF ]

    KatakanaScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x30A0..chr 0x30FF ]
          , [ chr 0x31F0..chr 0x31FF ]
          , [ chr 0x3200..chr 0x32FF ]
          , [ chr 0xFF00..chr 0xFFEF ]
          , [ chr 0x1AFF0..chr 0x1AFFF ]
          , [ chr 0x1B000..chr 0x1B0FF ]
          , [ chr 0x1B100..chr 0x1B12F ]
          , [ chr 0x1B130..chr 0x1B16F ]
          ]

    KawiScript ->
      Just $ UnicodeRange [ chr 0x11F00..chr 0x11F5F ]

    KayahLiScript ->
      Just $ UnicodeRange [ chr 0xA900..chr 0xA92F ]

    KharoshthiScript ->
      Just $ UnicodeRange [ chr 0x10A00..chr 0x10A5F ]

    KhitanLargeScript ->
      Nothing

    KhitanSmallScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x18B00..chr 0x18CFF ]
          , [ chr 0x16FE0..chr 0x16FFF ]
          ]

    KhmerScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1780..chr 0x17FF ]
          , [ chr 0x19E0..chr 0x19FF ]
          ]

    KhojkiScript ->
      Just $ UnicodeRange [ chr 0x11200..chr 0x1124F ]

    KhudawadiScript ->
      Just $ UnicodeRange [ chr 0x112B0..chr 0x112FF ]

    KhutsuriScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10A0..chr 0x10FF ]
          , [ chr 0x2D00..chr 0x2D2F ]
          , [ chr 0x1C90..chr 0x1CBF ]
          ]

    KiratRaiScript ->
      Just $ UnicodeRange [ chr 0x16D40..chr 0x16D7F ]

    KlingonScript ->
      Nothing

    KoreanScript ->
      Nothing

    KpelleScript ->
      Nothing

    LaoScript ->
      Just $ UnicodeRange [ chr 0x0E80..chr 0x0EFF ]

    LatinScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0000..chr 0x007F ]
          , [ chr 0x0080..chr 0x00FF ]
          , [ chr 0x0100..chr 0x017F ]
          , [ chr 0x0180..chr 0x024F ]
          , [ chr 0x1E00..chr 0x1EFF ]
          ]

    Latin_Fraktur ->
      Just $ UnicodeRange [ chr 0x0020..chr 0x00FF ]

    Latin_Gaelic ->
      Nothing

    LekeScript ->
      Nothing

    LepchaScript ->
      Just $ UnicodeRange [ chr 0x1C00..chr 0x1C4F ]

    LimbuScript ->
      Just $ UnicodeRange [ chr 0x1900..chr 0x194F ]

    LinearAScript ->
      Just $ UnicodeRange [ chr 0x10600..chr 0x1077F ]

    LinearBScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10000..chr 0x1007F ]
          , [ chr 0x10080..chr 0x100FF ]
          ]

    LisuScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0xA4D0..chr 0xA4FF ]
          , [ chr 0x11FB0..chr 0x11FBF ]
          ]

    LomaScript ->
      Nothing

    LycianScript ->
      Just $ UnicodeRange [ chr 0x10280..chr 0x1029F ]

    LydianScript ->
      Just $ UnicodeRange [ chr 0x10920..chr 0x1093F ]

    MahajaniScript ->
      Just $ UnicodeRange [ chr 0x11150..chr 0x1117F ]

    MakasarScript ->
      Just $ UnicodeRange [ chr 0x11EE0..chr 0x11EFF ]

    MalayalamScript ->
      Just $ UnicodeRange [ chr 0x0D00..chr 0x0D7F ]

    MandaicScript ->
      Just $ UnicodeRange [ chr 0x0840..chr 0x085F ]

    ManichaeanScript ->
      Just $ UnicodeRange [ chr 0x10AC0..chr 0x10AFF ]

    MarchenScript ->
      Just $ UnicodeRange [ chr 0x11C70..chr 0x11CBF ]

    MasaramGondiScript ->
      Just $ UnicodeRange [ chr 0x11D00..chr 0x11D5F ]

    MathematicalNotationScript ->
      Nothing

    MayanHieroglyphs ->
      Nothing

    MedefaidrinScript ->
      Nothing

    MeiteiMayekScript ->
      Just $ UnicodeRange [ chr 0xABC0..chr 0xABFF ]

    MendeKikakuiScript ->
      Just $ UnicodeRange [ chr 0x1E800..chr 0x1E8DF ]

    MeroiticCursive ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10980..chr 0x1099F ]
          , [ chr 0x109A0..chr 0x109FF ]
          ]

    MeroiticHieroglyphs ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10980..chr 0x1099F ]
          , [ chr 0x109A0..chr 0x109FF ]
          ]

    MiaoScript ->
      Just $ UnicodeRange [ chr 0x16F00..chr 0x16F9F ]

    ModiScript ->
      Just $ UnicodeRange [ chr 0x11600..chr 0x1165F ]

    MongolianScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1800..chr 0x18AF ]
          , [ chr 0x11660..chr 0x1167F ]
          ]

    MoonScript ->
      Nothing

    MroScript ->
      Nothing

    MultaniScript ->
      Just $ UnicodeRange [ chr 0x11280..chr 0x112AF ]

    MyanmarScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1000..chr 0x109F ]
          , [ chr 0xAA60..chr 0xAA7F ]
          , [ chr 0xA9E0..chr 0xA9FF ]
          , [ chr 0x116D0..chr 0x116FF ]
          ]

    NabataeanScript ->
      Just $ UnicodeRange [ chr 0x10880..chr 0x108AF ]

    NagMundariScript ->
      Just $ UnicodeRange [ chr 0x1E4D0..chr 0x1E4FF ]

    NandinagariScript ->
      Just $ UnicodeRange [ chr 0x119A0..chr 0x119FF ]

    NaxiDongbaScript ->
      Nothing

    NaxiGebaScript ->
      Nothing

    NewTaiLueScript ->
      Just $ UnicodeRange [ chr 0x1980..chr 0x19DF ]

    NewaScript ->
      Just $ UnicodeRange [ chr 0x11400..chr 0x1147F ]

    NkoScript ->
      Just $ UnicodeRange [ chr 0x07C0..chr 0x07FF ]

    NushuScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1B170..chr 0x1B2FF ]
          , [ chr 0x16FE0..chr 0x16FFF ]
          ]

    NyiakengPuachueHmongScript ->
      Just $ UnicodeRange [ chr 0x1E100..chr 0x1E14F ]

    OghamScript ->
      Just $ UnicodeRange [ chr 0x1680..chr 0x169F ]

    OlChikiScript ->
      Just $ UnicodeRange [ chr 0x1C50..chr 0x1C7F ]

    OlOnalScript ->
      Just $ UnicodeRange [ chr 0x1E5D0..chr 0x1E5FF ]

    OldHungarianScript ->
      Just $ UnicodeRange [ chr 0x10C80..chr 0x10CFF ]

    OldItalicScript ->
      Just $ UnicodeRange [ chr 0x10300..chr 0x1032F ]

    OldNorthArabianScript ->
      Just $ UnicodeRange [ chr 0x10A80..chr 0x10A9F ]

    OldPermicScript ->
      Just $ UnicodeRange [ chr 0x10350..chr 0x1037F ]

    OldPersianScript ->
      Just $ UnicodeRange [ chr 0x103A0..chr 0x103D5 ]

    OldSogdianScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10F00..chr 0x10F2F ]
          , [ chr 0x10F30..chr 0x10F6F ]
          ]

    OldSouthArabianScript ->
      Just $ UnicodeRange [ chr 0x10A60..chr 0x10A7F ]

    OldTurkicScript ->
      Just $ UnicodeRange [ chr 0x10C00..chr 0x10C4F ]

    OldUyghurScript ->
      Just $ UnicodeRange [ chr 0x10F70..chr 0x10FAF ]

    OriyaScript ->
      Just $ UnicodeRange [ chr 0x0B00..chr 0x0B7F ]

    OsageScript ->
      Just $ UnicodeRange [ chr 0x104B0..chr 0x104FF ]

    OsmanyaScript ->
      Just $ UnicodeRange [ chr 0x10480..chr 0x184AF ]

    PahawhHmongScript ->
      Just $ UnicodeRange [ chr 0x16B00..chr 0x16B8F ]

    PalmyreneScript ->
      Just $ UnicodeRange [ chr 0x10860..chr 0x1087F ]

    PauCinHauScript ->
      Just $ UnicodeRange [ chr 0x11AC0..chr 0x11AFF ]

    PhagsPaScript ->
      Just $ UnicodeRange [ chr 0xA840..chr 0xA87F ]

    PhoenicianScript ->
      Just $ UnicodeRange [ chr 0x10900..chr 0x1091F ]

    ProtoCuneiformScript ->
      Nothing

    ProtoElamiteScript ->
      Nothing

    ProtoSinaiticScript ->
      Nothing

    PsalterPahlaviScript ->
      Just $ UnicodeRange [ chr 0x10B80..chr 0x10BAF ]

    RanjanaScript ->
      Nothing

    RejangScript ->
      Just $ UnicodeRange [ chr 0xA930..chr 0xA95F ]

    RongorongoScript ->
      Nothing

    RunicScript ->
      Just $ UnicodeRange [ chr 0x16A0..chr 0x16FF ]

    SamaritanScript ->
      Just $ UnicodeRange [ chr 0x0800..chr 0x083F ]

    SaratiScript ->
      Nothing

    SaurashtraScript ->
      Just $ UnicodeRange [ chr 0xA880..chr 0xA8DF ]

    SharadaScript ->
      Just $ UnicodeRange [ chr 0x11180..chr 0x111DF ]

    ShavianScript ->
      Just $ UnicodeRange [ chr 0x10450..chr 0x1047F ]

    ShuishuScript ->
      Nothing

    SiddhamScript ->
      Just $ UnicodeRange [ chr 0x11580..chr 0x115FF ]

    SideticScript ->
      Nothing

    Signwriting ->
      Just $ UnicodeRange [ chr 0x1D800..chr 0x1DAAF ]

    SinhalaScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0D80..chr 0x0DFF ]
          , [ chr 0x111E0..chr 0x111FF ]
          ]

    SogdianScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x10F00..chr 0x10F2F ]
          , [ chr 0x10F30..chr 0x10F6F ]
          ]

    SoraSompengScript ->
      Just $ UnicodeRange [ chr 0x110D0..chr 0x110FF ]

    SoyomboScript ->
      Just $ UnicodeRange [ chr 0x11A50..chr 0x11AAF ]

    SundaneseScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1B80..chr 0x1BBF ]
          , [ chr 0x1CC0..chr 0x1CCF ]
          ]

    SunuwarScript ->
      Just $ UnicodeRange [ chr 0x11BC0..chr 0x11BFF ]

    SylotiNagriScript ->
      Just $ UnicodeRange [ chr 0xA800..chr 0xA82F ]

    Symbols ->
      Nothing

    Symbols_Emoji ->
      Nothing

    SyriacScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0700..chr 0x074F ]
          , [ chr 0x0860..chr 0x086F ]
          ]

    Syriac_Eastern ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0700..chr 0x074F ]
          , [ chr 0x0860..chr 0x086F ]
          ]

    Syriac_Estrangelo ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0700..chr 0x074F ]
          , [ chr 0x0860..chr 0x086F ]
          ]

    Syriac_Western ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0700..chr 0x074F ]
          , [ chr 0x0860..chr 0x086F ]
          ]

    TagalogScript ->
      Just $ UnicodeRange [ chr 0x1700..chr 0x171F ]

    TagbanwaScript ->
      Just $ UnicodeRange [ chr 0x1760..chr 0x177F ]

    TaiLeScript ->
      Just $ UnicodeRange [ chr 0x1950..chr 0x197F ]

    TaiThamScript ->
      Just $ UnicodeRange [ chr 0x1A20..chr 0x1AAF ]

    TaiVietScript ->
      Just $ UnicodeRange [ chr 0xAA80..chr 0xAADF ]

    TaiYoScript ->
      Nothing

    TakriScript ->
      Just $ UnicodeRange [ chr 0x11680..chr 0x116CF ]

    TamilScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x0B80..chr 0x0BFF ]
          , [ chr 0x11FC0..chr 0x11FFF ]
          ]

    TangsaScript ->
      Nothing

    TangutScript ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x17000..chr 0x187FF ]
          , [ chr 0x18D00..chr 0x18D7F ]
          , [ chr 0x18800..chr 0x18AFF ]
          , [ chr 0x16FE0..chr 0x16FFF ]
          ]

    TeluguScript ->
      Just $ UnicodeRange [ chr 0x0C00..chr 0x0C7F ]

    TengwarScript ->
      Nothing

    ThaanaScript ->
      Just $ UnicodeRange [ chr 0x0780..chr 0x07BF ]

    ThaiScript ->
      Just $ UnicodeRange [ chr 0x0E00..chr 0x0E7F ]

    TibetanScript ->
      Just $ UnicodeRange [ chr 0x0F00..chr 0x0FFF ]

    TifinaghScript ->
      Nothing

    TirhutaScript ->
      Just $ UnicodeRange [ chr 0x11480..chr 0x114DF ]

    TodhriScript ->
      Just $ UnicodeRange [ chr 0x105C0..chr 0x105FF ]

    TolongSikiScript ->
      Nothing

    TotoScript ->
      Nothing

    TuluTigalariScript ->
      Just $ UnicodeRange [ chr 0x11380..chr 0x113FF ]

    UgariticScript ->
      Just $ UnicodeRange [ chr 0x10380..chr 0x1039F ]

    UnifiedCanadianAboriginalSyllabics ->
      Just
        . UnicodeRange
        . concat
        $ [ [ chr 0x1400..chr 0x167F ]
          , [ chr 0x18B0..chr 0x18FF ]
          , [ chr 0x11AB0..chr 0x11ABF ]
          ]

    VaiScript ->
      Just $ UnicodeRange [ chr 0xA500..chr 0xA63F ]

    VisibleSpeech ->
      Nothing

    VithkuqiScript ->
      Just $ UnicodeRange [ chr 0x10570..chr 0x105BF ]

    WanchoScript ->
      Just $ UnicodeRange [ chr 0x1E2C0..chr 0x1E2FF ]

    WarangCitiScript ->
      Just $ UnicodeRange [ chr 0x118A0..chr 0x118FF ]

    WoleaiScript ->
      Nothing

    YezidiScript ->
      Nothing

    YiScript ->
      Nothing

    ZanabazarSquareScript ->
      Just $ UnicodeRange [ chr 0x11A00..chr 0x11A4F ]