module Ogma.Internal.Script.ISO_15924_Number
  ( ISO_15924_Number
  , iso15924NumberFromText
  , iso15924NumberToBytes
  , iso15924NumberToText
  , scriptISO15924Number
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

import Ogma.Internal.Script.Script (Script (..))

newtype ISO_15924_Number =
  ISO_15924_Number
    { unISO_15924Number :: String
    } deriving newtype (Eq, Show)

iso15924NumberFromText :: T.Text -> Either String ISO_15924_Number
iso15924NumberFromText txt =
  case T.toLower txt of
    "166" -> Right $ ISO_15924_Number "166"
    "439" -> Right $ ISO_15924_Number "439"
    "338" -> Right $ ISO_15924_Number "338"
    "080" -> Right $ ISO_15924_Number "080"
    "160" -> Right $ ISO_15924_Number "160"
    "161" -> Right $ ISO_15924_Number "161"
    "230" -> Right $ ISO_15924_Number "230"
    "134" -> Right $ ISO_15924_Number "134"
    "360" -> Right $ ISO_15924_Number "360"
    "435" -> Right $ ISO_15924_Number "435"
    "259" -> Right $ ISO_15924_Number "259"
    "365" -> Right $ ISO_15924_Number "365"
    "325" -> Right $ ISO_15924_Number "325"
    "334" -> Right $ ISO_15924_Number "334"
    "550" -> Right $ ISO_15924_Number "550"
    "133" -> Right $ ISO_15924_Number "133"
    "285" -> Right $ ISO_15924_Number "285"
    "300" -> Right $ ISO_15924_Number "300"
    "570" -> Right $ ISO_15924_Number "570"
    "367" -> Right $ ISO_15924_Number "367"
    "372" -> Right $ ISO_15924_Number "372"
    "201" -> Right $ ISO_15924_Number "201"
    "239" -> Right $ ISO_15924_Number "239"
    "349" -> Right $ ISO_15924_Number "349"
    "358" -> Right $ ISO_15924_Number "358"
    "445" -> Right $ ISO_15924_Number "445"
    "298" -> Right $ ISO_15924_Number "298"
    "109" -> Right $ ISO_15924_Number "109"
    "291" -> Right $ ISO_15924_Number "291"
    "994" -> Right $ ISO_15924_Number "994"
    "999" -> Right $ ISO_15924_Number "999"
    "998" -> Right $ ISO_15924_Number "998"
    "997" -> Right $ ISO_15924_Number "997"
    "204" -> Right $ ISO_15924_Number "204"
    "020" -> Right $ ISO_15924_Number "020"
    "403" -> Right $ ISO_15924_Number "403"
    "402" -> Right $ ISO_15924_Number "402"
    "220" -> Right $ ISO_15924_Number "220"
    "221" -> Right $ ISO_15924_Number "221"
    "250" -> Right $ ISO_15924_Number "250"
    "315" -> Right $ ISO_15924_Number "315"
    "342" -> Right $ ISO_15924_Number "342"
    "328" -> Right $ ISO_15924_Number "328"
    "755" -> Right $ ISO_15924_Number "755"
    "070" -> Right $ ISO_15924_Number "070"
    "060" -> Right $ ISO_15924_Number "060"
    "050" -> Right $ ISO_15924_Number "050"
    "226" -> Right $ ISO_15924_Number "226"
    "128" -> Right $ ISO_15924_Number "128"
    "430" -> Right $ ISO_15924_Number "430"
    "164" -> Right $ ISO_15924_Number "164"
    "240" -> Right $ ISO_15924_Number "240"
    "225" -> Right $ ISO_15924_Number "225"
    "206" -> Right $ ISO_15924_Number "206"
    "343" -> Right $ ISO_15924_Number "343"
    "200" -> Right $ ISO_15924_Number "200"
    "320" -> Right $ ISO_15924_Number "320"
    "312" -> Right $ ISO_15924_Number "312"
    "310" -> Right $ ISO_15924_Number "310"
    "397" -> Right $ ISO_15924_Number "397"
    "500" -> Right $ ISO_15924_Number "500"
    "503" -> Right $ ISO_15924_Number "503"
    "501" -> Right $ ISO_15924_Number "501"
    "502" -> Right $ ISO_15924_Number "502"
    "286" -> Right $ ISO_15924_Number "286"
    "167" -> Right $ ISO_15924_Number "167"
    "371" -> Right $ ISO_15924_Number "371"
    "127" -> Right $ ISO_15924_Number "127"
    "125" -> Right $ ISO_15924_Number "125"
    "410" -> Right $ ISO_15924_Number "410"
    "124" -> Right $ ISO_15924_Number "124"
    "610" -> Right $ ISO_15924_Number "610"
    "131" -> Right $ ISO_15924_Number "131"
    "130" -> Right $ ISO_15924_Number "130"
    "284" -> Right $ ISO_15924_Number "284"
    "413" -> Right $ ISO_15924_Number "413"
    "412" -> Right $ ISO_15924_Number "412"
    "361" -> Right $ ISO_15924_Number "361"
    "510" -> Right $ ISO_15924_Number "510"
    "317" -> Right $ ISO_15924_Number "317"
    "345" -> Right $ ISO_15924_Number "345"
    "411" -> Right $ ISO_15924_Number "411"
    "368" -> Right $ ISO_15924_Number "368"
    "357" -> Right $ ISO_15924_Number "357"
    "305" -> Right $ ISO_15924_Number "305"
    "505" -> Right $ ISO_15924_Number "505"
    "288" -> Right $ ISO_15924_Number "288"
    "355" -> Right $ ISO_15924_Number "355"
    "322" -> Right $ ISO_15924_Number "322"
    "318" -> Right $ ISO_15924_Number "318"
    "241" -> Right $ ISO_15924_Number "241"
    "396" -> Right $ ISO_15924_Number "396"
    "293" -> Right $ ISO_15924_Number "293"
    "287" -> Right $ ISO_15924_Number "287"
    "436" -> Right $ ISO_15924_Number "436"
    "356" -> Right $ ISO_15924_Number "356"
    "215" -> Right $ ISO_15924_Number "215"
    "217" -> Right $ ISO_15924_Number "217"
    "216" -> Right $ ISO_15924_Number "216"
    "364" -> Right $ ISO_15924_Number "364"
    "335" -> Right $ ISO_15924_Number "335"
    "336" -> Right $ ISO_15924_Number "336"
    "400" -> Right $ ISO_15924_Number "400"
    "401" -> Right $ ISO_15924_Number "401"
    "399" -> Right $ ISO_15924_Number "399"
    "437" -> Right $ ISO_15924_Number "437"
    "202" -> Right $ ISO_15924_Number "202"
    "116" -> Right $ ISO_15924_Number "116"
    "314" -> Right $ ISO_15924_Number "314"
    "366" -> Right $ ISO_15924_Number "366"
    "347" -> Right $ ISO_15924_Number "347"
    "140" -> Right $ ISO_15924_Number "140"
    "139" -> Right $ ISO_15924_Number "139"
    "332" -> Right $ ISO_15924_Number "332"
    "313" -> Right $ ISO_15924_Number "313"
    "995" -> Right $ ISO_15924_Number "995"
    "090" -> Right $ ISO_15924_Number "090"
    "265" -> Right $ ISO_15924_Number "265"
    "337" -> Right $ ISO_15924_Number "337"
    "438" -> Right $ ISO_15924_Number "438"
    "101" -> Right $ ISO_15924_Number "101"
    "100" -> Right $ ISO_15924_Number "100"
    "282" -> Right $ ISO_15924_Number "282"
    "324" -> Right $ ISO_15924_Number "324"
    "145" -> Right $ ISO_15924_Number "145"
    "218" -> Right $ ISO_15924_Number "218"
    "264" -> Right $ ISO_15924_Number "264"
    "323" -> Right $ ISO_15924_Number "323"
    "350" -> Right $ ISO_15924_Number "350"
    "159" -> Right $ ISO_15924_Number "159"
    "295" -> Right $ ISO_15924_Number "295"
    "311" -> Right $ ISO_15924_Number "311"
    "085" -> Right $ ISO_15924_Number "085"
    "420" -> Right $ ISO_15924_Number "420"
    "354" -> Right $ ISO_15924_Number "354"
    "333" -> Right $ ISO_15924_Number "333"
    "165" -> Right $ ISO_15924_Number "165"
    "499" -> Right $ ISO_15924_Number "499"
    "451" -> Right $ ISO_15924_Number "451"
    "212" -> Right $ ISO_15924_Number "212"
    "261" -> Right $ ISO_15924_Number "261"
    "296" -> Right $ ISO_15924_Number "296"
    "176" -> Right $ ISO_15924_Number "176"
    "210" -> Right $ ISO_15924_Number "210"
    "106" -> Right $ ISO_15924_Number "106"
    "227" -> Right $ ISO_15924_Number "227"
    "030" -> Right $ ISO_15924_Number "030"
    "142" -> Right $ ISO_15924_Number "142"
    "105" -> Right $ ISO_15924_Number "105"
    "175" -> Right $ ISO_15924_Number "175"
    "143" -> Right $ ISO_15924_Number "143"
    "327" -> Right $ ISO_15924_Number "327"
    "219" -> Right $ ISO_15924_Number "219"
    "260" -> Right $ ISO_15924_Number "260"
    "450" -> Right $ ISO_15924_Number "450"
    "126" -> Right $ ISO_15924_Number "126"
    "263" -> Right $ ISO_15924_Number "263"
    "331" -> Right $ ISO_15924_Number "331"
    "115" -> Right $ ISO_15924_Number "115"
    "015" -> Right $ ISO_15924_Number "015"
    "016" -> Right $ ISO_15924_Number "016"
    "103" -> Right $ ISO_15924_Number "103"
    "132" -> Right $ ISO_15924_Number "132"
    "303" -> Right $ ISO_15924_Number "303"
    "363" -> Right $ ISO_15924_Number "363"
    "620" -> Right $ ISO_15924_Number "620"
    "211" -> Right $ ISO_15924_Number "211"
    "123" -> Right $ ISO_15924_Number "123"
    "292" -> Right $ ISO_15924_Number "292"
    "344" -> Right $ ISO_15924_Number "344"
    "319" -> Right $ ISO_15924_Number "319"
    "281" -> Right $ ISO_15924_Number "281"
    "530" -> Right $ ISO_15924_Number "530"
    "302" -> Right $ ISO_15924_Number "302"
    "180" -> Right $ ISO_15924_Number "180"
    "095" -> Right $ ISO_15924_Number "095"
    "348" -> Right $ ISO_15924_Number "348"
    "141" -> Right $ ISO_15924_Number "141"
    "398" -> Right $ ISO_15924_Number "398"
    "329" -> Right $ ISO_15924_Number "329"
    "362" -> Right $ ISO_15924_Number "362"
    "274" -> Right $ ISO_15924_Number "274"
    "316" -> Right $ ISO_15924_Number "316"
    "996" -> Right $ ISO_15924_Number "996"
    "993" -> Right $ ISO_15924_Number "993"
    "135" -> Right $ ISO_15924_Number "135"
    "136" -> Right $ ISO_15924_Number "136"
    "138" -> Right $ ISO_15924_Number "138"
    "137" -> Right $ ISO_15924_Number "137"
    "370" -> Right $ ISO_15924_Number "370"
    "373" -> Right $ ISO_15924_Number "373"
    "353" -> Right $ ISO_15924_Number "353"
    "351" -> Right $ ISO_15924_Number "351"
    "359" -> Right $ ISO_15924_Number "359"
    "380" -> Right $ ISO_15924_Number "380"
    "321" -> Right $ ISO_15924_Number "321"
    "346" -> Right $ ISO_15924_Number "346"
    "275" -> Right $ ISO_15924_Number "275"
    "520" -> Right $ ISO_15924_Number "520"
    "340" -> Right $ ISO_15924_Number "340"
    "290" -> Right $ ISO_15924_Number "290"
    "170" -> Right $ ISO_15924_Number "170"
    "352" -> Right $ ISO_15924_Number "352"
    "330" -> Right $ ISO_15924_Number "330"
    "120" -> Right $ ISO_15924_Number "120"
    "326" -> Right $ ISO_15924_Number "326"
    "229" -> Right $ ISO_15924_Number "229"
    "299" -> Right $ ISO_15924_Number "299"
    "294" -> Right $ ISO_15924_Number "294"
    "341" -> Right $ ISO_15924_Number "341"
    "040" -> Right $ ISO_15924_Number "040"
    "440" -> Right $ ISO_15924_Number "440"
    "470" -> Right $ ISO_15924_Number "470"
    "280" -> Right $ ISO_15924_Number "280"
    "228" -> Right $ ISO_15924_Number "228"
    "283" -> Right $ ISO_15924_Number "283"
    "262" -> Right $ ISO_15924_Number "262"
    "480" -> Right $ ISO_15924_Number "480"
    "192" -> Right $ ISO_15924_Number "192"
    "460" -> Right $ ISO_15924_Number "460"
    "339" -> Right $ ISO_15924_Number "339"
    _ -> Left $ "Unknown ISO_15924_Number: " <> T.unpack txt

iso15924NumberToBytes :: ISO_15924_Number -> LBS.ByteString
iso15924NumberToBytes = LBS8.pack . unISO_15924Number

iso15924NumberToText :: ISO_15924_Number -> T.Text
iso15924NumberToText = T.pack . unISO_15924Number

scriptISO15924Number :: Script -> ISO_15924_Number
scriptISO15924Number script =
  case script of
    AdlamScript -> ISO_15924_Number "166"
    AfakaScript -> ISO_15924_Number "439"
    AhomScript -> ISO_15924_Number "338"
    AnatolianHieroglyphs -> ISO_15924_Number "080"
    ArabicScript -> ISO_15924_Number "160"
    Arabic_Nastaliq -> ISO_15924_Number "161"
    ArmenianScript -> ISO_15924_Number "230"
    AvestanScript -> ISO_15924_Number "134"
    BalineseScript -> ISO_15924_Number "360"
    BamumScript -> ISO_15924_Number "435"
    BassaVahScript -> ISO_15924_Number "259"
    BatakScript -> ISO_15924_Number "365"
    BengaliScript -> ISO_15924_Number "325"
    BhaiksukiScript -> ISO_15924_Number "334"
    BlissymbolsScript -> ISO_15924_Number "550"
    BookPahlaviScript -> ISO_15924_Number "133"
    BopomofoScript -> ISO_15924_Number "285"
    BrahmiScript -> ISO_15924_Number "300"
    BrailleScript -> ISO_15924_Number "570"
    BugineseScript -> ISO_15924_Number "367"
    BuhidScript -> ISO_15924_Number "372"
    CarianScript -> ISO_15924_Number "201"
    CaucasianAlbanianScript -> ISO_15924_Number "239"
    ChakmaScript -> ISO_15924_Number "349"
    ChamScript -> ISO_15924_Number "358"
    CherokeeScript -> ISO_15924_Number "445"
    ChisoiScript -> ISO_15924_Number "298"
    ChorasmianScript -> ISO_15924_Number "109"
    CirthScript -> ISO_15924_Number "291"
    CodeForInheritedScript -> ISO_15924_Number "994"
    CodeForUncodedScript -> ISO_15924_Number "999"
    CodeForUndeterminedScript -> ISO_15924_Number "998"
    CodeForUnwrittenDocuments -> ISO_15924_Number "997"
    CopticScript -> ISO_15924_Number "204"
    CuneiformScript -> ISO_15924_Number "020"
    CypriotSyllabary -> ISO_15924_Number "403"
    CyproMinoanScript -> ISO_15924_Number "402"
    CyrillicScript -> ISO_15924_Number "220"
    Cyrillic_OldChurchSlavonic -> ISO_15924_Number "221"
    DeseretScript -> ISO_15924_Number "250"
    DevanagariScript -> ISO_15924_Number "315"
    DivesAkuruScript -> ISO_15924_Number "342"
    DograScript -> ISO_15924_Number "328"
    DuployanShorthand -> ISO_15924_Number "755"
    EgyptianDemoticScript -> ISO_15924_Number "070"
    EgyptianHieraticScript -> ISO_15924_Number "060"
    EgyptianHieroglyphs -> ISO_15924_Number "050"
    ElbasanScript -> ISO_15924_Number "226"
    ElymaicScript -> ISO_15924_Number "128"
    EthiopicScript -> ISO_15924_Number "430"
    GarayScript -> ISO_15924_Number "164"
    GeorgianScript -> ISO_15924_Number "240"
    GlagoliticScript -> ISO_15924_Number "225"
    GothicScript -> ISO_15924_Number "206"
    GranthaScript -> ISO_15924_Number "343"
    GreekScript -> ISO_15924_Number "200"
    GujaratiScript -> ISO_15924_Number "320"
    GunjalaGondiScript -> ISO_15924_Number "312"
    GurmukhiScript -> ISO_15924_Number "310"
    GurungKhemaScript -> ISO_15924_Number "397"
    HanScript -> ISO_15924_Number "500"
    HanWithBopomofoScript -> ISO_15924_Number "503"
    Han_Simplified -> ISO_15924_Number "501"
    Han_Traditional -> ISO_15924_Number "502"
    HangulScript -> ISO_15924_Number "286"
    HanifiRohingyaScript -> ISO_15924_Number "167"
    HanunooScript -> ISO_15924_Number "371"
    HatranScript -> ISO_15924_Number "127"
    HebrewScript -> ISO_15924_Number "125"
    HiraganaScript -> ISO_15924_Number "410"
    ImperialAramaicScript -> ISO_15924_Number "124"
    IndusScript -> ISO_15924_Number "610"
    InscriptionalPahlaviScript -> ISO_15924_Number "131"
    InscriptionalParthianScript -> ISO_15924_Number "130"
    JamoScript -> ISO_15924_Number "284"
    JapaneseScript -> ISO_15924_Number "413"
    JapaneseSyllabaries -> ISO_15924_Number "412"
    JavaneseScript -> ISO_15924_Number "361"
    JurchenScript -> ISO_15924_Number "510"
    KaithiScript -> ISO_15924_Number "317"
    KannadaScript -> ISO_15924_Number "345"
    KatakanaScript -> ISO_15924_Number "411"
    KawiScript -> ISO_15924_Number "368"
    KayahLiScript -> ISO_15924_Number "357"
    KharoshthiScript -> ISO_15924_Number "305"
    KhitanLargeScript -> ISO_15924_Number "505"
    KhitanSmallScript -> ISO_15924_Number "288"
    KhmerScript -> ISO_15924_Number "355"
    KhojkiScript -> ISO_15924_Number "322"
    KhudawadiScript -> ISO_15924_Number "318"
    KhutsuriScript -> ISO_15924_Number "241"
    KiratRaiScript -> ISO_15924_Number "396"
    KlingonScript -> ISO_15924_Number "293"
    KoreanScript -> ISO_15924_Number "287"
    KpelleScript -> ISO_15924_Number "436"
    LaoScript -> ISO_15924_Number "356"
    LatinScript -> ISO_15924_Number "215"
    Latin_Fraktur -> ISO_15924_Number "217"
    Latin_Gaelic -> ISO_15924_Number "216"
    LekeScript -> ISO_15924_Number "364"
    LepchaScript -> ISO_15924_Number "335"
    LimbuScript -> ISO_15924_Number "336"
    LinearAScript -> ISO_15924_Number "400"
    LinearBScript -> ISO_15924_Number "401"
    LisuScript -> ISO_15924_Number "399"
    LomaScript -> ISO_15924_Number "437"
    LycianScript -> ISO_15924_Number "202"
    LydianScript -> ISO_15924_Number "116"
    MahajaniScript -> ISO_15924_Number "314"
    MakasarScript -> ISO_15924_Number "366"
    MalayalamScript -> ISO_15924_Number "347"
    MandaicScript -> ISO_15924_Number "140"
    ManichaeanScript -> ISO_15924_Number "139"
    MarchenScript -> ISO_15924_Number "332"
    MasaramGondiScript -> ISO_15924_Number "313"
    MathematicalNotationScript -> ISO_15924_Number "995"
    MayanHieroglyphs -> ISO_15924_Number "090"
    MedefaidrinScript -> ISO_15924_Number "265"
    MeiteiMayekScript -> ISO_15924_Number "337"
    MendeKikakuiScript -> ISO_15924_Number "438"
    MeroiticCursive -> ISO_15924_Number "101"
    MeroiticHieroglyphs -> ISO_15924_Number "100"
    MiaoScript -> ISO_15924_Number "282"
    ModiScript -> ISO_15924_Number "324"
    MongolianScript -> ISO_15924_Number "145"
    MoonScript -> ISO_15924_Number "218"
    MroScript -> ISO_15924_Number "264"
    MultaniScript -> ISO_15924_Number "323"
    MyanmarScript -> ISO_15924_Number "350"
    NabataeanScript -> ISO_15924_Number "159"
    NagMundariScript -> ISO_15924_Number "295"
    NandinagariScript -> ISO_15924_Number "311"
    NaxiDongbaScript -> ISO_15924_Number "085"
    NaxiGebaScript -> ISO_15924_Number "420"
    NewTaiLueScript -> ISO_15924_Number "354"
    NewaScript -> ISO_15924_Number "333"
    NkoScript -> ISO_15924_Number "165"
    NushuScript -> ISO_15924_Number "499"
    NyiakengPuachueHmongScript -> ISO_15924_Number "451"
    OghamScript -> ISO_15924_Number "212"
    OlChikiScript -> ISO_15924_Number "261"
    OlOnalScript -> ISO_15924_Number "296"
    OldHungarianScript -> ISO_15924_Number "176"
    OldItalicScript -> ISO_15924_Number "210"
    OldNorthArabianScript -> ISO_15924_Number "106"
    OldPermicScript -> ISO_15924_Number "227"
    OldPersianScript -> ISO_15924_Number "030"
    OldSogdianScript -> ISO_15924_Number "142"
    OldSouthArabianScript -> ISO_15924_Number "105"
    OldTurkicScript -> ISO_15924_Number "175"
    OldUyghurScript -> ISO_15924_Number "143"
    OriyaScript -> ISO_15924_Number "327"
    OsageScript -> ISO_15924_Number "219"
    OsmanyaScript -> ISO_15924_Number "260"
    PahawhHmongScript -> ISO_15924_Number "450"
    PalmyreneScript -> ISO_15924_Number "126"
    PauCinHauScript -> ISO_15924_Number "263"
    PhagsPaScript -> ISO_15924_Number "331"
    PhoenicianScript -> ISO_15924_Number "115"
    ProtoCuneiformScript -> ISO_15924_Number "015"
    ProtoElamiteScript -> ISO_15924_Number "016"
    ProtoSinaiticScript -> ISO_15924_Number "103"
    PsalterPahlaviScript -> ISO_15924_Number "132"
    RanjanaScript -> ISO_15924_Number "303"
    RejangScript -> ISO_15924_Number "363"
    RongorongoScript -> ISO_15924_Number "620"
    RunicScript -> ISO_15924_Number "211"
    SamaritanScript -> ISO_15924_Number "123"
    SaratiScript -> ISO_15924_Number "292"
    SaurashtraScript -> ISO_15924_Number "344"
    SharadaScript -> ISO_15924_Number "319"
    ShavianScript -> ISO_15924_Number "281"
    ShuishuScript -> ISO_15924_Number "530"
    SiddhamScript -> ISO_15924_Number "302"
    SideticScript -> ISO_15924_Number "180"
    Signwriting -> ISO_15924_Number "095"
    SinhalaScript -> ISO_15924_Number "348"
    SogdianScript -> ISO_15924_Number "141"
    SoraSompengScript -> ISO_15924_Number "398"
    SoyomboScript -> ISO_15924_Number "329"
    SundaneseScript -> ISO_15924_Number "362"
    SunuwarScript -> ISO_15924_Number "274"
    SylotiNagriScript -> ISO_15924_Number "316"
    Symbols -> ISO_15924_Number "996"
    Symbols_Emoji -> ISO_15924_Number "993"
    SyriacScript -> ISO_15924_Number "135"
    Syriac_Eastern -> ISO_15924_Number "136"
    Syriac_Estrangelo -> ISO_15924_Number "138"
    Syriac_Western -> ISO_15924_Number "137"
    TagalogScript -> ISO_15924_Number "370"
    TagbanwaScript -> ISO_15924_Number "373"
    TaiLeScript -> ISO_15924_Number "353"
    TaiThamScript -> ISO_15924_Number "351"
    TaiVietScript -> ISO_15924_Number "359"
    TaiYoScript -> ISO_15924_Number "380"
    TakriScript -> ISO_15924_Number "321"
    TamilScript -> ISO_15924_Number "346"
    TangsaScript -> ISO_15924_Number "275"
    TangutScript -> ISO_15924_Number "520"
    TeluguScript -> ISO_15924_Number "340"
    TengwarScript -> ISO_15924_Number "290"
    ThaanaScript -> ISO_15924_Number "170"
    ThaiScript -> ISO_15924_Number "352"
    TibetanScript -> ISO_15924_Number "330"
    TifinaghScript -> ISO_15924_Number "120"
    TirhutaScript -> ISO_15924_Number "326"
    TodhriScript -> ISO_15924_Number "229"
    TolongSikiScript -> ISO_15924_Number "299"
    TotoScript -> ISO_15924_Number "294"
    TuluTigalariScript -> ISO_15924_Number "341"
    UgariticScript -> ISO_15924_Number "040"
    UnifiedCanadianAboriginalSyllabics -> ISO_15924_Number "440"
    VaiScript -> ISO_15924_Number "470"
    VisibleSpeech -> ISO_15924_Number "280"
    VithkuqiScript -> ISO_15924_Number "228"
    WanchoScript -> ISO_15924_Number "283"
    WarangCitiScript -> ISO_15924_Number "262"
    WoleaiScript -> ISO_15924_Number "480"
    YezidiScript -> ISO_15924_Number "192"
    YiScript -> ISO_15924_Number "460"
    ZanabazarSquareScript -> ISO_15924_Number "339"