-- | TODO: Write module documentation
--
-- A complete list of the scripts and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/ISO_15924
--
module Ogma.Internal.Script.Script
  ( Script
      ( AdlamScript
      , AfakaScript
      , AhomScript
      , AnatolianHieroglyphs
      , ArabicScript
      , Arabic_Nastaliq
      , ArmenianScript
      , AvestanScript
      , BalineseScript
      , BamumScript
      , BassaVahScript
      , BatakScript
      , BengaliScript
      , BhaiksukiScript
      , BlissymbolsScript
      , BookPahlaviScript
      , BopomofoScript
      , BrahmiScript
      , BrailleScript
      , BugineseScript
      , BuhidScript
      , CarianScript
      , CaucasianAlbanianScript
      , ChakmaScript
      , ChamScript
      , CherokeeScript
      , ChisoiScript
      , ChorasmianScript
      , CirthScript
      , CodeForInheritedScript
      , CodeForUncodedScript
      , CodeForUndeterminedScript
      , CodeForUnwrittenDocuments
      , CopticScript
      , CuneiformScript
      , CypriotSyllabary
      , CyproMinoanScript
      , CyrillicScript
      , Cyrillic_OldChurchSlavonic
      , DeseretScript
      , DevanagariScript
      , DivesAkuruScript
      , DograScript
      , DuployanShorthand
      , EgyptianDemoticScript
      , EgyptianHieraticScript
      , EgyptianHieroglyphs
      , ElbasanScript
      , ElymaicScript
      , EthiopicScript
      , GarayScript
      , GeorgianScript
      , GlagoliticScript
      , GothicScript
      , GranthaScript
      , GreekScript
      , GujaratiScript
      , GunjalaGondiScript
      , GurmukhiScript
      , GurungKhemaScript
      , HanScript
      , HanWithBopomofoScript
      , Han_Simplified
      , Han_Traditional
      , HangulScript
      , HanifiRohingyaScript
      , HanunooScript
      , HatranScript
      , HebrewScript
      , HiraganaScript
      , ImperialAramaicScript
      , IndusScript
      , InscriptionalPahlaviScript
      , InscriptionalParthianScript
      , JamoScript
      , JapaneseScript
      , JapaneseSyllabaries
      , JavaneseScript
      , JurchenScript
      , KaithiScript
      , KannadaScript
      , KatakanaScript
      , KawiScript
      , KayahLiScript
      , KharoshthiScript
      , KhitanLargeScript
      , KhitanSmallScript
      , KhmerScript
      , KhojkiScript
      , KhudawadiScript
      , KhutsuriScript
      , KiratRaiScript
      , KlingonScript
      , KoreanScript
      , KpelleScript
      , LaoScript
      , LatinScript
      , Latin_Fraktur
      , Latin_Gaelic
      , LekeScript
      , LepchaScript
      , LimbuScript
      , LinearAScript
      , LinearBScript
      , LisuScript
      , LomaScript
      , LycianScript
      , LydianScript
      , MahajaniScript
      , MakasarScript
      , MalayalamScript
      , MandaicScript
      , ManichaeanScript
      , MarchenScript
      , MasaramGondiScript
      , MathematicalNotationScript
      , MayanHieroglyphs
      , MedefaidrinScript
      , MeiteiMayekScript
      , MendeKikakuiScript
      , MeroiticCursive
      , MeroiticHieroglyphs
      , MiaoScript
      , ModiScript
      , MongolianScript
      , MoonScript
      , MroScript
      , MultaniScript
      , MyanmarScript
      , NabataeanScript
      , NagMundariScript
      , NandinagariScript
      , NaxiDongbaScript
      , NaxiGebaScript
      , NewTaiLueScript
      , NewaScript
      , NkoScript
      , NushuScript
      , NyiakengPuachueHmongScript
      , OghamScript
      , OlChikiScript
      , OlOnalScript
      , OldHungarianScript
      , OldItalicScript
      , OldNorthArabianScript
      , OldPermicScript
      , OldPersianScript
      , OldSogdianScript
      , OldSouthArabianScript
      , OldTurkicScript
      , OldUyghurScript
      , OriyaScript
      , OsageScript
      , OsmanyaScript
      , PahawhHmongScript
      , PalmyreneScript
      , PauCinHauScript
      , PhagsPaScript
      , PhoenicianScript
      , ProtoCuneiformScript
      , ProtoElamiteScript
      , ProtoSinaiticScript
      , PsalterPahlaviScript
      , RanjanaScript
      , RejangScript
      , RongorongoScript
      , RunicScript
      , SamaritanScript
      , SaratiScript
      , SaurashtraScript
      , SharadaScript
      , ShavianScript
      , ShuishuScript
      , SiddhamScript
      , SideticScript
      , Signwriting
      , SinhalaScript
      , SogdianScript
      , SoraSompengScript
      , SoyomboScript
      , SundaneseScript
      , SunuwarScript
      , SylotiNagriScript
      , Symbols
      , Symbols_Emoji
      , SyriacScript
      , Syriac_Eastern
      , Syriac_Estrangelo
      , Syriac_Western
      , TagalogScript
      , TagbanwaScript
      , TaiLeScript
      , TaiThamScript
      , TaiVietScript
      , TaiYoScript
      , TakriScript
      , TamilScript
      , TangsaScript
      , TangutScript
      , TeluguScript
      , TengwarScript
      , ThaanaScript
      , ThaiScript
      , TibetanScript
      , TifinaghScript
      , TirhutaScript
      , TodhriScript
      , TolongSikiScript
      , TotoScript
      , TuluTigalariScript
      , UgariticScript
      , UnifiedCanadianAboriginalSyllabics
      , VaiScript
      , VisibleSpeech
      , VithkuqiScript
      , WanchoScript
      , WarangCitiScript
      , WoleaiScript
      , YezidiScript
      , YiScript
      , ZanabazarSquareScript
      )
  ) where

data Script
  = AdlamScript
  | AfakaScript
  | AhomScript
  | AnatolianHieroglyphs
  | ArabicScript
  | Arabic_Nastaliq
  | ArmenianScript
  | AvestanScript
  | BalineseScript
  | BamumScript
  | BassaVahScript
  | BatakScript
  | BengaliScript
  | BhaiksukiScript
  | BlissymbolsScript
  | BookPahlaviScript
  | BopomofoScript
  | BrahmiScript
  | BrailleScript
  | BugineseScript
  | BuhidScript
  | CarianScript
  | CaucasianAlbanianScript
  | ChakmaScript
  | ChamScript
  | CherokeeScript
  | ChisoiScript
  | ChorasmianScript
  | CirthScript
  | CodeForInheritedScript
  | CodeForUncodedScript
  | CodeForUndeterminedScript
  | CodeForUnwrittenDocuments
  | CopticScript
  | CuneiformScript
  | CypriotSyllabary
  | CyproMinoanScript
  | CyrillicScript
  | Cyrillic_OldChurchSlavonic
  | DeseretScript
  | DevanagariScript
  | DivesAkuruScript
  | DograScript
  | DuployanShorthand
  | EgyptianDemoticScript
  | EgyptianHieraticScript
  | EgyptianHieroglyphs
  | ElbasanScript
  | ElymaicScript
  | EthiopicScript
  | GarayScript
  | GeorgianScript
  | GlagoliticScript
  | GothicScript
  | GranthaScript
  | GreekScript
  | GujaratiScript
  | GunjalaGondiScript
  | GurmukhiScript
  | GurungKhemaScript
  | HanScript
  | HanWithBopomofoScript
  | Han_Simplified
  | Han_Traditional
  | HangulScript
  | HanifiRohingyaScript
  | HanunooScript
  | HatranScript
  | HebrewScript
  | HiraganaScript
  | ImperialAramaicScript
  | IndusScript
  | InscriptionalPahlaviScript
  | InscriptionalParthianScript
  | JamoScript
  | JapaneseScript
  | JapaneseSyllabaries
  | JavaneseScript
  | JurchenScript
  | KaithiScript
  | KannadaScript
  | KatakanaScript
  | KawiScript
  | KayahLiScript
  | KharoshthiScript
  | KhitanLargeScript
  | KhitanSmallScript
  | KhmerScript
  | KhojkiScript
  | KhudawadiScript
  | KhutsuriScript
  | KiratRaiScript
  | KlingonScript
  | KoreanScript
  | KpelleScript
  | LaoScript
  | LatinScript
  | Latin_Fraktur
  | Latin_Gaelic
  | LekeScript
  | LepchaScript
  | LimbuScript
  | LinearAScript
  | LinearBScript
  | LisuScript
  | LomaScript
  | LycianScript
  | LydianScript
  | MahajaniScript
  | MakasarScript
  | MalayalamScript
  | MandaicScript
  | ManichaeanScript
  | MarchenScript
  | MasaramGondiScript
  | MathematicalNotationScript
  | MayanHieroglyphs
  | MedefaidrinScript
  | MeiteiMayekScript
  | MendeKikakuiScript
  | MeroiticCursive
  | MeroiticHieroglyphs
  | MiaoScript
  | ModiScript
  | MongolianScript
  | MoonScript
  | MroScript
  | MultaniScript
  | MyanmarScript
  | NabataeanScript
  | NagMundariScript
  | NandinagariScript
  | NaxiDongbaScript
  | NaxiGebaScript
  | NewTaiLueScript
  | NewaScript
  | NkoScript
  | NushuScript
  | NyiakengPuachueHmongScript
  | OghamScript
  | OlChikiScript
  | OlOnalScript
  | OldHungarianScript
  | OldItalicScript
  | OldNorthArabianScript
  | OldPermicScript
  | OldPersianScript
  | OldSogdianScript
  | OldSouthArabianScript
  | OldTurkicScript
  | OldUyghurScript
  | OriyaScript
  | OsageScript
  | OsmanyaScript
  | PahawhHmongScript
  | PalmyreneScript
  | PauCinHauScript
  | PhagsPaScript
  | PhoenicianScript
  | ProtoCuneiformScript
  | ProtoElamiteScript
  | ProtoSinaiticScript
  | PsalterPahlaviScript
  | RanjanaScript
  | RejangScript
  | RongorongoScript
  | RunicScript
  | SamaritanScript
  | SaratiScript
  | SaurashtraScript
  | SharadaScript
  | ShavianScript
  | ShuishuScript
  | SiddhamScript
  | SideticScript
  | Signwriting
  | SinhalaScript
  | SogdianScript
  | SoraSompengScript
  | SoyomboScript
  | SundaneseScript
  | SunuwarScript
  | SylotiNagriScript
  | Symbols
  | Symbols_Emoji
  | SyriacScript
  | Syriac_Eastern
  | Syriac_Estrangelo
  | Syriac_Western
  | TagalogScript
  | TagbanwaScript
  | TaiLeScript
  | TaiThamScript
  | TaiVietScript
  | TaiYoScript
  | TakriScript
  | TamilScript
  | TangsaScript
  | TangutScript
  | TeluguScript
  | TengwarScript
  | ThaanaScript
  | ThaiScript
  | TibetanScript
  | TifinaghScript
  | TirhutaScript
  | TodhriScript
  | TolongSikiScript
  | TotoScript
  | TuluTigalariScript
  | UgariticScript
  | UnifiedCanadianAboriginalSyllabics
  | VaiScript
  | VisibleSpeech
  | VithkuqiScript
  | WanchoScript
  | WarangCitiScript
  | WoleaiScript
  | YezidiScript
  | YiScript
  | ZanabazarSquareScript
  deriving stock (Eq, Show)
