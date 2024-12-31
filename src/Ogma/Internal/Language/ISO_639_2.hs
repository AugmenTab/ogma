module Ogma.Internal.Language.ISO_639_2
  ( ISO_639_2
  , iso_639_2FromText
  , iso_639_2ToBytes
  , iso_639_2ToText
  , languageISO_639_2
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

import Ogma.Internal.Language.Language (Language (..))

newtype ISO_639_2 =
  ISO_639_2
    { unISO_639_2 :: String
    } deriving newtype (Eq, Show)

iso_639_2FromText :: T.Text -> Either String ISO_639_2
iso_639_2FromText txt =
  case T.toLower txt of
    "aar" -> Right $ ISO_639_2 "aar"
    "abk" -> Right $ ISO_639_2 "abk"
    "ace" -> Right $ ISO_639_2 "ace"
    "ach" -> Right $ ISO_639_2 "ach"
    "ada" -> Right $ ISO_639_2 "ada"
    "ady" -> Right $ ISO_639_2 "ady"
    "afh" -> Right $ ISO_639_2 "afh"
    "afr" -> Right $ ISO_639_2 "afr"
    "ain" -> Right $ ISO_639_2 "ain"
    "aka" -> Right $ ISO_639_2 "aka"
    "akk" -> Right $ ISO_639_2 "akk"
    "alb" -> Right $ ISO_639_2 "alb"
    "ale" -> Right $ ISO_639_2 "ale"
    "alt" -> Right $ ISO_639_2 "alt"
    "amh" -> Right $ ISO_639_2 "amh"
    "ang" -> Right $ ISO_639_2 "ang"
    "anp" -> Right $ ISO_639_2 "anp"
    "ara" -> Right $ ISO_639_2 "ara"
    "arc" -> Right $ ISO_639_2 "arc"
    "arg" -> Right $ ISO_639_2 "arg"
    "arm" -> Right $ ISO_639_2 "arm"
    "arn" -> Right $ ISO_639_2 "arn"
    "arp" -> Right $ ISO_639_2 "arp"
    "arw" -> Right $ ISO_639_2 "arw"
    "asm" -> Right $ ISO_639_2 "asm"
    "ast" -> Right $ ISO_639_2 "ast"
    "ava" -> Right $ ISO_639_2 "ava"
    "ave" -> Right $ ISO_639_2 "ave"
    "awa" -> Right $ ISO_639_2 "awa"
    "aym" -> Right $ ISO_639_2 "aym"
    "aze" -> Right $ ISO_639_2 "aze"
    "bak" -> Right $ ISO_639_2 "bak"
    "bal" -> Right $ ISO_639_2 "bal"
    "bam" -> Right $ ISO_639_2 "bam"
    "ban" -> Right $ ISO_639_2 "ban"
    "baq" -> Right $ ISO_639_2 "baq"
    "bas" -> Right $ ISO_639_2 "bas"
    "bej" -> Right $ ISO_639_2 "bej"
    "bel" -> Right $ ISO_639_2 "bel"
    "bem" -> Right $ ISO_639_2 "bem"
    "ben" -> Right $ ISO_639_2 "ben"
    "bho" -> Right $ ISO_639_2 "bho"
    "bin" -> Right $ ISO_639_2 "bin"
    "bis" -> Right $ ISO_639_2 "bis"
    "bla" -> Right $ ISO_639_2 "bla"
    "bos" -> Right $ ISO_639_2 "bos"
    "bra" -> Right $ ISO_639_2 "bra"
    "bre" -> Right $ ISO_639_2 "bre"
    "bua" -> Right $ ISO_639_2 "bua"
    "bug" -> Right $ ISO_639_2 "bug"
    "bul" -> Right $ ISO_639_2 "bul"
    "bur" -> Right $ ISO_639_2 "bur"
    "byn" -> Right $ ISO_639_2 "byn"
    "cad" -> Right $ ISO_639_2 "cad"
    "car" -> Right $ ISO_639_2 "car"
    "cat" -> Right $ ISO_639_2 "cat"
    "ceb" -> Right $ ISO_639_2 "ceb"
    "cha" -> Right $ ISO_639_2 "cha"
    "chb" -> Right $ ISO_639_2 "chb"
    "che" -> Right $ ISO_639_2 "che"
    "chg" -> Right $ ISO_639_2 "chg"
    "chi" -> Right $ ISO_639_2 "chi"
    "chk" -> Right $ ISO_639_2 "chk"
    "chm" -> Right $ ISO_639_2 "chm"
    "chn" -> Right $ ISO_639_2 "chn"
    "cho" -> Right $ ISO_639_2 "cho"
    "chp" -> Right $ ISO_639_2 "chp"
    "chr" -> Right $ ISO_639_2 "chr"
    "chu" -> Right $ ISO_639_2 "chu"
    "chv" -> Right $ ISO_639_2 "chv"
    "chy" -> Right $ ISO_639_2 "chy"
    "cnr" -> Right $ ISO_639_2 "cnr"
    "cop" -> Right $ ISO_639_2 "cop"
    "cor" -> Right $ ISO_639_2 "cor"
    "cos" -> Right $ ISO_639_2 "cos"
    "cre" -> Right $ ISO_639_2 "cre"
    "crh" -> Right $ ISO_639_2 "crh"
    "crp" -> Right $ ISO_639_2 "crp"
    "csb" -> Right $ ISO_639_2 "csb"
    "cze" -> Right $ ISO_639_2 "cze"
    "dak" -> Right $ ISO_639_2 "dak"
    "dan" -> Right $ ISO_639_2 "dan"
    "dar" -> Right $ ISO_639_2 "dar"
    "den" -> Right $ ISO_639_2 "den"
    "dgr" -> Right $ ISO_639_2 "dgr"
    "din" -> Right $ ISO_639_2 "din"
    "div" -> Right $ ISO_639_2 "div"
    "doi" -> Right $ ISO_639_2 "doi"
    "dsb" -> Right $ ISO_639_2 "dsb"
    "dua" -> Right $ ISO_639_2 "dua"
    "dum" -> Right $ ISO_639_2 "dum"
    "dut" -> Right $ ISO_639_2 "dut"
    "dyu" -> Right $ ISO_639_2 "dyu"
    "dzo" -> Right $ ISO_639_2 "dzo"
    "efi" -> Right $ ISO_639_2 "efi"
    "egy" -> Right $ ISO_639_2 "egy"
    "eka" -> Right $ ISO_639_2 "eka"
    "elx" -> Right $ ISO_639_2 "elx"
    "eng" -> Right $ ISO_639_2 "eng"
    "enm" -> Right $ ISO_639_2 "enm"
    "epo" -> Right $ ISO_639_2 "epo"
    "est" -> Right $ ISO_639_2 "est"
    "ewe" -> Right $ ISO_639_2 "ewe"
    "ewo" -> Right $ ISO_639_2 "ewo"
    "fan" -> Right $ ISO_639_2 "fan"
    "fao" -> Right $ ISO_639_2 "fao"
    "fat" -> Right $ ISO_639_2 "fat"
    "fij" -> Right $ ISO_639_2 "fij"
    "fil" -> Right $ ISO_639_2 "fil"
    "fin" -> Right $ ISO_639_2 "fin"
    "fon" -> Right $ ISO_639_2 "fon"
    "fre" -> Right $ ISO_639_2 "fre"
    "frm" -> Right $ ISO_639_2 "frm"
    "fro" -> Right $ ISO_639_2 "fro"
    "frr" -> Right $ ISO_639_2 "frr"
    "frs" -> Right $ ISO_639_2 "frs"
    "fry" -> Right $ ISO_639_2 "fry"
    "ful" -> Right $ ISO_639_2 "ful"
    "fur" -> Right $ ISO_639_2 "fur"
    "gaa" -> Right $ ISO_639_2 "gaa"
    "gay" -> Right $ ISO_639_2 "gay"
    "geo" -> Right $ ISO_639_2 "geo"
    "ger" -> Right $ ISO_639_2 "ger"
    "gez" -> Right $ ISO_639_2 "gez"
    "gil" -> Right $ ISO_639_2 "gil"
    "gla" -> Right $ ISO_639_2 "gla"
    "gle" -> Right $ ISO_639_2 "gle"
    "glg" -> Right $ ISO_639_2 "glg"
    "glv" -> Right $ ISO_639_2 "glv"
    "gmh" -> Right $ ISO_639_2 "gmh"
    "goh" -> Right $ ISO_639_2 "goh"
    "gon" -> Right $ ISO_639_2 "gon"
    "gor" -> Right $ ISO_639_2 "gor"
    "got" -> Right $ ISO_639_2 "got"
    "grb" -> Right $ ISO_639_2 "grb"
    "grc" -> Right $ ISO_639_2 "grc"
    "gre" -> Right $ ISO_639_2 "gre"
    "grn" -> Right $ ISO_639_2 "grn"
    "guj" -> Right $ ISO_639_2 "guj"
    "gwi" -> Right $ ISO_639_2 "gwi"
    "hai" -> Right $ ISO_639_2 "hai"
    "hat" -> Right $ ISO_639_2 "hat"
    "hau" -> Right $ ISO_639_2 "hau"
    "haw" -> Right $ ISO_639_2 "haw"
    "heb" -> Right $ ISO_639_2 "heb"
    "her" -> Right $ ISO_639_2 "her"
    "hil" -> Right $ ISO_639_2 "hil"
    "hin" -> Right $ ISO_639_2 "hin"
    "hit" -> Right $ ISO_639_2 "hit"
    "hmn" -> Right $ ISO_639_2 "hmn"
    "hmo" -> Right $ ISO_639_2 "hmo"
    "hrv" -> Right $ ISO_639_2 "hrv"
    "hsb" -> Right $ ISO_639_2 "hsb"
    "hun" -> Right $ ISO_639_2 "hun"
    "hup" -> Right $ ISO_639_2 "hup"
    "iba" -> Right $ ISO_639_2 "iba"
    "ibo" -> Right $ ISO_639_2 "ibo"
    "ice" -> Right $ ISO_639_2 "ice"
    "ido" -> Right $ ISO_639_2 "ido"
    "iii" -> Right $ ISO_639_2 "iii"
    "iku" -> Right $ ISO_639_2 "iku"
    "ile" -> Right $ ISO_639_2 "ile"
    "ilo" -> Right $ ISO_639_2 "ilo"
    "ina" -> Right $ ISO_639_2 "ina"
    "ind" -> Right $ ISO_639_2 "ind"
    "inh" -> Right $ ISO_639_2 "inh"
    "ipk" -> Right $ ISO_639_2 "ipk"
    "ita" -> Right $ ISO_639_2 "ita"
    "jav" -> Right $ ISO_639_2 "jav"
    "jbo" -> Right $ ISO_639_2 "jbo"
    "jpn" -> Right $ ISO_639_2 "jpn"
    "jpr" -> Right $ ISO_639_2 "jpr"
    "jrb" -> Right $ ISO_639_2 "jrb"
    "kaa" -> Right $ ISO_639_2 "kaa"
    "kab" -> Right $ ISO_639_2 "kab"
    "kac" -> Right $ ISO_639_2 "kac"
    "kal" -> Right $ ISO_639_2 "kal"
    "kam" -> Right $ ISO_639_2 "kam"
    "kan" -> Right $ ISO_639_2 "kan"
    "kar" -> Right $ ISO_639_2 "kar"
    "kas" -> Right $ ISO_639_2 "kas"
    "kau" -> Right $ ISO_639_2 "kau"
    "kaw" -> Right $ ISO_639_2 "kaw"
    "kaz" -> Right $ ISO_639_2 "kaz"
    "kbd" -> Right $ ISO_639_2 "kbd"
    "kbl" -> Right $ ISO_639_2 "kbl"
    "kha" -> Right $ ISO_639_2 "kha"
    "khm" -> Right $ ISO_639_2 "khm"
    "kho" -> Right $ ISO_639_2 "kho"
    "kik" -> Right $ ISO_639_2 "kik"
    "kin" -> Right $ ISO_639_2 "kin"
    "kir" -> Right $ ISO_639_2 "kir"
    "kmb" -> Right $ ISO_639_2 "kmb"
    "kok" -> Right $ ISO_639_2 "kok"
    "kon" -> Right $ ISO_639_2 "kon"
    "kor" -> Right $ ISO_639_2 "kor"
    "kos" -> Right $ ISO_639_2 "kos"
    "kpe" -> Right $ ISO_639_2 "kpe"
    "krc" -> Right $ ISO_639_2 "krc"
    "krl" -> Right $ ISO_639_2 "krl"
    "kua" -> Right $ ISO_639_2 "kua"
    "kum" -> Right $ ISO_639_2 "kum"
    "kur" -> Right $ ISO_639_2 "kur"
    "kut" -> Right $ ISO_639_2 "kut"
    "lad" -> Right $ ISO_639_2 "lad"
    "lah" -> Right $ ISO_639_2 "lah"
    "lam" -> Right $ ISO_639_2 "lam"
    "lao" -> Right $ ISO_639_2 "lao"
    "lat" -> Right $ ISO_639_2 "lat"
    "lav" -> Right $ ISO_639_2 "lav"
    "lez" -> Right $ ISO_639_2 "lez"
    "lim" -> Right $ ISO_639_2 "lim"
    "lin" -> Right $ ISO_639_2 "lin"
    "lit" -> Right $ ISO_639_2 "lit"
    "lol" -> Right $ ISO_639_2 "lol"
    "loz" -> Right $ ISO_639_2 "loz"
    "ltz" -> Right $ ISO_639_2 "ltz"
    "lua" -> Right $ ISO_639_2 "lua"
    "lub" -> Right $ ISO_639_2 "lub"
    "lug" -> Right $ ISO_639_2 "lug"
    "lui" -> Right $ ISO_639_2 "lui"
    "lun" -> Right $ ISO_639_2 "lun"
    "luo" -> Right $ ISO_639_2 "luo"
    "lus" -> Right $ ISO_639_2 "lus"
    "mac" -> Right $ ISO_639_2 "mac"
    "mad" -> Right $ ISO_639_2 "mad"
    "mag" -> Right $ ISO_639_2 "mag"
    "mah" -> Right $ ISO_639_2 "mah"
    "mai" -> Right $ ISO_639_2 "mai"
    "mak" -> Right $ ISO_639_2 "mak"
    "mal" -> Right $ ISO_639_2 "mal"
    "mao" -> Right $ ISO_639_2 "mao"
    "mar" -> Right $ ISO_639_2 "mar"
    "mas" -> Right $ ISO_639_2 "mas"
    "may" -> Right $ ISO_639_2 "may"
    "mdf" -> Right $ ISO_639_2 "mdf"
    "mdr" -> Right $ ISO_639_2 "mdr"
    "men" -> Right $ ISO_639_2 "men"
    "mga" -> Right $ ISO_639_2 "mga"
    "mic" -> Right $ ISO_639_2 "mic"
    "min" -> Right $ ISO_639_2 "min"
    "mlg" -> Right $ ISO_639_2 "mlg"
    "mlt" -> Right $ ISO_639_2 "mlt"
    "mnc" -> Right $ ISO_639_2 "mnc"
    "mni" -> Right $ ISO_639_2 "mni"
    "moh" -> Right $ ISO_639_2 "moh"
    "mol" -> Right $ ISO_639_2 "mol"
    "mon" -> Right $ ISO_639_2 "mon"
    "mos" -> Right $ ISO_639_2 "mos"
    "mus" -> Right $ ISO_639_2 "mus"
    "mwl" -> Right $ ISO_639_2 "mwl"
    "mwr" -> Right $ ISO_639_2 "mwr"
    "myv" -> Right $ ISO_639_2 "myv"
    "nap" -> Right $ ISO_639_2 "nap"
    "nau" -> Right $ ISO_639_2 "nau"
    "nav" -> Right $ ISO_639_2 "nav"
    "nbl" -> Right $ ISO_639_2 "nbl"
    "nde" -> Right $ ISO_639_2 "nde"
    "ndo" -> Right $ ISO_639_2 "ndo"
    "nds" -> Right $ ISO_639_2 "nds"
    "nep" -> Right $ ISO_639_2 "nep"
    "new" -> Right $ ISO_639_2 "new"
    "nia" -> Right $ ISO_639_2 "nia"
    "niu" -> Right $ ISO_639_2 "niu"
    "nno" -> Right $ ISO_639_2 "nno"
    "nob" -> Right $ ISO_639_2 "nob"
    "nog" -> Right $ ISO_639_2 "nog"
    "non" -> Right $ ISO_639_2 "non"
    "nor" -> Right $ ISO_639_2 "nor"
    "nqo" -> Right $ ISO_639_2 "nqo"
    "nso" -> Right $ ISO_639_2 "nso"
    "nwc" -> Right $ ISO_639_2 "nwc"
    "nya" -> Right $ ISO_639_2 "nya"
    "nym" -> Right $ ISO_639_2 "nym"
    "nyn" -> Right $ ISO_639_2 "nyn"
    "nyo" -> Right $ ISO_639_2 "nyo"
    "nzi" -> Right $ ISO_639_2 "nzi"
    "oci" -> Right $ ISO_639_2 "oci"
    "oji" -> Right $ ISO_639_2 "oji"
    "ori" -> Right $ ISO_639_2 "ori"
    "orm" -> Right $ ISO_639_2 "orm"
    "osa" -> Right $ ISO_639_2 "osa"
    "oss" -> Right $ ISO_639_2 "oss"
    "ota" -> Right $ ISO_639_2 "ota"
    "pag" -> Right $ ISO_639_2 "pag"
    "pal" -> Right $ ISO_639_2 "pal"
    "pam" -> Right $ ISO_639_2 "pam"
    "pan" -> Right $ ISO_639_2 "pan"
    "pap" -> Right $ ISO_639_2 "pap"
    "pau" -> Right $ ISO_639_2 "pau"
    "peo" -> Right $ ISO_639_2 "peo"
    "per" -> Right $ ISO_639_2 "per"
    "phn" -> Right $ ISO_639_2 "phn"
    "pli" -> Right $ ISO_639_2 "pli"
    "pol" -> Right $ ISO_639_2 "pol"
    "pon" -> Right $ ISO_639_2 "pon"
    "por" -> Right $ ISO_639_2 "por"
    "pro" -> Right $ ISO_639_2 "pro"
    "pus" -> Right $ ISO_639_2 "pus"
    "rap" -> Right $ ISO_639_2 "rap"
    "rar" -> Right $ ISO_639_2 "rar"
    "roh" -> Right $ ISO_639_2 "roh"
    "rom" -> Right $ ISO_639_2 "rom"
    "rum" -> Right $ ISO_639_2 "rum"
    "run" -> Right $ ISO_639_2 "run"
    "rup" -> Right $ ISO_639_2 "rup"
    "rus" -> Right $ ISO_639_2 "rus"
    "sad" -> Right $ ISO_639_2 "sad"
    "sag" -> Right $ ISO_639_2 "sag"
    "sah" -> Right $ ISO_639_2 "sah"
    "sam" -> Right $ ISO_639_2 "sam"
    "san" -> Right $ ISO_639_2 "san"
    "sas" -> Right $ ISO_639_2 "sas"
    "sat" -> Right $ ISO_639_2 "sat"
    "scn" -> Right $ ISO_639_2 "scn"
    "sco" -> Right $ ISO_639_2 "sco"
    "sel" -> Right $ ISO_639_2 "sel"
    "sga" -> Right $ ISO_639_2 "sga"
    "sgn" -> Right $ ISO_639_2 "sgn"
    "shn" -> Right $ ISO_639_2 "shn"
    "sid" -> Right $ ISO_639_2 "sid"
    "sin" -> Right $ ISO_639_2 "sin"
    "slo" -> Right $ ISO_639_2 "slo"
    "slv" -> Right $ ISO_639_2 "slv"
    "sma" -> Right $ ISO_639_2 "sma"
    "sme" -> Right $ ISO_639_2 "sme"
    "smj" -> Right $ ISO_639_2 "smj"
    "smn" -> Right $ ISO_639_2 "smn"
    "smo" -> Right $ ISO_639_2 "smo"
    "sms" -> Right $ ISO_639_2 "sms"
    "sna" -> Right $ ISO_639_2 "sna"
    "snd" -> Right $ ISO_639_2 "snd"
    "snk" -> Right $ ISO_639_2 "snk"
    "sog" -> Right $ ISO_639_2 "sog"
    "som" -> Right $ ISO_639_2 "som"
    "sot" -> Right $ ISO_639_2 "sot"
    "spa" -> Right $ ISO_639_2 "spa"
    "srd" -> Right $ ISO_639_2 "srd"
    "srn" -> Right $ ISO_639_2 "srn"
    "srp" -> Right $ ISO_639_2 "srp"
    "srr" -> Right $ ISO_639_2 "srr"
    "ssw" -> Right $ ISO_639_2 "ssw"
    "suk" -> Right $ ISO_639_2 "suk"
    "sun" -> Right $ ISO_639_2 "sun"
    "sus" -> Right $ ISO_639_2 "sus"
    "sux" -> Right $ ISO_639_2 "sux"
    "swa" -> Right $ ISO_639_2 "swa"
    "swe" -> Right $ ISO_639_2 "swe"
    "syc" -> Right $ ISO_639_2 "syc"
    "tah" -> Right $ ISO_639_2 "tah"
    "tam" -> Right $ ISO_639_2 "tam"
    "tat" -> Right $ ISO_639_2 "tat"
    "tel" -> Right $ ISO_639_2 "tel"
    "tem" -> Right $ ISO_639_2 "tem"
    "ter" -> Right $ ISO_639_2 "ter"
    "tet" -> Right $ ISO_639_2 "tet"
    "tgk" -> Right $ ISO_639_2 "tgk"
    "tgl" -> Right $ ISO_639_2 "tgl"
    "tha" -> Right $ ISO_639_2 "tha"
    "tib" -> Right $ ISO_639_2 "tib"
    "tig" -> Right $ ISO_639_2 "tig"
    "tir" -> Right $ ISO_639_2 "tir"
    "tiv" -> Right $ ISO_639_2 "tiv"
    "tkl" -> Right $ ISO_639_2 "tkl"
    "tlh" -> Right $ ISO_639_2 "tlh"
    "tli" -> Right $ ISO_639_2 "tli"
    "tog" -> Right $ ISO_639_2 "tog"
    "ton" -> Right $ ISO_639_2 "ton"
    "tpi" -> Right $ ISO_639_2 "tpi"
    "tsi" -> Right $ ISO_639_2 "tsi"
    "tsn" -> Right $ ISO_639_2 "tsn"
    "tso" -> Right $ ISO_639_2 "tso"
    "tuk" -> Right $ ISO_639_2 "tuk"
    "tum" -> Right $ ISO_639_2 "tum"
    "tur" -> Right $ ISO_639_2 "tur"
    "tvl" -> Right $ ISO_639_2 "tvl"
    "twi" -> Right $ ISO_639_2 "twi"
    "tyv" -> Right $ ISO_639_2 "tyv"
    "udm" -> Right $ ISO_639_2 "udm"
    "uga" -> Right $ ISO_639_2 "uga"
    "uig" -> Right $ ISO_639_2 "uig"
    "ukr" -> Right $ ISO_639_2 "ukr"
    "umb" -> Right $ ISO_639_2 "umb"
    "urd" -> Right $ ISO_639_2 "urd"
    "uzb" -> Right $ ISO_639_2 "uzb"
    "vai" -> Right $ ISO_639_2 "vai"
    "ven" -> Right $ ISO_639_2 "ven"
    "vie" -> Right $ ISO_639_2 "vie"
    "vol" -> Right $ ISO_639_2 "vol"
    "vot" -> Right $ ISO_639_2 "vot"
    "wal" -> Right $ ISO_639_2 "wal"
    "war" -> Right $ ISO_639_2 "war"
    "was" -> Right $ ISO_639_2 "was"
    "wel" -> Right $ ISO_639_2 "wel"
    "wln" -> Right $ ISO_639_2 "wln"
    "wol" -> Right $ ISO_639_2 "wol"
    "xal" -> Right $ ISO_639_2 "xal"
    "xho" -> Right $ ISO_639_2 "xho"
    "yao" -> Right $ ISO_639_2 "yao"
    "yap" -> Right $ ISO_639_2 "yap"
    "yid" -> Right $ ISO_639_2 "yid"
    "yor" -> Right $ ISO_639_2 "yor"
    "zen" -> Right $ ISO_639_2 "zen"
    "zgh" -> Right $ ISO_639_2 "zgh"
    "zha" -> Right $ ISO_639_2 "zha"
    "zul" -> Right $ ISO_639_2 "zul"
    "zun" -> Right $ ISO_639_2 "zun"
    "zza" -> Right $ ISO_639_2 "zza"
    _ -> Left $ "Unknown ISO_639_2: " <> T.unpack txt

iso_639_2ToBytes :: ISO_639_2 -> LBS.ByteString
iso_639_2ToBytes = LBS8.pack . unISO_639_2

iso_639_2ToText :: ISO_639_2 -> T.Text
iso_639_2ToText = T.pack . unISO_639_2

languageISO_639_2 :: Language -> Maybe ISO_639_2
languageISO_639_2 lang =
  case lang of
    APucikwar -> Nothing
    Aari -> Nothing
    Aariya -> Nothing
    Aasax -> Nothing
    Abadi -> Nothing
    Abaga -> Nothing
    AbaiSungai -> Nothing
    Abanyom -> Nothing
    Abar -> Nothing
    Abau -> Nothing
    Abaza -> Nothing
    Abe -> Nothing
    AbenlenAyta -> Nothing
    Abidji -> Nothing
    Abinomn -> Nothing
    Abipon -> Nothing
    Abishira -> Nothing
    Abkhazian -> Just $ ISO_639_2 "abk"
    Abnaki_Eastern -> Nothing
    Abom -> Nothing
    Abon -> Nothing
    Abron -> Nothing
    Abu -> Nothing
    AbuArapesh -> Nothing
    Abua -> Nothing
    Abui -> Nothing
    Abun -> Nothing
    Abung -> Nothing
    Abure -> Nothing
    Abureni -> Nothing
    AcatenangoSouthwesternCakchiquel -> Nothing
    AcatepecTlapanec -> Nothing
    Achagua -> Nothing
    Achang -> Nothing
    Ache -> Nothing
    AcheYi -> Nothing
    Acheron -> Nothing
    Achinese -> Just $ ISO_639_2 "ace"
    Achterhooks -> Nothing
    AchuarShiwiar -> Nothing
    Achumawi -> Nothing
    Acipa_Western -> Nothing
    Acoli -> Just $ ISO_639_2 "ach"
    Acroa -> Nothing
    Adabe -> Nothing
    Adai -> Nothing
    AdamorobeSignLanguage -> Nothing
    Adang -> Nothing
    Adangbe -> Nothing
    Adangme -> Just $ ISO_639_2 "ada"
    Adap -> Just $ ISO_639_2 "dzo"
    AdasenItneg -> Nothing
    Adele -> Nothing
    Adhola -> Nothing
    Adi -> Nothing
    Adi_Galo -> Nothing
    AdilabadGondi -> Just $ ISO_639_2 "gon"
    Adioukrou -> Nothing
    Adithinngithigh -> Nothing
    AdivasiOriya -> Nothing
    AdiwasiGarasia -> Nothing
    Adonara -> Nothing
    Aduge -> Nothing
    Adyghe -> Just $ ISO_639_2 "ady"
    Adynyamathanha -> Nothing
    Adzera -> Nothing
    Aeka -> Nothing
    Aekyom -> Nothing
    Aequian -> Nothing
    Aer -> Nothing
    Afade -> Nothing
    Afar -> Just $ ISO_639_2 "aar"
    AfghanSignLanguage -> Nothing
    Afitti -> Nothing
    Afrihili -> Just $ ISO_639_2 "afh"
    Afrikaans -> Just $ ISO_639_2 "afr"
    AfroSeminoleCreole -> Nothing
    Agarabi -> Nothing
    Agariya -> Nothing
    Agatu -> Nothing
    Agavotaguerra -> Nothing
    Aghem -> Nothing
    Aghu -> Nothing
    AghuTharnggala -> Nothing
    AghuTharnggalu -> Nothing
    Aghul -> Nothing
    Aghwan -> Nothing
    Agi -> Nothing
    Agob -> Nothing
    Agoi -> Nothing
    Agta_MtIraya -> Nothing
    Aguacateco -> Nothing
    Aguano -> Nothing
    Aguaruna -> Nothing
    Aguna -> Nothing
    AgusanManobo -> Nothing
    Agutaynen -> Nothing
    Agwagwune -> Nothing
    Ahan -> Nothing
    Ahanta -> Nothing
    Ahe -> Nothing
    AheriGondi -> Just $ ISO_639_2 "gon"
    Aheu -> Nothing
    Ahirani -> Nothing
    Ahom -> Nothing
    Ahtena -> Nothing
    AiCham -> Nothing
    Aigon -> Nothing
    Aiklep -> Nothing
    Aiku -> Nothing
    Aimaq -> Nothing
    Aimele -> Nothing
    Aimol -> Nothing
    Ainbai -> Nothing
    Ainu_China -> Nothing
    Ainu_Japan -> Just $ ISO_639_2 "ain"
    Aiome -> Nothing
    Airoran -> Nothing
    Aiton -> Nothing
    Aja_Benin -> Nothing
    Aja_Sudan -> Nothing
    Ajawa -> Nothing
    Ajie -> Nothing
    AjyininkaApurucayali -> Nothing
    Ak -> Nothing
    Aka -> Nothing
    AkaBea -> Nothing
    AkaBo -> Nothing
    AkaCari -> Nothing
    AkaJeru -> Nothing
    AkaKede -> Nothing
    AkaKol -> Nothing
    AkaKora -> Nothing
    Akan -> Just $ ISO_639_2 "aka"
    AkarBale -> Nothing
    Akaselem -> Nothing
    Akawaio -> Nothing
    Ake -> Nothing
    Akebu -> Nothing
    Akei -> Nothing
    Akeu -> Nothing
    Akha -> Nothing
    Akhvakh -> Nothing
    Akkadian -> Just $ ISO_639_2 "akk"
    AkkalaSami -> Nothing
    Aklanon -> Nothing
    Akolet -> Nothing
    Akoose -> Nothing
    Akoye -> Nothing
    Akpa -> Nothing
    Akpes -> Nothing
    Akrukay -> Nothing
    Akuku -> Nothing
    Akum -> Nothing
    Akuntsu -> Nothing
    Akurio -> Nothing
    Akwa -> Nothing
    AkyaungAriNaga -> Nothing
    AlSayyidBedouinSignLanguage -> Nothing
    Alaba -> Nothing
    Alabama -> Nothing
    AlabatIslandAgta -> Nothing
    AlacatlatzalaMixtec -> Nothing
    Alago -> Nothing
    Alagwa -> Nothing
    Alak -> Nothing
    Alamblak -> Nothing
    Alangan -> Nothing
    Alanic -> Nothing
    Alapmunte -> Nothing
    Alatil -> Nothing
    Alawa -> Nothing
    Albanian -> Just $ ISO_639_2 "alb"
    AlbanianSignLanguage -> Nothing
    Albanian_ArberesheDialect -> Nothing
    Albanian_Arvanitika -> Nothing
    Albanian_Gheg -> Nothing
    Albanian_Tosk -> Nothing
    AlbarradasSignLanguage -> Nothing
    AlbayBicolano -> Nothing
    AlcozaucaMixtec -> Nothing
    Alege -> Nothing
    Alekano -> Nothing
    Alemannic -> Nothing
    Aleut -> Just $ ISO_639_2 "ale"
    AlgerianJewishSignLanguage -> Nothing
    AlgerianSignLanguage -> Nothing
    Algonquin -> Nothing
    Ali -> Nothing
    Alladian -> Nothing
    Allar -> Nothing
    Alngith -> Nothing
    AloPhola -> Nothing
    AloapamZapotec -> Nothing
    Alor -> Nothing
    Alsea -> Nothing
    Alta_Northern -> Nothing
    Altai_Northern -> Nothing
    Altai_Southern -> Just $ ISO_639_2 "alt"
    AluKurumba -> Nothing
    Alugu -> Nothing
    AlumuTesu -> Nothing
    Alune -> Nothing
    Aluo -> Nothing
    Alur -> Nothing
    Alutor -> Nothing
    AlviriVidari -> Nothing
    Alyawarr -> Nothing
    Ama_PapuaNewGuinea -> Nothing
    Ama_Sudan -> Nothing
    Amahai -> Nothing
    Amahuaca -> Nothing
    Amaimon -> Nothing
    Amal -> Nothing
    AmamiKoniyaSignLanguage -> Nothing
    Amanab -> Nothing
    Amanaye -> Nothing
    AmapaCreole -> Nothing
    Amara -> Nothing
    Amarag -> Nothing
    Amarakaeri -> Nothing
    Amarasi -> Nothing
    AmatlanZapotec -> Nothing
    Amba_SolomonIslands -> Nothing
    Amba_Uganda -> Nothing
    Ambai -> Nothing
    Ambakich -> Nothing
    AmbalaAyta -> Nothing
    Ambelau -> Nothing
    Ambele -> Nothing
    Amblong -> Nothing
    Ambo -> Nothing
    AmboPascoQuechua -> Nothing
    AmboneseMalay -> Nothing
    Ambrak -> Nothing
    Ambulas -> Nothing
    Amdang -> Nothing
    AmdoTibetan -> Nothing
    Amele -> Nothing
    Amerax -> Nothing
    AmericanSignLanguage -> Nothing
    AmganadIfugao -> Nothing
    Amharic -> Just $ ISO_639_2 "amh"
    Ami -> Nothing
    Amikoana -> Nothing
    Amis -> Nothing
    Amo -> Nothing
    AmoltepecMixtec -> Nothing
    Ampanang -> Nothing
    AmpariDogon -> Nothing
    Amri -> Nothing
    Amto -> Nothing
    Amundava -> Nothing
    Amuzgo_Ipalapa -> Nothing
    Amuzgo_SanPedroAmuzgos -> Nothing
    AnaTingaDogon -> Nothing
    Anaang -> Nothing
    Anakalangu -> Nothing
    Anal -> Nothing
    Anam -> Nothing
    Anambe -> Nothing
    Anamgura -> Nothing
    Anasi -> Nothing
    Anca -> Nothing
    AncientGreek -> Just $ ISO_639_2 "grc"
    AncientHebrew -> Nothing
    AncientMacedonian -> Nothing
    AncientNorthArabian -> Nothing
    AncientZapotec -> Nothing
    Andaandi -> Nothing
    Andai -> Nothing
    Andajin -> Nothing
    AndalusianArabic -> Nothing
    AndamanCreoleHindi -> Nothing
    Andaqui -> Nothing
    Andarum -> Nothing
    Andegerebinha -> Nothing
    Andh -> Nothing
    Andi -> Nothing
    Andio -> Nothing
    Andoa -> Nothing
    Andoque -> Nothing
    AndraHus -> Nothing
    Aneityum -> Nothing
    Anem -> Nothing
    AnemeWake -> Nothing
    Anfillo -> Nothing
    Angaatiha -> Nothing
    Angaite -> Nothing
    Angal -> Nothing
    AngalEnen -> Nothing
    AngalHeneng -> Nothing
    AngamiNaga -> Nothing
    AnggurukYali -> Nothing
    Angika -> Just $ ISO_639_2 "anp"
    Angkamuthi -> Nothing
    AngloNorman -> Nothing
    AngloSaxon -> Just $ ISO_639_2 "ang"
    Angloromani -> Nothing
    Angolar -> Nothing
    Angor -> Nothing
    Angoram -> Nothing
    AngosturasTunebo -> Nothing
    Anguthimri -> Nothing
    Ani -> Nothing
    AniPhowa -> Nothing
    Anii -> Nothing
    Animere -> Nothing
    Anindilyakwa -> Nothing
    Aninka -> Nothing
    Anjam -> Nothing
    Ankave -> Nothing
    Anmatyerre -> Nothing
    Anor -> Nothing
    Anserma -> Nothing
    Ansus -> Nothing
    Antakarinya -> Nothing
    AntankaranaMalagasy -> Just $ ISO_639_2 "mlg"
    AntiguaAndBarbudaCreoleEnglish -> Nothing
    Anu -> Nothing
    Anuak -> Nothing
    Anufo -> Nothing
    Anuki -> Nothing
    Anus -> Nothing
    Anuta -> Nothing
    Anyin -> Nothing
    AnyinMorofo -> Nothing
    AoNaga -> Nothing
    Aoheng -> Nothing
    Aore -> Nothing
    ApMa -> Nothing
    Apache_Jicarilla -> Nothing
    Apache_Kiowa -> Nothing
    Apache_Lipan -> Nothing
    Apache_MescaleroChiricahua -> Nothing
    Apache_Western -> Nothing
    Apalachee -> Nothing
    Apalai -> Nothing
    Apali -> Nothing
    Apalik -> Nothing
    ApascoApoalaMixtec -> Nothing
    Apatani -> Nothing
    Apiaca -> Nothing
    Apinaye -> Nothing
    Apma -> Nothing
    AproumuAizi -> Nothing
    Apurina -> Nothing
    Aputai -> Nothing
    Aquitanian -> Nothing
    Arabana -> Nothing
    Arabela -> Nothing
    Arabic -> Just $ ISO_639_2 "ara"
    Arabic_Algerian -> Nothing
    Arabic_AlgerianSaharan -> Nothing
    Arabic_Baharna -> Nothing
    Arabic_Chadian -> Nothing
    Arabic_Cypriot -> Nothing
    Arabic_Dhofari -> Nothing
    Arabic_EasternEgyptianBedawi -> Nothing
    Arabic_Egyptian -> Nothing
    Arabic_Hadrami -> Nothing
    Arabic_Hijazi -> Nothing
    Arabic_JudeoMoroccan -> Nothing
    Arabic_JudeoTunisian -> Nothing
    Arabic_JudeoYemeni -> Nothing
    Arabic_Libyan -> Nothing
    Arabic_Mesopotamian -> Nothing
    Arabic_Moroccan -> Nothing
    Arabic_Najdi -> Nothing
    Arabic_NorthLevantine -> Nothing
    Arabic_NorthMesopotamian -> Nothing
    Arabic_Omani -> Nothing
    Arabic_Sanaani -> Nothing
    Arabic_SouthLevantine -> Nothing
    Arabic_Standard -> Nothing
    Arabic_Sudanese -> Nothing
    Arabic_Tajiki -> Nothing
    Arabic_Uzbeki -> Nothing
    Arafundi -> Nothing
    Aragonese -> Just $ ISO_639_2 "arg"
    Arakanese -> Nothing
    Araki -> Nothing
    Arakwal -> Nothing
    AralleTabulahan -> Nothing
    Aramaic -> Just $ ISO_639_2 "arc"
    Aramanik -> Nothing
    Arammba -> Nothing
    Aranadan -> Nothing
    AranamaTamique -> Nothing
    Arandai -> Nothing
    Araona -> Nothing
    Arapaho -> Just $ ISO_639_2 "arp"
    Arapaso -> Nothing
    Arara_MatoGrosso -> Nothing
    Ararandewara -> Nothing
    Araucanian -> Just $ ISO_639_2 "arn"
    Arawak -> Just $ ISO_639_2 "arw"
    Arawete -> Nothing
    Arawum -> Nothing
    Arbore -> Nothing
    Archi -> Nothing
    ArdhamagadhiPrakrit -> Nothing
    Are -> Nothing
    Areare -> Nothing
    Areba -> Nothing
    Arem -> Nothing
    ArequipaLaUnionQuechua -> Nothing
    ArgentineSignLanguage -> Nothing
    Argobba -> Nothing
    Arguni -> Nothing
    Arha -> Nothing
    Arho -> Nothing
    Arhuaco -> Nothing
    Ari -> Nothing
    Aribwatsa -> Nothing
    Aribwaung -> Nothing
    ArifamaMiniafia -> Nothing
    Arigidi -> Nothing
    Arikapu -> Nothing
    Arikara -> Nothing
    Arikem -> Nothing
    Arin -> Nothing
    Aringa -> Nothing
    Arma -> Nothing
    Armazic -> Nothing
    Armenian -> Just $ ISO_639_2 "arm"
    ArmenianSignLanguage -> Nothing
    Aromanian -> Just $ ISO_639_2 "rup"
    AropLukep -> Nothing
    AropSissano -> Nothing
    Arosi -> Nothing
    Arritinngithigh -> Nothing
    Arta -> Nothing
    Arua -> Nothing
    Aruamu -> Nothing
    Aruek -> Nothing
    Aruop -> Nothing
    Arutani -> Nothing
    As -> Nothing
    Asaroo -> Nothing
    Asas -> Nothing
    Ashaninka -> Nothing
    Ashe -> Nothing
    AsheninkaPajonal -> Nothing
    AsheninkaPerene -> Nothing
    Ashkun -> Nothing
    AshoChin -> Nothing
    Ashtiani -> Nothing
    Asilulu -> Nothing
    Askopan -> Nothing
    Asmat_Yaosakor -> Nothing
    Asoa -> Nothing
    Assamese -> Just $ ISO_639_2 "asm"
    Assan -> Nothing
    Assangori -> Nothing
    Assiniboine -> Nothing
    AssyrianNeoAramaic -> Nothing
    Asturian -> Just $ ISO_639_2 "ast"
    Asu_Nigeria -> Nothing
    Asu_Tanzania -> Nothing
    AsueAwyu -> Nothing
    Asumboa -> Nothing
    AsuncionMixtepecZapotec -> Nothing
    Asuri -> Nothing
    Asurini -> Nothing
    Ata -> Nothing
    AtaManobo -> Nothing
    Atakapa -> Nothing
    Atampaya -> Nothing
    AtatlahucaMixtec -> Nothing
    Atayal -> Nothing
    Atemble -> Nothing
    Athpariya -> Nothing
    Ati -> Nothing
    Atikamekw -> Nothing
    Atohwaim -> Nothing
    Atong -> Nothing
    Atorada -> Nothing
    Atruahi -> Nothing
    Atsahuaca -> Nothing
    Atsam -> Nothing
    Atsugewi -> Nothing
    Atta_Faire -> Nothing
    Atta_Pamplona -> Nothing
    Atta_Pudtol -> Nothing
    AttapadyKurumba -> Nothing
    Attie -> Nothing
    Atuence -> Nothing
    AtzingoMatlatzinca -> Nothing
    Au -> Nothing
    Auhelawa -> Nothing
    Aukan -> Nothing
    Aulua -> Nothing
    Aura -> Nothing
    Aushi -> Nothing
    Aushiri -> Nothing
    Austral -> Nothing
    AustralianAboriginesSignLanguage -> Nothing
    AustralianSignLanguage -> Nothing
    AustrianSignLanguage -> Nothing
    Auvergnat -> Nothing
    Auwe -> Nothing
    Auye -> Nothing
    Auyokawa -> Nothing
    AvaCanoeiro -> Nothing
    Avaric -> Just $ ISO_639_2 "ava"
    Avatime -> Nothing
    Avau -> Nothing
    Avestan -> Just $ ISO_639_2 "ave"
    Avikam -> Nothing
    Avokaya -> Nothing
    Awa -> Nothing
    AwaCuaiquer -> Nothing
    Awa_China -> Nothing
    Awabakal -> Nothing
    AwadBing -> Nothing
    Awadhi -> Just $ ISO_639_2 "awa"
    Awak -> Nothing
    Awar -> Nothing
    Awara -> Nothing
    Awbono -> Nothing
    Awera -> Nothing
    Aweti -> Nothing
    Awing -> Nothing
    Awiyaana -> Nothing
    Awjilah -> Nothing
    Awngi -> Nothing
    Awngthim -> Nothing
    Awtuw -> Nothing
    AwuYi -> Nothing
    Awun -> Nothing
    Awutu -> Nothing
    Awyi -> Nothing
    Awyu_Central -> Nothing
    Awyu_Edera -> Nothing
    Awyu_Jair -> Nothing
    Awyu_South -> Nothing
    Axamb -> Nothing
    AxiYi -> Nothing
    Ayabadhu -> Nothing
    AyacuchoQuechua -> Nothing
    AyautlaMazatec -> Nothing
    Ayere -> Nothing
    Ayerrerenge -> Nothing
    Ayi_China -> Nothing
    Ayi_PapuaNewGuinea -> Nothing
    Ayiwo -> Nothing
    Ayizi -> Nothing
    AyizoGbe -> Nothing
    Aymara -> Just $ ISO_639_2 "aym"
    Aymara_Central -> Just $ ISO_639_2 "aym"
    AyoquescoZapotec -> Nothing
    Ayoreo -> Nothing
    Ayta_Bataan -> Nothing
    Ayta_Sorsogon -> Nothing
    Ayu -> Nothing
    AyutlaMixtec -> Nothing
    Azerbaijani -> Just $ ISO_639_2 "aze"
    Azerbaijani_North -> Just $ ISO_639_2 "aze"
    Azha -> Nothing
    AzheYi -> Nothing
    AzoyuTlapanec -> Nothing
    Baan -> Nothing
    Baangi -> Nothing
    Baatonum -> Nothing
    Baba -> Nothing
    BabaMalay -> Nothing
    BabaliaCreoleArabic -> Nothing
    Babango -> Nothing
    Babanki -> Nothing
    Babatana -> Nothing
    Babine -> Nothing
    Babuza -> Nothing
    Bacama -> Nothing
    BacaneseMalay -> Nothing
    BachajonTzeltal -> Nothing
    Bactrian -> Nothing
    Bada_Indonesia -> Nothing
    Bada_Nigeria -> Nothing
    Badaga -> Nothing
    Bade -> Nothing
    Badeshi -> Nothing
    BadiKanum -> Nothing
    Badimaya -> Nothing
    Badjiri -> Nothing
    Badui -> Nothing
    Badyara -> Nothing
    Baeggu -> Nothing
    Baelelea -> Nothing
    Baetora -> Nothing
    Bafanji -> Nothing
    BafawBalong -> Nothing
    Bafia -> Nothing
    Bafut -> Nothing
    BagaBinari -> Nothing
    BagaKaloum -> Nothing
    BagaKoga -> Nothing
    BagaManduri -> Nothing
    BagaMboteni -> Nothing
    BagaSitemu -> Nothing
    BagaSobane -> Nothing
    Bagheli -> Nothing
    Bagirmi -> Nothing
    BagirmiFulfulde -> Nothing
    BagoKusuntu -> Nothing
    Bagri -> Nothing
    Bagupi -> Nothing
    Bagusa -> Nothing
    Bagvalal -> Nothing
    BahaBuyang -> Nothing
    Baham -> Nothing
    BahamasCreoleEnglish -> Nothing
    Bahau -> Nothing
    BahauRiverKenyah -> Nothing
    Bahinemo -> Nothing
    Bahing -> Nothing
    Bahnar -> Nothing
    Bahonsuai -> Nothing
    Bai -> Nothing
    Baibai -> Nothing
    Baikeno -> Nothing
    Baima -> Nothing
    Baimak -> Nothing
    BainoukGunyaamolo -> Nothing
    BainoukGunyuno -> Nothing
    BainoukSamik -> Nothing
    Baiso -> Nothing
    Bajan -> Nothing
    Bajelani -> Nothing
    Bajjika -> Nothing
    Baka_Cameroon -> Nothing
    Baka_Sudan -> Nothing
    Bakairi -> Nothing
    Bakaka -> Nothing
    Bakhtiari -> Nothing
    Baki -> Nothing
    Bakoko -> Nothing
    Bakole -> Nothing
    Bakpinka -> Nothing
    Bakumpai -> Nothing
    BakungKenyah -> Nothing
    Bakwe -> Nothing
    Balaesang -> Nothing
    Balaibalan -> Nothing
    Balangao -> Nothing
    Balangingi -> Nothing
    BalantaGanja -> Nothing
    BalantaKentohe -> Nothing
    Balantak -> Nothing
    Balau -> Just $ ISO_639_2 "iba"
    Baldemu -> Nothing
    BaliSignLanguage -> Nothing
    Bali_DemocraticRepublicOfCongo -> Nothing
    Bali_Nigeria -> Nothing
    Balinese -> Just $ ISO_639_2 "ban"
    BalineseMalay -> Nothing
    BalkanGagauzTurkish -> Nothing
    BalkanRomani -> Nothing
    Balo -> Nothing
    Baloi -> Nothing
    Balti -> Nothing
    BalticRomani -> Nothing
    BaluanPam -> Nothing
    Baluchi -> Just $ ISO_639_2 "bal"
    BamakoSignLanguage -> Nothing
    Bamali -> Nothing
    Bambalang -> Nothing
    Bambam -> Nothing
    Bambara -> Just $ ISO_639_2 "bam"
    Bambassi -> Nothing
    BambiliBambui -> Nothing
    Bamenyam -> Nothing
    Bamu -> Nothing
    Bamukumbit -> Nothing
    Bamun -> Nothing
    Bamunka -> Nothing
    Bamwe -> Nothing
    BanKhorSignLanguage -> Nothing
    Bana -> Nothing
    BanaoItneg -> Nothing
    Banaro -> Nothing
    Banawa -> Nothing
    BandaBambari -> Nothing
    BandaBanda -> Nothing
    BandaMalay -> Nothing
    BandaMbres -> Nothing
    BandaNdele -> Nothing
    BandaYangere -> Nothing
    Banda_Indonesia -> Nothing
    Bandi -> Nothing
    Bandial -> Nothing
    Bandjalang -> Nothing
    Bandjigali -> Nothing
    Bangala -> Nothing
    Bangandu -> Nothing
    Bangba -> Nothing
    BangeriMeDogon -> Nothing
    Banggai -> Nothing
    Banggarla -> Nothing
    Bangi -> Nothing
    Bangolan -> Nothing
    Bangubangu -> Nothing
    Bangwinji -> Nothing
    Baniva -> Nothing
    Baniwa -> Nothing
    Banjar -> Nothing
    Bankagooma -> Nothing
    Bankal -> Nothing
    BankanTeyDogon -> Nothing
    Bankon -> Nothing
    Bannoni -> Nothing
    Bantawa -> Nothing
    Bantayanon -> Nothing
    Bantik -> Nothing
    Bantoanon -> Nothing
    Baoule -> Nothing
    BaraMalagasy -> Just $ ISO_639_2 "mlg"
    Baraamu -> Nothing
    Barababaraba -> Nothing
    Barai -> Nothing
    Barakai -> Nothing
    BaramKayan -> Nothing
    Barama -> Nothing
    Barambu -> Nothing
    Baramu -> Nothing
    Barapasi -> Nothing
    Baras -> Nothing
    Barasana -> Nothing
    Barbacoas -> Nothing
    Barbareno -> Nothing
    BarclayvilleGrebo -> Just $ ISO_639_2 "grb"
    Bardi -> Nothing
    Bare -> Nothing
    Barein -> Nothing
    Bargam -> Nothing
    Bari -> Nothing
    Bariai -> Nothing
    Bariji -> Nothing
    Barikanchi -> Nothing
    Barikewa -> Nothing
    Barok -> Nothing
    Barombi -> Nothing
    BarroNegroTunebo -> Nothing
    BarrowPoint -> Nothing
    Baruga -> Nothing
    Baruya -> Nothing
    Barwe -> Nothing
    BarzaniJewishNeoAramaic -> Nothing
    BasaGumna -> Nothing
    BasaGurmana -> Nothing
    Basa_Cameroon -> Just $ ISO_639_2 "bas"
    Basa_Nigeria -> Nothing
    Basap -> Nothing
    Basay -> Nothing
    Bashkardi -> Nothing
    Bashkir -> Just $ ISO_639_2 "bak"
    Basketo -> Nothing
    Basque -> Just $ ISO_639_2 "baq"
    Bassa -> Nothing
    BassaKontagora -> Nothing
    Bassari -> Nothing
    Bassossi -> Nothing
    Bata -> Nothing
    BatadIfugao -> Nothing
    Batak -> Nothing
    BatakAlasKluet -> Nothing
    BatakAngkola -> Nothing
    BatakDairi -> Nothing
    BatakKaro -> Nothing
    BatakMandailing -> Nothing
    BatakSimalungun -> Nothing
    BatakToba -> Nothing
    Batanga -> Nothing
    Batek -> Nothing
    Bateri -> Nothing
    Bathari -> Nothing
    Bati_Cameroon -> Nothing
    Bati_Indonesia -> Nothing
    Bats -> Nothing
    Batu -> Nothing
    Batui -> Nothing
    Batuley -> Nothing
    Batyala -> Nothing
    Bau -> Nothing
    Bauchi -> Nothing
    Bauni -> Nothing
    Baure -> Nothing
    Bauria -> Nothing
    Bauro -> Nothing
    Bauwaki -> Nothing
    Bauzi -> Nothing
    Bavarian -> Nothing
    BawmChin -> Nothing
    BayMiwok -> Nothing
    Bayali -> Nothing
    Baybayanon -> Nothing
    Baygo -> Nothing
    Bayono -> Nothing
    Bayot -> Nothing
    Bayungu -> Nothing
    Bazigar -> Nothing
    Beami -> Nothing
    Beaver -> Nothing
    Beba -> Nothing
    Bebe -> Nothing
    Bebele -> Nothing
    Bebeli -> Nothing
    Bebil -> Nothing
    Bedjond -> Nothing
    Bedoanas -> Nothing
    Beeke -> Nothing
    Beele -> Nothing
    Beembe -> Nothing
    Beezen -> Nothing
    Befang -> Nothing
    BegbereEjar -> Nothing
    Beginci -> Nothing
    Beja -> Just $ ISO_639_2 "bej"
    Bekati -> Nothing
    Bekwarra -> Nothing
    Bekwil -> Nothing
    Belait -> Nothing
    BelandaBor -> Nothing
    BelandaViri -> Nothing
    Belarusian -> Just $ ISO_639_2 "bel"
    BelgianSignLanguage -> Nothing
    Belhariya -> Nothing
    Beli_PapuaNewGuinea -> Nothing
    Beli_Sudan -> Nothing
    BelizeKriolEnglish -> Nothing
    BellaCoola -> Nothing
    Bellari -> Nothing
    Belning -> Nothing
    Bemba_DemocraticRepublicOfCongo -> Nothing
    Bemba_Zambia -> Just $ ISO_639_2 "bem"
    Bembe -> Nothing
    BenTeyDogon -> Nothing
    Bena_Nigeria -> Nothing
    Bena_Tanzania -> Nothing
    Benabena -> Nothing
    Benamanga -> Nothing
    Bench -> Nothing
    Bende -> Nothing
    Bendi -> Nothing
    Beng -> Nothing
    Benga -> Nothing
    Bengali -> Just $ ISO_639_2 "ben"
    Benggoi -> Nothing
    Bengkulu -> Nothing
    Bentong -> Nothing
    Benyadu -> Nothing
    Beothuk -> Nothing
    Bepour -> Nothing
    Bera -> Nothing
    Berakou -> Nothing
    BerauMalay -> Nothing
    Berawan -> Nothing
    BerbiceCreoleDutch -> Nothing
    Berik -> Nothing
    Berinomo -> Nothing
    Bernde -> Nothing
    Berom -> Nothing
    Berta -> Nothing
    Berti -> Nothing
    Besisi -> Nothing
    Besme -> Nothing
    Besoa -> Nothing
    Betaf -> Nothing
    Betawi -> Nothing
    Bete -> Nothing
    BeteBendi -> Nothing
    Beti_Cameroon -> Nothing
    Beti_CoteDivoire -> Nothing
    BettaKurumba -> Nothing
    Bezhta -> Nothing
    Bhadrawahi -> Nothing
    Bhalay -> Nothing
    Bharia -> Nothing
    Bhatola -> Nothing
    Bhatri -> Nothing
    Bhattiyali -> Nothing
    Bhaya -> Nothing
    Bhele -> Nothing
    Bhilali -> Nothing
    Bhili -> Nothing
    Bhojpuri -> Just $ ISO_639_2 "bho"
    BhotiKinnauri -> Nothing
    Bhunjia -> Nothing
    Biafada -> Nothing
    Biage -> Nothing
    Biak -> Nothing
    Biali -> Nothing
    BianMarind -> Nothing
    Biangai -> Nothing
    Biao -> Nothing
    BiaoJiaoMien -> Nothing
    BiaoMon -> Nothing
    Biatah -> Nothing
    Bibbulman -> Nothing
    Bidhawal -> Nothing
    Bidiyo -> Nothing
    Bidyara -> Nothing
    Bidyogo -> Nothing
    Biem -> Nothing
    Bierebo -> Nothing
    Bieria -> Nothing
    Biete -> Nothing
    BigNambas -> Nothing
    Biga -> Nothing
    Bigambal -> Nothing
    Bih -> Nothing
    Bijim -> Nothing
    Bijori -> Nothing
    Bikaru -> Nothing
    Bikol -> Nothing
    Bikya -> Nothing
    Bila -> Nothing
    Bilakura -> Nothing
    Bilaspuri -> Nothing
    Bilba -> Nothing
    Bilbil -> Nothing
    Bile -> Nothing
    Bilin -> Just $ ISO_639_2 "byn"
    BilmaKanuri -> Just $ ISO_639_2 "kau"
    Biloxi -> Nothing
    Bilua -> Nothing
    Bilur -> Nothing
    Bima -> Nothing
    Bimin -> Nothing
    Bimoba -> Nothing
    Bina_Nigeria -> Nothing
    Bina_PapuaNewGuinea -> Nothing
    Binahari -> Nothing
    Binandere -> Nothing
    Bindal -> Nothing
    Bine -> Nothing
    Bini -> Just $ ISO_639_2 "bin"
    Binji -> Nothing
    BinonganItneg -> Nothing
    Bintauna -> Nothing
    Bintulu -> Nothing
    Binukid -> Nothing
    Binumarien -> Nothing
    Bipi -> Nothing
    Birale -> Nothing
    Birao -> Nothing
    Birgit -> Nothing
    Birhor -> Nothing
    Biri -> Nothing
    Biritai -> Nothing
    Birked -> Nothing
    Birri -> Nothing
    Birrpayi -> Nothing
    Birwa -> Just $ ISO_639_2 "nso"
    Biseni -> Nothing
    BishnupriyaManipuri -> Nothing
    Bishuo -> Nothing
    Bisis -> Nothing
    Bislama -> Just $ ISO_639_2 "bis"
    Bisorio -> Nothing
    Bissa -> Nothing
    Bisu -> Nothing
    Bit -> Nothing
    Bitare -> Nothing
    Bitur -> Nothing
    Biwat -> Nothing
    Biyo -> Nothing
    Biyom -> Nothing
    Blablanga -> Nothing
    Blackfoot -> Just $ ISO_639_2 "bla"
    Blafe -> Nothing
    Blagar -> Nothing
    Blang -> Nothing
    Blissymbols -> Nothing
    BoRukul -> Nothing
    Bo_Laos -> Nothing
    Bo_PapuaNewGuinea -> Nothing
    Boano_Maluku -> Nothing
    Boano_Sulawesi -> Nothing
    Bobongko -> Nothing
    Bobot -> Nothing
    BodoGadaba -> Nothing
    BodoParja -> Nothing
    Bodo_CentralAfricanRepublic -> Nothing
    Bodo_India -> Nothing
    Bofi -> Nothing
    Boga -> Nothing
    Bogan -> Nothing
    Bogaya -> Nothing
    Boghom -> Nothing
    Boguru -> Nothing
    BohtanNeoAramaic -> Nothing
    Boikin -> Nothing
    Bokha -> Nothing
    Boko_Benin -> Nothing
    Boko_DemocraticRepublicOfCongo -> Nothing
    Bokobaru -> Nothing
    Bokoto -> Nothing
    Bokyi -> Nothing
    Bola -> Nothing
    Bolango -> Nothing
    Bole -> Nothing
    Bolgarian -> Nothing
    Bolgo -> Nothing
    Bolia -> Nothing
    Bolinao -> Nothing
    BolivianSignLanguage -> Nothing
    Bolo -> Nothing
    Boloki -> Nothing
    Bolon -> Nothing
    Bolondo -> Nothing
    Bolongan -> Nothing
    Bolyu -> Nothing
    Bom -> Nothing
    Boma -> Nothing
    Bomboli -> Nothing
    Bomboma -> Nothing
    Bomitaba -> Nothing
    Bomu -> Nothing
    Bomwali -> Nothing
    BonGula -> Nothing
    Bonan -> Nothing
    Bondei -> Nothing
    Bondo -> Nothing
    BondoukouKulango -> Nothing
    BondumDomDogon -> Nothing
    Bonerate -> Nothing
    Bonggi -> Nothing
    Bonggo -> Nothing
    Bongili -> Nothing
    Bongo -> Nothing
    Bongu -> Nothing
    Boni -> Nothing
    Bonjo -> Nothing
    Bonkeng -> Nothing
    Bonkiman -> Nothing
    Bookan -> Nothing
    Boon -> Nothing
    Boor -> Nothing
    Bora -> Nothing
    BoranaArsiGujiOromo -> Nothing
    BorderKuna -> Nothing
    Borei -> Nothing
    BorguFulfulde -> Nothing
    Borna -> Nothing
    Boro -> Nothing
    Boro_Ghana -> Nothing
    Borong -> Nothing
    Bororo -> Nothing
    Boruca -> Nothing
    Boselewa -> Nothing
    Bosngun -> Nothing
    Bosnian -> Just $ ISO_639_2 "bos"
    BoteMajhi -> Nothing
    Botlikh -> Nothing
    BotolanSambal -> Nothing
    BounaKulango -> Nothing
    Bouni -> Nothing
    Bouyei -> Nothing
    Bozaba -> Nothing
    Bragat -> Nothing
    Brahui -> Nothing
    Braj -> Just $ ISO_639_2 "bra"
    BrazilianSignLanguage -> Nothing
    BrekKaren -> Nothing
    Brem -> Nothing
    Breri -> Nothing
    Breton -> Just $ ISO_639_2 "bre"
    Bribri -> Nothing
    BribriSignLanguage -> Nothing
    Brithenig -> Nothing
    BritishSignLanguage -> Nothing
    Brokkat -> Nothing
    Brokpake -> Nothing
    Brokskat -> Nothing
    BrookesPointPalawano -> Nothing
    BroomePearlingLuggerPidgin -> Nothing
    BruncaSignLanguage -> Nothing
    Brunei -> Nothing
    BruneiBisaya -> Nothing
    BrunyIslandTasmanian -> Nothing
    Bu -> Nothing
    BuNaoBunu -> Nothing
    Bu_BauchiState -> Nothing
    Bua -> Nothing
    BualkhawChin -> Nothing
    Buamu -> Nothing
    Bube -> Nothing
    Bubi -> Nothing
    Bubia -> Nothing
    BudehStieng -> Nothing
    Budibud -> Nothing
    Budik -> Nothing
    BudongBudong -> Nothing
    Budu -> Nothing
    Budukh -> Nothing
    Buduma -> Nothing
    Budza -> Nothing
    Bugan -> Nothing
    Bugawac -> Nothing
    Bughotu -> Nothing
    Buginese -> Just $ ISO_639_2 "bug"
    Buglere -> Nothing
    Bugun -> Nothing
    Buhid -> Nothing
    BuhinonBikol -> Nothing
    Buhutu -> Nothing
    Bujhyal -> Nothing
    BukarSadong -> Nothing
    Bukat -> Nothing
    Bukharic -> Nothing
    BukitMalay -> Nothing
    Bukitan -> Nothing
    Bukiyip -> Nothing
    Buksa -> Nothing
    Bukusu -> Nothing
    Bukwen -> Nothing
    Bulgarian -> Just $ ISO_639_2 "bul"
    BulgarianSignLanguage -> Nothing
    Bulgebi -> Nothing
    Buli -> Nothing
    Buli_Ghana -> Nothing
    Buli_Indonesia -> Nothing
    BullomSo -> Nothing
    BuloStieng -> Nothing
    Bulu_Cameroon -> Nothing
    Bulu_PapuaNewGuinea -> Nothing
    Bum -> Nothing
    Bumaji -> Nothing
    Bumang -> Nothing
    BumbitaArapesh -> Nothing
    Bumthangkha -> Nothing
    Bun -> Nothing
    Buna -> Nothing
    Bunaba -> Nothing
    Bunak -> Nothing
    Bunama -> Nothing
    Bundeli -> Nothing
    Bung -> Nothing
    Bungain -> Nothing
    Bunganditj -> Nothing
    Bungku -> Nothing
    Bungu -> Nothing
    BunogeDogon -> Nothing
    Bunun -> Nothing
    Buol -> Nothing
    BuraPabir -> Nothing
    Burak -> Nothing
    Buraka -> Nothing
    Burarra -> Nothing
    Burate -> Nothing
    Burduna -> Nothing
    Bure -> Nothing
    Buriat -> Just $ ISO_639_2 "bua"
    Burji -> Nothing
    Burmbar -> Nothing
    Burmese -> Just $ ISO_639_2 "bur"
    Burmeso -> Nothing
    Buru_Indonesia -> Nothing
    Buru_Nigeria -> Nothing
    Burui -> Nothing
    BurumMindik -> Nothing
    Burumakok -> Nothing
    Burun -> Nothing
    BurundianSignLanguage -> Nothing
    Burunge -> Nothing
    Burushaski -> Nothing
    Burusu -> Nothing
    Buruwai -> Nothing
    Busa -> Nothing
    Busam -> Nothing
    Busami -> Nothing
    BusangKayan -> Nothing
    Bushi -> Nothing
    Bushoong -> Nothing
    Buso -> Nothing
    Busoa -> Nothing
    Bussa -> Nothing
    Busuu -> Nothing
    ButbutKalinga -> Nothing
    ButmasTur -> Nothing
    Butuanon -> Nothing
    Buwal -> Nothing
    Buxinhua -> Nothing
    Buya -> Nothing
    Buyang -> Nothing
    Buyu -> Nothing
    BuyuanJinuo -> Nothing
    Bwa -> Nothing
    Bwaidoka -> Nothing
    Bwanabwana -> Nothing
    Bwatoo -> Nothing
    BweKaren -> Nothing
    Bwela -> Nothing
    Bwile -> Nothing
    Bwisi -> Nothing
    Byangsi -> Nothing
    Byep -> Nothing
    Caac -> Nothing
    Cabecar -> Nothing
    Cabiyari -> Nothing
    CacaloxtepecMixtec -> Nothing
    Cacaopera -> Nothing
    CacgiaRoglai -> Nothing
    Cacua -> Nothing
    Caddo -> Just $ ISO_639_2 "cad"
    CafundoCreole -> Nothing
    Cagua -> Nothing
    Cahuarano -> Nothing
    Cahuilla -> Nothing
    CajamarcaQuechua -> Nothing
    CajatamboNorthLimaQuechua -> Nothing
    CajonosZapotec -> Nothing
    CajunFrench -> Nothing
    Caka -> Nothing
    CakchiquelQuicheMixedLanguage -> Nothing
    CakfemMushere -> Nothing
    CalamianTagbanwa -> Nothing
    CalderonHighlandQuichua -> Nothing
    Callawalla -> Nothing
    Calo -> Nothing
    Caluyanun -> Nothing
    CamarinesNorteAgta -> Nothing
    CambodianSignLanguage -> Nothing
    CameroonMambila -> Nothing
    CameroonPidgin -> Nothing
    Camling -> Nothing
    Campalagian -> Nothing
    CampidaneseSardinian -> Just $ ISO_639_2 "srd"
    Camsa -> Nothing
    Camtho -> Nothing
    Camunic -> Nothing
    CanarHighlandQuichua -> Nothing
    CandoshiShapra -> Nothing
    Canela -> Nothing
    Canichana -> Nothing
    CaoLan -> Nothing
    CaoMiao -> Nothing
    Capanahua -> Nothing
    Capiznon -> Nothing
    Caquinte -> Nothing
    CarNicobarese -> Nothing
    Cara -> Nothing
    Carabayo -> Nothing
    Caramanta -> Nothing
    Carapana -> Nothing
    Carian -> Nothing
    Carib -> Just $ ISO_639_2 "car"
    CaribbeanHindustani -> Nothing
    CaribbeanJavanese -> Just $ ISO_639_2 "jav"
    Carijona -> Nothing
    CarolinaAlgonquian -> Nothing
    Carolinian -> Nothing
    CarpathianRomani -> Nothing
    Carrier -> Nothing
    Carutana -> Nothing
    CashiboCacataibo -> Nothing
    Cashinahua -> Nothing
    CasiguranDumagatAgta -> Nothing
    CasuarinaCoastAsmat -> Nothing
    CataelanoMandaya -> Nothing
    Catalan -> Just $ ISO_639_2 "cat"
    CatalanSignLanguage -> Nothing
    Catawba -> Nothing
    Cauca -> Nothing
    Cavinena -> Nothing
    Cayubaba -> Nothing
    Cayuga -> Nothing
    Cayuse -> Nothing
    CebaaraSenoufo -> Nothing
    Cebuano -> Just $ ISO_639_2 "ceb"
    Celtiberian -> Nothing
    Cemuhi -> Nothing
    Cen -> Nothing
    CentralAsmat -> Nothing
    CentralAtlasTamazight -> Nothing
    CentralBai -> Nothing
    CentralBerawan -> Nothing
    CentralBicolano -> Nothing
    CentralBontoc -> Nothing
    CentralBontok -> Nothing
    CentralCagayanAgta -> Nothing
    CentralCakchiquel -> Nothing
    CentralDusun -> Nothing
    CentralEasternNigerFulfulde -> Nothing
    CentralGrebo -> Just $ ISO_639_2 "grb"
    CentralHongshuiheZhuang -> Just $ ISO_639_2 "zha"
    CentralHuastecaNahuatl -> Nothing
    CentralHuishuiHmong -> Nothing
    CentralKanuri -> Nothing
    CentralKurdish -> Nothing
    CentralMaewo -> Nothing
    CentralMalay -> Nothing
    CentralMam -> Nothing
    CentralMasela -> Nothing
    CentralMashanHmong -> Nothing
    CentralMnong -> Nothing
    CentralNahuatl -> Nothing
    CentralNicobarese -> Nothing
    CentralOjibwa -> Nothing
    CentralOkinawan -> Nothing
    CentralPalawano -> Nothing
    CentralPame -> Nothing
    CentralPashto -> Nothing
    CentralPokomam -> Nothing
    CentralPomo -> Nothing
    CentralPueblaNahuatl -> Nothing
    CentralSama -> Nothing
    CentralSiberianYupik -> Nothing
    CentralSierraMiwok -> Nothing
    CentralSubanen -> Nothing
    CentralTagbanwa -> Nothing
    CentralTarahumara -> Nothing
    CentralTunebo -> Nothing
    CentralYupik -> Nothing
    Central_LolopoYi -> Nothing
    Centuum -> Nothing
    Cerma -> Nothing
    Chaari -> Nothing
    ChachapoyasQuechua -> Nothing
    Chachi -> Nothing
    Chacobo -> Nothing
    ChadianSignLanguage -> Nothing
    Chadong -> Nothing
    Chagatai -> Just $ ISO_639_2 "chg"
    Chaima -> Nothing
    ChajulIxil -> Nothing
    Chak -> Nothing
    Chakali -> Nothing
    Chakavian -> Nothing
    Chakma -> Nothing
    Chala -> Nothing
    ChaldeanNeoAramaic -> Nothing
    Chalikha -> Nothing
    Chamacoco -> Nothing
    Chamalal -> Nothing
    Chamari -> Nothing
    Chambeali -> Nothing
    Chambri -> Nothing
    Chamicuro -> Nothing
    Chamorro -> Just $ ISO_639_2 "cha"
    ChamulaTzotzil -> Nothing
    ChanSantaCruzMaya -> Nothing
    Chane -> Nothing
    ChangNaga -> Nothing
    Changriwa -> Nothing
    Changthang -> Nothing
    Chantyal -> Nothing
    Chara -> Nothing
    Charu -> Nothing
    Chaudangsi -> Nothing
    Chaungtha -> Nothing
    Chaura -> Nothing
    Chavacano -> Nothing
    Chayahuita -> Nothing
    ChayucoMixtec -> Nothing
    ChazumbaMixtec -> Nothing
    Che -> Nothing
    Chechen -> Just $ ISO_639_2 "che"
    ChekeHolo -> Nothing
    Chemakum -> Nothing
    ChenalhoTzotzil -> Nothing
    Chenapian -> Nothing
    Chenchu -> Nothing
    Chenoua -> Nothing
    Chepang -> Nothing
    Chepya -> Nothing
    Cherepon -> Nothing
    Cherokee -> Just $ ISO_639_2 "chr"
    Chesu -> Nothing
    Chetco -> Nothing
    Chewong -> Nothing
    Cheyenne -> Just $ ISO_639_2 "chy"
    Chhattisgarhi -> Nothing
    Chhintange -> Nothing
    Chhulung -> Nothing
    ChiangmaiSignLanguage -> Nothing
    Chiapanec -> Nothing
    Chibcha -> Just $ ISO_639_2 "chb"
    ChicahuaxtlaTriqui -> Nothing
    Chichewa -> Just $ ISO_639_2 "nya"
    ChichicapanZapotec -> Nothing
    ChichimecaJonaz -> Nothing
    Chickasaw -> Nothing
    Chicomuceltec -> Nothing
    Chiga -> Nothing
    ChigmecatitlanMixtec -> Nothing
    Chilcotin -> Nothing
    ChileanQuechua -> Nothing
    ChileanSignLanguage -> Nothing
    Chilisso -> Nothing
    ChiltepecChinantec -> Nothing
    Chimakum -> Nothing
    ChimalapaZoque -> Nothing
    Chimariko -> Nothing
    ChimborazoHighlandQuichua -> Nothing
    Chimila -> Nothing
    ChinaBuriat -> Just $ ISO_639_2 "bua"
    Chinali -> Nothing
    ChinbonChin -> Nothing
    ChinchaQuechua -> Nothing
    Chinese -> Just $ ISO_639_2 "chi"
    ChinesePidginEnglish -> Nothing
    ChineseSignLanguage -> Nothing
    Chinook -> Nothing
    ChinookJargon -> Just $ ISO_639_2 "chn"
    Chipaya -> Nothing
    Chipewyan -> Just $ ISO_639_2 "chp"
    Chipiajes -> Nothing
    Chippewa -> Nothing
    ChiquianAncashQuechua -> Nothing
    ChiquihuitlanMazatec -> Nothing
    Chiquitano -> Nothing
    Chiripa -> Nothing
    Chiru -> Nothing
    Chitimacha -> Nothing
    ChitkuliKinnauri -> Nothing
    Chittagonian -> Nothing
    ChitwaniaTharu -> Nothing
    ChoapanZapotec -> Nothing
    Chocangacakha -> Nothing
    Chochotec -> Nothing
    Choctaw -> Just $ ISO_639_2 "cho"
    Chodri -> Nothing
    ChokriNaga -> Nothing
    Chokwe -> Nothing
    Cholon -> Nothing
    Chong -> Nothing
    ChonganjiangHmong -> Nothing
    Choni -> Nothing
    Chonyi -> Nothing
    Chopi -> Nothing
    Chorasmian -> Nothing
    Chorotega -> Nothing
    Chorti -> Nothing
    ChotheNaga -> Nothing
    Chrau -> Nothing
    ChuanqiandianClusterMiao -> Just $ ISO_639_2 "hmn"
    Chuave -> Nothing
    Chug -> Nothing
    Chuj_SanSebastianCoatan -> Nothing
    Chuka -> Nothing
    Chukot -> Nothing
    Chukwa -> Nothing
    Chulym -> Nothing
    Chumash -> Nothing
    Chumburung -> Nothing
    Chung -> Nothing
    Churahi -> Nothing
    Chut -> Nothing
    Chuukese -> Just $ ISO_639_2 "chk"
    Chuvantsy -> Nothing
    Chuvash -> Just $ ISO_639_2 "chv"
    Chuwabu -> Nothing
    CiGbe -> Just $ ISO_639_2 "fon"
    CiaCia -> Nothing
    Cibak -> Nothing
    Cimbrian -> Nothing
    CinamiguinManobo -> Nothing
    CindaRegiTiyal -> Nothing
    Cineni -> Nothing
    CintaLarga -> Nothing
    CisalpineGaulish -> Nothing
    Cishingini -> Nothing
    Citak -> Nothing
    Ciwogai -> Nothing
    ClassicalArmenian -> Nothing
    ClassicalMandaic -> Nothing
    ClassicalMongolian -> Nothing
    ClassicalNahuatl -> Nothing
    ClassicalNewari -> Just $ ISO_639_2 "nwc"
    ClassicalQuechua -> Nothing
    ClassicalSyriac -> Just $ ISO_639_2 "syc"
    ClassicalTibetan -> Nothing
    Clela -> Nothing
    Coahuilteco -> Nothing
    CoastMiwok -> Nothing
    CoastalKadazan -> Nothing
    CoastalKonjo -> Nothing
    CoastalSaluan -> Nothing
    CoatecasAltasZapotec -> Nothing
    CoatepecNahuatl -> Nothing
    CoatlanMixe -> Nothing
    CoatlanZapotec -> Nothing
    CoatzospanMixtec -> Nothing
    CocamaCocamilla -> Nothing
    Cochimi -> Nothing
    Cocopa -> Nothing
    CocosIslandsMalay -> Nothing
    CoeurDalene -> Nothing
    Cofan -> Nothing
    Cogui -> Nothing
    Colognian -> Nothing
    ColombianSignLanguage -> Nothing
    ColoniaTovarGerman -> Nothing
    Colorado -> Nothing
    ColumbiaWenatchi -> Nothing
    ComaltepecChinantec -> Nothing
    Comanche -> Nothing
    Comecrudo -> Nothing
    ComoKarim -> Nothing
    Comorian -> Nothing
    Comox -> Nothing
    Con -> Nothing
    CongoSwahili -> Nothing
    Coong -> Nothing
    Coos -> Nothing
    CopainalaZoque -> Nothing
    CopalaTriqui -> Nothing
    Coptic -> Just $ ISO_639_2 "cop"
    Coquille -> Nothing
    Cori -> Nothing
    Cornish -> Just $ ISO_639_2 "cor"
    CorongoAncashQuechua -> Nothing
    Corsican -> Just $ ISO_639_2 "cos"
    CostaRicanSignLanguage -> Nothing
    CotabatoManobo -> Nothing
    Cotoname -> Nothing
    Cowlitz -> Nothing
    Coxima -> Nothing
    Coyaima -> Nothing
    CoyotepecPopoloca -> Nothing
    CoyutlaTotonac -> Nothing
    Cree -> Just $ ISO_639_2 "cre"
    Cree_Moose -> Nothing
    Cree_Plains -> Nothing
    Cree_Swampy -> Nothing
    Cree_Woods -> Nothing
    Creek -> Just $ ISO_639_2 "mus"
    CrimeanTatar -> Just $ ISO_639_2 "crh"
    CroatiaSignLanguage -> Nothing
    Croatian -> Just $ ISO_639_2 "hrv"
    CrossRiverMbembe -> Nothing
    Crow -> Nothing
    Cruzeno -> Nothing
    Cua -> Nothing
    CubaSignLanguage -> Nothing
    Cubeo -> Nothing
    CubulcoAchi -> Nothing
    Cuiba -> Nothing
    Cuitlatec -> Nothing
    Culina -> Nothing
    Cumanagoto -> Nothing
    Cumbric -> Nothing
    Cumeral -> Nothing
    Cun -> Nothing
    CuneiformLuwian -> Nothing
    CunenQuiche -> Nothing
    Cung -> Nothing
    Cupeno -> Nothing
    Curonian -> Nothing
    Curripaco -> Nothing
    CuscoQuechua -> Nothing
    CutchiSwahili -> Nothing
    Cuvok -> Nothing
    CuyamecalcoMixtec -> Nothing
    Cuyonon -> Nothing
    CwiBwamu -> Nothing
    Czech -> Just $ ISO_639_2 "cze"
    CzechSignLanguage -> Nothing
    DaaKaili -> Nothing
    DaaiChin -> Nothing
    Daasanach -> Nothing
    Daatsiin -> Nothing
    Daba -> Nothing
    Dabarre -> Nothing
    Dabe -> Nothing
    Dacian -> Nothing
    DadiDadi -> Nothing
    Dadibi -> Nothing
    Dadiya -> Nothing
    Daga -> Nothing
    DagaariDioula -> Nothing
    Dagba -> Nothing
    Dagbani -> Nothing
    Dagik -> Nothing
    Dagoman -> Nothing
    Dahalik -> Nothing
    Dahalo -> Nothing
    DahoDoo -> Nothing
    Dai -> Nothing
    DaiZhuang -> Nothing
    Dair -> Nothing
    Dakaka -> Nothing
    Dakka -> Nothing
    Dakota -> Just $ ISO_639_2 "dak"
    Dakpakha -> Nothing
    Dalmatian -> Nothing
    DaloaBete -> Nothing
    Dama -> Nothing
    Damakawa -> Nothing
    Damal -> Nothing
    Dambi -> Nothing
    Dameli -> Nothing
    Dampelas -> Nothing
    Dan -> Nothing
    Danaru -> Nothing
    Danau -> Nothing
    DandamiMaria -> Nothing
    Dangaleat -> Nothing
    DangauraTharu -> Nothing
    Danish -> Just $ ISO_639_2 "dan"
    DanishSignLanguage -> Nothing
    Dano -> Nothing
    Danu -> Nothing
    Dao -> Nothing
    Daonda -> Nothing
    DarDajuDaju -> Nothing
    DarFurDaju -> Nothing
    DarSilaDaju -> Nothing
    Darai -> Nothing
    DarangDeng -> Nothing
    Dargwa -> Just $ ISO_639_2 "dar"
    Dari_Persian -> Nothing
    Darkhat -> Nothing
    Darkinyung -> Nothing
    Darling -> Nothing
    Darlong -> Nothing
    Darmiya -> Nothing
    DaroMatu -> Nothing
    Darwazi -> Just $ ISO_639_2 "per"
    Dass -> Nothing
    Datooga -> Nothing
    Daungwurrung -> Nothing
    Daur -> Nothing
    Davawenyo -> Nothing
    Daw -> Nothing
    Dawawa -> Nothing
    DaweraDaweloor -> Nothing
    DawikKui -> Nothing
    Dawro -> Nothing
    Dawsahak -> Nothing
    Day -> Nothing
    DayaoYi -> Nothing
    Dayi -> Nothing
    Daza -> Nothing
    Dazaga -> Nothing
    Deccan -> Nothing
    Dedua -> Nothing
    Defaka -> Nothing
    DefiGbe -> Just $ ISO_639_2 "fon"
    Deg -> Nothing
    Degaru -> Nothing
    Degema -> Nothing
    Degenan -> Nothing
    Degexitan -> Nothing
    Dehu -> Nothing
    Dehwari -> Nothing
    Dek -> Nothing
    DelaOenale -> Nothing
    Delaware -> Nothing
    Delo -> Nothing
    Dem -> Nothing
    Dema -> Nothing
    Demisa -> Nothing
    Demta -> Nothing
    Dendi_Benin -> Nothing
    Dendi_CentralAfricanRepublic -> Nothing
    Dengese -> Nothing
    Dengka -> Nothing
    Deni -> Nothing
    Deno -> Nothing
    Denya -> Nothing
    Deori -> Nothing
    Dera_Indonesia -> Nothing
    Dera_Nigeria -> Nothing
    Desano -> Nothing
    DesiyaOriya -> Nothing
    DewasRai -> Nothing
    Dewoin -> Nothing
    Dezfuli -> Nothing
    Dghwede -> Nothing
    Dhaiso -> Just $ ISO_639_2 "kam"
    Dhalandji -> Nothing
    Dhangu -> Nothing
    Dhanki -> Nothing
    Dhanwar_Nepal -> Nothing
    Dhanwar_Spurious -> Nothing
    Dhao -> Nothing
    Dhargari -> Nothing
    Dharuk -> Nothing
    Dhatki -> Nothing
    Dhimal -> Nothing
    Dhodia -> Nothing
    Dhudhuroa -> Nothing
    Dhundari -> Nothing
    Dhungaloo -> Nothing
    Dhurga -> Nothing
    Dhuwal -> Nothing
    Dhuwaya -> Nothing
    Dia -> Nothing
    DibabawonManobo -> Nothing
    Dibiyaso -> Nothing
    Dibo -> Nothing
    Dibole -> Nothing
    DicamayAgta -> Nothing
    Didinga -> Nothing
    Dieri -> Nothing
    DigaroMishmi -> Nothing
    Digo -> Nothing
    Dii -> Nothing
    DijimBwilim -> Nothing
    Dilling -> Nothing
    Dima -> Nothing
    Dimasa -> Nothing
    Dimbong -> Nothing
    Dime -> Nothing
    Dimir -> Nothing
    Dimli_IndividualLanguage -> Just $ ISO_639_2 "zza"
    Ding -> Nothing
    Dinka -> Just $ ISO_639_2 "din"
    Diodio -> Nothing
    DirNyamzakMbarimi -> Nothing
    Dirari -> Nothing
    Dirasha -> Nothing
    Diri -> Nothing
    Diriku -> Nothing
    Dirim -> Nothing
    Disa -> Nothing
    Ditammari -> Nothing
    Ditidaht -> Nothing
    Diuwe -> Nothing
    DiuxiTilantongoMixtec -> Nothing
    Divehi -> Just $ ISO_639_2 "div"
    DixonReef -> Nothing
    Dizi -> Nothing
    Djabwurrung -> Nothing
    Djadjawurrung -> Nothing
    Djambarrpuyngu -> Nothing
    Djamindjung -> Nothing
    Djangun -> Nothing
    Djauan -> Nothing
    Djawi -> Nothing
    Djeebbana -> Nothing
    DjiminiSenoufo -> Nothing
    Djinang -> Nothing
    Djinba -> Nothing
    Djingili -> Nothing
    Djiwarli -> Nothing
    Djongkang -> Nothing
    Dobel -> Nothing
    Dobu -> Nothing
    Doe -> Nothing
    Doga -> Nothing
    Doghoro -> Nothing
    Dogose -> Nothing
    Dogoso -> Nothing
    Dogri_Generic -> Just $ ISO_639_2 "doi"
    Dogri_Specific -> Just $ ISO_639_2 "doi"
    Dogrib -> Just $ ISO_639_2 "dgr"
    DogulDomDogon -> Nothing
    Dohoi -> Nothing
    Doka -> Nothing
    DokoUyanga -> Nothing
    Dokshi -> Nothing
    Dolgan -> Nothing
    Dolpo -> Nothing
    Dom -> Nothing
    Domaaki -> Nothing
    Domari -> Nothing
    Dombe -> Nothing
    DominicanSignLanguage -> Nothing
    Dompo -> Nothing
    Domu -> Nothing
    Domung -> Nothing
    Dondo -> Nothing
    Dong -> Nothing
    Dongo -> Nothing
    Dongotono -> Nothing
    DongshanbaLaloYi -> Nothing
    Dongxiang -> Nothing
    DonnoSoDogon -> Nothing
    Doondo -> Nothing
    Dorio -> Nothing
    Doromu -> Nothing
    Dororo -> Nothing
    Dorze -> Nothing
    Doso -> Nothing
    Dotyali -> Nothing
    Doutai -> Nothing
    Doyayo -> Nothing
    Drents -> Nothing
    Drung -> Nothing
    Duala -> Just $ ISO_639_2 "dua"
    Duano -> Nothing
    Duau -> Nothing
    Dubli -> Nothing
    Dubu -> Nothing
    Duduela -> Nothing
    Dugun -> Nothing
    Duguri -> Nothing
    Duguza -> Nothing
    Dugwor -> Nothing
    Duhwa -> Nothing
    Duke -> Nothing
    Dulbu -> Nothing
    Duli -> Nothing
    Duma -> Nothing
    Dumbea -> Nothing
    Dumi -> Nothing
    Dumpas -> Nothing
    Dumpu -> Nothing
    Dumun -> Nothing
    Duna -> Nothing
    Dungan -> Nothing
    Dungmali -> Nothing
    DungraBhil -> Nothing
    Dungu -> Nothing
    DupaninanAgta -> Nothing
    Dura -> Nothing
    DurangoNahuatl -> Nothing
    Duri -> Nothing
    Duriankere -> Nothing
    Duruma -> Nothing
    Duruwa -> Nothing
    Dusner -> Nothing
    DusunDeyah -> Nothing
    DusunMalang -> Nothing
    DusunWitu -> Nothing
    Dutch -> Just $ ISO_639_2 "dut"
    DutchSignLanguage -> Nothing
    DuttonWorldSpeedwords -> Nothing
    Duungooma -> Nothing
    Duupa -> Nothing
    Duvle -> Nothing
    Duwai -> Nothing
    Duwet -> Nothing
    Dwang -> Nothing
    Dyaabugay -> Nothing
    Dyaberdyaber -> Nothing
    Dyan -> Nothing
    Dyangadi -> Nothing
    Dyarim -> Nothing
    Dyirbal -> Nothing
    Dyugun -> Nothing
    Dyula -> Just $ ISO_639_2 "dyu"
    Dza -> Nothing
    Dzalakha -> Nothing
    Dzando -> Nothing
    DzaoMin -> Nothing
    Dzodinka -> Nothing
    Dzongkha -> Just $ ISO_639_2 "dzo"
    Dzuungoo -> Nothing
    E -> Nothing
    EarlyTripuri -> Nothing
    EastAmbae -> Nothing
    EastBerawan -> Nothing
    EastCree_Northern -> Nothing
    EastCree_Southern -> Nothing
    EastDamar -> Nothing
    EastFutuna -> Nothing
    EastKewa -> Nothing
    EastLimba -> Nothing
    EastMakian -> Nothing
    EastMasela -> Nothing
    EastNyala -> Nothing
    EastTarangan -> Nothing
    EastYugur -> Nothing
    EasternAcipa -> Nothing
    EasternApurimacQuechua -> Nothing
    EasternArrernte -> Nothing
    EasternBalochi -> Just $ ISO_639_2 "bal"
    EasternBolivianGuarani -> Nothing
    EasternBontok -> Nothing
    EasternBru -> Nothing
    EasternCakchiquel -> Nothing
    EasternCham -> Nothing
    EasternDurangoNahuatl -> Nothing
    EasternFrisian -> Just $ ISO_639_2 "frs"
    EasternGorkhaTamang -> Nothing
    EasternGurung -> Nothing
    EasternHighlandChatino -> Nothing
    EasternHighlandOtomi -> Nothing
    EasternHongshuiheZhuang -> Nothing
    EasternHuastecaNahuatl -> Nothing
    EasternHuishuiHmong -> Nothing
    EasternJacalteco -> Nothing
    EasternKanjobal -> Nothing
    EasternKaraboro -> Nothing
    EasternKarnic -> Nothing
    EasternKatu -> Nothing
    EasternKayah -> Nothing
    EasternKeres -> Nothing
    EasternKhumiChin -> Nothing
    EasternKrahn -> Nothing
    EasternLaluYi -> Nothing
    EasternLawa -> Nothing
    EasternMagar -> Nothing
    EasternManinkakan -> Nothing
    EasternMari -> Nothing
    EasternMeohang -> Nothing
    EasternMinyag -> Nothing
    EasternMnong -> Nothing
    EasternMuria -> Nothing
    EasternNgada -> Nothing
    EasternNisuYi -> Nothing
    EasternOjibwa -> Nothing
    EasternOromo -> Nothing
    EasternParbate -> Nothing
    EasternPenan -> Nothing
    EasternPokomam -> Nothing
    EasternPokomchi -> Nothing
    EasternPomo -> Nothing
    EasternQiandongHmong -> Nothing
    EasternQuiche -> Nothing
    EasternSubanen -> Nothing
    EasternTamang -> Nothing
    EasternTawbuid -> Nothing
    EasternTzutujil -> Nothing
    EasternXiangxiHmong -> Nothing
    EasternXwlaGbe -> Nothing
    EasternYiddish -> Nothing
    Ebira -> Nothing
    Eblan -> Nothing
    Ebrie -> Nothing
    Ebughu -> Nothing
    EcuadorianSignLanguage -> Nothing
    EdeCabe -> Nothing
    EdeIca -> Nothing
    EdeIdaca -> Nothing
    EdeIje -> Nothing
    EdeNago -> Nothing
    Edolo -> Nothing
    Edomite -> Nothing
    Edopi -> Nothing
    EdwasBonerifBeneraf -> Nothing
    Efai -> Nothing
    Efe -> Nothing
    Efik -> Just $ ISO_639_2 "efi"
    Efutop -> Nothing
    Ega -> Nothing
    Eggon -> Nothing
    EgyptSignLanguage -> Nothing
    Egyptian -> Just $ ISO_639_2 "egy"
    Ehueun -> Nothing
    Eipomek -> Nothing
    Eitiep -> Nothing
    Ejagham -> Nothing
    Ejamat -> Nothing
    EkaiChin -> Nothing
    Ekajuk -> Just $ ISO_639_2 "eka"
    Ekari -> Nothing
    Eki -> Nothing
    Ekit -> Nothing
    Ekpeye -> Nothing
    ElAltoZapotec -> Nothing
    ElHugeirat -> Nothing
    ElMolo -> Nothing
    ElNayarCora -> Nothing
    Elamite -> Just $ ISO_639_2 "elx"
    Eleme -> Nothing
    Elepi -> Nothing
    Elip -> Nothing
    Elkei -> Nothing
    ElotepecZapotec -> Nothing
    Eloyi -> Nothing
    Elpaputih -> Nothing
    Elseng -> Nothing
    Elu -> Nothing
    Elymian -> Nothing
    EmaBuyang -> Nothing
    Emae -> Nothing
    EmaiIulehaOra -> Nothing
    Eman -> Nothing
    Embaloh -> Nothing
    EmberaBaudo -> Nothing
    EmberaCatio -> Nothing
    EmberaChami -> Nothing
    EmberaTado -> Nothing
    Embu -> Nothing
    Emerillon -> Nothing
    Emilian -> Nothing
    EmilianoRomagnolo -> Nothing
    Emok -> Nothing
    Emplawas -> Nothing
    Emumu -> Nothing
    En -> Nothing
    EnaweneNawe -> Nothing
    Ende -> Nothing
    Endo -> Nothing
    Enepa -> Nothing
    Enga -> Nothing
    Engenni -> Nothing
    Enggano -> Nothing
    English -> Just $ ISO_639_2 "eng"
    Enim -> Nothing
    Enlhet -> Nothing
    Enrekang -> Nothing
    Enu -> Nothing
    Enwan_AkwaIbomState -> Nothing
    Enwan_EduState -> Nothing
    Enxet -> Nothing
    Enya -> Nothing
    Epena -> Nothing
    EpiOlmec -> Nothing
    Epie -> Nothing
    EpigraphicMayan -> Nothing
    Eravallan -> Nothing
    Erave -> Nothing
    Ere -> Nothing
    Eritai -> Nothing
    Erokwanas -> Nothing
    Erre -> Nothing
    Erromintxela -> Nothing
    Ersu -> Nothing
    Eruwa -> Nothing
    Erzya -> Just $ ISO_639_2 "myv"
    Esan -> Nothing
    Ese -> Nothing
    EseEjja -> Nothing
    EshanXinping_NorthernNisuYi -> Nothing
    Eshtehardi -> Nothing
    Esimbi -> Nothing
    Eskayan -> Nothing
    Esperanto -> Just $ ISO_639_2 "epo"
    Esselen -> Nothing
    EstadoDeMexicoOtomi -> Nothing
    Estonian -> Just $ ISO_639_2 "est"
    EstonianSignLanguage -> Nothing
    Esuma -> Nothing
    Etchemin -> Nothing
    Etebi -> Nothing
    Eten -> Nothing
    Eteocretan -> Nothing
    Eteocypriot -> Nothing
    EthiopianSignLanguage -> Nothing
    Etkywan -> Nothing
    Eton_Cameroon -> Nothing
    Eton_Vanuatu -> Nothing
    Etruscan -> Nothing
    Etulo -> Nothing
    Eudeve -> Nothing
    Europanto -> Nothing
    Evant -> Nothing
    Even -> Nothing
    Evenki -> Nothing
    Eviya -> Nothing
    EwageNotu -> Nothing
    Ewe -> Just $ ISO_639_2 "ewe"
    Ewondo -> Just $ ISO_639_2 "ewo"
    Extremaduran -> Nothing
    Eyak -> Nothing
    Ezaa -> Nothing
    FaDambu -> Nothing
    Fagani -> Nothing
    Faifi -> Nothing
    Faita -> Nothing
    Faiwol -> Nothing
    Fala -> Nothing
    FalamChin -> Nothing
    Fali -> Nothing
    FaliOfBaissa -> Nothing
    Faliscan -> Nothing
    Fam -> Nothing
    Fanagalo -> Nothing
    Fanamaket -> Nothing
    Fanbak -> Nothing
    Fang_Cameroon -> Nothing
    Fang_EquatorialGuinea -> Just $ ISO_639_2 "fan"
    Fania -> Nothing
    Fanti -> Just $ ISO_639_2 "fat"
    FarWesternMuria -> Nothing
    Farefare -> Nothing
    Faroese -> Just $ ISO_639_2 "fao"
    Fas -> Nothing
    Fasu -> Nothing
    Fataleka -> Nothing
    Fataluku -> Nothing
    Fayu -> Nothing
    Fefe -> Nothing
    Fembe -> Nothing
    FernandoPoCreoleEnglish -> Nothing
    Feroge -> Nothing
    FijiHindi -> Nothing
    Fijian -> Just $ ISO_639_2 "fij"
    Filipino -> Just $ ISO_639_2 "fil"
    FilomenaMataCoahuitlanTotonac -> Nothing
    Finallig -> Nothing
    Finnish -> Just $ ISO_639_2 "fin"
    FinnishSignLanguage -> Nothing
    FinnishSwedishSignLanguage -> Nothing
    Finnish_Kven -> Nothing
    Finnish_Tornedalen -> Nothing
    Finongan -> Nothing
    Fipa -> Nothing
    Firan -> Nothing
    Fiwaga -> Nothing
    FlindersIsland -> Nothing
    Foau -> Nothing
    Foi -> Nothing
    FoiaFoia -> Nothing
    Folopa -> Nothing
    Foma -> Nothing
    Fon -> Just $ ISO_639_2 "fon"
    Fongoro -> Nothing
    Foodo -> Nothing
    Forak -> Nothing
    Fordata -> Nothing
    Fore -> Nothing
    ForestEnets -> Nothing
    ForestManinka -> Nothing
    Fortsenal -> Nothing
    FranciscoLeonZoque -> Nothing
    FrancoProvencal -> Nothing
    Frankish -> Nothing
    French -> Just $ ISO_639_2 "fre"
    FrenchSignLanguage -> Nothing
    Friulian -> Just $ ISO_639_2 "fur"
    Fulah -> Just $ ISO_639_2 "ful"
    Fulfulde_Adamawa -> Nothing
    Fulfulde_Nigerian -> Nothing
    Fuliiru -> Nothing
    Fulnio -> Nothing
    Fum -> Nothing
    Fungwa -> Nothing
    Fur -> Nothing
    Furu -> Nothing
    FutunaAniwa -> Nothing
    Fuyug -> Nothing
    Fwai -> Nothing
    Fwe -> Nothing
    Fyam -> Nothing
    Fyer -> Nothing
    Ga -> Just $ ISO_639_2 "gaa"
    Gaa -> Nothing
    Gaam -> Nothing
    Gaanda -> Nothing
    GabiGabi -> Nothing
    Gabri -> Nothing
    GabrielinoFernandeno -> Nothing
    Gabutamon -> Nothing
    Gadang -> Nothing
    Gaddang -> Nothing
    Gaddi -> Nothing
    Gade -> Nothing
    GadeLohar -> Nothing
    Gadjerawang -> Nothing
    Gadsup -> Nothing
    Gaelic_Scots -> Just $ ISO_639_2 "gla"
    Gafat -> Nothing
    Gagadu -> Nothing
    Gagauz -> Nothing
    GagnoaBete -> Nothing
    Gagu -> Nothing
    Gahri -> Nothing
    Gaikundi -> Nothing
    Gail -> Nothing
    Gaina -> Nothing
    Gal -> Nothing
    Galambu -> Nothing
    Galatian -> Nothing
    Galela -> Nothing
    Galeya -> Nothing
    Galice -> Nothing
    Galician -> Just $ ISO_639_2 "glg"
    Galindan -> Nothing
    GallureseSardinian -> Nothing
    Galoli -> Nothing
    GamaleKham -> Nothing
    Gambera -> Nothing
    GambianWolof -> Just $ ISO_639_2 "wol"
    Gamilaraay -> Nothing
    Gamit -> Nothing
    Gamkonora -> Nothing
    Gamo -> Nothing
    GamoGofaDawro -> Nothing
    GamoNingi -> Nothing
    Gan -> Nothing
    Gana -> Nothing
    Ganang -> Nothing
    Gandhari -> Nothing
    Gane -> Nothing
    Ganggalida -> Nothing
    Ganglau -> Nothing
    Gangte -> Nothing
    Gangulu -> Nothing
    Gants -> Nothing
    Ganza -> Nothing
    Ganzi -> Nothing
    Gao -> Nothing
    Gapapaiwa -> Nothing
    Garawa -> Nothing
    Garhwali -> Nothing
    Garifuna -> Nothing
    GarigIlgar -> Nothing
    Garingbal -> Nothing
    Garlali -> Nothing
    Garo -> Nothing
    Garre -> Nothing
    GarrehAjuran -> Nothing
    Garrwa -> Nothing
    Garus -> Nothing
    Garza -> Nothing
    Gascon -> Nothing
    Gata -> Nothing
    Gavar -> Nothing
    GaviaoDoJiparana -> Nothing
    GawarBati -> Nothing
    Gawwada -> Nothing
    Gayil -> Nothing
    Gayo -> Just $ ISO_639_2 "gay"
    Gazi -> Nothing
    Gbagyi -> Nothing
    Gbanu -> Nothing
    Gbanziri -> Nothing
    Gbari -> Nothing
    GbatiRi -> Nothing
    GbayaBossangoa -> Nothing
    GbayaBozoum -> Nothing
    GbayaMbodomo -> Nothing
    Gbaya_CentralAfricanRepublic -> Nothing
    Gbaya_Sudan -> Nothing
    Gbayi -> Nothing
    GbesiGbe -> Nothing
    Gbii -> Nothing
    Gbin -> Nothing
    GbiriNiragu -> Nothing
    GbolooGrebo -> Just $ ISO_639_2 "grb"
    GebaKaren -> Nothing
    Gebe -> Nothing
    Gedaged -> Nothing
    Gedeo -> Nothing
    Geez -> Just $ ISO_639_2 "gez"
    Geji -> Nothing
    GekoKaren -> Nothing
    Gela -> Nothing
    Gelao -> Nothing
    GemanDeng -> Nothing
    Geme -> Nothing
    Gen -> Nothing
    Gende -> Nothing
    Gengle -> Nothing
    Georgian -> Just $ ISO_639_2 "geo"
    Gepo -> Nothing
    Gera -> Nothing
    Gerai -> Nothing
    German -> Just $ ISO_639_2 "ger"
    GermanSignLanguage -> Nothing
    Geruma -> Nothing
    GeserGorom -> Nothing
    Gey -> Nothing
    Ghadames -> Nothing
    GhanaianPidginEnglish -> Nothing
    GhanaianSignLanguage -> Nothing
    GhandrukSignLanguage -> Nothing
    Ghanongga -> Nothing
    Ghari -> Nothing
    Ghayavi -> Nothing
    Ghera -> Nothing
    Ghodoberi -> Nothing
    Ghomala -> Nothing
    Ghomara -> Nothing
    Ghotuo -> Nothing
    Ghulfan -> Nothing
    Giangan -> Nothing
    Gibanawa -> Just $ ISO_639_2 "hau"
    Gidar -> Nothing
    Giiwo -> Nothing
    Gikyode -> Nothing
    Gilaki -> Nothing
    Gilbertese -> Just $ ISO_639_2 "gil"
    Gilima -> Nothing
    Gilyak -> Nothing
    Gimi_EasternHighlands -> Nothing
    Gimi_WestNewBritain -> Nothing
    Gimme -> Nothing
    Gimnime -> Nothing
    Ginuman -> Nothing
    Ginyanga -> Nothing
    Girawa -> Nothing
    Girirra -> Nothing
    Giryama -> Nothing
    Githabul -> Nothing
    Gitonga -> Nothing
    Gitua -> Nothing
    Gitxsan -> Nothing
    Giyug -> Nothing
    Gizrra -> Nothing
    GlaroTwabo -> Nothing
    Glavda -> Nothing
    GlioOubi -> Nothing
    Gnau -> Nothing
    GoanKonkani -> Just $ ISO_639_2 "kok"
    Goaria -> Nothing
    Gobasi -> Nothing
    Gobu -> Nothing
    Godie -> Nothing
    Godwari -> Nothing
    Goemai -> Nothing
    Gofa -> Nothing
    Gogo -> Nothing
    Gogodala -> Nothing
    Gokana -> Nothing
    Gola -> Nothing
    Golin -> Nothing
    Golpa -> Nothing
    Gondi -> Just $ ISO_639_2 "gon"
    GoneDau -> Nothing
    Gongduk -> Nothing
    Gonja -> Nothing
    Goo -> Nothing
    Gooniyandi -> Nothing
    Gor -> Nothing
    Gorakor -> Nothing
    Gorap -> Nothing
    Goreng -> Nothing
    Gorontalo -> Just $ ISO_639_2 "gor"
    Gorovu -> Nothing
    Gorowa -> Nothing
    Gothic -> Just $ ISO_639_2 "got"
    Goundo -> Nothing
    Gourmanchema -> Nothing
    Gowlan -> Nothing
    Gowli -> Nothing
    Gowro -> Nothing
    Gozarkhani -> Nothing
    Grangali -> Nothing
    GrassKoiari -> Nothing
    Grebo -> Just $ ISO_639_2 "grb"
    GreekSignLanguage -> Nothing
    Greek_Cappadocian -> Nothing
    GreenGelao -> Nothing
    GrenadianCreoleEnglish -> Nothing
    Gresi -> Nothing
    Groma -> Nothing
    Gronings -> Nothing
    GrosVentre -> Nothing
    Gua -> Nothing
    GuadeloupeanCreoleFrench -> Nothing
    Guahibo -> Nothing
    Guaja -> Nothing
    Guajajara -> Nothing
    Guambiano -> Nothing
    Guana_Brazil -> Just $ ISO_639_2 "ter"
    Guana_Paraguay -> Nothing
    Guanano -> Nothing
    Guanche -> Nothing
    Guanyinqiao -> Nothing
    Guarani -> Nothing
    Guarayu -> Nothing
    Guarequena -> Nothing
    GuatemalanSignLanguage -> Nothing
    Guato -> Nothing
    Guayabero -> Nothing
    Gudang -> Nothing
    Gudanji -> Nothing
    Gude -> Nothing
    Gudu -> Nothing
    GudufGava -> Nothing
    GuebieGabogbo -> Nothing
    GuerreroAmuzgo -> Nothing
    GuerreroNahuatl -> Nothing
    GueveaDeHumboldtZapotec -> Nothing
    Gugadj -> Nothing
    GuguBadhun -> Nothing
    GuguMini -> Nothing
    GuguWarra -> Nothing
    Gugubera -> Nothing
    Guguyimidjir -> Nothing
    GuhuSamane -> Nothing
    GuianeseCreoleFrench -> Nothing
    GuibeiZhuang -> Just $ ISO_639_2 "zha"
    GuiberouaBete -> Nothing
    GuibianZhuang -> Nothing
    GuilaZapotec -> Nothing
    GuineaBissauSignLanguage -> Nothing
    GuineaKpelle -> Just $ ISO_639_2 "kpe"
    GuineanSignLanguage -> Nothing
    Guiqiong -> Nothing
    Gujarati -> Just $ ISO_639_2 "guj"
    Gujari -> Nothing
    GulaIro -> Nothing
    Gula_CentralAfricanRepublic -> Nothing
    Gula_Chad -> Nothing
    Gulaalaa -> Nothing
    Gulay -> Nothing
    Gule -> Nothing
    GulfSpokenArabic -> Nothing
    Guliguli -> Nothing
    Gumalu -> Nothing
    Gumatj -> Nothing
    Gumawana -> Nothing
    Gumuz -> Nothing
    Gun -> Nothing
    Gundi -> Nothing
    Gunditjmara -> Nothing
    Gundungurra -> Nothing
    Gungabula -> Nothing
    Gungu -> Nothing
    Guntai -> Nothing
    Gunwinggu -> Nothing
    Gunya -> Nothing
    GupaAbawa -> Nothing
    Gupapuyngu -> Nothing
    Guragone -> Nothing
    Guramalum -> Nothing
    Gurani -> Nothing
    Gurdjar -> Nothing
    GurengGureng -> Nothing
    Gurgula -> Nothing
    Guriaso -> Nothing
    GurindjiKriol -> Nothing
    Gurinji -> Nothing
    Gurmana -> Nothing
    Guro -> Nothing
    GuruntumMbaaru -> Nothing
    Gusan -> Nothing
    Gusii -> Nothing
    Gusilay -> Nothing
    Guwa -> Nothing
    Guwamu -> Nothing
    Guwinmal -> Nothing
    Guya -> Nothing
    GuyaneseCreoleEnglish -> Nothing
    Guyani -> Nothing
    Gvoko -> Nothing
    Gwa -> Nothing
    Gwahatike -> Nothing
    Gwak -> Nothing
    GwamhiWuri -> Nothing
    Gwandara -> Nothing
    Gweda -> Nothing
    Gweno -> Nothing
    Gwere -> Nothing
    Gwi -> Nothing
    Gwichin -> Just $ ISO_639_2 "gwi"
    Gyalsumdo -> Nothing
    Gyele -> Nothing
    Gyem -> Nothing
    Ha -> Nothing
    Habu -> Nothing
    Hadiyya -> Nothing
    Hadrami -> Nothing
    Hadza -> Nothing
    Haeke -> Nothing
    Hahon -> Nothing
    Haida -> Just $ ISO_639_2 "hai"
    Haigwai -> Nothing
    HainyaxoBozo -> Nothing
    Haiom -> Nothing
    HaiphongSignLanguage -> Nothing
    Haisla -> Nothing
    Haitian -> Just $ ISO_639_2 "hat"
    HaitianVodounCultureLanguage -> Nothing
    Haji -> Nothing
    Hajong -> Nothing
    HakkaChinese -> Nothing
    Hako -> Nothing
    Halang -> Nothing
    HalangDoan -> Nothing
    Halbi -> Nothing
    HalhMongolian -> Nothing
    Halia -> Nothing
    Halkomelem -> Nothing
    Hamap -> Nothing
    Hamba -> Nothing
    HamerBanna -> Nothing
    Hamtai -> Nothing
    Han -> Nothing
    Hanga -> Nothing
    HangaHundi -> Nothing
    Hangaza -> Nothing
    Hani -> Nothing
    Hano -> Nothing
    HanoiSignLanguage -> Nothing
    Hanunoo -> Nothing
    Harami -> Nothing
    Harari -> Nothing
    Harauti -> Nothing
    HarijanKinnauri -> Nothing
    Haroi -> Nothing
    Harsusi -> Nothing
    Haruai -> Nothing
    Haruku -> Nothing
    Haryanvi -> Nothing
    Harzani -> Nothing
    Hasha -> Nothing
    Hassaniyya -> Nothing
    Hatam -> Nothing
    Hattic -> Nothing
    Hausa -> Just $ ISO_639_2 "hau"
    HausaSignLanguage -> Nothing
    HavasupaiWalapaiYavapai -> Nothing
    Haveke -> Nothing
    Havu -> Nothing
    HawaiiCreoleEnglish -> Nothing
    HawaiiPidginSignLanguage -> Nothing
    Hawaiian -> Just $ ISO_639_2 "haw"
    Haya -> Nothing
    Hazaragi -> Nothing
    Hdi -> Nothing
    Hebrew -> Just $ ISO_639_2 "heb"
    Hehe -> Nothing
    Heiban -> Nothing
    Heiltsuk -> Nothing
    HelambuSherpa -> Nothing
    Helong -> Nothing
    Hema -> Nothing
    Hemba -> Nothing
    Herde -> Nothing
    Herero -> Just $ ISO_639_2 "her"
    Hermit -> Nothing
    Hernican -> Nothing
    Hertevin -> Nothing
    Hewa -> Nothing
    Heyo -> Nothing
    HibernoScottishGaelic -> Nothing
    Hibito -> Nothing
    Hidatsa -> Nothing
    HieroglyphicLuwian -> Nothing
    Higaonon -> Nothing
    HighlandKonjo -> Nothing
    HighlandOaxacaChontal -> Nothing
    HighlandPopoluca -> Nothing
    HighlandTotonac -> Nothing
    Hijuk -> Nothing
    Hiligaynon -> Just $ ISO_639_2 "hil"
    Himarima -> Nothing
    Hindi -> Just $ ISO_639_2 "hin"
    Hinduri -> Nothing
    Hinukh -> Nothing
    HiriMotu -> Just $ ISO_639_2 "hmo"
    Hittite -> Just $ ISO_639_2 "hit"
    Hitu -> Nothing
    Hiw -> Nothing
    Hixkaryana -> Nothing
    Hlai -> Nothing
    HlephoPhowa -> Nothing
    Hlersu -> Nothing
    Hmar -> Nothing
    Hmong -> Nothing
    HmongDaw -> Just $ ISO_639_2 "hmn"
    HmongDo -> Just $ ISO_639_2 "hmn"
    HmongDon -> Just $ ISO_639_2 "hmn"
    HmongNjua -> Just $ ISO_639_2 "hmn"
    HmongShua -> Just $ ISO_639_2 "hmn"
    Hmwaveke -> Nothing
    Ho -> Nothing
    HoChiMinhCitySignLanguage -> Nothing
    HoChunk -> Nothing
    Hoava -> Nothing
    Hobyot -> Nothing
    HoiaHoia -> Nothing
    Holikachuk -> Nothing
    Holiya -> Nothing
    Holma -> Nothing
    Holoholo -> Nothing
    Holu -> Nothing
    Homa -> Nothing
    HondurasSignLanguage -> Nothing
    Hone -> Nothing
    HongKongSignLanguage -> Nothing
    Honi -> Nothing
    Hopi -> Nothing
    HornedMiao -> Just $ ISO_639_2 "hmn"
    Horo -> Nothing
    Horom -> Nothing
    Horpa -> Nothing
    Horuru -> Nothing
    Hote -> Nothing
    Hoti -> Nothing
    Hovongan -> Nothing
    Hoyahoya -> Nothing
    Hozo -> Nothing
    Hpon -> Nothing
    Hrangkhol -> Nothing
    Hre -> Nothing
    Hruso -> Nothing
    Hu -> Nothing
    Hua -> Nothing
    Huachipaeri -> Nothing
    HuallagaHuanucoQuechua -> Nothing
    HuamaliesDosDeMayoHuanucoQuechua -> Nothing
    Huambisa -> Nothing
    Huarijio -> Nothing
    Huaulu -> Nothing
    HuautlaMazatec -> Nothing
    HuaxcalecaNahuatl -> Nothing
    HuaylasAncashQuechua -> Nothing
    HuayllaWancaQuechua -> Nothing
    Huba -> Nothing
    HuehuetlaTepehua -> Nothing
    Huichol -> Nothing
    Huilliche -> Nothing
    HuitepecMixtec -> Nothing
    HuixtanTzotzil -> Nothing
    HuizhouChinese -> Nothing
    Hukumina -> Nothing
    Hula -> Nothing
    Hulaula -> Nothing
    Huli -> Nothing
    Hulung -> Nothing
    HumburiSenniSonghay -> Nothing
    Humene -> Nothing
    Humla -> Nothing
    HunSaare -> Nothing
    Hunde -> Nothing
    Hung -> Nothing
    Hungana -> Nothing
    Hungarian -> Just $ ISO_639_2 "hun"
    HungarianSignLanguage -> Nothing
    Hungu -> Nothing
    Hungworo -> Nothing
    HunjaraKainaKe -> Nothing
    Hunnic -> Nothing
    Hunsrik -> Nothing
    Hunzib -> Nothing
    Hupa -> Just $ ISO_639_2 "hup"
    Hupde -> Nothing
    Hupla -> Nothing
    Hurrian -> Nothing
    HutteriteGerman -> Nothing
    Hwana -> Nothing
    Hya -> Nothing
    Hyam -> Nothing
    IWak -> Nothing
    Iaai -> Nothing
    Iamalele -> Nothing
    Iapama -> Nothing
    Iatmul -> Nothing
    Iau -> Nothing
    IbaliTeke -> Nothing
    Ibaloi -> Nothing
    Iban -> Just $ ISO_639_2 "iba"
    Ibanag -> Nothing
    Ibani -> Nothing
    Ibatan -> Nothing
    Iberian -> Nothing
    Ibibio -> Nothing
    Ibilo -> Nothing
    Ibino -> Nothing
    Ibu -> Nothing
    Ibuoro -> Nothing
    Icelandic -> Just $ ISO_639_2 "ice"
    IcelandicSignLanguage -> Nothing
    IceveMaci -> Nothing
    Idaan -> Nothing
    IdakhoIsukhaTiriki -> Nothing
    Idate -> Nothing
    Idere -> Nothing
    Idesa -> Nothing
    Idi -> Nothing
    Ido -> Just $ ISO_639_2 "ido"
    Idoma -> Nothing
    Idon -> Nothing
    IduMishmi -> Nothing
    Idun -> Nothing
    Iduna -> Nothing
    Ife -> Nothing
    Ifo -> Nothing
    Igala -> Nothing
    Igana -> Nothing
    Igbo -> Just $ ISO_639_2 "ibo"
    Igede -> Nothing
    Ignaciano -> Nothing
    Igo -> Nothing
    Iguta -> Nothing
    Igwe -> Nothing
    Iha -> Nothing
    IhaBasedPidgin -> Nothing
    Ihievbe -> Nothing
    IjaZuba -> Nothing
    Ik -> Nothing
    Ika -> Nothing
    Ikaranggal -> Nothing
    IkhinArokho -> Nothing
    Ikizu -> Nothing
    Iko -> Nothing
    IkobiMena -> Nothing
    Ikoma -> Nothing
    Ikpeng -> Nothing
    Ikpeshi -> Nothing
    Ikposo -> Nothing
    IkuGoraAnkwa -> Nothing
    Ikulu -> Nothing
    Ikwere -> Nothing
    Ikwo -> Nothing
    Ila -> Nothing
    IleApe -> Nothing
    IliTurki -> Nothing
    IlianenManobo -> Nothing
    Iliuun -> Nothing
    Illyrian -> Nothing
    Iloko -> Just $ ISO_639_2 "ilo"
    Ilongot -> Nothing
    Ilue -> Nothing
    ImbaburaHighlandQuichua -> Nothing
    Imbongu -> Nothing
    Imeraguen -> Nothing
    Imonda -> Nothing
    Imotong -> Nothing
    Imroing -> Nothing
    Inabaknon -> Nothing
    Inapang -> Nothing
    Inapari -> Nothing
    InariSami -> Just $ ISO_639_2 "smn"
    IndianSignLanguage -> Nothing
    IndoPortuguese -> Nothing
    Indonesian -> Just $ ISO_639_2 "ind"
    IndonesianBajau -> Nothing
    IndonesianSignLanguage -> Nothing
    Indri -> Nothing
    IndusKohistani -> Nothing
    IndusValleyLanguage -> Nothing
    InebuOne -> Nothing
    Ineseno -> Nothing
    Inga -> Nothing
    Ingrian -> Nothing
    Ingush -> Just $ ISO_639_2 "inh"
    InlaodItneg -> Nothing
    InokeYate -> Nothing
    Inonhan -> Nothing
    Inor -> Nothing
    InpuiNaga -> Nothing
    Interglossa -> Nothing
    Interlingua -> Just $ ISO_639_2 "ina"
    Interlingue -> Just $ ISO_639_2 "ile"
    InternationalSign -> Nothing
    Interslavic -> Nothing
    Intha -> Nothing
    InuitSignLanguage -> Nothing
    Inuktitut -> Just $ ISO_639_2 "iku"
    Inuktitut_EasternCanadian -> Just $ ISO_639_2 "iku"
    Inuktitut_WesternCanadian -> Just $ ISO_639_2 "iku"
    Inupiaq -> Just $ ISO_639_2 "ipk"
    IowaOto -> Nothing
    IpekaTapuia -> Nothing
    Ipiko -> Nothing
    Ipili -> Nothing
    Ipulo -> Nothing
    Iquito -> Nothing
    Ir -> Nothing
    Irantxe -> Nothing
    Iranun -> Nothing
    Iranun_Malaysia -> Nothing
    Iranun_Philippines -> Nothing
    Iraqw -> Nothing
    Irarutu -> Nothing
    Iraya -> Nothing
    Iresim -> Nothing
    IrigaBicolano -> Nothing
    Irigwe -> Nothing
    Irish -> Just $ ISO_639_2 "gle"
    IrishSignLanguage -> Nothing
    Irula -> Nothing
    Isabi -> Nothing
    Isanzu -> Nothing
    IsarogAgta -> Nothing
    Isconahua -> Nothing
    Isebe -> Nothing
    Isekiri -> Nothing
    Ishkashimi -> Nothing
    Isinai -> Nothing
    Isirawa -> Nothing
    IslandCarib -> Nothing
    IslanderCreoleEnglish -> Nothing
    Isnag -> Nothing
    Isoko -> Nothing
    IsraeliSignLanguage -> Nothing
    IsthmusCosoleacaqueNahuatl -> Nothing
    IsthmusMecayapanNahuatl -> Nothing
    IsthmusMixe -> Nothing
    IsthmusPajapanNahuatl -> Nothing
    IsthmusZapotec -> Nothing
    Istriot -> Nothing
    IstroRomanian -> Nothing
    Isu_FakoDivision -> Nothing
    Isu_MenchumDivision -> Nothing
    Italian -> Just $ ISO_639_2 "ita"
    ItalianSignLanguage -> Nothing
    Itawit -> Nothing
    Itelmen -> Nothing
    Itene -> Nothing
    Iteri -> Nothing
    Itik -> Nothing
    Ito -> Nothing
    Itonama -> Nothing
    ItuMbonUzo -> Nothing
    ItundujiaMixtec -> Nothing
    Itutang -> Nothing
    Itza -> Nothing
    IuMien -> Nothing
    Ivatan -> Nothing
    IvbieNorthOkpelaArhe -> Nothing
    Iwaidja -> Nothing
    Iwal -> Nothing
    Iwam -> Nothing
    Iwur -> Nothing
    Ixcatec -> Nothing
    IxcatlanMazatec -> Nothing
    IxtatanChuj -> Nothing
    IxtayutlaMixtec -> Nothing
    IxtencoOtomi -> Nothing
    Iyayu -> Nothing
    Iyive -> Nothing
    Iyo -> Nothing
    IyojwajaChorote -> Nothing
    IyowujwaChorote -> Nothing
    Izere -> Nothing
    IziEzaaIkwoMgbo -> Nothing
    Izii -> Nothing
    Izon -> Nothing
    Izora -> Nothing
    Jabuti -> Nothing
    Jad -> Nothing
    Jadgali -> Nothing
    Jagoi -> Nothing
    JahHut -> Nothing
    Jahanka -> Nothing
    Jaitmatang -> Nothing
    Jakati -> Nothing
    Jakun -> Nothing
    JalapaDeDiazMazatec -> Nothing
    Jalkunan -> Nothing
    JamaicanCountrySignLanguage -> Nothing
    JamaicanCreoleEnglish -> Nothing
    JamaicanSignLanguage -> Nothing
    Jamamadi -> Nothing
    JambiMalay -> Nothing
    JamiltepecMixtec -> Nothing
    JamsayDogon -> Nothing
    Jandai -> Nothing
    Jandavra -> Nothing
    Jangshung -> Nothing
    Janji -> Nothing
    Japanese -> Just $ ISO_639_2 "jpn"
    JapaneseSignLanguage -> Nothing
    Japreria -> Nothing
    Jaqaru -> Nothing
    Jara -> Nothing
    Jarai -> Nothing
    Jarawa_India -> Nothing
    Jarawa_Nigeria -> Nothing
    Jarnango -> Nothing
    Jaru -> Nothing
    Jaruara -> Nothing
    JaujaWancaQuechua -> Nothing
    Jaunsari -> Nothing
    Javanese -> Just $ ISO_639_2 "jav"
    Javindo -> Nothing
    Jawe -> Nothing
    Jaya -> Nothing
    Jebero -> Nothing
    Jeh -> Nothing
    Jehai -> Nothing
    Jejueo -> Nothing
    Jemez -> Nothing
    JenaamaBozo -> Nothing
    Jeng -> Nothing
    JennuKurumba -> Nothing
    Jere -> Nothing
    JeriKuo -> Nothing
    JerriaisGuernesiais -> Nothing
    Jerung -> Nothing
    JewishPalestinianAramaic -> Nothing
    JhankotSignLanguage -> Nothing
    Jiamao -> Nothing
    Jiarong -> Nothing
    Jiba -> Nothing
    Jibu -> Nothing
    Jiiddu -> Nothing
    Jilbe -> Nothing
    Jilim -> Nothing
    Jimi_Cameroon -> Nothing
    Jimi_Nigeria -> Nothing
    Jina -> Nothing
    Jingpho -> Just $ ISO_639_2 "kac"
    JinyuChinese -> Nothing
    JiongnaiBunu -> Nothing
    Jirel -> Nothing
    Jiru -> Nothing
    Jita -> Nothing
    Jju -> Nothing
    Joba -> Nothing
    JofotekBromnya -> Nothing
    Jogi -> Nothing
    JolaFonyi -> Nothing
    JolaKasa -> Nothing
    JonkorBourmataguil -> Nothing
    Jora -> Nothing
    JordanianSignLanguage -> Nothing
    Jorto -> Nothing
    Jowulu -> Nothing
    JoyabajQuiche -> Nothing
    Ju -> Nothing
    Juang -> Nothing
    JudeoArabic -> Just $ ISO_639_2 "jrb"
    JudeoBerber -> Nothing
    JudeoCrimeanTatar -> Nothing
    JudeoGeorgian -> Nothing
    JudeoIraqiArabic -> Nothing
    JudeoItalian -> Nothing
    JudeoPersian -> Just $ ISO_639_2 "jpr"
    JudeoTat -> Nothing
    JudeoTripolitanianArabic -> Nothing
    Juhoan -> Nothing
    JukunTakum -> Nothing
    Juma -> Nothing
    Jumjum -> Nothing
    JumlaSignLanguage -> Nothing
    Jumli -> Nothing
    JungleInga -> Nothing
    JuquilaMixe -> Nothing
    JurModo -> Nothing
    Juray -> Nothing
    Jurchen -> Nothing
    Juruna -> Nothing
    Jutish -> Nothing
    Juwal -> Nothing
    JuxtlahuacaMixtec -> Nothing
    JwiraPepesa -> Nothing
    Kaamba -> Nothing
    Kaan -> Nothing
    KaangChin -> Nothing
    Kaansa -> Nothing
    Kaba -> Nothing
    KabaDeme -> Nothing
    KabaNa -> Nothing
    Kabalai -> Nothing
    Kabardian -> Just $ ISO_639_2 "kbd"
    Kabatei -> Nothing
    Kabixi -> Nothing
    Kabiye -> Nothing
    Kabola -> Nothing
    KaboreOne -> Nothing
    Kabras -> Nothing
    Kaburi -> Nothing
    Kabutra -> Nothing
    Kabuverdianu -> Nothing
    Kabwa -> Nothing
    Kabwari -> Nothing
    Kabyle -> Just $ ISO_639_2 "kab"
    KachamaGanjule -> Nothing
    Kachari -> Nothing
    Kachchi -> Nothing
    KachiKoli -> Nothing
    KacipoBalesi -> Nothing
    Kaco -> Nothing
    Kadai -> Nothing
    Kadar -> Nothing
    Kadara -> Nothing
    Kadaru -> Nothing
    Kadiweu -> Nothing
    Kado -> Nothing
    Kadu -> Nothing
    Kadung -> Nothing
    Kaduo -> Nothing
    Kaera -> Nothing
    Kafa -> Nothing
    Kafoa -> Nothing
    KagFerJiirKoorRorUsZuksun -> Nothing
    KaganKalagan -> Nothing
    Kagate -> Nothing
    Kagayanen -> Nothing
    Kagoma -> Nothing
    Kagoro -> Nothing
    Kagulu -> Nothing
    Kahayan -> Nothing
    Kahe -> Nothing
    Kahua -> Nothing
    KahumamahonSaluan -> Nothing
    Kaian -> Nothing
    Kaibobo -> Nothing
    Kaidipang -> Nothing
    Kaiep -> Nothing
    Kaikadi -> Nothing
    KaikavianLiteraryLanguage_Kajkavian -> Nothing
    Kaike -> Nothing
    Kaiku -> Nothing
    Kaimbe -> Nothing
    Kaimbulawa -> Nothing
    Kaingang -> Nothing
    Kairak -> Nothing
    Kairiru -> Nothing
    KairuiMidiki -> Nothing
    Kais -> Nothing
    Kaitag -> Nothing
    Kaivi -> Nothing
    Kaiwa -> Nothing
    Kaiy -> Nothing
    Kajakse -> Nothing
    Kajali -> Nothing
    Kajaman -> Nothing
    Kakabai -> Nothing
    Kakabe -> Nothing
    Kakanda -> Nothing
    Kakauhua -> Nothing
    KakiAe -> Nothing
    Kakihum -> Nothing
    Kako -> Nothing
    Kakwa -> Nothing
    KalaLagawYa -> Nothing
    Kalaallisut -> Just $ ISO_639_2 "kal"
    Kalaamaya -> Nothing
    Kalabakan -> Nothing
    Kalabari -> Nothing
    Kalabra -> Nothing
    Kalagan -> Nothing
    KalaktangMonpa -> Nothing
    Kalam -> Nothing
    Kalami -> Nothing
    Kalamse -> Nothing
    Kalanadi -> Nothing
    Kalanga -> Nothing
    Kalao -> Nothing
    Kalapuya -> Nothing
    Kalarko -> Nothing
    Kalasha -> Nothing
    Kalenjin -> Nothing
    KalispelPendDoreille -> Nothing
    Kalkoti -> Nothing
    Kalkutungu -> Nothing
    Kalmyk -> Just $ ISO_639_2 "xal"
    KaloFinnishRomani -> Nothing
    Kalou -> Nothing
    Kaluli -> Nothing
    Kalumpang -> Nothing
    Kam -> Nothing
    Kamakan -> Nothing
    Kamang -> Nothing
    Kamano -> Nothing
    Kamantan -> Nothing
    Kamar -> Nothing
    Kamara -> Nothing
    Kamarian -> Nothing
    Kamaru -> Nothing
    Kamas -> Nothing
    Kamasa -> Nothing
    Kamasau -> Nothing
    Kamayo -> Nothing
    Kamayura -> Nothing
    Kamba_Brazil -> Nothing
    Kamba_Kenya -> Just $ ISO_639_2 "kam"
    Kambaata -> Nothing
    Kambaira -> Nothing
    Kambera -> Nothing
    Kamberau -> Nothing
    Kambiwa -> Nothing
    Kami_Nigeria -> Nothing
    Kami_Tanzania -> Nothing
    Kamo -> Nothing
    Kamoro -> Nothing
    Kamu -> Nothing
    Kamula -> Nothing
    Kamviri -> Nothing
    Kamwe -> Nothing
    Kanakanabu -> Nothing
    Kanamari -> Nothing
    Kanan -> Nothing
    Kanashi -> Nothing
    Kanasi -> Nothing
    Kanauji -> Nothing
    Kandas -> Nothing
    Kandawo -> Nothing
    Kande -> Nothing
    Kanembu -> Just $ ISO_639_2 "kbl"
    Kang -> Nothing
    Kanga -> Nothing
    Kangean -> Nothing
    Kanggape -> Nothing
    Kangjia -> Nothing
    Kango_BasUeleDistrict -> Nothing
    Kango_TshopoDistrict -> Nothing
    Kangri -> Nothing
    Kaniet -> Nothing
    Kanikkaran -> Nothing
    KaningdonNindem -> Nothing
    Kaningi -> Nothing
    Kaningra -> Nothing
    Kaninuwa -> Nothing
    Kanite -> Nothing
    Kanjari -> Nothing
    Kanju -> Nothing
    Kankanaey -> Nothing
    Kannada -> Just $ ISO_639_2 "kan"
    Kanoe -> Nothing
    Kanowit -> Nothing
    Kansa -> Nothing
    Kantosi -> Nothing
    Kanu -> Nothing
    Kanufi -> Nothing
    Kanuri -> Just $ ISO_639_2 "kau"
    Kanyok -> Nothing
    Kao -> Nothing
    Kaonde -> Nothing
    Kapin -> Nothing
    Kapinawa -> Nothing
    Kapingamarangi -> Nothing
    Kapori -> Nothing
    Kapriman -> Nothing
    Kaptiau -> Nothing
    Kapya -> Nothing
    Kara_CentralAfricanRepublic -> Nothing
    Kara_Korea -> Nothing
    Kara_PapuaNewGuinea -> Nothing
    Kara_Tanzania -> Nothing
    KarachayBalkar -> Just $ ISO_639_2 "krc"
    Karadjeri -> Nothing
    KaragaMandaya -> Nothing
    Karagas -> Nothing
    Karahawyana -> Nothing
    Karaim -> Nothing
    Karaja -> Nothing
    Karakalpak -> Just $ ISO_639_2 "kaa"
    Karakhanid -> Nothing
    Karami -> Nothing
    Karamojong -> Nothing
    Karang -> Nothing
    Karanga -> Nothing
    Karankawa -> Nothing
    Karao -> Nothing
    Karas -> Nothing
    Karata -> Nothing
    Karawa -> Nothing
    Karbi -> Nothing
    Kare_CentralAfricanRepublic -> Nothing
    Kare_PapuaNewGuinea -> Nothing
    Karekare -> Nothing
    Karelian -> Just $ ISO_639_2 "krl"
    Karenggapa -> Nothing
    Karey -> Nothing
    Kari -> Nothing
    Karingani -> Nothing
    Karipuna -> Nothing
    KaripunaCreoleFrench -> Nothing
    KaririXoco -> Nothing
    Karitiana -> Nothing
    Kariya -> Nothing
    Kariyarra -> Nothing
    KarkarYuri -> Nothing
    Karkin -> Nothing
    Karko -> Nothing
    Karnai -> Nothing
    Karo_Brazil -> Nothing
    Karo_Ethiopia -> Nothing
    Karok -> Nothing
    Karolanos -> Nothing
    Karon -> Nothing
    KaronDori -> Nothing
    Karore -> Nothing
    Karranga -> Nothing
    Karuwali -> Nothing
    Kasanga -> Nothing
    Kasem -> Nothing
    Kashaya -> Nothing
    Kashmiri -> Just $ ISO_639_2 "kas"
    Kashubian -> Just $ ISO_639_2 "csb"
    Kasiguranin -> Nothing
    Kaska -> Nothing
    Kaskean -> Nothing
    Kasseng -> Nothing
    Kasua -> Nothing
    Kataang -> Nothing
    Katabaga -> Nothing
    Katawixi -> Nothing
    Katbol -> Nothing
    KatchaKadugliMiri -> Nothing
    Kate -> Nothing
    KathoriyaTharu -> Nothing
    Kathu -> Nothing
    Kati -> Nothing
    Katingan -> Nothing
    Katkari -> Nothing
    Katla -> Nothing
    Kato -> Nothing
    Katso -> Nothing
    Katua -> Nothing
    Katukina -> Nothing
    Kaulong -> Nothing
    Kaur -> Nothing
    Kaure -> Nothing
    Kaurna -> Nothing
    Kauwera -> Nothing
    Kavalan -> Nothing
    Kawacha -> Nothing
    Kawaiisu -> Nothing
    Kawe -> Nothing
    Kawi -> Just $ ISO_639_2 "kaw"
    Kaxarari -> Nothing
    Kaxuiana -> Nothing
    Kayabi -> Nothing
    Kayagar -> Nothing
    Kayan -> Nothing
    KayanMahakam -> Nothing
    KayanRiverKayan -> Nothing
    KayanRiverKenyah -> Nothing
    KayapaKallahan -> Nothing
    Kayapo -> Nothing
    Kayardild -> Nothing
    Kayeli -> Nothing
    Kayong -> Nothing
    Kayort -> Nothing
    Kaytetye -> Nothing
    KayuAgung -> Nothing
    Kayupulau -> Nothing
    Kazakh -> Just $ ISO_639_2 "kaz"
    Kazukuru -> Nothing
    Keak -> Nothing
    Keapara -> Nothing
    KedahMalay -> Nothing
    Kedang -> Nothing
    Keder -> Nothing
    KeerrayWoorroong -> Nothing
    Kehu -> Nothing
    Kei -> Nothing
    Keiga -> Nothing
    Kein -> Nothing
    Keiyo -> Nothing
    Kekchi -> Nothing
    Kela_DemocraticRepublicOfCongo -> Nothing
    Kela_PapuaNewGuinea -> Nothing
    Kelabit -> Nothing
    Kele -> Nothing
    Kele_DemocraticRepublicOfCongo -> Nothing
    Kele_PapuaNewGuinea -> Nothing
    KeleyIKallahan -> Nothing
    Keliko -> Nothing
    KelinyauKenyah -> Nothing
    Kelo -> Nothing
    Kelon -> Nothing
    Kemak -> Nothing
    Kembayan -> Nothing
    Kemberano -> Nothing
    Kembra -> Nothing
    Kemezung -> Nothing
    KemiSami -> Nothing
    Kemiehua -> Nothing
    Kemtuik -> Nothing
    Kenaboi -> Nothing
    Kenati -> Nothing
    Kendayan -> Nothing
    Kendeje -> Nothing
    Kendem -> Nothing
    Kenga -> Nothing
    KeningauMurut -> Nothing
    Keninjal -> Nothing
    Kensiu -> Nothing
    KensweiNsei -> Nothing
    KenuziDongola -> Nothing
    KenyanSignLanguage -> Nothing
    Kenyang -> Nothing
    Kenyi -> Nothing
    Kenzi -> Nothing
    Keo -> Nothing
    Kepkiriwat -> Nothing
    Kepo -> Nothing
    Kera -> Nothing
    Kerak -> Nothing
    KerehoUheng -> Nothing
    Kerek -> Nothing
    Kerewe -> Nothing
    Kerewo -> Nothing
    Kerinci -> Nothing
    Kesawai -> Nothing
    Ket -> Nothing
    Ketangalan -> Nothing
    Kete -> Nothing
    Ketengban -> Nothing
    Ketum -> Nothing
    Keuru -> Nothing
    Keyagana -> Nothing
    Kgalagadi -> Nothing
    Khah -> Nothing
    Khakas -> Nothing
    Khalaj -> Nothing
    Khaling -> Nothing
    Khamba -> Nothing
    KhamniganMongol -> Nothing
    KhamsTibetan -> Nothing
    Khamti -> Nothing
    Khamyang -> Nothing
    Khana -> Nothing
    Khandesi -> Nothing
    Khang -> Nothing
    Khanty -> Nothing
    Khao -> Nothing
    KharamNaga -> Nothing
    Kharia -> Nothing
    KhariaThar -> Nothing
    Khasi -> Just $ ISO_639_2 "kha"
    Khayo -> Nothing
    Khazar -> Nothing
    Khe -> Nothing
    Khehek -> Nothing
    Khengkha -> Nothing
    Khetrani -> Nothing
    KhezhaNaga -> Nothing
    KhiamniunganNaga -> Nothing
    Khinalugh -> Nothing
    Khirwar -> Nothing
    Khisa -> Nothing
    Khlor -> Nothing
    Khlula -> Nothing
    Khmer -> Just $ ISO_639_2 "khm"
    Khmu -> Nothing
    KhoibuNaga -> Nothing
    Khoini -> Nothing
    Kholok -> Nothing
    KhorasaniTurkish -> Nothing
    KhorezmianTurkic -> Nothing
    Khotanese -> Just $ ISO_639_2 "kho"
    Khowar -> Nothing
    Khua -> Nothing
    Khuen -> Nothing
    KhumiAwaChin -> Nothing
    KhumiChin -> Nothing
    Khun -> Nothing
    Khunsari -> Nothing
    Khvarshi -> Nothing
    Kibet -> Nothing
    Kibiri -> Nothing
    Kiche -> Nothing
    Kickapoo -> Nothing
    Kikai -> Nothing
    Kikuyu -> Just $ ISO_639_2 "kik"
    KildinSami -> Nothing
    Kilivila -> Nothing
    Kiliwa -> Nothing
    Kilmeri -> Nothing
    Kim -> Nothing
    KimMun -> Nothing
    Kimaama -> Nothing
    Kimaragang -> Nothing
    Kimbu -> Nothing
    Kimbundu -> Just $ ISO_639_2 "kmb"
    Kimki -> Nothing
    Kimre -> Nothing
    Kinabalian -> Nothing
    Kinalakna -> Nothing
    KinarayA -> Nothing
    Kinga -> Nothing
    Kinnauri -> Nothing
    Kintaq -> Nothing
    Kinuku -> Nothing
    Kinyarwanda -> Just $ ISO_639_2 "kin"
    Kioko -> Nothing
    Kiong -> Nothing
    Kiorr -> Nothing
    Kiowa -> Nothing
    Kipsigis -> Nothing
    Kiput -> Nothing
    KirBalar -> Nothing
    Kire -> Nothing
    Kirike -> Nothing
    Kirikiri -> Nothing
    Kirmanjki_IndividualLanguage -> Just $ ISO_639_2 "zza"
    KiryaKonzel -> Nothing
    Kis -> Nothing
    Kisa -> Nothing
    Kisan -> Nothing
    Kisankasa -> Nothing
    Kisar -> Nothing
    Kisi -> Nothing
    Kistane -> Nothing
    KitaManinkakan -> Nothing
    KitanKhitan -> Nothing
    Kitja -> Nothing
    Kitsai -> Nothing
    Kituba_Congo -> Nothing
    Kituba_DemocraticRepublicOfCongo -> Nothing
    Kizamani -> Nothing
    KlaDan -> Nothing
    KlallamClallam -> Nothing
    KlamathModoc -> Nothing
    Klao -> Nothing
    KliasRiverKadazan -> Nothing
    Klingon -> Just $ ISO_639_2 "tlh"
    Knaanic -> Nothing
    Ko -> Nothing
    Koalib -> Nothing
    Koasati -> Nothing
    Koba -> Nothing
    Kobiana -> Nothing
    Kobo -> Nothing
    Kobol -> Nothing
    Kobon -> Nothing
    Koch -> Nothing
    KochilaTharu -> Nothing
    Koda -> Nothing
    Kodava -> Nothing
    Kodeoha -> Nothing
    Kodi -> Nothing
    Kodia -> Nothing
    Koenoem -> Nothing
    Kofa -> Nothing
    Kofei -> Nothing
    Kofyar -> Nothing
    Koguryo -> Nothing
    Kohin -> Nothing
    KohistaniShina -> Nothing
    Koho -> Nothing
    Kohoroxitari -> Nothing
    Kohumono -> Nothing
    Koi -> Nothing
    Koibal -> Nothing
    Koireng -> Nothing
    Koitabu -> Nothing
    Koiwat -> Nothing
    KokBorok -> Nothing
    KokNar -> Nothing
    Kokata -> Nothing
    Koke -> Nothing
    KokiNaga -> Nothing
    KokoBabangk -> Nothing
    Kokoda -> Nothing
    Kokola -> Nothing
    Kokota -> Nothing
    Kol -> Nothing
    Kol_Cameroon -> Nothing
    Kol_PapuaNewGuinea -> Nothing
    Kola -> Nothing
    Kolbila -> Nothing
    KolibuganSubanon -> Nothing
    Kolom -> Nothing
    KolumSoDogon -> Nothing
    Koluwawa -> Nothing
    Kom_Cameroon -> Nothing
    Kom_India -> Nothing
    Koma -> Nothing
    Komba -> Nothing
    Kombai -> Nothing
    Kombio -> Nothing
    Komering -> Nothing
    Komi -> Nothing
    KomiPermyak -> Nothing
    KomiZyrian -> Nothing
    Kominimung -> Nothing
    Komo_DemocraticRepublicOfCongo -> Nothing
    Komo_Sudan -> Nothing
    Komodo -> Nothing
    Kompane -> Nothing
    Komso -> Nothing
    Komyandaret -> Nothing
    KonKeu -> Nothing
    Konai -> Nothing
    Konda -> Nothing
    KondaDora -> Nothing
    Koneraw -> Nothing
    Kongo -> Just $ ISO_639_2 "kon"
    Konjo -> Nothing
    Konkani_Generic -> Just $ ISO_639_2 "kok"
    Konkani_Specific -> Nothing
    Konkomba -> Nothing
    Konni -> Nothing
    Kono_Guinea -> Just $ ISO_639_2 "kpe"
    Kono_Nigeria -> Nothing
    Kono_SierraLeone -> Nothing
    Konomala -> Nothing
    Konongo -> Nothing
    KonyakNaga -> Nothing
    KonyankaManinka -> Nothing
    Koongo -> Just $ ISO_639_2 "kon"
    Koonzime -> Nothing
    Koorete -> Nothing
    Kopar -> Nothing
    Kopkaka -> Nothing
    Korafe -> Nothing
    Korak -> Nothing
    Koraku -> Nothing
    Korana -> Nothing
    Korandje -> Nothing
    Korean -> Just $ ISO_639_2 "kor"
    KoreanSignLanguage -> Just $ ISO_639_2 "sgn"
    Koreguaje -> Nothing
    KoreshERostam -> Nothing
    Korku -> Nothing
    KorlaiCreolePortuguese -> Nothing
    KoroNulu -> Nothing
    KoroZuba -> Nothing
    Koro_CoteDivoire -> Nothing
    Koro_India -> Nothing
    Koro_PapuaNewGuinea -> Nothing
    Koro_Vanuatu -> Nothing
    Koromfe -> Nothing
    Koromira -> Nothing
    KoronadalBlaan -> Nothing
    Koroni -> Nothing
    Korop -> Nothing
    Koropo -> Nothing
    Koroshi -> Nothing
    Korowai -> Nothing
    KorraKoraga -> Nothing
    Korubo -> Nothing
    KorupunSela -> Nothing
    Korwa -> Nothing
    Koryak -> Nothing
    Kosadle -> Nothing
    KosarekYale -> Nothing
    Kosena -> Nothing
    Koshin -> Nothing
    Kosraean -> Just $ ISO_639_2 "kos"
    KotaBangunKutaiMalay -> Nothing
    KotaMaruduTalantang -> Nothing
    KotaMaruduTinagas -> Nothing
    Kota_Gabon -> Nothing
    Kota_India -> Nothing
    KotafonGbe -> Nothing
    Kotava -> Nothing
    Koti -> Nothing
    Kott -> Nothing
    Kouya -> Nothing
    Kovai -> Nothing
    Kove -> Nothing
    Kowaki -> Nothing
    Kowiai -> Nothing
    KoySanjaqSurat -> Nothing
    Koya -> Nothing
    Koyaga -> Nothing
    Koyo -> Nothing
    KoyraChiiniSonghay -> Nothing
    KoyraboroSenniSonghai -> Nothing
    Koyukon -> Nothing
    Kpagua -> Nothing
    Kpala -> Nothing
    Kpan -> Nothing
    Kpasam -> Nothing
    Kpati -> Nothing
    Kpatili -> Nothing
    Kpeego -> Nothing
    Kpelle -> Just $ ISO_639_2 "kpe"
    Kpessi -> Just $ ISO_639_2 "ewe"
    Kplang -> Nothing
    Krache -> Nothing
    Kraho -> Nothing
    Kraol -> Nothing
    Kravet -> Nothing
    KreenAkarore -> Nothing
    Krenak -> Nothing
    Krevinian -> Nothing
    Kreye -> Nothing
    KrikatiTimbira -> Nothing
    Krim -> Nothing
    Krio -> Nothing
    Kriol -> Nothing
    Krisa -> Nothing
    Krobu -> Nothing
    Krongo -> Nothing
    Krui -> Nothing
    Krung2 -> Nothing
    Kryts -> Nothing
    Kua -> Nothing
    KuaNsi -> Nothing
    Kuamasi -> Nothing
    Kuan -> Nothing
    Kuanhua -> Nothing
    Kuanua -> Nothing
    Kuanyama -> Just $ ISO_639_2 "kua"
    Kubachi -> Nothing
    Kube -> Nothing
    Kubi -> Nothing
    Kubo -> Nothing
    Kubu -> Nothing
    Kucong -> Nothing
    Kudiya -> Nothing
    Kudmali -> Nothing
    KuduCamo -> Nothing
    KufrQassemSignLanguage_Kqsl -> Nothing
    Kugama -> Nothing
    Kugbo -> Nothing
    Kui -> Nothing
    Kui_India -> Nothing
    Kui_Indonesia -> Nothing
    Kuijau -> Nothing
    KuikuroKalapalo -> Nothing
    Kujarge -> Nothing
    Kuk -> Nothing
    Kukatja -> Nothing
    Kukele -> Nothing
    Kukna -> Nothing
    Kuku -> Nothing
    KukuMangk -> Nothing
    KukuMuinh -> Nothing
    KukuMuminh -> Nothing
    KukuUgbanh -> Nothing
    KukuUwanh -> Nothing
    KukuYalanji -> Nothing
    Kula -> Nothing
    Kulere -> Nothing
    Kulfa -> Nothing
    KulinaPano -> Nothing
    Kulisusu -> Nothing
    KulluPahari -> Nothing
    Kulon -> Nothing
    KulonPazeh -> Nothing
    Kulung_Nepal -> Nothing
    Kulung_Nigeria -> Nothing
    Kumak -> Nothing
    Kumalu -> Nothing
    Kumam -> Nothing
    Kuman -> Nothing
    Kuman_Russia -> Nothing
    KumarbhagPaharia -> Nothing
    Kumauni -> Nothing
    Kumba -> Nothing
    Kumbainggar -> Nothing
    Kumbaran -> Nothing
    Kumbewaha -> Nothing
    Kumhali -> Nothing
    Kumiai -> Nothing
    Kumukio -> Nothing
    Kumyk -> Just $ ISO_639_2 "kum"
    Kumzari -> Nothing
    Kunama -> Nothing
    Kunbarlang -> Nothing
    Kunda -> Nothing
    KundalShahi -> Nothing
    Kunduvadi -> Nothing
    Kunfal -> Nothing
    Kung -> Nothing
    KungEkoka -> Nothing
    Kungarakany -> Nothing
    Kungardutyi -> Nothing
    Kunggara -> Nothing
    Kunggari -> Nothing
    Kungkari -> Nothing
    Kuni -> Nothing
    KuniBoazi -> Nothing
    Kunigami -> Nothing
    Kunimaipa -> Nothing
    Kunja -> Nothing
    Kunjen -> Nothing
    Kunyi -> Nothing
    Kunza -> Nothing
    Kuo -> Nothing
    Kuot -> Nothing
    Kupa -> Nothing
    KupangMalay -> Nothing
    Kupia -> Nothing
    Kupsabiny -> Nothing
    Kur -> Nothing
    KuraEdeNago -> Nothing
    Kurama -> Nothing
    Kuranko -> Nothing
    Kurdish -> Just $ ISO_639_2 "kur"
    Kuri -> Nothing
    Kuria -> Nothing
    Kurichiya -> Nothing
    Kurmukar -> Nothing
    Kurnai -> Nothing
    Kurrama -> Nothing
    Kurti -> Nothing
    Kurtokha -> Nothing
    Kuruaya -> Nothing
    Kurudu -> Nothing
    Kurukh -> Nothing
    Kurumba -> Nothing
    Kusaal -> Nothing
    Kusaghe -> Nothing
    Kushi -> Nothing
    Kusu -> Nothing
    Kusunda -> Nothing
    KutangGhale -> Nothing
    Kutenai -> Just $ ISO_639_2 "kut"
    Kutep -> Nothing
    Kuthant -> Nothing
    Kutto -> Nothing
    Kutu -> Nothing
    Kuturmi -> Nothing
    KuukYak -> Nothing
    KuukuYau -> Nothing
    Kuvale -> Nothing
    Kuvi -> Nothing
    Kuwaa -> Nothing
    Kuwaataay -> Nothing
    Kuy -> Nothing
    Kwa -> Nothing
    Kwaami -> Nothing
    Kwadi -> Nothing
    Kwadza -> Nothing
    Kwaio -> Nothing
    Kwaja -> Nothing
    Kwak -> Nothing
    Kwakiutl -> Nothing
    Kwakum -> Nothing
    KwalhioquaTlatskanai -> Nothing
    Kwama -> Nothing
    Kwambi -> Nothing
    Kwamera -> Nothing
    Kwami -> Nothing
    KwamtimOne -> Nothing
    Kwandu -> Nothing
    Kwang -> Nothing
    Kwanga -> Nothing
    Kwangali -> Nothing
    Kwanja -> Nothing
    Kwaraae -> Nothing
    Kwato -> Nothing
    Kwaya -> Nothing
    Kwaza -> Nothing
    Kwegu -> Nothing
    Kwer -> Nothing
    Kwerba -> Nothing
    KwerbaMamberamo -> Nothing
    Kwere -> Nothing
    Kwerisa -> Nothing
    Kwese -> Nothing
    Kwesten -> Nothing
    Kwini -> Nothing
    Kwinsu -> Nothing
    Kwinti -> Nothing
    Kwoma -> Nothing
    Kwomtari -> Nothing
    Kxaullein -> Nothing
    Kxoe -> Nothing
    Kyak -> Nothing
    Kyaka -> Nothing
    KyanKaryawNaga -> Nothing
    Kyenele -> Nothing
    Kyenga -> Nothing
    Kyerung -> Nothing
    Kyrgyz -> Just $ ISO_639_2 "kir"
    LaaLaaBwamu -> Nothing
    Laadan -> Nothing
    Laal -> Nothing
    Laari -> Just $ ISO_639_2 "kon"
    Laba -> Nothing
    Label -> Nothing
    Labi -> Nothing
    Labir -> Nothing
    Labo -> Nothing
    LaboPhowa -> Nothing
    Labu -> Nothing
    LabukKinabatanganKadazan -> Nothing
    Lacandon -> Nothing
    Lachi -> Nothing
    LachiguiriZapotec -> Nothing
    LachirioagZapotec -> Nothing
    LachixioZapotec -> Nothing
    Ladakhi -> Nothing
    Ladin -> Nothing
    Ladino -> Just $ ISO_639_2 "lad"
    LadjiLadji -> Nothing
    LaekoLibuat -> Nothing
    Lafofa -> Nothing
    Laghu -> Nothing
    Laghuu -> Nothing
    Lagwan -> Nothing
    Laha_Indonesia -> Nothing
    Laha_VietNam -> Nothing
    Lahanan -> Nothing
    Lahnda -> Just $ ISO_639_2 "lah"
    LahtaKaren -> Nothing
    Lahu -> Nothing
    LahuShi -> Nothing
    LahulLohar -> Nothing
    LaiChin -> Nothing
    Laimbue -> Nothing
    LaituChin -> Nothing
    Laiyolo -> Nothing
    Lak -> Nothing
    Laka_Chad -> Nothing
    Laka_Nigeria -> Nothing
    Lakalei -> Nothing
    LakeMiwok -> Nothing
    Lakha -> Nothing
    Laki -> Nothing
    Lakkia -> Nothing
    Lakona -> Nothing
    Lakonde -> Nothing
    Lakota -> Nothing
    LakotaDida -> Nothing
    Lakurumau -> Nothing
    Lala -> Nothing
    LalaBisa -> Nothing
    LalaRoba -> Nothing
    LalanaChinantec -> Nothing
    Lalia -> Nothing
    Lama_Myanmar -> Nothing
    Lama_Togo -> Nothing
    Lamaholot -> Nothing
    Lamalera -> Nothing
    Lamam -> Nothing
    Lamang -> Nothing
    Lamatuka -> Nothing
    Lamba -> Just $ ISO_639_2 "lam"
    Lambadi -> Nothing
    LambayequeQuechua -> Nothing
    Lambichhong -> Nothing
    Lamboya -> Nothing
    Lambya -> Nothing
    Lame -> Nothing
    Lamenu -> Nothing
    Lamet -> Nothing
    LamjaDengsaTola -> Nothing
    Lamkang -> Nothing
    Lamma -> Nothing
    Lamnso -> Nothing
    Lamogai -> Nothing
    Lampung -> Nothing
    Lamu -> Nothing
    LamuLamu -> Nothing
    LanasLobu -> Nothing
    LandDayak -> Nothing
    Landoma -> Nothing
    Langam -> Nothing
    Langbashe -> Nothing
    Lange -> Nothing
    Langi -> Nothing
    LangnianBuyang -> Nothing
    Lango -> Nothing
    Lango_Sudan -> Nothing
    Lango_Uganda -> Nothing
    Langobardic -> Nothing
    LangueDesSignesDeBelgiqueFrancophone -> Nothing
    Languedocien -> Nothing
    Lanima -> Nothing
    Lanoh -> Nothing
    Lantanai -> Nothing
    Lao -> Just $ ISO_639_2 "lao"
    LaoNaga -> Nothing
    Laomian -> Nothing
    Laopang -> Nothing
    LaosSignLanguage -> Nothing
    LapuyanSubanun -> Nothing
    Lara -> Nothing
    Laragia -> Nothing
    LarantukaMalay -> Nothing
    Lardil -> Nothing
    Larevat -> Nothing
    Lari -> Nothing
    LarikeWakasihu -> Nothing
    Laro -> Nothing
    Larteh -> Nothing
    Laru -> Nothing
    LasDeliciasZapotec -> Nothing
    Lasalimu -> Nothing
    Lasgerdi -> Nothing
    Lashi -> Nothing
    Lasi -> Nothing
    LateMiddleChinese -> Nothing
    Latgalian -> Nothing
    Latin -> Just $ ISO_639_2 "lat"
    Latu -> Nothing
    Latunde -> Nothing
    Latvian -> Just $ ISO_639_2 "lav"
    LatvianSignLanguage -> Nothing
    Lau -> Nothing
    Laua -> Nothing
    Lauan -> Nothing
    Lauje -> Nothing
    Laura -> Nothing
    Laurentian -> Nothing
    LautuChin -> Nothing
    LavatburaLamusong -> Nothing
    Lave -> Nothing
    Laven -> Nothing
    Lavi -> Nothing
    Lavukaleve -> Nothing
    Lawangan -> Nothing
    Lawu -> Nothing
    Lawunuia -> Nothing
    Layakha -> Nothing
    Laz -> Nothing
    LealaoChinantec -> Nothing
    Leco -> Nothing
    LedoKaili -> Nothing
    Leelau -> Nothing
    Lefa -> Nothing
    LegaMwenga -> Nothing
    LegaShabunda -> Nothing
    Legbo -> Nothing
    Legenyem -> Nothing
    Lehali -> Nothing
    Lehalurup -> Nothing
    Lehar -> Nothing
    LeinongNaga -> Nothing
    Leipon -> Nothing
    Lelak -> Nothing
    Lele_Chad -> Nothing
    Lele_DemocraticRepublicOfCongo -> Nothing
    Lele_Guinea -> Nothing
    Lele_PapuaNewGuinea -> Nothing
    Lelemi -> Nothing
    Lelepa -> Nothing
    Lematang -> Nothing
    Lembak -> Nothing
    Lembena -> Nothing
    Lemerig -> Nothing
    Lemio -> Nothing
    Lemnian -> Nothing
    Lemolang -> Nothing
    Lemoro -> Nothing
    Lenakel -> Nothing
    Lenca -> Nothing
    Lendu -> Nothing
    Lengilu -> Nothing
    Lengo -> Nothing
    Lengola -> Nothing
    Lengua -> Nothing
    Leningitij -> Nothing
    Lenje -> Nothing
    Lenkau -> Nothing
    Lenyima -> Nothing
    Lepcha -> Nothing
    Lepki -> Nothing
    Lepontic -> Nothing
    Lere -> Nothing
    Lese -> Nothing
    LesingGelimi -> Nothing
    Letemboi -> Nothing
    Leti_Cameroon -> Nothing
    Leti_Indonesia -> Nothing
    Levuka -> Nothing
    Lewo -> Nothing
    LewoEleng -> Nothing
    Lewotobi -> Nothing
    Lezghian -> Just $ ISO_639_2 "lez"
    Lhokpu -> Nothing
    Lhomi -> Nothing
    Liabuku -> Nothing
    LianaSeti -> Nothing
    LiangmaiNaga -> Nothing
    LianshanZhuang -> Just $ ISO_639_2 "zha"
    LiberiaKpelle -> Just $ ISO_639_2 "kpe"
    LiberianEnglish -> Nothing
    Libido -> Nothing
    Libinza -> Nothing
    LibonBikol -> Nothing
    Liburnian -> Nothing
    LibyanSignLanguage -> Nothing
    Ligbi -> Nothing
    Ligenza -> Nothing
    Ligurian -> Nothing
    Ligurian_Ancient -> Nothing
    Lihir -> Nothing
    Lijili -> Nothing
    Lika -> Nothing
    Liki -> Nothing
    Likila -> Nothing
    Likuba -> Nothing
    Likum -> Nothing
    Likwala -> Nothing
    Lilau -> Nothing
    Lillooet -> Nothing
    Limassa -> Nothing
    Limbu -> Nothing
    Limbum -> Nothing
    LimburganLimburgish -> Just $ ISO_639_2 "lim"
    LimiYi -> Nothing
    Limilngan -> Nothing
    LimosKalinga -> Nothing
    Limousin -> Nothing
    Lindu -> Nothing
    LinearA -> Nothing
    Lingala -> Just $ ISO_639_2 "lin"
    Lingao -> Nothing
    Lingarak -> Nothing
    Lingkhim -> Nothing
    LinguaFranca -> Nothing
    LinguaFrancaNova -> Nothing
    Lintang -> Nothing
    Lio -> Nothing
    Lipo -> Nothing
    LisabataNuniali -> Nothing
    Lisela -> Nothing
    Lish -> Nothing
    LishanDidan -> Nothing
    LishanaDeni -> Nothing
    LishanidNoshan -> Nothing
    Lisu -> Nothing
    LiteraryChinese -> Nothing
    Lithuanian -> Just $ ISO_639_2 "lit"
    LithuanianSignLanguage -> Nothing
    Litzlitz -> Nothing
    LiujiangZhuang -> Just $ ISO_639_2 "zha"
    LiuqianZhuang -> Just $ ISO_639_2 "zha"
    Liv_Onian -> Nothing
    Livvi -> Nothing
    Loarki -> Nothing
    Lobala -> Nothing
    Lobi -> Nothing
    Lodhi -> Nothing
    Logba -> Nothing
    Logir -> Nothing
    Logo -> Nothing
    Logol -> Nothing
    Logooli -> Nothing
    Logorik -> Nothing
    LogudoreseSardinian -> Just $ ISO_639_2 "srd"
    LojaHighlandQuichua -> Nothing
    Lojban -> Just $ ISO_639_2 "jbo"
    Lokaa -> Nothing
    Loko -> Nothing
    Lokoya -> Nothing
    Lola -> Nothing
    Lolak -> Nothing
    Lole -> Nothing
    Lolo -> Nothing
    Loloda -> Nothing
    Lom -> Nothing
    Loma_CoteDivoire -> Nothing
    Loma_Liberia -> Nothing
    Lomaiviti -> Nothing
    Lomavren -> Nothing
    Lombard -> Nothing
    Lombi -> Nothing
    Lombo -> Nothing
    Lomwe -> Nothing
    Loncong -> Nothing
    LongPhuriNaga -> Nothing
    Longgu -> Nothing
    Longto -> Nothing
    Longuda -> Nothing
    Loniu -> Nothing
    Lonwolwol -> Nothing
    Lonzo -> Nothing
    Loo -> Nothing
    Lopa -> Nothing
    Lopi -> Nothing
    Lopit -> Nothing
    Lorang -> Nothing
    Lorediakarkar -> Nothing
    LoretoUcayaliSpanish -> Nothing
    Lote -> Nothing
    LothaNaga -> Nothing
    Lotud -> Nothing
    Lou -> Nothing
    LouisianaCreoleFrench -> Nothing
    Loun -> Nothing
    LoupA -> Nothing
    LoupB -> Nothing
    LowGerman -> Just $ ISO_639_2 "nds"
    Lowa -> Nothing
    LowerBurdekin -> Nothing
    LowerChehalis -> Nothing
    LowerGrandValleyDani -> Nothing
    LowerNossob -> Nothing
    LowerPokomo -> Nothing
    LowerSilesian -> Nothing
    LowerSorbian -> Just $ ISO_639_2 "dsb"
    LowerSouthernAranda -> Nothing
    LowerTanana -> Nothing
    LowerTanudanKalinga -> Nothing
    LowerTaoih -> Nothing
    LowlandOaxacaChontal -> Nothing
    LowlandTarahumara -> Nothing
    LoxichaZapotec -> Nothing
    Lozi -> Just $ ISO_639_2 "loz"
    Lu -> Nothing
    Lua -> Nothing
    Luang -> Nothing
    LubaKatanga -> Just $ ISO_639_2 "lub"
    LubaLulua -> Just $ ISO_639_2 "lua"
    Lubila -> Nothing
    Lubu -> Nothing
    LubuaganKalinga -> Nothing
    Luchazi -> Nothing
    Lucumi -> Nothing
    Ludian -> Nothing
    Lufu -> Nothing
    Luganda -> Just $ ISO_639_2 "lug"
    Lugbara -> Nothing
    Luguru -> Nothing
    Luhu -> Nothing
    Lui -> Nothing
    Luimbi -> Nothing
    Luiseno -> Just $ ISO_639_2 "lui"
    Lukpa -> Nothing
    Lule -> Nothing
    LuleSami -> Just $ ISO_639_2 "smj"
    LumbaYakkha -> Nothing
    Lumbee -> Nothing
    Lumbu -> Nothing
    Lumun -> Nothing
    Luna -> Nothing
    Lunanakha -> Nothing
    Lunda -> Just $ ISO_639_2 "lun"
    Lundayeh -> Nothing
    Lungga -> Nothing
    Luo -> Nothing
    Luo_KenyaAndTanzania -> Just $ ISO_639_2 "luo"
    LuopoheHmong -> Nothing
    Luri -> Nothing
    Lusengo -> Nothing
    Lushai -> Just $ ISO_639_2 "lus"
    Lushootseed -> Nothing
    Lusi -> Nothing
    Lusitanian -> Nothing
    Lutos -> Nothing
    Luvale -> Nothing
    Luwati -> Nothing
    Luwo -> Nothing
    Luxembourgish -> Just $ ISO_639_2 "ltz"
    Luyana -> Nothing
    Luyia -> Nothing
    Lwalu -> Nothing
    Lwel -> Nothing
    Lycian -> Nothing
    Lydian -> Nothing
    Lyele -> Nothing
    Lyngngam -> Nothing
    LyonsSignLanguage -> Nothing
    Ma_DemocraticRepublicOfCongo -> Nothing
    Ma_PapuaNewGuinea -> Nothing
    Maa -> Nothing
    Maaka -> Nothing
    Maanyan -> Nothing
    MaasinaFulfulde -> Just $ ISO_639_2 "ful"
    Maay -> Nothing
    Maba_Chad -> Nothing
    Maba_Indonesia -> Nothing
    Mabaale -> Nothing
    Mabaan -> Nothing
    MabakaValleyKalinga -> Nothing
    Mabire -> Nothing
    Maca -> Nothing
    Macaguaje -> Nothing
    Macaguan -> Nothing
    Macanese -> Nothing
    Macedonian -> Just $ ISO_639_2 "mac"
    Machame -> Nothing
    Machiguenga -> Nothing
    Machinere -> Nothing
    Machinga -> Nothing
    Maco -> Nothing
    Macuna -> Nothing
    Macushi -> Nothing
    Mada_Cameroon -> Nothing
    Mada_Nigeria -> Nothing
    MadagascarSignLanguage -> Nothing
    Madak -> Nothing
    Madang -> Nothing
    Maden -> Nothing
    MadhiMadhi -> Nothing
    Madi -> Nothing
    Madngele -> Nothing
    MadukayangKalinga -> Nothing
    Madurese -> Just $ ISO_639_2 "mad"
    Mae -> Nothing
    Maek -> Nothing
    MaengItneg -> Nothing
    Mafa -> Nothing
    Mafea -> Nothing
    MagAnchiAyta -> Nothing
    MagIndiAyta -> Nothing
    Magahat -> Nothing
    Magahi -> Just $ ISO_639_2 "mag"
    MagdalenaPenascoMixtec -> Nothing
    Maghdi -> Nothing
    Magi_MadangProvince -> Nothing
    Magiyi -> Nothing
    Magoma -> Nothing
    Magori -> Nothing
    Maguindanao -> Nothing
    MahakamKenyah -> Nothing
    Mahali -> Just $ ISO_639_2 "sat"
    MaharastriPrakrit -> Nothing
    MahasuPahari -> Nothing
    Mahei -> Nothing
    Mahican -> Nothing
    Mahongwe -> Nothing
    Mahou -> Nothing
    MaiBrat -> Nothing
    Maia -> Nothing
    Maiadomu -> Nothing
    Maiani -> Nothing
    Maii -> Nothing
    Mailu -> Nothing
    Maindo -> Nothing
    Mainfrankisch -> Nothing
    Mairasi -> Nothing
    Maisin -> Nothing
    Maithili -> Just $ ISO_639_2 "mai"
    Maiwa_Indonesia -> Nothing
    Maiwa_PapuaNewGuinea -> Nothing
    Maiwala -> Nothing
    Majang -> Nothing
    Majera -> Nothing
    Majhi -> Nothing
    Majhwar -> Nothing
    Mak_China -> Nothing
    Mak_Nigeria -> Nothing
    Makaa -> Nothing
    Makah -> Nothing
    Makalero -> Nothing
    Makasae -> Nothing
    Makasar -> Just $ ISO_639_2 "mak"
    MakassarMalay -> Nothing
    Makayam -> Nothing
    Makhuwa -> Nothing
    MakhuwaMarrevone -> Nothing
    MakhuwaMeetto -> Nothing
    MakhuwaMoniga -> Nothing
    MakhuwaSaka -> Nothing
    MakhuwaShirima -> Nothing
    Maklew -> Nothing
    Makolkol -> Nothing
    Makonde -> Nothing
    Maku -> Nothing
    Makua -> Nothing
    Makurap -> Nothing
    MakuriNaga -> Nothing
    Makwe -> Nothing
    MakyanNaga -> Nothing
    Mal -> Nothing
    MalPaharia -> Nothing
    MalaMalasar -> Nothing
    Mala_Nigeria -> Nothing
    Mala_PapuaNewGuinea -> Nothing
    MalaccanCreoleMalay -> Nothing
    MalaccanCreolePortuguese -> Nothing
    Malagasy -> Just $ ISO_639_2 "mlg"
    Malakhel -> Nothing
    Malakote -> Nothing
    Malalamai -> Nothing
    Malango -> Nothing
    Malankuravan -> Nothing
    Malapandaram -> Nothing
    Malaryan -> Nothing
    Malas -> Nothing
    Malasanga -> Nothing
    Malasar -> Nothing
    Malavedan -> Nothing
    MalawiLomwe -> Nothing
    MalawiSena -> Nothing
    MalawianSignLanguage -> Nothing
    Malay_Generic -> Just $ ISO_639_2 "may"
    Malay_IndividualLanguage -> Just $ ISO_639_2 "may"
    Malayalam -> Just $ ISO_639_2 "mal"
    MalayicDayak -> Nothing
    Malaynon -> Nothing
    Malayo -> Nothing
    MalaysianSignLanguage -> Nothing
    MalbaBirifor -> Nothing
    Male_Ethiopia -> Nothing
    Male_PapuaNewGuinea -> Nothing
    MalecitePassamaquoddy -> Nothing
    MalekuJaika -> Nothing
    Maleng -> Nothing
    MaleuKilenge -> Nothing
    Malfaxal -> Nothing
    Malgana -> Nothing
    Malgbe -> Nothing
    Mali -> Nothing
    Maligo -> Nothing
    Malila -> Nothing
    Malimba -> Nothing
    Malimpung -> Nothing
    MalinaltepecTlapanec -> Nothing
    Malinguat -> Nothing
    Malo -> Nothing
    Malol -> Nothing
    Maltese -> Just $ ISO_639_2 "mlt"
    MalteseSignLanguage -> Nothing
    MaluaBay -> Nothing
    Malvi -> Nothing
    Malyangapa -> Nothing
    Mama -> Nothing
    Mamaa -> Nothing
    Mamainde -> Nothing
    Mamanwa -> Nothing
    MamaraSenoufo -> Nothing
    Mamasa -> Nothing
    Mambae -> Nothing
    Mambai -> Nothing
    Mamboru -> Nothing
    MambweLungu -> Nothing
    Mampruli -> Nothing
    Mamuju -> Nothing
    Mamulique -> Nothing
    Mamusi -> Nothing
    Mamvu -> Nothing
    ManMet -> Nothing
    ManadoMalay -> Nothing
    Manam -> Nothing
    Manambu -> Nothing
    Manangba -> Nothing
    Manangkari -> Nothing
    Manchu -> Just $ ISO_639_2 "mnc"
    Manda_Australia -> Nothing
    Manda_India -> Nothing
    Manda_Tanzania -> Nothing
    Mandahuaca -> Nothing
    Mandaic -> Nothing
    Mandan -> Nothing
    Mandandanyi -> Nothing
    Mandar -> Just $ ISO_639_2 "mdr"
    Mandara -> Nothing
    Mandari -> Nothing
    MandarinChinese -> Nothing
    Mandeali -> Nothing
    Mander -> Nothing
    Mandingo -> Nothing
    Mandinka -> Nothing
    Mandjak -> Nothing
    MandoboAtas -> Nothing
    MandoboBawah -> Nothing
    Manem -> Nothing
    Mang -> Nothing
    MangaKanuri -> Just $ ISO_639_2 "kau"
    Mangala -> Nothing
    Mangarayi -> Nothing
    Mangareva -> Nothing
    Mangas -> Nothing
    Mangayat -> Nothing
    Mangbetu -> Nothing
    Mangbutu -> Nothing
    Mangerr -> Nothing
    MangettiDuneXung -> Nothing
    ManggaBuang -> Nothing
    Manggarai -> Nothing
    Mango -> Nothing
    Mangole -> Nothing
    Mangseng -> Nothing
    ManichaeanMiddlePersian -> Just $ ISO_639_2 "pal"
    ManigriKamboleEdeNago -> Nothing
    Manikion -> Nothing
    Manipa -> Nothing
    Mankanya -> Nothing
    Mankiyali -> Nothing
    Mann -> Nothing
    MannaDora -> Nothing
    Mannan -> Nothing
    Manombai -> Nothing
    Mansaka -> Nothing
    Mansi -> Nothing
    Mansoanka -> Nothing
    Manta -> Nothing
    Mantsi -> Nothing
    ManumanawKaren -> Nothing
    Manusela -> Nothing
    Manx -> Just $ ISO_639_2 "glv"
    Manya -> Nothing
    Manyawa -> Nothing
    Manyika -> Nothing
    Manza -> Nothing
    MaoNaga -> Nothing
    Maonan -> Nothing
    Maori -> Just $ ISO_639_2 "mao"
    Mape -> Nothing
    Mapena -> Nothing
    Mapia -> Nothing
    Mapidian -> Nothing
    MaposBuang -> Nothing
    Mapoyo -> Nothing
    Mapun -> Nothing
    Maquiritari -> Nothing
    Mara -> Nothing
    MaraChin -> Nothing
    Marachi -> Nothing
    Maraghei -> Nothing
    Maragus -> Nothing
    MaramNaga -> Nothing
    Marama -> Nothing
    Maramba -> Nothing
    Maranao -> Nothing
    Maranunggu -> Nothing
    Mararit -> Nothing
    Marathi -> Just $ ISO_639_2 "mar"
    Marau -> Nothing
    Marba -> Nothing
    MardinSignLanguage -> Nothing
    Maremgi -> Nothing
    Marenje -> Nothing
    Marfa -> Nothing
    Margany -> Nothing
    MarghiCentral -> Nothing
    MarghiSouth -> Nothing
    MargosYarowilcaLauricochaQuechua -> Nothing
    Margu -> Nothing
    Mari_EastSepikProvince -> Nothing
    Mari_MadangProvince -> Nothing
    Mari_Russia -> Just $ ISO_639_2 "chm"
    Maria_India -> Nothing
    Maria_PapuaNewGuinea -> Nothing
    Maricopa -> Nothing
    Maridan -> Nothing
    Maridjabin -> Nothing
    Marik -> Nothing
    Marimanindji -> Nothing
    Marind -> Nothing
    Maring -> Nothing
    MaringNaga -> Nothing
    Maringarr -> Nothing
    Marino -> Nothing
    Mariri -> Nothing
    Marithiel -> Nothing
    MaritimeSignLanguage -> Nothing
    Maritsaua -> Nothing
    Mariyedi -> Nothing
    Marka -> Nothing
    Marma -> Nothing
    Marovo -> Nothing
    Marriammu -> Nothing
    Marrucinian -> Nothing
    Marshall -> Just $ ISO_639_2 "mah"
    Marsian -> Nothing
    MarthasVineyardSignLanguage -> Nothing
    MartiKe -> Nothing
    MartuWangka -> Nothing
    Martuyhunira -> Nothing
    Maru -> Nothing
    Marubo -> Nothing
    Marwari -> Just $ ISO_639_2 "mwr"
    Marwari_India -> Just $ ISO_639_2 "mwr"
    Marwari_Pakistan -> Just $ ISO_639_2 "mwr"
    Masaba -> Nothing
    MasadiitItneg -> Nothing
    Masai -> Just $ ISO_639_2 "mas"
    Masalit -> Nothing
    Masana -> Nothing
    MasbateSorsogon -> Nothing
    Masbatenyo -> Nothing
    MashcoPiro -> Nothing
    Mashi_Nigeria -> Nothing
    Mashi_Zambia -> Nothing
    MasikoroMalagasy -> Just $ ISO_639_2 "mlg"
    Masimasi -> Nothing
    Masiwang -> Nothing
    Maskelynes -> Nothing
    MaskoyPidgin -> Nothing
    Maslam -> Nothing
    Masmaje -> Nothing
    Massalat -> Nothing
    Massep -> Nothing
    Matagalpa -> Nothing
    Matal -> Nothing
    Matambwe -> Nothing
    Matbat -> Nothing
    Matengo -> Nothing
    Matepi -> Nothing
    MatigsalugManobo -> Nothing
    Matipuhy -> Nothing
    Matis -> Nothing
    Mato -> Nothing
    Mator -> Nothing
    MatorTaygiKaragas -> Nothing
    Matses -> Nothing
    Mattole -> Nothing
    Matukar -> Nothing
    Matumbi -> Nothing
    MatyaSamo -> Nothing
    Maung -> Nothing
    MauritianSignLanguage -> Nothing
    Mauwake -> Nothing
    Mawa_Chad -> Nothing
    Mawa_Nigeria -> Nothing
    Mawak -> Nothing
    Mawan -> Nothing
    Mawayana -> Nothing
    Mawchi -> Nothing
    Mawes -> Nothing
    Maxakali -> Nothing
    MaxiGbe -> Just $ ISO_639_2 "fon"
    Maya -> Nothing
    MayaSamo -> Nothing
    Mayaguduna -> Nothing
    Mayangna -> Nothing
    Mayawali -> Nothing
    Mayeka -> Nothing
    MayiKulan -> Nothing
    MayiThakurti -> Nothing
    MayiYapi -> Nothing
    Maykulan -> Nothing
    Mayo -> Nothing
    Mayogo -> Nothing
    MayoyaoIfugao -> Nothing
    Mazagway -> Nothing
    MazahuaCentral -> Nothing
    MazaltepecZapotec -> Nothing
    Mazanderani -> Nothing
    MazatlanMazatec -> Nothing
    MazatlanMixe -> Nothing
    Mba -> Nothing
    Mbabaram -> Nothing
    Mbala -> Nothing
    Mbalanhu -> Just $ ISO_639_2 "kua"
    Mbandja -> Nothing
    Mbangala -> Nothing
    Mbangi -> Nothing
    Mbangwe -> Nothing
    Mbara_Australia -> Nothing
    Mbara_Chad -> Nothing
    MbarimanGudhinma -> Nothing
    Mbati -> Nothing
    Mbato -> Nothing
    Mbay -> Nothing
    Mbe -> Nothing
    Mbedam -> Nothing
    Mbelime -> Nothing
    Mbere -> Nothing
    Mbesa -> Nothing
    Mbessa -> Nothing
    MboUng -> Nothing
    Mbo_Cameroon -> Nothing
    Mbo_DemocraticRepublicOfCongo -> Nothing
    Mboi -> Nothing
    Mboko -> Nothing
    Mbole -> Nothing
    Mbonga -> Nothing
    Mbongno -> Nothing
    Mbosi -> Nothing
    Mbowe -> Nothing
    Mbre -> Nothing
    Mbu -> Nothing
    Mbugu -> Nothing
    Mbugwe -> Nothing
    Mbuk -> Nothing
    Mbuko -> Nothing
    Mbukushu -> Nothing
    Mbula -> Nothing
    MbulaBwazza -> Nothing
    Mbule -> Nothing
    Mbulungish -> Nothing
    Mbum -> Nothing
    Mbunda -> Nothing
    Mbunga -> Nothing
    Mburku -> Nothing
    Mbwela -> Nothing
    MbyaGuarani -> Nothing
    Mea -> Nothing
    Medebur -> Nothing
    Medefaidrin -> Nothing
    MediaLengua -> Nothing
    Mediak -> Nothing
    Median -> Nothing
    MednyjAleut -> Nothing
    Medumba -> Nothing
    Meen -> Nothing
    Mefele -> Nothing
    Megam -> Nothing
    MeglenoRomanian -> Nothing
    Mehek -> Nothing
    Mehinaku -> Nothing
    Mehri -> Nothing
    Meitei -> Just $ ISO_639_2 "mni"
    Mekeo -> Nothing
    Mekmek -> Nothing
    Mekwei -> Nothing
    MelKhaonh -> Nothing
    Melanau -> Nothing
    MeleFila -> Nothing
    Melo -> Nothing
    Melpa -> Nothing
    Memoni -> Nothing
    MendalamKayan -> Nothing
    MendankweNkwen -> Nothing
    Mende_PapuaNewGuinea -> Nothing
    Mende_SierraLeone -> Just $ ISO_639_2 "men"
    Mengaka -> Nothing
    Mengen -> Nothing
    Mengisa -> Nothing
    Menka -> Nothing
    Menominee -> Nothing
    Mentawai -> Nothing
    Menya -> Nothing
    Meoswar -> Nothing
    Mer -> Nothing
    Meramera -> Nothing
    Merei -> Nothing
    Merey -> Nothing
    Meriam -> Nothing
    Merlav -> Nothing
    Meroitic -> Nothing
    Meru -> Nothing
    Merwari -> Just $ ISO_639_2 "mwr"
    Mesaka -> Nothing
    Mese -> Nothing
    Mesme -> Nothing
    Mesmes -> Nothing
    Mesqan -> Nothing
    Mesquakie -> Nothing
    Messapic -> Nothing
    Meta -> Nothing
    MetlatonocMixtec -> Nothing
    Mewari -> Nothing
    Mewati -> Nothing
    MexicanSignLanguage -> Nothing
    Meyah -> Nothing
    MezontlaPopoloca -> Nothing
    MezquitalOtomi -> Nothing
    Mfinu -> Nothing
    Mfumte -> Nothing
    Mgbolizhia -> Nothing
    MiahuatlanZapotec -> Nothing
    Miami -> Nothing
    Mian -> Nothing
    Miani -> Nothing
    Miarra -> Nothing
    Michif -> Nothing
    Michigamea -> Nothing
    MichoacanMazahua -> Nothing
    MichoacanNahuatl -> Nothing
    Micmac -> Just $ ISO_639_2 "mic"
    MidGrandValleyDani -> Nothing
    MidSouthernBanda -> Nothing
    MiddleArmenian -> Nothing
    MiddleBreton -> Just $ ISO_639_2 "bre"
    MiddleCornish -> Just $ ISO_639_2 "cor"
    MiddleDutch -> Just $ ISO_639_2 "dum"
    MiddleEnglish -> Just $ ISO_639_2 "enm"
    MiddleFrench -> Just $ ISO_639_2 "frm"
    MiddleHighGerman -> Just $ ISO_639_2 "gmh"
    MiddleHittite -> Just $ ISO_639_2 "hit"
    MiddleIrish -> Just $ ISO_639_2 "mga"
    MiddleKhmer -> Nothing
    MiddleKorean -> Nothing
    MiddleLowGerman -> Nothing
    MiddleMongolian -> Nothing
    MiddleNewar -> Just $ ISO_639_2 "new"
    MiddleWatut -> Nothing
    MiddleWelsh -> Nothing
    Midob -> Nothing
    Migaama -> Nothing
    Migabac -> Nothing
    MijuMishmi -> Nothing
    Mikasuki -> Nothing
    MiliYi -> Nothing
    Miltu -> Nothing
    Miluk -> Nothing
    Milyan -> Nothing
    Mimi -> Nothing
    MinBeiChinese -> Nothing
    MinDongChinese -> Nothing
    MinNanChinese -> Nothing
    MinZhongChinese -> Nothing
    Mina_Cameroon -> Nothing
    Mina_India -> Nothing
    Minaean -> Nothing
    Minang -> Nothing
    Minangkabau -> Just $ ISO_639_2 "min"
    Minanibai -> Nothing
    Minaveha -> Nothing
    Minderico -> Nothing
    Mindiri -> Nothing
    MingangDoso -> Nothing
    Mingrelian -> Nothing
    MinicaHuitoto -> Nothing
    Minigir -> Nothing
    Minjungbal -> Nothing
    Minkin -> Nothing
    Minoan -> Nothing
    Minokok -> Nothing
    Minriq -> Nothing
    Mintil -> Nothing
    MinzZhuang -> Nothing
    MiqieYi -> Nothing
    Mirandese -> Just $ ISO_639_2 "mwl"
    MirayaBikol -> Nothing
    Mire -> Nothing
    Mirgan -> Nothing
    Miri -> Nothing
    Miriti -> Nothing
    MiriwoongSignLanguage -> Nothing
    Miriwung -> Nothing
    MirninyMirning -> Nothing
    MirpurPanjabi -> Nothing
    Miship -> Nothing
    MisimaPaneati -> Nothing
    Miskito -> Nothing
    MitlaZapotec -> Nothing
    MitlatongoMixtec -> Nothing
    Mittu -> Nothing
    Mituku -> Nothing
    Miu -> Nothing
    Miwa -> Nothing
    MixedGreatAndamanese -> Nothing
    Mixifore -> Nothing
    Mixtec_WesternJuxtlahuaca -> Nothing
    MixtepecMixtec -> Nothing
    MixtepecZapotec -> Nothing
    Miya -> Nothing
    Miyako -> Nothing
    MiyakuboSignLanguage -> Nothing
    Miyobe -> Nothing
    Mlabri -> Nothing
    Mlahso -> Nothing
    Mlap -> Nothing
    Mlomp -> Nothing
    Mmaala -> Nothing
    Mmen -> Nothing
    Moabite -> Nothing
    Moba -> Nothing
    Mobilian -> Nothing
    MobumrinAizi -> Nothing
    MobwaKaren -> Just $ ISO_639_2 "kar"
    Mocheno -> Nothing
    Mochi -> Nothing
    Mochica -> Nothing
    Mocho -> Nothing
    Mocovi -> Nothing
    Moda -> Nothing
    Modang -> Nothing
    Modole -> Nothing
    Moere -> Nothing
    MofuGudur -> Nothing
    Mogholi -> Nothing
    Mogum -> Nothing
    Mohave -> Nothing
    Mohawk -> Just $ ISO_639_2 "moh"
    MoheganMontaukNarragansett -> Nothing
    MoheganPequot -> Nothing
    Moi_Congo -> Nothing
    Moi_Indonesia -> Nothing
    Moikodi -> Nothing
    Moinba -> Nothing
    Moingi -> Nothing
    Moji -> Nothing
    Mok -> Nothing
    Moken -> Nothing
    Mokerang -> Nothing
    Mokilese -> Nothing
    Moklen -> Nothing
    Mokole -> Nothing
    Mokpwe -> Nothing
    Moksela -> Nothing
    Moksha -> Just $ ISO_639_2 "mdf"
    Molale -> Nothing
    Molbog -> Nothing
    Moldavian -> Just $ ISO_639_2 "mol"
    MoldovaSignLanguage -> Nothing
    Molengue -> Nothing
    Molima -> Nothing
    MolmoOne -> Nothing
    Molo -> Nothing
    Molof -> Nothing
    Moloko -> Nothing
    MomJango -> Nothing
    Moma -> Nothing
    Momare -> Nothing
    MomboDogon -> Nothing
    Mombum -> Nothing
    Momina -> Nothing
    Momuna -> Nothing
    Mon -> Nothing
    MonasticSignLanguage -> Nothing
    Monde -> Nothing
    Mondropolon -> Nothing
    Mongo -> Just $ ISO_639_2 "lol"
    Mongol -> Nothing
    MongoliaBuriat -> Just $ ISO_639_2 "bua"
    Mongolian -> Just $ ISO_639_2 "mon"
    MongolianSignLanguage -> Nothing
    Mongondow -> Nothing
    Moni -> Nothing
    Monimbo -> Nothing
    Mono_Cameroon -> Nothing
    Mono_DemocraticRepublicOfCongo -> Nothing
    Mono_SolomonIslands -> Nothing
    Mono_Usa -> Nothing
    Monom -> Nothing
    MonsangNaga -> Nothing
    Montagnais -> Nothing
    Montenegrin -> Just $ ISO_639_2 "cnr"
    Montol -> Nothing
    Monumbo -> Nothing
    Monzombo -> Nothing
    Moo -> Nothing
    MopanMaya -> Nothing
    Mor_BomberaiPeninsula -> Nothing
    Mor_MorIslands -> Nothing
    Moraid -> Nothing
    Morawa -> Nothing
    MorelosNahuatl -> Nothing
    Morerebi -> Nothing
    Moresada -> Nothing
    MoriAtas -> Nothing
    MoriBawah -> Nothing
    Morigi -> Nothing
    Moriori -> Nothing
    Morisyen -> Nothing
    Moro -> Nothing
    MoroccanSignLanguage -> Nothing
    Morokodo -> Nothing
    Moronene -> Nothing
    Morori -> Nothing
    Morouas -> Nothing
    Mortlockese -> Nothing
    Moru -> Nothing
    Mosimo -> Nothing
    Mosina -> Nothing
    Mosiro -> Nothing
    Moskona -> Nothing
    Mossi -> Just $ ISO_639_2 "mos"
    Mota -> Nothing
    Motlav -> Nothing
    Motu -> Nothing
    MoukAria -> Nothing
    MoundadanChetty -> Nothing
    MountainKoiali -> Nothing
    Mouwase -> Nothing
    Movima -> Nothing
    MoyadanItneg -> Nothing
    MoyonNaga -> Nothing
    MozambicanSignLanguage -> Nothing
    Mozarabic -> Nothing
    Mpade -> Nothing
    Mpalitjanh -> Nothing
    Mpi -> Nothing
    Mpiemo -> Nothing
    Mpinda -> Nothing
    Mpongmpong -> Nothing
    Mpoto -> Nothing
    Mpotovoro -> Nothing
    Mpuono -> Nothing
    Mpur -> Nothing
    MroChin -> Nothing
    Mru -> Nothing
    Mser -> Nothing
    MtIrigaAgta -> Nothing
    MuakSaAak -> Nothing
    Mualang -> Nothing
    Mubami -> Nothing
    Mubi -> Nothing
    Muda -> Nothing
    Mudbura -> Nothing
    Mudburra -> Nothing
    MudhiliGadaba -> Nothing
    MuduKoraga -> Nothing
    Muduapa -> Nothing
    Muduga -> Nothing
    Mufian -> Nothing
    Mugom -> Nothing
    Muinane -> Nothing
    MujiYi -> Nothing
    MukhaDora -> Nothing
    MukoMuko -> Nothing
    Mukulu -> Nothing
    Mulaha -> Nothing
    Mulam -> Nothing
    Mulao -> Nothing
    MulluKurumba -> Nothing
    Mullukmulluk -> Nothing
    Muluridyi -> Nothing
    Mum -> Nothing
    Mumuye -> Nothing
    MunChin -> Nothing
    Muna -> Nothing
    Munda -> Nothing
    Mundabli -> Nothing
    Mundang -> Nothing
    Mundani -> Nothing
    Mundari -> Nothing
    Mundat -> Nothing
    Mundu -> Nothing
    Munduruku -> Nothing
    Mungaka -> Nothing
    Munggui -> Nothing
    Muniche -> Nothing
    Munit -> Nothing
    Munji -> Nothing
    Munkip -> Nothing
    Munsee -> Nothing
    Muong -> Nothing
    MurPano -> Nothing
    Muratayak -> Nothing
    Murik -> Nothing
    MurikKayan -> Nothing
    Murkim -> Nothing
    Murle -> Nothing
    MurrinhPatha -> Nothing
    Mursi -> Nothing
    MuruiHuitoto -> Nothing
    Murupi -> Nothing
    Muruwari -> Nothing
    Musak -> Nothing
    Musan -> Nothing
    Musar -> Nothing
    Musasa -> Nothing
    Musey -> Nothing
    Musgu -> Nothing
    Mushungulu -> Nothing
    Musi -> Nothing
    Muskum -> Nothing
    MuslimTat -> Nothing
    Musom -> Nothing
    MussauEmira -> Nothing
    Muthuvan -> Nothing
    Mutu -> Nothing
    Muya -> Nothing
    Muyang -> Nothing
    Muyuw -> Nothing
    Muzi -> Nothing
    Mvanip -> Nothing
    Mvuba -> Nothing
    Mwaghavul -> Nothing
    MwaliComorian -> Nothing
    Mwan -> Nothing
    Mwani -> Nothing
    Mwatebu -> Nothing
    Mwera_Chimwera -> Nothing
    Mwera_Nyasa -> Nothing
    MwimbiMuthambi -> Nothing
    MyanmarSignLanguage -> Nothing
    MycenaeanGreek -> Nothing
    Myene -> Nothing
    Mysian -> Nothing
    MziemeNaga -> Nothing
    Na -> Nothing
    NaMeo -> Nothing
    Naaba -> Nothing
    Naasioi -> Nothing
    Naba -> Nothing
    Nabak -> Nothing
    Nabi -> Nothing
    Nachhiring -> Nothing
    Nadeb -> Nothing
    Nadruvian -> Nothing
    Nafaanra -> Nothing
    Nafi -> Nothing
    Nafri -> Nothing
    Nafusi -> Nothing
    NagaPidgin -> Nothing
    Nagarchal -> Nothing
    Nage -> Nothing
    Nagumi -> Nothing
    Nahali -> Nothing
    Nahari -> Nothing
    Nahuatl_HighlandPuebla -> Nothing
    Nai -> Nothing
    Nakaela -> Nothing
    Nakai -> Nothing
    Nakama -> Nothing
    Nakanai -> Nothing
    Nakara -> Nothing
    Nake -> Nothing
    Naki -> Nothing
    Nakwi -> Nothing
    Nalca -> Nothing
    Nali -> Nothing
    Nalik -> Nothing
    Nalogo -> Nothing
    Nalu -> Nothing
    NaluoYi -> Nothing
    Nama_Namibia -> Nothing
    Nama_PapuaNewGuinea -> Nothing
    Namakura -> Nothing
    Namat -> Nothing
    Nambo -> Nothing
    Nambya -> Nothing
    Namia -> Nothing
    Namiae -> Nothing
    NamibianSignLanguage -> Nothing
    Namla -> Nothing
    Namo -> Nothing
    Namonuito -> Nothing
    NamosiNaitasiriSerua -> Nothing
    Namuyi -> Nothing
    Nanai -> Nothing
    Nancere -> Nothing
    Nande -> Nothing
    Nandi -> Nothing
    NanerigeSenoufo -> Nothing
    NangaDamaDogon -> Nothing
    Nanggu -> Nothing
    Nangikurrunggurr -> Nothing
    Nankina -> Nothing
    Nanti -> Nothing
    Nanticoke -> Nothing
    Nanubae -> Nothing
    NapoLowlandQuechua -> Nothing
    Napu -> Nothing
    NarPhu -> Nothing
    Nara -> Nothing
    Narak -> Nothing
    Narango -> Nothing
    Narau -> Nothing
    NariNari -> Nothing
    Narim -> Nothing
    Naro -> Nothing
    Narom -> Nothing
    Narragansett -> Nothing
    Narrinyeri -> Nothing
    Narua -> Nothing
    Narungga -> Nothing
    Nasal -> Nothing
    Nasarian -> Nothing
    Naskapi -> Nothing
    Natagaimas -> Nothing
    Natanzi -> Nothing
    NataoranAmis -> Nothing
    Natchez -> Nothing
    Nateni -> Nothing
    Nathembo -> Nothing
    Natioro -> Nothing
    Natugu -> Nothing
    Nauete -> Nothing
    NaukanYupik -> Nothing
    Nauna -> Nothing
    Nauo -> Nothing
    Nauru -> Just $ ISO_639_2 "nau"
    Navajo -> Just $ ISO_639_2 "nav"
    NavarroLabourdinBasque -> Nothing
    Navut -> Nothing
    Nawaru -> Nothing
    Nawathinehena -> Nothing
    Nawdm -> Nothing
    Nawuri -> Nothing
    Naxi -> Nothing
    Nayi -> Nothing
    Nayini -> Nothing
    Ncane -> Nothing
    Nchumbulu -> Nothing
    Ndai -> Nothing
    Ndaka -> Nothing
    Ndaktup -> Nothing
    Ndali -> Nothing
    Ndam -> Nothing
    Ndamba -> Nothing
    Ndambomo -> Nothing
    Ndanda -> Nothing
    Ndasa -> Nothing
    Ndau -> Nothing
    NdeGbite -> Nothing
    NdeNseleNta -> Nothing
    Ndemli -> Nothing
    Ndendeule -> Nothing
    Ndengereko -> Nothing
    Nding -> Nothing
    Ndo -> Nothing
    Ndobo -> Nothing
    Ndoe -> Nothing
    Ndogo -> Nothing
    Ndolo -> Nothing
    Ndom -> Nothing
    Ndombe -> Nothing
    NdondeHamba -> Nothing
    Ndonga -> Just $ ISO_639_2 "ndo"
    Ndoola -> Nothing
    Ndragngith -> Nothing
    Nduga -> Nothing
    Ndumu -> Nothing
    Ndun -> Nothing
    Ndunda -> Nothing
    Ndunga -> Nothing
    Ndut -> Nothing
    Ndwewe -> Nothing
    NdyukaTrioPidgin -> Nothing
    NdzwaniComorian -> Nothing
    Neapolitan -> Just $ ISO_639_2 "nap"
    NebajIxil -> Nothing
    Nedebang -> Nothing
    Nefamese -> Nothing
    Negerhollands -> Nothing
    NegeriSembilanMalay -> Nothing
    Negidal -> Nothing
    Nehan -> Nothing
    Nek -> Nothing
    Nekgini -> Nothing
    Neko -> Nothing
    Neku -> Nothing
    Neme -> Nothing
    Nemi -> Nothing
    Nen -> Nothing
    Nend -> Nothing
    Nenets -> Nothing
    Nengone -> Nothing
    Neo -> Nothing
    NeoHittite -> Just $ ISO_639_2 "hit"
    NepaleseSignLanguage -> Nothing
    Nepali -> Just $ ISO_639_2 "nep"
    NepaliKurux -> Nothing
    Nepali_IndividualLanguage -> Just $ ISO_639_2 "nep"
    Nete -> Nothing
    NewCaledonianJavanese -> Just $ ISO_639_2 "jav"
    NewGreek -> Just $ ISO_639_2 "gre"
    NewZealandSignLanguage -> Nothing
    Newari -> Just $ ISO_639_2 "new"
    Neyo -> Nothing
    NezPerce -> Nothing
    NgaLa -> Nothing
    Ngaanyatjarra -> Nothing
    Ngabere -> Nothing
    Ngada -> Nothing
    Ngadjunmaya -> Nothing
    Ngadjuri -> Nothing
    Ngaing -> Nothing
    Ngaju -> Nothing
    Ngala -> Nothing
    Ngalakan -> Nothing
    Ngalkbun -> Nothing
    Ngalum -> Nothing
    Ngam -> Nothing
    Ngamambo -> Nothing
    Ngambay -> Nothing
    Ngamini -> Nothing
    Ngamo -> Nothing
    Nganakarti -> Nothing
    Nganasan -> Nothing
    Ngandi -> Nothing
    Ngando_CentralAfricanRepublic -> Nothing
    Ngando_DemocraticRepublicOfCongo -> Nothing
    Ngandyera -> Just $ ISO_639_2 "kua"
    Ngangam -> Nothing
    Ngantangarra -> Nothing
    Nganyaywana -> Nothing
    Ngardi -> Nothing
    Ngarigu -> Nothing
    Ngarinman -> Nothing
    Ngarinyin -> Nothing
    Ngarla -> Nothing
    Ngarluma -> Nothing
    Ngas -> Nothing
    Ngasa -> Nothing
    NgatikMensCreole -> Nothing
    NgawnChin -> Nothing
    Ngawun -> Nothing
    Ngayawung -> Nothing
    NgazidjaComorian -> Nothing
    Ngbaka -> Nothing
    NgbakaMabo -> Nothing
    NgbakaManza -> Nothing
    Ngbee -> Nothing
    Ngbinda -> Nothing
    Ngbundu -> Nothing
    Ngelima -> Nothing
    Ngemba -> Nothing
    Ngen -> Nothing
    Ngendelengo -> Nothing
    Ngeq -> Nothing
    Ngete -> Nothing
    Nggem -> Nothing
    Nggwahyi -> Nothing
    Ngie -> Nothing
    Ngiemboon -> Nothing
    Ngile -> Nothing
    Ngindo -> Nothing
    Ngiti -> Nothing
    Ngizim -> Nothing
    NgkalmpwKanum -> Nothing
    Ngom -> Nothing
    Ngomba -> Nothing
    Ngombale -> Nothing
    Ngombe_CentralAfricanRepublic -> Nothing
    Ngombe_DemocraticRepublicOfCongo -> Nothing
    Ngong -> Nothing
    Ngongo -> Nothing
    Ngoni -> Nothing
    Ngoni_Mozambique -> Nothing
    Ngoni_Tanzania -> Nothing
    Ngoshie -> Nothing
    Ngul -> Nothing
    Ngulu -> Nothing
    Nguluwan -> Nothing
    Ngumba -> Nothing
    Ngumbarl -> Nothing
    Ngumbi -> Nothing
    Ngunawal -> Nothing
    Ngundi -> Nothing
    Ngundu -> Nothing
    Ngungwel -> Nothing
    Nguon -> Nothing
    Ngura -> Nothing
    Ngurimi -> Nothing
    Ngurmbur -> Nothing
    Ngwaba -> Nothing
    Ngwe -> Nothing
    Ngwo -> Nothing
    Nhanda -> Nothing
    Nhengatu -> Nothing
    Nhirrpi -> Nothing
    Nhuwala -> Nothing
    Nias -> Just $ ISO_639_2 "nia"
    NicaraguaCreoleEnglish -> Nothing
    NicaraguanSignLanguage -> Nothing
    Niellim -> Nothing
    NigeriaMambila -> Nothing
    NigerianPidgin -> Nothing
    NigerianSignLanguage -> Nothing
    Nihali -> Nothing
    Nii -> Nothing
    Nijadali -> Nothing
    Niksek -> Nothing
    Nila -> Nothing
    Nilamba -> Nothing
    Nimadi -> Nothing
    Nimanbur -> Nothing
    Nimbari -> Nothing
    Nimboran -> Nothing
    Nimi -> Nothing
    Nimo -> Nothing
    Nimoa -> Nothing
    Ninam -> Nothing
    Nindi -> Nothing
    Ningera -> Nothing
    Ninggerum -> Nothing
    Ningil -> Nothing
    Ningye -> Nothing
    NiniaYali -> Nothing
    Ninzo -> Nothing
    Nipsan -> Nothing
    Nisa -> Nothing
    Nisenan -> Nothing
    Nisgaa -> Nothing
    Nisi -> Nothing
    Niuafoou -> Nothing
    Niuatoputapu -> Nothing
    Niuean -> Just $ ISO_639_2 "niu"
    Nivacle -> Nothing
    NiwerMil -> Nothing
    Njalgulgule -> Nothing
    Njebi -> Nothing
    Njen -> Nothing
    Njerep -> Nothing
    Njyem -> Nothing
    Nkami -> Nothing
    Nkangala -> Nothing
    Nkari -> Nothing
    NkemNkum -> Nothing
    Nkhumbi -> Nothing
    Nko_Nko -> Just $ ISO_639_2 "nqo"
    Nkongho -> Nothing
    Nkonya -> Nothing
    Nkoroo -> Nothing
    Nkoya -> Nothing
    Nkukoli -> Nothing
    Nkutu -> Nothing
    Nnam -> Nothing
    Nobiin -> Nothing
    Nobonob -> Nothing
    Nocaman -> Nothing
    NocteNaga -> Nothing
    Nogai -> Just $ ISO_639_2 "nog"
    Noipx -> Nothing
    Noiri -> Nothing
    Nokuku -> Nothing
    Nomaande -> Nothing
    Nomane -> Nothing
    Nomatsiguenga -> Nothing
    Nomlaki -> Nothing
    Nomu -> Nothing
    NongZhuang -> Nothing
    Nonuya -> Nothing
    Nooksack -> Nothing
    Noon -> Nothing
    Noone -> Nothing
    Nootka -> Nothing
    NopalaChatino -> Nothing
    Noric -> Nothing
    Norn -> Nothing
    Norra -> Nothing
    NorthAlaskanInupiatun -> Just $ ISO_639_2 "ipk"
    NorthAmbrym -> Nothing
    NorthAsmat -> Nothing
    NorthAwyu -> Nothing
    NorthBabar -> Nothing
    NorthBolivianQuechua -> Nothing
    NorthCentralMixe -> Nothing
    NorthEfate -> Nothing
    NorthFali -> Nothing
    NorthGiziga -> Nothing
    NorthJuninQuechua -> Nothing
    NorthKorowai -> Nothing
    NorthMarquesan -> Nothing
    NorthMofu -> Nothing
    NorthMoluccanMalay -> Nothing
    NorthMuyu -> Nothing
    NorthNdebele -> Just $ ISO_639_2 "nde"
    NorthNuaulu -> Nothing
    NorthPicene -> Nothing
    NorthSlavey -> Just $ ISO_639_2 "den"
    NorthTairora -> Nothing
    NorthTanna -> Nothing
    NorthTugen -> Nothing
    NorthWahgi -> Nothing
    NorthWatut -> Nothing
    NorthWemale -> Nothing
    NortheastKiwai -> Nothing
    NortheastMaidu -> Nothing
    NortheastPashayi -> Nothing
    NortheasternDianHmong -> Nothing
    NortheasternDinka -> Just $ ISO_639_2 "din"
    NortheasternPomo -> Nothing
    NortheasternThai -> Nothing
    NorthernAmamiOshima -> Nothing
    NorthernBai -> Nothing
    NorthernBetsimisarakaMalagasy -> Just $ ISO_639_2 "mlg"
    NorthernBoboMadare -> Nothing
    NorthernBontok -> Nothing
    NorthernCakchiquel -> Nothing
    NorthernCatanduanesBicolano -> Nothing
    NorthernConchucosAncashQuechua -> Nothing
    NorthernDagara -> Nothing
    NorthernDong -> Nothing
    NorthernEmbera -> Nothing
    NorthernFrisian -> Just $ ISO_639_2 "frr"
    NorthernGhale -> Nothing
    NorthernGondi -> Just $ ISO_639_2 "gon"
    NorthernGrebo -> Just $ ISO_639_2 "grb"
    NorthernGuiyangHmong -> Nothing
    NorthernHaida -> Just $ ISO_639_2 "hai"
    NorthernHindko -> Nothing
    NorthernHuishuiHmong -> Nothing
    NorthernKalapuya -> Nothing
    NorthernKankanay -> Nothing
    NorthernKatang -> Nothing
    NorthernKhmer -> Nothing
    NorthernKissi -> Nothing
    NorthernKurdish -> Nothing
    NorthernLenduNdrulo -> Nothing
    NorthernLorung -> Nothing
    NorthernLuri -> Nothing
    NorthernMam -> Nothing
    NorthernMashanHmong -> Nothing
    NorthernMuji -> Nothing
    NorthernNambikuara -> Nothing
    NorthernNgbandi -> Nothing
    NorthernNuni -> Nothing
    NorthernOaxacaNahuatl -> Nothing
    NorthernOne -> Nothing
    NorthernPaiute -> Nothing
    NorthernPame -> Nothing
    NorthernPashto -> Nothing
    NorthernPastazaQuichua -> Nothing
    NorthernPinghuaNorthernPingChinese -> Nothing
    NorthernPomo -> Nothing
    NorthernPueblaNahuatl -> Nothing
    NorthernPumi -> Nothing
    NorthernQiandongHmong -> Nothing
    NorthernQiang -> Nothing
    NorthernRengmaNaga -> Nothing
    NorthernRoglai -> Nothing
    NorthernSami -> Just $ ISO_639_2 "sme"
    NorthernSierraMiwok -> Nothing
    NorthernSubanen -> Nothing
    NorthernTarahumara -> Nothing
    NorthernTepehuan -> Nothing
    NorthernTidung -> Nothing
    NorthernTiwa -> Nothing
    NorthernTlaxiacoMixtec -> Nothing
    NorthernToussian -> Nothing
    NorthernTujia -> Nothing
    NorthernTutchone -> Nothing
    NorthernUzbek -> Just $ ISO_639_2 "uzb"
    NorthernYukaghir -> Nothing
    NorthernZhuang -> Just $ ISO_639_2 "zha"
    NorthwestAlaskaInupiatun -> Just $ ISO_639_2 "ipk"
    NorthwestGbaya -> Nothing
    NorthwestOaxacaMixtec -> Nothing
    NorthwestPashayi -> Nothing
    Northwest_KonkowMaidu -> Nothing
    NorthwesternDinka -> Just $ ISO_639_2 "din"
    NorthwesternFars -> Nothing
    NorthwesternKolami -> Nothing
    NorthwesternNisu -> Nothing
    NorthwesternOjibwa -> Nothing
    NorthwesternTamang -> Nothing
    Norwegian -> Just $ ISO_639_2 "nor"
    NorwegianBokmal -> Just $ ISO_639_2 "nob"
    NorwegianNynorsk -> Just $ ISO_639_2 "nno"
    NorwegianSignLanguage -> Nothing
    Notre -> Nothing
    Notsi -> Nothing
    Nottoway -> Nothing
    NottowayMeherrin -> Nothing
    Novial -> Nothing
    Noy -> Nothing
    Nsari -> Nothing
    Nsenga -> Nothing
    Nshi -> Nothing
    Nsongo -> Nothing
    Ntcham -> Nothing
    Nteng -> Nothing
    Ntomba -> Nothing
    Nu -> Nothing
    Nubaca -> Nothing
    Nubi -> Nothing
    Nubri -> Nothing
    Nuer -> Nothing
    Nugunu_Australia -> Nothing
    Nugunu_Cameroon -> Nothing
    Nuk -> Nothing
    NukakMaku -> Nothing
    Nukna -> Nothing
    Nukuini -> Nothing
    Nukumanu -> Nothing
    Nukunul -> Nothing
    Nukuoro -> Nothing
    Nukuria -> Nothing
    NumanaNunkuGbantuNumbu -> Nothing
    Numanggang -> Nothing
    Numbami -> Nothing
    Nume -> Nothing
    Numee -> Nothing
    Numidian -> Nothing
    Nung_Burma -> Nothing
    Nung_VietNam -> Nothing
    Nungali -> Nothing
    Nunggubuyu -> Nothing
    Nungu -> Nothing
    Nupbikha -> Nothing
    NupeNupeTako -> Nothing
    NupodeHuitoto -> Nothing
    NusaLaut -> Nothing
    Nusu -> Nothing
    NuuChahNulth -> Nothing
    Nyabwa -> Nothing
    Nyadu -> Nothing
    Nyaheun -> Nothing
    Nyahkur -> Nothing
    Nyakyusa -> Nothing
    Nyalayu -> Nothing
    Nyali -> Nothing
    Nyam -> Nothing
    Nyamal -> Nothing
    Nyambo -> Nothing
    NyamusaMolo -> Nothing
    Nyamwanga -> Nothing
    Nyamwezi -> Just $ ISO_639_2 "nym"
    Nyaneka -> Nothing
    Nyanga -> Nothing
    NyangaLi -> Nothing
    Nyangatom -> Nothing
    Nyangbo -> Nothing
    Nyangga -> Nothing
    Nyangi -> Nothing
    Nyangumarta -> Nothing
    Nyankole -> Just $ ISO_639_2 "nyn"
    NyarafoloSenoufo -> Nothing
    Nyaturu -> Nothing
    Nyaw -> Nothing
    Nyawaygi -> Nothing
    Nyemba -> Nothing
    Nyeng -> Nothing
    Nyengo -> Nothing
    Nyenkha -> Nothing
    Nyeu -> Nothing
    Nyigina -> Nothing
    Nyiha -> Nothing
    Nyiha_Malawi -> Nothing
    Nyika_MalawiAndZambia -> Nothing
    Nyika_Tanzania -> Nothing
    Nyindrou -> Nothing
    Nyindu -> Nothing
    Nyishi -> Nothing
    Nyiyaparli -> Nothing
    Nyokon -> Nothing
    Nyole -> Nothing
    Nyong -> Nothing
    Nyore -> Nothing
    Nyoro -> Just $ ISO_639_2 "nyo"
    Nyulnyul -> Nothing
    Nyungah -> Nothing
    Nyungwe -> Nothing
    Nzadi -> Nothing
    Nzakambay -> Nothing
    Nzakara -> Nothing
    Nzanyi -> Nothing
    Nzima -> Just $ ISO_639_2 "nzi"
    Obanliku -> Nothing
    Obispeno -> Nothing
    Oblo -> Nothing
    OboManobo -> Nothing
    Obokuitai -> Nothing
    Obolo -> Nothing
    Obulom -> Nothing
    Ocaina -> Nothing
    Occidental -> Just $ ISO_639_2 "ile"
    Occitan -> Just $ ISO_639_2 "oci"
    Ochichi -> Nothing
    OcotepecMixtec -> Nothing
    OcotlanZapotec -> Nothing
    Od -> Nothing
    Odiai -> Nothing
    Odoodee -> Nothing
    Odu -> Nothing
    Odual -> Nothing
    Odut -> Nothing
    Ofaye -> Nothing
    Ofo -> Nothing
    Ogan -> Nothing
    Ogbah -> Nothing
    Ogbia -> Nothing
    Ogbogolo -> Nothing
    Ogbronuagum -> Nothing
    Ogea -> Nothing
    Ohlone_Northern -> Nothing
    Ohlone_Southern -> Nothing
    Oirata -> Nothing
    Ojibwa -> Just $ ISO_639_2 "oji"
    OjitlanChinantec -> Nothing
    Okanagan -> Nothing
    OkiNoErabu -> Nothing
    Okiek -> Nothing
    OkoEniOsayen -> Nothing
    OkoJuwoi -> Nothing
    Okobo -> Nothing
    Okodia -> Nothing
    Okolie -> Nothing
    Okolod -> Nothing
    Okpamheri -> Nothing
    Okpe_NorthwesternEdo -> Nothing
    Okpe_SouthwesternEdo -> Nothing
    Oksapmin -> Nothing
    Oku -> Nothing
    OldAramaic -> Nothing
    OldAvar -> Just $ ISO_639_2 "ava"
    OldBreton -> Just $ ISO_639_2 "bre"
    OldBurmese -> Nothing
    OldCham -> Nothing
    OldChinese -> Nothing
    OldChurchSlavonic -> Just $ ISO_639_2 "chu"
    OldCornish -> Just $ ISO_639_2 "cor"
    OldDutch -> Nothing
    OldFrench -> Just $ ISO_639_2 "fro"
    OldFrisian -> Nothing
    OldGeorgian -> Nothing
    OldHighGerman -> Just $ ISO_639_2 "goh"
    OldHittite -> Just $ ISO_639_2 "hit"
    OldHungarian -> Nothing
    OldIrish -> Just $ ISO_639_2 "sga"
    OldJapanese -> Nothing
    OldKentishSignLanguage -> Nothing
    OldKhmer -> Nothing
    OldKorean -> Nothing
    OldLithuanian -> Just $ ISO_639_2 "lit"
    OldMalay -> Nothing
    OldManipuri -> Just $ ISO_639_2 "mni"
    OldMarathi -> Just $ ISO_639_2 "mar"
    OldMon -> Nothing
    OldNorse -> Just $ ISO_639_2 "non"
    OldNubian -> Nothing
    OldOssetic -> Nothing
    OldPersian -> Just $ ISO_639_2 "peo"
    OldProvencal -> Just $ ISO_639_2 "pro"
    OldRussian -> Nothing
    OldSaxon -> Nothing
    OldSpanish -> Nothing
    OldSundanese -> Nothing
    OldTamil -> Nothing
    OldTibetan -> Nothing
    OldTurkish -> Nothing
    OldUighur -> Nothing
    OldWelsh -> Nothing
    Olekha -> Nothing
    Olkol -> Nothing
    Olo -> Nothing
    Oloma -> Nothing
    Olrat -> Nothing
    Olubo -> Nothing
    OlulumoIkom -> Nothing
    OlutaPopoluca -> Nothing
    Omagua -> Nothing
    OmahaPonca -> Nothing
    Omati -> Nothing
    Ombamba -> Nothing
    Ombo -> Nothing
    Omejes -> Nothing
    OmetepecNahuatl -> Nothing
    Omi -> Nothing
    Omie -> Nothing
    Omok -> Nothing
    Omotik -> Nothing
    Omurano -> Nothing
    Ona -> Nothing
    Oneida -> Nothing
    Ong -> Nothing
    Onge -> Nothing
    Onin -> Nothing
    OninBasedPidgin -> Nothing
    Onjob -> Nothing
    Ono -> Nothing
    Onobasulu -> Nothing
    Onondaga -> Nothing
    Ontenu -> Nothing
    OntongJava -> Nothing
    Oorlams -> Nothing
    Opao -> Nothing
    Opata -> Nothing
    Opuuo -> Nothing
    OrangKanaq -> Nothing
    OrangSeletar -> Nothing
    OraonSadri -> Nothing
    Orejon -> Nothing
    Oring -> Nothing
    Oriya -> Just $ ISO_639_2 "ori"
    Oriya_IndividualLanguage -> Just $ ISO_639_2 "ori"
    OrizabaNahuatl -> Nothing
    Orma -> Nothing
    Ormu -> Nothing
    Ormuri -> Nothing
    Oro -> Nothing
    OroWin -> Nothing
    Oroch -> Nothing
    Oroha -> Nothing
    Orok -> Nothing
    Orokaiva -> Nothing
    Oroko -> Nothing
    Orokolo -> Nothing
    Oromo -> Just $ ISO_639_2 "orm"
    Oroqen -> Nothing
    Orowe -> Nothing
    Oruma -> Nothing
    Orya -> Nothing
    Osage -> Just $ ISO_639_2 "osa"
    Osatu -> Nothing
    Oscan -> Nothing
    Osing -> Nothing
    Ososo -> Nothing
    Ossetian -> Just $ ISO_639_2 "oss"
    Otank -> Nothing
    Oti -> Nothing
    Otoro -> Nothing
    Ottawa -> Nothing
    OttomanTurkish -> Just $ ISO_639_2 "ota"
    Otuho -> Nothing
    Otuke -> Nothing
    Ouma -> Nothing
    Oune -> Nothing
    Oung -> Nothing
    OvdalianElfdalian -> Nothing
    Owa -> Nothing
    Owenia -> Nothing
    Owiniga -> Nothing
    OxchucTzeltal -> Nothing
    Oy -> Nothing
    Oyaoya -> Nothing
    Oyda -> Nothing
    OysterBayTasmanian -> Nothing
    OzolotepecZapotec -> Nothing
    OzumacinChinantec -> Nothing
    OzumatlanTotonac -> Nothing
    PaDi -> Nothing
    PaHng -> Nothing
    Paa -> Nothing
    Paafang -> Nothing
    Paama -> Nothing
    Paasaal -> Nothing
    Pacahuara -> Nothing
    PacaraosQuechua -> Nothing
    PacificGulfYupik -> Nothing
    Pacoh -> Nothing
    Padoe -> Nothing
    Paekche -> Nothing
    Paelignian -> Nothing
    Paez -> Nothing
    Pagi -> Nothing
    Pagibete -> Nothing
    Pagu -> Nothing
    PahananAgta -> Nothing
    Pahari -> Nothing
    PahariPotwari -> Nothing
    Pahi -> Nothing
    Pahlavani -> Nothing
    Pahlavi -> Just $ ISO_639_2 "pal"
    PaiTavytera -> Nothing
    Paici -> Nothing
    Paipai -> Nothing
    PaiteChin -> Nothing
    Paiwan -> Nothing
    PakTong -> Nothing
    Pakaasnovos -> Nothing
    Pakanha -> Nothing
    PakistanSignLanguage -> Nothing
    Paku -> Nothing
    PakuKaren -> Just $ ISO_639_2 "kar"
    Pal -> Nothing
    Palaic -> Nothing
    PalakaSenoufo -> Nothing
    PalantlaChinantec -> Nothing
    Palauan -> Just $ ISO_639_2 "pau"
    PalePalaung -> Nothing
    Palembang -> Nothing
    Paleni -> Nothing
    Palenquero -> Nothing
    Pali -> Just $ ISO_639_2 "pli"
    Palikur -> Nothing
    Paliyan -> Nothing
    Pallanganmiddang -> Nothing
    Palor -> Nothing
    Palpa -> Nothing
    Palu -> Nothing
    Paluan -> Nothing
    Palue -> Nothing
    Palumata -> Nothing
    PalyaBareli -> Nothing
    Pam -> Nothing
    Pambia -> Nothing
    Pamlico -> Nothing
    Pamona -> Nothing
    Pamosu -> Nothing
    Pampanga -> Just $ ISO_639_2 "pam"
    Pana_BurkinaFaso -> Nothing
    Pana_CentralAfricanRepublic -> Nothing
    PanamanianSignLanguage -> Nothing
    Panamint -> Nothing
    Panang -> Nothing
    PanaoHuanucoQuechua -> Nothing
    Panasuan -> Nothing
    Panawa -> Nothing
    Pancana -> Nothing
    Panchpargania -> Nothing
    Pande -> Nothing
    Pangasinan -> Just $ ISO_639_2 "pag"
    Pangseng -> Nothing
    PangutaranSama -> Nothing
    Pangwa -> Nothing
    Pangwali -> Nothing
    Panim -> Nothing
    Paniya -> Nothing
    Panjabi -> Just $ ISO_639_2 "pan"
    Pankarare -> Nothing
    Pankararu -> Nothing
    Pankhu -> Nothing
    Pannei -> Nothing
    PanoanKatukina -> Nothing
    Panobo -> Nothing
    Panytyima -> Nothing
    Pao -> Nothing
    PaoKaren -> Nothing
    PapantlaTotonac -> Nothing
    Papapana -> Nothing
    Papar -> Nothing
    Papasena -> Nothing
    Papavo -> Nothing
    Papel -> Nothing
    Papi -> Nothing
    Papiamento -> Just $ ISO_639_2 "pap"
    Papitalai -> Nothing
    Papora -> Nothing
    PapuaNewGuineanSignLanguage -> Nothing
    PapuanMalay -> Nothing
    Papuma -> Nothing
    ParaArara -> Nothing
    ParaGaviao -> Nothing
    ParaNaga -> Nothing
    Parachi -> Nothing
    ParaguayanGuarani -> Just $ ISO_639_2 "grn"
    ParaguayanSignLanguage -> Nothing
    Parakana -> Nothing
    Paranan -> Nothing
    Paranawat -> Nothing
    Paraujano -> Nothing
    Parauk -> Nothing
    Parawen -> Nothing
    Pardhan -> Nothing
    Pardhi -> Nothing
    Pare -> Nothing
    Parecis -> Nothing
    Parenga -> Nothing
    Pari -> Nothing
    ParkariKoli -> Nothing
    Parkwa -> Nothing
    Parthian -> Nothing
    Parya -> Nothing
    Pasi -> Nothing
    PassValleyYali -> Nothing
    Patamona -> Nothing
    Patani -> Nothing
    PataxoHahaai -> Nothing
    Patep -> Nothing
    Pathiya -> Nothing
    PatlaChicontlaTotonac -> Nothing
    Patpatar -> Nothing
    Pattani -> Nothing
    PattaniMalay -> Nothing
    Pattapu -> Nothing
    Patwin -> Nothing
    Paulohi -> Nothing
    Paumari -> Nothing
    Paunaka -> Nothing
    PauriBareli -> Nothing
    Pauserna -> Nothing
    Pawaia -> Nothing
    Pawnee -> Nothing
    Paynamar -> Nothing
    Pazeh -> Nothing
    Pe -> Nothing
    Pear -> Nothing
    Pech -> Nothing
    Pecheneg -> Nothing
    PediNorthernSotho -> Just $ ISO_639_2 "nso"
    PeerapperNorthwesternTasmanian -> Nothing
    Peere -> Nothing
    Pei -> Nothing
    Pekal -> Nothing
    Pela -> Nothing
    PeleAta -> Nothing
    Pelende -> Nothing
    Pemon -> Nothing
    Pemono -> Nothing
    PenangSignLanguage -> Nothing
    Penchal -> Nothing
    Pendau -> Nothing
    Penesak -> Nothing
    Pengo -> Nothing
    PennsylvaniaGerman -> Nothing
    PenolesMixtec -> Nothing
    Penrhyn -> Nothing
    Pentlatch -> Nothing
    Perai -> Nothing
    PeranakanIndonesian -> Nothing
    PeripheralMongolian -> Nothing
    Pero -> Nothing
    Persian -> Nothing
    PersianSignLanguage -> Nothing
    Persian_Dari -> Nothing
    PeruvianSignLanguage -> Nothing
    Pesse -> Nothing
    PetapaZapotec -> Nothing
    Petats -> Nothing
    Petjo -> Nothing
    Peve -> Nothing
    Pfaelzisch -> Nothing
    Phai -> Nothing
    Phake -> Nothing
    Phala -> Nothing
    Phalura -> Nothing
    Phana -> Nothing
    Phangduwali -> Nothing
    Phende -> Nothing
    PhilippineSignLanguage -> Nothing
    Phimbi -> Nothing
    Phoenician -> Just $ ISO_639_2 "phn"
    Phola -> Nothing
    PhomNaga -> Nothing
    PhongKniang -> Nothing
    PhraePwoKaren -> Nothing
    Phrygian -> Nothing
    PhuThai -> Nothing
    Phuan -> Nothing
    Phudagi -> Nothing
    Phuie -> Nothing
    Phula -> Nothing
    Phuma -> Nothing
    Phunoi -> Nothing
    Phuong -> Nothing
    Phupa -> Nothing
    Phupha -> Nothing
    Phuza -> Nothing
    Piamatsina -> Nothing
    Piame -> Nothing
    Piapoco -> Nothing
    Piaroa -> Nothing
    Picard -> Nothing
    PichisAsheninka -> Nothing
    Pictish -> Nothing
    PidginDelaware -> Nothing
    Piedmontese -> Nothing
    Pijao -> Nothing
    Pije -> Nothing
    Pijin -> Nothing
    Pilaga -> Nothing
    Pileni -> Nothing
    PimaBajo -> Nothing
    Pimbwe -> Nothing
    PinaiHagahai -> Nothing
    Pingelapese -> Nothing
    Pini -> Nothing
    Pinigura -> Nothing
    Pinjarup -> Nothing
    Pinji -> Nothing
    PinotepaNacionalMixtec -> Nothing
    Pintiini -> Nothing
    PintupiLuritja -> Nothing
    Pinyin -> Nothing
    Pipil -> Nothing
    Piraha -> Nothing
    Piratapuyo -> Nothing
    Pirlatapa -> Nothing
    Piro -> Nothing
    Pirriya -> Nothing
    Piru -> Nothing
    Pisabo -> Nothing
    PisafloresTepehua -> Nothing
    Piscataway -> Nothing
    Pisidian -> Nothing
    PitcairnNorfolk -> Nothing
    PiteSami -> Nothing
    Piti -> Nothing
    Pitjantjatjara -> Nothing
    PittaPitta -> Nothing
    Piu -> Nothing
    PiyaKwonci -> Nothing
    PlainsIndianSignLanguage -> Nothing
    PlainsMiwok -> Nothing
    PlapoKrumen -> Nothing
    PlateauMalagasy -> Just $ ISO_639_2 "mlg"
    Plautdietsch -> Nothing
    Playero -> Nothing
    Pnar -> Nothing
    PochuriNaga -> Nothing
    Pochutec -> Nothing
    Podena -> Nothing
    Pogolo -> Nothing
    Pohnpeian -> Just $ ISO_639_2 "pon"
    Pokanga -> Nothing
    Poke -> Nothing
    Pokoot -> Nothing
    Polabian -> Nothing
    Polari -> Nothing
    Polci -> Nothing
    Polish -> Just $ ISO_639_2 "pol"
    PolishSignLanguage -> Nothing
    Polonombauk -> Nothing
    Poluo_PholoYi -> Nothing
    Pom -> Nothing
    Pomo -> Nothing
    Ponam -> Nothing
    Ponares -> Nothing
    Pongu -> Nothing
    Pongyong -> Nothing
    Ponosakan -> Nothing
    Pontic -> Nothing
    PonyoGongwangNaga -> Nothing
    Porohanon -> Nothing
    PortSandwich -> Nothing
    PortSorellTasmanian -> Nothing
    PortVato -> Nothing
    Portuguese -> Just $ ISO_639_2 "por"
    PortugueseBasedCreolesAndPidgins -> Nothing
    PortugueseSignLanguage -> Nothing
    Potawatomi -> Nothing
    Potiguara -> Nothing
    PottangiOllarGadaba -> Nothing
    PoumeiNaga -> Nothing
    Pouye -> Nothing
    Powari -> Nothing
    Powhatan -> Nothing
    Poyanawa -> Nothing
    Prasuni -> Nothing
    Pray3 -> Nothing
    PrimitiveIrish -> Nothing
    Principense -> Nothing
    Provencal -> Nothing
    ProvidenciaSignLanguage -> Nothing
    Prussian -> Nothing
    Psikye -> Nothing
    PuKo -> Nothing
    PuXianChinese -> Nothing
    Puari -> Nothing
    Pubian -> Nothing
    PueblaMazatec -> Nothing
    Puelche -> Nothing
    PuertoRicanSignLanguage -> Nothing
    PuimeiNaga -> Nothing
    Puinave -> Nothing
    Pukapuka -> Nothing
    PulaYi -> Nothing
    Pulaar -> Nothing
    Pulabu -> Nothing
    Pular -> Nothing
    Puluwatese -> Nothing
    Puma -> Nothing
    Pume -> Nothing
    Pumpokol -> Nothing
    PunanAput -> Nothing
    PunanBahBiau -> Nothing
    PunanBatu1 -> Nothing
    PunanMerah -> Nothing
    PunanMerap -> Nothing
    PunanTubu -> Nothing
    Punic -> Nothing
    PunoQuechua -> Nothing
    Punthamara -> Nothing
    Punu -> Nothing
    Puoc -> Nothing
    Puquina -> Nothing
    Puragi -> Nothing
    Purari -> Nothing
    Purepecha -> Nothing
    Puri -> Nothing
    Purik -> Nothing
    Purisimeno -> Nothing
    Purubora -> Nothing
    Purum -> Nothing
    PurumNaga -> Nothing
    Pushto -> Just $ ISO_639_2 "pus"
    Putai -> Nothing
    Putoh -> Nothing
    Putukwam -> Nothing
    PuwaYi -> Nothing
    Puyo -> Nothing
    PuyoPaekche -> Nothing
    Puyuma -> Nothing
    Pwaamei -> Nothing
    Pwapwa -> Nothing
    PwoEasternKaren -> Nothing
    PwoNorthernKaren -> Nothing
    PwoWesternKaren -> Nothing
    Pyapun -> Nothing
    PyeKrumen -> Nothing
    PyemmairrenerNortheasternTasmanian -> Nothing
    Pyen -> Nothing
    Pyu -> Nothing
    Pyu_Burma -> Nothing
    Qabiao -> Nothing
    Qaqet -> Nothing
    Qashqai -> Nothing
    Qatabanian -> Nothing
    Qau -> Nothing
    Qawasqar -> Nothing
    QilaMuji -> Nothing
    Qimant -> Nothing
    QiubeiZhuang -> Nothing
    Quapaw -> Nothing
    QuebecSignLanguage -> Nothing
    Quechan -> Nothing
    Quechua -> Nothing
    Quenya -> Nothing
    QueretaroOtomi -> Nothing
    QuetzaltepecMixe -> Nothing
    Queyu -> Nothing
    QuiavicuzasZapotec -> Nothing
    Quileute -> Nothing
    Quinault -> Nothing
    Quinqui -> Nothing
    QuioquitaniQuieriZapotec -> Nothing
    QuiotepecChinantec -> Nothing
    Quiripi -> Nothing
    Rabha -> Nothing
    RabinalAchi -> Nothing
    Rade -> Nothing
    Raetic -> Nothing
    RaetoRomance -> Just $ ISO_639_2 "roh"
    Rahambuu -> Nothing
    RajahKabunsuwanManobo -> Nothing
    Rajasthani -> Nothing
    Rajbanshi -> Nothing
    Raji -> Nothing
    Rajong -> Nothing
    RajputGarasia -> Nothing
    RakahangaManihiki -> Nothing
    Rakhine -> Nothing
    Ralte -> Nothing
    Rama -> Nothing
    Ramoaaina -> Nothing
    Ramopa -> Nothing
    Rampi -> Nothing
    RanaTharu -> Nothing
    Ranau -> Nothing
    Rang -> Nothing
    Rangkas -> Nothing
    Ranglong -> Nothing
    Rangpuri -> Nothing
    Rao -> Nothing
    Rapa -> Nothing
    Rapanui -> Just $ ISO_639_2 "rap"
    Rapoisi -> Nothing
    Rapting -> Nothing
    Rarotongan -> Just $ ISO_639_2 "rar"
    Rasawa -> Nothing
    Ratagnon -> Nothing
    Ratahan -> Nothing
    Rathawi -> Nothing
    RathwiBareli -> Nothing
    Raute -> Nothing
    Ravula -> Nothing
    Rawa -> Nothing
    Rawang -> Nothing
    Rawas -> Nothing
    Rawat -> Nothing
    Rawo -> Nothing
    RayonZoque -> Nothing
    Razajerdi -> Nothing
    Razihi -> Nothing
    RedGelao -> Nothing
    Reel -> Nothing
    Rejang -> Nothing
    RejangKayan -> Nothing
    Reli -> Nothing
    Rema -> Nothing
    Rembarunga -> Nothing
    Rembong -> Nothing
    Remo -> Nothing
    RemontadoAgta -> Nothing
    Rempi -> Nothing
    Remun -> Nothing
    Rendille -> Nothing
    Rengao -> Nothing
    RennellBelona -> Nothing
    RennelleseSignLanguage -> Nothing
    Repanbitip -> Nothing
    RerBare -> Nothing
    Rerau -> Nothing
    Rerep -> Nothing
    Reshe -> Nothing
    Resigaro -> Nothing
    Retta -> Nothing
    ReunionCreoleFrench -> Nothing
    Reyesano -> Nothing
    Riang_India -> Nothing
    Riang_Myanmar -> Nothing
    Riantana -> Nothing
    Ribun -> Nothing
    Rien -> Nothing
    Rikbaktsa -> Nothing
    RinconZapotec -> Nothing
    Ringgou -> Nothing
    Ririo -> Nothing
    Rishiwa -> Nothing
    Ritarungo -> Nothing
    Riung -> Nothing
    RiverainSango -> Just $ ISO_639_2 "sag"
    Rogo -> Nothing
    Rohingya -> Nothing
    Roma -> Nothing
    Romagnol -> Nothing
    Romam -> Nothing
    Romanian -> Just $ ISO_639_2 "rum"
    RomanianSignLanguage -> Nothing
    RomanoGreek -> Nothing
    RomanoSerbian -> Nothing
    Romanova -> Nothing
    Romany -> Just $ ISO_639_2 "rom"
    Romblomanon -> Nothing
    Rombo -> Nothing
    Romkun -> Nothing
    Ron -> Nothing
    Ronga -> Nothing
    Rongga -> Nothing
    RongmeiNaga -> Nothing
    Rongpo -> Nothing
    Ronji -> Nothing
    Roon -> Nothing
    Roria -> Nothing
    Rotokas -> Nothing
    Rotuman -> Nothing
    Roviana -> Nothing
    Rudbari -> Nothing
    Rufiji -> Nothing
    Ruga -> Nothing
    Rukai -> Nothing
    Ruli -> Nothing
    Ruma -> Nothing
    RumaiPalaung -> Nothing
    Rumu -> Nothing
    Runa -> Nothing
    Rundi -> Just $ ISO_639_2 "run"
    Runga -> Nothing
    RungtuChin -> Nothing
    Rungus -> Nothing
    Rungwa -> Nothing
    RussiaBuriat -> Just $ ISO_639_2 "bua"
    Russian -> Just $ ISO_639_2 "rus"
    RussianSignLanguage -> Nothing
    Rusyn -> Nothing
    RuthenianRusyn -> Nothing
    Rutul -> Nothing
    Ruund -> Nothing
    Ruwila -> Nothing
    Rwa -> Nothing
    RwandanSignLanguage -> Nothing
    Sa -> Nothing
    Saa -> Nothing
    SaafiSaafi -> Nothing
    Saam -> Nothing
    Saamia -> Nothing
    Saaroa -> Nothing
    Saba -> Nothing
    Sabaean -> Nothing
    SabahBisaya -> Nothing
    SabahMalay -> Nothing
    Saban -> Nothing
    Sabanes -> Nothing
    Sabaot -> Nothing
    Sabine -> Nothing
    Sabu -> Nothing
    Sabum -> Nothing
    Sacapulteco -> Nothing
    Sadri -> Nothing
    Saek -> Nothing
    Saep -> Nothing
    Safaliba -> Nothing
    Safeyoka -> Nothing
    Safwa -> Nothing
    Sagala -> Nothing
    Sagalla -> Nothing
    Saho -> Nothing
    Sahu -> Nothing
    SaidiSpokenArabic -> Nothing
    SaintLucianCreoleFrench -> Nothing
    Saisiyat -> Nothing
    Sajalong -> Nothing
    SajauBasap -> Nothing
    SakalavaMalagasy -> Just $ ISO_639_2 "mlg"
    Sakam -> Nothing
    Sakan -> Just $ ISO_639_2 "kho"
    Sakao -> Nothing
    Sakata -> Nothing
    Sake -> Nothing
    Sakechep -> Nothing
    Sakha -> Just $ ISO_639_2 "sah"
    Sakirabia -> Nothing
    Sakizaya -> Nothing
    Sala -> Nothing
    Salampasu -> Nothing
    Salar -> Nothing
    Salas -> Nothing
    SalasacaHighlandQuichua -> Nothing
    Salchuq -> Nothing
    Saleman -> Nothing
    Saliba -> Nothing
    Salinan -> Nothing
    Sallands -> Nothing
    SaltYui -> Nothing
    Saluma -> Nothing
    SalvadoranSignLanguage -> Nothing
    Sam -> Nothing
    Sama -> Nothing
    Samaritan -> Nothing
    SamaritanAramaic -> Just $ ISO_639_2 "sam"
    Samarokena -> Nothing
    Samatao -> Nothing
    Samay -> Nothing
    Samba -> Nothing
    SambaDaka -> Nothing
    SambaLeko -> Nothing
    Sambalpuri -> Nothing
    Sambe -> Nothing
    Samberigi -> Nothing
    Samburu -> Nothing
    Samei -> Nothing
    Samo -> Nothing
    Samoan -> Just $ ISO_639_2 "smo"
    Samogitian -> Nothing
    Samosa -> Nothing
    Sampang -> Nothing
    Samre -> Nothing
    Samtao -> Nothing
    Samvedi -> Nothing
    SanAgustinMixtepecZapotec -> Nothing
    SanAndresLarrainzarTzotzil -> Nothing
    SanAndresQuiche -> Nothing
    SanBaltazarLoxichaZapotec -> Nothing
    SanBlasKuna -> Nothing
    SanDionisioDelMarHuave -> Nothing
    SanFelipeOtlaltepecPopoloca -> Nothing
    SanFranciscoDelMarHuave -> Nothing
    SanFranciscoMatlatzinca -> Nothing
    SanJeronimoTecoatlMazatec -> Nothing
    SanJuanAtzingoPopoloca -> Nothing
    SanJuanColoradoMixtec -> Nothing
    SanJuanCotzalIxil -> Nothing
    SanJuanGuelaviaZapotec -> Nothing
    SanJuanTeitaMixtec -> Nothing
    SanLuisPotosiHuastec -> Nothing
    SanLuisTemalacayucaPopoloca -> Nothing
    SanMarcosTlalcoyalcoPopoloca -> Nothing
    SanMartinItunyosoTriqui -> Nothing
    SanMartinQuechua -> Nothing
    SanMateoDelMarHuave -> Nothing
    SanMiguelCreoleFrench -> Nothing
    SanMiguelElGrandeMixtec -> Nothing
    SanMiguelPiedrasMixtec -> Nothing
    SanPedroQuiatoniZapotec -> Nothing
    SanSalvadorKongo -> Just $ ISO_639_2 "kon"
    SanVicenteCoatlanZapotec -> Nothing
    Sanapana -> Nothing
    Sandawe -> Just $ ISO_639_2 "sad"
    Sanga_DemocraticRepublicOfCongo -> Nothing
    Sanga_Nigeria -> Nothing
    SangabMandaya -> Nothing
    Sanggau -> Nothing
    Sangil -> Nothing
    Sangir -> Nothing
    Sangisari -> Nothing
    Sangkong -> Nothing
    Sanglechi -> Nothing
    SanglechiIshkashimi -> Nothing
    Sango -> Just $ ISO_639_2 "sag"
    SangtamNaga -> Nothing
    Sangu_Gabon -> Nothing
    Sangu_Tanzania -> Nothing
    SaniYi -> Nothing
    Sanie -> Nothing
    SaniyoHiyewe -> Nothing
    SankaranManinka -> Nothing
    Sansi -> Nothing
    Sanskrit -> Just $ ISO_639_2 "san"
    Sansu -> Nothing
    SantaAnaDeTusiPascoQuechua -> Nothing
    SantaCatarinaAlbarradasZapotec -> Nothing
    SantaCruz -> Nothing
    SantaInesAhuatempanPopoloca -> Nothing
    SantaInesYatzechiZapotec -> Nothing
    SantaLuciaMonteverdeMixtec -> Nothing
    SantaMariaDeJesusCakchiquel -> Nothing
    SantaMariaDelMarHuave -> Nothing
    SantaMariaLaAltaNahuatl -> Nothing
    SantaMariaQuiegolaniZapotec -> Nothing
    SantaMariaZacatepecMixtec -> Nothing
    SantaTeresaCora -> Nothing
    Santali -> Just $ ISO_639_2 "sat"
    SantiagoDelEsteroQuichua -> Nothing
    SantiagoLapaguiaZapotec -> Nothing
    SantiagoXanicaZapotec -> Nothing
    SantoDomingoAlbarradasZapotec -> Nothing
    SantoDomingoXenacojCakchiquel -> Nothing
    Sanuma -> Nothing
    Sanye -> Nothing
    SaoPauloKaingang -> Nothing
    Saoch -> Nothing
    Saotomense -> Nothing
    Saparua -> Nothing
    Sape -> Nothing
    Sapo -> Nothing
    Saponi -> Nothing
    Saposa -> Nothing
    Sapuan -> Nothing
    Sar -> Nothing
    Sara -> Nothing
    SaraDunjo -> Nothing
    SaraKaba -> Nothing
    Saraiki -> Nothing
    Saramaccan -> Nothing
    SaranganiBlaan -> Nothing
    SaranganiManobo -> Nothing
    Sarasira -> Nothing
    Saraveca -> Nothing
    SarawakBisaya -> Nothing
    Sardinian -> Just $ ISO_639_2 "srd"
    Sarikoli -> Nothing
    Sarli -> Nothing
    Sarsi -> Nothing
    Sartang -> Nothing
    Sarua -> Nothing
    Sarudu -> Nothing
    Saruga -> Nothing
    Sasak -> Just $ ISO_639_2 "sas"
    Sasaru -> Nothing
    SassareseSardinian -> Nothing
    Satawalese -> Nothing
    SatereMawe -> Nothing
    SaterlandFrisian -> Nothing
    SaudiArabianSignLanguage -> Nothing
    Sauk -> Nothing
    SauraseniPrakrit -> Nothing
    Saurashtra -> Nothing
    Sauri -> Nothing
    SauriaPaharia -> Nothing
    Sause -> Nothing
    Sausi -> Nothing
    Savara -> Nothing
    Savi -> Nothing
    Savosavo -> Nothing
    Sawai -> Nothing
    Saweru -> Nothing
    Sawi -> Nothing
    Sawila -> Nothing
    Sawknah -> Nothing
    SaxweGbe -> Nothing
    Saya -> Nothing
    SayulaPopoluca -> Nothing
    Scots -> Just $ ISO_639_2 "sco"
    Scythian -> Nothing
    SeaIslandCreoleEnglish -> Nothing
    Seba -> Nothing
    SebatBetGurage -> Nothing
    Seberuang -> Nothing
    SebobKenyah -> Nothing
    Sebuyau -> Just $ ISO_639_2 "iba"
    Sechelt -> Nothing
    Secoya -> Nothing
    Sedang -> Nothing
    Sedoa -> Nothing
    Seediq -> Nothing
    Seeku -> Nothing
    Segai -> Nothing
    Segeju -> Nothing
    Seget -> Nothing
    Sehwi -> Nothing
    Seimat -> Nothing
    SeitKaitetu -> Nothing
    Sekani -> Nothing
    Sekapan -> Nothing
    Sekar -> Nothing
    Seke_Nepal -> Nothing
    Seke_Vanuatu -> Nothing
    Seki -> Nothing
    SekoPadang -> Nothing
    SekoTengah -> Nothing
    Sekpele -> Nothing
    Selako -> Nothing
    SelangorSignLanguage -> Nothing
    Selaru -> Nothing
    Selayar -> Nothing
    Selee -> Nothing
    Selepet -> Nothing
    Selian -> Nothing
    Selkup -> Just $ ISO_639_2 "sel"
    SelungaiMurut -> Nothing
    Seluwasan -> Nothing
    Semai -> Nothing
    Semandang -> Nothing
    SemaqBeri -> Nothing
    SembakungMurut -> Nothing
    Semelai -> Nothing
    Semendo -> Nothing
    Semimi -> Nothing
    Semnam -> Nothing
    Semnani -> Nothing
    Sempan -> Nothing
    Sena -> Nothing
    SenaraSenoufo -> Nothing
    Senaya -> Nothing
    Sene -> Nothing
    Seneca -> Nothing
    Sened -> Nothing
    Sengele -> Nothing
    Senggi -> Nothing
    Sengo -> Nothing
    Sengseng -> Nothing
    SenhajaDeSrair -> Nothing
    Sensi -> Nothing
    Sentani -> Nothing
    SenthangChin -> Nothing
    Sentinel -> Nothing
    Sepa_Indonesia -> Nothing
    Sepa_PapuaNewGuinea -> Nothing
    Sepen -> Nothing
    SepikIwam -> Nothing
    Sera -> Nothing
    Serawai -> Nothing
    Serbian -> Just $ ISO_639_2 "srp"
    SerboCroatian -> Nothing
    Sere -> Nothing
    Serer -> Just $ ISO_639_2 "srr"
    Seri -> Nothing
    Serili -> Nothing
    Seroa -> Nothing
    Serrano -> Nothing
    Seru -> Nothing
    Serua -> Nothing
    SerudungMurut -> Nothing
    SeruiLaut -> Nothing
    SeselwaCreoleFrench -> Nothing
    Seta -> Nothing
    Setaman -> Nothing
    Seti -> Nothing
    Settla -> Nothing
    SevernOjibwa -> Nothing
    SewaBay -> Nothing
    SeychellesSignLanguage -> Nothing
    Seze -> Nothing
    SgawKaren -> Just $ ISO_639_2 "kar"
    Sha -> Nothing
    Shabak -> Nothing
    Shabo -> Nothing
    Shahmirzadi -> Nothing
    Shahrudi -> Nothing
    Shakara -> Nothing
    ShallZwall -> Nothing
    ShamaSambuga -> Nothing
    Shamang -> Nothing
    Shambala -> Nothing
    Shan -> Just $ ISO_639_2 "shn"
    Shanenawa -> Nothing
    Shanga -> Nothing
    Shangzhai -> Nothing
    Sharanahua -> Nothing
    SharkBay -> Nothing
    Sharwa -> Nothing
    Shasta -> Nothing
    Shatt -> Nothing
    Shau -> Nothing
    Shawnee -> Nothing
    She -> Nothing
    Shehri -> Nothing
    Shekhawati -> Nothing
    Shekkacho -> Nothing
    Sheko -> Nothing
    Shelta -> Nothing
    ShempireSenoufo -> Nothing
    Shendu -> Nothing
    Sheni -> Nothing
    Sherbro -> Nothing
    Sherdukpen -> Nothing
    Sherpa -> Nothing
    SheshiKham -> Nothing
    Shi -> Nothing
    ShihhiSpokenArabic -> Nothing
    Shiki -> Nothing
    Shilluk -> Nothing
    Shina -> Nothing
    Shinabo -> Nothing
    ShipiboConibo -> Nothing
    Shixing -> Nothing
    Sholaga -> Nothing
    ShomPeng -> Nothing
    Shona -> Just $ ISO_639_2 "sna"
    ShooMindaNye -> Nothing
    Shor -> Nothing
    Shoshoni -> Nothing
    Shua -> Nothing
    Shuadit -> Nothing
    Shuar -> Nothing
    Shubi -> Nothing
    Shughni -> Nothing
    Shumashti -> Nothing
    Shumcho -> Nothing
    Shuswap -> Nothing
    ShuwaZamani -> Nothing
    Shwai -> Nothing
    ShwePalaung -> Nothing
    Sialum -> Nothing
    Siamou -> Nothing
    Sian -> Nothing
    Siane -> Nothing
    Siang -> Nothing
    SiarLak -> Nothing
    Sibe -> Nothing
    SiberianTatar -> Nothing
    Sibu -> Nothing
    Sicanian -> Nothing
    Sicel -> Nothing
    SichuanYi -> Just $ ISO_639_2 "iii"
    Sicilian -> Just $ ISO_639_2 "scn"
    SiciteSenoufo -> Nothing
    SiculoArabic -> Nothing
    Sidamo -> Just $ ISO_639_2 "sid"
    Sidetic -> Nothing
    Sie -> Nothing
    SierraDeJuarezZapotec -> Nothing
    SierraLeoneSignLanguage -> Nothing
    SierraNegraNahuatl -> Nothing
    Sighu -> Nothing
    Sihan -> Nothing
    SihuasAncashQuechua -> Nothing
    Sika -> Nothing
    Sikaiana -> Nothing
    Sikaritai -> Nothing
    Sikiana -> Nothing
    Sikkimese -> Nothing
    Sikule -> Nothing
    Sila -> Nothing
    SilacayoapanMixtec -> Nothing
    Sileibi -> Nothing
    Silesian -> Nothing
    Silimo -> Nothing
    Siliput -> Nothing
    Silopi -> Nothing
    Silte -> Nothing
    Simaa -> Nothing
    Simba -> Nothing
    Simbali -> Nothing
    Simbari -> Nothing
    Simbo -> Nothing
    Simeku -> Nothing
    Simeulue -> Nothing
    Simte -> Nothing
    Sinagen -> Nothing
    Sinasina -> Nothing
    Sinaugoro -> Nothing
    SindangKelingi -> Nothing
    Sindarin -> Nothing
    Sindhi -> Just $ ISO_639_2 "snd"
    SindhiBhil -> Just $ ISO_639_2 "snd"
    SindihuiMixtec -> Nothing
    Singa -> Nothing
    SingaporeSignLanguage -> Nothing
    Singpho -> Nothing
    Sinhala -> Just $ ISO_639_2 "sin"
    SinicahuaMixtec -> Nothing
    Sininkere -> Nothing
    Sinsauru -> Nothing
    SinteRomani -> Nothing
    Sinyar -> Nothing
    Sio -> Nothing
    Siona -> Nothing
    Sipacapense -> Nothing
    Sira -> Nothing
    Siraya -> Nothing
    SirenikYupik -> Nothing
    Siri -> Nothing
    Siriano -> Nothing
    Siriono -> Nothing
    Sirmauri -> Nothing
    Siroi -> Nothing
    Sissala -> Nothing
    Sissano -> Nothing
    Siuslaw -> Nothing
    Sivandi -> Nothing
    SiviaSignLanguage -> Nothing
    Siwai -> Nothing
    Siwi -> Nothing
    Siwu -> Nothing
    SiyinChin -> Nothing
    Sizaki -> Nothing
    Skagit -> Nothing
    Skalvian -> Nothing
    SkepiCreoleDutch -> Nothing
    SkoltSami -> Just $ ISO_639_2 "sms"
    Skou -> Nothing
    Slave_Athapascan -> Just $ ISO_639_2 "den"
    Slavomolisano -> Nothing
    Slovak -> Just $ ISO_639_2 "slo"
    SlovakianSignLanguage -> Nothing
    Slovenian -> Just $ ISO_639_2 "slv"
    SmallFloweryMiao -> Nothing
    SmarkyKanum -> Nothing
    Snohomish -> Nothing
    So -> Nothing
    So_Cameroon -> Nothing
    So_DemocraticRepublicOfCongo -> Nothing
    Soa -> Nothing
    Sobei -> Nothing
    SochiapanChinantec -> Nothing
    Soga -> Nothing
    Sogdian -> Just $ ISO_639_2 "sog"
    Soi -> Nothing
    Sok -> Nothing
    Sokoro -> Nothing
    Solano -> Nothing
    Soli -> Nothing
    SolomonIslandsSignLanguage -> Nothing
    Solong -> Nothing
    Solos -> Nothing
    Som -> Nothing
    Somali -> Just $ ISO_639_2 "som"
    Somrai -> Nothing
    Somray -> Nothing
    Somyev -> Nothing
    Sonaga -> Nothing
    Sonde -> Nothing
    Songa -> Nothing
    Songe -> Nothing
    SonglaiChin -> Nothing
    Songo -> Nothing
    Songomeno -> Nothing
    Songoora -> Nothing
    Sonha -> Nothing
    Sonia -> Nothing
    Soninke -> Just $ ISO_639_2 "snk"
    Sonsorol -> Nothing
    Soo -> Nothing
    Sop -> Nothing
    Soqotri -> Nothing
    Sora -> Nothing
    SoriHarengan -> Nothing
    Sorkhei -> Nothing
    Sorothaptic -> Nothing
    SosKundi -> Nothing
    SotaKanum -> Nothing
    Sou -> Nothing
    SouletinBasque -> Nothing
    SouthAfricanSignLanguage -> Nothing
    SouthAzerbaijani -> Just $ ISO_639_2 "aze"
    SouthBolivianQuechua -> Nothing
    SouthCentralBanda -> Nothing
    SouthCentralCakchiquel -> Nothing
    SouthCentralDinka -> Just $ ISO_639_2 "din"
    SouthEfate -> Nothing
    SouthFali -> Nothing
    SouthGiziga -> Nothing
    SouthLembata -> Nothing
    SouthMarquesan -> Nothing
    SouthMuyu -> Nothing
    SouthNdebele -> Just $ ISO_639_2 "nbl"
    SouthNuaulu -> Nothing
    SouthPicene -> Nothing
    SouthSlavey -> Just $ ISO_639_2 "den"
    SouthTairora -> Nothing
    SouthUcayaliAsheninka -> Nothing
    SouthWatut -> Nothing
    SouthWemale -> Nothing
    SouthWestBay -> Nothing
    SoutheastAmbrym -> Nothing
    SoutheastBabar -> Nothing
    SoutheastIjo -> Nothing
    SoutheastPashayi -> Nothing
    SoutheastTasmanianNuenonne -> Nothing
    SoutheasternDinka -> Just $ ISO_639_2 "din"
    SoutheasternHuastec -> Nothing
    SoutheasternIxtlanZapotec -> Nothing
    SoutheasternKolami -> Nothing
    SoutheasternLolo_NisiYi -> Nothing
    SoutheasternNochixtlanMixtec -> Nothing
    SoutheasternPomo -> Nothing
    SoutheasternPueblaNahuatl -> Nothing
    SoutheasternTarahumara -> Nothing
    SoutheasternTepehuan -> Nothing
    SouthernAlta -> Nothing
    SouthernAmamiOshima -> Nothing
    SouthernAymara -> Just $ ISO_639_2 "aym"
    SouthernBai -> Nothing
    SouthernBalochi -> Just $ ISO_639_2 "bal"
    SouthernBetsimisarakaMalagasy -> Just $ ISO_639_2 "mlg"
    SouthernBirifor -> Nothing
    SouthernBoboMadare -> Nothing
    SouthernBontok -> Nothing
    SouthernCakchiquel -> Nothing
    SouthernCarrier -> Nothing
    SouthernCatanduanesBicolano -> Nothing
    SouthernConchucosAncashQuechua -> Nothing
    SouthernDagaare -> Nothing
    SouthernDong -> Nothing
    SouthernGhale -> Nothing
    SouthernGondi -> Just $ ISO_639_2 "gon"
    SouthernGrebo -> Nothing
    SouthernGuiyangHmong -> Nothing
    SouthernHaida -> Just $ ISO_639_2 "hai"
    SouthernHindko -> Nothing
    SouthernKalapuya -> Nothing
    SouthernKalinga -> Nothing
    SouthernKatang -> Nothing
    SouthernKisi -> Nothing
    SouthernKiwai -> Nothing
    SouthernKurdish -> Nothing
    SouthernLolophoYi -> Nothing
    SouthernLorung -> Nothing
    SouthernLuri -> Nothing
    SouthernMadi -> Nothing
    SouthernMam -> Nothing
    SouthernMarakwet -> Nothing
    SouthernMashanHmong -> Nothing
    SouthernMnong -> Nothing
    SouthernMuji -> Nothing
    SouthernNambikuara -> Nothing
    SouthernNgbandi -> Nothing
    SouthernNicobarese -> Nothing
    SouthernNisu -> Nothing
    SouthernNuni -> Nothing
    SouthernOne -> Nothing
    SouthernPame -> Nothing
    SouthernPashto -> Nothing
    SouthernPastazaQuechua -> Nothing
    SouthernPesisir -> Nothing
    SouthernPinghuaSouthernPingChinese -> Nothing
    SouthernPokomam -> Nothing
    SouthernPomo -> Nothing
    SouthernPueblaMixtec -> Nothing
    SouthernPugetSoundSalish -> Nothing
    SouthernPumi -> Nothing
    SouthernQiandongHmong -> Nothing
    SouthernQiang -> Nothing
    SouthernRengmaNaga -> Nothing
    SouthernRinconZapotec -> Nothing
    SouthernRoglai -> Nothing
    SouthernSama -> Nothing
    SouthernSami -> Just $ ISO_639_2 "sma"
    SouthernSamo -> Nothing
    SouthernSierraMiwok -> Nothing
    SouthernSotho -> Just $ ISO_639_2 "sot"
    SouthernThai -> Nothing
    SouthernTidung -> Nothing
    SouthernTiwa -> Nothing
    SouthernToussian -> Nothing
    SouthernTujia -> Nothing
    SouthernTutchone -> Nothing
    SouthernUzbek -> Nothing
    SouthernYukaghir -> Nothing
    SouthernZhuang -> Just $ ISO_639_2 "zha"
    SouthwestGbaya -> Nothing
    SouthwestPalawano -> Nothing
    SouthwestPashayi -> Nothing
    SouthwestTanna -> Nothing
    SouthwesternBontok -> Nothing
    SouthwesternDinka -> Just $ ISO_639_2 "din"
    SouthwesternFars -> Nothing
    SouthwesternGuiyangHmong -> Nothing
    SouthwesternHuishuiHmong -> Nothing
    SouthwesternNisu -> Nothing
    SouthwesternTamang -> Nothing
    SouthwesternTarahumara -> Nothing
    SouthwesternTepehuan -> Nothing
    SouthwesternTlaxiacoMixtec -> Nothing
    Sowa -> Nothing
    Sowanda -> Nothing
    SoyaltepecMazatec -> Nothing
    SoyaltepecMixtec -> Nothing
    SpanishSignLanguage -> Nothing
    Spanish_Castilian -> Just $ ISO_639_2 "spa"
    SpitiBhoti -> Nothing
    Spokane -> Nothing
    Squamish -> Nothing
    Sranan -> Just $ ISO_639_2 "srn"
    SriLankanCreoleMalay -> Nothing
    SriLankanSignLanguage -> Nothing
    StandardEstonian -> Just $ ISO_639_2 "est"
    StandardLatvian -> Just $ ISO_639_2 "lav"
    StandardMalay -> Nothing
    StandardMoroccanTamazight -> Just $ ISO_639_2 "zgh"
    Stellingwerfs -> Nothing
    StodBhoti -> Nothing
    Stoney -> Nothing
    StraitsSalish -> Nothing
    Suabo -> Nothing
    Suarmin -> Nothing
    Suau -> Nothing
    Suba -> Nothing
    SubaSimbiti -> Nothing
    Subi -> Nothing
    Subiya -> Nothing
    Subtiaba -> Nothing
    SudaneseCreoleArabic -> Nothing
    Sudest -> Nothing
    Sudovian -> Nothing
    Suena -> Nothing
    Suga -> Nothing
    Suganga -> Nothing
    SugutDusun -> Nothing
    Sui -> Nothing
    Suki -> Nothing
    Suku -> Nothing
    Sukuma -> Just $ ISO_639_2 "suk"
    Sukur -> Nothing
    Sukurum -> Nothing
    Sula -> Nothing
    Sulka -> Nothing
    Sulod -> Nothing
    Sulung -> Nothing
    Suma -> Nothing
    Sumariup -> Nothing
    Sumau -> Nothing
    Sumbawa -> Nothing
    Sumbwa -> Nothing
    Sumerian -> Just $ ISO_639_2 "sux"
    SumiNaga -> Nothing
    SumoMayangna -> Nothing
    SumtuChin -> Nothing
    Sunam -> Nothing
    Sundanese -> Just $ ISO_639_2 "sun"
    Sungkai -> Nothing
    Sunwar -> Nothing
    Suoy -> Nothing
    SupyireSenoufo -> Nothing
    Sur -> Nothing
    Surajpuri -> Nothing
    Surbakhal -> Nothing
    Surgujia -> Nothing
    Suri -> Nothing
    Surigaonon -> Nothing
    Sursurunga -> Nothing
    Suruaha -> Nothing
    Surubu -> Nothing
    Surui -> Nothing
    SuruiDoPara -> Nothing
    Susquehannock -> Nothing
    Susu -> Just $ ISO_639_2 "sus"
    Susuami -> Nothing
    Suundi -> Nothing
    Suwawa -> Nothing
    Suya -> Nothing
    Svan -> Nothing
    Swabian -> Nothing
    Swahili_Generic -> Just $ ISO_639_2 "swa"
    Swahili_Specific -> Just $ ISO_639_2 "swa"
    Swati -> Just $ ISO_639_2 "ssw"
    Swedish -> Just $ ISO_639_2 "swe"
    SwedishSignLanguage -> Nothing
    SwissFrenchSignLanguage -> Nothing
    SwissGermanSignLanguage -> Nothing
    SwissItalianSignLanguage -> Nothing
    SyenaraSenoufo -> Nothing
    Sylheti -> Nothing
    Syriac -> Nothing
    Taabwa -> Nothing
    TabaaZapotec -> Nothing
    Tabaru -> Nothing
    TabascoChontal -> Nothing
    TabascoNahuatl -> Nothing
    TabascoZoque -> Nothing
    Tabassaran -> Nothing
    Tabla -> Nothing
    Tabo -> Nothing
    Tabriak -> Nothing
    TacahuaMixtec -> Nothing
    Tacana -> Nothing
    Tacanec -> Nothing
    Tachawit -> Nothing
    Tachelhit -> Nothing
    Tachoni -> Nothing
    Tadyawan -> Nothing
    Tae -> Nothing
    Tafi -> Nothing
    Tagabawa -> Nothing
    TagakauluKalagan -> Nothing
    TagalMurut -> Nothing
    Tagalaka -> Nothing
    Tagalog -> Just $ ISO_639_2 "tgl"
    Tagargrent -> Nothing
    Tagbanwa -> Nothing
    Tagbu -> Nothing
    Tagdal -> Nothing
    Tagin -> Nothing
    Tagish -> Nothing
    Tagoi -> Nothing
    TagwanaSenoufo -> Nothing
    TahaggartTamahaq -> Nothing
    Tahitian -> Just $ ISO_639_2 "tah"
    Tahltan -> Nothing
    Tai -> Nothing
    TaiDaeng -> Nothing
    TaiDam -> Nothing
    TaiDo -> Nothing
    TaiDon -> Nothing
    TaiHangTong -> Nothing
    TaiHongjin -> Nothing
    TaiLaing -> Nothing
    TaiLoi -> Nothing
    TaiLong -> Nothing
    TaiMene -> Nothing
    TaiNua -> Nothing
    TaiPao -> Nothing
    TaiThanh -> Nothing
    TaiYa -> Nothing
    Taiap -> Nothing
    Taikat -> Nothing
    Tainae -> Nothing
    Taino -> Nothing
    Tairuma -> Nothing
    Taita -> Nothing
    Taivoan -> Nothing
    TaiwanSignLanguage -> Nothing
    TaizziAdeniSpokenArabic -> Nothing
    Taje -> Nothing
    Tajik -> Just $ ISO_639_2 "tgk"
    Tajio -> Nothing
    Tajuasohn -> Nothing
    TajumulcoMam -> Nothing
    Takelma -> Nothing
    Takestani -> Nothing
    Takia -> Nothing
    Takpa -> Nothing
    Takua -> Nothing
    Takuu -> Nothing
    Takwane -> Nothing
    Tal -> Nothing
    Tala -> Nothing
    Talaud -> Nothing
    Taliabu -> Nothing
    Talieng -> Nothing
    TalingaBwisi -> Nothing
    Talise -> Nothing
    TalmudicAramaic -> Nothing
    Talodi -> Nothing
    Taloki -> Nothing
    Talondo -> Nothing
    Talossan -> Nothing
    Talu -> Nothing
    Talur -> Nothing
    Talysh -> Nothing
    Tama_Chad -> Nothing
    Tama_Colombia -> Nothing
    Tamagario -> Nothing
    Taman_Indonesia -> Nothing
    Taman_Myanmar -> Nothing
    Tamanaku -> Nothing
    Tamashek -> Nothing
    Tamasheq -> Nothing
    TamazolaMixtec -> Nothing
    Tambas -> Nothing
    Tambora -> Nothing
    Tambotalo -> Nothing
    TambunanDusun -> Nothing
    Tami -> Nothing
    Tamil -> Just $ ISO_639_2 "tam"
    Tamki -> Nothing
    TamnimCitak -> Nothing
    TampiasLobu -> Nothing
    Tampuan -> Nothing
    Tampulma -> Nothing
    Tanacross -> Nothing
    Tanahmerah -> Nothing
    Tanaina -> Nothing
    Tanapag -> Nothing
    Tandaganon -> Nothing
    Tandia -> Nothing
    TandroyMahafalyMalagasy -> Just $ ISO_639_2 "mlg"
    Tanema -> Nothing
    Tangale -> Nothing
    Tangchangya -> Nothing
    Tangga -> Nothing
    Tanggu -> Nothing
    TangkhulNaga -> Nothing
    TangkhulNaga_Myanmar -> Nothing
    Tangko -> Nothing
    Tanglang -> Nothing
    Tangoa -> Nothing
    Tangshewi -> Nothing
    Tanguat -> Nothing
    Tangut -> Nothing
    Tanimbili -> Nothing
    TanimucaRetuara -> Nothing
    Tanjijili -> Nothing
    Tanjong -> Nothing
    TanosyMalagasy -> Just $ ISO_639_2 "mlg"
    TanzanianSignLanguage -> Nothing
    Tapeba -> Nothing
    Tapei -> Nothing
    Tapiete -> Nothing
    Tapirape -> Nothing
    TaraoNaga -> Nothing
    Tareng -> Nothing
    Tariano -> Nothing
    Tarifit -> Nothing
    Tarjumo -> Nothing
    Tarok -> Nothing
    Tarpia -> Nothing
    Tartessian -> Nothing
    Taruma -> Nothing
    Tasawaq -> Nothing
    TaseNaga -> Nothing
    Tasmanian -> Nothing
    Tasmate -> Nothing
    TataltepecChatino -> Nothing
    Tatana -> Nothing
    Tatar -> Just $ ISO_639_2 "tat"
    Tatuyo -> Nothing
    Tauade -> Nothing
    Taulil -> Nothing
    Taungyo -> Nothing
    Taupota -> Nothing
    Tause -> Nothing
    Taushiro -> Nothing
    Tausug -> Nothing
    Tauya -> Nothing
    Taveta -> Nothing
    Tavoyan -> Nothing
    TavringerRomani -> Nothing
    Tawala -> Nothing
    TawallammatTamajaq -> Nothing
    Tawande -> Nothing
    TawangMonpa -> Nothing
    Tawara -> Just $ ISO_639_2 "sna"
    Taworta -> Nothing
    Tawoyan -> Nothing
    TawrChin -> Nothing
    Tay -> Nothing
    TayBoi -> Nothing
    TayKhang -> Nothing
    TaySaPa -> Nothing
    TayTac -> Nothing
    TayabasAyta -> Nothing
    TayartTamajeq -> Nothing
    Tayo -> Nothing
    Taznatit -> Nothing
    Tboli -> Nothing
    Tchitchege -> Nothing
    Tchumbuli -> Nothing
    Teanu -> Nothing
    Tebilung -> Nothing
    TebulSignLanguage -> Nothing
    TebulUreDogon -> Nothing
    TecpatlanTotonac -> Nothing
    Tedaga -> Nothing
    TedimChin -> Nothing
    Tee -> Nothing
    Teen -> Nothing
    Tefaro -> Nothing
    Tegali -> Nothing
    Tehit -> Nothing
    Tehuelche -> Nothing
    TejalapanZapotec -> Nothing
    TekeEbo -> Nothing
    TekeFuumu -> Nothing
    TekeKukuya -> Nothing
    TekeLaali -> Nothing
    TekeNzikou -> Nothing
    TekeTege -> Nothing
    TekeTsaayi -> Nothing
    TekeTyee -> Nothing
    Tektiteko -> Nothing
    TelaMasbuar -> Nothing
    Telefol -> Nothing
    Telugu -> Just $ ISO_639_2 "tel"
    Teluti -> Nothing
    Tem -> Nothing
    TemacineTamazight -> Nothing
    TemascaltepecNahuatl -> Nothing
    Tembe -> Nothing
    Tembo_Kitembo -> Nothing
    Tembo_Motembo -> Nothing
    Teme -> Nothing
    Temein -> Nothing
    Temi -> Nothing
    Temiar -> Nothing
    TemoayaOtomi -> Nothing
    Temoq -> Nothing
    TempasukDusun -> Nothing
    Temuan -> Nothing
    Ten -> Nothing
    TenaLowlandQuichua -> Nothing
    TenangoNahuatl -> Nothing
    TenangoOtomi -> Nothing
    TeneKanDogon -> Nothing
    TenggarongKutaiMalay -> Nothing
    Tengger -> Nothing
    Tenharim -> Nothing
    Tenino -> Nothing
    Tenis -> Nothing
    Tennet -> Nothing
    Teop -> Nothing
    Teor -> Nothing
    Tepecano -> Nothing
    TepetotutlaChinantec -> Nothing
    TepeuxilaCuicatec -> Nothing
    TepinapaChinantec -> Nothing
    TepoKrumen -> Nothing
    TerSami -> Nothing
    Tera -> Nothing
    Terebu -> Nothing
    Terei -> Nothing
    Tereno -> Just $ ISO_639_2 "ter"
    Teressa -> Nothing
    Tereweng -> Nothing
    Teribe -> Nothing
    Terik -> Nothing
    Termanu -> Nothing
    Ternate -> Nothing
    Ternateno -> Nothing
    TesakaMalagasy -> Just $ ISO_639_2 "mlg"
    Tese -> Nothing
    Teshenawa -> Nothing
    Teso -> Nothing
    Tetela -> Nothing
    TetelcingoNahuatl -> Nothing
    Tetete -> Nothing
    Tetserret -> Nothing
    Tetum -> Just $ ISO_639_2 "tet"
    TetunDili -> Just $ ISO_639_2 "tet"
    Teun -> Nothing
    TeutilaCuicatec -> Nothing
    Tewa_Indonesia -> Nothing
    Tewa_Usa -> Nothing
    Tewe -> Nothing
    TexcatepecOtomi -> Nothing
    TexistepecPopoluca -> Nothing
    TexmelucanZapotec -> Nothing
    TezoatlanMixtec -> Nothing
    Tha -> Nothing
    Thachanadan -> Nothing
    ThadoChin -> Nothing
    Thai -> Just $ ISO_639_2 "tha"
    ThaiSignLanguage -> Nothing
    ThaiSong -> Nothing
    Thai_Northern -> Nothing
    ThaiphumChin -> Nothing
    Thakali -> Nothing
    ThangalNaga -> Nothing
    Thangmi -> Nothing
    Thao -> Nothing
    Tharaka -> Nothing
    Thawa -> Nothing
    Thayore -> Nothing
    Thaypan -> Nothing
    The -> Nothing
    Thiin -> Nothing
    Tho -> Nothing
    Thompson -> Nothing
    Thopho -> Nothing
    Thracian -> Nothing
    ThuLao -> Nothing
    Thudam -> Nothing
    Thulung -> Nothing
    Thur -> Just $ ISO_639_2 "ach"
    Thurawal -> Nothing
    Thuri -> Nothing
    TiagbamrinAizi -> Nothing
    Tiale -> Nothing
    Tiang -> Nothing
    Tibea -> Nothing
    Tibetan -> Just $ ISO_639_2 "tib"
    TibetanSignLanguage -> Nothing
    Tichurong -> Nothing
    Ticuna -> Nothing
    TidaaMixtec -> Nothing
    TidikeltTamazight -> Nothing
    Tidong -> Nothing
    Tidore -> Nothing
    Tiefo -> Nothing
    TiemaCieweBozo -> Nothing
    Tiene -> Nothing
    TieyaxoBozo -> Nothing
    Tifal -> Nothing
    Tigak -> Nothing
    TigonMbembe -> Nothing
    Tigre -> Just $ ISO_639_2 "tig"
    Tigrinya -> Just $ ISO_639_2 "tir"
    Tii -> Nothing
    TijaltepecMixtec -> Nothing
    Tikar -> Nothing
    Tikopia -> Nothing
    TilaChol -> Nothing
    TilapaOtomi -> Nothing
    Tillamook -> Nothing
    TilquiapanZapotec -> Nothing
    Tilung -> Nothing
    Tima -> Nothing
    Timbe -> Nothing
    Time -> Just $ ISO_639_2 "tem"
    TimorPidgin -> Nothing
    Timucua -> Nothing
    TimugonMurut -> Nothing
    TinaSambal -> Nothing
    Tinani -> Nothing
    Tindi -> Nothing
    Tingal -> Nothing
    TinguiBoto -> Nothing
    Tinigua -> Nothing
    TinocKallahan -> Nothing
    Tinputz -> Nothing
    Tippera -> Nothing
    Tira -> Nothing
    Tirahi -> Nothing
    TiranigeDigaDogon -> Nothing
    Tiri -> Nothing
    Tiruray -> Nothing
    Tita -> Nothing
    Titan -> Nothing
    Tiv -> Just $ ISO_639_2 "tiv"
    Tiwa -> Nothing
    Tiwi -> Nothing
    Tiyaa -> Nothing
    Tjungundji -> Nothing
    Tjupany -> Nothing
    Tjurruru -> Nothing
    TlachichilcoTepehua -> Nothing
    TlacoapaTlapanec -> Nothing
    TlacoatzintepecChinantec -> Nothing
    TlacolulitaZapotec -> Nothing
    TlahuitoltepecMixe -> Nothing
    TlalitzlipaNahuatl -> Nothing
    TlamacazapaNahuatl -> Nothing
    TlazoyaltepecMixtec -> Nothing
    Tlingit -> Just $ ISO_639_2 "tli"
    To -> Nothing
    Toabaita -> Nothing
    Toala -> Nothing
    Toaripi -> Nothing
    Toba -> Nothing
    TobaMaskoy -> Nothing
    TobagonianCreoleEnglish -> Nothing
    Tobanga -> Nothing
    Tobati -> Nothing
    Tobelo -> Nothing
    Tobian -> Nothing
    Tobo -> Nothing
    Tocho -> Nothing
    Toda -> Nothing
    TodosSantosCuchumatanMam -> Nothing
    Todrah -> Nothing
    Tofanma -> Nothing
    TofinGbe -> Nothing
    Toga -> Nothing
    TogboVaraBanda -> Nothing
    Togoyo -> Nothing
    TohonoOodham -> Nothing
    Tojolabal -> Nothing
    TokPisin -> Just $ ISO_639_2 "tpi"
    Tokano -> Nothing
    Tokelau -> Just $ ISO_639_2 "tkl"
    TokharianA -> Nothing
    TokharianB -> Nothing
    TokiPona -> Nothing
    TokuNoShima -> Nothing
    Tol -> Nothing
    Tolaki -> Nothing
    Tolomako -> Nothing
    Tolowa -> Nothing
    Toma -> Nothing
    Tomadino -> Nothing
    Tombelala -> Nothing
    Tombonuwo -> Nothing
    Tombulu -> Nothing
    Tomedes -> Nothing
    Tomini -> Nothing
    TommeginneNorthernTasmanian -> Nothing
    TommoSoDogon -> Nothing
    TomoKanDogon -> Nothing
    Tomoip -> Nothing
    Tomyang -> Nothing
    Tondano -> Nothing
    TondiSongwayKiini -> Nothing
    Tonga_Nyasa -> Just $ ISO_639_2 "tog"
    Tonga_Thailand -> Nothing
    Tonga_TongaIslands -> Just $ ISO_639_2 "ton"
    Tonga_Zambia -> Nothing
    Tongwe -> Nothing
    Tonjon -> Nothing
    Tonkawa -> Nothing
    Tonsawang -> Nothing
    Tonsea -> Nothing
    Tontemboan -> Nothing
    ToogeeSouthwesternTasmanian -> Nothing
    Tooro -> Nothing
    Topoiyo -> Nothing
    Toposa -> Nothing
    Tora -> Nothing
    TorajaSadan -> Nothing
    Toram -> Nothing
    Torau -> Nothing
    Toro -> Nothing
    ToroSoDogon -> Nothing
    ToroTeguDogon -> Nothing
    Toromono -> Nothing
    Torona -> Nothing
    TorresStraitCreole -> Nothing
    Torricelli -> Nothing
    Torwali -> Nothing
    Totela -> Nothing
    Toto -> Nothing
    Totoli -> Nothing
    TotomachapanZapotec -> Nothing
    TotontepecMixe -> Nothing
    Totoro -> Nothing
    Touo -> Nothing
    Toura_CoteDivoire -> Nothing
    Toura_PapuaNewGuinea -> Nothing
    Towei -> Nothing
    TransalpineGaulish -> Nothing
    TravellerDanish -> Nothing
    TravellerNorwegian -> Nothing
    TravellerScottish -> Nothing
    Tregami -> Nothing
    Tremembe -> Nothing
    Trieng -> Nothing
    Trimuris -> Nothing
    Tring -> Nothing
    Tringgus -> Nothing
    TrinidadAndTobagoSignLanguage -> Nothing
    TrinidadianCreoleEnglish -> Nothing
    Trinitario -> Nothing
    Trio -> Nothing
    Truka -> Nothing
    Trumai -> Nothing
    Tsaangi -> Nothing
    Tsakhur -> Nothing
    Tsakonian -> Nothing
    Tsakwambo -> Nothing
    Tsamai -> Nothing
    Tsat -> Nothing
    Tseku -> Nothing
    Tsetsaut -> Nothing
    Tsez -> Nothing
    Tshangla -> Nothing
    Tsikimba -> Nothing
    Tsimane -> Nothing
    TsimihetyMalagasy -> Just $ ISO_639_2 "mlg"
    Tsimshian -> Just $ ISO_639_2 "tsi"
    Tsishingini -> Nothing
    Tso -> Nothing
    Tsoa -> Nothing
    Tsogo -> Nothing
    Tsonga -> Just $ ISO_639_2 "tso"
    Tsotsitaal -> Nothing
    Tsotso -> Nothing
    Tsou -> Nothing
    TsucubaCuba -> Nothing
    Tsum -> Nothing
    TsunLao -> Nothing
    Tsuvadi -> Nothing
    Tsuvan -> Nothing
    Tswa -> Nothing
    Tswana -> Just $ ISO_639_2 "tsn"
    Tswapong -> Just $ ISO_639_2 "nso"
    Tu -> Nothing
    Tuamotuan -> Nothing
    Tubar -> Nothing
    Tubarao -> Nothing
    Tubatulabal -> Nothing
    Tucano -> Nothing
    Tugun -> Nothing
    Tugutil -> Nothing
    TukangBesiNorth -> Nothing
    TukangBesiSouth -> Nothing
    Tuki -> Nothing
    Tukpa -> Nothing
    Tukudede -> Nothing
    Tukumanfed -> Nothing
    Tula -> Nothing
    Tulai -> Nothing
    Tulehu -> Nothing
    Tulishi -> Nothing
    Tulu -> Nothing
    TuluBohuai -> Nothing
    TumaIrumu -> Nothing
    Tumak -> Nothing
    TumariKanuri -> Just $ ISO_639_2 "kau"
    TumbalaChol -> Nothing
    Tumbuka -> Just $ ISO_639_2 "tum"
    Tumi -> Nothing
    Tumleo -> Nothing
    Tumshuqese -> Just $ ISO_639_2 "kho"
    Tumtum -> Nothing
    TumulungSisaala -> Nothing
    Tumzabt -> Nothing
    TundraEnets -> Nothing
    Tunen -> Nothing
    Tungag -> Nothing
    Tunggare -> Nothing
    Tunia -> Nothing
    Tunica -> Nothing
    TunisianSignLanguage -> Nothing
    TunisianSpokenArabic -> Nothing
    Tunjung -> Nothing
    Tunni -> Nothing
    Tuotomb -> Nothing
    Tupari -> Nothing
    Tupi -> Nothing
    Tupinamba -> Nothing
    Tupinikin -> Nothing
    Tupuri -> Nothing
    Turaka -> Nothing
    Turi -> Nothing
    Turiwara -> Nothing
    Turka -> Nothing
    Turkana -> Nothing
    TurkicKhalaj -> Nothing
    Turkish -> Just $ ISO_639_2 "tur"
    TurkishSignLanguage -> Nothing
    Turkmen -> Just $ ISO_639_2 "tuk"
    TurksAndCaicosCreoleEnglish -> Nothing
    Turoyo -> Nothing
    Turumsa -> Nothing
    Turung -> Nothing
    Tuscarora -> Nothing
    Tutelo -> Nothing
    TutohKenyah -> Nothing
    Tutong1 -> Nothing
    Tutong2 -> Nothing
    TutsaNaga -> Nothing
    Tutuba -> Nothing
    TututepecMixtec -> Nothing
    Tututni -> Nothing
    Tuvalu -> Just $ ISO_639_2 "tvl"
    Tuvinian -> Just $ ISO_639_2 "tyv"
    TuwaliIfugao -> Nothing
    Tuwari -> Nothing
    Tuwuli -> Nothing
    Tuxa -> Nothing
    Tuxinawa -> Nothing
    Tuyuca -> Nothing
    Twana -> Nothing
    Twendi -> Nothing
    Twents -> Nothing
    Twi -> Just $ ISO_639_2 "twi"
    Tyap -> Nothing
    Tyaraity -> Nothing
    TyerrenoterpannerNorthMidlandsTasmanian -> Nothing
    U -> Nothing
    UabMeto -> Nothing
    Uamue -> Nothing
    Uare -> Nothing
    Ubaghara -> Nothing
    Ubang -> Nothing
    Ubi -> Nothing
    Ubir -> Nothing
    Ubykh -> Nothing
    UcayaliYuruaAsheninka -> Nothing
    Uda -> Nothing
    Udi -> Nothing
    Udihe -> Nothing
    Udmurt -> Just $ ISO_639_2 "udm"
    Uduk -> Nothing
    Ufim -> Nothing
    UgandanSignLanguage -> Nothing
    Ugaritic -> Just $ ISO_639_2 "uga"
    Ughele -> Nothing
    Ugong -> Nothing
    Uhami -> Nothing
    Uighur -> Just $ ISO_639_2 "uig"
    Uisai -> Nothing
    Ujir -> Nothing
    Ukaan -> Nothing
    Ukhwejo -> Nothing
    Ukit -> Nothing
    UkpeBayobiri -> Nothing
    UkpetEhom -> Nothing
    Ukrainian -> Just $ ISO_639_2 "ukr"
    UkrainianSignLanguage -> Nothing
    Ukue -> Nothing
    Ukuriguma -> Nothing
    Ukwa -> Nothing
    UkwuaniAbohNdoni -> Nothing
    UlauSuain -> Nothing
    Ulch -> Nothing
    Ulithian -> Nothing
    Ullatan -> Nothing
    Ulukwumi -> Nothing
    Ulumanda -> Nothing
    Ulwa -> Nothing
    Uma -> Nothing
    UmaLung -> Nothing
    Umanakaina -> Nothing
    Umatilla -> Nothing
    Umbindhamu -> Nothing
    Umbrian -> Nothing
    UmbuUngu -> Nothing
    Umbugarla -> Nothing
    Umbundu -> Just $ ISO_639_2 "umb"
    Umbuygamu -> Nothing
    UmeSami -> Nothing
    Umeda -> Nothing
    Umiida -> Nothing
    UmirayDumagetAgta -> Nothing
    Umon -> Nothing
    Umotina -> Nothing
    Umpila -> Nothing
    Una -> Nothing
    Unami -> Nothing
    UndeKaili -> Nothing
    Uneapa -> Nothing
    Uneme -> Nothing
    Unggarranggu -> Nothing
    Unggumi -> Nothing
    Ungkue -> Nothing
    Uni -> Nothing
    Unserdeutsch -> Just $ ISO_639_2 "crp"
    Unua -> Nothing
    Unubahe -> Nothing
    Uokha -> Nothing
    UpperBaramKenyah -> Nothing
    UpperChehalis -> Nothing
    UpperGrandValleyDani -> Nothing
    UpperGuineaCrioulo -> Nothing
    UpperKinabatangan -> Nothing
    UpperKuskokwim -> Nothing
    UpperNecaxaTotonac -> Nothing
    UpperPokomo -> Nothing
    UpperSaxon -> Nothing
    UpperSorbian -> Just $ ISO_639_2 "hsb"
    UpperTanana -> Nothing
    UpperTanudanKalinga -> Nothing
    UpperTaoih -> Nothing
    UpperTaromi -> Nothing
    UpperUmpqua -> Nothing
    Ura_PapuaNewGuinea -> Nothing
    Ura_Vanuatu -> Nothing
    Uradhi -> Nothing
    UrakLawoi -> Nothing
    Urali -> Nothing
    Urapmin -> Nothing
    Urarina -> Nothing
    Urartian -> Nothing
    Urat -> Nothing
    Urdu -> Just $ ISO_639_2 "urd"
    Urhobo -> Nothing
    Uri -> Nothing
    Urigina -> Nothing
    Urim -> Nothing
    Urimo -> Nothing
    UripivWalaRanoAtchin -> Nothing
    Urningangg -> Nothing
    Uru -> Nothing
    UruEuWauWau -> Nothing
    UruPaIn -> Nothing
    Uruangnirin -> Nothing
    Uruava -> Nothing
    UrubuKaapor -> Nothing
    UrubuKaaporSignLanguage -> Nothing
    UruguayanSignLanguage -> Nothing
    Urum -> Nothing
    Urumi -> Nothing
    UsSaare -> Nothing
    Usaghade -> Nothing
    Usan -> Nothing
    Usarufa -> Nothing
    Ushojo -> Nothing
    UsilaChinantec -> Nothing
    Usku -> Nothing
    Uspanteco -> Nothing
    Usui -> Nothing
    UtHun -> Nothing
    Utarmbung -> Nothing
    UteSouthernPaiute -> Nothing
    Utu -> Nothing
    Uvbie -> Nothing
    Uya -> Nothing
    Uzbek -> Just $ ISO_639_2 "uzb"
    Uzekwe -> Nothing
    VaagriBooli -> Nothing
    Vafsi -> Nothing
    VaghatYaBijimLegeri -> Nothing
    Vaghri -> Nothing
    Vaghua -> Nothing
    Vagla -> Nothing
    Vai -> Just $ ISO_639_2 "vai"
    Vaiphei -> Nothing
    Vale -> Nothing
    ValencianSignLanguage -> Nothing
    ValleNacionalChinantec -> Nothing
    ValleyMaidu -> Nothing
    Valman -> Nothing
    Valpei -> Nothing
    Vamale -> Nothing
    Vame -> Nothing
    Vandalic -> Nothing
    Vangunu -> Nothing
    Vanimo -> Nothing
    Vano -> Nothing
    Vanuma -> Nothing
    Vao -> Nothing
    VarhadiNagpuri -> Nothing
    Varisi -> Nothing
    Varli -> Nothing
    Vasavi -> Nothing
    VasekelaBushman -> Nothing
    Vatrata -> Nothing
    Veddah -> Nothing
    Vehes -> Nothing
    Veluws -> Nothing
    VemgoMabas -> Nothing
    Venda -> Just $ ISO_639_2 "ven"
    Venetian -> Nothing
    Venetic -> Nothing
    VenezuelanSignLanguage -> Nothing
    Vengo -> Nothing
    Ventureno -> Nothing
    VenustianoCarranzaTzotzil -> Nothing
    Veps -> Nothing
    Veraa -> Nothing
    VeracruzHuastec -> Nothing
    Vestinian -> Nothing
    Vidunda -> Nothing
    Viemo -> Nothing
    Vietnamese -> Just $ ISO_639_2 "vie"
    Vilela -> Nothing
    Vili -> Nothing
    VillaViciosaAgta -> Nothing
    VincentianCreoleEnglish -> Nothing
    Vinmavis -> Nothing
    Vinza -> Nothing
    VirginIslandsCreoleEnglish -> Nothing
    Vishavan -> Nothing
    Viti -> Nothing
    Vitou -> Nothing
    VlaamseGebarentaal -> Nothing
    VlaxRomani -> Nothing
    Volapuk -> Just $ ISO_639_2 "vol"
    Volscian -> Nothing
    Vono -> Nothing
    Voro -> Nothing
    Votic -> Just $ ISO_639_2 "vot"
    Vumbu -> Nothing
    Vunapu -> Nothing
    Vunjo -> Nothing
    Vute -> Nothing
    Wa -> Nothing
    Waama -> Nothing
    Waamwang -> Nothing
    Wab -> Nothing
    Wabo -> Nothing
    Waboda -> Nothing
    WaciGbe -> Nothing
    Wadaginam -> Nothing
    Waddar -> Nothing
    WadiWadi -> Nothing
    Wadikali -> Nothing
    WadiyaraKoli -> Nothing
    Wadjabangayi -> Nothing
    Wadjiginy -> Nothing
    Wadjigu -> Nothing
    WaeRana -> Nothing
    Waema -> Nothing
    Waffa -> Nothing
    Wagawaga -> Nothing
    Wagaya -> Nothing
    Wagdi -> Nothing
    Wageman -> Nothing
    Wagi -> Nothing
    WahauKayan -> Nothing
    WahauKenyah -> Nothing
    Wahgi -> Nothing
    Waigali -> Nothing
    Waigeo -> Nothing
    Wailaki -> Nothing
    Wailapa -> Nothing
    Waima -> Nothing
    Waimaa -> Nothing
    Waimaha -> Nothing
    Waioli -> Nothing
    Waiwai -> Nothing
    Waja -> Nothing
    Wajarri -> Nothing
    Wajuk -> Nothing
    Waka -> Nothing
    Wakabunga -> Nothing
    Wakawaka -> Nothing
    Wakde -> Nothing
    Wakhi -> Nothing
    Wakona -> Nothing
    Wala -> Nothing
    Walak -> Nothing
    Walangama -> Nothing
    Wali_Ghana -> Nothing
    Wali_Sudan -> Nothing
    Waling -> Nothing
    Walio -> Nothing
    WallaWalla -> Nothing
    Wallisian -> Nothing
    Walloon -> Just $ ISO_639_2 "wln"
    Walmajarri -> Nothing
    WaloKumbeDogon -> Nothing
    Walser -> Nothing
    Walungge -> Nothing
    Wamas -> Nothing
    Wambaya -> Nothing
    Wambon -> Nothing
    Wambule -> Nothing
    Wamey -> Nothing
    Wamin -> Nothing
    Wampanoag -> Nothing
    Wampar -> Nothing
    Wampur -> Nothing
    Wan -> Nothing
    Wanambre -> Nothing
    Wanap -> Nothing
    WanchoNaga -> Nothing
    Wanda -> Nothing
    Wandala -> Nothing
    Wandamen -> Nothing
    Wandarang -> Nothing
    Wandji -> Nothing
    Wane -> Nothing
    Waneci -> Nothing
    Wanga -> Nothing
    WangaaybuwanNgiyambaa -> Nothing
    Wanggamala -> Nothing
    Wangganguru -> Nothing
    Wanggom -> Nothing
    Wangkayutyuru -> Nothing
    Wangkumara -> Nothing
    Wanji -> Nothing
    Wanman -> Nothing
    Wannu -> Nothing
    Wano -> Nothing
    Wantoat -> Nothing
    Wanukaka -> Nothing
    Wanyi -> Nothing
    Waorani -> Nothing
    Wapan -> Nothing
    Wapha -> Nothing
    Wapishana -> Nothing
    Wappo -> Nothing
    War -> Nothing
    Wara -> Nothing
    Warao -> Nothing
    Warapu -> Nothing
    WaraySorsogon -> Nothing
    Waray_Australia -> Nothing
    Waray_Philippines -> Just $ ISO_639_2 "war"
    Wardaman -> Nothing
    Wardandi -> Nothing
    Warduji -> Nothing
    Ware -> Nothing
    Warembori -> Nothing
    Wares -> Nothing
    Waris -> Nothing
    Waritai -> Nothing
    Wariyangga -> Nothing
    Warji -> Nothing
    WarkayBipim -> Nothing
    Warlmanpa -> Nothing
    Warlpiri -> Nothing
    Warluwara -> Nothing
    Warnang -> Nothing
    Waropen -> Nothing
    Warrgamay -> Nothing
    Warrwa -> Nothing
    Waru -> Nothing
    Warumungu -> Nothing
    Waruna -> Nothing
    Warungu -> Nothing
    WarwarFeni -> Nothing
    Wasa -> Just $ ISO_639_2 "aka"
    WascoWishram -> Nothing
    Wasembo -> Nothing
    Washo -> Just $ ISO_639_2 "was"
    Waskia -> Nothing
    Wasu -> Nothing
    Watakataui -> Nothing
    Watam -> Nothing
    Wathawurrung -> Nothing
    Watubela -> Nothing
    Waura -> Nothing
    Wauyai -> Nothing
    Wawa -> Nothing
    Wawonii -> Nothing
    Waxianghua -> Nothing
    Wayampi -> Nothing
    Wayana -> Nothing
    WayanadChetti -> Nothing
    Wayoro -> Nothing
    Wayu -> Nothing
    Wayuu -> Nothing
    WeNorthern -> Nothing
    WeSouthern -> Nothing
    WeWestern -> Nothing
    Wedau -> Nothing
    Weh -> Nothing
    Wejewa -> Nothing
    Welaun -> Nothing
    Welaung -> Nothing
    Weliki -> Nothing
    Welsh -> Just $ ISO_639_2 "wel"
    WelshRomani -> Nothing
    WembaWemba -> Nothing
    WemeGbe -> Just $ ISO_639_2 "fon"
    Wendat -> Nothing
    Were -> Nothing
    Wergaia -> Nothing
    Weri -> Nothing
    Wersing -> Nothing
    WestAlbayBikol -> Nothing
    WestAmbae -> Nothing
    WestBengalSignLanguage -> Nothing
    WestBerawan -> Nothing
    WestCentralBanda -> Nothing
    WestCentralLimba -> Nothing
    WestCentralOromo -> Just $ ISO_639_2 "orm"
    WestCentralQuiche -> Nothing
    WestCoastBajau -> Nothing
    WestDamar -> Nothing
    WestFlemish -> Nothing
    WestKewa -> Nothing
    WestLembata -> Nothing
    WestMakian -> Nothing
    WestMasela -> Nothing
    WestTarangan -> Nothing
    WestUvean -> Nothing
    WestYugur -> Nothing
    WesternAbnaki -> Nothing
    WesternArmenian -> Nothing
    WesternArrarnta -> Nothing
    WesternBalochi -> Just $ ISO_639_2 "bal"
    WesternBolivianGuarani -> Nothing
    WesternBru -> Nothing
    WesternBukidnonManobo -> Nothing
    WesternCakchiquel -> Nothing
    WesternCham -> Nothing
    WesternDani -> Nothing
    WesternDurangoNahuatl -> Nothing
    WesternFarsi -> Nothing
    WesternFijian -> Nothing
    WesternFrisian -> Just $ ISO_639_2 "fry"
    WesternGurung -> Nothing
    WesternHighlandChatino -> Nothing
    WesternHighlandPurepecha -> Nothing
    WesternHuastecaNahuatl -> Nothing
    WesternJacalteco -> Nothing
    WesternKanjobal -> Nothing
    WesternKaraboro -> Nothing
    WesternKatu -> Nothing
    WesternKayah -> Nothing
    WesternKenyah -> Nothing
    WesternKeres -> Nothing
    WesternKrahn -> Nothing
    WesternLaluYi -> Nothing
    WesternLawa -> Nothing
    WesternMagar -> Nothing
    WesternManinkakan -> Nothing
    WesternMari -> Nothing
    WesternMashanHmong -> Nothing
    WesternMeohang -> Nothing
    WesternMinyag -> Nothing
    WesternMuria -> Nothing
    WesternNeoAramaic -> Nothing
    WesternNigerFulfulde -> Nothing
    WesternOjibwa -> Nothing
    WesternPanjabi -> Just $ ISO_639_2 "pan"
    WesternParbate -> Nothing
    WesternPenan -> Nothing
    WesternPokomchi -> Nothing
    WesternSisaala -> Nothing
    WesternSubanon -> Nothing
    WesternTamang -> Nothing
    WesternTawbuid -> Nothing
    WesternTunebo -> Nothing
    WesternTzutujil -> Nothing
    WesternXiangxiHmong -> Nothing
    WesternXwlaGbe -> Nothing
    WesternYiddish -> Nothing
    Western_XishanbaLaloYi -> Nothing
    Westphalien -> Nothing
    Wetamut -> Nothing
    Wewaw -> Just $ ISO_639_2 "kar"
    Weyto -> Nothing
    WhiteGelao -> Nothing
    WhiteLachi -> Nothing
    Whitesands -> Nothing
    Wiaki -> Nothing
    Wiarumus -> Nothing
    WichiLhamtesGuisnay -> Nothing
    WichiLhamtesNocten -> Nothing
    WichiLhamtesVejoz -> Nothing
    Wichita -> Nothing
    WikEpa -> Nothing
    WikIiyanh -> Nothing
    WikKeyangan -> Nothing
    WikMeanha -> Nothing
    WikMungkan -> Nothing
    WikNgathana -> Nothing
    Wikalkan -> Nothing
    Wikngenchera -> Nothing
    Wilawila -> Nothing
    Wintu -> Nothing
    Winye -> Nothing
    Wipi -> Nothing
    Wiradhuri -> Nothing
    Wirafed -> Nothing
    Wirangu -> Nothing
    Wiru -> Nothing
    Wiyot -> Nothing
    Woccon -> Nothing
    Wogamusin -> Nothing
    Wogeo -> Nothing
    Woi -> Nothing
    Woiwurrung -> Nothing
    Wojenaka -> Nothing
    Wolaitta -> Just $ ISO_639_2 "wal"
    Wolane -> Nothing
    Wolani -> Nothing
    Woleaian -> Nothing
    Wolio -> Nothing
    Wolof -> Just $ ISO_639_2 "wol"
    Wom_Nigeria -> Nothing
    Wom_PapuaNewGuinea -> Nothing
    Womo -> Nothing
    Wongo -> Nothing
    Woria -> Nothing
    Worimi -> Nothing
    Worodougou -> Nothing
    Worora -> Nothing
    Worrorra -> Nothing
    WotapuriKatarqalai -> Nothing
    Wotjobaluk -> Nothing
    Wotu -> Nothing
    WounMeu -> Nothing
    WrittenOirat -> Just $ ISO_639_2 "xal"
    WuChinese -> Nothing
    WudingLuquanYi -> Nothing
    Wudu -> Nothing
    Wuliwuli -> Nothing
    Wulna -> Nothing
    Wumboko -> Nothing
    Wumbvu -> Nothing
    WumengYi -> Nothing
    WunaiBunu -> Nothing
    Wunambal -> Nothing
    Wunumara -> Nothing
    Wurrugu -> Nothing
    Wusa_WumengNasuYi -> Nothing
    Wusa_WusaNasuYi -> Nothing
    Wushi -> Nothing
    Wusi -> Nothing
    Wutung -> Nothing
    Wutunhua -> Nothing
    WuvuluAua -> Nothing
    Wuzlam -> Nothing
    Wyandot -> Nothing
    Wymysorys -> Nothing
    Xaasongaxango -> Nothing
    XadaniZapotec -> Nothing
    Xakriaba -> Nothing
    Xam -> Nothing
    Xamtanga -> Nothing
    XanaguiaZapotec -> Nothing
    Xaracuu -> Nothing
    Xaragure -> Nothing
    Xavante -> Nothing
    Xegwi -> Nothing
    Xerente -> Nothing
    Xeta -> Nothing
    Xhosa -> Just $ ISO_639_2 "xho"
    Xiandao -> Nothing
    XiangChinese -> Nothing
    Xibe -> Nothing
    XicotepecDeJuarezTotonac -> Nothing
    Xinca -> Nothing
    XinguAsurini -> Nothing
    Xipaya -> Nothing
    Xipinawa -> Nothing
    Xiri -> Nothing
    Xiriana -> Nothing
    Xokleng -> Nothing
    Xoo -> Nothing
    Xukuru -> Nothing
    XwelaGbe -> Nothing
    Yaaku -> Nothing
    Yabaana -> Nothing
    Yabarana -> Nothing
    Yabem -> Nothing
    Yaben -> Nothing
    Yabong -> Nothing
    YabulaYabula -> Nothing
    Yace -> Nothing
    Yaeyama -> Nothing
    Yafi -> Nothing
    Yagara -> Nothing
    Yagaria -> Nothing
    Yagnobi -> Nothing
    Yagomi -> Nothing
    Yagua -> Nothing
    Yagwoia -> Nothing
    Yahadian -> Nothing
    Yahang -> Nothing
    Yahuna -> Nothing
    Yaka_CentralAfricanRepublic -> Nothing
    Yaka_Congo -> Nothing
    Yaka_DemocraticRepublicOfCongo -> Nothing
    Yakaikeke -> Nothing
    Yakamul -> Nothing
    Yakan -> Nothing
    Yakha -> Nothing
    Yakima -> Nothing
    Yakoma -> Nothing
    Yala -> Nothing
    Yalahatan -> Nothing
    Yalakalore -> Nothing
    YalalagZapotec -> Nothing
    Yalarnnga -> Nothing
    Yale -> Nothing
    Yaleba -> Nothing
    Yalunka -> Nothing
    Yamana -> Nothing
    Yamap -> Nothing
    Yamba -> Nothing
    Yambes -> Nothing
    Yambeta -> Nothing
    Yamdena -> Nothing
    Yameo -> Nothing
    Yami -> Nothing
    Yaminahua -> Nothing
    Yamna -> Nothing
    Yamongeri -> Just $ ISO_639_2 "lol"
    Yamphe -> Nothing
    Yamphu -> Nothing
    YanNhanguSignLanguage -> Nothing
    Yana -> Nothing
    YanahuancaPascoQuechua -> Nothing
    Yanda -> Nothing
    YandaDomDogon -> Nothing
    Yandjibara -> Nothing
    Yandruwandha -> Nothing
    Yanesha -> Nothing
    YangZhuang -> Nothing
    Yangben -> Nothing
    Yangbye -> Nothing
    Yangho -> Nothing
    Yangkam -> Nothing
    Yangman -> Nothing
    Yango -> Nothing
    Yangulam -> Nothing
    YangumDey -> Nothing
    YangumGel -> Nothing
    YangumMon -> Nothing
    Yankunytjatjara -> Nothing
    Yanomami -> Nothing
    Yanomamo -> Nothing
    Yansi -> Nothing
    Yanyuwa -> Nothing
    Yao -> Just $ ISO_639_2 "yao"
    Yaoure -> Nothing
    Yapese -> Just $ ISO_639_2 "yap"
    Yapunda -> Nothing
    Yaqay -> Nothing
    Yaqui -> Nothing
    Yarawata -> Nothing
    Yardliyawarra -> Nothing
    Yareba -> Nothing
    YareniZapotec -> Nothing
    Yari -> Nothing
    Yarluyandi -> Nothing
    Yaroame -> Nothing
    Yarsun -> Nothing
    Yasa -> Nothing
    Yassic -> Nothing
    Yatay -> Nothing
    YateeZapotec -> Nothing
    YatzachiZapotec -> Nothing
    Yau_MorobeProvince -> Nothing
    Yau_SandaunProvince -> Nothing
    Yaul -> Nothing
    Yauma -> Nothing
    Yaur -> Nothing
    YautepecZapotec -> Nothing
    YauyosQuechua -> Nothing
    Yavitero -> Nothing
    Yawa -> Nothing
    Yawalapiti -> Nothing
    Yawanawa -> Nothing
    Yawarawarga -> Nothing
    Yaweyuha -> Nothing
    Yawijibaya -> Nothing
    Yawiyo -> Nothing
    Yawuru -> Nothing
    Yaygir -> Nothing
    Yazgulyam -> Nothing
    YecuatlaTotonac -> Nothing
    Yei -> Nothing
    Yekhee -> Nothing
    Yekora -> Nothing
    Yela -> Nothing
    Yele -> Nothing
    Yelmek -> Nothing
    Yelogu -> Nothing
    Yemba -> Nothing
    Yemsa -> Nothing
    Yendang -> Nothing
    Yeni -> Nothing
    Yeniche -> Nothing
    YepocapaSouthwesternCakchiquel -> Nothing
    Yerakai -> Nothing
    Yeretuar -> Nothing
    Yerong -> Nothing
    Yerukula -> Nothing
    Yeskwa -> Nothing
    YessanMayo -> Nothing
    Yetfa -> Nothing
    Yevanic -> Nothing
    Yeyi -> Nothing
    Yiddish -> Just $ ISO_639_2 "yid"
    YiddishSignLanguage -> Nothing
    Yidgha -> Nothing
    Yidiny -> Nothing
    Yigha -> Nothing
    Yil -> Nothing
    YilanCreole -> Nothing
    Yimas -> Nothing
    YimchungruNaga -> Nothing
    YinbawKaren -> Nothing
    Yinchia -> Nothing
    Yindjibarndi -> Nothing
    Yindjilandji -> Nothing
    Yine -> Nothing
    Yinggarda -> Nothing
    Yinglish -> Nothing
    Yinhawangka -> Nothing
    Yiningayi -> Nothing
    YintaleKaren -> Nothing
    Yinwum -> Nothing
    YirYoront -> Nothing
    Yirandali -> Nothing
    YirrkMel -> Nothing
    Yis -> Nothing
    YithaYitha -> Nothing
    Yiwom -> Nothing
    Yoba -> Nothing
    YocoboueDida -> Nothing
    Yogad -> Nothing
    Yoidik -> Nothing
    Yoke -> Nothing
    Yokuts -> Nothing
    Yola -> Nothing
    YolnguSignLanguage -> Nothing
    YoloxochitlMixtec -> Nothing
    Yom -> Nothing
    Yombe -> Just $ ISO_639_2 "kon"
    Yonaguni -> Nothing
    Yong -> Nothing
    YongbeiZhuang -> Nothing
    Yonggom -> Nothing
    YongnanZhuang -> Nothing
    Yopno -> Nothing
    Yora -> Nothing
    Yoron -> Nothing
    YortaYorta -> Nothing
    Yoruba -> Just $ ISO_639_2 "yor"
    Yos -> Nothing
    YosonduaMixtec -> Nothing
    Yotti -> Nothing
    YoujiangZhuang -> Nothing
    YouleJinuo -> Nothing
    YounuoBunu -> Nothing
    YoutWam -> Nothing
    Yoy -> Nothing
    Yuaga -> Nothing
    YuanjiangMojiangYi -> Nothing
    YucatanMaya -> Nothing
    YucatecMayaSignLanguage -> Nothing
    Yuchi -> Nothing
    YucuaneMixtec -> Nothing
    Yucuna -> Nothing
    YueChinese_Cantonese -> Nothing
    Yug -> Nothing
    Yugambal -> Nothing
    Yugh -> Nothing
    YugoslavianSignLanguage -> Nothing
    Yugul -> Nothing
    Yuhup -> Nothing
    Yuki -> Nothing
    Yukpa -> Nothing
    Yukuben -> Nothing
    Yulu -> Nothing
    Yuqui -> Nothing
    Yuracare -> Nothing
    Yurats -> Nothing
    Yurok -> Nothing
    Yuru -> Nothing
    Yuruti -> Nothing
    YutanduchiMixtec -> Nothing
    Yuwana -> Nothing
    Yuyu -> Nothing
    ZaachilaZapotec -> Nothing
    Zabana -> Nothing
    ZacatepecChatino -> Nothing
    Zaghawa -> Nothing
    Zaiwa -> Nothing
    Zakhring -> Nothing
    ZambianSignLanguage -> Nothing
    ZanGula -> Nothing
    Zanaki -> Nothing
    Zande_Specific -> Nothing
    Zangskari -> Nothing
    Zangwal -> Nothing
    ZanizaZapotec -> Nothing
    Zaparo -> Nothing
    Zapotec -> Nothing
    Zaramo -> Nothing
    Zari -> Nothing
    Zarma -> Nothing
    Zarphatic -> Nothing
    Zauzou -> Nothing
    Zay -> Nothing
    ZayeinKaren -> Nothing
    ZayseZergulla -> Nothing
    Zaza -> Just $ ISO_639_2 "zza"
    Zazao -> Nothing
    Zeem -> Nothing
    Zeeuws -> Nothing
    Zemba -> Nothing
    ZemeNaga -> Nothing
    Zemgalian -> Nothing
    Zenag -> Nothing
    Zenaga -> Just $ ISO_639_2 "zen"
    ZenzontepecChatino -> Nothing
    Zhaba -> Nothing
    ZhangZhung -> Nothing
    Zhire -> Nothing
    Zhoa -> Nothing
    Zhuang -> Just $ ISO_639_2 "zha"
    Zia -> Nothing
    Zialo -> Nothing
    Zigula -> Nothing
    Zimakani -> Nothing
    Zimba -> Nothing
    ZimbabweSignLanguage -> Nothing
    ZinacantanTzotzil -> Nothing
    Zinza -> Nothing
    Zire -> Nothing
    Zirenkel -> Nothing
    Ziriya -> Nothing
    Zizilivakan -> Nothing
    Zoe -> Nothing
    Zokhuo -> Nothing
    ZoogochoZapotec -> Nothing
    ZoroastrianDari -> Nothing
    ZotungChin -> Nothing
    Zou -> Nothing
    Zul -> Nothing
    Zula -> Nothing
    ZulgoGemzek -> Nothing
    Zulu -> Just $ ISO_639_2 "zul"
    Zumaya -> Nothing
    Zumbun -> Nothing
    Zuni -> Just $ ISO_639_2 "zun"
    ZuojiangZhuang -> Just $ ISO_639_2 "zha"
    Zyphe -> Nothing