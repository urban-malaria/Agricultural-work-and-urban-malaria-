library(readxl)
library(haven)
library(tidyverse)


#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
DataDir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'DHS')
MISDataDir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'MIS')

## function
#identify DHS folder function 
id.folder <- function(path, general_pattern) {
  tmpshot <- fileSnapshot(path = path, full.names = TRUE)
  folders <- rownames(tmpshot$info)[grepl(general_pattern, rownames(tmpshot$info))== TRUE]
  return(folders)
}

#Angola
id.folder(DataDir, "AO")
AO_2015_dhs <- read_dta(file.path(DataDir, "AO_2015-16_DHS_03152022_1237_141460/AOPR71DT", "AOPR71FL.dta")) %>% 
                mutate(country_year= "AO_2015")
AO_2011_mis <- read_dta(file.path(MISDataDir, "AO_2011_MIS_03152022_1238_141460/AOPR62DT", "AOPR62FL.dta")) %>% 
  mutate(country_year= "AO_2011")
#HR files
AO_2015_dhs_hr <- read_dta(file.path(DataDir, "AO_2015-16_DHS_03152022_1237_141460/AOHR71DT", "AOHR71FL.dta")) %>% 
  mutate(country_year= "AO_2015")
AO_2011_mis_hr <- read_dta(file.path(MISDataDir, "AO_2011_MIS_03152022_1238_141460/AOHR62DT", "AOHR62FL.dta")) %>% 
  mutate(country_year= "AO_2011")

#Benin
id.folder(DataDir, "BJ")
BJ_2012_dhs <- read_dta(file.path(DataDir, "BJ_2011-12_DHS_03152022_1240_141460/BJPR61DT", "BJPR61FL.dta")) %>% 
  mutate(country_year= "BJ_2012")
BJ_2017_dhs <- read_dta(file.path(DataDir, "BJ_2017-18_DHS_09202021_1431_141460/BJPR71DT", "BJPR71FL.dta")) %>% 
  mutate(country_year= "BJ_2017")
#HR files
BJ_2012_dhs_hr <- read_dta(file.path(DataDir, "BJ_2011-12_DHS_03152022_1240_141460/BJHR61DT", "BJHR61FL.dta")) %>% 
  mutate(country_year= "BJ_2012")
BJ_2017_dhs_hr <- read_dta(file.path(DataDir, "BJ_2017-18_DHS_09202021_1431_141460/BJHR71DT", "BJHR71FL.dta")) %>% 
  mutate(country_year= "BJ_2017")

#Burkina Faso
BF_2010_dhs <- read_dta(file.path(DataDir, "BF_2010_DHS_03152022_1241_141460/BFPR62DT", "BFPR62FL.dta")) %>% 
  mutate(country_year= "BF_2010")
BF_2014_dhs <- read_dta(file.path(DataDir, "BF_2014_MIS_09202021_1434_141460/BFPR71DT", "BFPR71FL.dta")) %>% 
  mutate(country_year= "BF_2014")
BF_2017_dhs <- read_dta(file.path(DataDir, "BF_2017-18_MIS_09202021_1434_141460/BFPR7ADT", "BFPR7AFL.dta")) %>% 
  mutate(country_year= "BF_2017")
#HR files
BF_2010_dhs_hr <- read_dta(file.path(DataDir, "BF_2010_DHS_03152022_1241_141460/BFHR62DT", "BFHR62FL.dta")) %>% 
  mutate(country_year= "BF_2010")
BF_2014_dhs_hr <- read_dta(file.path(DataDir, "BF_2014_MIS_09202021_1434_141460/BFHR71DT", "BFHR71FL.dta")) %>% 
  mutate(country_year= "BF_2014")
BF_2017_dhs_hr <- read_dta(file.path(DataDir, "BF_2017-18_MIS_09202021_1434_141460/BFHR7ADT", "BFHR7AFL.dta")) %>% 
  mutate(country_year= "BF_2017")

#Burundi
id.folder(DataDir, "BU")
BU_2012_mis <- read_dta(file.path(DataDir, "BU_2012_MIS_03302022_1851_141460/BUPR6ADT", "BUPR6AFL.dta")) %>% 
  mutate(country_year= "BU_2012")
BU_2016_dhs <- read_dta(file.path(DataDir, "BU_2016-17_DHS_09172021_1823_141460/BUPR71DT", "BUPR71FL.dta")) %>% 
  mutate(country_year= "BU_2016")
#HR files
BU_2012_mis_hr <- read_dta(file.path(DataDir, "BU_2012_MIS_03302022_1851_141460/BUHR6ADT", "BUHR6AFL.dta")) %>% 
  mutate(country_year= "BU_2012")
BU_2016_dhs_hr <- read_dta(file.path(DataDir, "BU_2016-17_DHS_09172021_1823_141460/BUHR71DT", "BUHR71FL.dta")) %>% 
  mutate(country_year= "BU_2016")

#Cameroon
id.folder(DataDir, "CM") 
CM_2011_dhs <- read_dta(file.path(DataDir, "CM_2011_DHS_03172023_234_191741/CMPR61DT", "CMPR61FL.dta")) %>% 
  mutate(country_year= "CM_2011")
CM_2018_dhs <- read_dta(file.path(DataDir, "CM_2018_DHS_09172021_1832_141460/CMPR71DT", "CMPR71FL.dta")) %>% 
  mutate(country_year= "CM_2018")
#HR files
CM_2011_dhs_hr <- read_dta(file.path(DataDir, "CM_2011_DHS_03172023_234_191741/CMHR61DT", "CMHR61FL.dta")) %>% 
  mutate(country_year= "CM_2011")
CM_2018_dhs_hr <- read_dta(file.path(DataDir, "CM_2018_DHS_09172021_1832_141460/CMHR71DT", "CMHR71FL.dta")) %>% 
  mutate(country_year= "CM_2018")

#Congo Democratic Republic
id.folder(DataDir, "CD") 
CD_2013_dhs <- read_dta(file.path(DataDir, "CD_2013-14_DHS_03152022_1247_141460/CDPR61DT", "CDPR61FL.dta")) %>% 
  mutate(country_year= "CD_2013")

#Cote d'Ivoire
id.folder(DataDir, "CI") 
CI_2012_dhs <- read_dta(file.path(DataDir, "CI_2011-12_DHS_09172021_1847_141460/CIPR62DT", "CIPR62FL.dta")) %>% 
  mutate(country_year= "CI_2012")

#Gambia
GM_2013_dhs <- read_dta(file.path(DataDir, "GM_2013_DHS_09212021_1454_141460/GMPR61DT", "GMPR61FL.dta")) %>% 
  mutate(country_year= "GM_2013")
GM_2019_dhs <- read_dta(file.path(DataDir, "GM_2019-20_DHS_03052022_62_141460/GMPR81DT", "GMPR81FL.dta")) %>% 
  mutate(country_year= "GM_2019")
GM_2013_dhs_hr <- read_dta(file.path(DataDir, "GM_2013_DHS_09212021_1454_141460/GMHR61DT", "GMHR61FL.dta")) %>% 
  mutate(country_year= "GM_2013")
GM_2019_dhs_hr <- read_dta(file.path(DataDir, "GM_2019-20_DHS_03052022_62_141460/GMHR81DT", "GMHR81FL.dta")) %>% 
  mutate(country_year= "GM_2019")

#Ghana
id.folder(DataDir, "GH") 
GH_2014_dhs <- read_dta(file.path(DataDir, "GH_2014_DHS_03302022_1926_141460/GHPR72DT", "GHPR72FL.dta")) %>% 
  mutate(country_year= "GH_2014")
GH_2016_mis <- read_dta(file.path(DataDir, "GH_2016_MIS_03302022_1925_141460/GHPR7BDT", "GHPR7BFL.dta")) %>% 
  mutate(country_year= "GH_2016")
GH_2019_mis <- read_dta(file.path(DataDir, "GH_2019_MIS_03052022_930_141460/GHPR82DT", "GHPR82FL.dta")) %>% 
  mutate(country_year= "GH_2019")
#HR datasets
GH_2014_dhs_hr <- read_dta(file.path(DataDir, "GH_2014_DHS_03302022_1926_141460/GHHR72DT", "GHHR72FL.dta")) %>% 
  mutate(country_year= "GH_2014")
GH_2016_mis_hr <- read_dta(file.path(DataDir, "GH_2016_MIS_03302022_1925_141460/GHHR7BDT", "GHHR7BFL.dta")) %>% 
  mutate(country_year= "GH_2016")
GH_2019_mis_hr <- read_dta(file.path(DataDir, "GH_2019_MIS_03052022_930_141460/GHHR82DT", "GHHR82FL.dta")) %>% 
  mutate(country_year= "GH_2019")

#Guinea
id.folder(DataDir, "GN") 
GN_2012_dhs <- read_dta(file.path(DataDir, "GN_2012_DHS_03302022_1930_141460/GNPR62DT", "GNPR62FL.dta")) %>% 
  mutate(country_year= "GN_2012")
GN_2021_mis <- read_dta(file.path(DataDir, "GN_2021_MIS_05192023_221_191741/GNPR81DT", "GNPR81FL.dta")) %>% 
  mutate(country_year= "GN_2021")
#HR files
GN_2012_dhs_hr <- read_dta(file.path(DataDir, "GN_2012_DHS_03302022_1930_141460/GNHR62DT", "GNHR62FL.dta")) %>% 
  mutate(country_year= "GN_2012")
GN_2021_mis_hr <- read_dta(file.path(DataDir, "GN_2021_MIS_05192023_221_191741/GNHR81DT", "GNHR81FL.dta")) %>% 
  mutate(country_year= "GN_2021")

#Kenya
id.folder(DataDir, "KE") 
KE_2015_mis <- read_dta(file.path(DataDir, "KE_2015_MIS_03302022_1932_141460/KEPR7ADT", "KEPR7AFL.dta")) %>% 
  mutate(country_year= "KE_2015")
KE_2020_mis <- read_dta(file.path(DataDir, "KE_2020_MIS_03052022_931_141460/KEPR81DT", "KEPR81FL.dta")) %>% 
  mutate(country_year= "KE_2020")
#HR files
KE_2015_mis_hr <- read_dta(file.path(DataDir, "KE_2015_MIS_03302022_1932_141460/KEHR7ADT", "KEHR7AFL.dta")) %>% 
  mutate(country_year= "KE_2015")
KE_2020_mis_hr <- read_dta(file.path(DataDir, "KE_2020_MIS_03052022_931_141460/KEHR81DT", "KEHR81FL.dta")) %>% 
  mutate(country_year= "KE_2020")

#Liberia
id.folder(DataDir, "LB") 
LB_2011_mis <- read_dta(file.path(MISDataDir, "LB_2011_MIS_03152022_131_141460/LBPR61DT", "LBPR61FL.dta")) %>% 
  mutate(country_year= "LB_2011")
LB_2016_mis <- read_dta(file.path(DataDir, "LB_2016_MIS_03302022_1940_141460/LBPR71DT", "LBPR71FL.dta")) %>% 
  mutate(country_year= "LB_2016")
#HR files
LB_2011_mis_hr <- read_dta(file.path(MISDataDir, "LB_2011_MIS_03152022_131_141460/LBHR61DT", "LBHR61FL.dta")) %>% 
  mutate(country_year= "LB_2011")
LB_2016_mis_hr <- read_dta(file.path(DataDir, "LB_2016_MIS_03302022_1940_141460/LBHR71DT", "LBHR71FL.dta")) %>% 
  mutate(country_year= "LB_2016")


#Madagascar
id.folder(DataDir, "MD") 
MD_2011_mis <- read_dta(file.path(MISDataDir, "MD_2011_MIS_03152022_133_141460/MDPR61DT", "MDPR61FL.dta")) %>% 
  mutate(country_year= "MD_2011")
MD_2013_mis <- read_dta(file.path(DataDir, "MD_2013_MIS_03302022_1943_141460/MDPR6ADT", "MDPR6AFL.dta")) %>% 
  mutate(country_year= "MD_2013")
MD_2016_mis <- read_dta(file.path(DataDir, "MD_2016_MIS_03052022_937_141460/MDPR71DT", "MDPR71FL.dta")) %>% 
  mutate(country_year= "MD_2016")
MD_2021_dhs <- read_dta(file.path(DataDir, "MD_2021_DHS_06062023_25_191741/MDPR81DT", "MDPR81FL.dta")) %>% 
  mutate(country_year= "MD_2021")
#HR files
MD_2011_mis_hr <- read_dta(file.path(MISDataDir, "MD_2011_MIS_03152022_133_141460/MDHR61DT", "MDHR61FL.dta")) %>% 
  mutate(country_year= "MD_2011")
MD_2013_mis_hr <- read_dta(file.path(DataDir, "MD_2013_MIS_03302022_1943_141460/MDHR6ADT", "MDHR6AFL.dta")) %>% 
  mutate(country_year= "MD_2013")
MD_2016_mis_hr <- read_dta(file.path(DataDir, "MD_2016_MIS_03052022_937_141460/MDHR71DT", "MDHR71FL.dta")) %>% 
  mutate(country_year= "MD_2016")
MD_2021_dhs_hr <- read_dta(file.path(DataDir, "MD_2021_DHS_06062023_25_191741/MDHR81DT", "MDHR81FL.dta")) %>% 
  mutate(country_year= "MD_2021")

#Malawi
id.folder(DataDir, "MW") 
MW_2012_mis <- read_dta(file.path(DataDir, "MW_2012_MIS_03302022_1954_141460/MWPR6ADT", "MWPR6AFL.dta")) %>% 
  mutate(country_year= "MW_2012")
MW_2014_mis <- read_dta(file.path(DataDir, "MW_2014_MIS_03302022_1949_141460/MWPR72DT", "MWPR72FL.dta")) %>% 
  mutate(country_year= "MW_2014")
MW_2017_mis <- read_dta(file.path(DataDir, "MW_2017_MIS_03052022_945_141460/MWPR7IDT", "MWPR7IFL.dta")) %>% 
  mutate(country_year= "MW_2017")
#HR files
MW_2012_mis_hr <- read_dta(file.path(DataDir, "MW_2012_MIS_03302022_1954_141460/MWHR6ADT", "MWHR6AFL.dta")) %>% 
  mutate(country_year= "MW_2012")
MW_2014_mis_hr <- read_dta(file.path(DataDir, "MW_2014_MIS_03302022_1949_141460/MWHR72DT", "MWHR72FL.dta")) %>% 
  mutate(country_year= "MW_2014")
MW_2017_mis_hr <- read_dta(file.path(DataDir, "MW_2017_MIS_03052022_945_141460/MWHR7IDT", "MWHR7IFL.dta")) %>% 
  mutate(country_year= "MW_2017")

#Mali
id.folder(DataDir, "ML") 
ML_2012_dhs <- read_dta(file.path(DataDir, "ML_2012-13_DHS_03152022_1312_141460/MLPR6ADT", "MLPR6AFL.dta")) %>% 
  mutate(country_year= "ML_2012")
ML_2015_mis <- read_dta(file.path(DataDir, "ML_2015_MIS_03302022_200_141460/MLPR72DT", "MLPR72FL.dta")) %>% 
  mutate(country_year= "ML_2015")
ML_2018_dhs <- read_dta(file.path(DataDir, "ML_2018_DHS_03052022_637_141460/MLPR7ADT", "MLPR7AFL.dta")) %>% 
  mutate(country_year= "ML_2018")
ML_2021_dhs <- read_dta(file.path(DataDir, "ML_2021_MIS_06062023_27_191741/MLPR81DT", "MLPR81FL.dta")) %>% 
  mutate(country_year= "ML_2021")
#HR files
ML_2012_dhs_hr <- read_dta(file.path(DataDir, "ML_2012-13_DHS_03152022_1312_141460/MLHR6ADT", "MLHR6AFL.dta")) %>% 
  mutate(country_year= "ML_2012")
ML_2015_mis_hr  <- read_dta(file.path(DataDir, "ML_2015_MIS_03302022_200_141460/MLHR72DT", "MLHR72FL.dta")) %>% 
  mutate(country_year= "ML_2015")
ML_2018_dhs_hr  <- read_dta(file.path(DataDir, "ML_2018_DHS_03052022_637_141460/MLHR7ADT", "MLHR7AFL.dta")) %>% 
  mutate(country_year= "ML_2018")
ML_2021_dhs_hr  <- read_dta(file.path(DataDir, "ML_2021_MIS_06062023_27_191741/MLHR81DT", "MLHR81FL.dta")) %>% 
  mutate(country_year= "ML_2021")

#Mauritania
id.folder(DataDir, "MR") 
MR_2020_dhs <- read_dta(file.path(DataDir, "MR_2019-21_DHS_03162023_243_191741/MRPR71DT", "MRPR71FL.dta")) %>% 
  mutate(country_year= "MR_2020")

#Mozambique
id.folder(DataDir, "MZ") 
MZ_2011_dhs <- read_dta(file.path(DataDir, "MZ_2011_DHS_03172023_37_191741/MZPR62DT", "MZPR62FL.dta")) %>% 
  mutate(country_year= "MZ_2011")
MZ_2015_dhs <- read_dta(file.path(DataDir, "MZ_2015_AIS_03302022_206_141460/MZPR71DT", "MZPR71FL.dta")) %>% 
  mutate(country_year= "MZ_2015")
MZ_2018_dhs <- read_dta(file.path(DataDir, "MZ_2018_MIS_03052022_936_141460/MZPR7ADT", "MZPR7AFL.dta")) %>% 
  mutate(country_year= "MZ_2018")
#HR files
MZ_2011_dhs_hr <- read_dta(file.path(DataDir, "MZ_2011_DHS_03172023_37_191741/MZHR62DT", "MZHR62FL.dta")) %>% 
  mutate(country_year= "MZ_2011")
MZ_2015_dhs_hr <- read_dta(file.path(DataDir, "MZ_2015_AIS_03302022_206_141460/MZHR71DT", "MZHR71FL.dta")) %>% 
  mutate(country_year= "MZ_2015")
MZ_2018_dhs_hr <- read_dta(file.path(DataDir, "MZ_2018_MIS_03052022_936_141460/MZHR7ADT", "MZHR7AFL.dta")) %>% 
  mutate(country_year= "MZ_2018")

#Nigeria
id.folder(DataDir, "NG") 
NG_2010_mis <- read_dta(file.path(MISDataDir, "NG_2010_MIS_03152022_1337_141460/NGPR61DT", "NGPR61FL.dta")) %>% 
  mutate(country_year= "NG_2010")
NG_2015_mis <- read_dta(file.path(DataDir, "NG_2015_MIS_06192019/NG_2015_MIS_06192019/NGPR71DT", "NGPR71FL.dta")) %>% 
  mutate(country_year= "NG_2015")
NG_2018_dhs <- read_dta(file.path(DataDir, "NG_2018_DHS_05162022_628_141460/NGPR7BDT", "NGPR7BFL.dta")) %>% 
  mutate(country_year= "NG_2018")
NG_2021_mis <- read_dta(file.path(DataDir, "NG_2021_MIS_06062023_29_191741/NGPR81DT", "NGPR81FL.dta")) %>% 
  mutate(country_year= "NG_2021")
#   HR data 
NG_2010_mis_hr <- read_dta(file.path(MISDataDir, "NG_2010_MIS_03152022_1337_141460/NGHR61DT", "NGHR61FL.dta")) %>% 
  mutate(country_year= "NG_2010")
NG_2015_mis_hr <- read_dta(file.path(DataDir, "NG_2015_MIS_06192019/NG_2015_MIS_06192019/NGHR71DT", "NGHR71FL.dta")) %>% 
  mutate(country_year= "NG_2015")
NG_2018_dhs_hr <- read_dta(file.path(DataDir, "NG_2018_DHS_05162022_628_141460/NGHR7BDT", "NGHR7BFL.dta")) %>% 
  mutate(country_year= "NG_2018")
NG_2021_mis_hr <- read_dta(file.path(DataDir, "NG_2021_MIS_06062023_29_191741/NGHR81DT", "NGHR81FL.dta")) %>% 
  mutate(country_year= "NG_2021")

#Rwanda
id.folder(DataDir, "RW") 
RW_2010_mis <- read_dta(file.path(DataDir, "RW_2010_DHS_03172023_319_191741/RWPR61DT", "RWPR61FL.dta")) %>% 
  mutate(country_year= "RW_2010")
RW_2015_dhs <- read_dta(file.path(DataDir, "RW_2014-15_DHS_03302022_2014_141460/RWPR70DT", "RWPR70FL.dta")) %>% 
  mutate(country_year= "RW_2015")
RW_2017_mis <- read_dta(file.path(DataDir, "RW_2017_MIS_09212021_1950_141460/RWPR7ADT", "RWPR7AFL.dta")) %>% 
  mutate(country_year= "RW_2017")
RW_2019_dhs <- read_dta(file.path(DataDir, "RW_2019-20_DHS_03152022_1343_141460/RWPR81DT", "RWPR81FL.dta")) %>% 
  mutate(country_year= "RW_2019")
#HR files
RW_2010_mis_hr <- read_dta(file.path(DataDir, "RW_2010_DHS_03172023_319_191741/RWHR61DT", "RWHR61FL.dta")) %>% 
  mutate(country_year= "RW_2010")
RW_2015_dhs_hr <- read_dta(file.path(DataDir, "RW_2014-15_DHS_03302022_2014_141460/RWHR70DT", "RWHR70FL.dta")) %>% 
  mutate(country_year= "RW_2015")
RW_2017_mis_hr <- read_dta(file.path(DataDir, "RW_2017_MIS_09212021_1950_141460/RWHR7ADT", "RWHR7AFL.dta")) %>% 
  mutate(country_year= "RW_2017")
RW_2019_dhs_hr <- read_dta(file.path(DataDir, "RW_2019-20_DHS_03152022_1343_141460/RWHR81DT", "RWHR81FL.dta")) %>% 
  mutate(country_year= "RW_2019")

#Senegal
id.folder(DataDir, "SN") 
SN_2010_dhs <- read_dta(file.path(DataDir, "SN_2010-11_DHS_03162023_230_191741/SNPR61DT", "SNPR61FL.dta")) %>% 
  mutate(country_year= "SN_2010")
SN_2012_dhs <- read_dta(file.path(DataDir, "SN_2012-13_CONTINUOUSDHS_03302022_2026_141460/SNPR6DDT", "SNPR6DFL.dta")) %>% 
  mutate(country_year= "SN_2012")
SN_2014_dhs <- read_dta(file.path(DataDir, "SN_2014_CONTINUOUSDHS_03302022_2025_141460/SNPR70DT/SNPR70DT", "SNPR70FL.dta")) %>% 
  mutate(country_year= "SN_2014")
SN_2015_dhs <- read_dta(file.path(DataDir, "SN_2015_CONTINUOUSDHS_03302022_2023_141460/SNPR7HDT", "SNPR7HFL.dta")) %>% 
  mutate(country_year= "SN_2015")
SN_2016_dhs <- read_dta(file.path(DataDir, "SN_2016_CONTINUOUSDHS_03302022_2022_141460/SNPR7IDT", "SNPR7IFL.dta")) %>% 
  mutate(country_year= "SN_2016")
SN_2017_dhs <- read_dta(file.path(DataDir, "SN_2017_CONTINUOUSDHS_03302022_2021_141460/SNPR7ZDT", "SNPR7ZFL.dta")) %>% 
  mutate(country_year= "SN_2017")
SN_2020_mis <- read_dta(file.path(DataDir, "SN_2020-21_MIS_03162023_239_191741/SNPR8IDT", "SNPR8IFL.dta")) %>% 
  mutate(country_year= "SN_2020")
#HR files
SN_2010_dhs_hr <- read_dta(file.path(DataDir, "SN_2010-11_DHS_03162023_230_191741/SNHR61DT", "SNHR61FL.dta")) %>% 
  mutate(country_year= "SN_2010")
SN_2012_dhs_hr  <- read_dta(file.path(DataDir, "SN_2012-13_CONTINUOUSDHS_03302022_2026_141460/SNHR6DDT", "SNHR6DFL.dta")) %>% 
  mutate(country_year= "SN_2012")
SN_2014_dhs_hr  <- read_dta(file.path(DataDir, "SN_2014_CONTINUOUSDHS_03302022_2025_141460/SNHR70DT/SNHR70DT", "SNHR70FL.dta")) %>% 
  mutate(country_year= "SN_2014")
SN_2015_dhs_hr  <- read_dta(file.path(DataDir, "SN_2015_CONTINUOUSDHS_03302022_2023_141460/SNHR7HDT", "SNHR7HFL.dta")) %>% 
  mutate(country_year= "SN_2015")
SN_2016_dhs_hr  <- read_dta(file.path(DataDir, "SN_2016_CONTINUOUSDHS_03302022_2022_141460/SNHR7IDT", "SNHR7IFL.dta")) %>% 
  mutate(country_year= "SN_2016")
SN_2017_dhs_hr  <- read_dta(file.path(DataDir, "SN_2017_CONTINUOUSDHS_03302022_2021_141460/SNHR7ZDT", "SNHR7ZFL.dta")) %>% 
  mutate(country_year= "SN_2017")
SN_2020_mis_hr  <- read_dta(file.path(DataDir, "SN_2020-21_MIS_03162023_239_191741/SNHR8IDT", "SNHR8IFL.dta")) %>% 
  mutate(country_year= "SN_2020")

#Tanzania
id.folder(DataDir, "TZ") 
TZ_2012_dhs <- read_dta(file.path(DataDir, "TZ_2011-12_AIS_03302022_2037_141460/TZPR6ADT", "TZPR6AFL.dta")) %>% 
  mutate(country_year= "TZ_2012")
TZ_2015_dhs <- read_dta(file.path(DataDir, "TZ_2015-16_DHS_09212021_1959_141460/TZPR7BDT", "TZPR7BFL.dta")) %>% 
  mutate(country_year= "TZ_2015")
TZ_2017_mis <- read_dta(file.path(DataDir, "TZ_2017_MIS_03052022_939_141460/TZPR7IDT", "TZPR7IFL.dta")) %>% 
  mutate(country_year= "TZ_2017")
#HR files
TZ_2012_dhs_hr <- read_dta(file.path(DataDir, "TZ_2011-12_AIS_03302022_2037_141460/TZHR6ADT", "TZHR6AFL.dta")) %>% 
  mutate(country_year= "TZ_2012")
TZ_2015_dhs_hr <- read_dta(file.path(DataDir, "TZ_2015-16_DHS_09212021_1959_141460/TZHR7BDT", "TZHR7BFL.dta")) %>% 
  mutate(country_year= "TZ_2015")
TZ_2017_mis_hr <- read_dta(file.path(DataDir, "TZ_2017_MIS_03052022_939_141460/TZHR7IDT", "TZHR7IFL.dta")) %>% 
  mutate(country_year= "TZ_2017")

#Togo
id.folder(DataDir, "TG") 
TG_2013_dhs <- read_dta(file.path(DataDir, "TG_2013-14_DHS_09212021_2015_141460/TGPR61DT", "TGPR61FL.dta")) %>% 
  mutate(country_year= "TG_2013")
TG_2017_mis <- read_dta(file.path(DataDir, "TG_2017_MIS_03052022_938_141460/TGPR71DT", "TGPR71FL.dta")) %>% 
  mutate(country_year= "TG_2017")
#HR files
TG_2013_dhs_hr <- read_dta(file.path(DataDir, "TG_2013-14_DHS_09212021_2015_141460/TGHR61DT", "TGHR61FL.dta")) %>% 
  mutate(country_year= "TG_2013")
TG_2017_mis_hr <- read_dta(file.path(DataDir, "TG_2017_MIS_03052022_938_141460/TGHR71DT", "TGHR71FL.dta")) %>% 
  mutate(country_year= "TG_2017")

#Uganda
id.folder(DataDir, "UG") 
UG_2014_mis <- read_dta(file.path(DataDir, "UG_2014-15_MIS_03302022_2044_141460/UGPR72DT", "UGPR72FL.dta")) %>% 
  mutate(country_year= "UG_2014")
UG_2016_dhs <- read_dta(file.path(DataDir, "UG_2016_DHS_09212021_204_141460/UGPR7BDT", "UGPR7BFL.dta")) %>% 
  mutate(country_year= "UG_2016")
UG_2018_mis <- read_dta(file.path(DataDir, "UG_2018-19_MIS_03052022_940_141460/UGPR7IDT", "UGPR7IFL.dta")) %>% 
  mutate(country_year= "UG_2018")
#HR files
UG_2014_mis_hr <- read_dta(file.path(DataDir, "UG_2014-15_MIS_03302022_2044_141460/UGHR72DT", "UGHR72FL.dta")) %>% 
  mutate(country_year= "UG_2014")
UG_2016_dhs_hr <- read_dta(file.path(DataDir, "UG_2016_DHS_09212021_204_141460/UGHR7BDT", "UGHR7BFL.dta")) %>% 
  mutate(country_year= "UG_2016")
UG_2018_mis_hr <- read_dta(file.path(DataDir, "UG_2018-19_MIS_03052022_940_141460/UGHR7IDT", "UGHR7IFL.dta")) %>% 
  mutate(country_year= "UG_2018")


#Zambia
id.folder(DataDir, "ZM") 
#2010 and 2012- we don't have DHS data for
#Neither 2013 nor 2018 DHS collected Pf prevalence, they have malaria indicators but not PfPR
ZM_2013_dhs <- read_dta(file.path(DataDir, "ZM_2013-14_DHS_09162021_184_141460/ZMPR61DT", "ZMPR61FL.dta")) %>%  #missing folder
  mutate(country_year= "ZM_2013")
ZM_2018_dhs <- read_dta(file.path(DataDir, "ZM_2018_DHS_09162021_181_141460/ZMPR71DT", "ZMPR71FL.dta")) %>% #missing DTA file
  mutate(country_year= "ZM_2018")

