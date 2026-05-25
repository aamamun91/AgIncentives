# Commodity and Country Mapping -----------------------------------------------
COUNTRY <- read.csv("./Mapping/COUNTRY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1") %>%
  dplyr::filter(WBCODE2015!="" & !(ISO3CODE %in% c("ALL","HIC","MIC","LIC")) )%>%
  select(ISO3CODE,REGIONFAOCODE,REGIONNAME,WBCODE2015, WBNAME2015, LISTNAME_EN)%>%
  mutate(COMPOSITE=paste0(REGIONFAOCODE," - ", WBCODE2015),
         COMPOSITE_LABEL=paste0(REGIONNAME," - ", WBNAME2015),
         REGIONFAOCODE=as.character(REGIONFAOCODE))

COUNTRY_A<-COUNTRY %>% select(ISO3CODE,REGIONFAOCODE,WBCODE2015,COMPOSITE)
REGION_WB<-COUNTRY_A%>% select(ISO3CODE,WBCODE2015)
REGION_LABEL0<-COUNTRY %>% select(ISO3CODE,LISTNAME_EN) %>%  na.omit()%>% mutate(REGION_CODE=ISO3CODE,REGION_DESC=LISTNAME_EN)%>%select(REGION_CODE,REGION_DESC)
REGION_LABEL1<-COUNTRY %>% select(WBCODE2015,WBNAME2015) %>% na.omit()%>%mutate(REGION_CODE=WBCODE2015,REGION_DESC=WBNAME2015)%>%select(REGION_CODE,REGION_DESC)
REGION_LABEL2<-COUNTRY %>% select(REGIONFAOCODE,REGIONNAME) %>% na.omit()%>%mutate(REGION_CODE=REGIONFAOCODE,REGION_DESC=REGIONNAME)%>%select(REGION_CODE,REGION_DESC)
REGION_LABEL3<-COUNTRY %>% select(COMPOSITE,COMPOSITE_LABEL) %>% na.omit()%>%mutate(REGION_CODE=COMPOSITE,REGION_DESC=COMPOSITE_LABEL)%>%select(REGION_CODE,REGION_DESC)
REGION_LABEL4<-data.frame("GLOBAL","GLOBAL")
names(REGION_LABEL4)<-c("REGION_CODE","REGION_DESC")
REGION_LABEL<-rbind(REGION_LABEL0,REGION_LABEL1,REGION_LABEL2,REGION_LABEL3,REGION_LABEL4)%>%distinct()


# short commodity mapping use for harmonization
COMMODITY_M <- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1")%>%
  select(AGCOMCODE,AGCOMNAME, IADB_CODE,MAFAP_CODE,OECD_CODE) %>%
  distinct()
# Commodity mapping use for aggregation
COMMODITY_A <- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1")%>%
  dplyr::filter(AGCOMCODE!="" & AGPROCODE!="TOTAL")%>%
  mutate(LVL1= ifelse(AGGRPCODE=="AnPr","Animal products","Crops"))%>%
  select(AGCOMCODE,AGPROCODE,AGGRPCODE,LVL1)%>%
  distinct()

PDCT_LABEL0<- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1") %>% 
  select(AGPROCODE,AGPRONAME) %>% mutate(PDCT_CODE=trimws(AGPROCODE,"b"),PDCT_DESC=trimws(AGPRONAME,"b"))%>%select(PDCT_CODE,PDCT_DESC)%>%distinct()
PDCT_LABEL1<- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE) %>% select(AGGRPCODE,AGGRPNAME) %>%
  mutate(
    AGGRPNAME=paste0(toupper(substr(AGGRPNAME, 1, 1)), tolower(substr(AGGRPNAME, 2, nchar(AGGRPNAME)))),
    PDCT_CODE=AGGRPCODE,PDCT_DESC=AGGRPNAME)  %>% dplyr::filter(AGGRPNAME != "Total")%>%
  select(PDCT_CODE,PDCT_DESC) %>% distinct()
PDCT_LABEL2<-read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1")  %>% 
  mutate(LVL1= ifelse(AGGRPCODE=="AnPr","Animal products","Crops")) %>% mutate(PDCT_CODE=LVL1,PDCT_DESC=LVL1)%>%
  select(PDCT_CODE,PDCT_DESC)%>% distinct()
PDCT_LABEL3<-data.frame("TOTAL","TOTAL")
names(PDCT_LABEL3)<-c("PDCT_CODE","PDCT_DESC")
PDCT_LABEL<-rbind(PDCT_LABEL0,PDCT_LABEL1,PDCT_LABEL2,PDCT_LABEL3)%>%distinct()%>%dplyr::filter(PDCT_CODE!="")

PDCT_COMMODITY<- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1")%>%
  select(AGPROCODE,IADB_CODE,MAFAP_CODE,OECD_CODE) %>%
  distinct()%>%
  group_by(AGPROCODE)
#ADD CONCATENATION PER CODE

#check the multi-mapping
CHECK_COMMODITYa<- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1") %>%
  select(AGPROCODE,AGPRONAME, AGCOMCODE,AGCOMNAME)%>%
  group_by(AGPROCODE)%>%
  summarize(OBS=n())%>%
  ungroup()%>%
  dplyr::filter(OBS>1)%>%
  left_join(PDCT_LABEL,by = c("AGPROCODE"="PDCT_CODE") )

#check all codes have a label
CHECK_COMMODITYb<-COMMODITY_A%>%
  left_join(PDCT_LABEL,by=c("AGPROCODE"="PDCT_CODE"))%>%
  mutate(LAB1=PDCT_DESC)%>% select(-PDCT_DESC)%>%
  left_join(PDCT_LABEL,by=c("AGGRPCODE"="PDCT_CODE"))%>%
  mutate(LAB2=PDCT_DESC)%>% select(-PDCT_DESC)%>%
  left_join(PDCT_LABEL,by=c("LVL1"="PDCT_CODE"))%>%
  mutate(LAB3=PDCT_DESC,
         LABT=coalesce(LAB1,LAB2,LAB3))

checkX<-CHECK_COMMODITYb%>%select(LABT,AGPROCODE)%>%distinct()
nrow(checkX)

print("Check multilabel")
print (nrow(COMMODITY_A)-nrow(CHECK_COMMODITYb))
print("NA multilabel")
CHECK_COMMODITYb<-CHECK_COMMODITYb %>% dplyr::filter(is.na(LABT))
print (nrow(CHECK_COMMODITYb))

PDCT_FULL<- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1")%>%
  rename(PDCT_CODE=AGPROCODE,PDCT2=AGGRPNAME)%>%
  mutate(PDCT0="Total",PDCT1=case_when(AGGRPCODE=="AnPr"~"Animal products",
                                       AGGRPCODE=="NAL"~"NAL",
                                       TRUE~"Crops"))%>%
  select(PDCT_CODE,PDCT0, PDCT1, PDCT2)%>%distinct()

REGI_FULL<-COUNTRY%>%rename(REGION_CODE=ISO3CODE, REGION_CONTINENT=REGIONNAME, 
                            REGION_INCOME=WBNAME2015)%>%
  mutate(REGION_ALL="World",REGION_DVPT=ifelse(WBCODE2015=="HIC","High Income","Low and Middle Income"))%>%
  select(REGION_CODE,REGION_ALL,REGION_DVPT,REGION_INCOME,REGION_CONTINENT)


HS_MAPPING<-read_xls(paste0(".\\mapping\\fao_mapping.xls"))%>%
  mutate( `HS07 Code`= ifelse(nchar(`HS07 Code`)==5, paste0("0",`HS07 Code`),paste0(`HS07 Code`) ),
          `HS12 Code`= ifelse(nchar(`HS12 Code`)==5, paste0("0",`HS12 Code`),paste0(`HS12 Code`) ))%>%
  rename(AGCOMCODE=`Item Code`,H2012=`HS12 Code`)%>%mutate(AGCOMCODE=as.character(AGCOMCODE))%>%
  separate(H2012, into = paste("V", 1:10, sep = "_"), ",", extra = "merge",fill="right")%>%
  select(AGCOMCODE,starts_with("V"))%>%gather(key,HS2012,-AGCOMCODE)%>%select(-key)%>%
  na.omit()%>%left_join(COMMODITY_A)%>%
  select(AGPROCODE,HS2012)%>%na.omit()


# fao.list.country <- FAOcountryProfile %>% select(ISO3_CODE, FAO_TABLE_NAME) %>% na.omit() %>% 
#   mutate(FAO_TABLE_NAME = case_when(FAO_TABLE_NAME=='China'~'China, mainland',
#                                     FAO_TABLE_NAME=='Taiwan, Province of China'~'China, Taiwan Province of',
#                                     FAO_TABLE_NAME=='the Democratic Republic of the Congo'~'Democratic Republic of the Congo',
#                                     FAO_TABLE_NAME=='the Dominican Republic'~'Dominican Republic',
#                                     FAO_TABLE_NAME=='Swaziland'~'Eswatini',
#                                     FAO_TABLE_NAME=='the Netherlands'~'Netherlands',
#                                     FAO_TABLE_NAME=='the Niger'~'Niger',
#                                     FAO_TABLE_NAME=='the Sudan'~'Sudan',
#                                     FAO_TABLE_NAME=='the United Republic of Tanzania'~'United Republic of Tanzania',
#                                     FAO_TABLE_NAME=='the United States of America'~'United States of America',
#                                     FAO_TABLE_NAME=='The former Yugoslav Republic of Macedonia'~'North Macedonia',
#                                     FAO_TABLE_NAME=='Occupied Palestinian Territory'~'Palestine',
#                                     FAO_TABLE_NAME=='the Philippines'~'Philippines',
#                                     FAO_TABLE_NAME=='the Russian Federation'~'Russian Federation',
#                                     FAO_TABLE_NAME=='the United Kingdom of Great Britain and Northern Ireland'~'United Kingdom of Great Britain and Northern Ireland',
#                                     FAO_TABLE_NAME=='the United Arab Emirates'~'United Arab Emirates',
#                                     FAO_TABLE_NAME=='the Syrian Arab Republic'~'Syrian Arab Republic',
#                                     FAO_TABLE_NAME=="the Lao People's Democratic Republic"~"Lao People's Democratic Republic",
#                                     FAO_TABLE_NAME=='the Gambia'~'Gambia',
#                                     FAO_TABLE_NAME=='the Bahamas'~'Bahamas',
#                                     FAO_TABLE_NAME=='the Comoros'~'Comoros',
#                                     FAO_TABLE_NAME=='Micronesia (Federated States of)'~'Micronesia',
#                                     FAO_TABLE_NAME=='French Polynesia'~'Polynesia',
#                                     FAO_TABLE_NAME=='the Czech Republic'~'Czechia',
#                                     FAO_TABLE_NAME=='Turkey'~"Türkiye",
#                                     TRUE~FAO_TABLE_NAME)) %>% rename(CountryCode=ISO3_CODE)
