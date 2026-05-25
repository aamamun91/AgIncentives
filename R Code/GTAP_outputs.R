library(tidyverse)

dropdir<-Sys.getenv(c("DROPDIR"))

#source(file=paste0(dropdir,"/R_projects/R4GAMS.R"))

extract.year<-c("2012","2013","2014","2015","2016")

GamsProject<-"MIRAGRODEP_EnviWB"
PathGams<-paste0(dropdir,"/GAMS_projects/",GamsProject)

map_GTAP_IAI <- read.csv("./Mapping/GTAP_I.csv", stringsAsFactors=FALSE)%>%
  mutate(GTAP_I=toupper(trimws(GTAP_I,"b")),
         PDCT_CODE=trimws(PDCT_CODE,"b"))

map_ISO<-read.csv(file=paste0(PathGams,"/Agreg/map_ISO.csv"),header=TRUE, sep=",",stringsAsFactors=FALSE) %>% mutate(ISO3=toupper(trimws(ISO3,"b")),R=toupper(trimws(R,"b")))
map_GTAP<-read.csv(file=paste0(PathGams,"/Agreg/map_GTAP.csv"),header=TRUE, sep=",",stringsAsFactors=FALSE) %>% mutate(GTAP_I=toupper(trimws(GTAP_I,"b")),I=toupper(trimws(I,"b")))

PRE_AG<-TPUBLICVIEW%>%filter(CAT=="COUNTRY_PRODUCT" & YEAR %in% extract.year)%>%
        left_join(map_ISO,by =c("REGION_CODE" = "ISO3"))%>%
        left_join(map_GTAP_IAI)%>%
        left_join(map_GTAP)%>%
        mutate(TS="NRPM",
               TS=ifelse(PDCT_CODE=="cXFCHN","NRPX",paste0(TS)))%>%
        group_by(I,R,TS)%>%
        summarize(VPFG=sum(VP_PROP,na.rm=TRUE),VPRP=sum(VP_REFP,na.rm=TRUE))%>%
        ungroup()%>%
        mutate(NRP=VPFG/VPRP-1,
               NRP=ifelse(NRP==0,0.00001,paste0(NRP)))%>%
        select(I,R,TS,NRP)%>%
        spread(TS,NRP)%>%
        mutate(NRPX=ifelse(is.na(NRPX),-1*as.numeric(NRPM),-1*as.numeric(NRPX)))%>%
        gather(TS,NRP,-I,-R)%>%
        mutate(extract=paste0(I,".",R,".",TS,"  ",NRP))%>%
        select(extract)
  
  write.table(PRE_AG, file=paste0(PathGams,"/Data/tariffs/NRP.inc"),row.names=FALSE,col.names = FALSE,quote=FALSE)
