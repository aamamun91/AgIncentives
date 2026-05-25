

write.csv(PUBLICVIEW, "./NRP/Consortium/FullDB.csv", row.names=FALSE)

Partner <- SOURCE[1]
EXCHECK <- PUBLICVIEW %>%
            filter(Source==Partner) %>%
            mutate(NPC_AgInc=ProducerPriceAtFGL/ReferencePriceAtFGL,DIFFNPC=round(NPC_SOURCE-NPC_AgInc, digits=4),REVERSAL=ifelse((NPC_SOURCE-1)*(NPC_AgInc-1)<0,1,0))

EXGAP <- EXCHECK%>%
         filter(abs(DIFFNPC)>0.01 | REVERSAL==1) %>%
         select(CountryName, ProductName,Year, NRP, contains("NPC"),REVERSAL)
head(EXGAP)
readr::write_excel_csv(EXCHECK, file = paste0("./NRP/Consortium/ToCheck",Partner,".csv"))

Partner <- SOURCE[2]
EXCHECK <- PUBLICVIEW %>%
           filter(Source==Partner) %>% 
           mutate(NPC_AgInc=ProducerPriceAtFGL/ReferencePriceAtFGL,DIFFNPC=round(NPC_SOURCE-NPC_AgInc, digits=4),REVERSAL=ifelse((NPC_SOURCE-1)*(NPC_AgInc-1)<0,1,0))
readr::write_excel_csv(EXCHECK, file = paste0("./NRP/Consortium/ToCheck",Partner,".csv"))

Partner <- SOURCE[3]
EXCHECK <- PUBLICVIEW %>%
           filter(Source==Partner) %>%
           mutate(NPC_AgInc=ProducerPriceAtFGL/ReferencePriceAtFGL,DIFFNPC=round(NPC_SOURCE-NPC_AgInc, digits=4),REVERSAL=ifelse((NPC_SOURCE-1)*(NPC_AgInc-1)<0,1,0))

EXGAP <- EXCHECK%>% 
         filter(abs(DIFFNPC)>0.01 | REVERSAL==1) %>%
         select(CountryName, ProductName,Year, NRP, contains("NPC"),REVERSAL)
head(EXGAP)

  
readr::write_excel_csv(EXCHECK, file = paste0("./NRP/Consortium/ToCheck",Partner,".csv"))

MAFAP_COVERAGE <- Partner<-SOURCE[2]

EXCHECK<- PUBLICVIEW %>%
          filter(Source==Partner) %>%
          select(CountryName, ProductName) %>%
          distinct()

readr::write_excel_csv(EXCHECK, file = paste0(".Coverage_",Partner,".csv"))

