CONS_PRIO_preDrop <- CONS_PRIO %>%
  mutate(tobeDropped = case_when(
# List of exclusion with explanations     
    #YEAR>=YEARMAX ~ 1,  # no observations about the agreed last year, defined in AgIncentivesProcessing.R ln28
# Special cases for MAFAP  
        COUNTRY_CODE=="MWI" & YEAR==2015 & AGCOMNAME=="Groundnuts, with shell" ~ 1,
    TRUE ~ 0
# Check Other exclusions in source files
#  For MAFAP --> mafap_conversion.py check m.drop at line 310+
  ))




CONS_PRIO_Drop <-CONS_PRIO_preDrop%>%filter(tobeDropped==1)
write.csv(CONS_PRIO_Drop,file="./Tests/DroppedData.csv",row.names=FALSE)
CONS_PRIO <-CONS_PRIO_preDrop%>%filter(tobeDropped==0)%>%select(-tobeDropped)
