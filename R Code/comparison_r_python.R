
library(tidyverse)

publicview.r <- read.csv('NRP/Final/2026-04-20/PUBLICVIEW_R.csv') |> 
                select(1,4:6,10:12,,14:17,19,21) |> 
                rename(PRODQ_R=7,REF_R=8,PROP_R=9,VP_PP_R=10,VP_RP_R=11,NRP_R=12,DIST_R=13)
publicview.python <- read.csv('NRP/Final/2026-04-20/PUBLICVIEW_Python.csv') |> 
                      select(1,4:6,10:12,,14:17,19,21) |> 
                      rename(PRODQ_P=7,REF_P=8,PROP_P=9,VP_PP_P=10,VP_RP_P=11,NRP_P=12,DIST_P=13)

publicview_joint <- publicview.r |> left_join(publicview.python) |> 
                    mutate(NRP_Diff=NRP_R-NRP_P) |> filter(NRP_Diff!=0)

write.csv(publicview_joint, 'Publicview_joint_R.csv', row.names = F)

nra_aggregate <- readxl::read_excel('NRA/Support_Database/Support_Database_2026_version1.xlsx', sheet = 'Aggregated_NRA') |> 
                 select(-c(4,6,11))

nra.agg.crp <- nra_aggregate |> filter(CATPROD=='CRP') |> filter(NRA_Cat!='NRA_Total')
write.csv(nra.agg.crp,'NRA/Support_Database/Support_Crop_2026.csv', row.names = F)
nra.agg.lvs <- nra_aggregate |> filter(CATPROD=='LVS') |> filter(NRA_Cat!='NRA_Total')
write.csv(nra.agg.lvs,'NRA/Support_Database/Support_livestock_2026.csv', row.names = F)
nra.agg.NAL <- nra_aggregate |> filter(CATPROD=='NAL') |> filter(NRA_Cat!='NRA_Total')
write.csv(nra.agg.NAL,'NRA/Support_Database/Support_non_allocated_2026.csv', row.names = F)
nra.agg.total <- nra_aggregate |> filter(CATPROD=='TOTAL') 
write.csv(nra.agg.total,'NRA/Support_Database/Support_total_2026.csv', row.names = F)

nra.agg.crp.lvs <- nra.agg.crp |> rbind(nra.agg.lvs) |> 
                   mutate(NRA_Cat=ifelse(NRA_Cat=='NRP','Price Support', 
                                         'Subsidy'))
                   
commodities <- c('Bovine Meat',"Cassava",'Coffee','Eggs','Maize','Milk','Palm oil','Pig meat','Poultry meat','Rice','Soybeans','Tea','Wheat')
                   
                   
nra_detailed <- readxl::read_excel('NRA/Support_Database/Support_Database_2026_version1.xlsx', sheet = 'Detailed_NRA') |> 
                     select(-c(1,3,5,6,8,10,11,15,17,19,21:24)) |> filter(NRA_Cat!='NRA_Total') 
                

write.csv(nra_detailed,'NRA/Support_Database/Support_detailed_2026.csv', row.names = F)





