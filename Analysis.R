library(tidyverse)
library(readxl)
library(ggplot2)
source("Functions.R")
source("Mapping.R")

NRPsignCountry <- AG_COUNTRY_TOTAL %>%
                  filter(YEAR<paste0(YEARMAX)) %>%
                  mutate(SignNRP=ifelse(NRP<0,"NRP<0","NRP>=0")) %>%
                  group_by(YEAR,SignNRP) %>%
                  summarize(nobs=n()) %>%
                  ungroup() %>%
                  group_by(YEAR) %>%
                  mutate(share=nobs/sum(nobs))

ggplot(data = NRPsignCountry, aes(x=factor(YEAR),y=share,fill=SignNRP))+
  geom_bar(stat="identity", colour="black" )

MainDistortions <-  AG_COUNTRY_PRODUCT %>%
                    filter(REGION_CODE=="ETH") %>%
                    left_join(PDCT_LABEL0) %>%
                    mutate(DISTORTION=VP_PROP-VP_REFP)

ggplot(data = MainDistortions, aes(x=YEAR,y=DISTORTION,fill=PDCT_DESC))+
  geom_bar(stat="identity")+theme_minimal()

NRPsignDetails <- AG_COUNTRY_PRODUCT %>%
                  filter(YEAR<paste0(YEARMAX)) %>%
                  left_join(COUNTRY_A, by= c("REGION_CODE"="ISO3CODE")) %>%
                  mutate(SignNRP=ifelse(NRP<0,"NRP<0","NRP>=0")) %>%
                  group_by(WBCODE2015,SignNRP) %>%
                  summarize(nobs=n(),valprod=sum(VP_REFP)) %>%
                  ungroup() %>%
                  group_by(WBCODE2015) %>%
                  mutate(share=nobs/sum(nobs),vshare=valprod/sum(valprod)) %>%
                  filter(SignNRP=="NRP<0") %>%
                  ungroup

ggplot(data = NRPsignDetails, aes(x=WBCODE2015,y=vshare,color=WBCODE2015))+
  geom_bar(stat="identity", fill="white")

### Comparison
# D:\R_PROJ\AGIncConsolidated\Tests
#  ./CurrentlyOnline/AgIncentivesNRP.csv
ONLINE <- read.csv("./CurrentlyOnline/AgIncentivesNRP.csv") %>%
          select(CountryCode, Year, ProductCode, ProductionQuantity, ReferencePriceAtFGL,ProducerPriceAtFGL,NRP) %>%
          mutate(CountryCode=ifelse(CountryCode=="ALL","GLOBAL",paste0(CountryCode)))

ONSITE <- read.csv("./Tests/PUBLICVIEW_ref.csv") %>% 
          select(CountryCode, Year, ProductCode, ProductionQuantity,  ReferencePriceAtFGL,ProducerPriceAtFGL,NRP) %>%
          mutate(CountryCode=ifelse(CountryCode=="ALL","GLOBAL",paste0(CountryCode)))

COMPARISON <- PUBLICVIEW %>% 
              left_join(ONLINE,by=c("CountryCode", "Year", "ProductCode"),suffix=c(".new",".old"))

COMPARISON_NEW <- COMPARISON %>% 
                  filter(is.na(NRP.old)& Category=="COUNTRY_PRODUCT")

COMPARISON_NEW_SUMMARY1 <-  COMPARISON_NEW %>%
                            group_by(Year,CountryCode) %>%
                            summarize(caseNEW=n())

COMPARISON_NEW_SUMMARY2 <- COMPARISON_NEW %>% group_by(Year,) %>% summarize(caseNEW=n())
COMPARISON_NEW_SUMMARY2

COMPARISON_1pct <-  COMPARISON %>% 
                    filter(!is.na(NRP.old) & abs(NRP.old-NRP.new)>1) %>%
                    select(CountryCode, Year, ProductName,NRP.new,NRP.old) %>%
                    mutate(NRP_GAP=NRP.new-NRP.old)

write.csv(COMPARISON_NEW, paste0("./Tests//NEW_DATA-", Sys.Date(),
                             ".csv", sep = ""), row.names = FALSE)
write.csv(COMPARISON_1pct, paste0("./Tests/CHANGES1pct-", Sys.Date(),
                                 ".csv", sep = ""), row.names = FALSE)

## Develop decomposition method

### Prices
REF_PRICES_missing<-TPUBLICVIEW %>% 
  filter(REFP==0 | is.na(REFP))
head(REF_PRICES_missing)
REF_PRICES_small<-TPUBLICVIEW %>% 
  filter(CAT=="COUNTRY_PRODUCT")%>%
  filter(REFP<.99)
head(REF_PRICES_small)

REF_PRICES<-TPUBLICVIEW %>% 
  filter(CAT=="COUNTRY_PRODUCT")%>%
  mutate(REFP=round(REFP,0))%>%
  select(YEAR,REGION_CODE,PDCT_CODE,PDCT_DESC,REFP)

REF_PRICES_SUMMARY<-REF_PRICES%>%
  group_by(YEAR,PDCT_DESC)%>%
  summarize(REFP_min=min(REFP),REFP_max=max(REFP),REFP_median=median(REFP),REFP_std=sd(REFP))

REF_PRICES_TOTAL<-REF_PRICES%>%
  group_by(PDCT_DESC)%>%
  summarize(REFP_min=min(REFP),REFP_max=max(REFP),REFP_median=median(REFP),REFP_std=sd(REFP))

REF_PRICES_CHK<-REF_PRICES%>%
  left_join(REF_PRICES_SUMMARY)%>%
  mutate(REFP_std=as.numeric(ifelse(is.na(REFP_std),REFP_median/3,paste0(REFP_std))),upb=REFP+2*REFP_std,lob=REFP-2*REFP_std,
         GAP=REFP-upb)%>%
  filter(GAP>=0)
head(REF_PRICES_CHK)

SOURCEX<-TPUBLICVIEW%>%select(SOURCE,YEAR,PDCT_DESC,REGION_CODE)

REF_PRICES_XTRM<-REF_PRICES%>%
  left_join(REF_PRICES_TOTAL)%>%
  left_join(SOURCEX)%>%
  filter(REFP_max==REFP | REFP_min==REFP)%>%
  filter(REFP>0 & REFP_std/REFP_median>.5)

Partner<-SOURCE[1]
EXCHECK<-REF_PRICES_XTRM%>%
  filter(SOURCE==Partner)
write.csv(EXCHECK, file = paste0("./Tests/Prices_",Partner,".csv"), row.names=FALSE)

Partner<-SOURCE[2]
EXCHECK<-REF_PRICES_XTRM%>%
  filter(SOURCE==Partner)
write.csv(EXCHECK, file = paste0("./Tests/Prices_",Partner,".csv"), row.names=FALSE)

Partner<-SOURCE[3]
EXCHECK<-REF_PRICES_XTRM%>%
  filter(SOURCE==Partner)
write.csv(EXCHECK, file = paste0("./Tests/Prices_",Partner,".csv"), row.names=FALSE)


### NRP
Graph_NRPdistriperYear <- ggplot(AG_COUNTRY_TOTAL, aes(x = YEAR, y = NRP, group=YEAR)) +
  geom_boxplot()
Graph_NRPdistriperYear

# to be ranked by gdp pc capira ppp
Graph_NRPdistriperCountry <- ggplot(AG_COUNTRY_TOTAL, aes(x = REGION_CODE, y = NRP, group=REGION_CODE)) +
  geom_boxplot()
Graph_NRPdistriperCountry


EXTVIEW<-TPUBLICVIEW %>% filter(CAT=="COUNTRY_GROUP")
Graph_NRPdistriperProduct <- ggplot(EXTVIEW, aes(x = PDCT_DESC, y = NRP, group=PDCT_DESC)) +
  geom_boxplot()
Graph_NRPdistriperProduct

EXTVIEW<-TPUBLICVIEW %>% filter(CAT=="COUNTRY_PRODUCT")
Graph_REFPdistriperProduct <- ggplot(EXTVIEW, aes(x = PDCT_DESC, y = REFP, group=PDCT_DESC)) +
  geom_boxplot()
Graph_REFPdistriperProduct

### Extract data for subset of countries
extract.countries<-c("ETH","IND","KEN","RWA","BFA","MWI","TZA","TGO","UGA","ZMB","MLI","MOZ","ZWE")
rank.countries<-c(1:14)
order.countries<-data.frame(CountryCode=extract.countries,
                            rk=rank.countries)
extract.year<-c(2013:2017)
extract.view<-PUBLICVIEW%>% 
  filter(Year %in% extract.year & CountryCode %in% extract.countries &
           Category=="COUNTRY_PRODUCT")%>%
  left_join(order.countries)%>%
  group_by(CountryCode,CountryName, rk, ProductName)%>%
  summarize(DistortionValue=sum(DistortionValue),
            ValueProduction_REF=sum(ValueProduction_REF))%>%
  ungroup()%>%group_by(CountryCode,CountryName,rk)%>%
  mutate(NRP_decompo=round(DistortionValue/sum(ValueProduction_REF)*100,1))%>%
  mutate(ProductName=ifelse(abs(NRP_decompo)<3,
                            "Other products",paste0(ProductName)))%>%
  mutate(NRP_Total=sum(NRP_decompo))%>%
  arrange(rk)

extract.top0<-PUBLICVIEW%>% 
  filter(Year %in% extract.year & CountryCode %in% extract.countries &
           Category=="COUNTRY_PRODUCT")%>%
  left_join(order.countries)%>%
  group_by(CountryCode,CountryName, rk, ProductCode, ProductName)%>%
  summarize(DistortionValue=sum(DistortionValue),
            ValueProduction_REF=sum(ValueProduction_REF))%>%
  ungroup()%>%group_by(CountryName,rk)%>%
  mutate(NRP=round(DistortionValue/ValueProduction_REF,3)*100,
         ref=abs(DistortionValue),
         shareNRP=round(ref/sum(ref),3)*100)%>%
  top_n(4, ref)%>%
  arrange(rk,-ref)%>%
  mutate(text=paste0(ProductName,": NRP=",NRP,
                     "%, share in distortions: ",shareNRP,"%"),
         id = row_number())%>% ungroup

extract.tariff<-extract.top0%>%select(CountryCode,CountryName,rk,id,ProductName,ProductCode)%>%
  left_join(HS_MAPPING,by=c("ProductCode"="AGPROCODE"))

extract.top<-extract.top0%>%select(CountryCode,CountryName,rk, id,text)%>%spread(id,text)%>%arrange(rk)

extract.view
gr.distri.focus<-ggplot(extract.view, aes(fill=ProductName, y=NRP_decompo, 
                                          x = CountryName )) + 
  geom_bar( stat="identity")+ 
  geom_text(aes(label = NRP_Total,y =NRP_Total),
                    vjust = 1.5, color = "black")+  
  geom_point(aes(y = NRP_Total, x = CountryName))+  #reorder(CountryCode, -rk)
  xlab("") + ylab("Nominal Rate of Protection: \n National average & Key production contribution")+
  labs(fill = "Key Products")+ 
  geom_hline(yintercept=0,color = "red", size=1.25)+
  coord_flip() 
gr.distri.focus



add_WDI<-WDI(country="MZ", indicator=c("TM.TAX.MANF.SM.FN.ZS"),start=2005, end=2015)%>%
  left_join(list.country)%>%select(-country)%>%
  filter(iso3c %in% extract.countries)



# NRP for selected commodities --------------------------------------------

PUBLICVIEW <- read.csv(file="Source/PUBLICVIEW_2024/PUBLICVIEW.csv") 

commodities <- c('Bovine Meat',"Cassava",'Coffee','Eggs','Maize','Milk','Palm oil','Pig meat','Poultry meat','Rice','Soybeans','Tea','Wheat')
year <- c(2019,2020,2021,2022,2023)

NRP_select <- PUBLICVIEW %>% filter(Category=='GLOBAL_PRODUCT') %>% filter(ProductName %in% commodities) %>% 
              filter(Year %in% year) %>% 
              select(ProductName,Year,NRP) %>% 
              spread(Year,NRP)

write.csv(NRP_select, './NRA_Output/Data_for_website/NRP_commodities_2019_2023.csv', row.names = F)


no.countries <- PUBLICVIEW %>% filter(Category=='COUNTRY_PRODUCT') %>% select(Source,CountryName) %>% 
                distinct() %>% group_by(Source) %>% summarise(ncountry=n())

no.product <- PUBLICVIEW %>% filter(Category=='COUNTRY_PRODUCT') %>% select(ProductName) %>% 
              distinct() %>% group_by(Source) %>% summarise(nproduct=n())

product.total <- PUBLICVIEW %>% filter(Category=='COUNTRY_PRODUCT') %>% select(ProductName) %>% distinct()



# Subsidy coverage --------------------------------------------------------

detailedsubsidy <- readxl::read_excel("./NRA_Output/Support_Database/Support_Database_2025_version3.xlsx", 
                                      sheet = 'Detailed_NRA') %>% filter(NRA_Cat!='NRP')


no.countries <- detailedsubsidy %>% select(Country_Code) %>% distinct()
no.products <- detailedsubsidy %>% select(Commodity_Label) %>% distinct()


# NRP by income group -----------------------------------------------------

NRP_byIncome <- PUBLICVIEW %>% filter(Category=='INCOME_TOTAL') %>% 
                select(CountryName,Year,NRP) %>% 
                spread(CountryName,NRP)

write.csv(NRP_byIncome, './NRA_Output/Data_for_website/NRP_by_IncomeGroup_2025.csv', row.names = F)


country_year <- PUBLICVIEW %>% filter(Category=='COUNTRY_PRODUCT') %>% 
                select(CountryName,Year) %>% distinct() %>% 
                group_by(CountryName) %>% summarise(yearavailable = paste0(ymin=min(Year), '-', ymax=max(Year))) 

write.csv(country_year, './NRA_Output/Data_for_website/country_year_available.csv', row.names = F)

list.country <- country_year %>% select(CountryName) %>% distinct() %>% summarise(countrylist=paste0(CountryName,collapse = ", "))

write.csv(list.country, './NRA_Output/Data_for_website/list_country.csv', row.names = F)


# price data for wheat, rice and maize ------------------------------------

ricewheatmaize <- readxl::read_excel('NRA_Output/Price data - wheat-maize-rice.xlsx')

pricematrix <- ricewheatmaize %>% group_by(CountryName, Product) %>% 
               summarise(Period=paste0(min(Year),'-', max(Year))) %>% 
               ungroup() %>% 
               spread(Product,Period)

write.csv(pricematrix,'./NRA_Output/DataCoverage_Table.csv', row.names = F)
                
                
