
# -----------------------------------------------------------------------------

# This is code to create tables needed for the Ag-Incentives Consortium: 
# Functions
# -----------------------------------------------------------------------------

################# Concatenating with a delimiter without NAs ######################

paste5 <- function(..., sep = "", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
} 

################# Aggregation NRP ######################

aggregateNRP <- function(tabx1,dim1="GLOBAL",dim2="TOTAL",labx,sourcex=5) {
  # tabx: dataframe to aggregate, should have the standard AG INC columns (see below)
  # dim1: Geographical aggregation, default value= Global results
  # dim2: Sectoral aggregation, default value= All sectors
  # labx: Label for the aggregation category
  # sourcex: Variable to indicate the source coding. Default=5, Consortium aggregation
  xdim1 <- enquo(dim1)
  xdim2 <- enquo(dim2)
  DATAAG <- tabx1 %>%
            mutate(
                CPTPRO    = AGPROCODE,
                CPTCOU    = COUNTRY_CODE,
                NUMSOURCEx = ifelse(YEAR*sourcex>0,as.numeric(sourcex),as.numeric(paste0(NUMSOURCE))),
                PRODQ_PHY_UNIT = ifelse(YEAR*sourcex>0,paste0("INDEX"), paste0(PRODQ_PHY_UNIT) ),
                REGION_CODE = as.character(!!xdim1),
                PDCT_CODE = as.character(!!xdim2)
               )
  
  DATAAGT<- DATAAG %>% 
            select(YEAR,AGPROCODE,COUNTRY_CODE) %>% distinct() %>% 
            mutate(YEAR=YEAR-1,FUTUREYEAR=1)
  
  DATAAG2<- DATAAG %>%
            left_join(DATAAGT, by = c("YEAR", "AGPROCODE", "COUNTRY_CODE")) %>%
            mutate(PREVWEIGHT=ifelse(is.na(FUTUREYEAR),0,VP_REFP)) %>%
            group_by(YEAR,REGION_CODE,PDCT_CODE,PRODQ_PHY_UNIT) %>%
            summarise(
              NUMSOURCE = mean(NUMSOURCEx, na.rm = TRUE),
              PRODNUM = n_distinct(CPTPRO),
              COMNUM  = n_distinct(AGCOMCODE),
              COUNTRYNUM = n_distinct(CPTCOU),
              PRODQ = sum(PRODQ, na.rm = TRUE),
              VP_PROP = sum(VP_PROP, na.rm = TRUE),
              VP_REFP = sum(VP_REFP, na.rm = TRUE),
              MPS = sum(MPS, na.rm = TRUE),
              PSCT = sum(PSCT, na.rm = TRUE),
              EFC = sum(EFC, na.rm = TRUE),
              # Simple average for NRP, NPC
              SAVGNRP = mean(NRP, na.rm = TRUE),
              NPC=mean(NPC, na.rm = TRUE),
              PREVWEIGHT=sum(PREVWEIGHT, na.rm = TRUE)
              ) %>%
            mutate(
                  NRP = ( (VP_PROP/VP_REFP) -1) * 100,
                  CAT=paste0(labx),
                  MISSINGWGTx=round(PREVWEIGHT/VP_REFP-1,2)*100
            ) %>%
            ungroup()  

  PREVWGT <- DATAAG2 %>%
             select(YEAR,REGION_CODE,PDCT_CODE,PRODQ_PHY_UNIT,MISSINGWGTx) %>%
             mutate(YEAR=YEAR+1) 
  
  DATAAG3 <- DATAAG2 %>% 
             left_join(PREVWGT) %>%
             select(-PREVWEIGHT,-MISSINGWGTx)
     
}