# General Tests before comparison
library(pointblank)
agent <- 
  create_agent() %>%           
  focus_on(
    tbl_name = "PUBLICVIEW") %>%     
  col_vals_gt(
    column = ProducerPriceAtFGL & ReferencePriceAtFGL,
    value = 0) %>%
  col_vals_not_null(
    column = all_cols()) %>%
  # col_vals_equal(
  #   column = ProducerPriceAtFGL * ProductionQuantity,
  #   column = ValueProduction_PP) %>%
  col_vals_equal(
    column = ReferencePriceAtFGL * ProductionQuantity,
    column = ValueProduction_REF) %>%
  interrogate(agent)  

get_interrogation_summary(agent)[1:5]