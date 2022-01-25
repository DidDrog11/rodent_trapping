fix_improbable_coordinates <- function(data = full_trap_locations) {
  
  
  data %>%
  mutate(lon = case_when(village == "lalehun" & visit == 5 & grid_number == 1 & trap_number == 34 ~ -11.07941,
                         village == "lalehun" & visit == 5 & grid_number == 2 & trap_number == 86 ~ -11.07901,
                         village == "lalehun" & visit == 5 & grid_number == 2 & trap_number == 94 ~ -11.0789,
                         village == "lalehun" & visit == 5 & grid_number == 4 & trap_number == 187 ~ -11.07817,
                         village == "lalehun" & visit == 5 & grid_number == 4 & trap_number == 174 ~ -11.07810,
                         
                         village == "seilama" & visit == 5 & grid_number == 1 & trap_number == 31 ~ -11.19249,
                         village == "seilama" & visit == 5 & grid_number == 2 & trap_number == 71 ~ -11.19526,
                         village == "seilama" & visit == 5 & grid_number == 4 & trap_number == 161 ~ -11.19457,
                         village == "seilama" & visit == 5 & grid_number == 4 & trap_number == 192 ~ -11.19483,
                         
                         TRUE ~ lon),
         lat = case_when(village == "lalehun" & visit == 5 & grid_number == 6 & trap_number == 275 ~ 8.197686,
                         village == "lalehun" & visit == 5 & grid_number == 4 & trap_number == 167 ~ 8.194110,
                         village == "lalehun" & visit == 5 & grid_number == 1 & trap_number == 8 ~ 8.196667,
                         village == "lalehun" & visit == 5 & grid_number == 1 & trap_number == 9 ~ 8.196497,
                         village == "lalehun" & visit == 5 & grid_number == 1 & trap_number == 16 ~ 8.196596,
                         village == "lalehun" & visit == 5 & grid_number == 3 & trap_number == 108 ~ 8.198896,
                         village == "lalehun" & visit == 5 & grid_number == 3 & trap_number == 115 ~ 8.198991,
                         village == "lalehun" & visit == 5 & grid_number == 3 & trap_number == 116 ~ 8.199034,
                         village == "lalehun" & visit == 5 & grid_number == 3 & trap_number == 123 ~ 8.199043,
                         village == "lalehun" & visit == 5 & grid_number == 3 & trap_number == 130 ~ 8.198838,
                         village == "lalehun" & visit == 5 & grid_number == 3 & trap_number == 131 ~ 8.198868,
                         
                         village == "seilama" & visit == 5 & grid_number == 3 & trap_number == 124 ~ 8.123936,
                         TRUE ~ lat))
  
  return(data)
  }
