fix_improbable_coordinates <- function(data = full_trap_locations) {
  
  
  updated_coordinates <- data %>%
  mutate(lon = case_when(village == "lalehun" & visit == 5 & grid_number == 1 & trap_number == 34 ~ -11.07941,
                         village == "lalehun" & visit == 5 & grid_number == 2 & trap_number == 86 ~ -11.07901,
                         village == "lalehun" & visit == 5 & grid_number == 2 & trap_number == 94 ~ -11.0789,
                         village == "lalehun" & visit == 5 & grid_number == 4 & trap_number == 187 ~ -11.07817,
                         village == "lalehun" & visit == 5 & grid_number == 4 & trap_number == 174 ~ -11.07810,
                         
                         village == "seilama" & visit == 5 & grid_number == 1 & trap_number == 31 ~ -11.19249,
                         village == "seilama" & visit == 5 & grid_number == 2 & trap_number == 71 ~ -11.19426,
                         village == "seilama" & visit == 5 & grid_number == 4 & trap_number == 161 ~ -11.19457,
                         village == "seilama" & visit == 5 & grid_number == 4 & trap_number == 192 ~ -11.19483,
                         
                         village == "lambayama" & visit == 3 & grid_number %in% c(6, 7) & trap_number == 237 ~ -11.19681,
                         village == "lambayama" & visit == 3 & grid_number == 2 & trap_number == 69 ~ -11.19463,
                         
                         TRUE ~ lon),
         lat = case_when(village == "lalehun" & visit == 5 & grid_number %in% c(6, 7) & trap_number == 275 ~ 8.197686,
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
                         
                         village == "baiama" & visit == 3 & grid_number %in% c(4, 5, 6, 7) & trap_number %in% c(206, 216, 250, 251, 252, 253) ~ (lat/10000) + 7,
                         village == "baiama" & visit == 3 & grid_number == 1 & trap_number == 13 ~ 7.82467,
                         village == "baiama" & visit == 3 & grid_number == 1 & trap_number == 14 ~ 7.82475,
                         village == "baiama" & visit == 3 & grid_number == 1 & trap_number == 40 ~ 7.824557,
                         
                         village == "lambayama" & visit == 3 & grid_number %in% c(6, 7) & trap_number == 214 ~ (lat/10000) + 7,
                         village == "lambayama" & visit == 3 & grid_number %in% c(6, 7) & trap_number == 216 ~ 7.8506575,
                         village == "lambayama" & visit == 3 & grid_number %in% c(6, 7) & trap_number == 217 ~ 7.8506282,
                         village == "lambayama" & visit == 3 & grid_number == 4 & trap_number == 154 ~ 7.849887,
                         village == "lambayama" & visit == 3 & grid_number %in% c(6, 7) & trap_number %in% c(271:274) ~ 7.850658,
                         TRUE ~ lat))
  
  return(updated_coordinates)
  }
