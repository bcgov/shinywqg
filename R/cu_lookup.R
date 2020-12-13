#Function to add missing info to Cu Dissolved Aquatic Life - Freshwater in wqg master
#some of this may already be done by updating the data
cu_add_codes <- function(x){

  cu_codes <- c("EMS_0004 EMS_1126 EMS_0107 ")

  x$Condition[x$Variable == "Copper" & x$Component == "Dissolved"] <- cu_codes
  x$Units[x$Variable == "Copper" & x$Component == "Dissolved"] <- "ug/L"
  x$LimitNotes[x$Variable == "Copper" & x$Component == "Dissolved"] <- NA
  x$Direction[x$Variable == "Copper" & x$Component == "Dissolved"] <- "Upper Limit"
  x$Limit[x$Variable == "Copper" &
              x$Use == "Aquatic Life - Freshwater" &
              x$Media == "Water" &
              x$Type == "Short-term acute"] <- "cu_dissolved_aquatic_fresh_acute_lookup.csv"
  x$Limit[x$Variable == "Copper" &
            x$Use == "Aquatic Life - Freshwater" &
            x$Media == "Water" &
            x$Type == "Long-term chronic"] <- "cu_dissolved_aquatic_fresh_chronic_lookup.csv"

  return(x)
}

lookups <- function(lookup_table, cvalues){
  
  
  # read's in lookup table 
  # this will need to be adjusted once we get them uploaded to BC Data Cat
  file_path <- paste0("../../data-raw/", lookup_table)
  data_lookup <- readr::read_csv(file_path)
  
  # need to switch out for Joe's generic function
  lookup_row <- data_lookup %>% 
    dplyr::filter(EMS_0004 == cvalues["EMS_0004"],
                  EMS_0107 == cvalues["EMS_0107"],
                  EMS_1126 == cvalues["EMS_1126"])
  
  lookup_row[[1,ncol(lookup_row)]]
}


