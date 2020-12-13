#Function to add missing info to Cu Dissolved Aquatic Life - Freshwater in wqg master
#some of this may already be done by updating the data
cu_add_codes <- function(x){

  cu_codes <- c("EMS_0004 EMS_1126 EMS_0107 ")

  x$Condition[x$Variable == "Copper" & x$Component == "Dissolved"] <- cu_codes
  x$LimitNotes[x$Variable == "Copper" & x$Component == "Dissolved"] <- NA
  
  # these items should be changed in Master Database 
  x$Units[x$Variable == "Copper" & x$Component == "Dissolved"] <- "ug/L"
  x$Direction[x$Variable == "Copper" & x$Component == "Dissolved"] <- "Upper Limit"
  
  # not 100% sure if this should be added to Master database or manually
  # depends on how we set it up
  x$Limit[x$Variable == "Copper" &
              x$Use == "Aquatic Life - Freshwater" &
              x$Media == "Water" &
              x$Type == "Short-term acute"] <- "EMS_CU-D_water_aquatic_fresh_acute_lookup.csv"
  x$Limit[x$Variable == "Copper" &
            x$Use == "Aquatic Life - Freshwater" &
            x$Media == "Water" &
            x$Type == "Long-term chronic"] <- "EMS_CU-D_water_aquatic_fresh_chronic_lookup.csv"

  return(x)
}
