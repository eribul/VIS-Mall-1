
########################## Hjälpfunktion för position ##########################
is.inca <- function(){
  unname(!(Sys.info()["user"] == "christianstaf"))
}



########## Lokala förberedelser samt inladdning av funktioner på INCA ##########
if (!is.inca()) {
  library(incavis)
  setwd("~/Documents/Github/VIS-Mall/Huvud-_och_halscancer")
  if (!file.exists("df.rda")) {
    df <- read.csv2("df.csv")
    save(df, file = "df.rda")
  } else {
    load("df.rda")
  }
  param  <- list(start = "2009-01-01", 
                 slut = "2009-12-31",
                 KON = 0)
} else {
  source("D:/R-Scripts/RCC/projekt/Vården i siffror/source_vis.R")
}


################################ Ladda in paket ################################
library(dplyr)


########### Skapa en ny dataram med länsvariabel och rätt tidsperiod ###########
df_ungrouped <-
    df %>%
    prepare_df() %>%
    transmute(
      CountyOidExtension = lkf2CountyOidExtension(a_lkf),
      behandlingsdatum   = pmin_dates(b_brachystart, b_stralstart, b_medstart,  b_op1dat, b_op2dat),
      ledtid             = as.numeric(behandlingsdatum - as.Date(a_remdat, format = "%Y-%m-%d"))
    ) %>%
    filter(
      !is.na(CountyOidExtension),
      ledtid %in% 0:365,
      between_param_dates(behandlingsdatum),
      if (param$KON %in% 1:2) kon_value %in% param$KON else TRUE
)

################## Skapa en ny dataram med gruppering på län ###################
df_lan <- group_by(df_ungrouped, CountyOidExtension )

#### Skapa funktion för att beräka ut kvotvärden samt första/sista mätvärde ####
summarise2 <- function(x) {
    x %>%
    summarise(
      Value                 = median(ledtid, na.rm = TRUE),
      Population            = n(),
      FirstServiceEncounter = min(behandlingsdatum, na.rm = TRUE),
      LastServiceEncounter  = max(behandlingsdatum, na.rm = TRUE)
    )
}


################# Slå samman olika dataramar med olika stratum #################
df_all <- bind_rows(summarise2(df_ungrouped), summarise2(df_lan)) 


############ Skicka in den sammanslagna dataramen i VIS-funktionen #############
vis(df_all)
