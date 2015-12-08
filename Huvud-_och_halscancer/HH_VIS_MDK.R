
########################## Hjälpfunktion för position ##########################
is.inca <- function(){
  unname(!(Sys.info()["user"] == "christianstaf"))
}



########## Lokala förberedelser samt inladdning av funktioner på INCA ##########
if (!is.inca()) {
    library(incavis)
    library(rccdates)
    setwd("~/Documents/Github/VIS-Mall/Huvud-_och_halscancer")
    if (!file.exists("df.rda")) {
      df <- read.csv2("df.csv")
      save(df, file = "df.rda")
    } else {
      load("df.rda")
    }
    param  <- list(start = "2009-01-01", 
                   slut = "2014-12-31",
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
    mutate(
        CountyOidExtension = lkf2CountyOidExtension(a_lkf),
        MeasurePeriodStart = first_day(a_besldat, "year"),
        MeasurePeriodEnd = last_day(a_besldat, "year")
    ) %>%
  group_by(MeasurePeriodStart, MeasurePeriodEnd) %>% 
    filter(
        !is.na(CountyOidExtension),
        between_param_dates(a_besldat),
        if (param$KON %in% 1:2) kon_value %in% param$KON else TRUE
    )


################## Skapa en ny dataram med gruppering på län ###################
df_lan <- group_by(df_ungrouped,  CountyOidExtension, add = TRUE )



#### Skapa funktion för att beräka ut kvotvärden samt första/sista mätvärde ####
summarise2 <- function(x) {
    x %>%
        summarise(
            Denominator           = sum(a_multkonf_beskrivning %in% c("Ja", "Nej")),
            Numerator             = sum(a_multkonf_beskrivning %in% "Ja"),
            FirstServiceEncounter = min(a_besldat, na.rm = TRUE),
            LastServiceEncounter  = max(a_besldat, na.rm = TRUE)
        )
}


################# Slå samman olika dataramar med olika stratum #################
df_all <- bind_rows(summarise2(df_ungrouped), summarise2(df_lan)) 



############ Skicka in den sammanslagna dataramen i VIS-funktionen #############
vis(df_all)





