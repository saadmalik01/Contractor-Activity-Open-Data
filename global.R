
#Loading all the required packages.

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(shiny)
library(shinythemes)
library(DT)
library(leaflet)
library(reshape2)
library(zoo)
library(lubridate)
library(scales)
library(htmltools)
library(shinyWidgets)
library(leaflet.extras)
library(stringr) #wrap pharmacy name.

library(rintrojs)
        
#Reading in Contractor Acitvity RDS file.
ContractorActivity <- readRDS("Data/PharmacyActivity.rds")
ContractorActivityM <- readRDS("Data/PharmacyActivityM.rds")

ContractorActivityM <- ContractorActivityM %>%
  separate(dategroup, c("year", "months"), " ") %>%
  mutate(months = case_when(
    months == "Q1"~ "Jan-Mar",
    months ==  "Q2" ~"Apr-Jun",
    months ==  "Q3" ~ "Jul-Aug",
    months == "Q4" ~ "Oct-Dec",
    TRUE ~ as.character(months))) %>%
  unite(dategroup, months, year, sep = " ")

#Create a function which creates all unique pharmacies.
AllPharmacies <- sort(unique(ContractorActivity$Pharmacy)) 
AllHBs <- sort(unique(ContractorActivity$Healthboard)) 
AllHSCPs <- sort(unique(ContractorActivity$HSCP)) 
  

#ContractorActivityM$dategroup <- as.factor(as.yearqtr(ContractorActivityM$dategroup))
#AllDates <- sort(unique(ContractorActivityM$dategroup)) 



AllDates <- ContractorActivityM %>%
  distinct(dategroup) %>%
  pull(dategroup)

#colour palletes.
Blues = c("#000066","#08519c","#3182bd","#6baed6","#aac1cf")
Colours = c("#56B4E9", "#009E73", "#D55E00","#F0E442","#CC79A7")




#limit exapnsion of select input for pharmacy in chart.
mycss <- "
#Pharmacy ~ .selectize-control .selectize-input {
  max-height: 85px;
  overflow-y: auto;
}
"
#limit exapnsion of select input for pharmacy in table.
mycss2 <- "
#Pharmacy2 ~ .selectize-control .selectize-input {
  max-height: 100px;
  overflow-y: auto;
}
"


#label choices for the select measure input for chart and datatable.
lab_choices = c(
            "Items"= "Items",
            "Cost (£)"="Cost",
            "MAS Items"="MASItems",
            "MAS Registrations"= "MASRegistrations",
            "MAS Capitation Payment (£)"="MASCapitationPayment",
            "CMS Items"="CMSItems",
            "CMS Registrations"="CMSRegistrations",
            "CMS Capitation Payment (£)"="CMSCapitationPayment",
            "EHC Items"="EHCItems",
            "Smoking Cessation Items"="SmokingCessationItems",
            "Smoking Cessation Payment (£)"= "SmokingCessationPayment",
            "Instalment Dispensings"="InstalmentDispensings",
            "Methadone Dispensing Fee Number"= "MethadoneDispensingFeeNumber",
            "Methadone Dispensing Fee Rate"="MethadoneDispensingFeeRate",
            "Final Payments (£)" = "FinalPayments",
            "Supervision Dispensing Fee Number"= "SupervisionDispensingFeeNumber",
            "Supervision Fee Rate"="SupervisionFeeRate")



# label choices for the select measure input for map.

# work with color factor.

lab_choices2 = c(
  "Items"= "Items",
  "Cost (£)"="Cost",
  "MAS Items"="MASItems",
  "MAS Registrations"= "MASRegistrations",
  "MAS Capitation Payment (£)"="MASCapitationPayment",
  "CMS Items"="CMSItems",
  "CMS Registrations"="CMSRegistrations",
  "CMS Capitation Payment (£)"="CMSCapitationPayment",
  "EHC Items"="EHCItems",
  "Smoking Cessation Items"="SmokingCessationItems",
  "Smoking Cessation Payment (£)"= "SmokingCessationPayment",
  "Instalment Dispensings"="InstalmentDispensings",
  "Methadone Dispensing Fee Number"= "MethadoneDispensingFeeNumber",
  "Methadone Dispensing Fee Rate"="MethadoneDispensingFeeRate",
  "Final Payments (£)" = "FinalPayments",
  "Supervision Dispensing Fee Number"= "SupervisionDispensingFeeNumber",
  "Supervision Fee Rate"="SupervisionFeeRate")
  