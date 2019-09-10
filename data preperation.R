
# Name of file: data preperation.R
# Data release (if applicable): Quarterly.
# Original author(s) : Saad Malik
# Original date: 26/08/2019
# Latest update author (if not using version control): n/a
# Latest update date (if not using version control): n/a
# Latest update description (if not using version control): n/a
# Type of script (e.g. extraction, preparation, modelling):preperation.
# Written/run on (e.g. R Studio SERVER): R studio.
# Version of R that the script was most recently run on: R 3.6.0.
# Description of content: Read in Contractor Open data files, combine and prepare them for analysis.
# Approximate run time: 1 minute.

# Package  Version
# dplyr   0.8.1
# tidyr   0.8.3
# readxl  1.3.1


# Section 1 - Housekeeping ----

#load packages.
library(dplyr) # to prepare data.
library(tidyr) # to prepare data.
library(readxl) # to read in excel files.

# Setting up an option so that high values of 100,000 apear as numbers on charts.
options("scipen"=10) 

# Section 2 - Create the main file   ----

# download and combine all datasets to create the full 'Contractor Activity' file ####

#Download the data using urls provided on the open data website.
ContractorActivity2014 <- "https://www.opendata.nhs.scot/dataset/a86fee95-8f92-443a-8ca4-9e814557f3a5/resource/9c22675d-c83f-4245-a0bd-0f1de64fa145/download/contractor-activity-2014.csv"
ContractorActivity2015 <- "https://www.opendata.nhs.scot/dataset/a86fee95-8f92-443a-8ca4-9e814557f3a5/resource/324c5d87-5d95-41aa-be38-140328d40b73/download/contractor-activity-2015.csv"
ContractorActivity2016 <- "https://www.opendata.nhs.scot/dataset/a86fee95-8f92-443a-8ca4-9e814557f3a5/resource/0ae561a7-e861-4854-8017-966bc6ad5eaf/download/contractor-activity-2016.csv" 
ContractorActivity2017 <- "https://www.opendata.nhs.scot/dataset/a86fee95-8f92-443a-8ca4-9e814557f3a5/resource/61f6e164-8e7e-4282-b691-50fbb14fd11c/download/contractor-activity-2017.csv"
ContractorActivity2018 <- "https://www.opendata.nhs.scot/dataset/a86fee95-8f92-443a-8ca4-9e814557f3a5/resource/a3484d2f-f744-4d20-876c-6e3db2909db6/download/contractor-activity-2018.csv"
ContractorActivity2019 <- "https://www.opendata.nhs.scot/dataset/a86fee95-8f92-443a-8ca4-9e814557f3a5/resource/80274dba-3cca-4e31-8d0c-0a90c1ae46da/download/contractor-activity-2019.csv"


#read the file into r and create a dataframe for each year.
ContractorActivity2014 <-  read.csv(ContractorActivity2014) 
ContractorActivity2015 <-  read.csv(ContractorActivity2015) 
ContractorActivity2016 <-  read.csv(ContractorActivity2016) 
ContractorActivity2017 <-  read.csv(ContractorActivity2017) 
ContractorActivity2018 <-  read.csv(ContractorActivity2018) 
ContractorActivity2019 <-  read.csv(ContractorActivity2019) 

#fix month name for 2019 data.
ContractorActivity2019 <- ContractorActivity2019 %>%
  rename("PaidDateMonth" = ï..PaidDateMonth)

#Combine dataframes for all years:
ContractorActivity <- rbind(ContractorActivity2014,
                            ContractorActivity2015,
                            ContractorActivity2016,
                            ContractorActivity2017,
                            ContractorActivity2018,
                            ContractorActivity2019)

####  create 'postcodes' file which has longitude, latitude and full pharmacy address for each pharmacy ####

#Load in Pc8 data.
PC8 <- read_excel("Data/PC8.xlsx")

#Load file with address details for each pharmacy.
contractor_code_with_postcode <- read_excel("Data/Pharmacy postcode Sept 18.xlsx")

# rename variables.
colnames(contractor_code_with_postcode) <- c("Contractor", "Healthboard","HSCP","PharmacyName", "PharmacyAddress1","PharmacyAddress2","PharmacyAddress3","PharmacyAddress4", "pc8")


#ensure equal spaces between first and second part of each postcode # 

# seperate the postcodes.
x <- contractor_code_with_postcode %>%
  separate(pc8, c("first", "second"),sep = 4)

# get rid of all empty spaces in the "first" part of the postcode.
x$first <- gsub(" ", "", x$first, fixed = TRUE)

# combine the "first" and "second" part using unite.
contractor_code_with_postcode <- x %>%
  unite("pc8",c("first", "second"), sep=" ")


# Join PC8 and boxi postcode file by "pc8".
Postcode <- right_join(PC8, contractor_code_with_postcode , by = "pc8") 

#postcode file contains the proper names of pharmacies + Longitude and latitude for map.
saveRDS(Postcode, "Data/Postcodes.rds")

#### Combine the 'postcodes' file with 'Contractor Activity' file ####

#load postcode file
Postcodes <- readRDS("Data/Postcodes.rds") %>%
  mutate (Contractor=as.character(Contractor)) # the contractor in open data is an integar.

ContractorActivity <- ContractorActivity %>%
  mutate(Contractor=as.character(Contractor))

#Join postcode file with the contractor acitivty file.
ContractorActivity <- left_join(ContractorActivity, Postcodes, by = "Contractor") 

#### Section 3 - Prepare the main file ----

#### prepare data for 'construct a chart' and 'create a datatable' ####

#rename variable names in the dataset. 
colnames(ContractorActivity) <- c("Date", "Pharmacy", "Items", "Cost", "MASItems", "MASRegistrations", "MASCapitationPayment","CMSItems","CMSRegistrations","CMSCapitationPayment","EHCItems","SmokingCessationItems","SmokingCessationPayment","InstalmentDispensings","MethadoneDispensingFeeNumber","MethadoneDispensingFeeRate","SupervisionDispensingFeeNumber","SupervisionFeeRate","FinalPayments","Postcode","Latitude","Longitude","Healthboard","HSCP","PharmacyName","PharmacyAddress1","PharmacyAddress2","PharmacyAddress3","PharmacyAddress4")

#replace NAs values with blanks.
ContractorActivity[is.na(ContractorActivity)] <- " "

#check if its worked.
is.na(ContractorActivity) 

#remove decimal points from cost variables
ContractorActivity$Cost <- formatC(ContractorActivity$Cost, format="f", digits=0)
ContractorActivity$MASCapitationPayment <- formatC(ContractorActivity$MASCapitationPayment, format="f", digits=0)
ContractorActivity$CMSCapitationPayment <- formatC(ContractorActivity$CMSCapitationPayment, format="f", digits=0)
ContractorActivity$SmokingCessationPayment <- formatC(ContractorActivity$SmokingCessationPayment, format="f", digits=0)
ContractorActivity$FinalPayments <- formatC(ContractorActivity$FinalPayments, format="f", digits=0)


#list of variables to be turned into numerics, needed to create line chart and data table.
turn_to_numeric <- c("Items","Cost","MASItems","MASRegistrations","MASCapitationPayment","CMSItems","CMSRegistrations",
                     "CMSCapitationPayment","EHCItems","SmokingCessationItems","SmokingCessationPayment","InstalmentDispensings",
                     "MethadoneDispensingFeeNumber","SupervisionDispensingFeeNumber","FinalPayments",
                     "Latitude","Longitude")

#turn the variables in the list created above into numerics.
ContractorActivity[c(turn_to_numeric)] <- sapply(ContractorActivity [turn_to_numeric],as.numeric)




#Create final table for analysis.
ContractorActivity<- ContractorActivity%>%
  
  #Create variable which has the full pharmacy address and postcode, making it easier for user to search pharmacies.
  mutate(Pharmacy=paste(Pharmacy,PharmacyName, sep= "-")) %>%
  mutate(Pharmacy=paste(Pharmacy, PharmacyAddress1,PharmacyAddress2,PharmacyAddress3,PharmacyAddress4,sep = " ")) %>% 
  mutate(Pharmacy=paste(Pharmacy,Postcode,sep="")) %>% #combining the address with the postcode
  
  #create a date variable, needed for data slider.
  mutate(Date=paste(Date,"1",sep = " "))%>% # attach a day coloumn onto the date
  mutate(Date=as.Date(Date, "%Y %m %d")) %>% # turn the date from an integer into a 'date'.
  mutate(Pharmacy=factor(Pharmacy)) %>%

  na.omit() %>% #this is removing three blank pharmacies.
  
    # summarise to remove duplicates.
    group_by(Pharmacy,Healthboard,HSCP,Date,Postcode, Latitude,Longitude) %>% 
    summarise(
    Items= sum(Items),
    Cost=sum(Cost),
    MASItems=sum(MASItems),
    MASRegistrations=sum(MASRegistrations),
    MASCapitationPayment=sum(MASCapitationPayment),
    CMSItems=sum(CMSItems),
    CMSRegistrations=sum(CMSRegistrations),
    CMSCapitationPayment=sum(CMSCapitationPayment),
    EHCItems=sum(EHCItems),
    SmokingCessationItems=sum(SmokingCessationItems),
    SmokingCessationPayment=sum(SmokingCessationPayment),
    InstalmentDispensings=sum(InstalmentDispensings),
    MethadoneDispensingFeeNumber=sum(MethadoneDispensingFeeNumber),
    MethadoneDispensingFeeRate=max(MethadoneDispensingFeeRate),
    SupervisionDispensingFeeNumber=sum(SupervisionDispensingFeeNumber),
    SupervisionFeeRate=max(SupervisionFeeRate),
    FinalPayments=sum(FinalPayments))%>%
  ungroup()


# modify duplicates with the same postocde, latitude and longitude so that pharmacies with the same postcode do not overlap on map.
ContractorActivity <- ContractorActivity %>%
  arrange(Postcode,Date) %>% 
  mutate(x = paste(Postcode,Date)) %>% #these two variables are being dupilcated so we combine them.
  mutate(y = lag(x)) %>%
  mutate(flag=ifelse(x==y,1,0)) %>% # create flag.
  mutate(Longitude=ifelse(flag==1, Longitude + 0.001, Longitude)) #modify based on flag.


#Save the file.
saveRDS(ContractorActivity, "Data/PharmacyActivity.rds")

# prepare data for 'populate a map' ####

#Contractor Activity file for map, aggregating data by quarter.
ContractorActivityM <- ContractorActivity %>%
  mutate(dategroup = lubridate::floor_date(Date, "3 months")) %>%
  mutate(dategroup= as.yearqtr(dategroup)) %>%
  group_by(dategroup,Pharmacy,Healthboard,HSCP,Latitude,Longitude) %>%
  summarise(Items=sum(Items),
            Cost=sum(Cost),
            MASItems=sum(MASItems),
            MASRegistrations=sum(MASRegistrations),
            MASCapitationPayment=sum(MASCapitationPayment),
            CMSItems=sum(CMSItems),
            CMSRegistrations=sum(CMSRegistrations),
            CMSCapitationPayment=sum(CMSCapitationPayment),
            EHCItems=sum(EHCItems),
            SmokingCessationItems=sum(SmokingCessationItems),
            SmokingCessationPayment=sum(SmokingCessationPayment),
            InstalmentDispensings=sum(InstalmentDispensings),
            MethadoneDispensingFeeNumber=sum(MethadoneDispensingFeeNumber),
            MethadoneDispensingFeeRate=mean(MethadoneDispensingFeeRate),
            SupervisionDispensingFeeNumber=sum(SupervisionDispensingFeeNumber),
            SupervisionFeeRate=mean(SupervisionFeeRate),
            FinalPayments=sum(FinalPayments)) 


#Save the file.
saveRDS(ContractorActivityM, "Data/PharmacyActivityM.rds")
