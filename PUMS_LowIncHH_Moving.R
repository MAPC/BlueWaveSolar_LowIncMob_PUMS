library(plyr)
library(dplyr)
library(tidyr)

pums201418p <- read.csv('K:/DataServices/Datasets/U.S. Census and Demographics/PUMS/Raw/pums_2014_18/csv_pma/psam_p25.csv')
pums201418h <- read.csv('K:/DataServices/Datasets/U.S. Census and Demographics/PUMS/Raw/pums_2014_18/csv_hma/psam_h25.csv')

#Combine person and household files
PHa18 <- join(pums201418h,pums201418p, by = c("SERIALNO"), type = "left", match = "all")
rm(pums201418p, pums201418h)

#adjust income
PHa18$hhinc2018 <- round(as.numeric(PHa18$ADJINC)*as.numeric(PHa18$HINCP)/1000000,0)

#counts as HHds below 60% of state median income
PHa18$HHinc_adj <- 0
PHa18$HHinc_adj[PHa18$hhinc2018<=48729] = 1

#filter to heads of households
LowIncMob_MA <- PHa18[PHa18$SPORDER==1,]
LowIncMob_MA <- LowIncMob_MA[LowIncMob_MA$TYPE==1,]

#removes the NAs which are vacant household records and makes the results cleaner
LowIncMob_MA <- LowIncMob_MA[!is.na(LowIncMob_MA$SPORDER),]

#add county variable
#LowIncMob_MA$county [LowIncMob_MA$PUMA==1 | LowIncMob_MA$PUMA==1] = "Norfolk"

#filter to MA
#LowIncMob_MA %>% 
  #rename(Puma = PUMA)

#rename(LowIncMob_MA, puma_id = PUMA)

# select variables v1, v2, v3 to export
myvars_PUMS <- c("SERIALNO", "SPORDER", "MIG", "HHinc_adj", "WGTP", "PUMA", "MV")
LowIncMob_MA_export <- LowIncMob_MA[myvars_PUMS]

#LowIncMob_MA <- LowIncMob_MA %>%
  #select(SERIALNO, SPORDER, MIG, HHinc_adj, WGTP, Puma_id)


write.csv(LowIncMob_MA_export, "K:\\DataServices\\Projects\\Data_Requests\\2021\\BlueWaveSolar_LowIncMobility\\LowIncMob_PUMS18.csv")

###########################################
#counts as child 5-18
PHa18$child <- 0
PHa18$child[PHa18$AGEP<=18 & PHa18$AGEP>=5 ] = 1

#Aggregate children in HHds
PHchld17 <- PHa18[c("SERIALNO", "child")]
temp <- aggregate(PHa18$child, by=list(PHa18$SERIALNO), FUN=sum)
colnames(temp) <- c("SERIALNO", "schlchildtot")
#regoin hh attribute (childtot) to person dataset
PHa18 <- join(temp, PHa18, by = c("SERIALNO"), type = "left", match = "all")
rm(PHchld17, temp)

#Tenure
PHa18$tenure [PHa18$TEN==1 | PHa18$TEN==2] = "Owner"
PHa18$tenure [PHa18$TEN==3 | PHa18$TEN==4] = "Renter"

#year built

#unit type

#age > 65
PHa18$hhder_age <- "under 65"
PHa18$hhder_age [PHa18$PAGE >= 65] = ">=65"

##year built
PHa18$yrblt <- "before 2000"
PHa18$yrblt [PHa18$YBL >=8] = "2000 or after"

##units in structure
PHa18$units [PHa18$BLD <=3] = "single family" #includes mobile homes, sf detatched, sf attached
PHa18$units [PHa18$BLD>3] = "multifamily"

#Remove any records in GQ
PHH18 <- PHa18[PHa18$TYPE==1,]
#keep householder
PHH18 <- PHH18[PHH18$SPORDER==1,]

#Break out Ipswich PUMA
Ipswich <- PHH18[PHH18$PUMA==703,]
Ipswich <- Ipswich %>% drop_na(SERIALNO)

Ipswich <- Ipswich %>%
  select(SERIALNO, SPORDER, tenure, schlchildtot, hhder_age, yrblt, units, WGTP)


write.csv(Ipswich, "K:\\DataServices\\Projects\\Data_Requests\\2021\\Marchant_Ipswich\\Ipswich_PUMS18.csv")

#filter all records to only households that have children
HH_chil <- Ipswich %>% filter(schlchildtot > 0)

#filter all records to multifam or single fam households with children
mf_chil <- HH_chil %>% filter(units == 'multifamily')
sf_chil <- HH_chil %>% filter(units == 'single family')

#filter all records to before or after 2000
before_chil <- HH_chil %>% filter(yrblt == 'before 2000')
after_chil <- HH_chil %>% filter(yrblt == '2000 or after')

#calculate the total number of children by HH for each subcategory by multiplying the number of children by the weight
HH_chil$totchil <- HH_chil$schlchildtot*HH_chil$WGTP
mf_chil$totchil <- mf_chil$schlchildtot*mf_chil$WGTP
sf_chil$totchil <- sf_chil$schlchildtot*sf_chil$WGTP
before_chil$totchil <- before_chil$schlchildtot*before_chil$WGTP
after_chil$totchil <- after_chil$schlchildtot*after_chil$WGTP

#sum total number of children. this is the numerator for the average.
Tot_Chil <- sum(HH_chil$totchil)
Tot_mf_Chil <- sum(mf_chil$totchil)
Tot_sf_Chil <- sum(sf_chil$totchil)
Tot_before_Chil <- sum(before_chil$totchil)
Tot_after_Chil <- sum(after_chil$totchil)

#sum total number of households for each subcategory - this is the denominator for the average.
TotHH_Chil <- sum(HH_chil$WGTP)
TotHH_mf_Chil <- sum(mf_chil$WGTP)
TotHH_sf_Chil <- sum(sf_chil$WGTP)
TotHH_before_Chil <- sum(before_chil$WGTP)
TotHH_after_Chil <- sum(after_chil$WGTP)

#divide numerator by denominator to get average
avgchil_allHH <- Tot_Chil/TotHH_Chil
avgchil_mfHH <- Tot_mf_Chil/TotHH_mf_Chil
avgchil_sfHH <- Tot_sf_Chil/TotHH_sf_Chil
avgchil_beforeHH <- Tot_before_Chil/TotHH_before_Chil
avgchil_afterHH <- Tot_after_Chil/TotHH_after_Chil
