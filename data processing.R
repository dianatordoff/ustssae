library(Hmisc)
library(memisc)
library(dplyr)
library(survey)
library(data.table)

usts <- read.csv("2015 USTS PUDS.csv")
zips <- read.csv("Q2_26_1_zipcodes.csv")
xwalk <- read.csv("xwalk.csv") 

# Geographies
usts <- merge(usts, zips, by="idcode", all.x=TRUE)
usts$zip <- as.character(usts$zip)
usts$missingzip <- ifelse(usts$zip==".", "no zipcode", NA)

xwalk$zip <- as.numeric(xwalk$zip)
xwalk <- xwalk[order(xwalk$zip,-xwalk$RES_RATIO), ] 
xwalk$dup <- duplicated(xwalk$zip) 
xwalk <- xwalk[xwalk$dup==FALSE, c("zip", "region", "subregion")] 

usts$zip <- as.numeric(usts$zip)
usts <- merge(usts, xwalk, by="zip", all.x=TRUE)
usts$region <- as.character(usts$region)
usts$subregion <- as.character(usts$subregion)
usts$region <- with(usts, ifelse(region=="NA", NA, region))
usts$subregion <- with(usts, ifelse(subregion=="NA", NA, subregion))
usts$missingzip <- with(usts, ifelse(is.na(missingzip), ifelse(is.na(subregion) | subregion %in% c("0", "NA", " NA"), "invalid zipcode", "valid zipcode"),  missingzip))

# Restrict Dataset
usts <- subset(usts, gender5!=1) 
usts <- subset(usts, missingzip=="valid zipcode") 
usts <- subset(usts, select = c(idcode, surveyweight, surveyfullweight, 
                                weight18yr, ageweight, raceweight, edweightya, edweightadult, edweighttotal, edraceageweight,
                                gender5, race, race2, raceacs, raceacs2, educationACS18, educationACS25, 
                                agemoyr, agecat4, q2_13, education7, education6, education4, unemprate, q11_1,
                                region, subregion, q25_4anydenied, q25_4anyattack, q25_4anyharass, q25_4any,
                                hivstatus, q14_1, q14_2, q14_3, q14_4_1, q14_4_2a, q14_4_2b, 
                                urbanrural, urbanruralcode, urbanruraldesc, timesubmitted, surveymonth))

# Clean variables
usts$gender <- with(usts, ifelse(gender5==2, "TW", ifelse(gender5==3, "TM", ifelse(gender5==4, "NB AFAB", ifelse(gender5==5, "NB AMAB", NA)))))
usts$raceacs2 <- with(usts, ifelse(raceacs2==1, "AIAN", ifelse(raceacs2==2, "asian_NHPI", ifelse(raceacs2==3, "multiracial_other", ifelse(raceacs2==4, "black", ifelse(raceacs2==5, "hispanic", ifelse(raceacs2==6, "white", NA)))))))
usts$race_black <- ifelse(usts$raceacs2=="black", 1, 0)
usts$race_hispanic <- ifelse(usts$raceacs2=="hispanic", 1, 0)
usts$ageunder25 <- with(usts, ifelse(agecat4==1, 1, ifelse(agecat4 %in% c(2,3,4), 0, NA)))
usts$educ.hsorless <- with(usts, ifelse(education4 %in% c(1,2), 1, ifelse(education4 %in% c(3,4), 0, NA)))
usts$anydiscrim <- with(usts, ifelse(q25_4any==1, 1, 0))

## ever tested
usts$evertested <- with(usts, ifelse(q14_1==1, 1, 0))

## tested in last year
usts$date <- substr(usts$timesubmitted, 1, 9) 
usts$date <- as.Date(usts$date, "%d%b%Y")
usts <- as.data.frame(usts) 
usts$hivtest_month <- with(usts, ifelse(q14_4_2a %in% c(88,99), NA, q14_4_2a))
usts$hivtest_year <- with(usts, ifelse(q14_4_2b %in% c(88,99), NA, 2016-q14_4_2b))
usts <- as.data.table(usts)
usts[,hivtest_date := cases(
  is.na(hivtest_month) | is.na(hivtest_year) -> NA,
  hivtest_month %in% c(1,3,5,7,8) -> paste0("31-0",hivtest_month,"-",hivtest_year),
  hivtest_month == 2              -> paste0("28-02-",hivtest_year),
  hivtest_month %in% c(4,6,9)     -> paste0("30-0",hivtest_month,"-",hivtest_year),
  hivtest_month %in% c(10, 12)    -> paste0("31-",hivtest_month,"-",hivtest_year),
  hivtest_month == 11             -> paste0("30-11-",hivtest_year)
)]
usts <- as.data.frame(usts)
usts$hivtest_date <- as.Date(usts$hivtest_date, "%d-%m-%Y")
usts$hivtest_yearsince <- (as.numeric(usts$date - usts$hivtest_date))/365 # calculate time since last test
usts$lasthivtest_1yr <- ifelse(usts$hivtest_yearsince<=1, 1, 0)
usts$lasthivtest_1yr <- ifelse(is.na(usts$lasthivtest_1yr), 0, usts$lasthivtest_1yr)

#Survey Design Object
design <- svydesign(ids= ~idcode, weights = ~surveyweight, data = usts)

#Save
save(usts, design, file = "2015 USTS PUDS processed.rdata")
