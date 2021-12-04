library(ggplot2)
library(ggmap)
library(maps) 
library(stringr)
library(dplyr)
library(sf)
library(sp)
library(spdep)
library(SpatialEpi)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(gridExtra)
library(Hmisc)
library(INLA)
library(SUMMER)
library(survey)
library(knitr)
library(kableExtra)
library(reshape2)

load("2015 USTS PUDS processed.rdata")


# Load spatial data (https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html) 
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) 
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states))) 
counties <- cbind(counties, st_coordinates(st_centroid(counties)))
names <- as.data.frame(str_split(counties$ID, ",", simplify = TRUE))
counties$region <- names[,1]
counties$subregion <- names[,2]
counties$county_state <- with(counties, paste0(subregion, ", ", region))
usts <- merge(usts, counties[,c("county_state", "ID")], by="county_state", all.x=TRUE)


# Aggregate data by county:
n <- with(usts, aggregate(n, FUN=sum, by=list(county_state)))
names(n) <- c("county_state", "n")

ngender <- with(usts, aggregate(n, FUN=sum, by=list(county_state, gender)))
names(ngender) <- c("county_state", "gender","n")
ngender$gender <- with(ngender, ifelse(gender=="NB AFAB", "NB_AFAB",ifelse(gender=="NB AMAB", "NB_AMAB", gender)))
ngender <- dcast(ngender, county_state ~ gender, value.var="n")

nrace<- with(usts, aggregate(n, FUN=sum, by=list(county_state, raceacs2)))
names(nrace) <- c("county_state", "race","n")
nrace <- dcast(nrace, county_state ~ race, value.var="n")
nrace$multiracial_other_AIAN <- rowSums(nrace[,c("AIAN","multiracial_other")])

nage<- with(usts, aggregate(n, FUN=sum, by=list(county_state, ageunder25)))
names(nage) <- c("county_state", "age","n")
nage <- dcast(nage, county_state ~ age, value.var="n")
names(nage) <- c("county_state", "ageover25", "ageunder25")

n <- merge(n, ngender, by="county_state", all=TRUE)
n$NB <- rowSums(n[,c("NB_AFAB", "NB_AMAB")], na.rm=TRUE)
n <- merge(n, nrace, by="county_state", all=TRUE)
n <- merge(n, nage, by="county_state", all=TRUE)
counties <- merge(counties, n, by="county_state", all.x=TRUE)
rm(n, ngender, nrace, nage)


# Maps
allcounties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
base <- ggplot(data = states) +
  geom_sf() +
  geom_sf(data = allcounties, fill="gray60", color = gray(.9), size = 0.01) +
  geom_sf(data = states, fill = NA) + 
  theme(line = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        title = element_blank(),
        panel.background = element_blank())
base +  
  geom_sf(data = counties, aes(fill = n), size=0.01, color = gray(.1)) + 
  scale_fill_viridis_c(trans="log", alpha = 1, na.value = "grey80", breaks = c(1,10,50,200,500)) + 
  theme(line = element_blank(),
        title = element_blank(),
        panel.background = element_blank())


# SAE Modeling with ICAR Spatial Component
remove <- counties[is.na(counties$n),]$ID #remove counties missing data
counties <- counties[!(counties$ID %in% remove), ]
usts <- usts[usts$ID %in% counties$ID,]
nb.map <- poly2nb(counties) # create neighbors file
nb2INLA("nb.map.graph", nb.map)
#format for input into "fitGeneric" function
mat <- nb2mat(nb.map, style = "B", zero.policy = TRUE) #create a NB MATRIX
rownames(mat) <- colnames(mat) <- counties$ID
geo <- sf:::as_Spatial(counties$geometry) #create a SpatialPolygonsDataFrame object
class(geo)
( pid <- sapply(slot(geo, "polygons"), function(x) slot(x, "ID")) ) # Extract polygon ID's
( df <- data.frame( polyID=1:length(geo), row.names = pid) ) # Create dataframe with correct rownames
geo <- SpatialPolygonsDataFrame(geo, df) # Try coersion again and check class
class(geo) 
geo@data$ID <- counties$ID
#create covariate matrix (Note: The first column of the covariate matrix should be the names of regions that match the column and row names of the adjacency matrix.*)
ETHN <- svyby(~race_hispanic, ~county_state, design, svymean, na.rm=TRUE)
BLKRACE <- svyby(~race_black, ~county_state, design, svymean, na.rm=TRUE)
AGE <- svyby(~ageunder25, ~county_state, design, svymean, na.rm=TRUE)
EDUC <- svyby(~educ.hsorless, ~county_state, design, svymean, na.rm=TRUE)
UNEMP <- svyby(~unemprate, ~county_state, design, svymean, na.rm=TRUE)
DISCRIM <- svyby(~anydiscrim, ~county_state, design, svymean, na.rm=TRUE)
covariates <- cbind(ETHN[,1:2], BLKRACE[,2], AGE[,2], EDUC[,2], UNEMP[,2], DISCRIM[,2])
names(covariates) <- c("county_state", "hispanic", "black", "age_under25", "educ_hsorless", "unemploy", "discrim")
IDxwalk <- with(usts, aggregate(n, FUN=sum, by=list(county_state, ID)))
IDxwalk <- IDxwalk[,1:2]
names(IDxwalk) <- c("county_state", "ID")
covariates <- merge(IDxwalk, covariates, by="county_state")
covariates <- covariates[,2:8]
#fit a smoothing model:
model.evertested <- fitGeneric(data = usts, geo = geo,
                                Amat = mat, X = covariates, responseType = "binary", 
                                responseVar = "evertested", strataVar = NULL, 
                                weightVar = "surveyweight", regionVar = "ID", 
                                clusterVar = NULL, CI = 0.95)
model.lasthivtest_1yr <- fitGeneric(data = usts, geo = geo,
                                     Amat = mat, X = covariates, responseType = "binary", 
                                     responseVar = "lasthivtest_1yr", strataVar = NULL, 
                                     weightVar = "surveyweight", regionVar = "ID", 
                                     clusterVar = NULL, CI = 0.95)

#Combine HT and SAE estimates for mapping
newnames <- c("HT.est", "HT.variance", "SAE.mean", "SAE.variance", "SAE.median", "SAE.lower", "SAE.upper")
evertested <- merge(model.evertested$HT[,c(1,6,7)],  model.evertested$smooth[c(1,8:12)], by="region")
names(evertested) <- c("ID", paste0("evertested.", newnames))
lasthivtest_1yr <- merge(model.lasthivtest_1yr$HT[,c(1,6,7)], model.lasthivtest_1yr$smooth[c(1,8:12)], by="region")
names(lasthivtest_1yr) <- c("ID", paste0("lasthivtest_1yr.", newnames))
counties <- merge(counties, evertested, by="ID", all.x=TRUE)
counties <- merge(counties, lasthivtest_1yr, by="ID", all.x=TRUE)
# map results
median.evertested <- median(counties$evertested.SAE.mean)
median.lasthivtest_1yr <- median(counties$lasthivtest_1yr.SAE.mean)
base + 
  geom_sf(data = counties, aes(fill = evertested.SAE.mean), size=0.01, color = gray(.1)) +
  scale_fill_gradient2(low = "dark red", mid = "white", high = "dark blue", midpoint = median.evertested,
                       limits=c(min(counties$evertested.SAE.mean), max(counties$evertested.SAE.mean)))
base + 
  geom_sf(data = counties, aes(fill = lasthivtest_1yr.SAE.mean), size=0.01, color = gray(.1)) +
  scale_fill_gradient2(low = "orangered3", mid = "white", high = "dark green", midpoint = median.lasthivtest_1yr,
                       limits=c(min(counties$lasthivtest_1yr.SAE.mean), max(counties$lasthivtest_1yr.SAE.mean)))


# Ending the HIV Epidemic geographies
end49 <- c("arizona,maricopa",  "california,alameda", "california,los angeles", "california,orange", "california,riverside", 
           "california,sacramento", "california,san bernardino", "california,san diego","california,san francisco", 
           "florida,broward", "florida,duval", "florida,hillsborough", "florida,miami-dade",  "florida,orange", "florida,palm beach", "florida,pinellas",
           "georgia,cobb", "georgia,de kalb", "georgia,fulton", "georgia,gwinnett", "illinois,cook", "indiana,marion", 
           "louisiana,east baton rouge", "louisiana,orleans", "maryland,baltimore", "maryland,montgomery", "maryland,prince georges",
           "massachusetts,suffolk", "michigan,wayne", "nevada,clark", "new jersey,essex", "new jersey,hudson", 
           "new york,bronx", "new york,kings", "new york,new york", "new york,queens","north carolina,mecklenburg",
           "ohio,cuyahoga", "ohio,franklin", "ohio,hamilton", "pennsylvania,philadelphia", "tennessee,shelby", 
           "texas,bexar", "texas,dallas", "texas,harris", "texas,tarrant", "texas,travis","washington,king", "district of columbia,washington")
states7 <- c("alabama", "arkansas", "kentucky", "mississippi", "missouri", "oklahoma", "south carolina")
counties$end49 <- ifelse(counties$ID %in% end49, 1, 0)
counties$states7 <- ifelse(counties$region %in% states7, 1, 0)
# Forest Plots
ggplot(counties[counties$end49==1,], aes(x=county_state, color=USregion, y=evertested.SAE.mean, ymin=evertested.SAE.lower, ymax=evertested.SAE.upper)) +
  geom_pointrange()+ geom_errorbar(aes(ymin=evertested.SAE.lower, ymax=evertested.SAE.upper),width=0.2,cex=1) + 
  geom_hline(yintercept = mean.evertested, linetype=2) +
  ylab("Proportion (95% CI)") + xlab("County") +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(counties[counties$end49==1,], aes(x=county_state, color=USregion,y=lasthivtest_1yr.SAE.mean, ymin=lasthivtest_1yr.SAE.lower, ymax=lasthivtest_1yr.SAE.upper)) +
  geom_pointrange()+ geom_errorbar(aes(ymin=lasthivtest_1yr.SAE.lower, ymax=lasthivtest_1yr.SAE.upper),width=0.2,cex=1) + 
  geom_hline(yintercept = mean.lasthivtest_1yr, linetype=2) +
  ylab("Proportion (95% CI)") + xlab("County") +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



