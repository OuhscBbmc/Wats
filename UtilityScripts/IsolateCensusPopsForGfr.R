rm(list=ls(all=TRUE))
require(plyr)
require(testit)
inputPathsCensus199x <- paste0("./Datasets/CensusIntercensal/STCH-icen199", 0:9, ".txt")
inputPathCensus200x <- "./Datasets/CensusIntercensal/CO-EST00INT-AGESEX-5YR.csv"
inputPathFips <- "./Datasets/CountyFipsCode.csv"
ouputPathsCensusCountyYear <- paste0("./Datasets/CensusIntercensal/CensusCountyYear.csv")

###################
# Read in the datasets
###################
lstDatasets199x <- lapply(inputPathsCensus199x, function(path) dsCensus199x <- read.table(path, header=FALSE, stringsAsFactors=F)) #A list, where each element is a data.frame.
dsCensus199x <- data.frame(do.call(plyr::rbind.fill, lstDatasets199x), stringsAsFactors=FALSE) #Vertically stack all the data.frames into a single data.frame

dsCensus200x <- read.csv(inputPathCensus200x, stringsAsFactors=F)

dsFips <- read.csv(inputPathFips, stringsAsFactors=F)

#For 199x: See the codebook at ./Datasets/CensusIntercensal/STCH-Intercensal_layout.txt.  The State FIPS is missing for some reason
#For 200x: See the codebook at ./Datasets/CensusIntercensal/CO-EST00INT-AGESEX-5YR.pdf.
colnames(dsCensus199x) <- c("Year", "Fips", "AgeGroup", "RaceGender", "Latino", "PopulationCount")
ageGroupLabels <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49","50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
raceGenderLabels <- c("White male", "White female", "Black male", "Black female", "American Indian or Alaska Native male", "American Indian or Alaska Native female", "Asian or Pacific Islander male", "Asian or Pacific Islander female") #Just for 199x.
genderLabels <- c("Total", "Male", "Female") #Just for 200x.

#Identify and isolate the levels need to calculate GFR (ie, females between 15 & 44)
eligibleRaceGenderLabels <- c("White female","Black female", "American Indian or Alaska Native female", "Asian or Pacific Islander female")
eligibleAgeLabels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")

###################
# Groom the Census data from the 1990s & Keep only the needed rows
###################

#Groom the variables and assign the appropriate factor levels
dsCensus199x$Year <- as.integer(dsCensus199x$Year + 1900) #Convert to a four digit year
# dsCensus199x$CountyFips #This looks good as it is.  Be careful if you live in a state with a leading zero in the FIPS
dsCensus199x$AgeGroup <- factor(x=dsCensus199x$AgeGroup, levels=0:18, labels=ageGroupLabels, ordered=TRUE)
dsCensus199x$RaceGender <- factor(x=dsCensus199x$RaceGender, levels=1:8, labels=raceGenderLabels)
dsCensus199x$Latino <- as.logical(factor(dsCensus199x$Latino, levels=1:2, labels=c(TRUE, FALSE)))

#Assert the values aren't too funky.
testit::assert("All years in dsCensus199x should be in the 1990s", all(1990 <= dsCensus199x$Year & dsCensus199x$Year <=1999))
testit::assert("All County FIPS should start with 40 (ie, be in Oklahoma).", all(grepl(pattern="^40\\d{3}$", x=dsCensus199x$CountyFips, perl=TRUE)))
testit::assert("The mean of the Latino values should be 0.5.", mean(dsCensus199x$Latino)==0.5)
sapply(dsCensus199x, class)

#Identify Mark the subgroups eligible to be included in the GFR denominator.
dsCensus199x$GfrEligible <- (dsCensus199x$AgeGroup %in% eligibleAgeLabels & dsCensus199x$RaceGender %in% eligibleRaceGenderLabels)
testit::assert("The proportion of GFR eligible rows should be correct.", mean(dsCensus199x$GfrEligible) == (6/19 * 1/2))

#Merge the counties by the FIPS to get their county name and urban/rural status.
dsCensus199x <- merge(x=dsCensus199x, y=dsFips, by="Fips", all.x=TRUE, all.y=FALSE)

#Keep only the eligble groups
dsCensus199x <- dsCensus199x[dsCensus199x$GfrEligible & dsCensus199x$WatsUrban, ]

#Sum across the remaining subgroups to get their total population.
dsCensus199xCounty <- plyr::ddply(dsCensus199x, .variables=c("Fips", "Year", "CountyName"), summarize, PopulationCount=sum(PopulationCount))

###################
# Groom the Census data from the 2000s & Keep only the needed rows
###################
dsCensus200x$Fips <- 40000L + dsCensus200x$COUNTY
dsCensus200x$AgeGroup <- factor(x=dsCensus200x$AGEGRP, levels=0:18, labels=ageGroupLabels, ordered=TRUE)
dsCensus200x$Gender <- factor(dsCensus200x$SEX, levels=0:2, labels=genderLabels)

#Identify Mark the subgroups eligible to be included in the GFR denominator.
dsCensus200x$GfrEligible <- (dsCensus200x$AgeGroup %in% eligibleAgeLabels & dsCensus200x$Gender %in% "Female")
testit::assert("The proportion of GFR eligible rows should be correct.", mean(dsCensus200x$GfrEligible) == (6/19 * 1/3))

#Merge the counties by the FIPS to get their county name and urban/rural status.
dsCensus200x <- merge(x=dsCensus200x, y=dsFips, by="Fips", all.x=TRUE, all.y=FALSE)

#Keep only the eligble groups
dsCensus200x <- dsCensus200x[dsCensus200x$GfrEligible & dsCensus200x$WatsUrban, ]

#Sum across the remaining subgroups to get their total population.  Keep only 2000 (Remember 200x is wide, not long)
dsCensus200xCounty <- plyr::ddply(dsCensus200x, .variables=c("Fips", "CountyName"), summarize, PopulationCount=sum(POPESTIMATE2000))

dsCensus200xCounty$Year <- 2000L 

###################
# Merge the two decades
###################
dsCensusCountyYear <- plyr::rbind.fill(dsCensus199xCounty, dsCensus200xCounty)
dsCensusCountyYear <- dsCensusCountyYear[order(dsCensusCountyYear$Fips, dsCensusCountyYear$Year), ]

###################
# Write to disk
###################
write.csv(dsCensusCountyYear, file=ouputPathsCensusCountyYear, row.names=FALSE)
