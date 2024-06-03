####################################################################################################
# Initial data check / overview and transformation of "raw" data into working state                #
# Input: data/input/Eksempeldata.csv                                                               #
# Main output: data/processed/Processeddata.csv                                                    #
# Additional output: Three subsets in csv format saved in data/processed/                          #
# Console output: Various data checks and brief data overview                                      #
# Kristoffer Klevjer - github.com/klvr - klevjer(a)gmail.com                                       #
####################################################################################################

# 00 Preparation -----------------------------------------------------------------------------------

## Source own functions
for (i in list.files("src/functions", full.names = TRUE)) {source(i)}

# 01 Load data -------------------------------------------------------------------------------------

df <- read.csv2("data/input/Eksempeldata.csv")

# 02 Surface quality control -----------------------------------------------------------------------

## Respirator
sum(df$Respirator==1) # 5972 were on a respirator at some point during their stay
sum(df$Respirator==2) # 4028 were NOT on a respirator at any point
### All data with valid values (10.000 in total)

## ReAdmitted
sum(df$ReAdmitted==1) # 280 stays were readmissions
sum(df$ReAdmitted==2) # 9720 were NOT readmissions
### All data with valid values (10.000 in total)
### Importantly -> quite few readmissions

## PrimaryReasonAdmitted
sum(df$PrimaryReasonAdmitted>0) # All values 1 or more (int)
sum(df$PrimaryReasonAdmitted<12) # All values 11 or below (int)
### Get a quick overview of the number of patients per primary reason to admittence
for (i in sort(unique(df$PrimaryReasonAdmitted))) {print(c(sum(df$PrimaryReasonAdmitted==i),i))}
#### As expected, some big differences in the amount of patients' primary reason for admittance

## DaysAdmitted
min(as.numeric(df$DaysAdmitted)) # 0 would be a stay below 0.01*24*60min ≈ 14min, i.e. possible
sum(as.numeric(df$DaysAdmitted)==0) # 79 out of 10.000 stays, seems reasonable
max(as.numeric(df$DaysAdmitted)) # 94.2 as max seems reasonable

## PatientAge
min(df$PatientAge, na.rm = TRUE) # 0 is possible
max(df$PatientAge, na.rm = TRUE) # 101 is possible
sum(is.na(df$PatientAge)) # One patient missing age
df[is.na(df$PatientAge),] # Manually inspect -> Not a big problem

## Respirator.1 (time with a respirator)
sum(is.na(df$Respirator.1)) # 372 patients have NA. Could be used as "not on respirator", must check
df[is.na(df$Respirator.1),3]==2 # Seemingly the case for 370 of the patients, 2 however were on resp
                                # yet have NA for the respirator length -> 2 missing values
sum(as.numeric(df[df$Respirator==2,10]) > 0, na.rm= TRUE) # No patients were registered with a resp
                                                          # time and at the same time NOT being on a
                                                          # resp -> As it should be

## TypeOfAdmission
sum(df$TypeOfAdmission==0) # 1055 planned
sum(df$TypeOfAdmission==6) # 7049 emergency medical
sum(df$TypeOfAdmission==8) # 1896 emergency surgical
### I.e., all with a valid value (10.000 in total)

## DischargedStatus
sum(df$DischargedStatus==0) # 8990 left alive
sum(df$DischargedStatus==1) # 1010 died
### I.e., all with a valid value (10.000 in total)

## Saps2ScoreNumber
min(as.numeric(df$Saps2ScoreNumber), na.rm=TRUE) # 0 seems to be the lowest possible number
max(as.numeric(df$Saps2ScoreNumber), na.rm=TRUE) # 114 is below the highest possible number (163;
                                                 # https://clincalc.com/icumortality/sapsii.aspx)
sum(is.na(df$Saps2ScoreNumber)) # 70 patients have missing Saps2ScoreN, must do a quick manual check
df[is.na(df$Saps2ScoreNumber),] # No clear-cut pattern for these. Maybe something related to what
                                # hospital the data is from.

## Saps2Score
sum(is.na(df$Saps2Score)) # As expected, 70 patients have missing Saps2Score as well
cor(as.numeric(df$Saps2Score), as.numeric(df$Saps2ScoreNumber),
    method = "spearman", use = "complete.obs") # Perfect (rank) correlation between the two measures
cor(as.numeric(df$Saps2Score), as.numeric(df$Saps2ScoreNumber),
    method = "pearson", use = "complete.obs") # r=0.96
### Some way of standardizing the Saps2ScoreNumber, going forward I'll just the original (Number)

## SykehusNavn
sum(is.na(df$SykehusNavn)) # All patients have a value for hospital name
sort(unique(df$SykehusNavn)) # All patients have a valid value for hospital name (one of 10)
### Get a quick overview of the number of patients per hospital
for (i in sort(unique(df$SykehusNavn))) {print(c(sum(df$SykehusNavn==i),i))}
#### Some big differences in the amount of data/n_patients from each hospital!

## PatientInRegistryID
length(unique(df$PatientInRegistryID)) # 8281 different patients
### Investigating multiple stays
#### Create DF with IDs and number of stays
IDs <- unique(df$PatientInRegistryID)
alldups <- NULL
singledup <- NULL
for (i in 1:length(IDs)) {
  if (sum(IDs[i]==df$PatientInRegistryID)>1) {
  singledup <- c(IDs[i], sum(IDs[i]==df$PatientInRegistryID))
    alldups <- rbind(singledup, alldups)
    }
}
alldups <- as.data.frame(alldups)
max(as.numeric(alldups[,2])) # Maximum number of stays was 11
for (i in 2:11) {print(c(sum(as.numeric(alldups[,2]==i)), i))} # n_patients with i stays
#### Important to note that multiple patients had multiple stays, some had a lot!

# 03 Reduction of data set -------------------------------------------------------------------------

## Form IDs are removed as they are not used
## ChronicDiseases is removed due to unknown coding
## Saps2Score is removed, but Saps2ScoreNumber is kept, due to unknown transformation
dfbackup <- df
df <- df[,-c(1,2,13,15)]

# 04 Transformations of variables ------------------------------------------------------------------

## Respirator is changed into 1 = TRUE, 0 = FALSE (no respirator used)
df$Respirator <- replace(df$Respirator, df$Respirator==2, 0)

## ReAdmitted is changed into 1 = TRUE, 0 = FALSE (not readmitted)
df$ReAdmitted <- replace(df$ReAdmitted, df$ReAdmitted==2, 0)

## Respirator.1 variable name is changed to RespiratorTime
colnames(df)[8] <- "RespiratorTime"

## Saps2ScoreNumber variable name is changed to Saps2Score (for better use in graphics)
colnames(df)[11] <- "Saps2Score"

## RespiratorTime are set to '0' for 370 patients (from NA; see line 50)
df$RespiratorTime <- replace(df$RespiratorTime, 
                             c(rowSums(cbind(df$Respirator==0, is.na(df$RespiratorTime)))==2), 0)

## Split DateAdmitted and DateDischarged into
## DateAdmitted, TimeAdmitted, DateDischarged, TimeDischarged
df$TimeAdmitted <- 0
df$TimeDischarged <- 0
for (i in 1:nrow(df)) {df[i,13] <- unlist(strsplit(df[i,2], split = " "))[2]}
for (i in 1:nrow(df)) {df[i,2] <- unlist(strsplit(df[i,2], split = " "))[1]}
for (i in 1:nrow(df)) {df[i,14] <- unlist(strsplit(df[i,5], split = " "))[2]}
for (i in 1:nrow(df)) {df[i,5] <- unlist(strsplit(df[i,5], split = " "))[1]}
### Create DayAdmitted, MonthAdmitted, YearAdmitted, DayDischarged, MonthDischarged, YearDischarged
df$DayAdmitted <- 0
df$MonthAdmitted <- 0
df$YearAdmitted <- 0
df$DayDischarged <- 0
df$MonthDischarged <- 0
df$YearDischarged <- 0
for (i in 1:nrow(df)) {df[i,15] <- paste(unlist(strsplit(df[i,2],
                                                         split = ""))[c(1,2)], collapse = "")}
for (i in 1:nrow(df)) {df[i,16] <- paste(unlist(strsplit(df[i,2],
                                                         split = ""))[c(4,5)], collapse = "")}
for (i in 1:nrow(df)) {df[i,17] <- paste(unlist(strsplit(df[i,2],
                                                         split = ""))[c(7,8,9,10)], collapse = "")}
for (i in 1:nrow(df)) {df[i,18] <- paste(unlist(strsplit(df[i,5],
                                                         split = ""))[c(1,2)], collapse = "")}
for (i in 1:nrow(df)) {df[i,19] <- paste(unlist(strsplit(df[i,5],
                                                         split = ""))[c(4,5)], collapse = "")}
for (i in 1:nrow(df)) {df[i,20] <- paste(unlist(strsplit(df[i,5],
                                                         split = ""))[c(7,8,9,10)], collapse = "")}

## Fix variable types
df$DaysAdmitted <- as.numeric(df$DaysAdmitted)
df$RespiratorTime <- as.numeric(df$RespiratorTime)
df <- cbind(dfbackup[,2], df) # Made an error on line 97
colnames(df)[1] <- "PatientInRegistryID"

## Create variable for number of stays per patients
df$Nstays <- 1
for (i in 1:nrow(df)) {
  if (sum(df[i,1]==alldups[,1])>0) {
  df[i,22] <- alldups[alldups[,1]==df[i,1],2]
    }
}
### Be aware that this variable is then replicated per stay! I.e., if you count all rows with Nstay
### of 11, you will get 11 rows. However all of these are the same one patient

# 05 Create reduced dfs  ---------------------------------------------------------------------------

## Create a reduced df with all readmitted, and their other stays
IDsReAd <- df[df$ReAdmitted==1,1]
df$ReAd <- 0
for (i in 1:nrow(df)) {
  if (sum(df[i,1]==IDsReAd)>0) {
    df[i,23] <- 1
  }
}
dfReAd <- df[df$ReAd==1,]

## Create a reduced df with all with multiple stays
dfMultiple <- df[df$Nstays>1,]

# 06 Create df with summary data for all paired stays (readmitted and original admission) ----------

## This section needs to be cleaned!
dfread <- dfReAd
### Fetch unique IDs
uniqueIDs <- unique(dfread$PatientInRegistryID)
### Record the ones with only one entry
onlyone <- NULL
for (i in uniqueIDs) {
  pati <- dfread[dfread$PatientInRegistryID==i,]
  if (nrow(pati)<2) {
    onlyone <- rbind(pati$PatientInRegistryID, onlyone)
  }
}
### Create reduced DF with only one entries removed
nyunique <- NULL
for (i in 1:length(uniqueIDs)) {
  if(sum(uniqueIDs[i]==onlyone[,1])==0) {
    nyunique <- c(uniqueIDs[i], nyunique)
  }
}
dfredorg <- NULL
for (i in 1:nrow(dfread)){
  if(sum(dfread$PatientInRegistryID[i]==nyunique)>0) {
    dfredorg <- rbind(dfredorg, dfread[i,])
  }
}
### Fix dates
dfredorg$DateAdmitted <- as.Date(dfredorg$DateAdmitted, tryFormats = c("%d.%m.%Y"))
dfredorg$DateDischarged <- as.Date(dfredorg$DateDischarged, tryFormats = c("%d.%m.%Y"))
### Sort based on DA
dfredorg <- dfredorg[order(dfredorg$DateAdmitted),]
### Create DFs for each patient and remove unrelated stays
dfredfin <- NULL
for (i in 1:length(nyunique)) {
  pati <- dfredorg[dfredorg$PatientInRegistryID==nyunique[i],]
  ### Order 'dis-ad'
  delete <- NULL
  for (i in 2:nrow(pati)) {
    deletethis <- (pati[i, 4] - pati[i-1, 7])
    if(deletethis<4) {delete <- c(delete,i,i-1)}
  }
  delete <- sort(unique(delete))
  pati <- pati[delete,]
  dfredfin <- rbind(dfredfin, pati)
}
### Delete stays that took place within the 72h mark, but nonetheless are marked as not readmitted
### Create DFs for each patient and get some overview over their stays
nyunique2 <- unique(dfredfin$PatientInRegistryID)
dfpaired <- NULL
for (i in 1:length(nyunique2)) {
  pati <- dfredfin[dfredfin$PatientInRegistryID==nyunique2[i],]
  #### First remove unrelated stays (not linked adm-readm)
  last <- NULL
  for (i in 1:nrow(pati)) {
    lastread <- pati[i,5]
    last <- c(last, lastread)
  }
  fixed <- which(last == TRUE)
  for (i in 1:length(fixed)) {
    last[fixed[i]-1] <- 1
  }
  last[which(last == TRUE)[1]-1] <- 1
  if(sum(last)==1) {last <- rep(0,length(last))}
  pati <- pati[which(last==1),]
  #### Create a summary for each patient
  patID <- pati[1,2]
  patPrimF <- pati[1,6]
  patPrimR <- pati[1,6]==pati[2,6]
  patDaysF <- pati[1,8]
  patDaysR <- pati[2,8]
  patRespF <- pati[1,3]
  patRespR <- pati[2,3]
  patTypF <- pati[1,11]
  patTypR <- pati[1,11]==pati[2,11]
  patSur <- sum(pati[,12])
  patSapsF <- pati[1,13]
  patSapsR <- pati[2,13]
  pati <- c(patID,patPrimF,patPrimR,patDaysF,patDaysR,patRespF,patRespR,patTypF,patTypR,patSur,
            patSapsF,patSapsR)
  dfpaired <- rbind(dfpaired, pati)
}
dfpaired <- as.data.frame(dfpaired)
dfpaired <- dfpaired[!is.na(dfpaired[,1]),]
colnames(dfpaired) <- c("PatientInRegistryID", "PrimaryReasonAdmittedFA", "PrimaryReasonAdmittedRA",
                        "DaysAdmittedFA","DaysAdmittedRA","RespiratorFA", "RespiratorRA",
                        "TypeOfAdmissionFA", "TypeOfAdmissionRA","DischargedStatus","Saps2ScoreFA",
                        "Saps2ScoreRA")

# 05 Save transformed data -------------------------------------------------------------------------

## Data saved in a separate folder named processed, as all in 'input' is treated as immutable
write.csv(df[,-23], "data/processed/Processeddata.csv")
write.csv(dfReAd, "data/processed/ReAddata.csv")
write.csv(dfMultiple, "data/processed/MultipleAd.csv")
write.csv(dfpaired, "data/processed/PairedSummary.csv")
