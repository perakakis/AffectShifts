# load libraries
library(dplyr)
library(psych)
options(warn = -1)

# df1 = postcovid ####
df1_ema <- read.csv(file = "./data/postcovid_EMA.csv")
df1_ema <- rename(df1_ema, day=study_day, beep=occasion)

# replace with NaNs rows where valence, arousal and energy remained at initial values
ind <- which(df1_ema$valence==50 & df1_ema$energetic_arousal==50 & 
               df1_ema$tense_arousal==50)
df1_ema$valence[ind] = NA

# estimate adherence
PIDs <- unique(df1_ema$participant)

for (i in seq(1, length(PIDs))) {
  y <- subset(df1_ema,participant==PIDs[i])
  y <- y$valence # select the valence variable
  df1_ema$adh[df1_ema$participant==PIDs[i]] <- 
    1-length(which(is.na(y)))/length(y)
}
  
df1_survey <- read.csv(file = "./data/postcovid_survey.csv")

# Get total scores
df1_survey$GAD <- df1_survey$GAD_1 + df1_survey$GAD_2 + df1_survey$GAD_3 + 
  df1_survey$GAD_4 + df1_survey$GAD_5 + df1_survey$GAD_6 + df1_survey$GAD_7

df1_survey$PHQ <- df1_survey$PHQ_1 + df1_survey$PHQ_2 + df1_survey$PHQ_3 + 
  df1_survey$PHQ_4 + df1_survey$PHQ_5 + df1_survey$PHQ_6 + df1_survey$PHQ_7 +
  df1_survey$PHQ_8 + df1_survey$PHQ_9

df1_survey$BRS <- (df1_survey$BRS_1 + df1_survey$BRS_3 + df1_survey$BRS_5 +
  (6-df1_survey$BRS_2) + (6-df1_survey$BRS_4) + (6-df1_survey$BRS_6))/6

df1_survey$FS <- df1_survey$FS_1 + df1_survey$FS_2 + df1_survey$FS_3 + 
  df1_survey$FS_4 + df1_survey$FS_5 + df1_survey$FS_6 + df1_survey$FS_7 + 
  df1_survey$FS_8

df1_survey$AAQ <- df1_survey$AAQ_1 + df1_survey$AAQ_2 + df1_survey$AAQ_3 + 
  df1_survey$AAQ_4 + df1_survey$AAQ_5 + df1_survey$AAQ_6 + df1_survey$AAQ_7

df1_survey <- rename(df1_survey, SWLS=GLS, sex=gender)

df1 <- merge(df1_ema,df1_survey, by = "participant")
df1$Study <- 1

# Create vector of participant number
df1 <- transform(df1,PID = match(participant, unique(participant)))

df1 <- df1[c("Study","participant","PID","age","sex","adh","day","beep","valence",
             "PHQ","GAD","AAQ",
             "FS", "BRS", "SWLS")]

df1 <- arrange(df1,PID, day, beep)

# df2 = marlon ####
df2_ema <- read.csv(file = "./data/marlon_EMA.csv")
df2_ema <- rename(df2_ema, PID=VID, valence=Core.Affect.Valence,
                  arousal=Core.Affect.Arousal, 
                  day=Day, beep=Session)

# replace with NaNs rows where valence, arousal and energy remained at initial values
ind <- which(df2_ema$valence==50 & df2_ema$arousal==50)
df2_ema$valence[ind] = NA

# replace -1 with NAs
ind <- which(df2_ema$valence==-1)
df2_ema$valence[ind] = NA

PIDs <- unique(df2_ema$PID)
for (i in seq(1, length(PIDs))) {
  y <- subset(df2_ema,PID==PIDs[i])
  y <- y$valence # select the valence variable
  df2_ema$adh[df2_ema$PID==PIDs[i]] <- 
    1-length(which(is.na(y)))/length(y)
}

# Create valence vector subtracting 50
df2_ema$valence <- df2_ema$valence-50

df2_survey <- read.csv(file = "./data/marlon_survey.csv")
df2_survey <- rename(df2_survey, PID=VID)

df2 <- merge(df2_ema,df2_survey, by = "PID")
df2$Study <- 2

df2 <- transform(df2,id = match(PID, unique(PID)))
df2$id <- df2$id + tail(df1$PID,n=1)

# Keep original participant variable
df2$participant <- df2$PID

df2 <- df2[c("Study","participant","id","adh","day","beep","Gender.x","Age.x","valence",
             "DASS.Depression","DASS.Anxiety","AAQ",
             "BriefResilience","SatisfactLife")]
df2 <- rename(df2, PID=id, DASSd=DASS.Depression, DASSa=DASS.Anxiety,
              SWLS=SatisfactLife, sex=Gender.x, age=Age.x, BRS=BriefResilience)

# all data ####
# df1
N1 <- length(unique(df1$PID))
df1 <- subset(df1,df1$adh>0.75)
N2 <- length(unique(df1$PID))
# exclude subjects for artifacts or livpotential
ls <- c(19,23,42,45,67,71,73,95,101) # PID=35 anxiety GAD=19
df1 <- subset(df1, !PID%in%ls)

#df2
N1 <- length(unique(df2$PID))
df2 <- subset(df2,df2$adh>0.75)
N2 <- length(unique(df2$PID))

dfs <- plyr::rbind.fill(df1,df2)
dfs <- dfs[c("Study","participant","PID","age","sex","adh","day","beep",
             "valence", "GAD", "PHQ","AAQ", "FS", "BRS", "SWLS",
             "DASSd", "DASSa")]

totalN <- length(unique(dfs$PID)) # 120