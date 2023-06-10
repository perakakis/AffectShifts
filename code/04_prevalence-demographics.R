source("./code/02_extract.R")

# Study 1 ####
df <- subset(dfs,Study==1)
# get bistability percentages
bistableS1 <- round(length(unique(df$PID[df$Type==1]))/
                    length(unique(df$PID)),digits = 3)
monostableS1 <- round(length(unique(df$PID[df$Type==0]))/
                      length(unique(df$PID)),digits = 3)

# print bistability percentage
print(paste0("Study 1, bistable: ",bistableS1, " monostable: ",monostableS1))

# Study 2 ####
df <- subset(dfs,Study==2)

# get bistability percentages
bistableS2 <- round(length(unique(df$PID[df$Type==1]))/
                    length(unique(df$PID)),digits = 2)
monostableS2 <- round(length(unique(df$PID[df$Type==0]))/
                      length(unique(df$PID)),digits = 2)

# print bistability percentage
print(paste0("Study 2, bistable: ",bistableS2, " monostable: ",monostableS2))

# demographics
df1 <- aggregate(df1, list(df1$PID), data = df1, FUN = head,1)
df2 <- aggregate(df2, list(df2$PID), data = df2, FUN = head,1)

meanAgeS1 <- mean(df1$age)
sdAgeS1 <- sd(df1$age)
minAgeS1 <- min(df1$age)
maxAgeS1 <- max(df1$age)
maleS1 <- nrow(df1[df1$sex == "male",])/nrow(df1)
femaleS1 <- nrow(df1[df1$sex == "female",])/nrow(df1)

meanAgeS2 <- mean(df2$age)
sdAgeS2 <- sd(df2$age)
minAgeS2 <- min(df2$age)
maxAgeS2 <- max(df2$age)
maleS2 <- nrow(df2[df2$sex == 2,])/nrow(df2)
femaleS2 <- nrow(df2[df2$sex == 1,])/nrow(df2)

write.csv(df1,"./data/df1.csv")
write.csv(df2,"./data/df2.csv")

# # Create and save summary table
# source("./code/table_studiesSummary.R")
# gtsave(tbl, "./tables/table_StudiesSummary.html")
# gtsave(tbl, file = "./tables/table_StudiesSummary.pdf")
# gtsave(tbl, file = "./tables/table_StudiesSummary.png",
#        vwidth = 1400,
#        vheight = 600,
#        zoom = 2)
# gtsave(tbl, file = "./tables/table_StudiesSummary.docx")
