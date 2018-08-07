library(readr)
Data <- read_csv("Data.csv")
install.packages("dplyr")
## Changing the char values in to factors 

Data$`Overall Pick` <- as.factor(Data$`Overall Pick`)
Data$`Salary Total` <- as.factor(Data$`Salary Total`)
Data$`Status` <- as.factor(Data$`Status`)
Data$`WAR Status` <- as.factor(Data$`WAR Status`)
Data$`Drafted` <- as.factor(Data$`Drafted`)
Data$`League` <- as.factor(Data$`League`)
# check if the type changed. 
head(Data)


# needed
install.packages("ggpubr")

## War first then voter percentagte
OPBP <- boxplot(`Adjusted WAR` ~ `Status`, data=Data, frame = FALSE, 
                 col = c("#00AFBB", "#E7B800"), ylab="WAR")
POSBP <- boxplot(`Adjusted WAR` ~ `Position`, data=Data, frame = FALSE, 
                col = c("#00AFBB", "#E7B800"), ylab="WAR")
LevelBP <- boxplot(`Adjusted WAR` ~ `WAR Status`, data=Data, frame = FALSE, 
                 col = c("#00AFBB", "#E7B800"), ylab="WAR")
TeamBP <- boxplot(`Adjusted WAR` ~ `Draft Team`, data=Data, frame = FALSE, 
                  col = c("#00AFBB", "#E7B800"), ylab="WAR")

VoteBP <- boxplot(`Vote Percentage` ~ `Race` * `Year Group`, data=Hall_of_Fame_Data_R_Data_HOF, frame = FALSE, 
                  col = c("#00AFBB", "#E7B800"), ylab="%")


# two way Anova War 
DraftAnova<-res.aov2 <- aov(`Adjusted WAR` ~ `Drafted`, data=Data)
summary(DraftAnova)
TeamAnova<-res.aov2 <- aov(`Adjusted WAR` ~ `Team`, data=Data)
summary(TeamAnova)
PositionAnova<-res.aov2 <- aov(`Adjusted WAR` ~ `Position`, data=Data)
summary(PositionAnova)

PDAnova <- aov(`Adjusted WAR` ~ `Position` + `Drafted` + `Position`:`Drafted`, data=Data)
summary(PDAnova)

PDTAnova <- aov(`Adjusted WAR` ~ `Position` + `Drafted` +`Team` + `Position`:`Drafted`+`Position`:`Drafted`:`Team`, data=Data)
summary(PDTAnova)
 
PTAnova <- aov(`Adjusted WAR` ~ `Position` + `Team` + `Position`:`Team`, data=Data)
summary(PTAnova)

## ANOVA Results 

SumAnova <- aov(`Adjusted WAR` ~ `Drafted`+`Position` +`Team` +`League`+ `Drafted`:`Position`+`Drafted`:`Team`+`Drafted:League`+`Position`:`Team`+`Position`:`League`+`Team`:`League`+`Drafted`:`Position`:`Team`+`Drafted`:`Position`:`Team`:`League`, data=Data)
summary(SumAnova)

Sum2Anova <- aov(`Adjusted WAR` ~`Position`:`League`+`Team`:`League`+`Drafted`:`Position`:`Team`+`Drafted`:`Position`:`Team`:`League`, data=Data)
summary(Sum2Anova)
DraftAnova2 <- aov(WAR ~ `Year Group` + `Race` + `Year Group`:`Race`, data=Hall_of_Fame_Data_R_Data_HOF)
summary(IndAnova2)

Sum2Anova <- aov(`Adjusted WAR` ~`Drafted`+`Position` +`Team` +`League`+ `Drafted`:`Position`+`Drafted`:`Team`+`Position`:`Team`+`Drafted`:`League`+`Position`:`League`+`Team`:`League`, data=Data)
summary(Sum2Anova)

DAnova <- aov(`Adjusted WAR` ~`Drafted`, data = Data)
summary(DAnova)

DAnova<-res.aov2 <- aov(`Adjusted WAR` ~ `Drafted`, data=Data)
summary(DAnova)

SalAnova<-res.aov2 <- aov(`Salary Total` ~ `Drafted`, data=Data)
summary(SalAnova)
