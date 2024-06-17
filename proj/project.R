#Loading in CSV
getwd()
setwd("C:/Masters/Predictive Modeling/proj")
FA<-read.csv("FA.csv",header=TRUE)
Lineups<-read.csv("Lineups.csv",header=TRUE)
NBA<-read.csv("NBA.csv",header=TRUE)
Rosters<-read.csv("Rosters.csv",header=TRUE)
SpursSalary<-read.csv("SpursSalary.csv",header=TRUE)

#comparing df
#Removing Spurs players from NBA df
library(dplyr)
NBA_filtered <- anti_join(NBA, Rosters, by = c("PLAYER" = "X2023.24.TEAM.ROSTER"))

#Fixing the FA df
#Removing Restricted Free Agents and Team contracts
FA_filtered <- FA[!grepl("\\bRFA\\b|\\bClub\\b", FA$Type), ]
#Fixing the positions
FA_filtered$Position <- gsub("SF", "F", FA_filtered$Position)
FA_filtered$Position <- gsub("PF", "F", FA_filtered$Position)
FA_filtered$Position <- gsub("PG", "G", FA_filtered$Position)
FA_filtered$Position <- gsub("SG", "G", FA_filtered$Position)
#Truncating the Age Col
FA_filtered$Age <- trunc(FA_filtered$Age)
#Looking into the Free agents' last salary
FA_filtered$X2024.Salary <- as.numeric(gsub("[$,]", "", FA_filtered$X2024.Salary))
FA_filtered$X2024.Salary[is.na(FA_filtered$X2024.Salary)] <- 0
boxplot(FA_filtered$X2024.Salary / 1000000, 
        main = "Boxplot of 2024 Salary (in Million)",
        ylab = "Salary (in Million)",
        col = "skyblue",
        border = "black")

#Understanding the Spurs roster
#Fixing the DF
colnames(Rosters) <- c("Player", "NO.", "Pos", "Height", "Weight", "BirthDate", "Age", "Exp", "School", "HowAcquired")
Rosters <- Rosters[-1, ]
#Average Age of a Spur
Rosters$Age <- as.numeric(as.character(Rosters$Age))
average_age <- mean(Rosters$Age)
print(average_age)
#The average age of a Spur is 23years old
average_age <- mean(Rosters$Age)
boxplot(Rosters$Age, 
        main = "Spur's Age",
        ylab = "Age")
#fixing the Exp column
Rosters$Exp[Rosters$Exp == "R"] <- 0
Rosters$Exp <- as.numeric(Rosters$Exp)

barplot(table(Rosters$Exp), 
        main = "Distribution of Spurs Player's Experience",
        ylab = "Years of Experience",
        xlab = "Frequency",
        col = "grey",
        border = "black",
        horiz = TRUE)
#Most of the spurs Roster has only played 1 year in the NBA 1 player played for 6 years

#Fixing the Pos column
Rosters$Pos <- gsub("F-C", "F", Rosters$Pos)
Rosters$Pos <- gsub("F-G", "F", Rosters$Pos)
Rosters$Pos <- gsub("G-F", "G", Rosters$Pos)
Rosters$Pos <- gsub("C-F", "C", Rosters$Pos)

position_counts <- table(Rosters$Pos)
F_count <- position_counts['F']
C_count <- position_counts['C']
G_count <- position_counts['G']
print(paste("Number of times 'F' is mentioned:", F_count))
print(paste("Number of times 'C' is mentioned:", C_count))
print(paste("Number of times 'G' is mentioned:", G_count))

barplot(table(Rosters$Pos), 
        main = "Distribution of Spurs Player's Positions",
        ylab = "Years of Experience",
        xlab = "Frequency",
        col = "grey",
        border = "black",
        horiz = TRUE)
#Spurs have 1 Center, 10 Forwards, and & 7 guards

#Salary Math
#Based off of the SpursSalary DF the Spurs are going to be spending $118,416,250 before the off season
Budget<-148302289-118416250
print(Budget)
#We see the Spurs can spend $29,886,039 this off season

#Study Lineups DF
#Best 10
#Games Played
sorted_lineups <- arrange(Lineups, desc(GP))
GP10 <- head(sorted_lineups, 10)
print(GP10)

#Minutes Played
sorted_lineups <- arrange(Lineups, desc(MIN))
MIN10 <- head(sorted_lineups, 10)
print(MIN10)

#Points
sorted_lineups <- arrange(Lineups, desc(PTS))
PTS10 <- head(sorted_lineups, 10)
print(PTS10)

#Field Goals Made
sorted_lineups <- arrange(Lineups, desc(FGM))
FGM10 <- head(sorted_lineups, 10)
print(FGM10)

#Field Goals Attempted
sorted_lineups <- arrange(Lineups, desc(FGA))
FGA10 <- head(sorted_lineups, 10)
print(FGA10)

#Field Goals Percentage
sorted_lineups <- arrange(Lineups, desc(FG.))
FG.10 <- head(sorted_lineups, 10)
print(FG.10)

#3points made
sorted_lineups <- arrange(Lineups, desc(X3PM))
X3PM10 <- head(sorted_lineups, 10)
print(X3PM10)

#3points attempted
sorted_lineups <- arrange(Lineups, desc(X3PA))
X3PA10 <- head(sorted_lineups, 10)
print(X3PA10)

#3point percetnage
sorted_lineups <- arrange(Lineups, desc(X3P.))
X3P.10 <- head(sorted_lineups, 10)
print(X3P.10)

#free throw made
sorted_lineups <- arrange(Lineups, desc(FTM))
FTM10 <- head(sorted_lineups, 10)
print(FTM10)

#free throws attempted
sorted_lineups <- arrange(Lineups, desc(FTA))
FTA10 <- head(sorted_lineups, 10)
print(FTA10)

#3points percentage
sorted_lineups <- arrange(Lineups, desc(FT.))
FT.10 <- head(sorted_lineups, 10)
print(FT.10)

#offensive rebound
sorted_lineups <- arrange(Lineups, desc(OREB))
OREB10 <- head(sorted_lineups, 10)
print(OREB10)

#defensive rebound
sorted_lineups <- arrange(Lineups, desc(DREB))
DREB10 <- head(sorted_lineups, 10)
print(DREB10)

#rebounds
sorted_lineups <- arrange(Lineups, desc(REB))
REB10 <- head(sorted_lineups, 10)
print(REB10)

#assists
sorted_lineups <- arrange(Lineups, desc(AST))
AST10 <- head(sorted_lineups, 10)
print(AST10)

#turnovers
sorted_lineups <- arrange(Lineups, desc(TOV))
TOV10 <- head(sorted_lineups, 10)
print(TOV10)

#steals
sorted_lineups <- arrange(Lineups, desc(STL))
STL10 <- head(sorted_lineups, 10)
print(STL10)

#blocks
sorted_lineups <- arrange(Lineups, desc(BLK))
BLK10 <- head(sorted_lineups, 10)
print(BLK10)

#blocks against
sorted_lineups <- arrange(Lineups, desc(BLKA))
BLKA10 <- head(sorted_lineups, 10)
print(BLKA10)

#personal fouls
sorted_lineups <- arrange(Lineups, desc(PF))
PF10 <- head(sorted_lineups, 10)
print(PF10)

#personal fouls drawn
sorted_lineups <- arrange(Lineups, desc(PFD))
PFD10 <- head(sorted_lineups, 10)
print(PFD10)

#plus-minus
sorted_lineups <- arrange(Lineups, desc(X...))
X...10 <- head(sorted_lineups, 10)
print(X...10)

#worst 10
sorted_lineups <- arrange(Lineups,(GP))
WGP10 <- head(sorted_lineups, 10)
print(WGP10)
summary(WGP10)
barplot(table(WGP10$Position.1), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WGP10$Position.2), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WGP10$Position.3), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WGP10$Position.4), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WGP10$Position.5), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
sorted_lineups <- arrange(Lineups,(MIN))
WMIN10 <- head(sorted_lineups, 10)
print(WMIN10)

sorted_lineups <- arrange(Lineups,(PTS))
WPTS10 <- head(sorted_lineups, 10)
print(WPTS10)
barplot(table(WPTS10$Position.4), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WPTS10$Position.2), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WPTS10$Position.3), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
barplot(table(WGP10$Position.4), 
        xlab = "Frequency",
        col = "grey",
        border = "black")
sorted_lineups <- arrange(Lineups, (FGM))
WFGM10 <- head(sorted_lineups, 10)
print(WFGM10)

sorted_lineups <- arrange(Lineups, (FGA))
WFGA10 <- head(sorted_lineups, 10)
print(WFGA10)

sorted_lineups <- arrange(Lineups, (FG.))
WFG.10 <- head(sorted_lineups, 10)
print(WFG.10)

sorted_lineups <- arrange(Lineups, (X3PM))
WX3PM10 <- head(sorted_lineups, 10)
print(WX3PM10)

sorted_lineups <- arrange(Lineups, (X3PA))
WX3PA10 <- head(sorted_lineups, 10)
print(WX3PA10)

sorted_lineups <- arrange(Lineups, (X3P.))
WX3P.10 <- head(sorted_lineups, 10)
print(WX3P.10)

sorted_lineups <- arrange(Lineups, (FTM))
WFTM10 <- head(sorted_lineups, 10)
print(WFTM10)

sorted_lineups <- arrange(Lineups, (FTA))
WFTA10 <- head(sorted_lineups, 10)
print(WFTA10)

sorted_lineups <- arrange(Lineups, (FT.))
WFT.10 <- head(sorted_lineups, 10)
print(WFT.10)

sorted_lineups <- arrange(Lineups, (OREB))
WOREB10 <- head(sorted_lineups, 10)
print(WOREB10)

sorted_lineups <- arrange(Lineups, (DREB))
WDREB10 <- head(sorted_lineups, 10)
print(WDREB10)

sorted_lineups <- arrange(Lineups, (REB))
WREB10 <- head(sorted_lineups, 10)
print(WREB10)

sorted_lineups <- arrange(Lineups, (AST))
WAST10 <- head(sorted_lineups, 10)
print(WAST10)
barplot(table(WAST10$Position.4), 
        main = "Power Foward",
        xlab = "Frequency",
        col = "grey",
        border = "black",
        horiz = TRUE)

sorted_lineups <- arrange(Lineups, (TOV))
WTOV10 <- head(sorted_lineups, 10)
print(WTOV10)

sorted_lineups <- arrange(Lineups, (STL))
WSTL10 <- head(sorted_lineups, 10)
print(WSTL10)

sorted_lineups <- arrange(Lineups, (BLK))
WBLK10 <- head(sorted_lineups, 10)
print(WBLK10)
barplot(table(WBLK10$Position.1), 
        main = "Point Guard",
        xlab = "Frequency",
        col = "grey",
        border = "black",
        horiz = TRUE)

sorted_lineups <- arrange(Lineups, (BLKA))
WBLKA10 <- head(sorted_lineups, 10)
print(WBLKA10)

sorted_lineups <- arrange(Lineups, (PF))
WPF10 <- head(sorted_lineups, 10)
print(WPF10)

sorted_lineups <- arrange(Lineups, (PFD))
WPFD10 <- head(sorted_lineups, 10)
print(WPFD10)

sorted_lineups <- arrange(Lineups, (X...))
WX...10 <- head(sorted_lineups, 10)
print(WX...10)
barplot(table(WX...10$Position.1), 
        main = "Point Guard",
        xlab = "Frequency",
        col = "grey",
        border = "black",
        horiz = TRUE)

#Creating a DF of only G free agents
FA_G <- FA_filtered[FA_filtered$Position == "G", ]
#Creating a DF of only F free agents
FA_F <- FA_filtered[FA_filtered$Position == "F", ]
# I will not be doing C since Victor Wembanyama mostly plays C and is the only safe players in the Spurs roster we wont be looking for a player to replace but a back up
FA_C <- FA_filtered[FA_filtered$Position == "C", ]
#We know that the Spurs only have $29,886,039 usually NBA players want a higher salary than their previous contract so we will remove players that are 29 years or younger that are already making $29,886,039 as the spurs only have this much
FA_G_filtered <- FA_G[!(FA_G$X2024.Salary > 29886039 & FA_G$Age > 29), ]
FA_F_filtered <- FA_F[!(FA_F$X2024.Salary > 29886039 & FA_F$Age > 29), ]
FA_C_filtered <- FA_C[!(FA_C$X2024.Salary > 29886039 & FA_C$Age > 29), ]

#compairing the Lineups DF and NBA DF the Spurs only used 11 players the NBA DF is missing Devonte' Graham and Blake Wesley this can be due to them being cut or injured at the time of data collection. I will add them to the new df 
NBA <- NBA[, !colnames(NBA) %in% "EFF"]
NBA <- NBA[, !colnames(NBA) %in% "X."]
SpursStats <- NBA[NBA$TEAM == "SAS" | NBA$PLAYER == "Doug McDermott", ]
new_row <-data.frame(PLAYER="Devonte Graham",TEAM="SAS",GP=13,MIN=8.51,PTS=3.4,FGM=1.1,FGA=2.6,FG.=31.5,X3PM=0.7,X3PA=1.8,X3P.=28.2,FTM=0.5,FTA=0.6,FT.=24.4,OREB=1.1,DREB=1.2,REB=2.3,AST=1.6,STL=0.3,BLK=0,TOV=0.4)
SpursStats<-rbind(SpursStats,new_row)

new_row <-data.frame(PLAYER="Blake Wesley",TEAM="SAS",GP=43,MIN=13.3,PTS=4.1,FGM=1.6,FGA=3.2,FG.=50.1,X3PM=0.1,X3PA=0.7,X3P.=15.6,FTM=0.7,FTA=1.2,FT.=64,OREB=0.3,DREB=1,REB=1.3,AST=2.7,STL=0.5,BLK=0.2,TOV=0.8)
SpursStats<-rbind(SpursStats,new_row)

new_row <-data.frame(PLAYER="Sidy Cissoko",TEAM="SAS",GP=5,MIN=6.3,PTS=1.2,FGM=0.2,FGA=1.2,FG.=16.7,X3PM=0,X3PA=0.8,X3P.=0,FTM=0.8,FTA=0.8,FT.=100,OREB=0,DREB=1.2,REB=1.2,AST=0,STL=0.6,BLK=0,TOV=0.2)
SpursStats<-rbind(SpursStats,new_row)

new_row <-data.frame(PLAYER="Dominick Barlow",TEAM="SAS",GP=26,MIN=13.5,PTS=4.9,FGM=1.9,FGA=3.7,FG.=50.5,X3PM=0.1,X3PA=0.3,X3P.=37.5,FTM=1,FTA=1.5,FT.=67.5,OREB=1.5,DREB=2.1,REB=3.6,AST=1.2,STL=0.4,BLK=0.5,TOV=0.2)
SpursStats<-rbind(SpursStats,new_row)

new_row <-data.frame(PLAYER="Charles Bassey",TEAM="SAS",GP=19,MIN=10.8,PTS=3.3,FGM=1.5,FGA=3.1,FG.=72.5,X3PM=0,X3PA=0.1,X3P.=0,FTM=0.3,FTA=0.3,FT.=83.3,OREB=1.2,DREB=2.8,REB=4,AST=1.1,STL=0.4,BLK=0.9,TOV=0.8)
SpursStats<-rbind(SpursStats,new_row)

new_row <-data.frame(PLAYER="Sandro Mamukelashvili",TEAM="SAS",GP=35,MIN=6.4,PTS=2.3,FGM=0.9,FGA=2.1,FG.=42.5,X3PM=0.2,X3PA=1,X3P.=22.9,FTM=0.3,FTA=0.5,FT.=70.6,OREB=0.6,DREB=1.1,REB=1.7,AST=0.8,STL=0.2,BLK=0.2,TOV=0.2)
SpursStats<-rbind(SpursStats,new_row)

new_row <-data.frame(PLAYER="Mamadi Diakite",TEAM="SAS",GP=3,MIN=5.3,PTS=4.0,FGM=1.3,FGA=1.7,FG.=80,X3PM=0,X3PA=0,X3P.=0,FTM=1.3,FTA=2,FT.=66.7,OREB=0.3,DREB=0.7,REB=1,AST=0.7,STL=0.7,BLK=0.3,TOV=0.0)
SpursStats<-rbind(SpursStats,new_row)



