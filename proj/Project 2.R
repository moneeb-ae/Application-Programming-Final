#Loading in CSV
getwd()
setwd("C:/Masters/Predictive Modeling/proj")
Lineups<-read.csv("Lineups.csv",header=TRUE)
#NBA players that are active and not injured some df cleaning
NBA<-read.csv("NBA.csv",header=TRUE)
NBA <- NBA[, !colnames(NBA) %in% "EFF"]
NBA <- NBA[, !colnames(NBA) %in% "X."]

#Creating a Spurs Roster DF adding missing players
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

SpursStats <- subset(SpursStats, select = -TEAM)



#Train and test Data
SpursStats$PLAYER <- factor(SpursStats$PLAYER)
levels(SpursStats$PLAYER)
set.seed(123)
train_index <- sample(1:nrow(SpursStats), 0.7 * nrow(SpursStats))
train_data <- SpursStats[train_index, ]
test_data <- SpursStats[-train_index, ]
library(car)
library(randomForest)
model<-randomForest(PTS~PLAYER+GP + MIN + FGM + FGA + FG. + X3PM + X3PA + X3P. + FTM + FTA + FT. + OREB + DREB + REB + AST + TOV + STL + BLK, data=train_data)
print(model)
# Load required packages
library(rpart)

# Visualize the decision tree
plot(model)
text(model)

# Make predictions on the test data using the decision tree model
predictions <- predict(model, newdata = test_data)

# Convert predictions to binary class labels based on a threshold (if needed)
# For example, if predictions > 0.5, classify as 1, else classify as 0
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Create the confusion matrix
conf_matrix <- table(test_data$PTS, binary_predictions)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

library(rpart)
library(rpart.plot)
# Step 1: Load the required packages

# Step 2: Prepare your data
# Assuming you have your data loaded into a dataframe called 'your_data'

# Step 3: Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_index <- sample(1:nrow(SpursStats), 0.7 * nrow(SpursStats))  # 70% of data for training
train_data <- SpursStats[train_index, ]
test_data <- SpursStats[-train_index, ]

# Step 4: Train the decision tree model
model <- rpart(PTS~PLAYER+GP + MIN + FGM + FGA + FG. + X3PM + X3PA + X3P. + FTM + FTA + FT. + OREB + DREB + REB + AST + TOV + STL + BLK, data = train_data)

# Step 5: Visualize the decision tree (optional)
rpart.plot(model)

# Step 6: Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "class")

# Step 7: Evaluate the model
# For classification tasks, you can use metrics like accuracy, precision, recall, F1-score, etc.
# For simplicity, let's calculate accuracy
accuracy <- mean(predictions == test_data$PTS)
print(paste("Accuracy:", accuracy))





