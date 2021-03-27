---
title: "trab final I"
author: "Daniel Serra"
date: "12/09/2020"
output: html_document
---

knitr::opts_chunk$set(echo = TRUE, message = F, warning = F)

# Objective: Create a predictor capable of answering whether a player will stay at least 5 years in the NBA, according to his rookie stats.


library(caret)
library(gbm)
library(readr)
library(readxl)
library(dplyr)
library(VIM)


# Loading datasets.
# I've used two datasets to complement the original. The first contains the draft year of the players. The second one contains their college and height.


# Original dataset
original <- read.csv("https://query.data.world/s/6uhvcowok54gcj5jz3i2ccwldlb6rl", 
                  header=TRUE, stringsAsFactors=FALSE)

# Draft Year data
data_year <- read_excel("C:/Users/Daniel/Desktop/Estatística/Faculdade/6º Período/Aprendizado de Máquinas/Listas de Exercícios/Trabalho Final I/NBA Rookies by Year.xlsx")
data_year <- data_year[,c(1,2)]

# College and height data
data_height <- read_csv("C:/Users/Daniel/Desktop/Estatística/Faculdade/6º Período/Aprendizado de Máquinas/Listas de Exercícios/Trabalho Final I/ano e college.csv", 
    col_types = cols(player_height = col_double()))
data_height <- data_height[,c(2,6,8)]

# Join them all
data_ <- inner_join(original, data_year, by = "Name")
data_height <- data_height %>% rename(Name = player_name) %>% distinct()
data_ <- left_join(data_, data_height, by = 'Name')


# I notice that, from the original dataset the only variable with NA is 3 point percentage. As we have 3 point attempt and 3 points made, we can calculate the missing ones.
# They're all zeros.


names(data_)[colSums(is.na(data_)) > 1]
data_$X3P.[which(is.na(data_$X3P.))] <- 0 
names(data_)[colSums(is.na(data_)) > 1]


# With data_draft we can see that some players didn't had the chance to complete 5 years in the league(the original data is from 2016), so I'm gonna remove them.


data_ <- filter(data_, `Year Drafted` < 2013)


# Removing duplicated rows.
# Removing duplicated players with different heights. Maintaining the tallest ones.


data_ <- distinct(data_)

data_1 <- data_ %>% group_by(GP, MIN, PTS, REB, AST, `Year Drafted`, college, TARGET_5Yrs) %>%  slice_max(player_height, n=1, with_ties = F) %>% ungroup()


# Here I fix some observations of the dataset that contained some wrong information.


freq <- as.data.frame(epiDisplay::tab1(data_1$Name, graph = F))
repeated <- filter(freq, output.table.Frequency >1)
repeated <- repeated[,c(1,2)]

data_2 <- data_1[!(data_1$Name == 'Bobby Jones' & data_1$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Carlos Rogers' & data_2$GP == 81),]
data_2 <- data_2[!(data_2$Name == 'Cedric Henderson' & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Charles Jones'),]

data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 71 & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 71 & data_2$player_height == 1.93),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 71 & data_2$`Year Drafted` == 1989),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 71 & data_2$`Year Drafted` == 1997),]

data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$TARGET_5Yrs == 1 & data_2$GP == 34),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 34 & data_2$player_height == 2.08),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 34 & data_2$`Year Drafted` == 1988),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 34 & data_2$`Year Drafted` == 1989),]

data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$TARGET_5Yrs == 1 & data_2$GP == 60),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 60 & data_2$`Year Drafted` == 1988),]
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 60 & data_2$`Year Drafted` == 1997),]
data_2[527, 24] <- 'Georgetown'
data_2[527, 23] <- 1.85
data_2 <- data_2[!(data_2$Name == 'Charles Smith' & data_2$GP == 60 & data_2$player_height == 2.08),]

data_2 <- data_2[!(data_2$Name == 'Cory Alexander' & data_2$GP == 59),]
data_2 <- data_2[!(data_2$Name == 'David Lee' & data_2$TARGET_5Yrs == 0),]

data_2 <- data_2[!(data_2$Name == 'Dee Brown' & data_2$GP == 49 & data_2$TARGET_5Yrs == 1),]
data_2 <- data_2[!(data_2$Name == 'Dee Brown' & data_2$GP == 49 & data_2$`Year Drafted` == 1990),]
data_2 <- data_2[!(data_2$Name == 'Dee Brown' & data_2$GP == 49 & data_2$college == 'Jacksonville'),]

data_2 <- data_2[!(data_2$Name == 'Dee Brown' & data_2$GP == 82 & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Dee Brown' & data_2$GP == 82 & data_2$`Year Drafted` == 2006),]
data_2 <- data_2[!(data_2$Name == 'Dee Brown' & data_2$GP == 82 & data_2$college == 'Illinois'),]

data_2 <- data_2[!(data_2$Name == 'Glen Rice' & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Glen Rice' & data_2$college == 'Georgia Tech'),]

data_2 <- data_2[!(data_2$Name == 'Glenn Robinson' & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'James Johnson' & data_2$GP == 68),]

data_2 <- data_2[!(data_2$Name == 'Jamie Feick' & data_2$GP == 28),]
data_2 <- data_2[!(data_2$Name == 'Jamie Feick' & data_2$`Year Drafted` == 1998),]

data_2 <- data_2[!(data_2$Name == 'Jeff Taylor' & data_2$GP == 77 & data_2$`Year Drafted` == 1982),]
data_2 <- data_2[!(data_2$Name == 'Jeff Taylor' & data_2$GP == 44 & data_2$`Year Drafted` == 2012),]

data_2 <- data_2[!(data_2$Name == 'Larry Drew' & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Larry Johnson' & data_2$TARGET_5Yrs == 0),]


data_2 <- data_2[!(data_2$Name == 'Marcus Williams' & data_2$college == 'Arizona'),]

data_2 <- data_2[!(data_2$Name == 'Mark Bryant' & data_2$GP == 36),]

data_2 <- data_2[!(data_2$Name == 'Mark Davis' & data_2$GP == 57 & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Mark Davis' & data_2$GP == 57 & data_2$`Year Drafted` == 1988),]

data_2 <- data_2[!(data_2$Name == 'Mark Davis' & data_2$GP == 33 & data_2$TARGET_5Yrs == 1),]
data_2 <- data_2[!(data_2$Name == 'Mark Davis' & data_2$GP == 33 & data_2$`Year Drafted` == 1995),]
data_2[50,24] <- 'Old Dominion'
data_2[50,23] <- 1.98
data_2[50,22] <- 1985

data_2 <- data_2[!(data_2$Name == 'Michael Anderson' & data_2$GP == 65),]
data_2[94, 24] <- 'Drexel'
data_2[94, 23] <- 1.80

data_2 <- data_2[!(data_2$Name == 'Michael Smith' & data_2$GP == 65 & data_2$TARGET_5Yrs == 1),]
data_2 <- data_2[!(data_2$Name == 'Michael Smith' & data_2$GP == 65 & data_2$`Year Drafted` == 1994),]
data_2[610, 24] <- 'BYU'
data_2[610, 23] <- 2.08

data_2 <- data_2[!(data_2$Name == 'Michael Smith' & data_2$GP == 82 & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Michael Smith' & data_2$GP == 82 & data_2$`Year Drafted` == 1989),]

data_2 <- data_2[!(data_2$Name == 'Reggie Williams' & data_2$player_height == 1.98),]
data_2 <- data_2[!(data_2$Name == 'Steven Smith' & data_2$player_height == 2.06),]
data_2 <- data_2[!(data_2$Name == 'Tim Hardaway' & data_2$TARGET_5Yrs == 0),]
data_2 <- data_2[!(data_2$Name == 'Walker Russell' & data_2$TARGET_5Yrs == 0),]


# Removing variables: Name, Year Drafted, REB, FGM, FGA, X3P.Made, X3PA, FTM, FTA.


data_3 <- select(data_2, -Name, -`Year Drafted`, -REB, -FGM, -FGA, -X3P.Made, -X3PA, -FTM, -FTA)


# Mutating the response variable into logic.

data_3$TARGET_5Yrs <- as.logical(data_3$TARGET_5Yrs)
data_3$college <- as.factor(data_3$college)


# The kNN function solve the problem with the NA in the remaining variables.

set.seed(217054060)
dataKNN <- kNN(data_3, variable = c('player_height', 'college'), k =6)
dataNoNA <- subset(dataKNN, select = GP:college)

# Trying to find any variable with the variation near zero or zero.
# No one was found.

nzv_ <- nearZeroVar(dataNoNA, saveMetrics = T)
nzv_

# Creating data partition for the model.

set.seed(217054060)
inTrain <- createDataPartition(dataNoNA$TARGET_5Yrs, p = 0.75, list = F)
training <- dataNoNA[inTrain,]
testing <- dataNoNA[-inTrain,]

# I've created a function to try some hyperparameters.
# The criteria was the AUC.

trainGBM <- function(train_data, test_data){
  results <- as.data.frame(matrix(ncol = 4))
  x <- list('interactionDepth', 'n.trees', 'shrinkage', 'AUC')
  colnames(results) <- x
  intDep <- c(1, 2, 3, 4, 5)
  for(i in intDep){
    nt <- c(25, 50, 75, 100)
    for(n in nt){
      shr <- c(0.05, 0.01, 0.1)
      for(s in shr){
        set.seed(217054060)
        model <- gbm(TARGET_5Yrs ~ ., data = train_data, distribution = 'bernoulli', 
                     n.trees = n, 
                     interaction.depth = i,
                     shrinkage = s,
                     bag.fraction = 1,
                     cv.fold=5,
                     )
        pred <- predict(model, test_data)
        AUC <- gbm.roc.area(test_data$TARGET_5Yrs, pred)
        results <- rbind(results, c(i, n, s, AUC))
        }
    }
  }
  print(results)
}

trainGBM(training, testing)

# Best result:
# n.trees =  50
# interaction.depth =  5
# shrinkage =  0.05
# AUC =  0.8388475

# Fact model.

set.seed(217054060)
modelFit <- gbm(TARGET_5Yrs ~ ., data = training, distribution =  'bernoulli', 
                n.trees = 50, interaction.depth = 5, shrinkage = 0.05,
                bag.fraction = 1, cv.fold=5)


head(summary(modelFit))
gbm.perf(modelFit)

# n.trees = 32

pred <- predict(modelFit, testing, type = 'response', n.trees = 32)
gbm.roc.area(testing$TARGET_5Yrs, pred)

# AUC = 0.8388475

# ROC:
par(pty = "s")
pROC::roc(testing$TARGET_5Yrs, pred, plot=T)
par(pty = "m")

# I've used the optimizer function to estimate better results.

optimizer<-function(predic,response){
  require(ROCR)
  predic2 = prediction(predic, response)
  performance = performance(predic2,"tpr","fpr")
  func <- attr(performance, "y.values")[[1]] -
    (attr(performance, "x.values")[[1]])
  c <-  performance@alpha.values[[1]][which.max(func)]
  plot(performance, colorize=T, lwd=2)
  c
}

set.seed(217054060)
predic_training <- predict(modelFit, training, type = 'response', n.trees = 32)
c <- optimizer(predic_training, training$TARGET_5Yrs)
c  # = 0.6273926

# With c defined I can apply the model to the test sample.

classif <- pred >= c

# Results:

confusionMatrix(data = as.factor(classif), reference = as.factor(testing$TARGET_5Yrs))


