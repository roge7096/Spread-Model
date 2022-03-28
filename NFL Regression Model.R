# NFL Point Spread Regression

# Load Packages
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")
library(tidyverse)
library(caret)
library(leaps)

# Import Data
NFL_Regression_Data <- read_excel("Data/NFL/NFL Regression Model.xlsx", sheet = "Input Data")
View(NFL_Regression_Data)

Model_Data <- NFL_Regression_Data[c(13:33,35)]

# Compute the best subset regressions
models <- regsubsets(Result ~ ., data = Model_Data, nvmax = 21)
summary(models)

# Model Evaluation
res.sum <- summary(models)
  data.frame(
    Adj.R2 = which.max(res.sum$adjr2),
    CP = which.min(res.sum$cp),
    BIC = which.min(res.sum$bic)
  )
  
Model_5 <- lm(Result ~ H_QB_EPA_Career + H_PRSH + A_QB_EPA_Career + A_PBLK + A_COV, data = Model_Data)
Model_4 <- lm(Result ~ H_QB_EPA_Career + H_PRSH + A_QB_EPA_Career + A_COV, data = Model_Data)
Model_3 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_COV, data = Model_Data)
summary(Model_5)
summary(Model_4)
summary(Model_3)

# Create scatter plots
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r <- round(cor(x,y),2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

upper.panel <- function(x,y){
  points(x,y, pch = 19)
}

pairs(NFL_Regression_Data[,12:35],
      lower.panel = panel.cor,
      upper.panel = upper.panel)

# Regression Predictor Selection: Stepwise Forward Selection
## Response variable is column 13, predictor variables begin in column 14

p1.1 <- lm(Result ~ H_QB_EPA_Career, data = NFL_Regression_Data) #Adj R^2: .10
summary(p1.1)
p1.2 <- lm(Result ~ H_PBLK, data = NFL_Regression_Data) #Adj R^2: .02
summary(p1.2)
p1.3 <- lm(Result ~ H_RECV, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.3)
p1.4 <- lm(Result ~ H_RUN, data = NFL_Regression_Data) #Adj R^2: .02
summary(p1.4)
p1.5 <- lm(Result ~ H_RDEF, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.5)
p1.6 <- lm(Result ~ H_PRSH, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.6)
p1.7 <- lm(Result ~ H_COV, data = NFL_Regression_Data) #Adj R^2: .01
summary(p1.7)
p1.8 <- lm(Result ~ H_Special_z, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.8)
p1.9 <- lm(Result ~ H_SoS_Adjustment, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.9)
p1.10 <- lm(Result ~ H_First_Down_Adjustment, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.10)
p1.11 <- lm(Result ~ A_QB_EPA_Career, data = NFL_Regression_Data) #Adj R^2: .05
summary(p1.11)
p1.12 <- lm(Result ~ A_PBLK, data = NFL_Regression_Data) #Adj R^2: .04
summary(p1.12)
p1.13 <- lm(Result ~ A_RECV, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.13)
p1.14 <- lm(Result ~ A_RUN, data = NFL_Regression_Data) #Adj R^2: .01
summary(p1.14)
p1.15 <- lm(Result ~ A_RDEF, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.15)
p1.16 <- lm(Result ~ A_PRSH, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.16)
p1.17 <- lm(Result ~ A_COV, data = NFL_Regression_Data) #Adj R^2: .03
summary(p1.17)
p1.18 <- lm(Result ~ A_Special_z, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.18)
p1.19 <- lm(Result ~ A_SoS_Adjustment, data = NFL_Regression_Data) #Adj R^2: .02
summary(p1.19)
p1.20 <- lm(Result ~ A_First_Down_Adjustment, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.20)
p1.21 <- lm(Result ~ Wind, data = NFL_Regression_Data) #Adj R^2: .00
summary(p1.21)

# Second Predictor Selection
p2.1.2 <- lm(Result ~ H_QB_EPA_Career + H_PBLK, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.2)
p2.1.3 <- lm(Result ~ H_QB_EPA_Career + H_RECV, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.3)
p2.1.4 <- lm(Result ~ H_QB_EPA_Career + H_RUN, data = NFL_Regression_Data) #Adj R^2: .10
summary(p2.1.4)
p2.1.5 <- lm(Result ~ H_QB_EPA_Career + H_RDEF, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.5)
p2.1.6 <- lm(Result ~ H_QB_EPA_Career + H_PRSH, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.6)
p2.1.7 <- lm(Result ~ H_QB_EPA_Career + H_COV, data = NFL_Regression_Data) #Adj R^2: .10
summary(p2.1.7)
p2.1.8 <- lm(Result ~ H_QB_EPA_Career + H_Special_z, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.8)
p2.1.9 <- lm(Result ~ H_QB_EPA_Career + H_SoS_Adjustment, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.9)
p2.1.10 <- lm(Result ~ H_QB_EPA_Career + H_First_Down_Adjustment, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.10)
p2.1.11 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career, data = NFL_Regression_Data) #Adj R^2: .15
summary(p2.1.11)
p2.1.12 <- lm(Result ~ H_QB_EPA_Career + A_PBLK, data = NFL_Regression_Data) #Adj R^2: .12
summary(p2.1.12)
p2.1.13 <- lm(Result ~ H_QB_EPA_Career + A_RECV, data = NFL_Regression_Data) #Adj R^2: .10
summary(p2.1.13)
p2.1.14 <- lm(Result ~ H_QB_EPA_Career + A_RUN, data = NFL_Regression_Data) #Adj R^2: .11
summary(p2.1.14)
p2.1.15 <- lm(Result ~ H_QB_EPA_Career + A_RDEF, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.15)
p2.1.16 <- lm(Result ~ H_QB_EPA_Career + A_PRSH, data = NFL_Regression_Data) #Adj R^2: .10
summary(p2.1.16)
p2.1.17 <- lm(Result ~ H_QB_EPA_Career + A_COV, data = NFL_Regression_Data) #Adj R^2: .15
summary(p2.1.17)
p2.1.18 <- lm(Result ~ H_QB_EPA_Career + A_Special_z, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.18)
p2.1.19 <- lm(Result ~ H_QB_EPA_Career + A_SoS_Adjustment, data = NFL_Regression_Data) #Adj R^2: .11
summary(p2.1.19)
p2.1.20 <- lm(Result ~ H_QB_EPA_Career + A_First_Down_Adjustment, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.20)
p2.1.21 <- lm(Result ~ H_QB_EPA_Career + Wind, data = NFL_Regression_Data) #Adj R^2: .09
summary(p2.1.21)

# Third Predictor Selection
p3.1.11.2 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_PBLK, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.2)
p3.1.11.3 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_RECV, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.3)
p3.1.11.4 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_RUN, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.4)
p3.1.11.5 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_RDEF, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.5)
p3.1.11.6 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_PRSH, data = NFL_Regression_Data) #Adj R^2: .16
summary(p3.1.11.6)
p3.1.11.7 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_COV, data = NFL_Regression_Data) #Adj R^2: .16
summary(p3.1.11.7)
p3.1.11.8 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_Special_z, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.8)
p3.1.11.9 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_SoS_Adjustment, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.9)
p3.1.11.10 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + H_First_Down_Adjustment, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.10)
p3.1.11.12 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_PBLK, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.12)
p3.1.11.13 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_RECV, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.13)
p3.1.11.14 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_RUN, data = NFL_Regression_Data) #Adj R^2: .16
summary(p3.1.11.14)
p3.1.11.15 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_RDEF, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.15)
p3.1.11.16 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_PRSH, data = NFL_Regression_Data) #Adj R^2: .16
summary(p3.1.11.16)
p3.1.11.17 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_COV, data = NFL_Regression_Data) #Adj R^2: .18
summary(p3.1.11.17)
p3.1.11.18 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_Special_z, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.18)
p3.1.11.19 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_SoS_Adjustment, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.19)
p3.1.11.20 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + A_First_Down_Adjustment, data = NFL_Regression_Data) #Adj R^2: .16
summary(p3.1.11.20)
p3.1.11.21 <- lm(Result ~ H_QB_EPA_Career + A_QB_EPA_Career + Wind, data = NFL_Regression_Data) #Adj R^2: .15
summary(p3.1.11.21)