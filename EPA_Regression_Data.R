install.packages("expss")
install.packages("rlang")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggimage")
install.packages("nflfastR")
install.packages("aod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("car")
install.packages("smooth")
install.packages("Mcomp")
install.packages("pracma")
install.packages("caret")
install.packages("rpart")
install.packages("randomForest")
library(randomForest)
library(rpart)
library(caret)
library(pracma)
library(Mcomp)
library(smooth)
library(car)
library(dplyr)
library(aod)
library(ggplot2)
library(rlang)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(expss)
library(zoo)

seasons <- 2010:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

# Create a list of games
games <- pbp %>% 
  select(game_id, home_team, away_team, home_score, away_score, result, spread_line, roof) %>% 
  group_by(game_id, home_team, away_team, away_score, home_score, result, spread_line, roof) %>% 
  summarise(games = n_distinct(game_id)) %>% 
  mutate(home_margin_to_spread = result + spread_line)

rolling_n <- 14 # 14 was determined to be optimal based on cross validation of n between 10 and 20

# Summarize offensive EPA by team
Off_EPA <- pbp %>%
  filter(play_type == 'pass' | play_type == 'run') %>%
  filter(!is.na(epa)) %>%
  group_by(posteam, game_id) %>%
  summarize(Off_EPA = round(sum(epa),2),
            plays = n_distinct(play_id),
            Off_EPA_per_Play = round(sum(epa) / n_distinct(play_id),2)) %>%
  mutate(cum_off_epa = round(cumsum(Off_EPA),2)) %>%
  mutate(cum_plays = cumsum(plays)) %>%
  mutate(cum_EPA_per_play = round(cumsum(Off_EPA) / cumsum(plays),2))%>%
  mutate(roll_off_epa = rollapplyr(Off_EPA,rolling_n,sum,partial=TRUE))%>%
  mutate(roll_plays = rollapplyr(plays,rolling_n,sum,partial=TRUE))%>%
  mutate(roll_EPA_per_play = round(rollapplyr(Off_EPA,rolling_n,sum,partial=TRUE) / rollapplyr(plays,rolling_n,sum,partial=TRUE),2)) %>%
  mutate(row_num = row_number()) %>% 
  mutate(epa_prediction = lag(roll_EPA_per_play, 1))

# Summarize defensive EPA by team
Def_EPA <- pbp %>%
  filter(play_type == 'pass' | play_type == 'run') %>%
  filter(!is.na(epa)) %>%
  group_by(defteam, game_id) %>%
  summarize(Def_EPA = round(sum(epa),2),
            plays = n_distinct(play_id),
            Def_EPA_per_Play = round(sum(epa) / n_distinct(play_id),2)) %>%
  mutate(cum_def_epa = round(cumsum(Def_EPA),2)) %>%
  mutate(cum_plays = cumsum(plays)) %>%
  mutate(cum_EPA_per_play = round(cumsum(Def_EPA) / cumsum(plays),2))%>%
  mutate(roll_def_epa = rollapplyr(Def_EPA,rolling_n,sum,partial=TRUE))%>%
  mutate(roll_plays = rollapplyr(plays,rolling_n,sum,partial=TRUE))%>%
  mutate(roll_EPA_per_play = round(rollapplyr(Def_EPA,rolling_n,sum,partial=TRUE) / rollapplyr(plays,rolling_n,sum,partial=TRUE),2)) %>%
  mutate(row_num = row_number()) %>% 
  mutate(epa_prediction_def = lag(roll_EPA_per_play, 1))


# Join offensive EPA to list of games
EPA_regression_data_off <- left_join(left_join(games, Off_EPA, c('game_id' = 'game_id', 'home_team' = 'posteam')), Off_EPA, c('game_id' = 'game_id', 'away_team' = 'posteam')) %>% 
  filter(!is.na(epa_prediction.x) & !is.na(epa_prediction.y)) %>% 
  select(game_id, home_team, away_team, away_score, home_score, result, spread_line, roof, epa_prediction.x, epa_prediction.y, home_margin_to_spread) %>% 
  rename(epa_prediction_off_home = epa_prediction.x) %>% 
  rename(epa_prediction_off_away = epa_prediction.y) %>% 
  mutate(roof = ifelse(roof == 'closed', 'dome', ifelse(roof == 'open','outdoors',roof))) %>% 
  mutate(environment = ifelse(roof == 'outdoors',0,1))

# Join defensive EPA to list of games
EPA_regression_data_def <- left_join(left_join(games, Def_EPA, c('game_id' = 'game_id', 'home_team' = 'defteam')), Def_EPA, c('game_id' = 'game_id', 'away_team' = 'defteam')) %>% 
  filter(!is.na(epa_prediction_def.x) & !is.na(epa_prediction_def.y)) %>% 
  select(game_id, home_team, away_team, away_score, home_score, result, spread_line, roof, epa_prediction_def.x, epa_prediction_def.y, home_margin_to_spread) %>% 
  rename(epa_prediction_def_home = epa_prediction_def.x) %>% 
  rename(epa_prediction_def_away = epa_prediction_def.y) %>% 
  mutate(roof = ifelse(roof == 'closed', 'dome', ifelse(roof == 'open','outdoors',roof))) %>% 
  mutate(environment = ifelse(roof == 'outdoors',0,1))

# Join offensive EPA to defensive EPA
EPA_regression_data <- left_join(EPA_regression_data_off, EPA_regression_data_def, c('game_id' = 'game_id')) %>% 
  select(game_id, home_team.x, away_team.x, away_score.x, home_score.x, result.x, spread_line.x, epa_prediction_off_home, epa_prediction_off_away, epa_prediction_def_home, epa_prediction_def_away, home_margin_to_spread.x) %>% 
  rename_with(~ tolower(gsub(".x", "", .x, fixed = TRUE)))

# Create scatter plots to evaluate correlation
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r <- round(cor(x,y),2)
  txt <- paste0("R = ", r)
  text(0.5, 0.5, txt)
}

upper.panel <- function(x,y){
  points(x,y, pch = 19)
}

pairs(EPA_regression_data[,9:12],
      lower.panel = panel.cor,
      upper.panel = upper.panel)

# Linear Regression
lm <- lm(result ~ epa_prediction_off_home + epa_prediction_off_away
         + epa_prediction_def_home + epa_prediction_def_away
         , data = EPA_regression_data)
summary(lm)

b0 <- summary(lm)$coef[1,1]
b_home_off_epa <- summary(lm)$coef[2,1]
b_away_off_epa <- summary(lm)$coef[3,1]
b_home_def_epa <- summary(lm)$coef[4,1]
b_away_def_epa <- summary(lm)$coef[5,1]

residualPlot(lm)

# Validation Set (http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#k-fold-cross-validation)
set.seed(123)
training.samples <- EPA_regression_data$result %>% 
  createDataPartition(p = .5, list = FALSE)
train.data <- EPA_regression_data[training.samples,]
test.data <- EPA_regression_data[-training.samples, ]
model_vs <- lm(result ~ epa_prediction_off_home + epa_prediction_off_away
               + epa_prediction_def_home + epa_prediction_def_away
            , data = train.data)
predictions <- model_vs %>% predict(test.data)
data.frame( test_RMSE = RMSE(predictions, test.data$result),
            test_R2 = R2(predictions, test.data$result),
            test_MAE = MAE(predictions, test.data$result))

# LOOCV (http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#k-fold-cross-validation)
train.control <- trainControl(method = "LOOCV")
model_loocv <- train(result ~ epa_prediction_off_home + epa_prediction_off_away
                     + epa_prediction_def_home + epa_prediction_def_away
                     , data = EPA_regression_data, method = "lm", trControl = train.control)
print(model_loocv)

# K-Fold (http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#k-fold-cross-validation)
set.seed(123)
train.control <- trainControl(method = "cv", number = 5)
model_kf <- train(result ~ epa_prediction_off_home + epa_prediction_off_away
                  + epa_prediction_def_home + epa_prediction_def_away
                  , data = EPA_regression_data, method = "lm", trControl = train.control)
print(model_kf)

######### Results from cross validation: include offensive EPA, include defensive EPA, exclude environment (negligible effect, and non-significant)


# Regression Tree
model_class <- rpart(result ~ epa_prediction_off_home + epa_prediction_off_away
                     + epa_prediction_def_home + epa_prediction_def_away,
                     method = "anova", 
                     data = EPA_regression_data)

printcp(model_class) # display the results
plotcp(model_class) # visualize cross-validation results
summary(model_class) # detailed summary of splits
par(mfrow=c(1,1)) # two plots on one page
rsq.rpart(model_class) # visualize cross-validation results  

plot(model_class, uniform=TRUE,
     main="Regression Tree for Margin of Victory")
text(model_class, use.n=TRUE, all=TRUE, cex=.7)

# Prune Tree
pfit<- prune(model_class, cp=0.011) # Min error from cptable   

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=FALSE, cex=.8)


# Random Forest
set.seed(101)
rf_train <- sample(1:nrow(EPA_regression_data), 500)
model_rf <- randomForest(result ~ epa_prediction_off_home + epa_prediction_off_away
                        + epa_prediction_def_home + epa_prediction_def_away
                        , data = EPA_regression_data
                        , subset = rf_train, mtry = 2)
print(model_rf)

EPA_regression_data$rf_pred <- round(predict(model_rf, EPA_regression_data[]), digits = 1)

importance(model_rf)
oob.err = double(3)
test.err = double(3)
for(mtry in 1:3){
  fit = randomForest(result ~ epa_prediction_off_home + epa_prediction_off_away
                     + epa_prediction_def_home + epa_prediction_def_away,
                     data = EPA_regression_data, subset= rf_train, mtry=mtry, ntree = 350)
  oob.err[mtry] = fit$mse[350]
  pred = predict(fit, EPA_regression_data[-rf_train,])
  test.err[mtry] = with(EPA_regression_data[-rf_train,], mean( (result-pred)^2 ))
}
matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))