rm(list = ls())
library(data.table)
library(ggplot2)
library(corrplot)
library(caret)
library(randomForest)
library(clusterGeneration)
library(mnormt)
library(randomForest)
library(zoo)
library(plyr)


work_dir <- 'C:/Users/IBM_ADMIN/Desktop/Box Sync/_Nilesh_Files/_Projects/ag_Cog_Testing_Pred'
setwd(work_dir)
set.seed(1234)

# Functions: General -----------------------------------------------------------
R2 <- function(y, yhat){
  1 - (sum((y-yhat )^2)/sum((y-mean(y))^2))
}

MAE <- function(y, yhat){
  mean(abs((y - yhat)))
}

MAPE <- function(y, yhat){
  mean(abs((y - yhat)/y))
}

RMSE <- function(y, yhat){
  sqrt(mean((y - yhat )^2))
}

PrintResult <- function(model, dat_model, 
                        .idx_train = idx_train, .idx_valid = idx_valid,
                        .fs_target = fs_target, .fs_ind = fs_ind, 
                        is_caret = FALSE){
  y_true <- unlist(dat_model[.idx_train, .fs_target, with = FALSE])
  if(is_caret){
    y_pred <- predict(model, dat_model[.idx_train, .fs_ind, with = FALSE], type="raw")
  }else{
    y_pred <- predict(model, dat_model[.idx_train, .fs_ind, with = FALSE], type="response")
  }
  y_pred <- as.numeric(as.character(y_pred))
  
  print.noquote(paste('Train set:', 'R2:', round(R2(y_true, y_pred), 4), '  ',
                                    'MAE:', round(MAE(y_true, y_pred), 4),'  ',
                                    'RMSE:', round(RMSE(y_true, y_pred), 4),'  ',
                                    'MAPE:', round(MAPE(y_true, y_pred), 4),'  '))
  
  y_true <- unlist(dat_model[.idx_valid, .fs_target, with = FALSE])
  if(is_caret){
    y_pred <- predict(model, dat_model[.idx_valid, .fs_ind, with = FALSE], type="raw")
  }else{
    y_pred <- predict(model, dat_model[.idx_valid, .fs_ind, with = FALSE], type="response")
  }
  y_pred <- as.numeric(as.character(y_pred))
  
  print.noquote(paste('Valid set:', 'R2:', round(R2(y_true, y_pred), 4), '  ',
                                    'MAE:', round(MAE(y_true, y_pred), 4),'  ',
                                    'RMSE:', round(RMSE(y_true, y_pred), 4),'  ',
                                    'MAPE:', round(MAPE(y_true, y_pred), 4),'  '))
  print(qplot(y_true, y_pred))
  NULL
}


# Functions: Load Data ---------------------------------------------------------
LoadData <- function(){
  dat <- fread('c_input_data/wkly_data.csv')
  c("V1", "ProjCMMI", "Release", "Project", "phase", "app_name", 
  "app_category", "app_complexity", "phase_app_duration", "test_case", 
  "dev_team_size", "test_team_size", "Year", "WEEK", "defect_count", 
  "combin1", "combin2", "week_number")
  
  
  # Rename field names
  setnames(dat, 'app_phase_wklyDef', 'defect_count')
  setnames(dat, 'phase_app_duration', 'app_phase_duration')
  setnames(dat, 'test_case', 'test_case_count')
  
  dat[, app_complexity := as.numeric(app_complexity)]
  dat[, app_phase_duration := as.numeric(app_phase_duration)]
  dat[, test_case_count := as.numeric(test_case_count)]
  dat[, dev_team_size := as.numeric(dev_team_size)]
  dat[, test_team_size := as.numeric(test_team_size)]

  dat[, ProjCMMI := as.factor(ProjCMMI)]
  dat[, phase := as.factor(phase)]
  dat[, week_number := as.factor(week_number)]

  sapply(dat, class)
  dat
}


# Functions: Preprocess Data and Feature Enginering ---------------------------

GetProjAppPhaseData <- function(dat){
  ## Prepare Data
  # "ProjCMMI" dropped as it is 3 for all apps
  fs_all <- c('Project', 'Release', 'app_name', "phase", "app_complexity",
              "app_phase_duration",  "test_case_count",
              "dev_team_size", "test_team_size",  "week_number")
  fs_target <- c('defect_count')
  dat <- dat[,c(fs_all,  fs_target), with= FALSE]
  
  dat <- na.omit(dat)
 
  dat_app_phase <- dat[, list(defect_count = sum(defect_count),
                              app_complexity = mean(app_complexity),
                              app_phase_duration = mean(app_phase_duration),
                              test_case_count = mean(test_case_count),
                              dev_team_size = mean(dev_team_size),
                              test_team_size = mean(test_team_size)
                              ),
                       by = c('Project', 'Release', 'app_name', 'phase')]
  
  dat_app_phase <- dat_app_phase[,c("defect_count", "phase", "app_complexity", 
                                    "app_phase_duration", "test_case_count", 
                                    "dev_team_size", "test_team_size"), 
                                 with = FALSE]
  qplot(defect_count, data = dat_app_phase)
  # outlier detection: remove obs with defect_count more than 200
  sum(dat_app_phase$defect_count >= 200) # 4

  # x <- dat_app_phase$defect_count
  # qnt <- quantile(x, probs=c(.25, .75))
  # H <- 1.5 * IQR(x)
  # # number of outliers: value > 122
  # sum((qnt[1] - H) > x |  x > (qnt[2] + H)) # 4
  # idx <- (qnt[1] - H) > x |  x > (qnt[2] + H)
  # print(dat_app_phase$defect_count[idx])
  # dat_app_phase <- dat_app_phase[!idx,] 
  
  dat_app_phase <- dat_app_phase[dat_app_phase$defect_count < 200,]
  dat_app_phase
}

FeatureEng_ProjAppPhaseData <- function(dat_ProjAppPhase){
  # dat_ProjAppPhase[, test_efforts := app_phase_duration * test_team_size]
  # dat_ProjAppPhase[, dev_efforts := app_phase_duration * dev_team_size]
  dat_ProjAppPhase
}

GetProjAppPhaseWeekData <- function(dat){
  ## Prepare Data
  # "ProjCMMI" dropped as it is 3 for all apps
  fs_all <- c('Project', 'Release', 'app_name', 'phase', "week_number",
              "app_complexity", "app_phase_duration",  "test_case_count",
              "dev_team_size", "test_team_size"  )
  fs_target <- c('defect_count')
  dat_ProjAppPhaseWeek <- dat[,c(fs_all,  fs_target), with= FALSE]
  setorder(dat, 'Project', 'Release', 'app_name', "phase", "week_number")
  dat <- na.omit(dat)
 
  dat_ProjAppPhaseWeek <- 
    dat_ProjAppPhaseWeek[,c('Project', 'Release', 'app_name',  "phase", 'week_number',
                            "defect_count",
                            "app_complexity", "app_phase_duration", "test_case_count", 
                            "dev_team_size", "test_team_size"), 
                            with = FALSE]
  qplot(defect_count, data = dat_ProjAppPhaseWeek)

  # outlier detection: remove obs with defect_count more than 200
  sum(dat_ProjAppPhaseWeek$defect_count >= 40) # 13
  dat_ProjAppPhaseWeek <- dat_ProjAppPhaseWeek[dat_ProjAppPhaseWeek$defect_count < 40,]
  
  dat_ProjAppPhaseWeek
}

FeatureEng_ProjAppPhaseWeekData <- function(dat_ProjAppPhaseWeek){
  # dat_ProjAppPhaseWeek[, test_efforts := app_phase_duration * test_team_size]
  # dat_ProjAppPhaseWeek[, dev_efforts := app_phase_duration * dev_team_size]
  dat_ProjAppPhaseWeek
}

GetProjAppPhaseWeekMaxData <- function(dat){
  fs_all <- c('Project', 'Release', 'app_name', "phase", "app_complexity",
              "app_phase_duration",  "test_case_count",
              "dev_team_size", "test_team_size",  "week_number")
  fs_target <- c('defect_count')
  dat <- dat[,c(fs_all,  fs_target), with= FALSE]
  dat_ProjAppPhaseWeekMax <- dat[, list(week_number_max = max(week_number),
                              defect_count = sum(defect_count),
                              app_complexity = mean(app_complexity),
                              app_phase_duration = mean(app_phase_duration),
                              test_case_count = mean(test_case_count),
                              dev_team_size = mean(dev_team_size),
                              test_team_size = mean(test_team_size)
                              ),
                       by = c('Project', 'Release', 'app_name', 'phase')]
  dat_ProjAppPhaseWeekMax <- 
    dat_ProjAppPhaseWeekMax[,c('week_number_max', "defect_count", "phase",
                               "app_complexity", "app_phase_duration", "test_case_count", 
                                    "dev_team_size", "test_team_size"), 
                                 with = FALSE]
  dt <- dat_ProjAppPhaseWeekMax
  # outlier detection: remove obs with defect_count more than 200
  sum(dt$defect_count > 100) # 15
  dt <- dt[dt$defect_count <= 100,]
  dt[, week_number_max := as.numeric(as.character(week_number_max))]
  sum(dt$week_number_max > 10) # 13
  dt <- dt[dt$week_number_max <= 10,]
  qplot(dt$defect_count)
  qplot(dt$week_number_max)
  
  dat_ProjAppPhaseWeekMax <- dt
  dat_ProjAppPhaseWeekMax
}

FeatureEng_GetProjAppPhaseWeekMaxData <- function(dat_ProjAppPhaseWeekMax){
  dat_ProjAppPhaseWeekMax[, test_cases_per_tester := test_case_count / test_team_size]
}


# Functions: Explore Data ------------------------------------------------------
ExploreData <- function(dat){
  qplot(x = defect_count, data = dat)
  qplot(x = week_number, y = defect_count, data = dat)
  qplot(x = week_number, y = defect_count, color = phase, data = dat)
  
  
  qplot(x = app_complexity, y = sum(defect_count), data = dat)
  dat[, mean(defect_count), by = app_complexity]
  dat[, sum(defect_count), by = app_complexity]
  
  
  ggplot(dat[, list(defect_count_mean = mean(defect_count)), by = phase],
         aes(x = phase, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  
  ggplot(dat[, list(defect_count_mean = mean(defect_count)), by = app_complexity],
         aes(x = factor(app_complexity), y = defect_count_mean)) +
  geom_bar(stat = "identity")
  
  ggplot(dat[, list(defect_count_mean = mean(defect_count)), by = test_team_size],
         aes(x = test_team_size, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dat[, list(defect_count_mean = mean(defect_count)), by = dev_team_size],
         aes(x = dev_team_size, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dat[, list(defect_count_mean = mean(defect_count)), by = test_case_count],
         aes(x = test_case_count, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dat[, list(defect_count_mean = mean(defect_count)), by = app_phase_duration],
         aes(x = app_phase_duration, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  
  # WIP
  set.seed(1)
  n=500
  S=genPositiveDefMat("eigen",dim=15)
  S=genPositiveDefMat("unifcorrmat",dim=15)
  X=rmnorm(n,varcov=S$Sigma)
  corrplot(cor(X), order = "hclust")
  
}

Explore_ProjAppPhaseWeekData <- function(dat_ProjAppPhaseWeek){
  dt <- dat_ProjAppPhaseWeek
  
  qplot(x = defect_count, data = dt)
  qplot(x = week_number, y = defect_count, data = dt)
  qplot(x = week_number, y = defect_count, color = phase, data = dt)
  
  for(ph in unique(dt$phase)){
    p <- qplot(x = week_number, y = defect_count, color = app_name, group = app_name,
          data = dt[dt$phase == ph, ], geom = 'line', main = paste('Phase:', ph))
    print(p)
  }
  
  dt[, mean(defect_count), by = app_complexity]
  
  ##### Visualise for an proj - release- app -phase combination
  dat_sub <- dt[dt$Project =='CONSOLIDATED DEFECTS DUMP Q2' & 
                   dt$Release == 'DRM_METLIFE_2014-Q2' &
                   dt$app_name == "TAM - 7351", ]
  ggplot(dat_sub, aes(x = week_number, y = defect_count,
                      fill=phase)) + geom_bar(stat = "identity")
  ggplot(dat_sub, aes(x = week_number, y = defect_count,
                      fill=phase)) + geom_bar(stat = "identity", position = 'dodge')
  
  ####### distribution of defects accross weeks normalised by total defect count
  dt[, week_number_max := max(as.numeric(as.character(week_number))),
                       by = c('Project', 'Release', 'app_name', 'phase')]
  dt[, defect_count_sum_by_app_phase := sum(defect_count),
      by = c('Project', 'Release', 'app_name', 'phase')]
  dt[, defect_count_norm:= defect_count / defect_count_sum_by_app_phase]
  qplot(defect_count_norm, data = dt)
  
  dat_sub <- dt[dt$phase == "5.5-UAT" & 
                  dt$week_number_max > 5  & dt$week_number_max < 10 , ]
  ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=app_name, color = app_name)) + geom_line()
  # WIP require normalisation by week
  dt[, defect_count_cumsum:= cumsum(defect_count), 
        by = c('Project', 'Release', 'app_name', 'phase')]
  dt[, defect_count_norm := defect_count_cumsum / defect_count_sum_by_app_phase, 
      by = c('Project', 'Release', 'app_name', 'phase')]
  qplot(defect_count_norm, data = dt)
  
  dat_sub <- dt[dt$phase == "5.5-UAT" & 
                  dt$week_number_max > 5  & dt$week_number_max < 10 , ]
  ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=app_name, color = app_name)) + geom_line()
  ggplot(dt[dt$phase == "5.2-FUNCTIONAL" & 
               dt$week_number_max > 5  & 
                 dt$week_number_max < 15,], aes(x = week_number, y = defect_count_norm, 
                      group=app_name, color = app_name)) + geom_line()
  
  
  ###### how long testing will last for app - phase combination
  dat_WeekMax <- dt[, list(week_number_max = max(week_number)), 
                       by = c('Project', 'Release', 'app_name', 'phase')]
  qplot(week_number_max, data = dat_WeekMax)
  ggplot(dat_WeekMax, aes(x = week_number_max,  
                      fill=phase)) + geom_bar()
  ggplot(dat_WeekMax[, list(count = .N), by = c('phase', 'week_number_max')], 
         aes(week_number_max, phase)) + geom_tile(aes(fill = count))
  
  
  #### Detect importance of veriables visualy  
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), 
             by = c('phase','week_number')],
         aes(x = week_number, y = defect_count_mean), color = phase) +
  geom_bar(stat = "identity")
  
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), by = phase],
         aes(x = phase, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), by = app_complexity],
         aes(x = factor(app_complexity), y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), by = test_team_size],
         aes(x = test_team_size, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), by = dev_team_size],
         aes(x = dev_team_size, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), by = test_case_count],
         aes(x = test_case_count, y = defect_count_mean)) +
  geom_bar(stat = "identity")
  ggplot(dt[, list(defect_count_mean = mean(defect_count)), by = app_phase_duration],
         aes(x = app_phase_duration, y = defect_count_mean)) +
  geom_bar(stat = "identity")
}



# Functions: Predictive Model --------------------------------------------------
BuildGLMModel <- function(dat){
  # "ProjCMMI" dropped as it is 3 for all apps
  fs_ind <- c( "app_complexity", "app_phase_duration", 
              "phase", "test_case_count", "dev_team_size", "test_team_size",  
              "week_number")
  fs_target <- c('defect_count')
  dat <- dat[,c(fs_ind,  fs_target), with= FALSE]
  dat <- na.omit(dat)
  
  
  model <- glm(defect_count ~ . , data = dat, family = poisson)
  summary(model)
  
  y_true <- dat$defect_count
  y_pred <- predict(model, dat, type="response")
  MAE(y_true, y_pred) # 4.894344 defects
  MAPE(y_true, y_pred) # 1.720789
  RMSE(y_true, y_pred) # 7.848099
  R2(y_true, y_pred) #0.2616332
  
  qplot(y_true, y_pred)
  sum(y_true)
  sum(y_pred)
  
}

AppLevelData_GLM_Pois_Model <- function(dat){
  # "ProjCMMI" dropped as it is 3 for all apps
  fs_ind <- c('app_name', "app_complexity", 
              "app_phase_duration", "phase", "test_case_count", 
              "dev_team_size", "test_team_size",  "week_number") 
  fs_target <- c('defect_count')
  dat <- dat[,c(fs_ind,  fs_target), with= FALSE]

  dat <- na.omit(dat)
  
  dat_app_phase <- dat[, list(defect_count = sum(defect_count),
                              app_complexity = mean(app_complexity),
                              app_phase_duration = mean(app_phase_duration),
                              test_case_count = mean(test_case_count),
                              dev_team_size = mean(dev_team_size),
                              test_team_size = mean(test_team_size)
                              ), 
                       by = c('app_name', 'phase')]
  
  dat_app_phase <- dat_app_phase[,c("defect_count", "phase", "app_complexity", 
                                    "app_phase_duration", "test_case_count", 
                                    "dev_team_size", "test_team_size"), 
                                 with = FALSE]
  
  model <- glm(defect_count ~ . , data = dat_app_phase, family = poisson)
  summary(model)
  
  y_true <- dat_app_phase$defect_count
  y_pred <- predict(model, dat_app_phase, type="response")
  MAE(y_true, y_pred)
  MAPE(y_true, y_pred) 
  RMSE(y_true, y_pred) 
  R2(y_true, y_pred)
  
  # > MAE(y_true, y_pred) 
  # [1] 32.51216
  # >   MAPE(y_true, y_pred) 
  # [1] 4.912405
  # >   RMSE(y_true, y_pred) 
  # [1] 52.38678
  # >   R2(y_true, y_pred)
  # [1] 0.4970688

  qplot(defect_count, data = dat_app_phase)
  qplot(log(defect_count), data = dat_app_phase)
  model <- glm(log(defect_count) ~ . , data = dat_app_phase)
  summary(model)
  
  y_true <- dat_app_phase$defect_count
  y_pred <- exp(predict(model, dat_app_phase, type="response"))
  MAE(y_true, y_pred) 
  MAPE(y_true, y_pred) 
  RMSE(y_true, y_pred) 
  R2(y_true, y_pred)
  
  qplot(y_true, y_pred)
  sum(y_true)
  sum(y_pred)
  
  # > MAE(y_true, y_pred)
  # [1] 31.88724
  # > MAPE(y_true, y_pred)
  # [1] 1.811671
  # > RMSE(y_true, y_pred)
  # [1] 63.77663
  # > R2(y_true, y_pred)
  # [1] 0.2546019
    
}

ProjAppPhaseData_GLM_RF_Model <- function(dat_model){
  idx_train <- sample(seq(1, nrow(dat_model)), 0.75 * nrow(dat_model))
  idx_valid <- setdiff(seq(1, nrow(dat_model)), idx_train)
  fs_ind <- c("phase", "app_complexity", "app_phase_duration",  "test_case_count",
              "dev_team_size", "test_team_size")
  fs_target <- c('defect_count')
  
  if(FALSE){  # do not run all models
  ##### GLM Model #####-
  model <- glm(defect_count ~ . , data = dat_model[idx_train, c(fs_ind, fs_target),
                                                   with = FALSE], family = poisson)
  print(summary(model))
  PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)
  # Train set: R2: 0.4866    MAE: 19.5121    RMSE: 28.1121    MAPE: 3.4561   
  # Valid set: R2: 0.5135    MAE: 22.8085    RMSE: 28.9979    MAPE: 4.0314  
  }
  ##### RF Model #####-
  model <- randomForest(defect_count ~ . , ntree = 200, data = dat_model[idx_train,
                                                            c(fs_ind, fs_target),
                                                            with = FALSE])
  summary(model)
  importance(model)
  varImpPlot(model)
  PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)
  # Train set: R2: 0.785    MAE: 13.7742    RMSE: 18.1933    MAPE: 2.6936   
  # Valid set: R2: 0.599    MAE: 20.4357    RMSE: 26.329     MAPE: 3.8022 

  model
}

Score_ProjAppPhaseData_GLM_RF_Model <- function(model, phase,
                                                app_complexity,
                                                app_phase_duration,
                                                test_case_count,
                                                dev_team_size,
                                                test_team_size, .dat = dat){
  dat_test <- GetProjAppPhaseData(.dat)
  i <- 1
  dat_test$phase[i] <- phase
  dat_test$app_complexity[i] <- app_complexity
  dat_test$app_phase_duration[i] <- app_phase_duration
  dat_test$test_case_count[i] <- test_case_count
  dat_test$dev_team_size[i] <- dev_team_size
  dat_test$test_team_size[i] <- test_team_size
  dat_test <- FeatureEng_ProjAppPhaseData(dat_test)
  print.noquote(paste('Predicted Defects:', predict(model, dat_test[i], type = 'response')))
}

ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model <- function(dat_model){
  idx_train <- sample(seq(1, nrow(dat_model)), 0.75 * nrow(dat_model))
  idx_valid <- setdiff(seq(1, nrow(dat_model)), idx_train)
  fs_ind <- c("phase",   "test_case_count",
               "test_team_size" , 'test_cases_per_tester', "app_complexity",  
              "dev_team_size", 'defect_count_pred', "app_phase_duration") 
  fs_target <- c('week_number_max')
  dat_model[, week_number_max := as.numeric(as.character(week_number_max))]
  # qplot(week_number_max , data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE])
  
  if(FALSE){ # do not run all models
    ##### GLM Model #####-
    model <- glm(week_number_max ~ . , data = dat_model[idx_train, c(fs_ind, fs_target),
                                                     with = FALSE], family = poisson)
    print(summary(model))
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)
    # Train set: R2: 0.3562    MAE: 1.6705    RMSE: 2.1783    MAPE: 0.6362   
    # Valid set: R2: -0.1724    MAE: 2.0756    RMSE: 2.7939    MAPE: 0.5556   
    
    ##### RF Model #####-
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    model <- train(week_number_max ~ ., 
                   data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE], 
                   method = "rf", 
                   trControl = fitControl,
                   verbose = FALSE)
    model
    print(summary(model))
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind, is_caret = TRUE)
    # Train set: R2: 0.6706    MAE: 1.2424    RMSE: 1.558    MAPE: 0.5128   
    # Valid set: R2: 0.0563    MAE: 1.7848    RMSE: 2.5067    MAPE: 0.48     
  
    model <- randomForest(week_number_max ~ . , 
                          data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE])
    summary(model)
    importance(model)
    varImpPlot(model)
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)
    # Train set: R2: 0.7643    MAE: 1.0545    RMSE: 1.318    MAPE: 0.4282   
    # Valid set: R2: 0.0688    MAE: 1.7015    RMSE: 2.49    MAPE: 0.4507  
  }
  ##### GBM Model #####-
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  model <- train(week_number_max ~ .,
                 data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE], 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
  print(summary(model))
  print(plot(varImp(model)))
  PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind, is_caret = TRUE)
  # Train set: R2: 0.3421    MAE: 1.7625    RMSE: 2.202    MAPE: 0.7073   
  # Valid set: R2: 0.1363    MAE: 1.6966    RMSE: 2.398    MAPE: 0.422
  model
}

Score_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model <- function(model_DefectCount, 
                                                           model_WeekMax,
                                                           dat,
                                                           phase,
                                                           app_complexity,
                                                           app_phase_duration,
                                                           test_case_count,
                                                           dev_team_size,
                                                           test_team_size){
  
  dat_test <- GetProjAppPhaseWeekMaxData(dat)
  i <- 1
  dat_test$phase[i] <- phase
  dat_test$app_complexity[i] <- app_complexity
  dat_test$app_phase_duration[i] <- app_phase_duration
  dat_test$test_case_count[i] <- test_case_count
  dat_test$dev_team_size[i] <- dev_team_size
  dat_test$test_team_size[i] <- test_team_size
  dat_test <- FeatureEng_ProjAppPhaseData(dat_test)
  print.noquote(paste('Predicted Defects:', predict(model_DefectCount, dat_test[i], type = 'response')))
  dat_test$defect_count_pred <- predict(model_DefectCount, dat_test, type = 'response')
  
  dat_test <- FeatureEng_GetProjAppPhaseWeekMaxData(dat_test)
  print.noquote(paste('Number of Weeks Testing will Continue:', 
                      predict(model_WeekMax, dat_test[i], type = 'raw')))

}


ProjAppPhaseWeekMaxWeekDistData_Mean_Model <- function(model_DefectCount, 
                                                       model_WeekMax,
                                                       dat,
                                                       phase,
                                                       app_complexity,
                                                       app_phase_duration,
                                                       test_case_count,
                                                       dev_team_size,
                                                       test_team_size){
  dat_ProjAppPhaseWeek <- GetProjAppPhaseWeekData(dat)
  dt <- dat_ProjAppPhaseWeek
  dt[, proj_rls_app:=paste(Project, app_name, Release, sep='-'), 
     by = c('Project', 'app_name', 'Release')]
  
  
  ####### distribution of defects accross weeks normalised by total defect count
  dt[, week_number_max := max(as.numeric(as.character(week_number))),
                       by = c('Project', 'Release', 'app_name', 'phase')]
  dt[, defect_count_sum_by_app_phase := sum(defect_count),
      by = c('Project', 'Release', 'app_name', 'phase')]
  dt[, defect_count_norm:= defect_count / defect_count_sum_by_app_phase]
  qplot(defect_count_norm, data = dt)
  
  # Visualise normalised defect distribution
  ph = "5.2-FUNCTIONAL"
  dat_sub <- dt[dt$phase == ph & 
                  dt$week_number_max > 5  & dt$week_number_max < 10 , ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(title = paste('Normalised Defect Distribution (5 to 10 week) for phase:', ph))
  print(p)
  ph = "5.5-UAT"
  dat_sub <- dt[dt$phase == ph & 
                  dt$week_number_max > 5  & dt$week_number_max < 10 , ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(title = paste('Normalised Defect Distribution (5 to 10 week) for phase:', ph))
  print(p)
  
  # Normalisation defect distribution using cumulative count
  dt[, defect_count_cumsum:= cumsum(defect_count), 
        by = c('Project', 'Release', 'app_name', 'phase')]
  dt[, defect_count_norm := defect_count_cumsum / defect_count_sum_by_app_phase, 
      by = c('Project', 'Release', 'app_name', 'phase')]
  qplot(defect_count_norm, data = dt)
  
  ph = "5.5-UAT"
  dat_sub <- dt[dt$phase == ph & 
                  dt$week_number_max > 5  & dt$week_number_max < 10 , ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(title = paste('Normalised Defect Distribution (5 to 10 week) for phase:', ph))
  print(p)
  
  ph = "5.2-FUNCTIONAL"
  dat_sub <- dt[dt$phase == ph & 
                  dt$week_number_max > 5  & dt$week_number_max < 10 , ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(title = paste('Normalised Defect Distribution (5 to 10 week) for phase:', ph))
  print(p)
  
  
  dat_model <- expand.grid(proj_rls_app = unique(dt$proj_rls_app),
                           phase = unique(dt$phase),
                           week_number = 0:100)
  dat_model <- data.table(dat_model)
  dat_model[, defect_count_norm:= as.numeric(NA)]
  dat_model[week_number == 0, defect_count_norm := 0]
  dt[, week_number_old := week_number]
  setorder(dat_model, proj_rls_app, phase, week_number)
  setorder(dt, proj_rls_app, phase, week_number)
  setkey(dat_model, proj_rls_app, phase, week_number)
  setkey(dt, proj_rls_app, phase, week_number_old)
  for(ph in unique(dt$phase)){
    print(paste('Working on phase:', ph))
    for(proj_rls_app_val in unique(dt$proj_rls_app)){
      t <- dt[proj_rls_app == proj_rls_app_val & phase == ph]
      if(nrow(t)>= 1){
        t <- t[, list(proj_rls_app, phase, week_number_old, defect_count_norm)]
        percentile <- ecdf(t$week_number_old)
        t$week_number <- round(sapply(t$week_number_old, percentile) * 100, 0)
        setkey(t, proj_rls_app, phase, week_number)
        set(dat_model, i = which(dat_model$proj_rls_app == proj_rls_app_val & 
                               dat_model$phase == ph &
                               dat_model$week_number %in% t$week_number),
            j = 'defect_count_norm', t$defect_count_norm)
      }
    }
  }
  
  # check correctness
  proj_rls_app_val = 'CONSOLIDATED DEFECTS DUMP Q2-CONTRACT PRINT - 6560-DRM_METLIFE_2014-Q2'
  ph = '2-ANALYSIS'
  dat_model[dat_model$proj_rls_app == proj_rls_app_val & 
                               dat_model$phase == ph]
  dat_model[, defect_count_norm_na_filled:=na.approx(defect_count_norm), by = c('proj_rls_app', 'phase')]
  
  dat_model <- join(dat_model,
                     dt[, list(Project, Release, app_name, phase, proj_rls_app, 
                               week_number_max)],
                     by = c('proj_rls_app', 'phase'), match = 'first')
  dat_model[, defect_count_norm := defect_count_norm_na_filled]

  # remove ghost lines added by "dat_model[week_number == 0, defect_count_norm := 0]"
  dat_model<- dat_model[!is.na(week_number_max)]
  
  ph = "5.2-FUNCTIONAL"
  dat_sub <- dat_model[dat_model$phase == ph & 
                  dat_model$week_number_max > 5  & dat_model$week_number_max < 10 , ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(ylab = 'week_number_norm') + 
        labs(title = paste('Normalised Cumulative Defect Distribution (5 to 10 week) for phase:', ph))
  print(p)
  
  ph = "5.2-FUNCTIONAL"
  dat_sub <- dat_model[dat_model$phase == ph, ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(ylab = 'week_number_norm') + 
        labs(title = paste('Normalised Cumulative Defect Distribution for phase:', ph)) +
    theme(legend.position="bottom")
  print(p)
  
  ph = "5.4-REGRESSION"
  dat_sub <- dat_model[dat_model$phase == ph, ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(ylab = 'week_number_norm') + 
        labs(title = paste('Normalised Cumulative Defect Distribution for phase:', ph)) +
    theme(legend.position="bottom")
  print(p)
  
  ph = "5.5-UAT"
  dat_sub <- dat_model[dat_model$phase == ph, ]
  p <- ggplot(dat_sub, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app, color = proj_rls_app)) + 
        geom_line()  + labs(ylab = 'week_number_norm') + 
        labs(title = paste('Normalised Cumulative Defect Distribution for phase:', ph)) +
    theme(legend.position="bottom")
  print(p)
  
  # all curves
  dat_model[,proj_rls_app_phase := paste(proj_rls_app, phase, sep='-')]
  p <- ggplot(dat_model, aes(x = week_number, y = defect_count_norm, 
                      group=proj_rls_app_phase, color = phase)) + 
        geom_line()  + labs(ylab = 'week_number_norm') + 
        labs(title = paste('Normalised Cumulative Defect Distribution for phase:', ph)) +
    theme(legend.position="none")
  print(p)
  
  
  ### explore whether averaing distribution gives relevent results
  model_WeekDist <- dat_model[, list(proj_rls_app, phase, week_number,
                                     defect_count_norm)]
  model_WeekDist[, proj_rls_app_phase := paste(proj_rls_app, phase, sep = '-')]
  model_WeekDist[, lag.value:=c(NA, defect_count_norm[-.N]), 
                  by='proj_rls_app_phase']
  model_WeekDist[, defect_count_norm_pdf := defect_count_norm - lag.value]
  model_WeekDist[is.na(defect_count_norm_pdf), defect_count_norm_pdf := 0]
  model_WeekDist_phase <- model_WeekDist[, list(defect_count_norm_pdf_phase = mean(defect_count_norm_pdf)),
                                         by = c('phase', 'week_number')]
  model_WeekDist_phase[, defect_count_norm_phase := cumsum(defect_count_norm_pdf_phase), 
                       by = c('phase')]
  model_WeekDist_phase[, defect_count_norm_pdf_phase:=NULL]
  # model_WeekDist <- dat_model[, list(defect_count_norm_phase = mean(defect_count_norm)),
  #                             by = c('phase', 'week_number')]
  for(ph in unique(model_WeekDist$phase)){
    print(qplot(week_number, defect_count_norm_phase, 
                data= model_WeekDist_phase[phase == ph], 
          group = phase, color = phase))
  }
  qplot(week_number, defect_count_norm_phase, data= model_WeekDist_phase, 
        group = phase, color = phase)
  
  model_WeekDist <- model_WeekDist_phase
  model_WeekDist
}

Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model <- function(model_DefectCount, 
                                                       model_WeekMax,
                                                       model_WeekDist,
                                                       dat,
                                                       phase,
                                                       app_complexity,
                                                       app_phase_duration,
                                                       test_case_count,
                                                       dev_team_size,
                                                       test_team_size){
  
  dat_test <- GetProjAppPhaseWeekMaxData(dat)
  i <- 1
  dat_test$phase[i] <- phase
  dat_test$app_complexity[i] <- app_complexity
  dat_test$app_phase_duration[i] <- app_phase_duration
  dat_test$test_case_count[i] <- test_case_count
  dat_test$dev_team_size[i] <- dev_team_size
  dat_test$test_team_size[i] <- test_team_size
  dat_test <- FeatureEng_ProjAppPhaseData(dat_test)
  defect_cnt_pred <- predict(model_DefectCount, dat_test[i], type = 'response')
  
  print.noquote(paste('Predicted Defects:', defect_cnt_pred))
  dat_test$defect_count_pred <- predict(model_DefectCount, dat_test, type = 'response')
  dat_test <- FeatureEng_GetProjAppPhaseWeekMaxData(dat_test)
  week_max_pred <- predict(model_WeekMax, dat_test[i], type = 'raw')
  print.noquote(paste('Number of Weeks Testing will Continue:', week_max_pred))

  week_number_range <- 1:round(week_max_pred, 0)
  percentile <- ecdf(week_number_range)
  week_number_percentile <- round(sapply(week_number_range, percentile) * 100, 0)
  # phase = '6-POST-DEPLOYMENT'
  ph <- phase
  out <- model_WeekDist[phase == ph & week_number %in% week_number_percentile, ]
  out[, lag.value:=c(0, defect_count_norm_phase[-.N])]
  out[, defect_count_pdf_norm_phase := defect_count_norm_phase - lag.value]
  out[, lag.value := NULL]
  out[, defect_count_pred := round(defect_cnt_pred * defect_count_pdf_norm_phase, 0)]
  out <- out[, list(week_number, defect_count_pred)]
  out[, week_number := week_number_range]
  print(out)
  print(ggplot(out, aes(week_number, defect_count_pred)) +
          geom_bar(stat="identity"))
  out
}


# Functions: Run Code ----------------------------------------------------------
############## Project - Relese - App - Phase Model
if(FALSE){
  dat <- LoadData()
  dat_ProjAppPhase <- GetProjAppPhaseData(dat)
  dat_ProjAppPhase <- FeatureEng_ProjAppPhaseData(dat_ProjAppPhase)
  model <- ProjAppPhaseData_GLM_RF_Model(dat_ProjAppPhase)
  # save(model, file = 'i_model_scoring/ProjAppPhaseData_GLM_RF_Model.Rdata')
  load('i_model_scoring/ProjAppPhaseData_GLM_RF_Model.Rdata')
  Score_ProjAppPhaseData_GLM_RF_Model(model, 
                                     phase = "4.1-UNIT TEST",
                                     app_complexity = 52,
                                     app_phase_duration = 4,
                                     test_case_count = 1020,
                                     dev_team_size = 18,
                                     test_team_size = 80)
  # Predicted Defects: 28.2985
  Score_ProjAppPhaseData_GLM_RF_Model(model, 
                                     phase = "4.1-UNIT TEST",
                                     app_complexity = 52,
                                     app_phase_duration = 12,
                                     test_case_count = 1020,
                                     dev_team_size = 18,
                                     test_team_size = 80)
  # Predicted Defects: 30.6555
  Score_ProjAppPhaseData_GLM_RF_Model(model, 
                                     phase = "4.1-UNIT TEST",
                                     app_complexity = 52,
                                     app_phase_duration = 16,
                                     test_case_count = 1020,
                                     dev_team_size = 18,
                                     test_team_size = 80)
  # Predicted Defects: 34.80275
  Score_ProjAppPhaseData_GLM_RF_Model(model,
                                     phase = "5.2-FUNCTIONAL",
                                     app_complexity = 52,
                                     app_phase_duration = 12,
                                     test_case_count = 1020,
                                     dev_team_size = 18,
                                     test_team_size = 80)
  # Predicted Defects: 89.2247147435898
}

############## Project - Relese - App - Phase - Week - WeekMax - Week Distribution Model
if(TRUE){
  if(FALSE){ 
    # Build Model for total defects count & weeks testing will last & week distribution
    dat <- LoadData()
    dat_ProjAppPhase <- GetProjAppPhaseData(dat)
    dat_ProjAppPhase <- FeatureEng_ProjAppPhaseData(dat_ProjAppPhase)
    model_DefectCount <- ProjAppPhaseData_GLM_RF_Model(dat_ProjAppPhase)
    save(model_DefectCount, file = 'i_model_scoring/ProjAppPhaseData_GLM_RF_Model.Rdata')
    load('i_model_scoring/ProjAppPhaseData_GLM_RF_Model.Rdata')
    
    dat <- LoadData()
    dat_ProjAppPhaseWeekMax <- GetProjAppPhaseWeekMaxData(dat)
    dat_ProjAppPhaseWeekMax <- FeatureEng_GetProjAppPhaseWeekMaxData(dat_ProjAppPhaseWeekMax)
    dat_ProjAppPhaseWeekMax$defect_count_pred <- predict(model_DefectCount, dat_ProjAppPhaseWeekMax, type = 'response')
    model_WeekMax <- ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model(dat_ProjAppPhaseWeekMax)
    save(model_WeekMax, file = 'i_model_scoring/ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model.Rdata')
    load('i_model_scoring/ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model.Rdata')
    model_WeekDist <- ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount, 
                                          model_WeekMax, 
                                          dat,
                                          phase = "5.2-FUNCTIONAL",
                                          app_complexity = 52,
                                          app_phase_duration = 16,
                                          test_case_count = 1020,
                                          dev_team_size = 18,
                                          test_team_size = 80)
    save(model_WeekDist, file = 'i_model_scoring/ProjAppPhaseWeekMaxWeekDistData_Mean_Model.Rdata')
    load('i_model_scoring/ProjAppPhaseWeekMaxWeekDistData_Mean_Model.Rdata')
  }else{
    # Score / Predict for total defects count & weeks testing will last & week distribution
    dat <- LoadData()
    load('i_model_scoring/ProjAppPhaseData_GLM_RF_Model.Rdata')
    load('i_model_scoring/ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model.Rdata')
    load('i_model_scoring/ProjAppPhaseWeekMaxWeekDistData_Mean_Model.Rdata')
    Score_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model(model_DefectCount, 
                                                   model_WeekMax, 
                                                   dat,
                                       phase = "4.1-UNIT TEST",
                                       app_complexity = 52,
                                       app_phase_duration = 4,
                                       test_case_count = 1020,
                                       dev_team_size = 18,
                                       test_team_size = 20)
   # Predicted Defects: 26.0540833333333
   # Number of Weeks Testing will Continue: 3.24505211594527
   Score_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model(model_DefectCount, 
                                                   model_WeekMax, 
                                                   dat,
                                       phase = "4.1-UNIT TEST",
                                       app_complexity = 52,
                                       app_phase_duration = 16,
                                       test_case_count = 1020,
                                       dev_team_size = 18,
                                       test_team_size = 80)
   # Predicted Defects: 34.80275
   # Number of Weeks Testing will Continue: 4.47116423008192
   Score_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model(model_DefectCount,
                                                   model_WeekMax,
                                                   dat,
                                       phase = "5.2-FUNCTIONAL",
                                       app_complexity = 52,
                                       app_phase_duration = 16,
                                       test_case_count = 1020,
                                       dev_team_size = 18,
                                       test_team_size = 20)
   # Predicted Defects: 92.9652083333334
   # Number of Weeks Testing will Continue: 4.47116423008192
   out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                           model_WeekMax,
                                                           model_WeekDist,
                                                           dat,
                                                           phase = "4.1-UNIT TEST",
                                                           app_complexity = 52,
                                                           app_phase_duration = 4,
                                                           test_case_count = 1020,
                                                           dev_team_size = 18,
                                                           test_team_size = 20)
   out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                           model_WeekMax,
                                                           model_WeekDist,
                                                           dat,
                                                           phase = "4.1-UNIT TEST",
                                                           app_complexity = 52,
                                                           app_phase_duration = 16,
                                                           test_case_count = 1020,
                                                           dev_team_size = 18,
                                                           test_team_size = 20)
   out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                           model_WeekMax,
                                                           model_WeekDist,
                                                           dat,
                                                           phase = "5.2-FUNCTIONAL",
                                                           app_complexity = 52,
                                                           app_phase_duration = 16,
                                                           test_case_count = 1020,
                                                           dev_team_size = 18,
                                                           test_team_size = 20)
   
  }
}


#### output format
# Project, relese, app_name, phase, weeknum, defect_count_predicted



