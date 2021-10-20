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
library(RANN)


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
    y_pred <- predict(model, dat_model[.idx_train, .fs_ind, with = FALSE], type="raw", na.action = na.pass)
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
    y_pred <- predict(model, dat_model[.idx_valid, .fs_ind, with = FALSE], type="raw", na.action = na.pass)
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
  dat <- fread('c_input_data/MetLife_Consolidated_v4.csv')
  c("V1", "ProjCMMI", "Release", "Project", "phase", "app_name", 
  "app_category", "app_complexity", "phase_app_duration", "test_case", 
  "dev_team_size", "test_team_size", "Year", "WEEK", "defect_count", 
  "combin1", "combin2", "week_number")
  # new data with zero padding
  c("V1", "ProjCMMI", "app_type", "project", "app_category", "release", 
    "app_complexity", "app_name", "Planned_Duration_new", "phase", 
    "Calender_Year", "test_case", "Calender_Week", "dev_team_size", 
    "Actual_Defect", "test_team_size", "app_phase_wk_no", "app_wk_no"
  )
  
  # Rename field names
  setnames(dat, 'Actual_Defect', 'defect_count')
  setnames(dat, 'Planned_Duration_new', 'app_phase_duration')
  setnames(dat, 'test_case', 'test_case_count')
  setnames(dat, 'app_phase_wk_no', 'week_number')
  setnames(dat, 'app_wk_no', 'week_number_app')
  setnames(dat, 'project', 'Project')
  setnames(dat, 'release', 'Release')
  
  dat[, app_complexity := as.numeric(app_complexity)]
  dat[, app_phase_duration := as.numeric(app_phase_duration)]
  dat[, test_case_count := as.numeric(test_case_count)]
  dat[, dev_team_size := as.numeric(dev_team_size)]
  dat[, test_team_size := as.numeric(test_team_size)]

  dat[, ProjCMMI := as.factor(ProjCMMI)]
  dat[, phase := as.factor(phase)]
  dat[, week_number := as.factor(week_number)]
  
  dat[, week_number_app := as.factor(week_number_app)]
  dat[, app_category := as.factor(app_category)]
  
  sapply(dat, class)
  dat
}

PreProcessNAWeek <- function(d){
  cols <- paste('Week', seq(10), sep='_')
  for(cl in cols){
    d[, 'temp'] = 0
    d[is.na(get(cl)), temp:= 1]
    d[is.na(get(cl)), (cl):=0]
    setnames(d, 'temp', paste(cl, 'NA', sep = '_'))
  }
  d
}

# Functions: Preprocess Data and Feature Enginering ---------------------------

GetProjAppPhaseData <- function(dat){
  ## Prepare Data
  # "ProjCMMI" dropped as it is 3 for all apps
  fs_all <- c('Project', 'Release', 'app_name', "phase", "app_complexity",
              "app_phase_duration",  "test_case_count",
              "dev_team_size", "test_team_size",  "week_number",
              'prj_rel_app_phase')
  fs_target <- c('defect_count')
  dat[, prj_rel_app_phase:= paste(Project, Release, app_name, phase, sep = '-')]
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
  
  dat_app_phase[, prj_rel_app_phase:= paste(Project, Release, app_name, 
                                            phase, sep = '-')]
  dat_app_phase <- dat_app_phase[,c('prj_rel_app_phase', "defect_count", "phase",
                                    "app_complexity", 
                                    "app_phase_duration", "test_case_count", 
                                    "dev_team_size", "test_team_size"), 
                                 with = FALSE]
  qplot(defect_count, data = dat_app_phase)
  # outlier detection: remove obs with defect_count more than 200
  sum(dat_app_phase$defect_count >= 200) # 4
  dat_app_phase <- dat_app_phase[dat_app_phase$defect_count < 200,]
  
  dat_WeekNewData <- dcast(dat, prj_rel_app_phase + week_number ~ week_number, 
                           value.var = 'defect_count')
  cols <- paste('Week', seq(10), sep = '_')  
  setnames(dat_WeekNewData, colnames(dat_WeekNewData)[3:12], cols)
  dat_WeekNewData <- dat_WeekNewData[, c('prj_rel_app_phase', 'week_number',
                                         cols), with = FALSE]
  setkey(dat_WeekNewData, prj_rel_app_phase, week_number)
  dat_WeekNewData[ ,  Week_1:= na.locf(Week_1, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_2:= na.locf(Week_2, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_3:= na.locf(Week_3, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_4:= na.locf(Week_4, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_5:= na.locf(Week_5, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_6:= na.locf(Week_6, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_7:= na.locf(Week_7, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_8:= na.locf(Week_8, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_9:= na.locf(Week_9, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_10:= na.locf(Week_10, na.rm=FALSE), by = 'prj_rel_app_phase']

  dat_app_phase <- join(dat_app_phase, 
            dat_WeekNewData,
            by = 'prj_rel_app_phase')
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
  dat_ProjAppPhaseWeekMax[, prj_rel_app_phase:= paste(Project, Release, app_name, phase, sep = '-')]
  
  dat_ProjAppPhaseWeekMax <- 
    dat_ProjAppPhaseWeekMax[,c('prj_rel_app_phase', 'week_number_max', "defect_count", "phase",
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
  
  dat[, prj_rel_app_phase:= paste(Project, Release, app_name, phase, sep = '-')]
  dat_WeekNewData <- dcast(dat, prj_rel_app_phase + week_number ~ week_number, 
                           value.var = 'defect_count')
  cols <- paste('Week', seq(10), sep = '_')  
  setnames(dat_WeekNewData, colnames(dat_WeekNewData)[3:12], cols)
  dat_WeekNewData <- dat_WeekNewData[, c('prj_rel_app_phase', 'week_number',
                                         cols), with = FALSE]
  setkey(dat_WeekNewData, prj_rel_app_phase, week_number)
  dat_WeekNewData[ ,  Week_1:= na.locf(Week_1, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_2:= na.locf(Week_2, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_3:= na.locf(Week_3, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_4:= na.locf(Week_4, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_5:= na.locf(Week_5, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_6:= na.locf(Week_6, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_7:= na.locf(Week_7, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_8:= na.locf(Week_8, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_9:= na.locf(Week_9, na.rm=FALSE), by = 'prj_rel_app_phase']
  dat_WeekNewData[ ,  Week_10:= na.locf(Week_10, na.rm=FALSE), by = 'prj_rel_app_phase']
  
  dat_ProjAppPhaseWeekMax <- join(dat_ProjAppPhaseWeekMax, 
                        dat_WeekNewData,
                        by = 'prj_rel_app_phase')
  
  dat_ProjAppPhaseWeekMax
}

FeatureEng_GetProjAppPhaseWeekMaxData <- function(dat_ProjAppPhaseWeekMax){
  dat_ProjAppPhaseWeekMax[, test_cases_per_tester := test_case_count / test_team_size]
  dat_ProjAppPhaseWeekMax
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
  # dat_model = dat_ProjAppPhase
  # idx_train <- sample(seq(1, nrow(dat_model)), 0.75 * nrow(dat_model))
  idx_train <- seq(1, nrow(dat_model))[1:(0.75 * nrow(dat_model))] 
  idx_valid <- setdiff(seq(1, nrow(dat_model)), idx_train)
  
  fs_ind <- c("phase", "app_complexity", "app_phase_duration",  "test_case_count",
              "dev_team_size", "test_team_size", "Week_1", "Week_2", "Week_3", "Week_4", "Week_5", 
              "Week_6", "Week_7", "Week_8", "Week_9", "Week_10") # paste('Week_', seq(10), '_NA', sep = '') # not used as varImp is low
  fs_target <- c('defect_count')
  
  # impute NAs
  # model_impute_ProjAppPhase <- preProcess(dat_model[idx_train, c(fs_ind),
  #                                                   with = FALSE], 
  #                                         method = c("knnImpute"))
  # dat_model_sub <- predict(model_impute_ProjAppPhase, dat_model[,c(fs_ind), with = FALSE])
  # dat_model <- cbind(dat_model[, fs_target, with = FALSE], 
  #                    dat_model_sub)
  # NA impute other method
  dat_model <- PreProcessNAWeek(dat_model)
  
  if(FALSE){  # do not run all models
    ##### GLM Model #####-
    model <- glm(defect_count ~ . , data = dat_model[idx_train, c(fs_ind, fs_target),
                                                     with = FALSE], family = poisson)
    print(summary(model))
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)
    
    ##### RF Model #####-
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    model <- train(defect_count ~ ., 
                   data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE], 
                   method = "rf", 
                   preProcess=c("knnImpute"),
                   trControl = fitControl,
                   verbose = TRUE)
    print(summary(model))
    print(plot(varImp(model)))
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind, is_caret = TRUE)
  }

  ##### Final Model #####-
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  model <- train(defect_count ~ ., 
                 data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE], 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
  print(summary(model))
  print(plot(varImp(model)))
  PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind, is_caret = TRUE)
  # Train set: R2: 0.9437    MAE: 6.1048     RMSE: 10.0195    MAPE: 0.7226   
  # Valid set: R2: 0.4126    MAE: 19.8598    RMSE: 30.5704    MAPE: 0.7621   
  
  model <- list(model = model, model_impute_ProjAppPhase = NA)
  model
}

Predict_DefectCount <- function(model, x){
  # t <- predict(model$model_impute_ProjAppPhase, x)
  t <- PreProcessNAWeek(x)
  predict(model$model, t, type = 'raw')
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
  print.noquote(paste('Predicted Defects:', Predict_DefectCount(model, dat_test[i])))
}

ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model <- function(dat_model){
  # dat_model = dat_ProjAppPhaseWeekMax
  # idx_train <- sample(seq(1, nrow(dat_model)), 0.75 * nrow(dat_model))
  idx_train <- seq(1, nrow(dat_model))[1:(0.75 * nrow(dat_model))] 
  idx_valid <- setdiff(seq(1, nrow(dat_model)), idx_train)
  fs_ind <- c("phase",   "test_case_count",
               "test_team_size" , 'test_cases_per_tester', "app_complexity",  
              "dev_team_size", 'defect_count_pred', "app_phase_duration",
              "Week_1", "Week_2", "Week_3", "Week_4", "Week_5", 
              "Week_6", "Week_7", "Week_8") #, "Week_9", "Week_10" # removed as does not has variation
  # fs_ind <- c('defect_count_pred', 'test_cases_per_tester')
  fs_target <- c('week_number_max')
  dat_model[, week_number_max := as.numeric(as.character(week_number_max))]
  # qplot(week_number_max , data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE])
  
  # impute NAs
  # model_impute_ProjAppPhaseWeekMax <- preProcess(dat_model[idx_train, c(fs_ind),
  #                                                   with = FALSE], 
  #                                         method = c("knnImpute"))
  # dat_model_sub <- predict(model_impute_ProjAppPhaseWeekMax, 
  #                          dat_model[,c(fs_ind), with = FALSE])
  # dat_model <- cbind(dat_model[, fs_target, with = FALSE], 
  #                    dat_model_sub)
  dat_model <- PreProcessNAWeek(dat_model)
  
  if(FALSE){ # do not run all models
    ##### GLM Model #####-
    model <- glm(week_number_max ~ . , data = dat_model[idx_train, c(fs_ind, fs_target),
                                                     with = FALSE], family = poisson)
    print(summary(model))
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)

    ##### RF Model #####-
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    model <- train(week_number_max ~ ., 
                   data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE], 
                   method = "rf", 
                   trControl = fitControl,
                   verbose = FALSE)
    print(summary(model))
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind, is_caret = TRUE)

    model <- randomForest(week_number_max ~ . , 
                          data = dat_model[idx_train, c(fs_ind, fs_target), with = FALSE])
    summary(model)
    importance(model)
    varImpPlot(model)
    PrintResult(model, dat_model, idx_train, idx_valid, fs_target, fs_ind)
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
  # [1] Train set: R2: 0.9978    MAE: 0.0754    RMSE: 0.127    MAPE: 0.0157   
  # [1] Valid set: R2: 0.615    MAE: 0.7959    RMSE: 0.8462    MAPE: 0.1165   
  
  model = list(model = model, 
               model_impute_ProjAppPhaseWeekMax=NA )
  model
}

Predict_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model <- function(model, x){
  # t <- predict(model$model_impute_ProjAppPhaseWeekMax, x)
  t <- PreProcessNAWeek(x)
  predict(model$model, t, type = 'raw')
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
  print.noquote(paste('Predicted Defects:', 
                      Predict_DefectCount(model_DefectCount, dat_test[i])))
  dat_test$defect_count_pred <- 
    Predict_DefectCount(model_DefectCount, dat_test)
  
  dat_test <- FeatureEng_GetProjAppPhaseWeekMaxData(dat_test)
  print.noquote(paste('Number of Weeks Testing will Continue:', 
        Predict_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model(model_WeekMax, dat_test[i])))

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
                                                       test_team_size,
                                                       defect_count_week){
  # defect_count_week = c(5, 6)
  dat_test <- GetProjAppPhaseWeekMaxData(dat)
  i <- 1
  dat_test$phase[i] <- phase
  dat_test$app_complexity[i] <- app_complexity
  dat_test$app_phase_duration[i] <- app_phase_duration
  dat_test$test_case_count[i] <- test_case_count
  dat_test$dev_team_size[i] <- dev_team_size
  dat_test$test_team_size[i] <- test_team_size
  defect_count_week_true <- defect_count_week
  if(length(defect_count_week) < 10){
    defect_count_week <- c(defect_count_week, rep(NA, 10 - length(defect_count_week)))
  }
  dat_test$Week_1[i] <- defect_count_week[1]
  dat_test$Week_2[i] <- defect_count_week[2]
  dat_test$Week_3[i] <- defect_count_week[3]
  dat_test$Week_4[i] <- defect_count_week[4]
  dat_test$Week_5[i] <- defect_count_week[5]
  dat_test$Week_6[i] <- defect_count_week[6]
  dat_test$Week_7[i] <- defect_count_week[7]
  dat_test$Week_8[i] <- defect_count_week[8]
  dat_test$Week_9[i] <- defect_count_week[9]
  dat_test$Week_10[i] <- defect_count_week[10]
  
  dat_test <- FeatureEng_ProjAppPhaseData(dat_test)
  defect_cnt_pred <- Predict_DefectCount(model_DefectCount, dat_test[i])
  
  print.noquote(paste('Predicted Defects:', defect_cnt_pred))
  dat_test$defect_count_pred <- Predict_DefectCount(model_DefectCount, dat_test)
  dat_test <- FeatureEng_GetProjAppPhaseWeekMaxData(dat_test)
  week_max_pred <- Predict_ProjAppPhaseWeekMaxData_GLM_RF_GBM_Model(model_WeekMax, 
                                                                    dat_test[i])
  print.noquote(paste('Number of Weeks Testing will Continue:', week_max_pred))

  # max_week <- ifelse(round(week_max_pred, 0) < 1, 1, round(week_max_pred, 0))
  max_week <- round(week_max_pred, 0)
  week_number_range <- 1:max_week
  percentile <- ecdf(week_number_range)
  week_number_percentile <- round(sapply(week_number_range, percentile) * 100, 0)
  # phase = '6-Post-Deployment'
  ph <- phase
  out <- model_WeekDist[phase == ph & week_number %in% week_number_percentile, ]
  out[, lag.value:=c(0, defect_count_norm_phase[-.N])]
  out[, defect_count_pdf_norm_phase := defect_count_norm_phase - lag.value]
  out[, defect_count_pred := round(defect_cnt_pred * defect_count_pdf_norm_phase, 0)]
  out[, week_number := week_number_range]
  out[, defect_count_week_true := defect_count_week[1:nrow(out)]]
  defect_cnt_pred_rem <- max(defect_cnt_pred - sum(out$defect_count_week_true,
                                                   na.rm = TRUE), 0)
  out[, 'pred_remaining'] = 0
  out[is.na(defect_count_week_true), pred_remaining := 1] 
  cross_prod <- crossprod(out$pred_remaining, 
                          out$defect_count_pdf_norm_phase)
  out[, defect_count_pdf_norm_phase_scaled := 
        defect_count_pdf_norm_phase / cross_prod]
  out[, defect_count_pred := round(defect_cnt_pred_rem * defect_count_pdf_norm_phase_scaled, 0)]
  out[!is.na(defect_count_week_true), 'defect_count_pred'] = defect_count_week_true[which(out$pred_remaining == 0)]
  out[, lag.value := NULL]
  out[, defect_count_pdf_norm_phase_scaled := NULL]
  out <- out[, list(week_number, defect_count_pred, defect_count_week_true)]
  print(out)
  p <- ggplot(out, aes(x=week_number, y=defect_count_week_true)) +
    geom_bar(stat="identity", alpha=0.75) + 
    geom_line(data=out, aes(x=week_number, y=defect_count_pred), colour="blue")
  
  print(p)
  
  out[, defect_count_week_true := NULL]
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
    dat_ProjAppPhaseWeekMax$defect_count_pred <- Predict_DefectCount(model_DefectCount,
                                                         dat_ProjAppPhaseWeekMax)
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
    out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                            model_WeekMax,
                                                            model_WeekDist,
                                                            dat,
                                                            phase = "5.2-Functional",
                                                            app_complexity = 52,
                                                            app_phase_duration = 4,
                                                            test_case_count = 1020,
                                                            dev_team_size = 18,
                                                            test_team_size = 20,
                                                            defect_count_week = NA)
    out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                            model_WeekMax,
                                                            model_WeekDist,
                                                            dat,
                                                            phase = "4.1-Unit Test",
                                                            app_complexity = 52,
                                                            app_phase_duration = 4,
                                                            test_case_count = 1020,
                                                            dev_team_size = 18,
                                                            test_team_size = 20,
                                                            defect_count_week = c(5, 6))
    out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                            model_WeekMax,
                                                            model_WeekDist,
                                                            dat,
                                                            phase = "4.1-Unit Test",
                                                            app_complexity = 52,
                                                            app_phase_duration = 4,
                                                            test_case_count = 1020,
                                                            dev_team_size = 18,
                                                            test_team_size = 20,
                                                            defect_count_week = c(10, 12))
    out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                            model_WeekMax,
                                                            model_WeekDist,
                                                            dat,
                                                            phase = "4.1-Unit Test",
                                                            app_complexity = 52,
                                                            app_phase_duration = 16,
                                                            test_case_count = 1020,
                                                            dev_team_size = 18,
                                                            test_team_size = 20,
                                                            defect_count_week = c(5, 4, 4))
    out <- Score_ProjAppPhaseWeekMaxWeekDistData_Mean_Model(model_DefectCount,
                                                            model_WeekMax,
                                                            model_WeekDist,
                                                            dat,
                                                            phase = "5.2-Functional",
                                                            app_complexity = 52,
                                                            app_phase_duration = 16,
                                                            test_case_count = 1020,
                                                            dev_team_size = 18,
                                                            test_team_size = 20,
                                                            defect_count_week = c(5, 10, 15, 11))
  }
}



