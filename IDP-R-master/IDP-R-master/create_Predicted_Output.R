
# #####################################################################
# Performing Basic Operations & Rollup @ Application-Phase Weekly level
# #####################################################################
f_predict_defect =  function(df,rc_agg_mrged)
{
  Consolidated_Training_df = df
  Consolidated_Training_df$WeekNum <-  week(Consolidated_Training_df$openDate)
  Consolidated_Training_df$year <- year(Consolidated_Training_df$openDate)
  Consolidated_Training_df$Freq <-  1
  
  # Getting the Weekly Defect Identified
  all_agg <- aggregate(x = Consolidated_Training_df$Freq, list(ProjCMMI = Consolidated_Training_df$maturity
                                                               ,project =Consolidated_Training_df$projectName
                                                               ,phase = Consolidated_Training_df$phase
                                                               ,app_type = Consolidated_Training_df$appVersion
                                                               ,app_name = Consolidated_Training_df$application
                                                               #,app_category = Consolidated_Training_df$appCategory
                                                               ,app_complexity = Consolidated_Training_df$appComplexity
                                                               ,phase_app_duration = Consolidated_Training_df$durationWeeks
                                                               ,test_case = Consolidated_Training_df$noOfExecutedTestCases
                                                               ,dev_team_size = Consolidated_Training_df$developerTeamSize
                                                               ,test_team_size = Consolidated_Training_df$testTeamSize
                                                               ,Flag = Consolidated_Training_df$Flag
                                                               ,Year = Consolidated_Training_df$year
                                                               ,WEEK = Consolidated_Training_df$WeekNum
                                                               ,week_Number = Consolidated_Training_df$WeekNum)
                       , FUN = "sum")
  
  
  all_agg <- setnames(all_agg, old = c('x'), new =c('app_phase_wklyDef'))
  all_agg1 <- all_agg[with(all_agg, order(all_agg$ProjCMMI, all_agg$project 
                                          ,all_agg$phase,all_agg$app_name,all_agg$Flag
                                          ,all_agg$Year,all_agg$WEEK,all_agg$week_Number)), ]
  
  # Generating Week Number
  
  all_agg1$combin <- paste( all_agg1$ProjCMMI,all_agg1$project
                            ,all_agg1$phase,all_agg1$app_name,all_agg1$Flag
                            , sep = "_")
  # for(i in 1:nrow(all_agg1))
  # {
  #   if(i ==1){
  #     all_agg1$WEEK[i]=1
  #   }else{
  #     if(all_agg1$combin[i]!=all_agg1$combin[i-1]){
  #       all_agg1$WEEK[i]=1
  #     }else{
  #       all_agg1$WEEK[i]=all_agg1$WEEK[i-1]+1
  #     }
  #   }
  # }
  #Sys.sleep(1)
 
#NATZ   
 for(i in 1:nrow(all_agg1)){
   if(i==1){
     all_agg1$week_Number[i]=1
   }else{
     if(all_agg1$combin[i]!=all_agg1$combin[i-1]){
       all_agg1$week_Number[i]=1
     }else{
       all_agg1$week_Number[i]=all_agg1$week_Number[i-1]+1
     }
   }
 }
  
  # ##################################################################
  # Calculating Total Cumulative Defect till last week
  # ##################################################################
  
  all_agg2 <- all_agg1
  
  for (i in 1:nrow(all_agg2)) {
    if(i==1){
      all_agg2$cum_def_last_wk[i]=0
    }else{
      if(all_agg2$combin[i]==all_agg2$combin[i-1]){
        all_agg2$cum_def_last_wk[i]=all_agg2$app_phase_wklyDef[i-1]+all_agg2$cum_def_last_wk[i-1]
      }else{
        all_agg2$cum_def_last_wk[i]=0
      }
    }
  }
  
  # ##########################################################
  # Creating Indicator Variables for Phases
  # ##########################################################
  
  #9 Phase
  for(level in unique(all_agg2$phase)){
    all_agg2[paste("dummyPhase", level, sep = "_")] <- ifelse(all_agg2$phase == level, 1, 0)
  }
  
  # ##########################################################
  # Creating Baseline for Calibration
  # ##########################################################
  
# Check if the inprogress data belongs to a duffernt population by means of volume
  
  avg_val_bse = mean(all_agg2$app_phase_wklyDef[all_agg2$Flag == "Base"])
  avg_val_inp = mean(all_agg2$app_phase_wklyDef[all_agg2$Flag == "Inprogress"])
  sd_val_bse  = sd(all_agg2$app_phase_wklyDef[all_agg2$Flag == "Base"])
  
  duration_actual = max(all_agg2$week_Number[all_agg2$Flag == "Inprogress"]) - min(all_agg2$week_Number[all_agg2$Flag == "Inprogress"])
  
  mean_shift_index = 1
  Z_distance =  (avg_val_inp - avg_val_bse) / sd_val_bse
   
  
  if(duration_actual> 3 & Z_distance > 3.5)
    {
      mean_shift_index = 0.8 * (avg_val_inp/avg_val_bse)
    }
    
#
  
  
  baseline_df1 <- all_agg2
  baseline_df4 <- data.frame(0,0, 0, 0)
  
  for (p in unique(baseline_df1$phase)) {
    baseline_df2 <- baseline_df1[baseline_df1$phase %in% p,]
    baseline_df3 <- data.frame(0,0, 0, 0)
    
    for (i in 1:max(baseline_df2$week_Number)) {
      
      df <- baseline_df2[which(baseline_df2$week_Number==i),]
      temp <- data.frame(p, i, mean(df$app_phase_wklyDef)
                         , sd(df$app_phase_wklyDef))
      colnames(temp) <- colnames(baseline_df3)
      baseline_df3 <- rbind(baseline_df3, temp)
    }
    colnames(baseline_df4) <- colnames(baseline_df3)
    baseline_df4 <- rbind(baseline_df4,baseline_df3)
  }
  
  baseline_df4 <- baseline_df4[-which(baseline_df4$X0==0), ]
  baseline_df4$X0.3 = ifelse(is.na(baseline_df4$X0.3),0,baseline_df4$X0.3)
  
  baseline_df4$upper_lmt <- baseline_df4$X0.2+baseline_df4$X0.3
  baseline_df4$lower_lmt <- baseline_df4$X0.2-baseline_df4$X0.3
  baseline_df4 <- setnames(baseline_df4, old=c('X0','X0.1','X0.2','X0.3'), new = c('phase','week_Number','Average','SD'))
  
  baseline_df4$lower_lmt = ifelse(baseline_df4$lower_lmt<0,0,baseline_df4$lower_lmt)
  baseline_df4$Average <- round(baseline_df4$Average,0)
  baseline_df4$upper_lmt <- round(baseline_df4$upper_lmt,0)
  baseline_df4$lower_lmt <- round(baseline_df4$lower_lmt,0)
  
  # ##########################################################
  # Generating Complete Prediction Table for Inprogress Application
  # ##########################################################
  
  Pred_Table <- all_agg2[all_agg2$Flag == "Inprogress", ]
  
  max_wk <- aggregate(x = Pred_Table$week_Number, list(project = Pred_Table$project
                                                       ,Pred_Table=Pred_Table$app_name
                                                       ,phase = Pred_Table$phase
                                                       ,app_complexity= Pred_Table$app_complexity
                                                       ,test_case= Pred_Table$test_case
                                                       ,dev_team_size= Pred_Table$dev_team_size
                                                       ,test_team_size =Pred_Table$test_team_size
  ), FUN="max")
  
  max_end_wk <- aggregate(x = projectProfile$durationWeeks
                          , list(project = projectProfile$projectName
                                 ,app_name=projectProfile$application
                                 , phase = projectProfile$phase
                                 ,app_complexity= projectProfile$appComplexity
                                 ,test_case= projectProfile$noOfExecutedTestCases
                                 ,dev_team_size= projectProfile$developerTeamSize
                                 ,test_team_size =projectProfile$testTeamSize
                          )
                          , FUN="max")
  
  
  Pred_Table1 <- merge(Pred_Table, max_wk)
  Pred_Table1 <- setnames(Pred_Table1, old=c('x'), new = c('MaxWeek'))
  Pred_Table2 <- merge(Pred_Table1, max_end_wk)
  Pred_Table2 <- setnames(Pred_Table2, old=c('x'), new = c('MaxEndWeek'))
  
  
  
  current_week <- Pred_Table2[Pred_Table2$week_Number == Pred_Table2$MaxWeek, ]
  
  for (i in 1:nrow(current_week)) {
    for (j in (current_week[i, which(colnames(current_week) %in% c('MaxWeek'))]+1):
         current_week[i, which(colnames(current_week) %in% c('MaxEndWeek'))]) {
      tmp <- current_week[i,]
      tmp$week_Number <- j
      current_week <- rbind(current_week, tmp)
    }
  }
  
  obs_rows <- Pred_Table2[which(Pred_Table2$week_Number < Pred_Table2$MaxWeek),]
  Final_Pred_Table <- rbind(obs_rows,current_week)
  
  
  # Creating Final Prediction Table with 'cum_def_last_wk'
  
  Final_Pred_Table <- Final_Pred_Table[with(Final_Pred_Table, order(Final_Pred_Table$project
                                                                    ,Final_Pred_Table$phase
                                                                    ,Final_Pred_Table$app_name)), ]
  
  Final_Pred_Table$cum_def_last_wk_old <- Final_Pred_Table$cum_def_last_wk
  features <-  c(
    'app_phase_wklyDef'
    , 'week_Number'
    , 'cum_def_last_wk'
    ,'dummyPhase_5.2-Functional'
    ,"dummyPhase_5.3-E2E"
  ) 
  
  
  pred <-  c()
  for(i in 1:nrow(Final_Pred_Table)){
    if(i > 1){
      if (Final_Pred_Table$combin[i]==Final_Pred_Table$combin[i-1]) {
        Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk'))] <- 
          Final_Pred_Table[i-1, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk'))] + pred[i - 1]
      }else{
        Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk'))] <- 
          Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk_old'))] 
      }
    }
    if(Final_Pred_Table[i, which(colnames(current_week) %in% c('week_Number'))] <= Final_Pred_Table[i, which(colnames(current_week) %in% c('MaxWeek'))]){
      pred[i] <- Final_Pred_Table[i, which(colnames(current_week) %in% c('app_phase_wklyDef'))]
    }else{
      glmod <- glm(app_phase_wklyDef ~ .
                  , data = all_agg2[, which(colnames(all_agg2) %in% features)]
                  , family = "gaussian")
      
      pred[i] <- predict(glmod, Final_Pred_Table[i,features])
    }
  }
  
  pred <- data.frame(pred)
  pred <- round(pred$pred,0)
  #pred <- ifelse(pred$pred < 0, 0, pred$pred)
  
  c <- c("project",	"app_type",
         "phase",	"app_category",
         "app_complexity",	"phase_app_duration",
         "test_case",	"Flag",
         "dev_team_size",	"week_Number",
         "test_team_size",	"cum_def_last_wk",
         "app_name",	"MaxWeek",
         "ProjCMMI",	"MaxEndWeek","Year","WEEK", "pred")
  
  final_pred_mat <- cbind(Final_Pred_Table[,which(colnames(Final_Pred_Table) %in% c )], pred)
  final_pred_mat$pred <- ifelse(final_pred_mat$pred<0,0,final_pred_mat$pred)
  
  # ##########################################################
  # Comparison with baseline & calibration
  # ##########################################################
  
  calibration_df1 <- merge(x = final_pred_mat, y = baseline_df4, by = c("phase",'week_Number'), all.x = TRUE)
  calibration_df1$Average = ifelse(is.na(calibration_df1$Average),0,calibration_df1$Average)
  calibration_df1$upper_lmt = ifelse(is.na(calibration_df1$upper_lmt),0,calibration_df1$upper_lmt)
  calibration_df1$lower_lmt = ifelse(is.na(calibration_df1$lower_lmt),0,calibration_df1$lower_lmt)
  
  for (i in 1:nrow(calibration_df1)) {
    if (calibration_df1$pred[i]>calibration_df1$upper_lmt[i]) {
      calibration_df1$Pred_Final[i]=calibration_df1$upper_lmt[i]
    }else{
      if (calibration_df1$pred[i]<calibration_df1$lower_lmt[i]) {
        calibration_df1$Pred_Final[i]=calibration_df1$lower_lmt[i]
      }else{
        calibration_df1$Pred_Final[i]=calibration_df1$pred[i]
      }
    }
  }
  calibration_df1$Pred_Final <- ifelse(calibration_df1$pred<calibration_df1$upper_lmt,calibration_df1$pred,calibration_df1$upper_lmt)
  calibration_df1$Pred_Final <- ifelse(calibration_df1$Pred_Final<0,0,calibration_df1$Pred_Final)
  
  # ##########################################################
  # Calculation of calender week number & calender year
  # ##########################################################
  obs_df <- calibration_df1[which(calibration_df1$week_Number<calibration_df1$MaxWeek),]

  #added by Vimal - for new MetLife project and multiple appplication  
  #when 'which' return zero records, '-which' is also returning zero records. code changed to resolve the same.
  #pred_df <- calibration_df1[-which(calibration_df1$week_Number<calibration_df1$MaxWeek),]
  pred_df <- calibration_df1[which(calibration_df1$week_Number>=calibration_df1$MaxWeek),]
  
  pred_df$combin <- paste(pred_df$ProjCMMI,pred_df$project,pred_df$app_name,pred_df$phase,sep='_')
  
  #Vimal added
  #Calendar week calculation going wrong due to unsorted file.
  pred_df  <-  pred_df %>% arrange(ProjCMMI,project,app_name,phase,week_Number)
  
  for (i in 1:nrow(pred_df)) {
    if (i==1) {
      pred_df$Calender_week[i]=pred_df$WEEK[i]
    }else{
      if (pred_df$combin[i]!=pred_df$combin[i-1]){
        pred_df$Calender_week[i]=pred_df$WEEK[i]
      }else{
        pred_df$Calender_week[i]=pred_df$Calender_week[i-1]+1
      }
    }
  }
  
  obs_df$Calender_week <- obs_df$WEEK
  pred_df <- pred_df[,-which(colnames(pred_df) %in% 'combin')]
  calibration_df1 <- rbind(obs_df,pred_df)
  
  calibration_df1$Calender_year <- ifelse(calibration_df1$Calender_week>53,calibration_df1$Year+1,calibration_df1$Year)
  calibration_df1$Calender_week <- ifelse(calibration_df1$Calender_week>53,calibration_df1$Calender_week-53,calibration_df1$Calender_week)
  
  # ##########################################################
  # Calculation of app level week number
  # ##########################################################
  
  calibration_df1 <- calibration_df1[with(calibration_df1, order(calibration_df1$ProjCMMI, calibration_df1$project 
                                                                 ,calibration_df1$app_name
                                                                 ,calibration_df1$Calender_year,calibration_df1$Calender_week)), ]
  
  calibration_df1$app_combin <- paste(calibration_df1$ProjCMMI, calibration_df1$project
                                      ,calibration_df1$app_name,sep='_')
  calibration_df1$app_combin2 <- paste(calibration_df1$ProjCMMI, calibration_df1$project
                                       ,calibration_df1$app_name,calibration_df1$Calender_year
                                       ,calibration_df1$Calender_week,sep='_')
  
  for (i in 1:nrow(calibration_df1)) {
    if (i==1) {
      calibration_df1$app_wk_no[i]=1
    }else{
      if (calibration_df1$app_combin[i]==calibration_df1$app_combin[i-1]) {
        if (calibration_df1$app_combin2[i]==calibration_df1$app_combin2[i-1]) {
          calibration_df1$app_wk_no[i]=calibration_df1$app_wk_no[i-1]
        }else{
          calibration_df1$app_wk_no[i]=calibration_df1$app_wk_no[i-1]+1
        }
      }else{
        calibration_df1$app_wk_no[i]=1
      }
    }
  }
  
  # ##########################################################
  # Current week and Actual observed defect calculation
  # ##########################################################
  c <- c("ProjCMMI",	"project",	"phase",	"app_name",	"app_type"
         ,	"app_complexity",	"test_case", "dev_team_size",	"test_team_size"
         ,	"phase_app_duration",	"Flag",	"week_Number","app_wk_no","Calender_week","Calender_year"
         ,	"MaxWeek",	"MaxEndWeek",	"Pred_Final")
  
  final_pred_mat <- calibration_df1[,c]
  final_pred_mat$actual_obs_def <- ifelse(final_pred_mat$week_Number<=final_pred_mat$MaxWeek,final_pred_mat$Pred_Final,0)
  final_pred_mat$Flag <- ifelse(final_pred_mat$week_Number<=final_pred_mat$MaxWeek,'Inprogress_Actual','Inprogress_Predicted')
  final_pred_mat_Predicted =  final_pred_mat[final_pred_mat$Flag == "Inprogress_Predicted",]
  final_pred_mat_Predicted$current_wk = 0
  
  
  # #################################################################################################################################
  # Split Pred_Final in Root-Cause & Severity                                                                                       #
  # #################################################################################################################################
  
  final_pred_mat_Predicted$Pred_Final = round(final_pred_mat_Predicted$Pred_Final*mean_shift_index)   
  
  final_pred_mat_Predicted$Select_Flag=0
  
  final_pred_mat_Predicted$Select_Flag[final_pred_mat_Predicted$Pred_Final == 0] = 1
  
  final_pred_mat__Predicted_non_zero = final_pred_mat_Predicted[final_pred_mat_Predicted$Pred_Final > 0,]
  
  if (nrow(final_pred_mat__Predicted_non_zero)>0)
  {
    final_pred_mat__Predicted_non_zero$row_num  = 1:nrow(final_pred_mat__Predicted_non_zero)
    mrg1 = merge(x=final_pred_mat__Predicted_non_zero,y=rc_agg_mrged, by = "phase")
    mrg1$count_break = mrg1$RC_Probability_by_Phase*mrg1$Pred_Final

    Cols = c("ProjCMMI","project",
           "phase","app_name",
           "app_type","app_complexity",
           "test_case","dev_team_size",
           "test_team_size","phase_app_duration",
           "Flag","week_Number","app_wk_no",
           "Calender_week","Calender_year",
           "MaxWeek","MaxEndWeek","Pred_Final",
           "actual_obs_def","current_wk",
           "Select_Flag","row_num","Severity",
           "rootCause","count_break")
  
  
    mrg1 = mrg1[,Cols]
  
  }
  
  final = final_pred_mat_Predicted[final_pred_mat_Predicted$Pred_Final == 0,]
  
  if (nrow(final)) #NATZ
  {
    final$Severity = "4-Low"
    final$row_num = 0
    final$rootCause = "Non-classified"
    #final$Count = 0
    final$count_break = 0
  }
  
  roundup = function(mrg_subset)
  {
    mrg_subset = mrg_subset[order(mrg_subset$count_break,decreasing = T),]
    mrg_subset$count_break = ceiling(mrg_subset$count_break)
    
    j=0
    
    for (i in 1:nrow(mrg_subset)) 
    { 
      
      j = j + mrg_subset$count_break[i]
      
      if( j <= mrg_subset$Pred_Final[i])
      {mrg_subset$Select_Flag[i] = 1}   
      
    }
    
    return(mrg_subset[mrg_subset$Select_Flag == 1,]) 
  }
  
  if(nrow(final_pred_mat__Predicted_non_zero)> 0 )
  {
    for (k in 1:nrow(final_pred_mat__Predicted_non_zero)) 
    {
      final = rbind(final,roundup(mrg1[mrg1$row_num == k,])) 
    }
  }    
  
  final$releaseName = 2
  
  final_Predicted = final[,c("project","app_name","app_wk_no","Severity","phase","rootCause","actual_obs_def","count_break","releaseName","current_wk")]
  names(final_Predicted) = c("projectName","application","weekNumber","severity","phase","rootCause","actualNoDefects","predictNoDefects","releaseName","currentWeek")
  
  
  return(final_Predicted)
  
}
