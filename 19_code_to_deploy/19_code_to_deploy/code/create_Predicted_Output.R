# #####################################################################
# Performing Basic Operations & Rollup @ Application-Phase Weekly level
# #####################################################################
f_predict_defect =  function(df,rc_agg_mrged,code_drops,drop_duration)
{
  Consolidated_Training_df = df
  
  Consolidated_Training_df$WeekNum <-  week(Consolidated_Training_df$openDate)
  Consolidated_Training_df$year <- year(Consolidated_Training_df$openDate)
  Consolidated_Training_df$Freq <-  1
  
  #Vimal change  
  Consolidated_Training_df = Consolidated_Training_df[order(Consolidated_Training_df$Flag, Consolidated_Training_df$projectName, Consolidated_Training_df$application,Consolidated_Training_df$phase,Consolidated_Training_df$openDate),]
  
  #--------------------------------------------------------------------------------------------------------------------------
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
  all_agg1 <- all_agg[with(all_agg, order(all_agg$Flag,all_agg$ProjCMMI, all_agg$project 
                                          ,all_agg$phase,all_agg$app_name
                                          ,all_agg$Year,all_agg$WEEK,all_agg$week_Number)), ]
  
  # Generating Week Number
  
  all_agg1$combin <- paste( all_agg1$Flag,all_agg1$ProjCMMI,all_agg1$project
                            ,all_agg1$phase,all_agg1$app_name
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

  all_agg1_base <- all_agg1 %>% filter (Flag == "Base")
  
  proj_phase_det <- all_agg1 %>% filter (Flag == "Base") %>% group_by(app_name,phase) %>% summarize(max_def = max(app_phase_wklyDef))
  
  #Get the 5% cut-off value.
  proj_phase_det$min_cut <- round(.05 * proj_phase_det$max_def)
  
  #filt_def_gt_1 <- proj_phase_det %>% filter(max_def > 1)
  
  
  agg_min_max <- merge(all_agg1_base, proj_phase_det)
  agg_min_max <- agg_min_max %>% arrange(ProjCMMI,project,phase,app_name,Year,WEEK,week_Number)
  
  #Mark the records greater than cut-off value or not.
  agg_min_max$gt_cut <-  ifelse(agg_min_max$app_phase_wklyDef > agg_min_max$min_cut, 1, 0)
  
  #Filter the records greater than the cut-off value.
  agg_filt <- agg_min_max %>% filter(gt_cut == 1)
  
  #Renumber the weeks for the current dataset.
  for(i in 1:nrow(agg_filt)){
    if(i==1){
      agg_filt$week_Number_c[i]=1
    }else{
      if(agg_filt$combin[i]!=agg_filt$combin[i-1]){
        agg_filt$week_Number_c[i]=1
      }else{
        if((agg_filt$week_Number[i] - agg_filt$week_Number[i-1]) > 1){
          agg_filt$week_Number_c[i]=1}else{
            agg_filt$week_Number_c[i] <- agg_filt$week_Number_c[i-1] + 1
          }
      }
    }
  }
  
  #Identify the cycles related in the dataset.
  for(i in 1:nrow(agg_filt)){
    if(i==1){
      agg_filt$cycle_c[i]=1
    }else{
      if(agg_filt$combin[i]!=agg_filt$combin[i-1]){
        agg_filt$cycle_c[i]=1
      }else{
        if((agg_filt$week_Number_c[i] - agg_filt$week_Number_c[i-1]) > 0){
          agg_filt$cycle_c[i]=agg_filt$cycle_c[i-1]}else{
            agg_filt$cycle_c[i] <- agg_filt$cycle_c[i-1] + 1
          }
      }
    }
  }
  
  #Remove the cycles having 1 week duration
  agg_rem_1_wk_dur <- agg_filt %>% group_by(phase, app_name, cycle_c) %>% summarize(max_dur_cyc = max(week_Number_c))
  
  agg_filt_w_max_wk_dur <- merge(agg_filt, agg_rem_1_wk_dur)
  
  agg_filt_rm_1_dur <- agg_filt_w_max_wk_dur %>% filter(max_dur_cyc > 1)
  
  agg_filt_sel <- agg_filt_rm_1_dur %>% select(phase, app_name, ProjCMMI,  project,  app_type,  app_complexity,  phase_app_duration, 
                                               test_case,  dev_team_size,  test_team_size, Flag, Year, WEEK, app_phase_wklyDef,
                                               combin, week_Number_c, cycle_c)
  
  agg_cyc_renum <- agg_filt_sel
  
  #Renumber the new cycles identified.
  for(i in 1:nrow(agg_cyc_renum)){
    if(i==1){
      agg_cyc_renum$cycle_c2[i]=1
    }else{
      if(agg_cyc_renum$combin[i]!=agg_cyc_renum$combin[i-1]){
        agg_cyc_renum$cycle_c2[i]=1
      }else{
        if((agg_cyc_renum$cycle_c[i] - agg_cyc_renum$cycle_c[i-1]) == 0){
          agg_cyc_renum$cycle_c2[i]=agg_cyc_renum$cycle_c2[i-1]}else{
            agg_cyc_renum$cycle_c2[i] <- agg_cyc_renum$cycle_c2[i-1] + 1
          }
      }
    }
  }
  
  #Remove the Release projects from the SIT phase.
  filt_rel_neg <-   agg_cyc_renum %>%  filter(!(phase == "System Integration Test (SIT)"  & grepl("Release",app_name)))
  
  filt_rel_neg$cycle_c <- NULL
  
  filt_rel_neg <- setnames(filt_rel_neg, old = c("week_Number_c", "cycle_c2"), 
                           new = c("week_Number", "cycle"))
  
  agg_2_base <- filt_rel_neg
  #--------------------------------------------------------------------------------------------------------------------
  
  all_agg1_Inp <- all_agg1 %>% filter (Flag == "Inprogress")
  #all_agg1_Inp <- all_agg1_Inp %>% select (-c(Year, WEEK, combin))
  all_agg1_Inp <- all_agg1_Inp %>% select (-c(Year, WEEK))
  
  agg_2_base <- agg_2_base %>% select("ProjCMMI", "project","phase", "app_type", "app_name",
                                      "app_complexity", "phase_app_duration", "test_case", "dev_team_size", 
                                      "test_team_size", "Flag", "week_Number", "cycle", "app_phase_wklyDef", "combin" )
  
  agg_2_base_df <- as.data.frame(agg_2_base)
  #class(agg_2_base_df)
  
  wk_max_pr_ph_cy <- agg_2_base_df %>% group_by(app_name, phase,cycle) %>% summarize(max_week = max(week_Number))
  tot_wk_pr_ph <- wk_max_pr_ph_cy %>% group_by(app_name, phase) %>% summarize(tot_week = sum(max_week))
  
  agg_2_base_df <- merge(agg_2_base_df,tot_wk_pr_ph)
  #agg_2_base_df$der_var3 <- agg_2_base_df$tot_week * agg_2_base_df$cycle * agg_2_base_df$week_Number
  agg_2_base_df$der_var3 <- agg_2_base_df$tot_week * agg_2_base_df$week_Number
  
  code_drops_num <- code_drops
  cycles <- drop_duration
  
  #Read the duration of each code drop.
  cycles <- unlist(strsplit(cycles, split=","))
  length(cycles)
  cycles <- as.numeric(cycles)
  
  if (length(cycles) != code_drops_num){
    msg = paste(Sys.time(),"Number of code drops is not matching with the cycle details provided")
    print(msg)
    write(msg,file="IDP_Log.txt",append=TRUE)
    stop("Execution Stopped: Please check the Log")
  }
  
  df_cycle_n_dur = data.frame(test_cycle = numeric(0),
                              dur = numeric(0),
                              tot_dur = numeric(0),
                              stringsAsFactors=FALSE)
  temp <- 0
  
  # Table with cycle number, duration and cumulative duration
  for(i in 1:length(cycles)){
    temp <- temp + cycles[i]
    df_cycle_n_dur[nrow(df_cycle_n_dur)+1, ] <- c(i,cycles[i],temp)
  }
  
  tot_duration <- sum(df_cycle_n_dur$dur)
  all_agg1_Inp$tot_week <- tot_duration
  
  #Generating the cycle number for Inprogress data
  for (i in 1:(nrow(all_agg1_Inp)))
  {
    chk_flag <- "N"
    for (j in 1:(nrow(df_cycle_n_dur)))
      if ((all_agg1_Inp$week_Number[i] <= df_cycle_n_dur$tot_dur[j]) & chk_flag == "N")
      {
        all_agg1_Inp$cycle[i] = df_cycle_n_dur$test_cycle[j]
        chk_flag <- "Y"
        #print("i is ", i, "j is ", j)
      }
  }
  
  #all_agg1_Inp$der_var3 <- all_agg1_Inp$tot_week * all_agg1_Inp$cycle * all_agg1_Inp$week_Number
  all_agg1_Inp$der_var3 <- all_agg1_Inp$tot_week * all_agg1_Inp$week_Number
  
  #Generating week_Number for all cycles in the in-progress data
  all_agg1_Inp$combin_cyc <- paste( all_agg1_Inp$combin,all_agg1_Inp$cycle
                                    , sep = "_") 
  
  for(i in 1:nrow(all_agg1_Inp)){
    if(i==1){
      all_agg1_Inp$week_Number[i]=1
    }else{
      if(all_agg1_Inp$combin_cyc[i]!=all_agg1_Inp$combin_cyc[i-1]){
        all_agg1_Inp$week_Number[i]=1
      }else{
        all_agg1_Inp$week_Number[i]=all_agg1_Inp$week_Number[i-1]+1
      }
    }
  }
  
  all_agg1_Inp$combin_cyc <- NULL
  
  inp_cyc_cum_def <- all_agg1_Inp %>% group_by(cycle) %>% summarize(inp_cum_def = sum(app_phase_wklyDef))
  inp_curr_cy <- all_agg1_Inp %>% filter(cycle == max(cycle))
  
  #Creating the week_Number for the current cycle
  for(i in 1:nrow(inp_curr_cy)){
    if(i==1){
      inp_curr_cy$week_Number[i]=1
    }else{
      if(inp_curr_cy$combin[i]!=inp_curr_cy$combin[i-1]){
        inp_curr_cy$week_Number[i]=1
      }else{
        inp_curr_cy$week_Number[i]=inp_curr_cy$week_Number[i-1]+1
      }
    }
  }
  
  current_cycle <- max(inp_curr_cy$cycle)
  current_cyc_wk <- max(inp_curr_cy$week_Number) + 1
  
  df_cyc_for_pred <-  data.frame(pred_cycle = numeric(0),
                                 start_week = numeric(0),
                                 end_week = numeric(0),
                                 stringsAsFactors=FALSE)
  
  if (current_cyc_wk > df_cycle_n_dur$dur[df_cycle_n_dur$test_cycle == current_cycle]){
    current_cycle <- current_cycle + 1
    current_cyc_wk <-  1
  }
  
  #Identifying start and end weeks for current cycle
  df_cyc_for_pred[nrow(df_cyc_for_pred)+1, ] <- c(current_cycle,current_cyc_wk, df_cycle_n_dur$dur[df_cycle_n_dur$test_cycle == current_cycle])
  
  #Identifying start and end weeks for future cycles
  if ((length(cycles) > 1) & (current_cycle < length(cycles))) {
    for(i in (current_cycle + 1):length(cycles)){
      df_cyc_for_pred[nrow(df_cyc_for_pred)+1, ] <- c(i,1,df_cycle_n_dur$dur[i])
    }
  }
  
  #quantile(all_agg1$app_phase_wklyDef,c(0.25,0.5,0.68,0.75,0.8,0.82,0.90,.95,0.98, 0.99,1))
  #out_val <- quantile(all_agg1$app_phase_wklyDef,c(0.97))
  #all_agg1$app_phase_wklyDef <- ifelse(all_agg1$app_phase_wklyDef > out_val, out_val, all_agg1$app_phase_wklyDef)
  
  # ##################################################################
  # Calculating Total Cumulative Defect till last week
  # ##################################################################
  
  all_agg2 <- rbind(agg_2_base_df, all_agg1_Inp)
  
  all_agg2$combin <- paste(all_agg2$phase,all_agg2$app_name,all_agg2$Flag, sep = "_")
  
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
  
  # ####################################################################
  # Calculating Total Cumulative Defect till last week for current cycle
  # ####################################################################
  
  all_agg2$combin2 <- paste(all_agg2$phase,all_agg2$app_name,all_agg2$Flag,all_agg2$cycle, sep = "_")
  
  for (i in 1:nrow(all_agg2)) {
    if(i==1){
      all_agg2$cum_def_last_wk_cyc[i]=0
    }else{
      if(all_agg2$combin2[i]==all_agg2$combin2[i-1]){
        all_agg2$cum_def_last_wk_cyc[i]=all_agg2$app_phase_wklyDef[i-1]+all_agg2$cum_def_last_wk_cyc[i-1]
      }else{
        all_agg2$cum_def_last_wk_cyc[i]=0
      }
    }
  }
  
  # ####################################################################
  # Calculating last 2 weeks Moving average for current cycle
  # ####################################################################
  
  for (i in 1:nrow(all_agg2)) {
    if ((i == 1) | (i ==2)) {
      all_agg2$m_avg_2_weeks[i]=0
    }else{
      if((all_agg2$combin2[i]==all_agg2$combin2[i-1]) & (all_agg2$combin2[i]==all_agg2$combin2[i-2])){
        all_agg2$m_avg_2_weeks[i]=round((all_agg2$app_phase_wklyDef[i-1]+all_agg2$app_phase_wklyDef[i-2])/2)
      }else{
        all_agg2$m_avg_2_weeks[i]=0
      }
    }
  }
  
  # ####################################################################
  # Calculating MA_by_prev_week_Def
  # ####################################################################
  
  for (i in 1:nrow(all_agg2)) {
    if((i == 1) | (i ==2)){
      all_agg2$MA_by_prev_week_Def[i] <- 0
    }else{
      if((all_agg2$combin2[i] == all_agg2$combin2[i-1]) & (all_agg2$combin2[i] == all_agg2$combin2[i-2]) & (all_agg2$m_avg_2_weeks[i-1] != 0)){
          all_agg2$MA_by_prev_week_Def[i] <-  (all_agg2$m_avg_2_weeks[i-1] - all_agg2$app_phase_wklyDef[i-1])/all_agg2$m_avg_2_weeks[i-1]
      }else{
        all_agg2$MA_by_prev_week_Def[i] <- 0
      }
    }
  }
  
  # #######################################################################
  # Calculating Total Cumulative Defect till last 2 weeks for current cycle
  # #######################################################################
  
  for (i in 1:nrow(all_agg2)) {
    if((i==1) | (i==2)){
      all_agg2$cum_def_last_2wk_cyc[i]=0
    }else{
      if((all_agg2$combin2[i]==all_agg2$combin2[i-1]) & (all_agg2$combin2[i]==all_agg2$combin2[i-2])){
        all_agg2$cum_def_last_2wk_cyc[i]= all_agg2$cum_def_last_wk_cyc[i-1]
      }else{
        all_agg2$cum_def_last_2wk_cyc[i]=0
      }
    }
  }
  
  # #######################################################################
  # Assigning weight to defects based on weekNumber
  # #######################################################################
  
  
  for (i in 1:nrow(all_agg2)) {
    if(i==1){
      all_agg2$der_var2[i]=0
    }else{
      if(all_agg2$combin2[i]==all_agg2$combin2[i-1]){
        all_agg2$der_var2[i] <- all_agg2$app_phase_wklyDef[i-1] * all_agg2$week_Number[i-1]
        #all_agg2$der_var2[i] <- all_agg2$app_phase_wklyDef[i-1] 
      }else{
        all_agg2$der_var2[i] <- 0
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
  
  # Check if the inprogress data belongs to a different population by means of volume
  
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
  
  #inp_curr_cycle <-  all_agg2 %>% filter (Flag == "Inprogress" & cycle == curr_cycle)
  inp_curr_cycle_calc <-  all_agg2 %>% filter (Flag == "Inprogress")

  #inp_curr_cycle_max_week <- inp_curr_cycle_calc %>% filter((cycle == max(cycle)) & (week_Number == max(week_Number)))
  #broken to 2 separate filters
  inp_curr_max_cycle <- inp_curr_cycle_calc %>% filter(cycle == max(cycle))
  inp_curr_cycle_max_week <- inp_curr_max_cycle %>% filter(week_Number == max(week_Number))
  
  # This flags are considered while calculation of moving average
  inp_max_cycle <- max(inp_curr_max_cycle$cycle)
  two_wk_flag <- ifelse(nrow(inp_curr_max_cycle) >=2,1,0)
  
  cum_def <- inp_curr_cycle_calc %>% group_by(cycle) %>% summarize(cum_def_cycle = sum(app_phase_wklyDef))
  cum_def$cum_def_till_last_cycle <- 0
  
  # last_week_defect <-
  #   inp_curr_cycle %>% filter (cycle == max(cycle) &
  #                                week_Number == max(week_Number)) %>% summarize(lst_week_defect = app_phase_wklyDef)
  
  temp_cum_def <- 0
  
  if (nrow(cum_def) > 1){
    for(i in 1:nrow(cum_def)){
      cum_def$cum_def_till_last_cycle[i] <- temp_cum_def
      temp_cum_def <- temp_cum_def + cum_def$cum_def_cycle[i]
    }
  }
  
  cnt <- 0
  flag_cnt <- 0
  
  for(cycle_pred in 1:nrow(df_cyc_for_pred)){
    cnt <- cnt + 1
    curr_cycle <- df_cyc_for_pred$pred_cycle[cycle_pred]
    start_week <- df_cyc_for_pred$start_week[cycle_pred]
    end_week <- df_cyc_for_pred$end_week[cycle_pred]
    
    base_curr_cycle <- all_agg2 %>% filter (Flag == "Base" & cycle == curr_cycle)
    inp_curr_cycle <-  all_agg2 %>% filter (Flag == "Inprogress" & cycle == curr_cycle)
    
    baseline_df1 <- rbind(base_curr_cycle, inp_curr_cycle)
    train_data <- rbind(base_curr_cycle, inp_curr_cycle)

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
    
    current_week <- inp_curr_cycle_max_week
    
    for (i in 1:nrow(current_week)) {
      for (j in start_week: end_week) {
        
        tmp <- current_week[i,]
        tmp$week_Number <- j
        current_week <- rbind(current_week, tmp)
      }
    }
    
    current_week <- current_week[-c(1),]
    
    if ((start_week == 1) & (cnt == 1)){
      current_week$cum_def_last_wk[1] <- current_week$cum_def_last_wk[1] + current_week$app_phase_wklyDef[1]
      current_week$cum_def_last_wk_cyc <- 0
      
      cum_def[nrow(cum_def)+1, ] <- c(nrow(cum_def)+1,0,current_week$cum_def_last_wk[1])
      
    }else{
      for (cycle_num in 1:nrow(cum_def)){
        if (cum_def$cycle[cycle_num] == curr_cycle){
          current_week$cum_def_last_wk[1] <- cum_def$cum_def_till_last_cycle[cycle_num] + cum_def$cum_def_cycle[cycle_num]
          current_week$cum_def_last_wk_cyc[1] <- cum_def$cum_def_cycle[cycle_num]
        }
      }}
    
    if ((inp_max_cycle == curr_cycle) & (two_wk_flag == 1)){
      rec_cnt <- nrow(inp_curr_max_cycle)
      current_week$m_avg_2_weeks <-  round((inp_curr_max_cycle[rec_cnt,'app_phase_wklyDef'] + inp_curr_max_cycle[(rec_cnt - 1),'app_phase_wklyDef'])/2)
      current_week$cum_def_last_2wk_cyc <- inp_curr_max_cycle[rec_cnt,'cum_def_last_wk_cyc']
      
      #This needs 3 previous weeks' data. But div by zero will not happen even if we have 2 previous weeks' data.
      current_week$MA_by_prev_week_Def <-
        (inp_curr_max_cycle[rec_cnt, 'm_avg_2_weeks'] - inp_curr_max_cycle[rec_cnt, 'app_phase_wklyDef']) /
        inp_curr_max_cycle[rec_cnt, 'app_phase_wklyDef']
    }else{
      current_week$m_avg_2_weeks <- 0
      current_week$MA_by_prev_week_Def <- 0
      current_week$cum_def_last_2wk_cyc <- inp_curr_max_cycle[rec_cnt,'cum_def_last_wk_cyc']
    }
    

    #current_week$der_var3 <- current_week$tot_week * current_week$cycle * current_week$week_Number    
    current_week$der_var3 <- current_week$tot_week * current_week$week_Number    
    
    #current_week$cum_def_last_2wk_cyc[1] <- current_week$cum_def_last_wk_cyc[1]
    
    current_week$MaxWeek <- start_week - 1
    current_week$MaxEndWeek <- end_week
    Final_Pred_Table <- current_week
    
    # Creating Final Prediction Table with 'cum_def_last_wk'
    
    Final_Pred_Table <- Final_Pred_Table[with(Final_Pred_Table, order(Final_Pred_Table$project
                                                                      ,Final_Pred_Table$phase
                                                                      ,Final_Pred_Table$app_name)), ]
    
    Final_Pred_Table$cum_def_last_wk_old <- Final_Pred_Table$cum_def_last_wk
    
    features_cycle_1 <-  c(
      'app_phase_wklyDef'
      , 'week_Number'   #sig
     #, 'cum_def_last_wk'  #sig
      , 'cum_def_last_wk_cyc'
     #,'m_avg_2_weeks'
      , 'cum_def_last_wk_cyc_sq' #sig
      , 'der_var1'
     #, 'tot_week'
     #, 'der_var2'
     #, 'der_var3'
     # , 'perc_change'
     # , 'MA_by_prev_week_Def'
     #, 'week_n_avg'
      ,'dummyPhase_Employee Pilot (EP)'
      ,'dummyPhase_Live Proving (LP)')  #sig
     #,'dummyPhase_Warranty'
     #,'dummyPhase_System Test (ST)')
     
    
    features_cycle_2 <-  c(
      'app_phase_wklyDef'
      , 'week_Number'
      , 'cum_def_last_wk' #sig
     #, 'tot_week'
     #, 'cum_def_last_wk_cyc'
     #,'m_avg_2_weeks'
     #,'cum_def_last_wk_cyc_sq' #sig
     #, 'der_var1'
     #, 'der_var2'
      , 'der_var3'
     #, 'perc_change'
     #,'MA_by_prev_week_Def'
      ,'dummyPhase_Business Acceptance Test (BAT)') #sig
     #,'dummyPhase_DDC System Integration Test (SIT)'
     #,'dummyPhase_Warranty'
     #,'dummyPhase_System Test (ST)')
    
    features_cycle_3 <-  c(
      'app_phase_wklyDef'
      , 'week_Number'                #sig
      , 'cum_def_last_wk'            #sig
      , 'cum_def_last_wk_cyc'        #sig
      ,'dummyPhase_Business Acceptance Test (BAT)' #sig
      ,'dummyPhase_Dev Testing (DT)')  #sig
     #,'dummyPhase_System Test (ST)')
     #,'dummyPhase_System Integration Test (SIT)')
    
    features_cycle_4 <-  c(
      'app_phase_wklyDef'
      , 'cum_def_last_wk')
    
    if (curr_cycle == 1){
      train_data$cum_def_last_wk_cyc_sq <- train_data$cum_def_last_wk_cyc^2
      
      # for (i in 1:nrow(train_data)){
      #   if (train_data$week_Number[i] - 1 != 0){
      #     train_data$der_var1[i] <- train_data$cum_def_last_wk_cyc[i]/(train_data$week_Number[i] - 1)
      #     }else{
      #       train_data$der_var1[i] <- 0
      #     }
      #   }
      
      train_data$der_var1 <- train_data$cum_def_last_wk_cyc/train_data$week_Number
      
      train_data$week_n_avg <- train_data$week_Number * train_data$m_avg_2_weeks
      
      for(i in 1:nrow(train_data)){
      if (train_data$cum_def_last_2wk_cyc[i] !=0){
        train_data$perc_change[i] <- ((train_data$cum_def_last_wk_cyc[i] - train_data$cum_def_last_2wk_cyc[i])/train_data$cum_def_last_2wk_cyc[i]) * 100
      }else{
        train_data$perc_change[i] <- 0
      }
      }
      Final_Pred_Table$cum_def_last_wk_cyc_sq <- Final_Pred_Table$cum_def_last_wk_cyc^2
      
      # Final_Pred_Table$der_var1 <- Final_Pred_Table$cum_def_last_wk_cyc/Final_Pred_Table$week_Number
      # for (i in 1:nrow(Final_Pred_Table)){
      #   if ((Final_Pred_Table$week_Number[i] - 1) != 0){
      #     Final_Pred_Table$der_var1[i] <- Final_Pred_Table$cum_def_last_wk_cyc[i]/(Final_Pred_Table$week_Number[i] - 1)
      #     }else{
      #       Final_Pred_Table$der_var1[i] <- 0
      #     }
      #   }
      
      Final_Pred_Table$der_var1 <- Final_Pred_Table$cum_def_last_wk_cyc/Final_Pred_Table$week_Number
      
      for (i in 1:nrow(Final_Pred_Table)){
        if (Final_Pred_Table$week_Number[i] == 1){
          Final_Pred_Table$der_var2[i] <- 0
          }else{
            Final_Pred_Table$der_var2[i] <- Final_Pred_Table$app_phase_wklyDef[i] * Final_Pred_Table$week_Number[i]
            #Final_Pred_Table$der_var2[i] <- Final_Pred_Table$app_phase_wklyDef[i]
          }
        }
      
      
      if (Final_Pred_Table[1, 'cum_def_last_2wk_cyc'] != 0) {
        Final_Pred_Table[1, 'perc_change'] <-
         ((Final_Pred_Table[1, 'cum_def_last_wk_cyc'] - Final_Pred_Table[1, 'cum_def_last_2wk_cyc']) /
          Final_Pred_Table[1, 'cum_def_last_2wk_cyc']) * 100
      } else{
        Final_Pred_Table[1, 'perc_change'] <- 0
      }
      
      #Final_Pred_Table$week_n_avg <- Final_Pred_Table$week_Number * Final_Pred_Table$m_avg_2_weeks
      features <- features_cycle_1
    }else if(curr_cycle == 2){
        train_data$cum_def_last_wk_cyc_sq <- train_data$cum_def_last_wk_cyc^2
        Final_Pred_Table$cum_def_last_wk_cyc_sq <- Final_Pred_Table$cum_def_last_wk_cyc^2
       # 
       train_data$der_var1 <- train_data$cum_def_last_wk_cyc/train_data$week_Number
       Final_Pred_Table$der_var1 <- Final_Pred_Table$cum_def_last_wk_cyc/Final_Pred_Table$week_Number
       
       for (i in 1:nrow(Final_Pred_Table)){
         if (Final_Pred_Table$week_Number[i] == 1){
           Final_Pred_Table$der_var2[i] <- 0
         }else{
           Final_Pred_Table$der_var2[i] <- Final_Pred_Table$app_phase_wklyDef[i] * Final_Pred_Table$week_Number[i]
           #Final_Pred_Table$der_var2[i] <- Final_Pred_Table$app_phase_wklyDef[i]
         }
       }
       
       for(i in 1:nrow(train_data)){
         if (train_data$cum_def_last_2wk_cyc[i] !=0){
           train_data$perc_change[i] <- ((train_data$cum_def_last_wk_cyc[i] - train_data$cum_def_last_2wk_cyc[i])/train_data$cum_def_last_2wk_cyc[i]) * 100
         }else{
           train_data$perc_change[i] <- 0
         }
       }

       if (Final_Pred_Table[1, 'cum_def_last_2wk_cyc'] != 0) {
         Final_Pred_Table[1, 'perc_change'] <-
           ((Final_Pred_Table[1, 'cum_def_last_wk_cyc'] - Final_Pred_Table[1, 'cum_def_last_2wk_cyc']) /
              Final_Pred_Table[1, 'cum_def_last_2wk_cyc']) * 100
       } else{
         Final_Pred_Table[1, 'perc_change'] <- 0
       }

       features <- features_cycle_2
    }else if(curr_cycle == 3){
      features <- features_cycle_3
    }else if(curr_cycle ==4){
      features <- features_cycle_4
    }
        
    # features <-  c(
    #   'app_phase_wklyDef'
    #   , 'week_Number'
    #   , 'cum_def_last_wk'
    #   , 'cum_def_last_wk_cyc'
    #   , 'm_avg_2_weeks'
    #   ,'dummyPhase_Business Acceptance Test (BAT)'
    #   ,'dummyPhase_DDC System Integration Test (SIT)'
    #   ,'dummyPhase_Design Verification (DV)'
    #   ,'dummyPhase_Dev Testing (DT)'
    #   ,'dummyPhase_Employee Pilot (EP)'
    #   ,'dummyPhase_Employee Pilot SO (SOEP)'
    #   ,'dummyPhase_Live Proving (LP)'
    #   ,'dummyPhase_Performance Test (PT)'
    #   ,'dummyPhase_Security Test (SEC)'
    #   ,'dummyPhase_Static Testing (STT)'
    #   ,'dummyPhase_System Integration Test (SIT)'
    #   ,'dummyPhase_System Non Functional Test (SNFT)'
    #   #,'dummyPhase_Warranty'
    #   ,'dummyPhase_System Test (ST)')

    pred <-  c()
    for(i in 1:nrow(Final_Pred_Table)){
      if(i > 1){
        if (Final_Pred_Table$combin[i]==Final_Pred_Table$combin[i-1]) {
          Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk'))] <- 
            Final_Pred_Table[i-1, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk'))] + pred[i - 1]
          Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk_cyc'))] <- 
            Final_Pred_Table[i-1, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk_cyc'))] + pred[i - 1]
          
          Final_Pred_Table[i,'cum_def_last_wk_cyc_sq'] <- Final_Pred_Table[i,'cum_def_last_wk_cyc']^2
          
          # if (Final_Pred_Table[i,'week_Number'] - 1 !=0){
          #   Final_Pred_Table[i,'der_var1'] <- Final_Pred_Table[i,'cum_def_last_wk']/(Final_Pred_Table[i,'week_Number'] - 1)
          # }else{
          #   Final_Pred_Table[i,'der_var1'] <- 0
          # }
          Final_Pred_Table[i,'der_var1'] <- Final_Pred_Table[i,'cum_def_last_wk']/Final_Pred_Table[i,'week_Number']
          Final_Pred_Table[i,'der_var2'] <- pred[i-1] * Final_Pred_Table[(i-1), 'week_Number']   
          #Final_Pred_Table[i,'der_var2'] <- pred[i-1]
          
        }else{
          Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk'))] <- 
            Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk_old'))] 
          Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk_cyc'))] <- 
            Final_Pred_Table[i, which(colnames(Final_Pred_Table) %in% c('cum_def_last_wk_old'))] #check if this has to be changed to 
          #new variable
        }
      }
      
      if(i == 2){
        Final_Pred_Table[i, 'm_avg_2_weeks'] <- round((pred[i-1] + inp_curr_max_cycle[rec_cnt,'app_phase_wklyDef'])/2)
      }

      if(i > 2){
          Final_Pred_Table[i, 'm_avg_2_weeks'] <- round((pred[i-1]  + pred[i-2])/2)
      }

      Final_Pred_Table[i,'week_n_avg'] <- Final_Pred_Table[i,'week_Number'] * Final_Pred_Table[i, 'm_avg_2_weeks']

      
      
      
      # if(i == 2){
      #   
      #   Final_Pred_Table[i, 'perc_change'] <- ((Final_Pred_Table[i, 'cum_def_last_wk_cyc'] - inp_curr_max_cycle[rec_cnt,'cum_def_last_wk_cyc'])/inp_curr_max_cycle[rec_cnt,'cum_def_last_wk_cyc']) * 100
      # }
      
      if(i >= 2){
        Final_Pred_Table[i, 'cum_def_last_2wk_cyc'] <- Final_Pred_Table[i-1, 'cum_def_last_wk_cyc'] 
        Final_Pred_Table[i, 'perc_change'] <- ((Final_Pred_Table[i, 'cum_def_last_wk_cyc'] - Final_Pred_Table[i, 'cum_def_last_2wk_cyc'])/Final_Pred_Table[i, 'cum_def_last_2wk_cyc']) * 100
        Final_Pred_Table[i, 'MA_by_prev_week_Def'] <- ((Final_Pred_Table[i-1, 'm_avg_2_weeks'] - pred[i-1])/Final_Pred_Table[i-1, 'm_avg_2_weeks'])
      }

      Final_Pred_Table[i,'week_n_avg'] <- Final_Pred_Table[i,'week_Number'] * Final_Pred_Table[i, 'm_avg_2_weeks']

      
      glmod <- glm(app_phase_wklyDef ~ . 
                   , data = train_data[, which(colnames(train_data) %in% features)]
                   , family = "gaussian")
      

      
      pred[i] <- predict(glmod, Final_Pred_Table[i,features])
      #    }
    }

    # lmod <- lm(app_phase_wklyDef ~ . 
    #              , data = train_data[, which(colnames(train_data) %in% features)])
    # 
    # a <- summary(lmod)
    # print(a)        
        
    pred <- data.frame(pred)
    pred <- round(pred$pred,0)
    #pred <- ifelse(pred$pred < 0, 0, pred$pred)
    
    c <- c("project",	"app_type",
           "phase",	"app_category",
           "app_complexity",	"phase_app_duration",
           "test_case",	"Flag",
           "dev_team_size",	"week_Number",
           "test_team_size",	"cum_def_last_wk", "cum_def_last_wk_cyc", #newly added
           "app_name",	"MaxWeek",
           "ProjCMMI",	"MaxEndWeek","Year","WEEK", "pred")
    
    final_pred_mat <- cbind(Final_Pred_Table[,which(colnames(Final_Pred_Table) %in% c )], pred)
    final_pred_mat$pred <- ifelse(final_pred_mat$pred<0,0,final_pred_mat$pred)
    
    #if ((curr_cycle == 1)| (curr_cycle == 2)) {
    if (curr_cycle == 1) {
      train_data$cum_def_last_wk_cyc_sq <- NULL
      #Final_Pred_Table$cum_def_last_wk_cyc_sq <- Final_Pred_Table$cum_def_last_wk_cyc^2
      Final_Pred_Table$cum_def_last_wk_cyc_sq <- NULL
      train_data$der_var1 <- NULL
      Final_Pred_Table$der_var1 <- NULL
      train_data$week_n_avg <- NULL
      Final_Pred_Table$week_n_avg <- NULL
      }
    
    # ##########################################################
    # Comparison with baseline & calibration
    # ##########################################################
    
    calibration_df1 <- merge(x = final_pred_mat, y = baseline_df4, by = c("phase",'week_Number'), all.x = TRUE)
    calibration_df1$Average = ifelse(is.na(calibration_df1$Average),0,calibration_df1$Average)
    #calibration_df1$upper_lmt = ifelse(is.na(calibration_df1$upper_lmt),0,calibration_df1$upper_lmt)
    calibration_df1$upper_lmt = ifelse(is.na(calibration_df1$upper_lmt),min(calibration_df1$upper_lmt, na.rm =  TRUE),calibration_df1$upper_lmt)
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
    #pred_df <- calibration_df1[-which(calibration_df1$week_Number<calibration_df1$MaxWeek),]
    pred_df <- calibration_df1[which(calibration_df1$week_Number>=calibration_df1$MaxWeek),]
    
    pred_df$combin <- paste(pred_df$ProjCMMI,pred_df$project,pred_df$app_name,pred_df$phase,sep='_')
    
    #Vimal added
    pred_df  <-  pred_df %>% arrange(ProjCMMI,project,app_name,phase,week_Number)
    
    
    obs_df$Calender_week <- obs_df$WEEK
    pred_df <- pred_df[,-which(colnames(pred_df) %in% 'combin')]
    
    #calibration_df1 <- rbind(obs_df,pred_df)
    calibration_df1 <- pred_df
    
    calibration_df1$app_wk_no <- calibration_df1$week_Number
    
    #Re-calculating the cum_defect after finalizing the prediction
    for (cal_num in 2:nrow(calibration_df1)){
      calibration_df1$cum_def_last_wk[cal_num] <- calibration_df1$cum_def_last_wk[cal_num - 1] + calibration_df1$Pred_Final[cal_num-1]
      calibration_df1$cum_def_last_wk_cyc[cal_num] <- calibration_df1$cum_def_last_wk_cyc[cal_num - 1] + calibration_df1$Pred_Final[cal_num-1]
    }
    
    calibration_df1_last_rec <- calibration_df1 %>% filter(app_wk_no == max(app_wk_no))
    cum_def_curr_cycle <- calibration_df1_last_rec$cum_def_last_wk_cyc + calibration_df1_last_rec$Pred_Final
    
    #Updating the cumulative defect for the current cycle
    cum_def$cum_def_cycle[cum_def$cycle == max(cum_def$cycle)] <- cum_def_curr_cycle
    
    #Updating the cumulative defect for the next cycle
    cum_def[nrow(cum_def)+1, ] <- c(nrow(cum_def)+1,0,0)
    next_cycle_cum_def_till_last_cycle <- cum_def %>% filter(cycle !=max(cycle)) %>% summarize(cum_sum = sum(cum_def_cycle))
    cum_def$cum_def_till_last_cycle[cum_def$cycle == max(cum_def$cycle)] <- next_cycle_cum_def_till_last_cycle$cum_sum
    
    #View(calibration_df1[,c(28,23)])
    
    # ##########################################################
    # Current week and Actual observed defect calculation
    # ##########################################################
    
    #Vimal commented for project cycle change
    # c <- c("ProjCMMI",	"project",	"phase",	"app_name",	"app_type"
    #        ,	"app_complexity",	"test_case", "dev_team_size",	"test_team_size"
    #        ,	"phase_app_duration",	"Flag",	"week_Number","app_wk_no","Calender_week","Calender_year"
    #        ,	"MaxWeek",	"MaxEndWeek",	"Pred_Final")
    
    c <- c("ProjCMMI",	"project",	"phase",	"app_name",	"app_type"
           ,	"app_complexity",	"test_case", "dev_team_size",	"test_team_size"
           ,	"phase_app_duration",	"Flag",	"week_Number","app_wk_no" #"Calender_week","Calender_year"
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
               #"Calender_week","Calender_year",
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
    
    cycle_Predicted <- final[,c("project","app_name","app_wk_no","Severity","phase","rootCause","actual_obs_def","count_break","releaseName","current_wk")]
    cycle_Predicted$cycle <- curr_cycle
    
    if (cnt == 1){
      final_Predicted <- cycle_Predicted
    }else{
      final_Predicted <- rbind(final_Predicted, cycle_Predicted)
    }
    
    
    #final_Predicted = final[,c("project","app_name","app_wk_no","Severity","phase","rootCause","actual_obs_def","count_break","releaseName","current_wk","cycle")]
    
  }
  
  names(final_Predicted) = c("projectName","application","weekNumber","severity","phase","rootCause","actualNoDefects","predictNoDefects","releaseName","currentWeek","cycle")
  
  final_Predicted <- final_Predicted %>% arrange(projectName, application, phase, cycle, weekNumber)
  
  final_Predicted$combin3 <- paste(final_Predicted$projectName, final_Predicted$application, final_Predicted$phase
                                   ,final_Predicted$cycle,sep='_')
  
  final_Predicted$combin4 <- paste(final_Predicted$projectName, final_Predicted$application, final_Predicted$phase
                                   ,final_Predicted$cycle,final_Predicted$weekNumber,sep='_')
  
  for (i in 1:nrow(final_Predicted)) {
    if (i==1) {
      final_Predicted$weekNumber[i]=final_Predicted$weekNumber[i]
    }else{
      if (final_Predicted$combin3[i]==final_Predicted$combin3[i-1]) {
        if (final_Predicted$combin4[i]==final_Predicted$combin4[i-1]) {
          final_Predicted$weekNumber[i]=final_Predicted$weekNumber[i-1]
        }else{
          final_Predicted$weekNumber[i]=final_Predicted$weekNumber[i-1]+1
        }
      }else{
        final_Predicted$weekNumber[i]=final_Predicted$weekNumber[i-1]+1
      }
    }
  }
  
  #final_Predicted$cycle <- NULL
  final_Predicted$combin3 <- NULL
  final_Predicted$combin4 <- NULL
  
  return(final_Predicted) 
}
