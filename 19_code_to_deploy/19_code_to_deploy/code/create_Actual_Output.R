f_create_actua_output =  function(defectData_in,projectProfile_in)

{
  defectData = defectData_in
  projectProfile = projectProfile_in
  # ##########################################################################################################
  # Merging Inprogress app defect table with project profile to get complete information of Inprogress App
  # ##########################################################################################################
  
  Inprogress_App1 <- merge(x=defectData,y=projectProfile, by=c("application","phase","projectName"),all.x = TRUE)
  
  Inprogress_App1[is.na(Inprogress_App1$rootCause),"rootCause"] = "Non-classified"
  Inprogress_App1 <- Inprogress_App1[!is.na(Inprogress_App1$rootCause),]
  
  Inprogress_App2 <- subset(Inprogress_App1,select=c("projectName",	"phase",
                                                     "application",	"defectId",
                                                     "severity",	"openDate",
                                                     "closedDate",	"durationWeeks",
                                                     "developerTeamSize",	"testTeamSize",
                                                     "noOfExecutedTestCases",	"rootCause",
                                                     "appVersion",	"appComplexity",
                                                     "maturity"))
  Inprogress_App2$Flag <- "Inprogress_Actual"
  #Inprogress_App2$openDate <-  as.Date(Inprogress_App2$openDate, "%Y-%m-%d")
  #Inprogress_App2$closedDate = as.Date(Inprogress_App2$closedDate,"%Y-%m-%d")
  
  #Inprogress_App2$openDate <-  mdy(Inprogress_App2$openDate)
  Inprogress_App2$closedDate = mdy(Inprogress_App2$closedDate)

  Inprogress_App2$Freq <-  1
  
  ##############################################################################################################
  ## Here Start date has to be added
  ##############################################################################################################
  
  min_date_by_phase = aggregate(x=Inprogress_App2$openDate,list(projectName=Inprogress_App2$projectName
                                                                ,application=Inprogress_App2$application
                                                                ,phase = Inprogress_App2$phase),FUN="min")
  
  mg1 = merge(Inprogress_App2,min_date_by_phase,by=c("projectName","application","phase")) 
  mg1$phase_start_date = mg1$x
  mg1$x = NULL
  
  Inprogress_App2$phase_start_date = mg1$phase_start_date
  
  ######################################################################################################################
  ##  Impute inbetween zeroes
  ######################################################################################################################
  mg1 = mg1[order(mg1$application,mg1$phase,mg1$openDate),]
  
  buffer_df = mg1[0,]
  #Vimal changes
  #Moved buffer_df1 creation to handle scenario for defects only for week 1
  buffer_df1 = mg1[0,]

  if (nrow(mg1) > 1)
  {
    for (i in 1:(nrow(mg1) - 1))
    {
      days_diff = as.numeric(mg1$openDate[i + 1] - mg1$openDate[i])
      #print(days_diff)
      
      
      if (days_diff > 1 & (mg1$phase[i] == mg1$phase[i + 1]))
      {
        for (j in 1:(days_diff - 1))
        {
          insert_row = mg1[i,]
          insert_row$Freq = 0
          insert_row$openDate = insert_row$openDate + j
          insert_row$closedDate = insert_row$openDate
          insert_row$severity = "4-Low"
          insert_row$rootCause = "Data"
          buffer_df =  rbind(buffer_df,insert_row)
          
        }
      }
    }
    
    #####################################################################################################################
    ## Impute initial zeroes
    #####################################################################################################################
    
    # for testing
    #mg1$phase_start_date[1] = "2014-10-01"
    
    #Vimal changed to fix MetLife testing issue. This statement is moved to top.
    #buffer_df1 = mg1[0,]
    
    #i ="2-Analysis"
    
    for (i in unique(mg1$phase))
    {
      days_diff = as.numeric(min(mg1$openDate[mg1$phase == i]) - mg1$phase_start_date[mg1$phase == i][1])
      
      if (days_diff > 0)
      {
        for (j in 1:(days_diff))
        {
          insert_row = mg1[mg1$phase == i,][1,]
          insert_row$Freq = 0
          insert_row$openDate = mg1$phase_start_date[mg1$phase == i][1] + j - 1
          insert_row$closedDate = insert_row$openDate
          insert_row$severity = "4-Low"
          insert_row$rootCause = "Data"
          buffer_df1 =  rbind(buffer_df1,insert_row)
        }
      }
    }
    
  } 
  ########################### Merge & Order - zero imputes to Inprogress ####################################################
  Inprogress_App2 = rbind(Inprogress_App2,buffer_df,buffer_df1)
  Inprogress_App2 =   Inprogress_App2[order(Inprogress_App2$projectName,
                                            Inprogress_App2$application,
                                            Inprogress_App2$phase,
                                            Inprogress_App2$openDate),]
  
  
  # #######################################################################################################################
  # Performing Basic Operations & Rollup @ Application-Phase Weekly level
  # #######################################################################################################################
  
  Inprogress_App2$WeekNum <-  week(Inprogress_App2$openDate)
  Inprogress_App2$year <- year(Inprogress_App2$openDate)
  
  
  ##################################################################################################
  # Getting the Weekly Defect Identified
  
  inprogress_agg <- aggregate(x = Inprogress_App2$Freq, list(   ProjCMMI = Inprogress_App2$maturity
                                                                ,project =Inprogress_App2$projectName
                                                                ,app_name = Inprogress_App2$application
                                                                ,app_type = Inprogress_App2$appVersion
                                                                ,app_complexity = Inprogress_App2$appComplexity
                                                                ,phase = Inprogress_App2$phase
                                                                ,Severity = Inprogress_App2$severity
                                                                ,rootCause = Inprogress_App2$rootCause
                                                                ,phase_app_duration = Inprogress_App2$durationWeeks
                                                                ,test_case = Inprogress_App2$noOfExecutedTestCases
                                                                ,dev_team_size = Inprogress_App2$developerTeamSize
                                                                ,test_team_size = Inprogress_App2$testTeamSize
                                                                ,Flag = Inprogress_App2$Flag
                                                                ,Year = Inprogress_App2$year
                                                                ,WEEK = Inprogress_App2$WeekNum)
                              , FUN = "sum")
  
  
  inprogress_agg$defect_count = inprogress_agg$x 
  inprogress_agg$x  = NULL
  
  #names(consolidated_out_datFrame)
  agg <- aggregate(x = inprogress_agg$WEEK, list(Year = inprogress_agg$Year,WEEK = inprogress_agg$WEEK), FUN = "max")
  agg$x = NULL
  agg = agg[order(agg$Year,agg$WEEK),]
  agg$Entire_Project_Week = 1:nrow(agg)
  
  m_Actual=merge(inprogress_agg,agg,by=c("Year","WEEK"),all.x = TRUE)
  m_Actual$releaseName = "2"
  
  m_Actual$actual_obs_def = m_Actual$predictNoDefects = m_Actual$defect_count
  
  ###########################################################################################################################
  ### Current Week Calculation:
  
  m_Actual$current_week =  0
  
  for(i in unique(m_Actual$phase))
  {
    max_year = max(m_Actual$Year[m_Actual$phase == i])
    max_week = max(m_Actual$WEEK[m_Actual$phase == i & m_Actual$Year == max_year])
    #m_Actual$current_week [m_Actual$phase == i & m_Actual$Year == max_year & m_Actual$WEEK == max_week] = 1
	  m_Actual$current_week [m_Actual$phase == i & ((m_Actual$Year == max_year) & (m_Actual$WEEK == max_week))] = 1
  }
  
  ###########################################################################################################################
  
  final_Actual = m_Actual[,c("project","app_name","Entire_Project_Week","Severity","phase","rootCause","actual_obs_def","predictNoDefects","releaseName","current_week")]
  names(final_Actual) = c("projectName","application","weekNumber","severity","phase","rootCause","actualNoDefects","predictNoDefects","releaseName","currentWeek")
  
  
  
 return(final_Actual)   
}

