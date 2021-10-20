# <<<<<<< HEAD
# setwd("/home/exttest2/Production/idp/R")    # changed path with forward slash ("/") where all the R modules saved
# =======
# setwd("C:/docker/Production/idp/R")    # changed path with forward slash ("/") where all the R modules saved
# >>>>>>> origin/master

setwd("C:/Vimal/IDP/NBS/Felix_mapped_data/18_code_clean")

source("required_library.R")
source("read_mongoDB.R")
#source("create_Predicted_Output.R")
source("create_Predicted_Output.R")
source("create_Actual_Output.R")
source("data_Validation.R")

# code_drops <- 3
# drop_duration <- "9,11,2"
#drop_duration <- "4"
# ####################################: Format Date Function :#####################################
format_date = function(date_in)
{
  Correct_Input_Date = F
  
  if((regexpr("-",date_in[1])[[1]]>0))
    { 
      date_in = as.Date(date_in)
      Correct_Input_Date = T
    }
  
  if((regexpr("/",date_in[1])[[1]]>0))
    {
      date_in = as.Date(date_in,format = "%m/%d/%Y")
      Correct_Input_Date = T
    }

  if(!Correct_Input_Date)
    {
      msg = NULL
      CONTINUE_EXECUTION = FALSE
      msg = paste(Sys.time()," ERROR: Incorrect input date format of openDate either in History or Defect data")
      print(msg)
      write(msg,file="IDP_Log.txt",append=TRUE)
      stop("Execution Stopped: Please check the Log")
    }
    
  return(date_in)
}

# #################################################################################################
#  load libraries                                                                                 #
# #################################################################################################

f_library()

# #################################################################################################
# Configuaration Parameters
###################################################################################################

# url1 = "mongodb://idaroot:idaroot@127.0.0.1:27017"
# <<<<<<< HEAD
# db_in =  "ignite"
# setwd("/home/exttest2/Production/idp/R")    # Directory for Source code and Logfile
# =======
# db_in =  "idp"
# setwd("C:/docker/Production/idp/R")    # Directory for Source code and Logfile
# >>>>>>> origin/master

# #################################################################################################
# Pulling Base-data/Profile/Inprogress Application Data from MongoDB & performing base operations #
# #################################################################################################

# data 		= f_read_history(url1,db_in)
# projectProfile  = f_read_profile(url1,db_in)
# defectData      = f_read_inprogress_defects(url1,db_in)


data  <-  read.csv("proj_hist_data_input_with_RC.csv", stringsAsFactors = FALSE)
projectProfile <-  read.csv("projectProfile.csv", stringsAsFactors = FALSE)
defectData <-  read.csv("SIT_phase11_cleaned_11weeks.csv", stringsAsFactors = FALSE)

data$X <- NULL

defectData[is.na(defectData$rootCause),"rootCause"] = "Component Code"
defectData[is.na(defectData$closedDate),"closedDate"] = "9999-99-99"

# #################################################################################################
# Data Preparation                                                                                #
# #################################################################################################

#   Field name check for Inprogress Defect Tabale

defectdata_fields = c(
  "defectId","projectName",
  "status","phase",
  "application","rootCause",
  "severity","openDate",
  "closedDate")

for(i in defectdata_fields)
{
 if(!(i %in% names(defectData))) 
  {
    msg = NULL
    CONTINUE_EXECUTION = FALSE
    msg = paste(Sys.time()," ERROR: Following column not present in Inprogress defect table : ",i)
    print(msg)
    write(msg,file="IDP_Log.txt",append=TRUE)
    stop("Execution Stopped: Please check the Log")
  }
}

#   Field name check for ProjectProfile


fiellds_ProjectProfile = c(
  "projectName","releaseName",
  "application","appVersion",
  "appComplexity","phase",
  "developerTeamSize","testTeamSize",
  "startDate","durationWeeks",
  "noOfExecutedTestCases","maturity") 


for(i in fiellds_ProjectProfile)
{
 if(!(i %in% names(projectProfile))) 
  {
    msg = NULL
    CONTINUE_EXECUTION = FALSE
    msg = paste(Sys.time()," ERROR: Following column not present in projectProfile table : ",i)
    print(msg)
    write(msg,file="IDP_Log.txt",append=TRUE)
    stop("Execution Stopped: Please check the Log")
  }
 }

#   Field name check for history

fields_History = c("Project",	"Phase",
                "Component",	"DefectId",
                "Severity",	"openDate",
                "closedDate",	"plannedDurationInWeeks",
                "developmentTeamSizeForRelease",	"testingTeamSizeForRelease",
                "numberOfTestCaseExecuted",	"rootCauseCategoryNG",
                "Complexity","projectCMMILevel",	"applicationVersion")

for(i in fields_History)
  {
   if(!(i %in% names(data))) 
    {
      msg = NULL
      CONTINUE_EXECUTION = FALSE
      msg = paste(Sys.time()," ERROR: Following column not present in History : ",i)
      print(msg)
      write(msg,file="IDP_Log.txt",append=TRUE)
      stop("Execution Stopped: Please check the Log")
    }
  }
  


#Step1:  Create/Modify Filed's details of data pulled from mongoDB
data <- setnames(data, old = c("Project",	"Phase",
                               "Component",	"DefectId",
                               "Severity",	"openDate",
                               "closedDate",	"plannedDurationInWeeks",
                               "developmentTeamSizeForRelease",	"testingTeamSizeForRelease",
                               "numberOfTestCaseExecuted",	"rootCauseCategoryNG",
                               "Complexity","projectCMMILevel",	"applicationVersion"), 
                       
                       new = c("projectName",	"phase",
                               "application",	"defectId",
                               "severity",	"openDate",
                               "closedDate",	"durationWeeks",
                               "developerTeamSize",	"testTeamSize",
                               "noOfExecutedTestCases",	"rootCause",
                               "appComplexity","maturity",	"appVersion"))

data$Flag <- "Base"
data$applicationCategory =  NULL 

###########################################Data Validation##########################################################################
CONTINUE_EXECUTION = TRUE
CONTINUE_EXECUTION  = data_validation()

data$openDate = format_date(data$openDate)
defectData$openDate = format_date(defectData$openDate)

if (any(is.na(data$openDate)))
{
  msg = NULL
  CONTINUE_EXECUTION = FALSE 
  msg = paste(Sys.time()," ERROR: Incorrect openDate format in Inprogress defect table. Should be 'YYYY-mm-dd' or 'mm/dd/yyyy' :")
  print(msg)
  write(msg,file="IDP_Log.txt",append=TRUE)
  stop("Execution Stopped: Please check the Log")
}

if (any(is.na(defectData$openDate)))
{
  msg = NULL
  CONTINUE_EXECUTION = FALSE 
  msg = paste(Sys.time()," ERROR: Incorrect openDate format in Inprogress defect table. Should be 'YYYY-mm-dd' or 'mm/dd/yyyy' :")
  print(msg)
  write(msg,file="IDP_Log.txt",append=TRUE)
  stop("Execution Stopped: Please check the Log")
}

###################################################RC Scoring############################################
data1 =  data
data1$Freq = 1

rc_agg1 <- aggregate(x = data1$Freq, list( phase = data1$phase
                                          ,rootCause = data1$rootCause
                                          ,Severity = data1$severity)
                                          , FUN = "sum")


rc_agg2 <- aggregate(x = data1$Freq, list(phase = data1$phase)
                                   , FUN = "sum")

rc_agg1$RC_level_count = rc_agg1$x
rc_agg2$Phase_level_count = rc_agg2$x

rc_agg1$x = rc_agg2$x = NULL

rc_agg_mrged = merge(rc_agg1,rc_agg2) 
rc_agg_mrged$RC_Probability_by_Phase = rc_agg_mrged$RC_level_count/rc_agg_mrged$Phase_level_count

# #######################################################################################################
# Merging Inprogress app defect table with project profile to get complete information of Inprogress App#
# #######################################################################################################
Inprogress_App1 <- merge(x=defectData,y=projectProfile, by=c("application","phase","projectName"),all.x = TRUE)
Inprogress_App1[is.na(Inprogress_App1$rootCause),"rootCause"] = "Code"
Inprogress_App2 <- subset(Inprogress_App1,select=c("projectName",	"phase",
                                                   "application",	"defectId",
                                                   "severity",	"openDate",
                                                   "closedDate",	"durationWeeks",
                                                   "developerTeamSize",	"testTeamSize",
                                                   "noOfExecutedTestCases",	"rootCause",
                                                   "appComplexity","maturity",	"appVersion"))

# This for Actual Data Preparation
Inprogress_App2_Actual = Inprogress_App2
Inprogress_App2$Flag <- "Inprogress"
Inprogress_App2$openDate <-  as.Date(Inprogress_App2$openDate, "%m-%d-%Y")

drop_n_dur <- Inprogress_App2$durationWeeks[1]
drop_dur_split <- unlist(strsplit(drop_n_dur, split=","))

code_drops <- drop_dur_split[1]
code_drops <- as.numeric(code_drops)

drop_duration <- drop_dur_split[-1]

# ######################################################################
# Appending Base-Data + Inprogress App to get consolidated training data
# ######################################################################

Consolidated_Training_df <- rbind(data,Inprogress_App2)
Consolidated_Training_df$week_Number <- Consolidated_Training_df$WeekNum

#write.csv(Consolidated_Training_df,"Consolidated_Training_df.csv")

##################################################################################################################################
#   Get predicted Ouptput Data
##################################################################################################################################
df_op_predicted = data.frame(projectName = character(),
                             application = character(),
                             weekNumber = integer(),
                             Severity = character(),
                             phase = character(),
                             rootCause = character(),
                             actualNoDefects = integer(),
                             predictNoDefects = integer(),
                             releaseName = integer(),
                             currentWeek = integer(),
                             stringsAsFactors=FALSE)

#projs <- "GSvP - 9147"
#projs <- "NGBA Phase 1.1"
for(projs in unique(Inprogress_App2$application))
{
  Consolidated_Training_df <- rbind(data,Inprogress_App2[Inprogress_App2$application == projs,])
  final_predicted = f_predict_defect( Consolidated_Training_df,rc_agg_mrged,code_drops,drop_duration)
  df_op_predicted = rbind(df_op_predicted,final_predicted)
}
count(defectData)
sum(df_op_predicted$predictNoDefects)

# df_op_predicted %>% group_by(cycle) %>% summarize(sum_def = sum(predictNoDefects))
df_op_predicted$cycle <- NULL

# predicted <- df_op_predicted %>% group_by(application, phase, weekNumber) %>% summarize(pred_def = sum(predictNoDefects))
# predicted$weekNumber <- predicted$weekNumber + 9
# 
# test_file <- read.csv("test_data.csv")
# 
# # ngba_1.1 <- test_file %>% filter(application == 'NGBA Phase 1.1' & phase == 'System Integration Test (SIT)' & Flag == 'Base')
# # 
# # ngba_1.1_week_def <- ngba_1.1 %>% group_by(weekNumber) %>% summarize(def_Count = n())
# # 
# # ngba_1.1_week_def$application <- 'NGBA Phase 1.1'
# # 
# # ngba_1.1_week_def$phase <- 'System Integration Test (SIT)'
# # 
# # cmp_file <- merge(predicted, ngba_1.1_week_def,all.y = TRUE) %>% arrange(weekNumber)
# 
# pt <- test_file %>% filter(application == 'Pending Transactions' & phase == 'System Integration Test (SIT)' & Flag == 'Base')
# pt_week_def <- pt %>% group_by(weekNumber) %>% summarize(def_Count = n())
# pt_week_def$application <- 'Pending Transactions'
# pt_week_def$phase <- 'System Integration Test (SIT)'
# cmp_file <- merge(predicted, pt_week_def,all.y = TRUE) %>% arrange(weekNumber)
# 
# write.csv(df_op_predicted, "out_PT.csv" )
# 
# write.csv(cmp_file, "cmp_file.csv" )
# 
# #x_limit <- seq(min(cmp_file$weekNumber), max(cmp_file$weekNumber))
# plot(cmp_file$weekNumber, cmp_file$def_Count, type = "o",col = "blue", xlim = c(1,22), ylim = c(1,50), xlab = "WeekNumber", ylab = "No. of defects", 
#      main = "Actual vs Predicted")
# 
# lines(cmp_file$weekNumber,cmp_file$pred_def, type = "o", col = "red")
# 
# grid(NA, 5, lwd = 2) # grid only in y-direction

##################################################################################################################################
#   Get actual Ouptput Data
##################################################################################################################################

df_op_actual=data.frame(projectName = character(),
                        application = character(),
                        weekNumber = integer(),
                        Severity = character(),
                        phase = character(),
                        rootCause = character(),
                        actualNoDefects = integer(),
                        predictNoDefects = integer(),
                        releaseName = integer(),
                        currentWeek = integer(),
                        stringsAsFactors=FALSE)

for (projs in unique(defectData$application))
{
  final_Actual = f_create_actua_output(defectData[defectData$application == projs,],projectProfile[projectProfile$application == projs,])
  df_op_actual = rbind(df_op_actual,final_Actual)
}

##################################################################################################################################
#   Get last week in the actual defects output file & predicted output file.
##################################################################################################################################

actual <- df_op_actual %>% filter(currentWeek == 1) %>% group_by(currentWeek) %>% summarize(last_week = max(weekNumber))
actual_last_week <- actual$last_week

predicted_min_week <- min(df_op_predicted$weekNumber)

if (actual_last_week >= predicted_min_week){
  diff_in_week <- actual_last_week - predicted_min_week + 1
  df_op_predicted$weekNumber <- df_op_predicted$weekNumber + diff_in_week
}


##################################################################################################################################
#   Merge Actual & Predited
##################################################################################################################################

final_all = rbind(df_op_actual,df_op_predicted)

############################################ RC- Format ############################################################
#rcMap 		= f_read_rcMap(url1,db_in)
rcMap 		= read.csv("rcmap_NBS.csv",stringsAsFactors = FALSE)

rcMap$rootCause = rcMap$Root_Cause1
rcMap$Root_Cause1 = NULL
#rcMap$rootCause[rcMap$rootCause == "Coding"] = "Code"

#NATZ
#final_all$rootCause = ifelse(final_all$rootCause %in% rcMap$rootCause,rcMap$Root_Cause2,"Not-classified")

#Vimal changes
#Added code to resolve incorrect root cause mapping issue

for (i in 1:(nrow(final_all)))
{
  for (j in 1:(nrow(rcMap)))
    if (final_all$rootCause[i] == rcMap$rootCause[j])
    {
      final_all$rootCause[i] = rcMap$Root_Cause2[j]
    }
}

final_all_rcMapped = merge(final_all,rcMap,by = "rootCause", all.x =  T)
final_all_rcMapped$rootCause = final_all_rcMapped$Root_Cause2
final_all_rcMapped$rootCause1 = final_all_rcMapped$rootCause2 = NULL

# Added lines to handle Data issues specific to NationWide 21-Feb-2017 
final_all_rcMapped[is.na(final_all_rcMapped$rootCause),"rootCause"] = "Component Code"

#  rcMap table is modified to have "value" as colum name . change the case in below code.
final_all_rcMapped[is.na(final_all_rcMapped$value),"value"] = "Low"

######################################################################################################################

# if(T)
# {
#   con <- mongo(collection = "defectPredictFact", db = db_in, url = url1)
#   if(con$count() != 0){
#     con$drop()
#     con$insert(final_all_rcMapped)
#   } else {
#     con$insert(final_all_rcMapped)
#   }
# }

