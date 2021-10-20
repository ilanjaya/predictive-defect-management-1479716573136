setwd("/home/exttest2/Production/idp/R")    # changed path with forward slash ("/") where all the R modules saved


source("required_library.R")
source("read_mongoDB.R")
source("create_Predicted_Output.R")
source("create_Actual_Output.R")
source("data_Validation.R")

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

url1 = "mongodb://idaroot:idaroot@127.0.0.1:27017"
db_in =  "ignite"
setwd("/home/exttest2/Production/idp/R")    # Directory for Source code and Logfile

# #################################################################################################
# Pulling Base-data/Profile/Inprogress Application Data from MongoDB & performing base operations #
# #################################################################################################

data 		= f_read_history(url1,db_in)
projectProfile  = f_read_profile(url1,db_in)
defectData      = f_read_inprogress_defects(url1,db_in)

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
for(projs in unique(Inprogress_App2$application))
{
  Consolidated_Training_df <- rbind(data,Inprogress_App2[Inprogress_App2$application == projs,])
  final_predicted = f_predict_defect( Consolidated_Training_df,rc_agg_mrged)
  df_op_predicted = rbind(df_op_predicted,final_predicted)
}

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
#   Merge Actual & Predited
##################################################################################################################################

final_all = rbind(df_op_actual,df_op_predicted)

############################################ RC- Format ############################################################
rcMap 		= f_read_rcMap(url1,db_in)

rcMap$rootCause = rcMap$Root_Cause1
rcMap$Root_Cause1 = NULL
rcMap$rootCause[rcMap$rootCause == "Coding"] = "Code"

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
final_all_rcMapped[is.na(final_all_rcMapped$rootCause),"rootCause"] = "Code"

#  rcMap table is modified to have "value" as colum name . change the case in below code.
final_all_rcMapped[is.na(final_all_rcMapped$value),"value"] = "Low"

######################################################################################################################

if(T)
{
  con <- mongo(collection = "defectPredictFact", db = "idp", url = url1)
  if(con$count() != 0){
    con$drop()
    con$insert(final_all_rcMapped)
  } else {
    con$insert(final_all_rcMapped)
  }
}

