check_Null = function(dataset)
{
  j = 1 
  col_names = ""
  for(i in names(dataset))
  {
    if(any(is.na(dataset[,i])))
    {
      
      j = j+1
      print(i)
      col_names[j] = i
    }
  }
  return(col_names[-1])
}



data_validation = function()
{       
        CONTINUE_EXECUTION = TRUE

#########################################################################################################################################
#                          Check for presence of NULL values in the input table 
#########################################################################################################################################
        
        missing_Data_History =  check_Null(data)
        missing_Data_defectData =  check_Null(defectData)
        missing_Data_projectProfile =  check_Null(projectProfile)
        
        write("\n",file="IDP_Log.txt",append=TRUE)
        
        if(length(missing_Data_History) > 0)
        {
          msg = NULL
          CONTINUE_EXECUTION = FALSE 
          msg = paste(Sys.time()," ERROR: NULL value present in following column of History table            : ",missing_Data_History)
          print(msg)
          write(msg,file="IDP_Log.txt",append=TRUE)
          stop("Execution Stopped: Please check the Log")
        }
        
        
        if(length(missing_Data_defectData) > 0)
        {
          msg = NULL
          CONTINUE_EXECUTION = FALSE
          msg = paste(Sys.time()," ERROR: NULL value present in following columns of Inprogress defect table : ",missing_Data_defectData)
          print(msg)
          write(msg,file="IDP_Log.txt",append=TRUE)
          stop("Execution Stopped: Please check the Log")
        }
        
        
        if(length(missing_Data_projectProfile) > 0)
        {
          msg = NULL
          CONTINUE_EXECUTION = FALSE
          msg = paste(Sys.time()," ERROR: NULL value present in following columns of ProjectProfile table    : ",missing_Data_projectProfile)
          print(msg)
          write(msg,file="IDP_Log.txt",append=TRUE)
          stop("Execution Stopped: Please check the Log")
        }
        
        
#########################################################################################################################################
#                          Check for presence of duplicates in Project Profile 
#########################################################################################################################################

        
        project_combins = paste(projectProfile$projectName,projectProfile$application,projectProfile$phase)
        
        if(length(unique(project_combins)) != length(project_combins))
        {
          msg = NULL
          CONTINUE_EXECUTION = FALSE
          msg = paste(Sys.time()," ERROR: Duplicate project/application/phase present")
          print(msg)
          write(msg,file="IDP_Log.txt",append=TRUE)
          stop("Execution Stopped: Please check the Log")
        }
        

#########################################################################################################################################
#                          Check for type correctness of the numeric Fields  
#########################################################################################################################################
        
        
        if(!is.numeric(data$durationWeeks))
          {
            data$durationWeeks = as.numeric(data$durationWeeks)
            if(any(is.na(data$durationWeeks)))
              {
                msg = NULL
                CONTINUE_EXECUTION = FALSE
                msg = paste(Sys.time()," ERROR: Non-numeric filed created for durationWeeks in History table.")
                print(msg)
                write(msg,file="IDP_Log.txt",append=TRUE)
                stop("Execution Stopped: Please check the Log")
              }
           }
        
        if(!is.numeric(data$developerTeamSize))
          { 
            data$developmentTeamSizeForRelease = as.numeric(data$developerTeamSize)
            if(any(is.na(data$developerTeamSize)))
            {
              msg = NULL
              CONTINUE_EXECUTION = FALSE
              msg = paste(Sys.time()," ERROR: Non-numeric filed created for developerTeamSize in History table.")
              print(msg)
              write(msg,file="IDP_Log.txt",append=TRUE)
              stop("Execution Stopped: Please check the Log")
            }
          }      
        
        if(!is.numeric(data$testTeamSize))
          { 
            data$testTeamSize = as.numeric(data$testTeamSize)  
            if(any(is.na(data$testTeamSize)))
                  {     
                    msg = NULL
                    CONTINUE_EXECUTION = FALSE
                    msg = paste(Sys.time()," ERROR: Non-numeric filed created for testTeamSize in History table.")
                    print(msg)
                    write(msg,file="IDP_Log.txt",append=TRUE)
                    stop("Execution Stopped: Please check the Log")
                  }
          }
        
        if(!is.numeric(data$noOfExecutedTestCases))
          {
            data$noOfExecutedTestCases = as.numeric(data$noOfExecutedTestCases)
            if(any(is.na(data$noOfExecutedTestCases)))
             {
                msg = NULL
                CONTINUE_EXECUTION = FALSE
                msg = paste(Sys.time()," ERROR: Non-numeric filed created for noOfExecutedTestCases in History table.")
                print(msg)
                write(msg,file="IDP_Log.txt",append=TRUE)
                stop("Execution Stopped: Please check the Log")
             }
           }
        
        if(!is.numeric(data$appComplexity))
          {
              data$Complexity = as.numeric(data$appComplexity)
              if(any(is.na(data$appComplexity)))
              {
                msg = NULL
                CONTINUE_EXECUTION = FALSE
                msg = paste(Sys.time()," ERROR: Non-numeric filed created for appComplexity in History table.")
                print(msg)
                write(msg,file="IDP_Log.txt",append=TRUE)
                stop("Execution Stopped: Please check the Log")
              }
           }          
        
        if(!is.numeric(projectProfile$appComplexity))
          {
            projectProfile$appComplexity = as.numeric(projectProfile$appComplexity)
            if(any(is.na(projectProfile$appComplexity)))
             {
                msg = NULL
                CONTINUE_EXECUTION = FALSE
                msg = paste(Sys.time()," ERROR: Non-numeric filed created for appComplexity in projectProfile table.")
                print(msg)
                write(msg,file="IDP_Log.txt",append=TRUE)
                stop("Execution Stopped: Please check the Log")
             }
           }
          
        
        if(!is.numeric(projectProfile$developerTeamSize))
          {
            projectProfile$developerTeamSize = as.numeric(projectProfile$developerTeamSize)
            if(any(is.na(projectProfile$developerTeamSize)))
              {
                msg = NULL
                CONTINUE_EXECUTION = FALSE
                msg = paste(Sys.time()," ERROR: Non-numeric filed created for developerTeamSize in projectProfile table.")
                print(msg)
                write(msg,file="IDP_Log.txt",append=TRUE)
                stop("Execution Stopped: Please check the Log")
              }
          }
        
        
        if(!is.numeric(projectProfile$testTeamSize))
          {
            if(any(is.na(projectProfile$testTeamSize)))
            {
              msg = NULL
              CONTINUE_EXECUTION = FALSE
              msg = paste(Sys.time()," ERROR: Non-numeric filed created for testTeamSize in projectProfile table.")
              print(msg)
              write(msg,file="IDP_Log.txt",append=TRUE)
              stop("Execution Stopped: Please check the Log")
            }
          }
        
        if(!is.numeric(projectProfile$durationWeeks))
          {
            projectProfile$durationWeeks =  projectProfile$durationWeeks
            if(any(is.na(projectProfile$durationWeeks)))
            {
              msg = NULL
              CONTINUE_EXECUTION = FALSE
              msg = paste(Sys.time()," ERROR: Non-numeric filed created for durationWeeks in projectProfile table.")
              print(msg)
              write(msg,file="IDP_Log.txt",append=TRUE)
              stop("Execution Stopped: Please check the Log")
            }
          }
          
        
        if(!is.numeric(projectProfile$noOfExecutedTestCases))
          {
            projectProfile$noOfExecutedTestCases = as.numeric(projectProfile$noOfExecutedTestCases)
            if(any(is.na(projectProfile$noOfExecutedTestCases)))
            {
              msg = NULL
              CONTINUE_EXECUTION = FALSE
              msg = paste(Sys.time()," ERROR: Non-numeric filed created for noOfExecutedTestCases in projectProfile table.")
              print(msg)
              write(msg,file="IDP_Log.txt",append=TRUE)
              stop("Execution Stopped: Please check the Log")
            }
          }
        
        
        return(CONTINUE_EXECUTION) 
}
