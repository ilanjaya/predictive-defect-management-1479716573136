url1 = "mongodb://idaroot:LHEWGOJWAREPGREWAOTIHHAS@169.47.19.170:6015"

db_in =  "idpmet"
data 		= f_read_history(url1,db_in)
projectProfile  = f_read_profile(url1,db_in)
defectData      = f_read_inprogress_defects(url1,db_in)


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


project_combins = paste(projectProfile$projectName,projectProfile$application,projectProfile$phase)

if(length(unique(project_combins)) != length(project_combins))
  {
    Print("Duplicate project/application present")
  }



if(!is.numeric(data$plannedDurationInWeeks))
{
  print("invalid filed type in History: plannedDurationInWeeks should be numeric")
}


if(!is.numeric(data$developmentTeamSizeForRelease))
{
  print("invalid filed type in History: developmentTeamSizeForRelease should be numeric")
}


if(!is.numeric(data$testingTeamSizeForRelease))
{
  print("invalid filed type in History: testingTeamSizeForRelease should be numeric")
}


if(!is.numeric(data$numberOfTestCaseExecuted))
{
  print("invalid filed type in History: numberOfTestCaseExecuted should be numeric")
}


if(!is.numeric(data$Complexity))
{
  print("invalid filed type in History: Complexity should be numeric")
}



if(!is.numeric(projectProfile$appComplexity))
{
  print("invalid filed type in projectProfile: appComplexity should be numeric")
}



if(!is.numeric(projectProfile$developerTeamSize))
{
  print("invalid filed type in projectProfile: developerTeamSize should be numeric")
}


if(!is.numeric(projectProfile$testTeamSize))
{
  print("invalid filed type in projectProfile: testTeamSize should be numeric")
}


if(!is.numeric(projectProfile$durationWeeks))
{
  print("invalid filed type in projectProfile: durationWeeks should be numeric")
}


if(!is.numeric(projectProfile$noOfExecutedTestCases))
{
  print("invalid filed type in projectProfile: noOfExecutedTestCases should be numeric")
}


