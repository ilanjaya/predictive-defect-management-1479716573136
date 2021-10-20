
f_read_history =  function(url,db_in)
{
  url =  url1 
  # read historical defects 
  conn <- mongo(collection = "History_Defect_Table", db = db_in, url = url1)
  data <- conn$find()
  print(paste0("Info: # of historical records read =  ", nrow(data)))
  return(data)
}



f_read_profile =  function(url,db_in)
{
  url =  url1 
  # read project profile
  con <- mongo(collection = "projectProfile", db = db_in, url = url1)
  projectProfile<-con$find('{"closedDate" : {"$ne" : ""}}')
  print(paste0("Info: # of projectProfile records read =  ", nrow(projectProfile)))
  return(projectProfile)
}

f_read_inprogress_defects =  function(url,db_in)

{
  url =  url1
  # read inprogress defect data
  con <- mongo(collection = "defectData", db = db_in, url = url1)
  defectData<-con$find('{"defectId" : {"$ne" : ""}}')
  print(paste0("Info: # of defectData records read =  ", nrow(defectData)))
  return(defectData)
}

f_read_rcMap =  function(url,db_in)
{
  url =  url1
  # read rootCauseMap
  con <- mongo(collection = "rootCauseMap", db = db_in, url = url1)
  rcMap<-con$find()
  print(paste0("Info: # of root-Cause records read =  ", nrow(rcMap)))
  return(rcMap)
}




