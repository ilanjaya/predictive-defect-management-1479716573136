rm(list=ls())

setwd('C:/Niladri/Work/Cognitive Defect Prediction/Regression Model/Insurance Data/MetLife_DRM_Data/Model using NN')
# install.packages('ecodist') # Need to run this line only once for a particular system
library('ecodist')
# install.packages('FactoMineR') # Need to run this line only once for a particular system
library(FactoMineR)

# install.packages('polycor') # Need to run this line only once for a particular system
library('polycor')
# install.packages('reshape2') # Need to run this line only once for a particular system
library(reshape2)

# install.packages('psych') # Need to run this line only once for a particular system
library('psych')

# install.packages('plyr') # Need to run this line only once for a particular system
library('plyr')
# install.packages("fmsb") # Need to run this line only once for a particular system
library(dplyr)
# install.packages("car") # Need to run this line only once for a particular system
library(car)
# install.packages("dplyr") # Need to run this line only once for a particular system
library(dplyr)

# install.packages("e1071")  # Need to run this line only once for a particular system
# library("e1071")

# install.packages("nnet")  # Need to run this line only once for a particular system
library(nnet)

# Read input data
data=read.csv('data_tbc_6Dec_remY65.csv',sep=",",header = T,check.names = FALSE)
# data$`Open Date`=as.Date(data$`Open Date`,origin = "1899-12-30") # Convert numeric to date
data$`Open Date`=as.Date(data$`Open Date`,"%d-%b-%Y") # Convert numeric to date

# str(data)
# rm(data_tbc)
data_tbc=data[,c(2:5,7:9,52:55,57:61)]
# data_tbc$Component
# str(data_tbc)
# rm(data_tbc_train)
#----------------------- Training Set---------------------
data_tbc_train=subset (data_tbc, (Component %in% c("V3 - 8058","Global Sales Platform - USA Installation - 9306","Global Comp 2 - 8954","CAS - 2936","SmartApp Suite - 4400","iPAS - 7870","TAM - 7351","UCS - Dental Claims - 1156","MetOnline - 8911","Wealth Management Accelerator - 4371I ","Met Process Manager - 10103","DPA - 6525")))
week_number <- as.POSIXlt(data_tbc_train$`Open Date`)
data_tbc_train$week_number=strftime(week_number,format="%W")
#---------- Estimate Week Number ----------
# data$`Defect ID`
#---------- Estimate Year ----------
data_tbc_train$year<-as.numeric(format(data_tbc_train$`Open Date`, "%Y"))

# data_agg=ddply(data, .(year, week_number,Component,Phase), summarize,   #Aggregating defect count
#                defect_agg=length(`Defect ID`))
# 
#Add application category before application after recieving application
  # Grouping data at year & week level
  data_agg=ddply(data_tbc_train, .(Project,`Proj CMMI`,`Phase`,`No. of Test Case Executed`,`Planned Duration in weeks`,`Development Team Size- For Release`,`Testing Team Size- For Release`,Component,`Application Version`,Complexity,Severity,`Root Cause Category_NG`,year,week_number), 
                 summarize,   #Aggregating defect count
                 defect_agg=length(`Defect ID`)) #2167X15 Matrix
# write.csv(data_agg,"agg_train.csv")
# Creating a combined field to generate wk sl number
data_agg$combine=toupper(paste(data_agg$Project,data_agg$`Proj CMMI`,data_agg$`Phase`,data_agg$`No. of Test Case Executed`,data_agg$`Planned Duration in weeks`,data_agg$`Development Team Size- For Release`,data_agg$`Testing Team Size- For Release`,data_agg$Component,data_agg$`Application Version`,data_agg$Complexity,data_agg$Severity,data_agg$`Root Cause Category_NG`,sep="_"))
data_agg1=data_agg[order(data_agg$combine,data_agg$year,data_agg$week_number),]
  
  
data_agg1[1,17]=1
  for (i in 2:nrow(data_agg1))
  {
    if(data_agg1[i,16]==data_agg1[i-1,16]){
      data_agg1[i,17]=data_agg1[i-1,17]+1
    }else{
      data_agg1[i,17]=1
    }
    
  }
  colnames(data_agg1)[17]="wk"
  
# write.csv(data_agg1,"agg_data_train_13Dec_comp_RC_NG.csv")
data_train=data_agg1
  
#--------------------------------- Validation Set---------------------------------
rm(data_tbc_vali)
data_tbc_vali=subset (data_tbc, (Component %in% c("eOps - 2597","Contract Print - 6560","ID Comp Web Portal - 7091","TAM - Web App - 7981")))

week_number <- as.POSIXlt(data_tbc_vali$`Open Date`)
data_tbc_vali$week_number=strftime(week_number,format="%W")
#---------- Estimate Week Number ----------
# data$`Defect ID`
#---------- Estimate Year ----------
data_tbc_vali$year<-as.numeric(format(data_tbc_vali$`Open Date`, "%Y"))
rm(data_agg)

# Creating a combined field to generate wk sl number
data_agg=ddply(data_tbc_vali, .(Project,`Proj CMMI`,`Phase`,`No. of Test Case Executed`,`Planned Duration in weeks`,`Development Team Size- For Release`,`Testing Team Size- For Release`,Component,`Application Version`,Complexity,Severity,`Root Cause Category_NG`,year,week_number), 
               summarize,   #Aggregating defect count
               defect_agg=length(`Defect ID`)) #246X15 Matrix
# write.csv(data_agg,"agg_vali.csv")
rm(data_agg1)

data_agg$combine=toupper(paste(data_agg$Project,data_agg$`Proj CMMI`,data_agg$`Phase`,data_agg$`No. of Test Case Executed`,data_agg$`Planned Duration in weeks`,data_agg$`Development Team Size- For Release`,data_agg$`Testing Team Size- For Release`,data_agg$Component,data_agg$`Application Version`,data_agg$Complexity,data_agg$Severity,data_agg$`Root Cause Category_NG`,sep="_"))
# rm(data_agg1)
data_agg1=data_agg[order(data_agg$combine,data_agg$year,data_agg$week_number),]

data_agg1[1,17]=1
for (i in 2:nrow(data_agg1))
{
  if(data_agg1[i,16]==data_agg1[i-1,16]){
    data_agg1[i,17]=data_agg1[i-1,17]+1
  }else{
    data_agg1[i,17]=1
  }
  
}
colnames(data_agg1)[17]="wk"

data_vali=data_agg1
#--------------------------------- Validation Set---------------------------------

#-------------------- Dummy Variable Creation --------------------
rm(data_tmp)
data_tmp <- mutate_each(data_train, funs(toupper))

#-------------------- Dummy Variable Creation for Training--------------------

# 7 relase
for(level in unique(data_tmp$`Project`)){
  data_tmp[paste("dummyProject", level, sep = "_")] <- ifelse(data_tmp$`Project` == level, 1, 0)
}

#9 Phase
for(level in unique(data_tmp$`Phase`)){
  data_tmp[paste("dummyPhase", level, sep = "_")] <- ifelse(data_tmp$`Phase` == level, 1, 0)
}
# write.csv(data_tmp,"test_agg.csv")


# Creating Dummies for Severity #4 Severity 
for(level in unique(data_tmp$Severity)){
  data_tmp[paste("dummySeverity", level, sep = "_")] <- ifelse(data_tmp$Severity == level, 1, 0)
}

# Creating Dummies for Component #11

for(level in unique(data_tmp$Component)){
  data_tmp[paste("dummyComponent", level, sep = "_")] <- ifelse(data_tmp$Component == level, 1, 0)
}

# Creating Dummies for Root Cause Modified #10
for(level in unique(data_tmp$`Root Cause Category_NG`)){
  data_tmp[paste("dummyRoot Cause Category", level, sep = "_")] <- ifelse(data_tmp$`Root Cause Category_NG` == level, 1, 0)
}
# str(data_tmp)
#-------------------- Dummy Variable Creation for Training--------------------

data_tmp$defect_agg=as.numeric(data_tmp$defect_agg)
data_tmp$`Proj CMMI`=as.numeric(data_tmp$`Proj CMMI`)
data_tmp$`No. of Test Case Executed`=as.numeric(data_tmp$`No. of Test Case Executed`)
data_tmp$`Planned Duration in weeks`=as.numeric(data_tmp$`Planned Duration in weeks`)
data_tmp$`Development Team Size- For Release`=as.numeric(data_tmp$`Development Team Size- For Release`)
data_tmp$`Testing Team Size- For Release`=as.numeric(data_tmp$`Testing Team Size- For Release`)
data_tmp$wk=as.numeric(data_tmp$wk)
data_tmp$Complexity=as.numeric(data_tmp$Complexity)
data_tmp$`Application Version`=1

data_tmp$log_TestCases=log(data_tmp$`No. of Test Case Executed`)
data_tmp$log_plannedDuration=log(data_tmp$`Planned Duration in weeks`)
data_tmp$log_DevTeam=log(data_tmp$`Development Team Size- For Release`)
data_tmp$log_testTeam=log(data_tmp$`Testing Team Size- For Release`)
data_tmp$log_complexity=log(data_tmp$Complexity)
data_tmp$defect_agg=as.numeric(data_tmp$defect_agg)
# write.csv(data_tmp,"test.csv")


# set.seed(130) # 94 Match using this seed
# set.seed(10) #109 Match using this seed
# write.csv(data_tmp,"temp.csv")
train=data_tmp[,c(15,17,18,22,28,29,30,32,34,35,39,43,50,51,52,53,54,59,62)]
# install.packages('nnet')


# summarize the fit
# summary(fit)
# make predictions

#------------------- Validation-----------------


# write.csv(data_svm,"svm.csv")

# rm(data_tmp)
data_tmp <- mutate_each(data_vali, funs(toupper))

#-------------------- Dummy Variable Creation for Training--------------------

#2 Projects
for(level in unique(data_tmp$`Project`)){
  data_tmp[paste("dummyProject", level, sep = "_")] <- ifelse(data_tmp$`Project` == level, 1, 0)
}

#8 Phase
for(level in unique(data_tmp$`Phase`)){
  data_tmp[paste("dummyPhase", level, sep = "_")] <- ifelse(data_tmp$`Phase` == level, 1, 0)
}
# write.csv(data_tmp,"test_agg.csv")


# Creating Dummies for Severity #4 Severity 
for(level in unique(data_tmp$Severity)){
  data_tmp[paste("dummySeverity", level, sep = "_")] <- ifelse(data_tmp$Severity == level, 1, 0)
}

# Creating Dummies for Component #4

for(level in unique(data_tmp$Component)){
  data_tmp[paste("dummyComponent", level, sep = "_")] <- ifelse(data_tmp$Component == level, 1, 0)
}

# Creating Dummies for Root Cause Modified #10
for(level in unique(data_tmp$`Root Cause Category_NG`)){
  data_tmp[paste("dummyRoot Cause Category", level, sep = "_")] <- ifelse(data_tmp$`Root Cause Category_NG` == level, 1, 0)
}

data_tmp$defect_agg=as.numeric(data_tmp$defect_agg)
data_tmp$`Proj CMMI`=as.numeric(data_tmp$`Proj CMMI`)
data_tmp$`No. of Test Case Executed`=as.numeric(data_tmp$`No. of Test Case Executed`)
data_tmp$`Planned Duration in weeks`=as.numeric(data_tmp$`Planned Duration in weeks`)
data_tmp$`Development Team Size- For Release`=as.numeric(data_tmp$`Development Team Size- For Release`)
data_tmp$`Testing Team Size- For Release`=as.numeric(data_tmp$`Testing Team Size- For Release`)
data_tmp$wk=as.numeric(data_tmp$wk)
data_tmp$Complexity=as.numeric(data_tmp$Complexity)
data_tmp$`Application Version`=1
# str(data_tmp)

data_tmp$log_TestCases=log(data_tmp$`No. of Test Case Executed`)
data_tmp$log_plannedDuration=log(data_tmp$`Planned Duration in weeks`)
data_tmp$log_DevTeam=log(data_tmp$`Development Team Size- For Release`)
data_tmp$log_testTeam=log(data_tmp$`Testing Team Size- For Release`)
data_tmp$log_complexity=log(data_tmp$Complexity)

# write.csv(data_tmp,"test.csv")
data_tmp$`dummyComponent_UCS - DENTAL CLAIMS - 1156`=0
data_tmp$dummyProject_COMPENSATION_TRIALONE=0
data_tmp$dummyProject_LOCALMARKET=0
data_tmp$`dummyComponent_CAS - 2936`=0
# write.csv(data_tmp,"temp.csv")

rm(data_svm_vali)
vali=data_tmp[,c(15,17,50,51,22,23,24,26,29,28,52,49,37,38,43,42,41,44,47)]
str(vali)

# df1=data.frame(seed=-9,MAPE0=-9)
# 
# for (i in c(1:1000))
# {
  
  # seed=5000+100*i
  seed=57900
  set.seed(seed)
  fit <- nnet(defect_agg~., train, size=3, maxit=500, linout=T, decay=0.01)
  predictions1 <- predict(fit, vali, type="raw")
  predictions=round(predictions1,0)
  pred=data.frame(data_tmp,pred=predictions)
  pred[,54]=abs(pred$defect_agg-round(pred$pred,0))/pred$defect_agg
  colnames(pred)[54]="MAPE"
  # MAPE0=sum(pred[,54]==0)
#   df=data.frame(seed=seed,MAPE0=MAPE0)
#   df1=rbind(df1,df)  
# }
  
# write.csv(df1,"seed.csv")

# summarize accuracy
write.csv(pred,"NN pred.csv")
