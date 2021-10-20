rm(list=ls())
setwd('C:/Niladri/Work/Cognitive Defect Prediction/RO_Mongo1')
library(mongolite)
# new mongo url
url = "mongodb://169.47.19.170:6015"

#---------------------------- Input from MongoDB ----------------------------
con <- mongo(collection = "IDP_METLIFE", db = "idp", 
             url = "mongodb://169.44.126.89:27017")
Raw_data<-con$find()
# str(Raw_data)
data_tbc=data.frame(Raw_data)
data_tbc$Open.Date=as.Date(data_tbc$Open.Date,"%d-%b-%Y") # Convert numeric to date

con <- mongo(collection = "projectProfile", db = "idp", url = "mongodb://169.44.126.89:27017")
project=con$find()
# write.csv(data_project,"projectProfile.csv")
data_project=data.frame(project)
data_project=mutate_each(data_project, funs(toupper)) 

con <- mongo(collection = "projectProfile", db = "idp", url = "mongodb://169.44.126.89:27017")
projectProfile<-con$find()
projectProfile = mutate_each(projectProfile, funs(toupper))



con <- mongo(collection = "defectData", db = "idp", url = "mongodb://169.44.126.89:27017")
defectData<-con$find()
defectData = mutate_each(defectData, funs(toupper))
# write.csv(defectData,"defectData.csv")
# write.csv(defectData,"defect_data.csv")
# rm(Raw_data2)
Raw_data2 <- merge(x=defectData,y=projectProfile, by=c("application","phase","projectName"),all.x = TRUE)
Raw_data3=setnames(Raw_data2, old = c('projectName','phase','application','id','severity','openDate','closedDate','actualEndWeek','developerTeamSize','testTeamSize','noOfExecutedTestCases','rootCause','appCategory','appComplexity','maturity','appVersion'), 
                   new = c('Project','Phase','Component','Defect.ID','Severity','Open.Date','Closed.Date','Planned.Duration.in.weeks','Development.Team.Size..For.Release','Testing.Team.Size..For.Release','No..of.Test.Case.Executed','Root.Cause.Category_NG','Application.Category','Complexity','Proj.CMMI','Application.Version'))

write.csv(Raw_data3,"Raw_data3.csv")
Raw_data3$Proj.CMMI=unique(Raw_data$`Proj CMMI`)
Raw_data3$week=week(as.Date(Raw_data3$Open.Date, format = "%Y-%m-%d"))
Raw_data3$year=year(as.Date(Raw_data3$Open.Date, format = "%Y-%m-%d"))
# rm(Raw_data4)
# Raw_data3$
# Raw_data4=subset(Raw_data3,select=c(Project,Proj.CMMI,Phase,Component,Severity,Open.Date,Closed.Date,Planned.Duration.in.weeks,Development.Team.Size..For.Release,Testing.Team.Size..For.Release,No..of.Test.Case.Executed,Root.Cause.Category_NG,Application.Category,Complexity,Proj.CMMI,Application.Version,year,week,defectId))

# Raw_data3$year=format(as.Date(Raw_data3$Open.Date, format = "%Y-%d-%m"), "%Y")
# write.csv(Raw_data3,"raw_date3.csv")
# str(Raw_data4)
# rm(Raw_data4)
Raw_data4=subset(Raw_data3,select=c(Project,Proj.CMMI,Phase,No..of.Test.Case.Executed,Planned.Duration.in.weeks,Development.Team.Size..For.Release,Testing.Team.Size..For.Release,Component,Application.Version,Complexity,Severity,Root.Cause.Category_NG,week,year,defectId))
#########################
rm(data_agg_vali)
data_agg_vali=ddply(Raw_data4, .(Project,Proj.CMMI,Phase,No..of.Test.Case.Executed,Planned.Duration.in.weeks,Development.Team.Size..For.Release,Testing.Team.Size..For.Release,Component,Application.Version,Complexity,Severity,Root.Cause.Category_NG,year,week), 
                    summarize,   #Aggregating defect count
                    defect_agg=length(defectId)) #2167X15 Matrix

# write.csv(data_agg_vali,"data_agg_vali.csv")
# sum(data_agg_vali$defect_agg)

# write.csv(data_agg_vali,"data_agg_vali.csv")
data_agg_vali$combine=toupper(paste(data_agg_vali$Project,data_agg_vali$Proj.CMMI,data_agg_vali$`Phase`,data_agg_vali$No..of.Test.Case.Executed,data_agg_vali$Planned.Duration.in.weeks,data_agg_vali$Development.Team.Size..For.Release,data_agg_vali$Testing.Team.Size..For.Release,data_agg_vali$Component,data_agg_vali$Application.Version,data_agg_vali$Complexity,data_agg_vali$Severity,data_agg_vali$Root.Cause.Category_NG,sep="_"))
# write.csv(data_agg_vali1,"data_agg_vali1.csv")
# str(data_agg_vali)
# rm(data_agg_vali1)
# sum(data_agg_vali$defect_agg)
data_agg_vali1=data_agg_vali[order(data_agg_vali$combine,data_agg_vali$year,data_agg_vali$week),]
# rm(data_agg_vali2)
# write.csv(data_agg_vali1,"data_agg_vali1.csv")
# data_agg_vali2=ddply(data_agg_vali1, .(data_agg_vali$Project,data_agg_vali$Proj.CMMI,data_agg_vali$Phase,data_agg_vali$No..of.Test.Case.Executed,data_agg_vali$Planned.Duration.in.weeks,data_agg_vali$Development.Team.Size..For.Release,data_agg_vali$Testing.Team.Size..For.Release,data_agg_vali$Component,data_agg_vali$Application.Version,data_agg_vali$Complexity,data_agg_vali$Severity,data_agg_vali$Root.Cause.Category_NG,combine,year,week), 
#                     summarize,   #Aggregating defect count
#                     defect_agg=sum(defect_agg)) #2167X15 Matrix

# sum(data_agg_vali1$defect_agg)
# 340
# str(data_agg_vali3)


# data_agg_vali2$comb=toupper(paste(data_agg_vali2$Project,as.character(data_agg_vali2$Proj.CMMI),data_agg_vali2$Phase,data_agg_vali2$No..of.Test.Case.Executed,data_agg_vali2$Planned.Duration.in.weeks,data_agg_vali2$Development.Team.Size..For.Release,data_agg_vali2$Testing.Team.Size..For.Release,data_agg_vali2$Component,data_agg_vali2$Application.Version,data_agg_vali2$Complexity,data_agg_vali2$Severity,data_agg_vali2$Root.Cause.Category_NG,combine,as.character(data_agg_vali2$year),as.character(data_agg_vali2$week),sep="_"))
# data_agg_vali2$comb=toupper(paste(data_agg_vali2$Project,as.character(data_agg_vali2$Proj.CMMI),data_agg_vali2$Phase,data_agg_vali2$No..of.Test.Case.Executed,data_agg_vali2$Planned.Duration.in.weeks,data_agg_vali2$Development.Team.Size..For.Release,data_agg_vali2$Testing.Team.Size..For.Release,data_agg_vali2$Component,data_agg_vali2$Application.Version,data_agg_vali2$Complexity,data_agg_vali2$Severity,data_agg_vali2$Root.Cause.Category_NG,combine,as.character(data_agg_vali2$year),as.character(data_agg_vali2$week),sep="_"))
# data_agg_vali3=data_agg_vali2[,c(1:12,14:16)]
# data_agg_vali3$comb=toupper(paste(data_agg_vali3$Project,as.character(data_agg_vali3$Proj.CMMI),data_agg_vali3$Phase,data_agg_vali3$No..of.Test.Case.Executed,data_agg_vali3$Planned.Duration.in.weeks,data_agg_vali3$Development.Team.Size..For.Release,data_agg_vali3$Testing.Team.Size..For.Release,data_agg_vali3$Component,data_agg_vali3$Application.Version,data_agg_vali3$Complexity,data_agg_vali3$Severity,data_agg_vali3$Root.Cause.Category_NG,combine,as.character(data_agg_vali3$year),as.character(data_agg_vali3$week),sep="_"))
# data_agg_vali3$Project,data_agg_vali3$Proj.CMMI,data_agg_vali3$Phase,data_agg_vali3$No..of.Test.Case.Executed,data_agg_vali3$Planned.Duration.in.weeks,data_agg_vali3$Development.Team.Size..For.Release,data_agg_vali3$Testing.Team.Size..For.Release,data_agg_vali3$Component,data_agg_vali3$Application.Version,data_agg_vali3$Complexity,data_agg_vali3$Severity,data_agg_vali3$Root.Cause.Category_NG,combine,data_agg_vali3$year,data_agg_vali3$week
# data_agg_vali3=setnames(data_agg_vali2, old = c('data_agg_vali$Project','data_agg_vali$Proj.CMMI','data_agg_vali$Phase','data_agg_vali$No..of.Test.Case.Executed','data_agg_vali$Planned.Duration.in.weeks','data_agg_vali$Development.Team.Size..For.Release','data_agg_vali$Testing.Team.Size..For.Release','data_agg_vali$Component','data_agg_vali$Application.Version','data_agg_vali$Complexity','data_agg_vali$Severity','data_agg_vali$Root.Cause.Category_NG','combine','year','week','defect_agg'),
#                       new = c('Project','Proj.CMMI','Phase','No..of.Test.Case.Executed','Planned.Duration.in.weeks','Development.Team.Size..For.Release','Testing.Team.Size..For.Release','Component','Application.Version','Complexity','Severity','Root.Cause.Category_NG','combine','year','week','defect_agg'))
# data_agg_vali3$comb=toupper(paste(Project,Proj.CMMI,Phase,No..of.Test.Case.Executed,Planned.Duration.in.weeks,Development.Team.Size..For.Release,Testing.Team.Size..For.Release,Component,Application.Version,Complexity,Severity,Root.Cause.Category_NG,sep="_"))
# 
# data_agg_vali3$cmmiweekyear=transform(data_agg_vali3,cmmiweekyr=paste0())
# str(data_agg_vali3)
# data_agg_vali3$comb=toupper(paste0(data_agg_vali3$Project,data_agg_vali3$Phase,data_agg_vali3$No..of.Test.Case.Executed,data_agg_vali3$Planned.Duration.in.weeks,data_agg_vali3$Development.Team.Size..For.Release,data_agg_vali3$Testing.Team.Size..For.Release,data_agg_vali3$Component,data_agg_vali3$Application.Version,data_agg_vali3$Complexity,data_agg_vali3$Severity,data_agg_vali3$Root.Cause.Category_NG,combine,sep="_"))
# rm(z)
str(data_agg_vali1)
# z=subset(data_agg_vali3,select=c(Project,Phase,No..of.Test.Case.Executed,Planned.Duration.in.weeks,Development.Team.Size..For.Release,Testing.Team.Size..For.Release,Component,Application.Version,Complexity,Severity,Root.Cause.Category_NG))

# rm(data_tmp1)
# data_tmp1=cbind(data_agg_vali3,do.call(paste0, z[c(1:ncol(z))]))
# colnames(data_tmp1)[ncol(data_tmp1)]="combine"
# # write.csv(data_tmp1,"data_agg_vali3.csv")
# data_tmp1$combine=as.character(data_tmp1$combine)
# # str(data_tmp1)
# data_tmp2=data_tmp1[order(data_tmp1$combine,data_tmp1$year,data_tmp1$week),]
# write.csv()

data_agg_vali1[1,ncol(data_agg_vali1)+1]=1
for (i in 2:nrow(data_agg_vali1))
{
  if(data_agg_vali1[i,ncol(data_agg_vali1)-1]==data_agg_vali1[i-1,ncol(data_agg_vali1)-1]){
    data_agg_vali1[i,ncol(data_agg_vali1)]=data_agg_vali1[i-1,ncol(data_agg_vali1)]+1
  }else{
    data_agg_vali1[i,ncol(data_agg_vali1)]=1
  }
  
}

colnames(data_agg_vali1)[ncol(data_agg_vali1)]="wk"
# write.csv(data_agg_vali1,"weekly_new_defect.csv")
# write.csv(data_tmp2,"data_agg_vali1.csv")
new_data_set=data_agg_vali1
# sum(new_data_set$defect_agg)
# 340
trainig_week=max(new_data_set$wk)
#---------------------------- Input from MongoDB ----------------------------


#---------------------------- Input from MongoDB ----------------------------con <- mongo(collection = "defectData", db = "idp", url = "mongodb://169.44.126.89:27017")
# defectData<-con$find()
# defectData = mutate_each(defectData, funs(toupper)) 
# 
# 
# write.csv(data_project,"proj_profile.csv")
week_number <- as.POSIXlt(data_tbc$Open.Date)
data_tbc$week_number=strftime(week_number,format="%W")
#---------- Estimate Week Number ----------
#---------- Estimate Year ----------
data_tbc$year<-as.numeric(format(data_tbc$Open.Date, "%Y"))
# data_tbc$
data_agg=ddply(data_tbc, .(Project,Proj.CMMI,Phase,No..of.Test.Case.Executed,Planned.Duration.in.weeks,Development.Team.Size..For.Release,Testing.Team.Size..For.Release,Component,Application.Version,Complexity,Severity,Root.Cause.Category_NG,year,week_number), 
               summarize,   #Aggregating defect count
               defect_agg=length(Defect.ID)) #2167X15 Matrix

data_agg$combine=toupper(paste(data_agg$Project,data_agg$Proj.CMMI,data_agg$`Phase`,data_agg$No..of.Test.Case.Executed,data_agg$Planned.Duration.in.weeks,data_agg$Development.Team.Size..For.Release,data_agg$Testing.Team.Size..For.Release,data_agg$Component,data_agg$Application.Version,data_agg$Complexity,data_agg$Severity,data_agg$Root.Cause.Category_NG,sep="_"))
# write.csv(data_agg,"data_agg.csv")
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

# write.csv(data_agg1,"data_metlife.csv")
# write.csv(new_data_set,"new_data_set.csv")
# new_data_set
#----------- Phase -----------
# rm(Phase_prop)
# rm(Phase_prop_max)
Phase_prop=ddply(data_agg1, .(Phase,wk), 
                 summarize,   #Aggregating defect count
                 defect_count=sum(defect_agg)) #2167X15 Matrix

Phase_prop_max=ddply(Phase_prop, .(wk), 
                         summarize,   #Aggregating defect count
                         defect_max=max(defect_count)) #2167X15 Matrix
colnames(Phase_prop_max)[2]='defect_count'
# str(Phase_prop)
# str(Phase_prop_max)
# rm(phase_max)
phase_max=merge(Phase_prop,Phase_prop_max,by=c("wk","defect_count"))
#----------- Phase -----------

#----------- Severity -----------
sev_prop=ddply(data_agg1, .(Severity,wk), 
                 summarize,   #Aggregating defect count
                 defect_count=sum(defect_agg)) #2167X15 Matrix
sev_prop_max=ddply(sev_prop, .(wk), 
                     summarize,   #Aggregating defect count
                     defect_max=max(defect_count)) #2167X15 Matrix
colnames(sev_prop_max)[2]='defect_count'
sev_max=merge(sev_prop,sev_prop_max,by=c("wk","defect_count"))
# write.csv(sev_max,"sev_max.csv")
# write.csv(sev_prop,"sev.csv")
# write.csv(sev_prop_max,"sev_prop_max.csv")

#----------- Severity -----------

# ----------------------Test Cases ----------------------

Test_case_prop=ddply(data_agg1, .(No..of.Test.Case.Executed,wk), 
               summarize,   #Aggregating defect count
               defect_count=sum(defect_agg)) #2167X15 Matrix
# rm(Phase_prop_max)
test_case_prop_max=ddply(Test_case_prop, .(wk), 
                   summarize,   #Aggregating defect count
                   defect_max=max(defect_count)) #2167X15 Matrix
colnames(test_case_prop_max)[2]='defect_count'

test_case_max=merge(Test_case_prop,test_case_prop_max,by=c("wk","defect_count"))

# write.csv(Test_case_prop,"Test_case_prop.csv")
# write.csv(test_case_prop_max,"test_case_prop_max.csv")
# write.csv(test_case_max,"test_case_max.csv")

# ----------------------Test Cases ----------------------
# ----------------------Testing Team ----------------------

Test_team_prop=ddply(data_agg1, .(Testing.Team.Size..For.Release,wk), 
                     summarize,   #Aggregating defect count
                     defect_count=sum(defect_agg)) #2167X15 Matrix
# rm(Phase_prop_max)
test_team_prop_max=ddply(Test_team_prop, .(wk), 
                         summarize,   #Aggregating defect count
                         defect_max=max(defect_count)) #2167X15 Matrix
colnames(test_team_prop_max)[2]='defect_count'

test_team_max=merge(Test_team_prop,test_team_prop_max,by=c("wk","defect_count"))

# write.csv(Test_team_prop,"Test_case_prop.csv")
# write.csv(test_team_prop_max,"test_case_prop_max.csv")
# write.csv(test_team_max,"test_case_max.csv")



# ----------------------Testing Team ----------------------

# ----------------------Development Team ----------------------

dev_team_prop=ddply(data_agg1, .(Development.Team.Size..For.Release,wk), 
                     summarize,   #Aggregating defect count
                     defect_count=sum(defect_agg)) #2167X15 Matrix
# rm(Phase_prop_max)
dev_team_prop_max=ddply(dev_team_prop, .(wk), 
                         summarize,   #Aggregating defect count
                         defect_max=max(defect_count)) #2167X15 Matrix
colnames(dev_team_prop_max)[2]='defect_count'

dev_team_max=merge(dev_team_prop,dev_team_prop_max,by=c("wk","defect_count"))

# write.csv(dev_team_prop,"dev_team_prop.csv")
# write.csv(dev_team_prop_max,"dev_team_prop_max.csv")
# write.csv(dev_team_max,"dev_team_max.csv")

# ----------------------Development Team ----------------------

# ----------------------Root Cause Category ----------------------

Root.Cause.Category_prop=ddply(data_agg1, .(Root.Cause.Category_NG,wk), 
                    summarize,   #Aggregating defect count
                    defect_count=sum(defect_agg)) #2167X15 Matrix
# rm(Phase_prop_max)
Root.Cause.Category_prop_max=ddply(Root.Cause.Category_prop, .(wk), 
                        summarize,   #Aggregating defect count
                        defect_max=max(defect_count)) #2167X15 Matrix
colnames(Root.Cause.Category_prop_max)[2]='defect_count'

Root.Cause.Category_max=merge(Root.Cause.Category_prop,Root.Cause.Category_prop_max,by=c("wk","defect_count"))

# ----------------------Root Cause Category----------------------
# ----------------------Component-Complexity----------------------

# write.csv(Root.Cause.Category_prop,"Root.Cause.Category_prop.csv")
# write.csv(Root.Cause.Category_prop_max,"Root.Cause.Category_prop_max.csv")
# write.csv(Root.Cause.Category_max,"Root.Cause.Category_max.csv")
tmp=data.frame(data_agg1$Component,data_agg1$Complexity)
comp_complex=unique(tmp)

 # write.csv(comp_complex,"comp_complex_max.csv")
 # ----------------------Component-Complexity----------------------

# ----------------------Component-Complexity----------------------

# write.csv(Root.Cause.Category_prop,"Root.Cause.Category_prop.csv")
# write.csv(Root.Cause.Category_prop_max,"Root.Cause.Category_prop_max.csv")
# write.csv(Root.Cause.Category_max,"Root.Cause.Category_max.csv")
rm(tmp)
tmp=data.frame(Raw_data$Component,Raw_data$`Application Category`)
comp_app_cat=unique(tmp)

# write.csv(comp_app_cat,"comp_app_cat.csv")
# ----------------------Component-Complexity----------------------

########################### Combining Old & New Data ###########################

data_old=subset(data_agg1,select=-c(year,week_number,combine))
# write.csv(data_old,"data_old.csv")
# rm(data_tmp)
################## Modifying new data in old data format ##################
data_temp=subset(new_data_set,select=c(Project,Proj.CMMI,Planned.Duration.in.weeks,Component,Application.Version))
data_tmp1=unique(data_temp)
Actual_duration <- round(mean(as.numeric(projectProfile$actualEndWeek)))

data_tmp2=data_tmp1[rep(seq_len(nrow(data_tmp1)), each=Actual_duration-trainig_week),]

data_tmp3_vali=cbind(data_tmp2,do.call(paste0, data_tmp2[c(1:ncol(data_tmp2))]))

# write.csv(data_tmp3_vali,"test1.csv")
data_tmp3_vali[,ncol(data_tmp3_vali)+1]=trainig_week+1 #(70 = last column +1)
colnames(data_tmp3_vali)[ncol(data_tmp3_vali)]="wk"

for (i in 2:nrow(data_tmp3_vali))
{
  if(data_tmp3_vali[i,ncol(data_tmp3_vali)-1]==data_tmp3_vali[i-1,ncol(data_tmp3_vali)-1]){
    data_tmp3_vali[i,ncol(data_tmp3_vali)]=data_tmp3_vali[i-1,ncol(data_tmp3_vali)]+1
  }else{
    data_tmp3_vali[i,ncol(data_tmp3_vali)]=trainig_week+1
  }
  
}
# write.csv(data_tmp3_vali,"data_temp3.csv")
data_tmp=merge(data_tmp3_vali,phase_max[,c(1,3)],by="wk")
data_tmp1=merge(data_tmp,test_case_max[,c(1,3)],by="wk")
# write.csv(data_tmp1,"test.csv")
rm(data_tmp2)
data_tmp2=merge(data_tmp1,dev_team_max[,c(1,3)],by="wk")
# rm(data_tmp)
# rm(data_tmp1)
# rm(data_tmp2)

data_tmp=merge(data_tmp2,test_team_max[,c(1,3)],by="wk")
## Check sev_max
data_tmp1=merge(data_tmp,sev_max[,c(1,3)],by="wk")
# write.csv(data_tmp1,"data_tmp1.csv")
setnames(comp_complex, old = c('data_agg1.Component','data_agg1.Complexity'), 
         new = c('Component','Complexity'))


comp_complex <- mutate_each(comp_complex, funs(toupper))
data_tmp2=merge(data_tmp1,comp_complex,by="Component")
data_tmp3=merge(data_tmp2,Root.Cause.Category_max[,c(1,3)],by="wk")
# write.csv(data_tmp3,"data_tmp3.csv")
# write.csv(data_tmp2,"data_tmp2.csv")
# 
# write.csv(comp_complex,"comp_complex.csv")
# str(Root.Cause.Category_max)
data_tmp3$defect_agg=0
data_formated_new=data_tmp3[,c(3,4,8,9,5,10,11,2,6,13,12,14,15,1)]
data_formated_new$Flag="V"
data_old$Flag="T"
# write.csv()
rm(data_agg1)
data_agg1=rbind(data_old,data_formated_new)

################## Modifying new data in old data format ##################
########################### Combining Old & New Data ###########################

# write.csv(data_formated_new,"data_formated_new.csv")
#-------------------- Dummy Variable Creation --------------------
rm(data_tmp)
data_tmp <- mutate_each(data_agg1, funs(toupper))
# write.csv(data_tmp,"temp.csv")


#-------------------- Dummy Variable Creation for Training--------------------

# 8 relase
for(level in unique(data_tmp$Project)){
  data_tmp[paste("dummyProject", level, sep = "_")] <- ifelse(data_tmp$Project == level, 1, 0)
}

#9 Phase
for(level in unique(data_tmp$Phase)){
  data_tmp[paste("dummyPhase", level, sep = "_")] <- ifelse(data_tmp$Phase == level, 1, 0)
}
# write.csv(data_tmp,"test_agg.csv")


# Creating Dummies for Severity #4 Severity
for(level in unique(data_tmp$Severity)){
  data_tmp[paste("dummySeverity", level, sep = "_")] <- ifelse(data_tmp$Severity == level, 1, 0)
}

# Creating Dummies for Component #16

for(level in unique(data_tmp$Component)){
  data_tmp[paste("dummyComponent", level, sep = "_")] <- ifelse(data_tmp$Component == level, 1, 0)
}

# Creating Dummies for Root Cause Modified #10
for(level in unique(data_tmp$Root.Cause.Category_NG)){
  data_tmp[paste("dummyRoot Cause Category", level, sep = "_")] <- ifelse(data_tmp$Root.Cause.Category_NG == level, 1, 0)
}
# str(data_tmp)
#-------------------- Dummy Variable Creation for Training--------------------

data_tmp$defect_agg=as.numeric(data_tmp$defect_agg)
data_tmp$Proj.CMMI=as.numeric(data_tmp$Proj.CMMI)
data_tmp$No..of.Test.Case.Executed=as.numeric(data_tmp$No..of.Test.Case.Executed)
data_tmp$Planned.Duration.in.weeks=as.numeric(data_tmp$Planned.Duration.in.weeks)
data_tmp$Development.Team.Size..For.Release=as.numeric(data_tmp$Development.Team.Size..For.Release)
data_tmp$Testing.Team.Size..For.Release=as.numeric(data_tmp$Testing.Team.Size..For.Release)
data_tmp$wk=as.numeric(data_tmp$wk)
data_tmp$Complexity=as.numeric(data_tmp$Complexity)
data_tmp$Application.Version=1 # Coded to 1 as only 1 application version available in data

data_tmp$log_TestCases=log(data_tmp$No..of.Test.Case.Executed)
data_tmp$log_plannedDuration=log(data_tmp$Planned.Duration.in.weeks)
data_tmp$log_DevTeam=log(data_tmp$Development.Team.Size..For.Release)
data_tmp$log_testTeam=log(data_tmp$Testing.Team.Size..For.Release)
data_tmp$log_complexity=log(data_tmp$Complexity)
data_tmp$defect_agg=as.numeric(data_tmp$defect_agg)

# write.csv(data_tmp,"temp_nil.csv")
# train=data_tmp[which(data_tmp$wk<=trainig_week & data_tmp$Flag=="T"),][,c(13,14,16,20,27,28,29,31,33,34,38,45,53,55,56,57,59,63,66)]
train=data_tmp[which(data_tmp$wk<=trainig_week & data_tmp$Flag=="T"),][,c(13,14,16:67)]

# write.csv(train,"train.csv")
# rm(data_tmp1)
data_tmp1=data_tmp[which(data_tmp$Flag=="V"),]
# write.csv(data_tmp1,"data_tmp1.csv")
# vali=data_tmp1[,c(13,14,16,20,27,28,29,31,33,34,38,45,53,55,56,57,59,63,66)]
vali=data_tmp1[,c(13,14,16:67)]
# write.csv(data_tmp1,"data_tmp1.csv")

# 33050
# df1=data.frame(seed=-9,MAPE0=-9)

 # for (i in c(1:1000))
 # {
 #   print(i)
 #   # seed=30000+100*i
 #   seed=19000+100*i


  seed=19000
# seed=57900  
set.seed(seed)
  # print(i)
  # fit <- nnet(defect_agg~., train, size=3, maxit=500, linout=T, decay=0.01)
  # predictions1 <- predict(fit, vali, type="raw")
  # predictions=round(predictions1,0)
  # t1 <- as.matrix(train[,-1])
  # t2 <- as.matrix(train[,1])
  # dnn_mod <- dbn.dnn.train( t1, t2, activationfun = "tanh")
  # # dnn_mod <- dbn.dnn.train( t1, t2,hidden = 13, activationfun = "tanh")
  # vali1=as.matrix(vali[,-1])
  # pred_training <- nn.predict(dnn_mod, vali1) 
  # 
  # predictions=round(pred_training,0)
  tuneResult1 <- tune(svm, defect_agg ~ .,  data = train,
                      ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
  )

  tunedModel1=tuneResult1$best.model
  tunedModelY1 <- predict(tunedModel1, vali) 
  data_svm_predict=data.frame(data_tmp1,round(tunedModelY1,0))
  # write.csv(data_svm_predict,"svm_preict.csv")
  
  
  # rm(pred)
  # pred=data.frame(data_tmp1,pred=predictions)
  # write.csv(pred,"pred11.csv")
  # 
  ################################## Mongo Collection##########################
  #con <- mongo(collection = "cognitive_input", db = "idp", url = "mongodb://169.44.126.89:27017")
  #data_pull<-con$find()
  #con$drop()
  #cognitive_input<-read.csv('data.csv',sep=",",header = T,check.names = FALSE)
  #con$insert(cognitive_input)
  
  #Phase map
  con <- mongo(collection = "Phase_Map", db = "idp", url = "mongodb://169.44.126.89:27017")
  Phase_Map<-con$find()
  
  #con$drop()
  #Phase_Map<-read.csv('Phase_Map.csv',sep=",",header = T,check.names = FALSE)
  #con$insert(Phase_Map)
  
  #Severity map
  con <- mongo(collection = "Severity_Map", db = "idp", url = "mongodb://169.44.126.89:27017")
  Severity_Map<-con$find()
  #con$drop()
  # Severity_Map<-read.csv('Severity_Map.csv',sep=",",header = T,check.names = FALSE)
  # con$insert(Severity_Map)

  #Root Cause map
  con <- mongo(collection = "Root_Cause_Map", db = "idp", url = "mongodb://169.44.126.89:27017")
  Root_Cause_Map<-con$find()
  #con$drop()
  #Root_Cause_Map<-read.csv('Root_Cause_Map.csv',sep=",",header = T,check.names = FALSE)
  #con$insert(Root_Cause_Map)
  
  
  ################################## Mongo Collection##########################
  
  ##################################
  #change names
  ##################################
  # rm(final1.1)
  final1.1 = data_svm_predict
  colnames(final1.1)[ncol(final1.1)]="pred"
  # str(final1.1)
  # write.csv(final1.1,"final1.1.csv")
  final1.1$defect_agg = 0
  # rm(final1.2)
  final1.2 <- new_data_set
  # write.csv(final1.2,"final1.2.csv")
  final1.2$pred <- 0
  common_cols <- intersect(colnames(final1.1), colnames(final1.2))
  final1.3 <- rbind(
    subset(final1.1, select = common_cols), 
    subset(final1.2, select = common_cols)
  )
  # write.csv(final1.3,"final1.3.csv")
  # rm(final1)
  final1=final1.3 # Created by NC. Need to validate by SG
  # final1$pred = ifelse(final1.3$wk <= trainig_week, final1.3$defect_agg,final1.3$pred)
  final1$pred = ifelse(final1.3$wk <= trainig_week, final1.3$defect_agg,final1.3$pred)
  write.csv(final1,"final1.csv")
  
  # names(final1) <- gsub(".", "", names(final1), fixed = TRUE)
  # Removed ,'year','week_number'
  setnames(final1, old = c('Project','Proj.CMMI','Phase','No..of.Test.Case.Executed','Planned.Duration.in.weeks','Development.Team.Size..For.Release','Testing.Team.Size..For.Release','Component','Application.Version','Complexity','Severity','Root.Cause.Category_NG','defect_agg','wk','pred'), 
           new = c('projectName','maturity','phase_key','noOfExecutedTestCases','actualEndWeek','developerTeamSize','testTeamSize','application','appVersion','Complexity','severity_key','rootCause','actualNoDefects','weekNumber','predictNoDefects'))
  # str(final1)
  # write.csv(final1,"final13.csv")
  rm(final2)
  final1=within(final1, rootCause[rootCause == 'BUILD/PACKAGE'] <- 'Build')
  final1[c("rootCause")][is.na(final1[c("rootCause")])] <- 'non-classified' # Replace NA with non-classified
  # write.csv(final1,"final13.csv") 
  final2 <- final1[,c('projectName','maturity','phase_key','noOfExecutedTestCases','actualEndWeek','developerTeamSize','testTeamSize','application','appVersion','Complexity','severity_key','rootCause','actualNoDefects','weekNumber','predictNoDefects')]
  
  final2$phase_key<-tolower(final1$phase_key)
  final2$rootCause<-tolower(final1$rootCause)
  final2$severity_key<-tolower(final1$severity_key)
  Phase_Map$Phase1<-tolower(Phase_Map$Phase1)
  Root_Cause_Map$Root_Cause2<-tolower(Root_Cause_Map$Root_Cause2)
  Severity_Map$Severity1<-tolower(Severity_Map$Severity1)
  
  setnames(Phase_Map, old = ,c('Phase1','Phase2'),new = c('phase_key','phase'))
  setnames(Root_Cause_Map, old = ,c('Root_Cause1','Root_Cause2','Value'),new = c('rootCause_key','rootCause','value'))
  setnames(Severity_Map, old = ,c('Severity1','Severity2'),new = c('severity_key','severity'))
  
  # str(final3.1)
  # str(Root_Cause_Map)
  # rm(final3.3)
  write.csv(final3.1,"final3.1.csv")
  write.csv(Root_Cause_Map,"Root_Cause_Map.csv")
  
  final3.1 <- left_join(final2, Phase_Map, by='phase_key')
  final3.2 <- left_join(final3.1, Root_Cause_Map, by='rootCause')
  
  final3.3 <- left_join(final3.2,Severity_Map, by='severity_key')
  # write.csv(final3.3,"final3.3_jan2.csv")
  
  final3 <- final3.3[,c('projectName','application','weekNumber','severity','phase','rootCause','value','actualNoDefects','predictNoDefects')]
  
  final3=within(final3, application[application == 'SMARTAPP SUITE - 4400'] <- 'SmartApp Suite - 4400')
  final3=within(final3, phase[phase == 'Functional'] <- '5.2-Functional')
  final3=within(final3, phase[phase == 'Analysis'] <- '2-Analysis')
  final3=within(final3, phase[phase == 'Design'] <- '3-Design')
  final3=within(final3, phase[phase == 'Unit Test'] <- '4.1-Unit Test')
  final3=within(final3, phase[phase == 'Integration Test'] <- '4.2-Integration Test')
  final3=within(final3, phase[phase == 'Regression'] <- '5.4-Regression')
  final3=within(final3, phase[phase == 'UAT'] <- '5.5-UAT')
  final3=within(final3, phase[phase == 'Post-Deployment'] <- '6-Post-Deployment')
  final3$releaseName <- 2.0
  write.csv(final3,"final33 3rd Jan.csv")
  ##################################
  #current week
  ##################################
  final3$currentWeek = ifelse(final3$weekNumber == trainig_week , 1,0)
  
  con <- mongo(collection = "defectPredictFact", db = "idp", url = "mongodb://169.44.126.89:27017")
  if(con$count() != 0){
    con$drop()
    con$insert(final3)
  } else {
    con$insert(final3)
  }
  
  con <- mongo(collection = "defectPredictFact", db = "idp", url = "mongodb://169.44.126.89:27017")
  Predict_Map<-con$find()
  write.csv(Predict_Map,"Predict_Map.csv")
  