Azure<- FALSE
if(Azure==FALSE)
{
  data<- read.csv(file = "C:\\IHFD2014.csv")
}else
{
  data <- maml.mapInputPort(1) 
}

#Pruning irrelevat features
#$Hosp<- NULL
data$MRN_ENC<- NULL
#data$Resid<- NULL
data$DisStatus<- NULL
#data$DischargeCode<- NULL
data$AdmSource<- NULL
data$AdmType<- NULL
data$AdmSource<- NULL
data$PrivDays<- NULL
data$PubDays<- NULL
data$SemiPrivDays<- NULL
data$TransInHosp<- NULL
data$TransOutHosp<- NULL
#data$Diag1<- NULL
data$ADM_TRAUMA_DATE<- NULL
data$ADM_TRAUMA_TIME<- NULL
data$ADM_TRAUMA_TYPE<- NULL
data$ADM_VIA_ED<- NULL
data$ADM_AE_DATE<- NULL
#data$ADM_AE_TIME<- NULL
data$ADM_AE_DIS_DATE<- NULL
data$ADM_AE_DIS_TIME<- NULL
data$ADM_ESTIMATED_DISDTE<- NULL
data$ADM_PRE_FRACTURE_MOBILITY<- NULL
data$ADM_GER_ASSESS_DATE<- NULL
data$ADM_PRIMARY_SURGERY_TIME<- NULL
data$ADM_SPEC_FALLS_ASSESS<- NULL
data$ADM_MULTI_REHAB_ASSESS<- NULL

#Renaming columns

colnames(data)[colnames(data)=="AdmDate"] <- "Admission_Date"
colnames(data)[colnames(data)=="DisDate"] <- "Discharge_Date"
colnames(data)[colnames(data)=="ProcDate1"] <- "Procedure_Date"
colnames(data)[colnames(data)=="ADM_FIRST_PRES_HOSP_ADM_DATE"] <- "Presentation_Date"
colnames(data)[colnames(data)=="ADM_PRIMARY_SURGERY_DATE"] <- "Surgery_Date"
colnames(data)[colnames(data)=="ADM_FIRST_PRES_HOSP_ADM_TIME"] <- "Presentation_Time"
colnames(data)[colnames(data)=="ADM_FRACTURE_TYPE"] <- "Fracture_Type"
colnames(data)[colnames(data)=="ADM_FRAGILITY"] <- "Fragility"
colnames(data)[colnames(data)=="ADM_AE_TIME"] <- "Admission_Time"
colnames(data)[colnames(data)=="DischargeCode"] <- "Discharge_Destination"


#Formatting date values
data$Admission_Date <- as.Date(data$Admission_Date,"%d-%m-%y")
data$Discharge_Date <- as.Date(data$Discharge_Date,"%d-%m-%y")
data$Procedure_Date <- as.Date(data$Procedure_Date,"%d-%m-%y")
data$Presentation_Date <- as.Date(data$Presentation_Date,"%d-%m-%y")
data$Surgery_Date <- as.Date(data$Surgery_Date,"%d-%m-%y")

#Replacing Fragility missing and undocumented with 2 as "No history of fragility"
index <- data$Fragility == 9 #9 refers to undocumented
data$Fragility[index] <- 2

index <- is.na(data$Fragility) #na exists when the value is missing
data$Fragility[index] <- 9

#replace surgery with procedure date if null
index<- is.na(data$Surgery_Date)
data$Surgery_Date[index] <- data$Procedure_Date[index]



data$ActualLOS<- as.integer(difftime( data$Discharge_Date ,data$Surgery_Date,units = "days"))
#Replacing null actual LOS
index<-is.na(data$ActualLOS)
data$ActualLOS[index] <- data$LOS[index]

#Replacing zeros with ones
index<-data$ActualLOS ==0
data$ActualLOS[index] <- data$LOS[index]

#replacing negatives with original LOS
index<-data$ActualLOS <0
data$ActualLOS[index] <- 1
#Replacing if actual LOS > original LOS
index<-data$ActualLOS > data$LOS
data$ActualLOS[index] <- data$LOS[index]



#columns to be computed
#1st, Time TO Surgery
data$TimeToSurgery<- as.integer(difftime(data$Surgery_Date ,data$Presentation_Date ,units = "days"))

#Inspecting which samples has null time to surgery due to that the presentation date is null
#Then replacing the presentation time with the admission time
index<- is.na(data$TimeToSurgery)
data$TimeToSurgery[index]<-difftime(data$Surgery_Date[index] ,data$Admission_Date[index] ,units = "days")
#In case that the surgery and procedure dates are null
#Then set the Time To Suregery as the mean
index<- is.na(data$TimeToSurgery)
data$TimeToSurgery[index]<-round(mean(!is.na(data$TimeToSurgery )))

#Formatting Time values of presentation time
index<- is.na(data$Presentation_Time)
data$Presentation_Time[index]<-"NO TIME"


index<-nchar(data$Presentation_Time)==3# if the time misses zero at the end
data$Presentation_Time[index]<-paste(data$Presentation_Time[index],"0",sep = "" )

index<-nchar(data$Presentation_Time)==2# if the time misses two zeros at the end
data$Presentation_Time[index]<-paste(data$Presentation_Time[index],"00",sep = "" )

index<- data$Presentation_Time!="NO TIME"
data$Presentation_Time[index]<-paste(substring(data$Presentation_Time[index],1,2),":",substring(data$Presentation_Time[index],3),sep = "" )



#Checking if adding zero in the end was wrong and it should have been added in the begining
index<- which(nchar(data$Presentation_Time)>2 & as.integer(substring(data$Presentation_Time,1,2))>23)
data$Presentation_Time[index]<-paste("0",substring(data$Presentation_Time[index],1,1),":",substring(data$Presentation_Time[index],2,2),substring(data$Presentation_Time[index],4,4),sep = "" )

#Check again length of time string
index<-nchar(data$Presentation_Time)==3# if the time misses zero at the end
data$Presentation_Time[index]<-paste(data$Presentation_Time[index],"0",sep = "" )

index<-nchar(data$Presentation_Time)==2# if the time misses two zeros at the end
data$Presentation_Time[index]<-paste(data$Presentation_Time[index],"00",sep = "" )



#Formatting Time values of  admission time
index<- is.na(data$Admission_Time)# if the time is null
data$Admission_Time[index]<- "NO TIME"

index<-nchar(data$Admission_Time)==1# if the time misses zeros at the end
data$Admission_Time[index]<-paste(data$Admission_Time[index],"000",sep = "" )

index<-nchar(data$Admission_Time)==3# if the time misses zero at the end
data$Admission_Time[index]<-paste(data$Admission_Time[index],"0",sep = "" )

index<-nchar(data$Admission_Time)==2# if the time misses two zeros at the end
data$Admission_Time[index]<-paste(data$Admission_Time[index],"00",sep = "" )

index<- data$Admission_Time!="NO TIME"
data$Admission_Time[index]<-paste(substring(data$Admission_Time[index],1,2),":",substring(data$Admission_Time[index],3),sep = "" )



#Checking if adding zero in the end was wrong and it should have been added in the begining
index<- which(nchar(data$Admission_Time)>2 & as.integer(substring(data$Admission_Time,1,2))>23)
data$Admission_Time[index]<-paste("0",substring(data$Admission_Time[index],1,1),":",substring(data$Admission_Time[index],2,2),substring(data$Admission_Time[index],4,4),sep = "" )

#Check again length of time string
index<-nchar(data$Admission_Time)==3# if the time misses zero at the end
data$Admission_Time[index]<-paste(data$Admission_Time[index],"0",sep = "" )

index<-nchar(data$Admission_Time)==2# if the time misses two zeros at the end
data$Admission_Time[index]<-paste(data$Admission_Time[index],"00",sep = "" )


#Adding Presentation Date Time column
data$Pres_DateTime<-0

index<- !is.na(data$Presentation_Date) & data$Presentation_Time !="NO TIME"
data$Pres_DateTime[index]<- paste(data$Presentation_Date[index]," ",data$Presentation_Time[index], sep = "")

#Adding Admission Date Time column
data$Adm_DateTime<-0
index<- data$Admission_Time!="NO TIME"
data$Adm_DateTime[index]<- paste(data$Admission_Date[index]," ",data$Admission_Time[index], sep = "")

#Adding Time To Admission column
data$TimeToAdmission<-0 #Initializing column
index<- data$Pres_DateTime!=0 & data$Adm_DateTime!=0

data$TimeToAdmission[index]<- difftime(data$Adm_DateTime[index],data$Pres_DateTime[index],units="hours")



#Mapping the codes of fracture types 
index <- data$Fracture_Type==1
data$Fracture_Type[index] <- "Intracapsular-Displaced"
index <- data$Fracture_Type==2
data$Fracture_Type[index] <- "Intracapsular-Undisplaced"
index <- data$Fracture_Type==3
data$Fracture_Type[index] <- "Intertrochanteric"
index <- data$Fracture_Type==4
data$Fracture_Type[index] <- "Subtrochanteric"
index <- data$Fracture_Type==8
data$Fracture_Type[index] <- "Other"
index <- data$Fracture_Type==9
data$Fracture_Type[index] <- "Not Documented"


#Mapping the codes of patient's gender
index <- data$Sex==1
data$Sex[index] <- "Male"
index <- data$Sex==2
data$Sex[index] <- "Female"

#Mapping patient's fragility history
index <- data$Fragility==1
data$Fragility[index] <- "YES"
index <- data$Fragility==2
data$Fragility[index] <- "NO"

#Mapping destination discharge codes
index <- data$Discharge_Destination==0 | data$Discharge_Destination==1 | data$Discharge_Destination==14 | data$Discharge_Destination==15
data$Discharge_Destination[index] <- "Home"

index <- data$Discharge_Destination==2 | data$Discharge_Destination==10 | data$Discharge_Destination==11
data$Discharge_Destination[index] <- "Nursing Home"

index <- data$Discharge_Destination==6 | data$Discharge_Destination==7
data$Discharge_Destination[index] <- "Mortality"

index <- data$Discharge_Destination==3 | data$Discharge_Destination==4 |  data$Discharge_Destination==5 |  data$Discharge_Destination==9
data$Discharge_Destination[index] <- "Transfer to Hospital"


#Removing outliers
#Include those of LOS <=60
data<-data[data$ActualLOS <=60, ]

if(Azure==TRUE){
  maml.mapOutputPort("data");
}