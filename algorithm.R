#Load necessary packages

require(readstata13)
require(data.table)

################################
#Read in different data sources#
################################

#1) Hospital episodes statistics 
baseline <- fread("S:/FPHS_IHI_DataLab_UKBiobank/application-992/baseline/November2016/ukb8413.csv",
                  select = c("eid", "53-0.0"), sep=",")
hes <- read.dta13("S:/FPHS_IHI_DataLab_UKBiobank/application-992/HES/November2016/hesinNov2016.dta")
baseline$eid<-as.numeric(baseline$eid)

#Merge the files and sort out the dates of admission and enrollment

dataset<-merge(baseline,hes,by="eid")
dataset$date<-as.Date(dataset$"53-0.0")
dataset$epistart <- as.Date(dataset$epistart)
dataset$admidate <- as.Date(dataset$admidate)


dataset$datediff1<-dataset$epistart-dataset$date
dataset$numericdiff1<-as.numeric(dataset$datediff1, units="days")

dataset$datediff2<-dataset$admidate-dataset$date
dataset$numericdiff2<-as.numeric(dataset$datediff2, units="days")

#2) Mortality register

death <- fread("S:/FPHS_IHI_DataLab_UKBiobank/application-992/baseline/November2016/ukb8413.csv",
               select = c(1,1642:1686,1639), sep=",")

#3) Self-reported operations

selfreportops <- fread("S:/FPHS_IHI_DataLab_UKBiobank/application-992/baseline/November2016/ukb8413.csv",
                       select = c(1, 783:814), sep=",")
selfreportops$eid<-as.numeric(selfreportops$eid)


#4) Self-reported illnesses

selfreport <- fread("S:/FPHS_IHI_DataLab_UKBiobank/application-992/baseline/November2016/ukb8413.csv",
                      select = c(1, 446:457,552:580,290,317), sep=",")


#5) Self-reported medications

medications<- fread("S:/FPHS_IHI_DataLab_UKBiobank/application-992/baseline/November2016/ukb8413.csv",
                    select = c(1,639:686), sep=",")



#############
# ALGORITHM #
#############

output<-baseline[,c(1)]
######################################
#First row: I21 in secondary care EHR#
######################################

#a) Inclusion criteria
#I21 in secondary care EHR
i21<-c("I210", "I211", "I212", "I213", "I214", "I219")
subsqmi<-c("I220", "I221", "I228", "I229")
oldmi<-c("I252")

all<-c("I210", "I211", "I212", "I213", "I214", "I219",
       "I220", "I221", "I228", "I229", "I252")

incl<-dataset[which(dataset$diag_icd10%in%i21),]
row1<-dataset[which(dataset$eid%in%incl$eid),]

#b) Level I

#Output
output$Level1[output$eid%in%row1$eid]<-"MI"

#c) Level II

#Any incident MIs
anyinc<-row1[which(row1$diag_icd10%in%all &numericdiff1>0
                   |row1$diag_icd10%in%all &numericdiff2>0),]


#Any prevalent MIs
anyprev<-row1[which(row1$diag_icd10%in%all &numericdiff1<=0 
                    |row1$diag_icd10%in%all &numericdiff2<=0),]

#Only incident MIs

onlyinc<-anyinc[which(!anyinc$eid%in%anyprev$eid),]

#Only prevalent MIs

onlyprev<-anyprev[which(!anyprev$eid%in%anyinc$eid),]

#Output
output$Level2[output$eid%in%anyinc$eid
              &output$eid%in%anyprev$eid]<-"Incident & prevalent MI"

output$Level2[output$eid%in%onlyinc$eid]<-"Incident MI"
output$Level2[output$eid%in%onlyprev$eid]<-"Prevalent MI"


#d) Level III

#Single or  multiple incident only

#More than one new MI code separated by 28 days or more

list<-unique(onlyinc$eid)
checki<-list()

for (i in 1:length(list)) {
  temp<-paste(list[i])
  checki[[i]]<-onlyinc[which(onlyinc$eid==temp),]
  checki[[i]]$max1<-max(checki[[i]]$numericdiff1)-min(checki[[i]]$numericdiff1)
  checki[[i]]$max2<-max(checki[[i]]$numericdiff2)-min(checki[[i]]$numericdiff2)
}


temp<-do.call(rbind.data.frame, checki)
mult1<-temp[which(temp$max1>=28 | temp$max2>=28),]

#Any subsequent MI code

mult2<-onlyinc[which(onlyinc$diag_icd10%in%subsqmi),]

#An old MI code at the same time as MI code or 7 days or less after
incl<-onlyinc[which(onlyinc$diag_icd10%in%oldmi),]
temp<-onlyinc[which(onlyinc$eid%in%incl$eid),]

list<-unique(temp$eid)
test<-list()

for (i in 1:length(list)) {
  id<-paste(list[i])
  test[[i]]<-temp[which(temp$eid==id),]
  #If more than two entries they have multiple MI anyway
  test[[i]]$yes[nrow(test[[i]]>2)]<-1
  test[[i]]$max1<-max(test[[i]]$numericdiff1)-min(test[[i]]$numericdiff1)
  test[[i]]$max2<-max(test[[i]]$numericdiff2)-min(test[[i]]$numericdiff2)
  test[[i]]$yes[test[[i]]$max1<8 | test[[i]]$max2<8]<-1
}
mult3a<-do.call(rbind.data.frame, test)
mult3b<-mult3a[which(mult3a$yes==1),]
multinc<-output[which(output$eid%in%mult1$eid 
                      |output$eid%in%mult2$eid
                      |output$eid%in%mult3b$eid),]

#Single or  multiple prevalent only

#More than one new MI code separated by 28 days or more

list<-unique(onlyprev$eid)
checkp<-list()

for (i in 1:length(list)) {
  temp<-paste(list[i])
  checkp[[i]]<-onlyprev[which(onlyprev$eid==temp),]
  checkp[[i]]$max1<-max(checkp[[i]]$numericdiff1)-min(checkp[[i]]$numericdiff1)
  checkp[[i]]$max2<-max(checkp[[i]]$numericdiff2)-min(checkp[[i]]$numericdiff2)
}


temp<-do.call(rbind.data.frame, checkp)
mult1<-temp[which(temp$max1>=28 | temp$max2>=28),]

#Any subsequent MI code

mult2<-onlyprev[which(onlyprev$diag_icd10%in%subsqmi),]

#An old MI code at the same time as MI code or 7 days or less after
incl<-onlyprev[which(onlyprev$diag_icd10%in%oldmi),]
temp<-onlyprev[which(onlyprev$eid%in%incl$eid),]

list<-unique(temp$eid)
test<-list()

for (i in 1:length(list)) {
  id<-paste(list[i])
  test[[i]]<-temp[which(temp$eid==id),]
  #If more than two entries they have multiple MI anyway
  test[[i]]$yes[nrow(test[[i]]>2)]<-1
  test[[i]]$max1<-max(test[[i]]$numericdiff1)-min(test[[i]]$numericdiff1)
  test[[i]]$max2<-max(test[[i]]$numericdiff2)-min(test[[i]]$numericdiff2)
  test[[i]]$yes[test[[i]]$max1<8 | test[[i]]$max2<8]<-1
}
mult3a<-do.call(rbind.data.frame, test)
mult3b<-mult3a[which(mult3a$yes==1),]
multprev<-output[which(output$eid%in%mult1$eid 
                      |output$eid%in%mult2$eid
                      |output$eid%in%mult3b$eid),]

#Output
output$Level3[output$eid%in%row1$eid]<-"Single MI"
output$Level3[output$Level2=="Incident & prevalent MI"
              |output$eid%in%multinc$eid
              |output$eid%in%multprev$eid]<-"Multiple MI"




########################################
#Second row: Other secondary care codes#
########################################

#a) Inclusion criteria
#ICD codes
codes<-c("I231", "I236", "I238", "I241", "I252")

row2<-dataset[which(dataset$diag_icd10%in%codes
                    &!dataset$eid%in%row1$eid),]

#b) Level I
#Output
output$Level1[output$eid%in%row2$eid]<-"Possible MI"

#c) Level II

#Any incident MIs
anyinc<-row2[which(row2$diag_icd10%in%codes &numericdiff1>0
                   |row2$diag_icd10%in%codes &numericdiff2>0),]


#Any prevalent MIs
anyprev<-row2[which(row2$diag_icd10%in%codes &numericdiff1<=0 
                    |row2$diag_icd10%in%codes &numericdiff2<=0),]

#Only incident MIs

onlyinc<-anyinc[which(!anyinc$eid%in%anyprev$eid),]

#Only prevalent MIs

onlyprev<-anyprev[which(!anyprev$eid%in%anyinc$eid),]

#Output
output$Level2[output$eid%in%anyinc$eid
              &output$eid%in%anyprev$eid]<-"Incident & prevalent MI"

output$Level2[output$eid%in%onlyinc$eid]<-"Incident MI"
output$Level2[output$eid%in%onlyprev$eid]<-"Prevalent MI"


#d) Level III

#Single or  multiple incident only

#Multiple entries

list<-unique(onlyinc$eid)
checki<-list()

for (i in 1:length(list)) {
  temp<-paste(list[i])
  checki[[i]]<-onlyinc[which(onlyinc$eid==temp),]
  checki[[i]]$max1<-max(checki[[i]]$numericdiff1)-min(checki[[i]]$numericdiff1)
  checki[[i]]$max2<-max(checki[[i]]$numericdiff2)-min(checki[[i]]$numericdiff2)
  checki[[i]]$yes<-0
  checki[[i]][checki[[i]]$max1>=28 
              |checki[[i]]$max2>=28]$yes<-1
}


temp<-do.call(rbind.data.frame, checki)

multinc<-temp[which(temp$yes==1),]

#Single or  multiple prevalent only

#Multiple entries

list<-unique(onlyprev$eid)
checkp<-list()

for (i in 1:length(list)) {
  temp<-paste(list[i])
  checkp[[i]]<-onlyprev[which(onlyprev$eid==temp),]
  checkp[[i]]$max1<-max(checkp[[i]]$numericdiff1)-min(checkp[[i]]$numericdiff1)
  checkp[[i]]$max2<-max(checkp[[i]]$numericdiff2)-min(checkp[[i]]$numericdiff2)
  checkp[[i]]$yes<-0
  checkp[[i]][checkp[[i]]$max1>=28 
              |checkp[[i]]$max2>=28]$yes<-1
}


temp<-do.call(rbind.data.frame, checkp)

multprev<-temp[which(temp$yes==1),]


#Output
output$Level3[output$eid%in%row2$eid]<-"Single MI"
output$Level3[output$Level2=="Incident & prevalent MI"
              |output$eid%in%multinc$eid
              |output$eid%in%multprev$eid]<-"Multiple MI"



###########
#Third row#
###########

#a) Inclusion criteria
#ICD codes
mideath<-c("I210", "I211", "I212", "I213", "I214", "I219",
                   "I252")
death$mideath<-apply(death, 1, function(r) any(r %in% mideath))
death$i21<-apply(death, 1, function(r) any(r %in% i21))

row3<-death[which(death$mideath==1
                  &!death$eid%in%row1$eid
                  &!death$eid%in%row2$eid),]

#b) Level I
#Output
output$Level1[output$eid%in%row3$eid]<-"MI"

#c) Level II

#Have I21

havei21<-row3[which(row3$i21==TRUE),]
#Non I21

noni21<-row3[which(row3$i21==FALSE),]

#I21 as primary cause of death

primary<-row3[,c(1:4)]
primary$i21primary<-apply(primary, 1, function(r) any(r %in% i21))

#Output

output$Level2[output$eid%in%row3$eid
              &!output$eid%in%noni21$eid]<-"Incident MI"

output$Level2[output$eid%in%noni21$eid]<-"Unknown date MI"

#d) Level III
#Single or Multiple MI
havei21$multiple<-apply(havei21, 1, function(r) any(r %in% "I252"))
mortmult<-havei21[which(havei21$multiple==TRUE),]

#Output
output$Level3[output$eid%in%row3$eid]<-"Single MI"
output$Level3[output$eid%in%mortmult$eid]<-"Multiple MI"

#############################
#Fourth row: Unstable angina#
#############################

#a) Inclusion
ua<-c("I200")
row4<-dataset[which(dataset$diag_icd10%in%ua
                    &!dataset$eid%in%row1$eid
                    &!dataset$eid%in%row2$eid
                    &!dataset$eid%in%row3$eid),]

#b) Level I
#Output
output$Level1[output$eid%in%row4$eid]<-"CAD"

#c) Level II

#Incident UA
anyinc<-row4[which(row4$diag_icd10%in%ua &numericdiff1>0
                   |row4$diag_icd10%in%ua &numericdiff2>0),]


#Prevalent UA
anyprev<-row4[which(row4$diag_icd10%in%ua &numericdiff1<=0 
                    |row4$diag_icd10%in%ua &numericdiff2<=0),]

#Only incident UA

onlyinc<-anyinc[which(!anyinc$eid%in%anyprev$eid),]

#Only prevalent UA

onlyprev<-anyprev[which(!anyprev$eid%in%anyinc$eid),]

#Output
output$Level2[output$eid%in%onlyprev$eid]<-"Prevalent UA"
output$Level2[output$eid%in%onlyinc$eid]<-"Incident UA"
output$Level2[output$eid%in%anyinc$eid
              & output$eid%in%anyprev$eid]<-"Incident & prevalent UA"
              
##################################################################
#Fifth row
#Revasc in secondary care

#a) Inclusion criteria

cadop<-c("K401", "K402", "K403", "K404", "K409",
         "K411", "K412", "K413", "K414",
         "K424", "K431", "K433", "K439",
         "K441", "K442", "K449",
         "K451", "K452", "K453", "K454", "K455", "K456", "K458", "K459",
         "K461", "K462", "K463", "K464", "K469", "K471",
         "K491", "K492", "K493", "K494", "K498", "K499",
         "K501", "K502", "K503", "K504", "K508", "K509",
         "K751", "K752", "K753", "K754", "K758", "K759")

row5<-dataset[which(dataset$oper4%in%cadop
                    &!dataset$eid%in%row1$eid
                    &!dataset$eid%in%row2$eid
                    &!dataset$eid%in%row3$eid
                    &!dataset$eid%in%row4$eid),]

#b) Level I
#Output
output$Level1[output$eid%in%row5$eid]<-"CAD"

#c) Level II

#Incident revasc
anyinc<-row5[which(row5$oper4%in%cadop &numericdiff1>0
                   |row5$oper4%in%cadop &numericdiff2>0),]


#Prevalent revasc
anyprev<-row5[which(row5$oper4%in%cadop &numericdiff1<=0 
                    |row5$oper4%in%cadop &numericdiff2<=0),]

#Only incident revasc

onlyinc<-anyinc[which(!anyinc$eid%in%anyprev$eid),]

#Only prevalent revasc

onlyprev<-anyprev[which(!anyprev$eid%in%anyinc$eid),]

#Output
output$Level2[output$eid%in%onlyprev$eid]<-"Prevalent revasc"
output$Level2[output$eid%in%onlyinc$eid]<-"Incident revasc"
output$Level2[output$eid%in%anyinc$eid
              & output$eid%in%anyprev$eid]<-"Incident & prevalent revasc"

##################################################################
#Sixth row
#CAD in secondary care

#a) Inclusion criteria

cadicd<-c("Z955", "I250", "I251", "I253", "I254", "I255", "I256", "I258", "I259",
          "I248", "I249", "I240")

row6<-dataset[which(dataset$diag_icd10%in%cadicd
                    &!dataset$eid%in%row1$eid
                    &!dataset$eid%in%row2$eid
                    &!dataset$eid%in%row3$eid
                    &!dataset$eid%in%row4$eid
                    &!dataset$eid%in%row5$eid),]

#b) Level I
#Output
output$Level1[output$eid%in%row6$eid]<-"CAD"

#c) Level II

#Incident revasc
anyinc<-row6[which(row6$diag_icd10%in%cadicd &numericdiff1>0
                   |row6$diag_icd10%in%cadicd &numericdiff2>0),]


#Prevalent revasc
anyprev<-row6[which(row6$diag_icd10%in%cadicd &numericdiff1<=0 
                    |row6$diag_icd10%in%cadicd &numericdiff2<=0),]

#Only incident revasc

onlyinc<-anyinc[which(!anyinc$eid%in%anyprev$eid),]

#Only prevalent revasc

onlyprev<-anyprev[which(!anyprev$eid%in%anyinc$eid),]

#Output
output$Level2[output$eid%in%onlyprev$eid]<-"Prevalent revasc"
output$Level2[output$eid%in%onlyinc$eid]<-"Incident revasc"
output$Level2[output$eid%in%anyinc$eid
              & output$eid%in%anyprev$eid]<-"Incident & prevalent revasc"

##################################################################
#Seventh row
#Self-reported CAD procedures

#a) Inclusion criteria
ops<-c("1070", "1514", "1095")
cabg<-c("1095")
angp<-c("1070")
angg<-c("1514")

selfreportops$incl<-apply(selfreportops, 1, function(r) any(r %in% ops))
selfreportops$cabg<-apply(selfreportops, 1, function(r) any(r %in% cabg))
selfreportops$angioplasty<-apply(selfreportops, 1, function(r) any(r %in% angp))
selfreportops$angiogram<-apply(selfreportops, 1, function(r) any(r %in% angg))


row7<-selfreportops[which(selfreportops$incl==1
                          &!selfreportops$eid%in%row1$eid
                          &!selfreportops$eid%in%row2$eid
                          &!selfreportops$eid%in%row3$eid
                          &!selfreportops$eid%in%row4$eid
                          &!selfreportops$eid%in%row5$eid
                          &!selfreportops$eid%in%row6$eid),]


#b) Level I

cabgyes<-row7[which(row7$cabg==TRUE),]
angpyes<-row7[which(row7$angioplasty==TRUE
                    &row7$cabg==FALSE),]
anggyes<-row7[which(row7$angiogram==TRUE
                    &row7$cabg==FALSE
                    &row7$angioplasty==FALSE),]

#CAD medications at baseline: statins or aspirin


statintypes<-c("1141146234", "1141192414", "1140910632", "1140888594", "1140864592", 
               "1141146138", "1140888648", "1141192410", "1140861958", "1140910654",
               "1140881748", "1141200040", 
               "1140861806", "1140864860", "1140868226", "1140868282", "1140872040",
               "1140882108", "1140882190", "1140882268", "1140882392", "1141163138",
               "1141164044", "1141167844")


medications$statins<-apply(medications, 1, function(r) any(r %in% statintypes))
medsyes<-medications[which(medications$statins==TRUE),]

#Output
output$Level1[output$eid%in%cabgyes$eid]<-"CAD"
output$Level1[output$eid%in%angpyes$eid
              &output$eid%in%medsyes$eid]<-"CAD"

output$Level1[output$eid%in%angpyes$eid
              &!output$eid%in%medsyes$eid]<-"Possible CAD"

output$Level1[output$eid%in%anggyes$eid
              &output$eid%in%medsyes$eid]<-"Possible CAD"

output$Level1[output$eid%in%anggyes$eid
              &!output$eid%in%medsyes$eid]<-"No CAD"

output$Level2[output$eid%in%cabgyes$eid]<-"Prevalent CAD"
output$Level2[output$eid%in%angpyes$eid
              &output$eid%in%medsyes$eid]<-"Prevalent CAD"

output$Level2[output$eid%in%angpyes$eid
              &!output$eid%in%medsyes$eid]<-"Unverifiable prevalent CAD"

output$Level2[output$eid%in%anggyes$eid
              &output$eid%in%medsyes$eid]<-"Unverifiable prevalent CAD"


##################################################################
#Eighth row
#Death records CAD 

#a) Inclusion criteria

cadicd<-c("Z955", "I250", "I251", "I253", "I254", "I255", "I256", "I258", "I259",
            "I248", "I249", "I240")

death$caddeath<-apply(death, 1, function(r) any(r %in% cadicd))

row8<-death[which(death$caddeath==1
                    &!death$eid%in%row1$eid
                    &!death$eid%in%row2$eid
                    &!death$eid%in%row3$eid
                    &!death$eid%in%row4$eid
                    &!death$eid%in%row5$eid
                    &!death$eid%in%row6$eid
                    &!death$eid%in%row7$eid),]
#b) Level I
#Output
output$Level1[output$eid%in%row8$eid]<-"CAD"

#c) Level II
#Primary cause of death
primary<-row8[,c(1:4)]
primary$cadprimary<-apply(primary, 1, function(r) any(r %in% cadicd))
caddeathyes<-primary[which(primary$cadprimary==TRUE),]

#Output
output$Level2[output$eid%in%caddeathyes$eid]<-"CAD death"
output$Level2[output$eid%in%row8$eid
              &!output$eid%in%caddeathyes$eid]<-"Incident CAD"


##################################################################
#Ninth row
#Self-reported MI 
#a) Inclusion

question1<-selfreport[,c(1,14:42)]
miq1<-c("1075")

question1$mi<-apply(question1, 1, function(r) any(r %in% miq1))
temp1a<-question1[which(question1$mi==TRUE),]
temp1b<-temp1a[,c(1)]

question2<-selfreport[,c(1:13)]
miq2<-c("1")

question2$mi<-apply(question2, 1, function(r) any(r %in% miq2))
temp2a<-question2[which(question2$mi==TRUE),]
temp2b<-temp2a[,c(1)]

srmi<-rbind(temp1b,temp2b)

row9<-selfreport[which(selfreport$eid%in%srmi$eid
                  &!selfreport$eid%in%row1$eid
                  &!selfreport$eid%in%row2$eid
                  &!selfreport$eid%in%row3$eid
                  &!selfreport$eid%in%row4$eid
                  &!selfreport$eid%in%row5$eid
                  &!selfreport$eid%in%row6$eid
                  &!selfreport$eid%in%row7$eid
                  &!selfreport$eid%in%row7$eid
                  &!selfreport$eid%in%row8$eid),]

#b) Level I
output$Level1[output$eid%in%row9$eid]<-"Possible MI"

#c) Level II
#3894 age heart attack

output$Level2[output$eid%in%row9$eid]<-"Unverifiable prevalent MI"

##################################################################
#Tenth row
#Self-reported angina
#a) Inclusion

angina1<-c("1074")
question1$angina<-apply(question1, 1, function(r) any(r %in% angina1))
temp1a<-question1[which(question1$angina==TRUE),]
temp1b<-temp1a[,c(1)]

angina2<-c("2")
question2$angina<-apply(question2, 1, function(r) any(r %in% angina2))
temp2a<-question2[which(question2$angina==TRUE),]
temp2b<-temp2a[,c(1)]

srangina<-rbind(temp1b,temp2b)

row10<-selfreport[which(selfreport$eid%in%srangina$eid
                       &!selfreport$eid%in%row1$eid
                       &!selfreport$eid%in%row2$eid
                       &!selfreport$eid%in%row3$eid
                       &!selfreport$eid%in%row4$eid
                       &!selfreport$eid%in%row5$eid
                       &!selfreport$eid%in%row6$eid
                       &!selfreport$eid%in%row7$eid
                       &!selfreport$eid%in%row7$eid
                       &!selfreport$eid%in%row8$eid
                       &!selfreport$eid%in%row9$eid),]

#b) Level I
#Output
output$Level1[output$eid%in%row10$eid]<-"Possible CAD"

#c) Level II
#3627 age angina
output$Level2[output$eid%in%row10$eid]<-"Unverifiable prevalent UA"
##################################################################
#Eleventh row
#Death from other cause 

#a) Inclusion

row11<-death[which(!death$eid%in%row1$eid
                   &!death$eid%in%row2$eid
                   &!death$eid%in%row3$eid
                   &!death$eid%in%row4$eid
                   &!death$eid%in%row5$eid
                   &!death$eid%in%row6$eid
                   &!death$eid%in%row7$eid
                   &!death$eid%in%row8$eid
                   &!death$eid%in%row9$eid
                   &!death$eid%in%row10$eid),]

alive<-row11[which(row11$`40000-0.0`==""), ]
dead<-row11[which(!row11$`40000-0.0`==""), ]

#b) Level I
#Output
output$Level1[output$eid%in%dead$eid]<-"Never CAD"
output$Level1[output$eid%in%alive$eid]<-"No CAD"




#c) Level II

##############################################################
#Save output

write.table(output, "//ad.ucl.ac.uk/homem/rmgpljm/Documents/Phenotyping/algorithm.txt", quote=T, row.names=F)

fam<-read.table("//ad.ucl.ac.uk/homem/rmgpljm/Documents/Phenotyping/9p21.fam")
output2<-output[which(output$eid%in%fam$V2),]


write.table(output2, "//ad.ucl.ac.uk/homem/rmgpljm/Documents/Phenotyping/algorithm-qc.txt", quote=T, row.names=F)


