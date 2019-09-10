#########
# SETUP #
#########

###STEP 1: Load necessary packages

require(data.table)


###STEP 2: Read in different data sources


#1) Hospital episodes statistics 
hes <- fread("hes.file")

#2) Date of enrollment

baseline <- fread("phenotype.file",
                  select = c("eid", "53-0.0"), sep = ",")
baseline$eid <- as.numeric(baseline$eid)


#3) Mortality register

death <- fread("phenotype.file",
               select = c(1, 1639, 1642:1686), sep = ",") #Change to columns of mortality registry data in phenotype file

#4) Self-reported operations

selfreportops <- fread("phenotype.file",
                       select = c(1, 783:814), sep = ",") #Change to columns of self-report operations in phenotype file
selfreportops$eid <- as.numeric(selfreportops$eid)


#5) Self-reported illnesses

selfreport <- fread("phenotype.file",
                      select = c(1, 446:457, 552:580, 290, 317), sep = ",") #Change to columns of self-report questionnaires in phenotype file


#6) Self-reported medications

medications <- fread("phenotype.file",
                    select = c(1, 639:686), sep = ",") #Change to columns of medications in phenotype file



### STEP 3: Combine HES with enrollment date to distinguish between prevalence and incidence 

dataset <- merge(baseline, hes, by="eid")

#Convert dates to R date format
dataset$date <- as.Date(dataset$"53-0.0")
dataset$epistart <- as.Date(dataset$epistart)
dataset$admidate <- as.Date(dataset$admidate)

#Take the difference between the two HES dates and enrollment date.
dataset$datediff1 <- dataset$epistart - dataset$date
dataset$numericdiff1 <- as.numeric(dataset$datediff1, units = "days")

dataset$datediff2 <- dataset$admidate-dataset$date
dataset$numericdiff2 <- as.numeric(dataset$datediff2, units = "days")

### STEP 4: Set up the output file

output <- baseline[,c(1)]

#############
# ALGORITHM #
#############

###
#First row: I21 in secondary care EHR


#a) Inclusion criteria (I21 in secondary care EHR)

i21 <- c("I210", "I211", "I212", "I213", "I214", "I219") #I21
subsqmi <- c("I220", "I221", "I228", "I229") #Subsequent MI ICD codes
oldmi <- c("I252") #Old MI ICD code

all <- c(i21, subsqmi, oldmi) #Combination of all 3

incl <- dataset[which(dataset$diag_icd10 %in% i21), ] #Extract all data-points from HES involving an I21 code
row1 <- dataset[which(dataset$eid %in% incl$eid), ] #Extract individuals with an I21 data-point in HES

#b) Level I

#Output
output$Level1[output$eid %in% row1$eid] <- "MI" #Set Level 1 to MI for these individuals

#c) Level II

#Identify incident MIs
anyinc <- row1[which(row1$diag_icd10 %in% all & numericdiff1 > 0
                   | row1$diag_icd10 %in% all & numericdiff2 > 0), ]


#Identify prevalent MIs
anyprev <- row1[which(row1$diag_icd10 %in% all & numericdiff1 <= 0 
                    | row1$diag_icd10 %in% all & numericdiff2 <= 0), ]

#Identify individuals with only incident MIs

onlyinc <- anyinc[which(! anyinc$eid %in% anyprev$eid), ]

#Identify individuals with only prevalent MIs

onlyprev <- anyprev[which(! anyprev$eid %in% anyinc$eid), ]

#Output
output$Level2[output$eid %in% anyinc$eid
              & output$eid %in% anyprev$eid] <- "Incident & prevalent MI"

output$Level2[output$eid %in% onlyinc$eid] <- "Incident MI"
output$Level2[output$eid %in% onlyprev$eid] <- "Prevalent MI"

###
#Second row: Only other secondary care MI codes


#a) Inclusion criteria
#ICD codes
noni21 <- c("I231", "I236", "I238", "I241", "I252")

row2 <- dataset[which(dataset$diag_icd10 %in% noni21
                    & ! dataset$eid %in% row1$eid), ] #Remove individuals covered by previous rows

#b) Level I

#Output
output$Level1[output$eid %in% row2$eid] <- "Possible MI"

#c) Level II

#Any incident MIs
anyinc <- row2[which(row2$diag_icd10 %in% noni21 & numericdiff1 > 0
                   | row2$diag_icd10 %in% noni21 & numericdiff2 > 0), ]


#Any prevalent MIs
anyprev <- row2[which(row2$diag_icd10 %in% noni21 & numericdiff1 <= 0 
                    | row2$diag_icd10 %in% noni21 & numericdiff2 <= 0),]

#Only incident MIs

onlyinc <- anyinc[which(! anyinc$eid %in% anyprev$eid), ]

#Only prevalent MIs

onlyprev <- anyprev[which(! anyprev$eid %in% anyinc$eid), ]

#Output
output$Level2[output$eid %in% anyinc$eid
              & output$eid %in% anyprev$eid] <- "Incident & prevalent MI"

output$Level2[output$eid %in% onlyinc$eid] <- "Incident MI"
output$Level2[output$eid %in% onlyprev$eid] <- "Prevalent MI"

###
#Third row: MI death


#a) Inclusion criteria
#ICD codes
mideath <- c("I210", "I211", "I212", "I213", "I214", "I219",
                   "I252")

death$mideath <- apply(death, 1, function(r) any(r %in% mideath))
death$i21 <- apply(death, 1, function(r) any(r %in% i21))

row3 <- death[which(death$mideath == 1
                  & ! death$eid %in% row1$eid #Remove individuals in previous rows
                  & ! death$eid %in% row2$eid), ]

#b) Level I
#Output
output$Level1[output$eid %in% row3$eid] < -"MI"

#c) Level II

#Have I21

havei21 <- row3[which(row3$i21 == TRUE), ]
                   
#Non I21

noni21 <- row3[which(row3$i21 == FALSE), ]

#I21 as primary cause of death

primary <- row3[, c(1:4)]
primary$i21primary <- apply(primary, 1, function(r) any(r %in% i21))

#Output

output$Level2[output$eid %in% row3$eid
              & ! output$eid %in% noni21$eid] <- "Incident MI"

output$Level2[output$eid %in% noni21$eid] <- "Unknown date MI"

###
#Fourth row: Unstable angina

#a) Inclusion criteria
                            
ua <- c("I200")
row4 <- dataset[which(dataset$diag_icd10 %in% ua
                    & ! dataset$eid %in% row1$eid #Remove individuals in previous rows
                    & ! dataset$eid %in% row2$eid
                    & ! dataset$eid %in% row3$eid), ]

#b) Level I
#Output
output$Level1[output$eid %in% row4$eid] <- "CAD"

#c) Level II

#Incident UA
anyinc <- row4[which(row4$diag_icd10 %in% ua & numericdiff1 > 0
                   | row4$diag_icd10 %in% ua & numericdiff2 > 0), ]


#Prevalent UA
anyprev <- row4[which(row4$diag_icd10 %in% ua & numericdiff1 <= 0 
                    | row4$diag_icd10 %in% ua & numericdiff2 <= 0), ]

#Only incident UA

onlyinc <- anyinc[which(! anyinc$eid %in% anyprev$eid), ]

#Only prevalent UA

onlyprev <- anyprev[which(! anyprev$eid %in% anyinc$eid), ]

#Output
output$Level2[output$eid %in% onlyprev$eid] <- "Prevalent UA"
output$Level2[output$eid %in% onlyinc$eid] <- "Incident UA"
output$Level2[output$eid %in% anyinc$eid
              & output$eid %in% anyprev$eid] <- "Incident & prevalent UA"
              
###
#Fifth row: Revasc in secondary care

#a) Inclusion criteria

cadop <- c("K401", "K402", "K403", "K404", "K409",
         "K411", "K412", "K413", "K414",
         "K424", "K431", "K433", "K439",
         "K441", "K442", "K449",
         "K451", "K452", "K453", "K454", "K455", "K456", "K458", "K459",
         "K461", "K462", "K463", "K464", "K469", "K471",
         "K491", "K492", "K493", "K494", "K498", "K499",
         "K501", "K502", "K503", "K504", "K508", "K509",
         "K751", "K752", "K753", "K754", "K758", "K759")

row5 <- dataset[which(dataset$oper4 %in% cadop
                    & ! dataset$eid %in% row1$eid #Remove individuals in previous rows
                    & ! dataset$eid %in% row2$eid
                    & ! dataset$eid %in% row3$eid
                    & ! dataset$eid %in% row4$eid), ]

#b) Level I
#Output
output$Level1[output$eid %in% row5$eid] <- "CAD"

#c) Level II

#Incident revasc
anyinc <- row5[which(row5$oper4 %in% cadop & numericdiff1 > 0
                   | row5$oper4 %in% cadop & numericdiff2 > 0), ]


#Prevalent revasc
anyprev <- row5[which(row5$oper4 %in% cadop & numericdiff1 <= 0 
                    | row5$oper4 %in% cadop & numericdiff2 <=0 ), ]

#Only incident revasc

onlyinc <- anyinc[which(! anyinc$eid %in% anyprev$eid), ]

#Only prevalent revasc

onlyprev <- anyprev[which(! anyprev$eid %in% anyinc$eid), ]

#Output
output$Level2[output$eid %in% onlyprev$eid] <- "Prevalent revasc"
output$Level2[output$eid %in% onlyinc$eid] <- "Incident revasc"
output$Level2[output$eid %in% anyinc$eid
              & output$eid %in% anyprev$eid] <- "Incident & prevalent revasc"

###
#Sixth row: CAD ICD codes in secondary care

#a) Inclusion criteria

cadicd <- c("Z955", "I250", "I251", "I253", "I254", "I255", "I256", "I258", "I259",
          "I248", "I249", "I240")

row6 <- dataset[which(dataset$diag_icd10 %in% cadicd
                    & ! dataset$eid %in% row1$eid
                    & ! dataset$eid %in% row2$eid
                    & ! dataset$eid %in% row3$eid
                    & ! dataset$eid %in% row4$eid
                    & ! dataset$eid %in% row5$eid), ]

#b) Level I
#Output
output$Level1[output$eid %in% row6$eid] <- "CAD"

#c) Level II

#Incident revasc
anyinc <- row6[which(row6$diag_icd10 %in% cadicd & numericdiff1 > 0
                   | row6$diag_icd10 %in% cadicd & numericdiff2 > 0), ]


#Prevalent revasc
anyprev <- row6[which(row6$diag_icd10 %in% cadicd & numericdiff1 <= 0 
                    | row6$diag_icd10 %in% cadicd & numericdiff2 <= 0), ]

#Only incident revasc

onlyinc <- anyinc[which(! anyinc$eid %in% anyprev$eid), ]

#Only prevalent revasc

onlyprev <- anyprev[which(! anyprev$eid %in% anyinc$eid), ]

#Output
output$Level2[output$eid %in% onlyprev$eid] <- "Prevalent revasc"
output$Level2[output$eid %in% onlyinc$eid] <- "Incident revasc"
output$Level2[output$eid %in% anyinc$eid
              & output$eid %in% anyprev$eid] <- "Incident & prevalent revasc"

###
#Seventh row: Self-reported CAD procedures

#a) Inclusion criteria

cabg <- c("1095") #CABG code
angp <- c("1070") #Angioplasty code
angg <- c("1514") #Angiogram code
ops <- c("1070", "1514", "1095") #All 3 codes
                            
#Identify individuals with any of the above:
                
selfreportops$incl <- apply(selfreportops, 1, function(r) any(r %in% ops))
selfreportops$cabg <- apply(selfreportops, 1, function(r) any(r %in% cabg))
selfreportops$angioplasty <- apply(selfreportops, 1, function(r) any(r %in% angp))
selfreportops$angiogram <- apply(selfreportops, 1, function(r) any(r %in% angg))


row7 <- selfreportops[which(selfreportops$incl == 1
                          & ! selfreportops$eid %in% row1$eid #Remove individuals in previous rows
                          & ! selfreportops$eid %in% row2$eid
                          & ! selfreportops$eid %in% row3$eid
                          & ! selfreportops$eid %in% row4$eid
                          & ! selfreportops$eid %in% row5$eid
                          & ! selfreportops$eid %in% row6$eid), ]


#b) Level I
                                 
cabgyes <- row7[which(row7$cabg == TRUE), ] #CABG 
angpyes <- row7[which(row7$angioplasty == TRUE #Angioplasty with no CABG
                    & row7$cabg == FALSE), ]
anggyes <- row7[which(row7$angiogram == TRUE #Angiogram with no Angioplasty or CABG
                    & row7$cabg == FALSE
                    & row7$angioplasty == FALSE), ]

#CAD medications at baseline: statins or aspirin codes

statintypes<-c("1141146234", "1141192414", "1140910632", "1140888594", "1140864592", 
               "1141146138", "1140888648", "1141192410", "1140861958", "1140910654",
               "1140881748", "1141200040", 
               "1140861806", "1140864860", "1140868226", "1140868282", "1140872040",
               "1140882108", "1140882190", "1140882268", "1140882392", "1141163138",
               "1141164044", "1141167844")


medications$statins <- apply(medications, 1, function(r) any(r %in% statintypes))
medsyes <- medications[which(medications$statins == TRUE), ]

#Output
#CABG or Angioplasty + Meds = CAD
output$Level1[output$eid %in% cabgyes$eid] <- "CAD"
output$Level1[output$eid %in% angpyes$eid
              & output$eid %in% medsyes$eid] <- "CAD"

#Angioplasty with no meds = Possible CAD
output$Level1[output$eid %in% angpyes$eid
              & ! output$eid %in% medsyes$eid] <- "Possible CAD"

#Angiogram with meds = Possible CAD
output$Level1[output$eid %in% anggyes$eid
              & output$eid %in% medsyes$eid] <- "Possible CAD"

#Angiogram with no meds = No CAD
                             
output$Level1[output$eid %in% anggyes$eid
              & ! output$eid %in% medsyes$eid] <- "No CAD"

#c) Level II
                             
output$Level2[output$eid %in% cabgyes$eid] <- "Prevalent CAD"
output$Level2[output$eid %in% angpyes$eid
              & output$eid %in% medsyes$eid] <- "Prevalent CAD"

output$Level2[output$eid %in% angpyes$eid
              & ! output$eid %in% medsyes$eid] <- "Unverifiable prevalent CAD"

output$Level2[output$eid %in% anggyes$eid
              & output$eid %in% medsyes$eid] <- "Unverifiable prevalent CAD"


####
#Eighth row: Death records CAD 

#a) Inclusion criteria

cadicd <- c("Z955", "I250", "I251", "I253", "I254", "I255", "I256", "I258", "I259",
            "I248", "I249", "I240")

death$caddeath <- apply(death, 1, function(r) any(r %in% cadicd))

row8 <- death[which(death$caddeath == 1
                    & ! death$eid %in% row1$eid #Remove individuals in previous rows
                    & ! death$eid %in% row2$eid
                    & ! death$eid %in% row3$eid
                    & ! death$eid %in% row4$eid
                    & ! death$eid %in% row5$eid
                    & ! death$eid %in% row6$eid
                    & ! death$eid %in% row7$eid), ]
#b) Level I
#Output
output$Level1[output$eid %in% row8$eid] <- "CAD"

#c) Level II
#Primary cause of death
primary <- row8[, c(1:4)]
primary$cadprimary <- apply(primary, 1, function(r) any(r %in% cadicd))
caddeathyes <- primary[which(primary$cadprimary == TRUE), ]

#Output
output$Level2[output$eid %in% caddeathyes$eid] <- "CAD death"
output$Level2[output$eid %in% row8$eid
              & ! output$eid %in% caddeathyes$eid] <- "Incident CAD"


###
#Ninth row: self-reported MI 
#a) Inclusion

#First self-report question on MI
question1 <- selfreport[,c(1, 14:42)]
miq1 <- c("1075")

question1$mi <- apply(question1, 1, function(r) any(r %in% miq1))
temp1a <- question1[which(question1$mi == TRUE), ]
temp1b <- temp1a[, c(1)]

#Second self-report question on MI
question2 <- selfreport[,c(1:13)]
miq2 <- c("1")

question2$mi <- apply(question2, 1, function(r) any(r %in% miq2))
temp2a <- question2[which(question2$mi == TRUE), ]
temp2b <- temp2a[, c(1)]

#Combine both questions
srmi <- rbind(temp1b, temp2b)

row9 <- selfreport[which(selfreport$eid %in% srmi$eid
                  & ! selfreport$eid %in% row1$eid #Remove individuals in previous rows
                  & ! selfreport$eid %in% row2$eid
                  & ! selfreport$eid %in% row3$eid
                  & ! selfreport$eid %in% row4$eid
                  & ! selfreport$eid %in% row5$eid
                  & ! selfreport$eid %in% row6$eid
                  & ! selfreport$eid %in% row7$eid
                  & ! selfreport$eid %in% row7$eid
                  & ! selfreport$eid %in% row8$eid), ]

#b) Level I
output$Level1[output$eid %in% row9$eid] <- "Possible MI"

#c) Level II

output$Level2[output$eid %in% row9$eid] <- "Unverifiable prevalent MI"

###
#Tenth row: self-reported angina
#a) Inclusion

#Angina self-report question 1
angina1 <- c("1074")
question1$angina <- apply(question1, 1, function(r) any(r %in% angina1))
temp1a <- question1[which(question1$angina == TRUE), ]
temp1b <- temp1a[, c(1)]

#Angina self-report question 2
angina2 <- c("2")
question2$angina <- apply(question2, 1, function(r) any(r %in% angina2))
temp2a <- question2[which(question2$angina == TRUE), ]
temp2b <- temp2a[, c(1)]
                        
#Combine both questions
srangina <- rbind(temp1b, temp2b)

row10 <- selfreport[which(selfreport$eid %in% srangina$eid
                       & ! selfreport$eid %in% row1$eid #Remove individuals which are in previous rows
                       & ! selfreport$eid %in% row2$eid
                       & ! selfreport$eid %in% row3$eid
                       & ! selfreport$eid %in% row4$eid
                       & ! selfreport$eid %in% row5$eid
                       & ! selfreport$eid %in% row6$eid
                       & ! selfreport$eid %in% row7$eid
                       & ! selfreport$eid %in% row7$eid
                       & ! selfreport$eid %in% row8$eid
                       & ! selfreport$eid %in% row9$eid), ]

#b) Level I
#Output
output$Level1[output$eid %in% row10$eid] <- "Possible CAD"

#c) Level II

output$Level2[output$eid %in% row10$eid] <- "Unverifiable prevalent UA"
###
#Eleventh row: death from other cause 

#a) Inclusion

row11 <- death[which(! death$eid %in% row1$eid #Remove individuals which are in previous rows
                   & ! death$eid %in% row2$eid
                   & ! death$eid %in% row3$eid
                   & ! death$eid %in% row4$eid
                   & ! death$eid %in% row5$eid
                   & ! death$eid %in% row6$eid
                   & ! death$eid %in% row7$eid
                   & ! death$eid %in% row8$eid
                   & ! death$eid %in% row9$eid
                   & ! death$eid %in% row10$eid), ]

                          #Subset the fatal events
alive <- row11[which(row11$`40000-0.0` == ""), ]
dead <- row11[which(! row11$`40000-0.0` == ""), ]

#b) Level I
#Output
output$Level1[output$eid %in% dead$eid] <- "Never CAD"
output$Level1[output$eid %in% alive$eid] <- "No CAD"


#######
# END #
#######
