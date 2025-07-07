#setwd("/scratch/jz4698/rpackages")

#setwd("/scratch/jz4698/data")
# library(tidyverse)
# pkgs <- c("dplyr","readxl","ggplot2","tibble")
# lapply(pkgs, require, character.only = TRUE, lib = "/scratch/jz4698/rpackages")
library(dplyr)
library(readxl)
library(ggplot2)
library(tibble)


#seed <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
#set.seed(seed, kind = "L'Ecuyer-CMRG")
#import data from excel
sofa <- read_excel("daily_values 20221108a share.xlsx",skip = 1)
cohort2 <- read_excel("cohort results 20221104 share.xlsx",sheet = "Sheet1", skip = 1)


#select important variables
sofa_new <- sofa %>% dplyr::select("pat_enc_csn_id",pat_id, date, "SOFA Vtot")
sofa_new <- sofa_new[!is.na(sofa_new$pat_id),]
cohort_new <- cohort2 %>% dplyr::select("pat_enc_csn_id","inp_adm_date","hosp_disch_time", "pat_id","death date", "ever_on_ecmo","ecmo_start_time", "race", "ethnicity", "age (y)", "sex")
cohort_new$"inp_adm_date" <- as.Date(cohort_new$"inp_adm_date", format = "%Y-%m-%d", origin = "1900-01-01") - 2
cohort_new$"hosp_disch_time" <- as.Date(cohort_new$"hosp_disch_time", format = "%Y-%m-%d", origin = "1900-01-01") - 2
cohort_new$"ecmo_start_time" <- as.Date(cohort_new$"ecmo_start_time", format = "%Y-%m-%d")
# #race regroup
# cohort_new <- add_column(cohort_new, race_new = NA)
# cohort_new[cohort_new$race %in% c("White","Unknown, White",
#                                   "Other Race, White","White, Other Race",
#                                   "White, Unknown",
#                                   "White, Other Pacific Islander"),]$race_new <- "White"
# cohort_new[cohort_new$race %in% c("African American (Black)",
#                                   "African American (Black), White",
#                                   "African American (Black), Bangladeshi"),]$race_new <- "African American"
# cohort_new[cohort_new$race %in% c("Asian","Chinese","Chinese, Asian - unspecified","Japanese",
#                                   "Pakistani","Bangladeshi","Asian - unspecified, Other Race",
#                                   "Korean, Unknown","Other Pacific Islander","Asian Indian",
#                                   "Asian - unspecified","Unknown, Chinese","Chinese, Other Race",
#                                   "Filipino","Filipino, Other Pacific Islander",
#                                   "Other Race, Chinese"),]$race_new <- "Asian"
# cohort_new[cohort_new$race %in% c("Native American (American Indian/Eskimo/Aleutian)",
#                                   "Native American (American Indian/Eskimo/Aleutian) , Other Pacific Islander"),]$race_new <- "Native American"
# cohort_new[cohort_new$race %in% c("Unknown","Other Race",NA,"Patient Refused",
#                                   "Unknown, Other Race","Other Race, Unknown"),]$race_new <- "Unknown"
# cohort_new[is.na(cohort_new$race_new),]
# #ethnicity regroup
# cohort_new <- add_column(cohort_new, ethnicity_new = NA)
# cohort_new[cohort_new$ethnicity %in% c("Not of Spanish/Hispanic Origin",
#                                        "Not of Spanish/Hispanic Origin, Unknown"),]$ethnicity_new <- "Not Hispanic"
# cohort_new[cohort_new$ethnicity %in% c("Patient Refused","Unknown",
#                                        "Unknown, Not of Spanish/Hispanic Origin",NA),]$ethnicity_new <- "Unknown"
# cohort_new[is.na(cohort_new$ethnicity_new),]$ethnicity_new <- "Hispanic"
# cohort_new[is.na(cohort_new$ethnicity_new),]

#select patients that ever on ecmo
ecmo <- cohort_new[which(cohort_new$ever_on_ecmo == TRUE),]
sofa_new$date <- as.Date(sofa_new$date, format = "%Y-%m-%d")

#for patients that ever on ecmo, remove rows with dates on and after ecmo
id_ecmo <- unique(sofa_new$pat_id)[unique(sofa_new$pat_id) %in% ecmo$pat_id]
#patients who ever on ecmo
list_ecmo <- lapply(id_ecmo, function(i){
  edate <- ecmo$ecmo_start_time[ecmo$pat_id == i]
  df <- sofa_new[sofa_new$pat_id == i,]
  df <- df[df$date < edate,]
})
sofa2 <- sofa_new[!sofa_new$pat_id %in% id_ecmo,] #patients who are never on ecmo
for (i in seq_along(list_ecmo)) {
  sofa2 <- rbind(list_ecmo[[i]],sofa2)
}

#remove rows with NA in sofa2
sofa2 <- na.omit(sofa2)

# #special cases
# #a <- sofa_new[sofa_new$pat_id == "Z6223347",]
# cohort_new <- cohort_new[!(cohort_new$discharge_date == "2020/12/11" & cohort_new$pat_id == "Z6223347"),]

# # special case
# # "Z1189510"
# cohort_new <- cohort_new[!(cohort_new$pat_id == "Z1189510" & cohort_new$inp_adm_date == "2020-01-16"),]
# sofa2 <- sofa2[!(sofa2$pat_id == "Z1189510" & sofa2$date == "2020-01-16" & sofa2$`SOFA Vtot` == 6),]
#
# #"Z1325186"
# cohort_new <- cohort_new[!(cohort_new$pat_id == "Z1325186" & cohort_new$inp_adm_date == "2020-03-27"),]
# sofa2 <- sofa2[!(sofa2$pat_id == "Z1325186" & sofa2$date == "2020-03-27" & sofa2$`SOFA Vtot` == 1),]
#
# #"Z1539433"
# cohort_new <- cohort_new[!(cohort_new$pat_id == "Z1539433" & cohort_new$inp_adm_date == "2020-02-16"),]
#
# #"Z1901802"
# cohort_new <- cohort_new[!(cohort_new$pat_id == "Z1901802" & cohort_new$inp_adm_date == "2020-05-06"),]
#
# #"Z2743386"
# cohort_new <- cohort_new[!(cohort_new$pat_id == "Z2743386" & cohort_new$inp_adm_date == "2020-06-21"),]
#
# #"Z992380"
# cohort_new <- cohort_new[!(cohort_new$pat_id == "Z992380"),]

# "Z8282529"
cohort_new <- cohort_new[!(cohort_new$pat_id == "Z8282529" & cohort_new$pat_enc_csn_id == 788823744),]
sofa2 <- sofa2[!(sofa2$pat_id == "Z8282529" & sofa2$`SOFA Vtot` == "EMPTY"),]

#patients who have enrolled for several times
patients <- unique(sofa2$pat_id)
all_patients <- patients[patients %in% cohort_new$pat_id]
#2209 in total
#length(all_patients)
sofa2 <- sofa2[sofa2$pat_id %in% all_patients,]
cohort_new <- cohort_new[cohort_new$pat_id %in% all_patients,]
a <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a]

#enroll once
n_patients <- all_patients[!all_patients %in% more2]


#patients who enrolled for several times
#the number of discharge
discharge_time <- rep(0, length(more2))
for (i in seq_along(more2)) {
  discharge_time[i] <- nrow(cohort_new[cohort_new$pat_id == more2[i],])
}

#the number of discharge is 2: first discharge date = the second admission date
for (i in more2) {
  c_more2 <- cohort_new[cohort_new$pat_id == i,]
  s_more2 <- sofa2[sofa2$pat_id == i,]
  date_more2 <- sort(c_more2$"hosp_disch_time")
  date2_more2 <- sort(c_more2$"inp_adm_date")
  if(length(date_more2) == 2){
    if(as.character(date2_more2[2]) == as.character(date_more2[1])){
      cohort_new[(cohort_new$pat_id == i & cohort_new$hosp_disch_time == date_more2[2]),]$inp_adm_date <- date2_more2[1]
      cohort_new <- cohort_new[!(cohort_new$pat_id == i & cohort_new$hosp_disch_time == date_more2[1]),]
    }
  }
}

#update more2
a2 <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a2]

#the number of discharge is 3: first discharge date = the second admission date or the second discharge date = the third admission date
for (i in more2) {
  c_more2 <- cohort_new[cohort_new$pat_id == i,]
  s_more2 <- sofa2[sofa2$pat_id == i,]
  date_more2 <- sort(c_more2$"hosp_disch_time")
  date2_more2 <- sort(c_more2$"inp_adm_date")
  if(length(date_more2) == 3){
    if(as.character(date2_more2[2]) == as.character(date_more2[1])){
      cohort_new[(cohort_new$pat_id == i & cohort_new$inp_adm_date == date2_more2[1]),]$hosp_disch_time <- date_more2[2]
      cohort_new <- cohort_new[!(cohort_new$pat_id == i & cohort_new$inp_adm_date == date2_more2[2]),]
    } else if(as.character(date2_more2[3]) == as.character(date_more2[2])){
      cohort_new[(cohort_new$pat_id == i & cohort_new$inp_adm_date == date2_more2[2]),]$hosp_disch_time <- date_more2[3]
      cohort_new <- cohort_new[!(cohort_new$pat_id == i & cohort_new$inp_adm_date == date2_more2[3]),]
    }
  }
}

#special case: "Z992380"
cohort_new <- cohort_new[!(cohort_new$pat_id == "Z992380" & as.character(cohort_new$hosp_disch_time) == "2020-05-22"),]
cohort_new <- cohort_new[!(cohort_new$pat_id == "Z992380" & as.character(cohort_new$inp_adm_date) == "2020-05-28"),]
cohort_new[(cohort_new$pat_id == "Z992380" & as.character(cohort_new$inp_adm_date) == "2020-05-20"),]$inp_adm_date <- as.Date("2020-05-20", format = "%Y-%m-%d")

#special case: "Z4356816"
cohort_new <- cohort_new[!(cohort_new$pat_id == "Z4356816" & as.character(cohort_new$hosp_disch_time) == "2020-04-09"),]


#update more2
a2 <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a2]
#the number of discharge
discharge_time <- rep(0, length(more2))
for (i in seq_along(more2)) {
  discharge_time[i] <- nrow(cohort_new[cohort_new$pat_id == more2[i],])
}




#use the higher sofa score
l <- rep(0, length(all_patients))
for (i in seq_along(all_patients)) {
  s_more2 <- sofa2[sofa2$pat_id == all_patients[i],]
  v1 <- names(table(s_more2$date))[table(s_more2$date) > 1]
  l[i] <- length(v1)
  if(length(v1) == 1){
    if(sum(s_more2[s_more2$date == v1,]$`SOFA Vtot` == "EMPTY") > 0){
    sofa2 <- sofa2[!(sofa2$pat_id == all_patients[i] & sofa2$date == v1 & sofa2$`SOFA Vtot` == "EMPTY"),]
    } else {
      b <- min(s_more2[s_more2$date == v1,]$`SOFA Vtot`)
      sofa2 <- sofa2[!(sofa2$pat_id == all_patients[i] & sofa2$date == v1 & sofa2$`SOFA Vtot` == b),]
    }
  }
}

# which(l == 2) #95
# s_s <- sofa2[sofa2$pat_id == all_patients[95],]
# sofa2 <- sofa2[!(sofa2$pat_id == all_patients[95] & sofa2$date == "2020-05-22" & sofa2$`SOFA Vtot` %in% c(6,10)),]
# sofa2 <- sofa2[!(sofa2$pat_id == all_patients[95] & sofa2$date == "2020-05-28" & sofa2$`SOFA Vtot` == "EMPTY"),]


#add a column "intubated_day" in sofa2
#patients who enrolled once
sofa2 <- sofa2 %>% add_column(intubated_day = NA)

#patients who enrolled once
n_patients <- all_patients[!all_patients %in% more2]
for (i in n_patients) {
  a <- nrow(sofa2[sofa2$pat_id == i,][,"intubated_day"])
  sofa2[sofa2$pat_id == i,][,"intubated_day"] <- 0:(a-1)
}

#2 or 3
for (i in more2) {
  c_more2 <- cohort_new[cohort_new$pat_id == i,]
  s_more2 <- sofa2[sofa2$pat_id == i,]
  date_more2 <- sort(c_more2$"hosp_disch_time")
  date2_more2 <- sort(c_more2$"inp_adm_date")
  ss <- NULL
  ss[[1]] <- s_more2[as.character(s_more2$date) < as.character(date2_more2[2]),]
  n_ss1 <- nrow(ss[[1]][,"intubated_day"])
  if(n_ss1 == 0){
    ss[[1]] <- ss[[1]]
  } else {
  ss[[1]][,"intubated_day"] <- 0:(n_ss1-1)
  }
  #date <- sort(s_more2[s_more2$date <= date_more2[j] & s_more2$date > date_more2[j-1],]$date)
  if(length(date_more2) == 2){
    ss[[2]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[1]),]
    n_ss2 <- nrow(ss[[2]][,"intubated_day"])
    if(n_ss2 == 0){
      ss[[2]] <- ss[[2]]
    } else {
    ss[[2]][,"intubated_day"] <- 0:(n_ss2-1)
    }
  } else if(length(date_more2) == 3){
    ss[[2]] <- s_more2[(as.character(s_more2$date) > as.character(date_more2[1]) & as.character(s_more2$date) < as.character(date2_more2[3])),]
    n_ss2 <- nrow(ss[[2]][,"intubated_day"])
    if(n_ss2 == 0){
      ss[[2]] <- ss[[2]]
    } else {
    ss[[2]][,"intubated_day"] <- 0:(n_ss2-1)
    }
    ss[[3]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[2]),]
    n_ss3 <- nrow(ss[[3]][,"intubated_day"])
    if(n_ss2 == 0){
      ss[[3]] <- ss[[3]]
    } else {
    ss[[3]][,"intubated_day"] <- 0:(n_ss3-1)
    }
  } else if (length(date_more2) == 4){
    ss[[2]] <- s_more2[(as.character(s_more2$date) > as.character(date_more2[1]) & as.character(s_more2$date) < as.character(date2_more2[3])),]
    n_ss2 <- nrow(ss[[2]][,"intubated_day"])
    if(n_ss2 == 0){
      ss[[2]] <- ss[[2]]
    } else {
      ss[[2]][,"intubated_day"] <- 0:(n_ss2-1)
    }
    ss[[3]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[2]) & as.character(s_more2$date) < as.character(date2_more2[4]),]
    n_ss3 <- nrow(ss[[3]][,"intubated_day"])
    if(n_ss3 == 0){
      ss[[3]] <- ss[[3]]
    } else {
      ss[[3]][,"intubated_day"] <- 0:(n_ss3-1)
    }
    ss[[4]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[3]),]
    n_ss4 <- nrow(ss[[4]][,"intubated_day"])
    if(n_ss4 == 0){
      ss[[4]] <- ss[[4]]
    } else {
      ss[[4]][,"intubated_day"] <- 0:(n_ss4-1)
    }
  } else if (length(date_more2) == 5){
    ss[[2]] <- s_more2[(as.character(s_more2$date) > as.character(date_more2[1]) & as.character(s_more2$date) < as.character(date2_more2[3])),]
    n_ss2 <- nrow(ss[[2]][,"intubated_day"])
    if(n_ss2 == 0){
      ss[[2]] <- ss[[2]]
    } else {
      ss[[2]][,"intubated_day"] <- 0:(n_ss2-1)
    }
    ss[[3]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[2]) & as.character(s_more2$date) < as.character(date2_more2[4]),]
    n_ss3 <- nrow(ss[[3]][,"intubated_day"])
    if(n_ss3 == 0){
      ss[[3]] <- ss[[3]]
    } else {
      ss[[3]][,"intubated_day"] <- 0:(n_ss3-1)
    }
    ss[[4]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[3]) & as.character(s_more2$date) < as.character(date2_more2[5]),]
    n_ss4 <- nrow(ss[[4]][,"intubated_day"])
    if(n_ss4 == 0){
      ss[[4]] <- ss[[4]]
    } else {
      ss[[4]][,"intubated_day"] <- 0:(n_ss4-1)
    }
    ss[[5]] <- s_more2[as.character(s_more2$date) > as.character(date_more2[4]),]
    n_ss5 <- nrow(ss[[5]][,"intubated_day"])
    if(n_ss5 == 0){
      ss[[5]] <- ss[[5]]
    } else {
      ss[[5]][,"intubated_day"] <- 0:(n_ss5-1)
    }
  }
  ss_f <- NULL
  for (m in 1:length(ss)){
    ss_f <- rbind(ss[[m]],ss_f)
  }
  dates <- ss_f$date
  for(t in dates){
    sofa2[sofa2$pat_id == i & sofa2$date == t,][,"intubated_day"] <- ss_f[ss_f$date == t, ]$intubated_day
  }
}

# 14 patients with gaps
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 788161858, ]
sofa2[sofa2$pat_enc_csn_id == 788161858, ]$intubated_day <- 0
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 783883925, ]
sofa2[sofa2$pat_enc_csn_id == 783883925, ]$intubated_day <- 0
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787589003, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787589003 & sofa2$date >= "2020-04-08",])
sofa2[sofa2$pat_enc_csn_id == 787589003 & sofa2$date >= "2020-04-08",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787201513, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787201513 & sofa2$date >= "2020-04-09",])
sofa2[sofa2$pat_enc_csn_id == 787201513 & sofa2$date >= "2020-04-09",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 786808306, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 786808306 & sofa2$date >= "2020-04-13",])
sofa2[sofa2$pat_enc_csn_id == 786808306 & sofa2$date >= "2020-04-13",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 785992482, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 785992482 & as.character(sofa2$date) %in% c("2020-04-07","2020-04-08"),])
sofa2[sofa2$pat_enc_csn_id == 785992482 & as.character(sofa2$date) %in% c("2020-04-07","2020-04-08"),][,"intubated_day"] <- 0:(a-1)
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 785992482 & sofa2$date >= "2020-04-12",])
sofa2[sofa2$pat_enc_csn_id == 785992482 & sofa2$date >= "2020-04-12",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 786079332, ]
sofa2[sofa2$pat_enc_csn_id == 786079332 & sofa2$date == "2020-04-14",][,"intubated_day"] <- 0
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 788093316, ]
sofa2[sofa2$pat_enc_csn_id == 788093316 & sofa2$date == "2020-04-15",][,"intubated_day"] <- 0
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787235801, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787235801 & sofa2$date >= "2020-04-14",])
sofa2[sofa2$pat_enc_csn_id == 787235801 & sofa2$date >= "2020-04-14",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787507206, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787507206 & sofa2$date >= "2020-04-11",])
sofa2[sofa2$pat_enc_csn_id == 787507206 & sofa2$date >= "2020-04-11",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787021423, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787021423 & sofa2$date >= "2020-04-10",])
sofa2[sofa2$pat_enc_csn_id == 787021423 & sofa2$date >= "2020-04-10",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 788329900, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 788329900 & sofa2$date >= "2020-04-18",])
sofa2[sofa2$pat_enc_csn_id == 788329900 & sofa2$date >= "2020-04-18",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787685061, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787685061 & sofa2$date >= "2020-04-19",])
sofa2[sofa2$pat_enc_csn_id == 787685061 & sofa2$date >= "2020-04-19",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 787651330, ]
a <- nrow(sofa2[sofa2$pat_enc_csn_id == 787651330 & sofa2$date >= "2020-04-16",])
sofa2[sofa2$pat_enc_csn_id == 787651330 & sofa2$date >= "2020-04-16",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 788386858, ]
sofa_gap <- sofa2[sofa2$pat_enc_csn_id == 788266226, ]


#use the value of day1 for day0
#ONLY FOR PATIENTS WITH SOFA AS EMPTY ON DAY0
E0 <- sofa2$`SOFA Vtot` == "EMPTY" & sofa2$intubated_day == 0
p0 <- sofa2[E0,]
t0 <- table(p0$pat_id)
p1 <- names(t0)[t0 == 1]
for (i in seq_along(p1)) {
  a <- nrow(sofa2[sofa2$pat_id == p1[i],])
  if(a > 1){
    sofa2[sofa2$pat_id == p1[i], "SOFA Vtot"][1,] <- sofa2[sofa2$pat_id == p1[i], "SOFA Vtot"][2,]
  }
}

#replace the remaining EMPTY as 0
sofa2$`SOFA Vtot`[sofa2$`SOFA Vtot` == "EMPTY"] <- "0"
sofa2$`SOFA Vtot` <- as.numeric(sofa2$`SOFA Vtot`)



#add a column "group" in sofa2
sofa <- as.numeric(sofa2$`SOFA Vtot`)
intu <- sofa2$intubated_day
sofa2 <- sofa2 %>% add_column(group = NA)
for (i in 1:nrow(sofa2)) {
  if(intu[i] == 0){ #day0
    if(sofa[i] > 11){
      sofa2[i,]$group <-"Blue"
    } else if(sofa[i] <= 7){
      sofa2[i,]$group <-"Red"
    } else {
      sofa2[i,]$group <-"Yellow"
    }
  } else if(intu[i] == 2){ #day2
    if(sofa[i] <= 11 & (sofa[i] < sofa[i-2])){
      sofa2[i,]$group <-"Red"
    } else if(sofa[i] <= 7 & (sofa[i] == sofa[i-2])){
      sofa2[i,]$group <-"Yellow"
    } else {
      sofa2[i,]$group <-"Blue"
    }
  } else if(intu[i] == 5){ #day5
    if(sofa[i] <= 7 & ((sofa[i-3]-sofa[i])>=3)){
      sofa2[i,]$group <-"Red"
    } else if(sofa[i] <= 7 & ((sofa[i-3]-sofa[i])<3) & (0<(sofa[i-3]-sofa[i]))){
      sofa2[i,]$group <-"Yellow"
    } else {
      sofa2[i,]$group <-"Blue"
    }
  } else if(intu[i] > 5 & (intu[i]%%2==1)){ #odd days after day5
    if(sofa[i] <= 7 &(sofa[i-2]-sofa[i]>0)){
      sofa2[i,]$group <-"Yellow"
    } else {
      sofa2[i,]$group <-"Blue"
    }
  } else {
    sofa2[i,]$group <-"White"
  }
}
#now the big dataset is `sofa2`


#filter rows with the same date
b <- unique(sofa2$date)
d <- b[order(b)]
#calculate the number of ventilators needed on each day
origin_survive <- rep(0,length(d))
for (i in seq_along(d)) {
  small_df <- sofa2 %>% filter(date == d[i])
  origin_survive[i] <- nrow(small_df)
}
df3 <- data.frame(date = d, number = origin_survive)
the_first_date <- df3$date[df3$number > 250][1]
cohort_new <- cohort_new %>% add_column(dead = NA)
cohort_new$dead[!is.na(cohort_new$`death date`)] <- 1
cohort_new$dead[is.na(cohort_new$`death date`)] <- 0
#table(cohort_new$"dead")

file_name2 <- paste0("date",".RData")
save(d,file = file_name2)

file_name3 <- paste0("df",".RData")
save(df3, file = file_name3)


file_name4 <- paste0("dataset",".RData")
save(sofa2, cohort_new, sofa_new, file = file_name4)

# cohort_new[is.na(cohort_new$"died_hosp"),]$"died_hosp" <- 0
# cohort_new$"died_hosp" <- as.character(cohort_new$"died_hosp")
# cohort_new[cohort_new$"died_hosp" == "1",]$"died_hosp" <- "deceased"
# cohort_new[cohort_new$"died_hosp" == "0",]$"died_hosp" <- "alive"

#start allocation

start_date <- as.Date(character(0))
last_date <- as.Date(character(0))
total <- rep(0,10000)
red_all <- NULL
yellow_all <- NULL
blue_all <- NULL
brown_all <- NULL
white_all <- NULL
v_all <- NULL
new_red <- NULL
new_yellow <- NULL
new_blue <- NULL
groupa_all <- rep(0,10000)
groupb_all <- rep(0,10000)
survival_a <- rep(0,10000)
survival_b <- rep(0,10000)
survival_all <- rep(0,10000)
f3_all <- NULL
f4_all <- NULL
red_id_all <- NULL
for(r in 1:10000){
  v <- rep(0,length(d))
  nwhite <- rep(0,length(d))
  nred <- rep(0,length(d))
  nyellow <- rep(0,length(d))
  nblue <- rep(0,length(d))
  nbrown <- rep(0,length(d))
  new_r <- rep(0,length(d))
  new_y <- rep(0,length(d))
  new_b <- rep(0,length(d))
  
  groupa <- NULL
  groupb <- NULL
  cat("seed = ", r, "\n")
  sofa3 <- sofa2
  groupa <- NULL
  groupb <- NULL
  f3 <- NULL
  f4 <- NULL
  set.seed(r)
  for (i in seq_along(d)){ #for each day  
    #cat("i = ", i, "\n")
    if(d[i] == the_first_date) {
      rp = 787668565
      f3 <- rbind(f3,sofa3[(sofa3$date == d[i] & sofa3$"pat_enc_csn_id" == 787668565),])
      groupa <- c(groupa, 787668565)
      sofa3 <- sofa3[sofa3$pat_enc_csn_id %in% setdiff(sofa3$pat_enc_csn_id,787668565),]
      #groupa information
    }
    if(d[i] == "2020-04-14") {
      rp = 788566195
      f3 <- rbind(f3,sofa3[(as.character(sofa3$date) == d[i] & sofa3$pat_enc_csn_id == 788566195),])
      groupa <- c(groupa, 788566195)
      sofa3 <- sofa3[sofa3$pat_enc_csn_id %in% setdiff(sofa3$pat_enc_csn_id,788566195),]
      #groupa information
    }
    sdf <- sofa3 %>% filter(date == d[i])
    if(nrow(sdf) <= 250){
      v[i] <- length(unique(sdf$pat_id))
      remove <- NULL
      nwhite[i] <- sum(sdf$group == "White")
      nred[i] <- sum(sdf$group == "Red")
      nyellow[i] <- sum(sdf$group == "Yellow")
      nblue[i] <- sum(sdf$group == "Blue")
    } else if(nrow(sdf) > 250){
      sdf <- sofa3 %>% filter(date == d[i])
      nwhite[i] <- sum(sdf$group == "White")
      nr <- sum(sdf$group == "Red")
      ny <- sum(sdf$group == "Yellow")
      nb <- sum(sdf$group == "Blue")
      red <- sdf[sdf$group == "Red",]$pat_enc_csn_id
      yellow <- sdf[sdf$group == "Yellow",]$pat_enc_csn_id
      blue <- sdf[sdf$group == "Blue",]$pat_enc_csn_id
      if(nwhite[i] >= 250){ #randomly choose 250 from white group
        pid <- sdf[sdf$group %in% c("Red","Yellow","Blue"),]$pat_enc_csn_id
        sofa3 <- sofa3[!sofa3$pat_enc_csn_id %in% pat_enc_csn_id,]
      } else if((nr+nwhite[i]) >= 250){
        nred[i] <- x <- 250-nwhite[i]
        y <- sample(red,x) #patients who revived ventilators
        potential_b <- NULL #patients who are allocated ventilators are on their day0
        for (m in y) {
          single <- sdf[sdf$pat_enc_csn_id == m,]$intubated_day
          if(single == 0){
            potential_b <- c(potential_b,m) #part of y and on day0
          }
        }
        n <- red[!red %in% y] #n = nr - y
        # for (g in n) {
        #   single <- sdf[sdf$pat_id == g,]$intubated_day
        #   if(single != 0){
        #     n_not0 <- c(n_not0,g) #for patients on day0, they are not in group a
        #   }
        # }
        groupa <- c(groupa, n)
        #groupa information
        f3 <- rbind(f3,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% n,])

        #1. patients on day 0
        #2. patients in red group without ventilators
        new_groupb <- sample(potential_b, length(n))
        groupb <- c(groupb,new_groupb)
        #groupb information
        f4 <- rbind(f4,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb,])

        nyellow[i] <- 0
        nblue[i] <- 0
        nbrown[i] <- nrow(sdf) - 250 #all the blue, all the yellow, red without ventilators
        remove <- c(n, blue, yellow)
      } else if((nr + nwhite[i] + ny) >= 250){
        nred[i] <- nr
        nyellow[i] <- x <- 250-nwhite[i]-nr
        y <- sample(yellow,x)
        #patients who are allocated ventilators are on their day0
        potential_b <- NULL
        y2 <- c(y,red)
        for (m in y2) {
          single <- sdf[sdf$pat_enc_csn_id == m,]$intubated_day
          if(single == 0){
            potential_b <- c(potential_b,m)
          }
        }

        remove <- c(yellow[!yellow %in% y],blue)
        groupa <- c(groupa,remove)
       
        give <- NULL
        for (z in remove) {
          single <- sdf[sdf$pat_enc_csn_id == z,]$intubated_day
          if(single != 0){
            give <- c(give,z) #patients on day 0 and in remove
          }
        }
        
        #groupa information
        f3 <- rbind(f3,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% remove,])
        set.seed(r)
        new_groupb <- sample(potential_b, length(give))
        groupb <- c(groupb,new_groupb)
        #groupb information
        f4 <- rbind(f4,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb,])
        
        if(d[i] == "2020-03-31") {
          new_groupb2 <- sample(potential_b,1)
          groupb <- c(groupb,new_groupb2)
          f4 <- rbind(f4,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb2,])
        }
        
        nblue[i] <- 0
        nbrown[i] <- nrow(sdf) - 250 #all_patients - 250
        
        sofa3 <- sofa3[!(sofa3$pat_enc_csn_id %in% remove),] #remove patients who are not allocated
        sdf2 <- sofa3 %>% filter(date == d[i])
        v[i] <- length(sdf2$pat_enc_csn_id)
        red <- sdf2[sdf2$group == "Red",]$pat_enc_csn_id
        yellow <- sdf2[sdf2$group == "Yellow",]$pat_enc_csn_id
        blue <- sdf2[sdf2$group == "Blue",]$pat_enc_csn_id
      } else if((nr + nwhite[i] + ny + nb) >= 250){
        #remove patients that meet the exclusion criterion
        nred[i] <- nr
        if(i == 97){
        red_id <- sdf[sdf$group == "Red",]$pat_id
        }
        nyellow[i] <- ny
        nblue[i] <- x <- 250-nwhite[i]-nr-ny
        new_r[i] <- nrow(sdf[sdf$group == "Red" & sdf$intubated_day == 0,])
        new_y[i] <- nrow(sdf[sdf$group == "Yellow" & sdf$intubated_day == 0,])
        new_b[i] <- nrow(sdf[sdf$group == "Blue" & sdf$intubated_day == 0,])
        set.seed(r)
        y <- sample(blue,x) #patients in blue and get the ventilators
        #patients who received ventilators are on their day0
        potential_b <- NULL
        y2 <- c(y,yellow,red)
        for (m in y2) {
          single <- sdf[sdf$pat_enc_csn_id == m,]$intubated_day
          if(single == 0){
            potential_b <- c(potential_b,m) #patients on day 0 and in y2
          }
        }
        remove <- blue[!(blue %in% y)]
        nbrown[i] <- length(remove)
        groupa <- c(groupa,remove)
        #groupa information
        f3 <- rbind(f3,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% remove,])

        #new patients for red and yellow and blue (day0) can be in group b
        #1. in red group and on day 0
        #2. in yellow group and on day 0
        #3. in blue group and on day 0
        
        give <- NULL
        for (z in remove) {
          single <- sdf[sdf$pat_enc_csn_id == z,]$intubated_day
          if(single != 0){
            give <- c(give,z) #patients on day 0 and in remove
          }
        }
        
        set.seed(r)
        new_groupb <- sample(potential_b, length(give))
        groupb <- c(groupb,new_groupb)
        #groupb information
        f4 <- rbind(f4,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb,])

        if(d[i] == "2020-03-31") {
          new_groupb2 <- sample(potential_b,1)
          groupb <- c(groupb,new_groupb2)
          f4 <- rbind(f4,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb2,])
        }
        
        sofa3 <- sofa3[!(sofa3$pat_enc_csn_id %in% remove),] #remove patients who are not allocated
        sdf2 <- sofa3 %>% filter(date == d[i])
        v[i] <- length(sdf2$pat_enc_csn_id)
        red <- sdf2[sdf2$group == "Red",]$pat_enc_csn_id
        yellow <- sdf2[sdf2$group == "Yellow",]$pat_enc_csn_id
        blue <- sdf2[sdf2$group == "Blue",]$pat_enc_csn_id
      }
    }
  
  }
  #survival rate for patients who ever been in the crisis period
  s_date <-  as.Date(d[v==250][1],format = ("%Y-%m-%d"))
  la_date <- as.Date(d[v==250][length(d[v==250])],format = ("%Y-%m-%d"))
  v_date <- seq(s_date,la_date,by = 1)
  group_all <- NULL
  for (k in v_date) {
    c_df <- sofa3 %>% filter(date == k)
    group_all <- unique(c(group_all,unique(c_df$pat_id)))
  }
  
  
  #enroll for once
  group_all1 <- group_all[!group_all %in% more2]
  outcome_all1 <- as.character(rep(0,length(group_all1)))
  for (i in seq_along(group_all1)) {
    outcome_all1[i] <- cohort_new[cohort_new$pat_id == group_all1[i],]$"dead"
  }
  #enroll for several times
  group_all2 <- group_all[group_all %in% more2]
  outcome_all2 <- rep(0,length(group_all2))
  for (i in seq_along(group_all2)) {
    se_outcome <- cohort_new[cohort_new$pat_id == outcome_all2[i],]$"dead"
    if(sum(se_outcome == "1") > 0){
      outcome_all2[i] <- "1"
    } else {
      outcome_all2[i] <- "0"
    }
  }
  outcome_all <- c(outcome_all1, outcome_all2)
  t3 <- table(outcome_all)
  sur_all <- t3[1]/length(outcome_all)

  #survival rate for groupa
  #enroll for once
  groupa_id <- cohort_new[cohort_new$pat_enc_csn_id %in% groupa,]$pat_id
  id1 <- groupa_id[!groupa_id %in% more2]
  outcome <- as.character(rep(0,length(id1)))
  for (i in seq_along(id1)) {
    outcome[i] <- cohort_new[cohort_new$pat_id == id1[i],]$"dead"
  }
  #enroll for several times
  id2 <- groupa_id[groupa_id %in% more2]
  outcome2 <- rep(0,length(id2))
  for (i in seq_along(id2)) {
    se_outcome <- cohort_new[cohort_new$pat_id == id2[i],]$"dead"
    if(sum(se_outcome == "1") > 0){
      outcome2[i] <- "1"
    } else {
      outcome2[i] <- "0"
    }
  }
  outcome <- c(outcome,outcome2)
  t1 <- table(outcome)
  sur_a <- t1[1]/length(outcome)
  #survival rate for groupb
  #enroll for once
  groupb_id <- cohort_new[cohort_new$pat_enc_csn_id %in% groupb,]$pat_id
  id3 <- groupb_id[!groupb_id %in% more2]
  outcome3 <- as.character(rep(0,length(id3)))
  for (i in seq_along(id3)) {
    outcome3[i] <- cohort_new[cohort_new$pat_id == id3[i],]$"dead"
  }
  #enroll for several times
  id4 <- groupb_id[groupb_id %in% more2]
  outcome4 <- rep(0,length(id4))
  for (i in seq_along(id4)) {
    se_outcome <- cohort_new[cohort_new$pat_id == id4[i],]$"dead"
    if(sum(se_outcome == "1") > 1){
      outcome4[i] <- "1"
    } else {
      outcome4[i] <- "0"
    }
  }
  outcome3 <- c(outcome3,outcome4)
  t2 <- table(outcome3)
  sur_b <- t2[1]/length(outcome3)
  


  start_date[r] <-  as.Date(d[v==250][1],format = ("%Y-%m-%d"))
  last_date[r] <- as.Date(d[v==250][length(d[v==250])],format = ("%Y-%m-%d"))
  total[r] <- as.numeric(last_date[r]-start_date[r]+1)
  red_all[[r]] <- nred
  yellow_all[[r]] <- nyellow
  blue_all[[r]] <- nblue
  brown_all[[r]] <- nbrown
  white_all[[r]] <- nwhite
  v_all[[r]] <- v
  new_red[[r]] <- new_r
  new_yellow[[r]] <- new_y
  new_blue[[r]] <- new_b
  groupa_all[r] <- length(groupa)
  groupb_all[r] <- length(groupb)
  survival_a[r] <- sur_a
  survival_b[r] <- sur_b
  survival_all[r] <- sur_all
  f3_all[[r]] <- f3
  f4_all[[r]] <- f4
  red_id_all[[r]] <- red_id
}

#crisis period
#dates that need ventilators = 250


setwd("/scratch/jz4698/ventilator_big")
file_name <- paste0("ventilator_big",".RData")
save(start_date,last_date,total,red_all,yellow_all,blue_all,
     brown_all,white_all,v_all,groupa_all,groupb_all,survival_a,
     survival_b,survival_all,new_red,new_yellow,new_blue,f3_all,f4_all,red_id_all,
     file = file_name)






# file_name <- paste0("ventilator_big2",".RData")
# save(start_date,last_date,total,red_all,yellow_all,blue_all,
#      brown_all,white_all,v_all,groupa_all,groupb_all,survival_a,
#      survival_b,survival_all,new_red,new_yellow,new_blue,f3_all,f4_all,
#      file = file_name)









#for last simulation
groupa
groupb
#race for groupa
table1 <- table(cohort_new[cohort_new$pat_id %in% groupa,]$race_new)
table1 <- append(table1,0,2)
names(table1)[3] <- "Native American"
table2 <- table(cohort_new[cohort_new$pat_id %in% groupb,]$race_new)
chisq.test(table1, table2)
#ethnicity for groupb
table3 <- table(cohort_new[cohort_new$pat_id %in% groupa,]$ethnicity_new)
table4 <- table(cohort_new[cohort_new$pat_id %in% groupb,]$ethnicity_new)
chisq.test(table3, table4)

group_all
table(cohort_new[cohort_new$pat_id %in% group_all,]$race_new)

#red, blue, yellow, brown, white after allocation or before allocation


