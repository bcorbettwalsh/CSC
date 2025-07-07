library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(tibble)
library(data.table)

#import data from excel
sofa <- read_excel("daily_values 20230118 share.xlsx",skip = 1)
cohort2 <- read_excel("cohort results 20230118 share.xlsx",sheet = "Sheet1", skip = 1)

#select important variables
sofa_new <- sofa %>% dplyr::select("pat_enc_csn_id",pat_id, date, "Canada")
sofa_new <- sofa_new[!is.na(sofa_new$pat_id),]
sofa_new$date <- as.Date(sofa_new$date, format = "%Y-%m-%d")
cohort_new <- cohort2 %>% dplyr::select("pat_enc_csn_id","intubation_date","extubation_date","pat_id","914","ever_on_ecmo","ecmo_start_time","race","ethnicity","age (y)","sex","...125","healthcare worker...596","Saskatchewan...711")
cohort_new <- cohort_new[!is.na(cohort_new$intubation_date),]
cohort_new$"intubation_date" <- as.Date(cohort_new$"intubation_date",format = "%Y-%m-%d")
cohort_new$"extubation_date" <- as.Date(cohort_new$"extubation_date",format = "%Y-%m-%d")
cohort_new$"ecmo_start_time" <- as.Date(cohort_new$"ecmo_start_time",format = "%Y-%m-%d")

#variable names
colnames(cohort_new)[10] <- "age"
colnames(cohort_new)[5] <- "dead"
colnames(cohort_new)[12] <- "pregnant"
colnames(cohort_new)[13] <- "healthcare"
colnames(cohort_new)[14] <- "exclusion"


#select patients that ever on ecmo
ecmo <- cohort_new[which(cohort_new$ever_on_ecmo == TRUE),]
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

#patients who have intubated for several times
patients <- unique(sofa2$pat_id)
all_patients <- patients[patients %in% cohort_new$pat_id]
#2209 in total
sofa2 <- sofa2[sofa2$pat_id %in% all_patients,]
cohort_new <- cohort_new[cohort_new$pat_id %in% all_patients,]
a <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a]

#intubate once
n_patients <- all_patients[!all_patients %in% more2]

#patients who intubated for several times
#the number of excubation
discharge_time <- rep(0, length(more2))
for (i in seq_along(more2)) {
  discharge_time[i] <- nrow(cohort_new[cohort_new$pat_id == more2[i],])
}

#"Z1189510"
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 781250273,]

#"Z1325186"
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787622316,]

#"Z1460829"
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 788520215,]

#"Z1539433"
cohort_new[cohort_new$pat_enc_csn_id == 781931719,]$extubation_date <- as.Date("2020-02-16",format = "%Y-%m-%d")
cohort_new[cohort_new$pat_enc_csn_id == 781931719,]$dead <- 1
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 784104437,]

#"Z1551445"
cohort_new[cohort_new$pat_enc_csn_id == 787199699,]$extubation_date <- as.Date("2020-03-24",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787332607,]

#"Z1901802"
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 789961365,]

#Z1966561
cohort_new[cohort_new$pat_enc_csn_id == 783153432,]$extubation_date <- as.Date("2020-02-15",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 784061993,]

#Z2430292
cohort_new[cohort_new$pat_enc_csn_id == 787690321,]$extubation_date <- as.Date("2020-04-05",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 788091835,]

#Z2743386
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 793950421,]

#Z2986501
cohort_new[cohort_new$pat_enc_csn_id == 783121775,]$extubation_date <- as.Date("2020-06-03",format = "%Y-%m-%d")

#Z3280540
cohort_new[cohort_new$pat_enc_csn_id == 782413361,]$extubation_date <- as.Date("2020-02-18",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 783521400,]
sofa2[sofa2$pat_id == "Z3280540",]$pat_enc_csn_id <- 782413361


#Z3396672
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 782427976,]


#Z3991010
cohort_new[cohort_new$pat_enc_csn_id == 784132634,]$extubation_date <- as.Date("2020-03-06",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 785875951,]

#Z4084619
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787203795,]

#Z4356816
cohort_new[cohort_new$pat_enc_csn_id == 786664746,]$extubation_date <- as.Date("2020-04-09",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 788339050,]
sofa2[sofa2$pat_id == "Z4356816",]$pat_enc_csn_id <- 786664746

#Z5575038
cohort_new[cohort_new$pat_enc_csn_id == 786592453,]$dead <- 1
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 786665706,]

#Z5765322
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 790201458,]

#Z5845300
cohort_new[cohort_new$pat_enc_csn_id == 788329900,]$dead <- 1
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 789956895,]

#Z591857
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 781435755,]

#Z6081432
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 793109618,]

#Z6300185
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 789883043,]

#Z6408895
cohort_new[cohort_new$pat_enc_csn_id == 788701958,]$extubation_date <- as.Date("2020-05-13",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 790362941,]

#Z6420054
cohort_new[cohort_new$pat_enc_csn_id == 780602665,]$dead <- 1
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 782039091,]

#Z6447323
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 790884907,]

#Z7178494
cohort_new[cohort_new$pat_enc_csn_id == 785074895,]$extubation_date <- as.Date("2020-03-02",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 785347051,]

#Z762792
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 786811053,]

#Z7802291
cohort_new[cohort_new$pat_enc_csn_id == 784111572,]$extubation_date <- as.Date("2020-02-26",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 784932184,]

#Z8164048
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787003384,]

#Z8211323
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 785205737,]

#Z8251032
cohort_new[cohort_new$pat_enc_csn_id == 787592951,]$extubation_date <- as.Date("2020-03-30",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787679165,]

#Z8282529
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 788823744,]

#Z8290808
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 791771182,]

#Z992380
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id %in% c(791601522,791193771,790942699),]
sofa2 <- sofa2[!sofa2$pat_enc_csn_id == 790942699,]

#8294446
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 790136689,]

#update more2
a2 <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a2]

#add a column "intubated_day" in sofa2
#patients who enrolled once
sofa2 <- sofa2 %>% add_column(intubated_day = NA)
#patients who intubated once
n_patients <- all_patients[!all_patients %in% more2]
for (i in n_patients) {
  a <- nrow(sofa2[sofa2$pat_id == i,][,"intubated_day"])
  sofa2[sofa2$pat_id == i,][,"intubated_day"] <- 0:(a-1)
}

#2 or 3
for (i in more2) {
  c_more2 <- cohort_new[cohort_new$pat_id == i,]
  s_more2 <- sofa2[sofa2$pat_id == i,]
  date_more2 <- sort(c_more2$"extubation_date")
  date2_more2 <- sort(c_more2$"intubation_date")
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

#patients with gaps
sofa2[sofa2$pat_enc_csn_id == 785303551 & sofa2$date >= "2020-03-17",][,"intubated_day"] <- 18:21
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
E0 <- sofa2$Canada == "EMPTY" & sofa2$intubated_day == 0
p0 <- sofa2[E0,]
t0 <- table(p0$pat_id)
p1 <- names(t0)[t0 == 1]
for (i in seq_along(p1)) {
  a <- nrow(sofa2[sofa2$pat_id == p1[i],])
  if(a > 1){
    sofa2[sofa2$pat_id == p1[i], "Canada"][1,] <- sofa2[sofa2$pat_id == p1[i], "Canada"][2,]
  }
}

#replace the remaining EMPTY as 0
sofa2$"Canada"[sofa2$"Canada" == "EMPTY"] <- "0"
sofa2$"Canada" <- as.numeric(sofa2$"Canada")

#add a column "group" in sofa2
sofa <- as.numeric(sofa2$Canada)
intu <- sofa2$intubated_day
sofa2 <- sofa2 %>% add_column(group = NA)
for (i in 1:nrow(sofa2)) {
  if(intu[i]%%3==0){ #every 72 hours
    if(sofa[i] <= 7){
      sofa2[i,]$group <-"Red"
    } else {
      sofa2[i,]$group <-"Yellow"
    }
  } else {
    sofa2[i,]$group <-"White"
  }
}

#add pregnancy
sofa2 <- sofa2 %>% add_column(pregnant = NA)
cohort_new[is.na(cohort_new$pregnant),]$pregnant <- 0
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$pregnant <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$pregnant
}
sofa2$pregnant <- as.numeric(sofa2$pregnant)

#add healthworker
sofa2 <- sofa2 %>% add_column(healthcare = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$healthcare <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$healthcare
}
sofa2$healthcare[is.na(sofa2$healthcare)] <- 0

#add 12-40 in cohort_new
cohort_new[cohort_new$pat_enc_csn_id == 787668565,]$age <- 15
#remove 787668565
#sofa2 <- sofa2[!sofa2$pat_enc_csn_id == 787668565,]
#cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787668565,]
cohort_new <- cohort_new %>% add_column(age_12_40 = 0)
cohort_new[(cohort_new$age >= 12 & cohort_new$age < 41),]$age_12_40 <- 1
#add 12-40 in sofa2
sofa2 <- sofa2 %>% add_column(age_12_40 = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_12_40 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_12_40
}

#add 41-60 in cohort_new
cohort_new <- cohort_new %>% add_column(age_41_60 = 0)
cohort_new[(cohort_new$age >= 41 & cohort_new$age < 61),]$age_41_60 <- 1
#add 41-60 in sofa2
sofa2 <- sofa2 %>% add_column(age_41_60 = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_41_60 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_41_60
}

#add 61-75 in cohort_new
cohort_new <- cohort_new %>% add_column(age_61_75 = 0)
cohort_new[(cohort_new$age >= 61 & cohort_new$age <= 75),]$age_61_75 <- 1
#add 61-75 in sofa2
sofa2 <- sofa2 %>% add_column(age_61_75 = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_61_75 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_61_75
}

#add 75+ in cohort_new
cohort_new <- cohort_new %>% add_column(age_75_more = 0)
cohort_new[(cohort_new$age > 75),]$age_75_more <- 1
#add 75+ in sofa2
sofa2 <- sofa2 %>% add_column(age_75_more = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_75_more <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_75_more
}
#now the big dataset is `sofa2`


#white_number
b <- unique(sofa2$date)
d <- b[order(b)]
white_number <- rep(0,length(d))
for(i in seq_along(d)){
  small_df <- sofa2 %>% filter(date == d[i])
  white_number[i] <- sum(small_df$group == "White")
}
sum(white_number > 250)
#none of the days has more than 250 white patients

red_preganent_number <- rep(0,length(d))
for(i in seq_along(d)){
  small_df <- sofa2 %>% filter(date == d[i])
  red_preganent_number[i] <- sum(small_df$group == "Red" & small_df$pregnant == 1)
}
sum((white_number + red_preganent_number) > 250)
#none of the days has more than 250 white patients + red pregnant

red_health <- rep(0,length(d))
for(i in seq_along(d)){
  small_df <- sofa2 %>% filter(date == d[i])
  red_health[i] <- sum(small_df$group == "Red" & small_df$healthcare == 1)
}
sum((white_number + red_preganent_number + red_health) > 250)
#none of the days has more than 250 white patients + red pregnant + red health

red_12_40 <- rep(0,length(d))
for(i in seq_along(d)){
  small_df <- sofa2 %>% filter(date == d[i])
  red_12_40[i] <- sum(small_df$group == "Red" & small_df$age_12_40 == 1)
}
sum((white_number + red_preganent_number + red_health + red_12_40) > 250)
#none of the days has more than 250 white patients + red pregnant + red health + red_12_40

red_41_60 <- rep(0,length(d))
for(i in seq_along(d)){
  small_df <- sofa2 %>% filter(date == d[i])
  red_41_60[i] <- sum(small_df$group == "Red" & small_df$age_41_60 == 1)
}
sum((white_number + red_preganent_number + red_health + red_12_40 + red_41_60) > 250)
#start with red age_41_60

#for patients who meet the exclusion criteria
cohort_new$exclusion[is.na(cohort_new$exclusion)] <- 0
sofa2 <- sofa2 %>% add_column(exclusion = 0)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$exclusion <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$exclusion
}

#exclusion-high/low
sofa2 <- sofa2 %>% add_column(exclusion_high = 0)
sofa2 <- sofa2 %>% add_column(exclusion_low = 0)
for (i in 1:nrow(sofa2)) {
  if(sofa2[i,]$exclusion == 1 & sofa2[i,]$Canada <= 7){
    sofa2[i,]$exclusion_high <- 1
    sofa2[i,]$exclusion_low <- 0
  } else if(sofa2[i,]$exclusion == 1 & sofa2[i,]$Canada >= 8){
    sofa2[i,]$exclusion_high <- 0
    sofa2[i,]$exclusion_low <- 1
  } else {
    sofa2[i,]$exclusion_high <- 0
    sofa2[i,]$exclusion_low <- 0
  }
  
  
}


#first_day
origin_survive <- rep(0,length(d))
for (i in seq_along(d)) {
  small_df <- sofa2 %>% filter(date == d[i])
  origin_survive[i] <- nrow(small_df)
}
df3 <- data.frame(date = d, number = origin_survive)
the_first_date <- df3$date[df3$number > 250][1]

#"2020-03-31"
small_df <- sofa2 %>% filter(date == "2020-03-31")
ex_df_0 <- sofa2[sofa2$exclusion == 1 & sofa2$date == "2020-03-31" & sofa2$intubated_day == 0,] #in exclusion group & on day 0, no ventilator





sofa2$group <- factor(sofa2$group, levels = c("White","Red","Yellow"))
sofa2$pregnant <- factor(sofa2$pregnant, levels = c(1,0))
sofa2$healthcare <- factor(sofa2$healthcare, levels = c(1,0))
sofa2$exclusion_high <- factor(sofa2$exclusion_high, levels = c(1,0))
sofa2$age_12_40 <- factor(sofa2$age_12_40, levels = c(1,0))
sofa2$age_41_60 <- factor(sofa2$age_41_60, levels = c(1,0))
sofa2$age_61_75 <- factor(sofa2$age_61_75, levels = c(1,0))
sofa2$age_75_more <- factor(sofa2$age_75_more, levels = c(1,0))

#dead
sofa2 <- sofa2 %>% add_column(dead = 0)
cohort_new$dead[is.na(cohort_new$dead)] <- 0
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$dead <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$dead
  
  
}

#new
sofa2 <- sofa2 %>% add_column(new = 0)
sofa2$new <- ifelse(sofa2$intubated_day == 0, 1, 0)

file_name <- paste0("date",".RData")
save(d,df3,file = file_name)

file_name2 <- paste0("dataset",".RData")
save(sofa2, cohort_new, sofa_new, file = file_name2)


start_date <- as.Date(character(0))
last_date <- as.Date(character(0))
total <- rep(0,10000)
v_all <- NULL
df_sum <- NULL
group_give_all <- NULL
group_receive_all <- NULL
sofa3_all <- NULL
for (r in 1:10000) {
  
  cat("seed = ", r, "\n")
  set.seed(r)
  sofa3 <- sofa2
  df <- vector("list", 110)
  group_give <- NULL
  group_receive <- NULL
  v <- rep(0,length(d))
  
  for (i in seq_along(d)) { #for each day
    sdf <- sofa3 %>% filter(date == d[i])
    v[i] <- length(unique(sdf$pat_id))
    if(d[i] == "2020-04-10" & nrow(sdf) <= 250){
      sdf2_noe <- sdf2[sdf2$exclusion == 0,]
      sdf2_noe$give <- 1
      df_given1 <- sdf2_noe %>% group_by(group,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
      sdf2_e <- sdf2[sdf2$exclusion == 1,]
      sdf2_e$give <- 1
      df_given2 <- sdf2_e %>% group_by(exclusion_high,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
      colnames(df_given1)[1] <- "group/exclusion"
      colnames(df_given2)[1] <- "group/exclusion"
      df[[i]] <- rbind(df_given1,df_given2)
    }
    
    if(d[i] == "2020-04-15" & nrow(sdf) <= 250){
      sdf2_noe <- sdf2[sdf2$exclusion == 0,]
      sdf2_noe$give <- 1
      df_given1 <- sdf2_noe %>% group_by(group,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
      sdf2_e <- sdf2[sdf2$exclusion == 1,]
      sdf2_e$give <- 1
      df_given2 <- sdf2_e %>% group_by(exclusion_high,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
      colnames(df_given1)[1] <- "group/exclusion"
      colnames(df_given2)[1] <- "group/exclusion"
      df[[i]] <- rbind(df_given1,df_given2)
    }
    
    
    if(nrow(sdf) > 250){
      exclu <- sofa3[(sofa3$exclusion == 1 & sofa3$date == d[i]),]
      exclu_n <- nrow(exclu)
      more_n <- nrow(sdf) - 250
      
      if(exclu_n >= more_n){
        rank <- exclu %>% frankv(c("exclusion_high","pregnant","healthcare","age_12_40","age_41_60","age_61_75","age_75_more"),ties.method="random")
        exclu$rank <- rank
        
        #patients who received ventilators
        #n_receive <- exclu_n - more_n
        rank_id_exclu <- exclu$pat_enc_csn_id[order(exclu$rank)]
        rank_id_noexclu <- sofa3[(sofa3$exclusion == 0 & sofa3$date == d[i]),]$pat_enc_csn_id
        
        if(exclu_n == more_n){
          y <- rank_id_noexclu
        } else {
          y <- c(rank_id_exclu[1:(exclu_n-more_n)],rank_id_noexclu)
        }
       
        #patients who are allocated ventilators are on their day0
        potential_b <- NULL
        for (m in y) {
          single <- sdf[sdf$pat_enc_csn_id == m,]$intubated_day
          if(single == 0){
            potential_b <- c(potential_b,m) #part of y and on day0
          }
        }
        remove <- rank_id_exclu[!(rank_id_exclu %in% y)]
        #group_give information
        group_give <- rbind(group_give, sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% remove,])
        
        #group_receive information
        set.seed(r)
        give <- NULL
        for (z in remove) {
          single <- sdf[sdf$pat_enc_csn_id == z,]$intubated_day
          if(single != 0){
            give <- c(give,z) #patients not on day 0 and in remove
          }
        }
        new_groupb <- sample(potential_b, length(give))
        group_receive <- rbind(group_receive,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb,])
        
        #remove patients who are not allocated
        sofa3 <- sofa3[!(sofa3$pat_enc_csn_id %in% remove),] 
        sdf2 <- sofa3 %>% filter(date == d[i])
        v[i] <- length(sdf2$pat_enc_csn_id) 
        
        
        sdf2_noe <- sdf2[sdf2$exclusion == 0,]
        sdf2_noe$give <- 1
        df_given1 <- sdf2_noe %>% group_by(group,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
        sdf2_e <- sdf2[sdf2$exclusion == 1,]
        sdf2_e$give <- 1
        df_given2 <- sdf2_e %>% group_by(exclusion_high,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
        remove_df <- sdf[sdf$pat_enc_csn_id %in% remove,]
        remove_df$give <- 0
        df_taken1 <- remove_df %>% group_by(exclusion_high,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
        colnames(df_given1)[1] <- "group/exclusion"
        colnames(df_given2)[1] <- "group/exclusion"
        colnames(df_taken1)[1] <- "group/exclusion"
        df[[i]] <- rbind(df_given1,df_given2,df_taken1)
        last_date[r] <- d[i]
        
      } else if(exclu_n < more_n){
        exclu <- sofa3[(sofa3$exclusion == 1 & sofa3$date == d[i]),]
        anoth_df <- sofa3 %>% filter(date == d[i])
        sofa3 <- sofa3[!(sofa3$pat_enc_csn_id %in% exclu$pat_enc_csn_id),]
        sdf <- sofa3 %>% filter(date == d[i])
        #v[i] <- length(sdf$pat_enc_csn_id) 

        
          rank <- sdf %>% frankv(c("group","pregnant","healthcare","age_12_40","age_41_60","age_61_75","age_75_more"),ties.method="random")
          
          sdf$rank <- rank
          
          #patients who received ventilators
          #n_receive <- nrow(sdf) - 250
          rank_id <- sdf$pat_enc_csn_id[order(sdf$rank)]
          y <- rank_id[1:250]
          #patients who are allocated ventilators are on their day0
          potential_b <- NULL
          for (m in y) {
            single <- sdf[sdf$pat_enc_csn_id == m,]$intubated_day
            if(single == 0){
              potential_b <- c(potential_b,m) #part of y and on day0
            }
          }
          remove <- rank_id[!(rank_id %in% y)]
          remove_all <- c(remove,exclu$pat_enc_csn_id)
          
          #group_give information
          group_give <- rbind(group_give,anoth_df[anoth_df$date == d[i] & anoth_df$pat_enc_csn_id %in% remove_all,])
          
          #group_receive information
          set.seed(r)
          give <- NULL
          for (z in remove_all) {
            single <- anoth_df[anoth_df$pat_enc_csn_id == z,]$intubated_day
            if(single != 0){
              give <- c(give,z) #patients not on day 0 and in remove_all
            }
          }
          new_groupb <- sample(potential_b, length(give))
          group_receive <- rbind(group_receive,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb,])
          
          
          #remove patients who are not allocated
          sofa3 <- sofa3[!(sofa3$pat_enc_csn_id %in% remove),] 
          sdf2 <- sofa3 %>% filter(date == d[i])
          v[i] <- length(sdf2$pat_enc_csn_id) 
          
        
        sdf2$give <- 1
        df_given <- sdf2 %>% group_by(group,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
        remove_df <- sdf[sdf$pat_enc_csn_id %in% remove,]
        remove_df$give <- 0
        df_taken_1 <- remove_df %>% group_by(group,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
        exclu$give <- 0
        df_taken_2 <- exclu %>% group_by(exclusion_high,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new) %>% dplyr::summarize(n=n())
        colnames(df_given)[1] <- "group/exclusion"
        colnames(df_taken_1)[1] <- "group/exclusion"
        colnames(df_taken_2)[1] <- "group/exclusion"
        df[[i]] <- rbind(df_given,df_taken_1,df_taken_2)
        last_date[r] <- d[i]
      }

    } 
    
  }
  
  df_sum[[r]] <- df
  start_date[r] <- d[v==250][1]
  total[r] <- as.numeric(last_date[r]-start_date[r]+1)
  v_all[[r]] <- v
  group_give_all[[r]] <- group_give
  group_receive_all[[r]] <- group_receive
  sofa3_all[[r]] <- sofa3
  
  
  
}

file_name <- paste0("ventilator_CA",".RData")
save(start_date,last_date,total,v_all,group_give_all,group_receive_all,
     df_sum,sofa3_all,file = file_name)


df_allocation <- NULL
for(i in 91:106){
  cat("i = ", i, "\n")
  df <- NULL
  for(j in 1:10000){
    #cat("j = ", j, "\n")
    df <- rbind(data.frame(df_sum[[j]][[i]]),df)
  }

  
  df2 <- ddply(df,.(group.exclusion,pregnant,healthcare,age_12_40,age_41_60,age_61_75,age_75_more,give,new),summarize,n=sum(n))
  df2$n <- df2$n/10000
  df_allocation[[i]] <- df2
}

library(openxlsx)
sheetnames <- paste0("Sheet", seq_along(df_allocation))
names(df_allocation) <- sheetnames
write.xlsx(df_allocation,"df.xlsx")

a <- sapply(1:100,function(j){
  v_all[[j]][101]
}
  )

#id frequency in group_give
id_all <- NULL
for (i in 1:10000) {
  cat("i = ", i, "\n")
  id_all <- c(id_all,group_give_all[[i]]$pat_enc_csn_id)
}
df1 <- data.frame(table(id_all))
writexl::write_xlsx(df1,"id_give.xlsx")

df_new <- data.frame(adi2[adi2$date >= "2020-03-31" & adi2$date <= "2020-04-14" & adi2$new == 1,]$pat_enc_csn_id)
writexl::write_xlsx(df_new,"new_id_penn.xlsx")

id_all <- NULL
for (i in 1:9909) {
  id_all <- c(id_all,group_give_all_15[[i]]$pat_enc_csn_id)
}
df1 <- data.frame(table(id_all))
writexl::write_xlsx(df1,"f.xlsx")

