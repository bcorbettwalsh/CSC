library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(tibble)
library(data.table)

#import data from excel
adi <- read_excel("daily_values 20230912 share.xlsx",skip = 1)
cohort2 <- read_excel("cohort results 20230912 share.xlsx",sheet = "Sheet1", skip = 1)

#select important variables
adi_new <- adi %>% dplyr::select("pat_enc_csn_id",pat_id, date,"Pittsburgh-psN...9")
colnames(adi_new)[4] <- "penn"
adi_new$date <- as.Date(adi_new$date, format = "%Y-%m-%d")
cohort_new <- cohort2 %>% dplyr::select("pat_enc_csn_id","intubation_date","extubation_date","pat_id","914","ever_on_ecmo","ecmo_start_time","race","ethnicity","age (y)","sex","m**Pittsburgh...683","Essential Worker","ADI 8+")
cohort_new <- cohort_new[!is.na(cohort_new$intubation_date),]
cohort_new$"intubation_date" <- as.Date(cohort_new$"intubation_date",format = "%Y-%m-%d")
cohort_new$"extubation_date" <- as.Date(cohort_new$"extubation_date",format = "%Y-%m-%d")
cohort_new$"ecmo_start_time" <- as.Date(cohort_new$"ecmo_start_time",format = "%Y-%m-%d")
#788010911
adi_new[adi_new$pat_enc_csn_id == 788010911,]$penn <- 0

#788268315
adi_new[adi_new$pat_enc_csn_id == 788268315,]$penn <- 0

#787331860
adi_new[adi_new$pat_enc_csn_id == 787331860,]$penn <- 0

#788170310
adi_new[adi_new$pat_enc_csn_id == 788170310 & adi_new$date == "2020-04-06",]$penn <- 0

#788107440
adi_new[adi_new$pat_enc_csn_id == 788107440 & adi_new$date == "2020-04-05",]$penn <- 0

adi_new <- na.omit(adi_new)

#variable names
colnames(cohort_new)[10] <- "age"
colnames(cohort_new)[5] <- "dead"
colnames(cohort_new)[12] <- "severe_comor"
colnames(cohort_new)[13] <- "essential"
colnames(cohort_new)[14] <- "social"


#select patients that ever on ecmo
ecmo <- cohort_new[which(cohort_new$ever_on_ecmo == TRUE),]
#for patients that ever on ecmo, remove rows with dates on and after ecmo
id_ecmo <- unique(adi_new$pat_id)[unique(adi_new$pat_id) %in% ecmo$pat_id]
#patients who ever on ecmo
list_ecmo <- lapply(id_ecmo, function(i){
  edate <- ecmo$ecmo_start_time[ecmo$pat_id == i]
  df <- adi_new[adi_new$pat_id == i,]
  df <- df[df$date < edate,]
})
adi2 <- adi_new[!adi_new$pat_id %in% id_ecmo,] #patients who are never on ecmo
for (i in seq_along(list_ecmo)) {
  adi2 <- rbind(list_ecmo[[i]],adi2)
}

#remove rows with NA in adi2
adi2 <- na.omit(adi2)

#patients who have intubated for several times
patients <- unique(adi2$pat_id)
all_patients <- patients[patients %in% cohort_new$pat_id]
#2202 in total
adi2 <- adi2[adi2$pat_id %in% all_patients,]
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
adi2[adi2$pat_id == "Z3280540",]$pat_enc_csn_id <- 782413361


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
adi2[adi2$pat_id == "Z4356816",]$pat_enc_csn_id <- 786664746


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
adi2 <- adi2[!adi2$pat_enc_csn_id == 790942699,]

#8294446
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 790136689,]

#update more2
a2 <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a2]

#add a column "intubated_day" in adi2
#patients who enrolled once
adi2 <- adi2 %>% add_column(intubated_day = NA)
#patients who intubated once
n_patients <- all_patients[!all_patients %in% more2]
for (i in n_patients) {
  a <- nrow(adi2[adi2$pat_id == i,][,"intubated_day"])
  adi2[adi2$pat_id == i,][,"intubated_day"] <- 0:(a-1)
}

#2 or 3
for (i in more2) {
  c_more2 <- cohort_new[cohort_new$pat_id == i,]
  s_more2 <- adi2[adi2$pat_id == i,]
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
    adi2[adi2$pat_id == i & adi2$date == t,][,"intubated_day"] <- ss_f[ss_f$date == t, ]$intubated_day
  }
}

#patients with gaps
adi2[adi2$pat_enc_csn_id == 785303551 & adi2$date >= "2020-03-17",][,"intubated_day"] <- 18:21
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787589003, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787589003 & adi2$date >= "2020-04-08",])
adi2[adi2$pat_enc_csn_id == 787589003 & adi2$date >= "2020-04-08",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787201513, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787201513 & adi2$date >= "2020-04-09",])
adi2[adi2$pat_enc_csn_id == 787201513 & adi2$date >= "2020-04-09",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 786808306, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 786808306 & adi2$date >= "2020-04-13",])
adi2[adi2$pat_enc_csn_id == 786808306 & adi2$date >= "2020-04-13",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 785992482, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 785992482 & as.character(adi2$date) %in% c("2020-04-07","2020-04-08"),])
adi2[adi2$pat_enc_csn_id == 785992482 & as.character(adi2$date) %in% c("2020-04-07","2020-04-08"),][,"intubated_day"] <- 0:(a-1)
a <- nrow(adi2[adi2$pat_enc_csn_id == 785992482 & adi2$date >= "2020-04-12",])
adi2[adi2$pat_enc_csn_id == 785992482 & adi2$date >= "2020-04-12",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 786079332, ]
adi2[adi2$pat_enc_csn_id == 786079332 & adi2$date == "2020-04-14",][,"intubated_day"] <- 0
sofa_gap <- adi2[adi2$pat_enc_csn_id == 788093316, ]
adi2[adi2$pat_enc_csn_id == 788093316 & adi2$date == "2020-04-15",][,"intubated_day"] <- 0
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787235801, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787235801 & adi2$date >= "2020-04-14",])
adi2[adi2$pat_enc_csn_id == 787235801 & adi2$date >= "2020-04-14",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787507206, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787507206 & adi2$date >= "2020-04-11",])
adi2[adi2$pat_enc_csn_id == 787507206 & adi2$date >= "2020-04-11",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787021423, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787021423 & adi2$date >= "2020-04-10",])
adi2[adi2$pat_enc_csn_id == 787021423 & adi2$date >= "2020-04-10",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 788329900, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 788329900 & adi2$date >= "2020-04-18",])
adi2[adi2$pat_enc_csn_id == 788329900 & adi2$date >= "2020-04-18",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787685061, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787685061 & adi2$date >= "2020-04-19",])
adi2[adi2$pat_enc_csn_id == 787685061 & adi2$date >= "2020-04-19",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 787651330, ]
a <- nrow(adi2[adi2$pat_enc_csn_id == 787651330 & adi2$date >= "2020-04-16",])
adi2[adi2$pat_enc_csn_id == 787651330 & adi2$date >= "2020-04-16",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- adi2[adi2$pat_enc_csn_id == 788386858, ]
sofa_gap <- adi2[adi2$pat_enc_csn_id == 788266226, ]


#use the value of day1 for day0
#ONLY FOR PATIENTS WITH SOFA AS EMPTY ON DAY0
E0 <- adi2$"penn" == "EMPTY" & adi2$intubated_day == 0
p0 <- adi2[E0,]
t0 <- table(p0$pat_id)
p1 <- names(t0)[t0 == 1]
for (i in seq_along(p1)) {
  a <- nrow(adi2[adi2$pat_id == p1[i],])
  if(a > 1){
    adi2[adi2$pat_id == p1[i], "penn"][1,] <- adi2[adi2$pat_id == p1[i], "penn"][2,]
  }
}

#replace the remaining EMPTY as 0
adi2$"penn"[adi2$"penn" == "EMPTY"] <- "0"
adi2$"penn" <- as.numeric(adi2$"penn")

#add white
intu <- adi2$intubated_day
adi2 <- adi2 %>% add_column(white = NA)
for (i in 1:nrow(adi2)) {
  if(intu[i]%%3==0){ #every 72 hours
    adi2$"white"[i] = 1
  } else {
    adi2$"white"[i] = 0
  }
}


#add 12-40 in cohort_new
#remove 787668565
adi2 <- adi2[!adi2$pat_enc_csn_id == 787668565,]
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787668565,]
cohort_new <- cohort_new %>% add_column(age_12_40 = 0)
cohort_new[(cohort_new$age >= 12 & cohort_new$age < 40),]$age_12_40 <- 1
#add 12-40 in adi2
adi2 <- adi2 %>% add_column(age_12_40 = NA)
for (i in 1:nrow(adi2)) {
  pid <- adi2[i,]$pat_enc_csn_id
  adi2[i,]$age_12_40 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_12_40
}

#add 41-60 in cohort_new
cohort_new <- cohort_new %>% add_column(age_41_60 = 0)
cohort_new[(cohort_new$age >= 40 & cohort_new$age < 60),]$age_41_60 <- 1
#add 41-60 in adi2
adi2 <- adi2 %>% add_column(age_41_60 = NA)
for (i in 1:nrow(adi2)) {
  pid <- adi2[i,]$pat_enc_csn_id
  adi2[i,]$age_41_60 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_41_60
}

#add 61-75 in cohort_new
cohort_new <- cohort_new %>% add_column(age_61_75 = 0)
cohort_new[(cohort_new$age >= 60 & cohort_new$age < 75),]$age_61_75 <- 1
#add 61-75 in adi2
adi2 <- adi2 %>% add_column(age_61_75 = NA)
for (i in 1:nrow(adi2)) {
  pid <- adi2[i,]$pat_enc_csn_id
  adi2[i,]$age_61_75 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_61_75
}

#add 75+ in cohort_new
cohort_new <- cohort_new %>% add_column(age_75_more = 0)
cohort_new[(cohort_new$age >= 75),]$age_75_more <- 1
#add 75+ in adi2
adi2 <- adi2 %>% add_column(age_75_more = NA)
for (i in 1:nrow(adi2)) {
  pid <- adi2[i,]$pat_enc_csn_id
  adi2[i,]$age_75_more <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_75_more
}

#essential
cohort_new[is.na(cohort_new$essential),]
#now the big dataset is `adi2`

#first_day
b <- unique(adi2$date)
d <- b[order(b)]
origin_survive <- rep(0,length(d))
for (i in seq_along(d)) {
  small_df <- adi2 %>% filter(date == d[i])
  origin_survive[i] <- nrow(small_df)
}
df3 <- data.frame(date = d, number = origin_survive)
the_first_date <- df3$date[df3$number > 250][1]

adi2$penn <- factor(adi2$penn)
adi2$white <- factor(adi2$white)
adi2$age_12_40 <- factor(adi2$age_12_40, levels = c(1,0))
adi2$age_41_60 <- factor(adi2$age_41_60, levels = c(1,0))
adi2$age_61_75 <- factor(adi2$age_61_75, levels = c(1,0))
adi2$age_75_more <- factor(adi2$age_75_more, levels = c(1,0))

#dead
adi2 <- adi2 %>% add_column(dead = 0)
cohort_new$dead[is.na(cohort_new$dead)] <- 0
for (i in 1:nrow(adi2)) {
  pid <- adi2[i,]$pat_enc_csn_id
  adi2[i,]$dead <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$dead

}

#new
adi2 <- adi2 %>% add_column(new = 0)
adi2$new <- ifelse(adi2$intubated_day == 0, 1, 0)


file_name <- paste0("date",".RData")
save(d,df3,file = file_name)

file_name2 <- paste0("dataset",".RData")
save(adi2, cohort_new, adi_new, file = file_name2)


start_date <- as.Date(character(0))
last_date <- as.Date(character(0))
total <- rep(0,10000)
v_all <- NULL
df_sum <- NULL
group_give_all <- NULL
group_receive_all <- NULL
adi3_all <- NULL
df_receive_all <- NULL
d2 <- d[91:306]

for (r in 1:10000) {
  
  cat("seed = ", r, "\n")
  set.seed(r)
  adi3 <- adi2
  df <- vector("list", 110)
  group_give <- NULL
  group_receive <- NULL
  v <- rep(0,length(d2))
  df_receive <- NULL
  
  
  
  for (i in 1:15) { #for each day
    sdf <- adi3 %>% filter(date == d2[i])
    v[i] <- length(unique(sdf$pat_id))
    
    if(nrow(sdf) > 250){
      
      rank <- sdf %>% frankv(c("white","penn","age_12_40","age_41_60","age_61_75","age_75_more"),ties.method="random")
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
      remove_all <- rank_id[!(rank_id %in% y)]
      
      #group_give information
      group_give <- rbind(group_give,sdf[sdf$date == d2[i] & sdf$pat_enc_csn_id %in% remove_all,])
      
      set.seed(r)
      give <- NULL
      for (z in remove_all) {
        single <- sdf[sdf$pat_enc_csn_id == z,]$intubated_day
        if(single != 0){
          give <- c(give,z) #patients not on day 0 and in remove_all
        }
      }
      
      #group_receive information
      new_groupb <- sample(potential_b, length(give))
      group_receive <- rbind(group_receive,adi3[adi3$date == d2[i] & adi3$pat_enc_csn_id %in% new_groupb,])
      
      #remove patients who are not allocated
      adi3 <- adi3[!(adi3$pat_enc_csn_id %in% remove_all),] 
      sdf2 <- adi3 %>% filter(date == d2[i])
      v[i] <- length(sdf2$pat_enc_csn_id) 
      last_date[r] <- d2[i]
    }
    
    sdf2 <- adi3 %>% filter(date == d2[i])
    # sdf2$new <- 0
    # sdf2[sdf2$"day2" == 0,]$new <- 1
    sdf2$n <- rep(1,nrow(sdf2))
    options(dplyr.summarise.inform = FALSE)
    df_receive[[i]] <- sdf2 %>% group_by(penn,age_12_40,age_41_60,age_61_75,age_75_more,white,new) %>% dplyr::summarize(n=n())
    #sdf_all_id <- c(sdf_all_id,sdf2[sdf2$new == 1,]$pat_enc_csn_id)
      
  } 
  
  for (k in 16:216) { #for each day
    sdf <- adi3 %>% filter(date == d2[k])
    v[k] <- length(unique(sdf$pat_id))
  }
    
  df_receive_all[[r]] <- df_receive
  df_sum[[r]] <- df
  start_date[r] <- d2[v==250][1]
  total[r] <- as.numeric(last_date[r]-start_date[r]+1)
  v_all[[r]] <- v
  group_give_all[[r]] <- group_give
  group_receive_all[[r]] <- group_receive
  adi3_all[[r]] <- adi3
  
  
  
}

file_name <- paste0("ventilator_penn",".RData")
save(start_date,last_date,total,v_all,group_give_all,group_receive_all,df_sum,adi3_all,df_receive_all,file = file_name)

#give table everyday
df_give <- NULL
for(i in 91:105){
  cat("i = ", i, "\n")
  df <- NULL
  for(j in 1:10000){
    cat("j = ", j, "\n")
    ndf <- group_give_all[[j]]
    df <- rbind(data.frame(ndf[ndf$date == d[i],]),df)
  }
  
  #df$new <- 0
  #df[df$"day2" == 0,]$new <- 1
  df$n <- rep(1,nrow(df))
  df2 <- ddply(df,.(penn,age_12_40,age_41_60,age_61_75,age_75_more,new),summarize,n=sum(n))
  df2$n <- df2$n/10000
  df_give[[i]] <- df2
}

library(openxlsx)
sheetnames <- paste0("Sheet", seq_along(df_give))
names(df_give) <- sheetnames
write.xlsx(df_give,"df_give.xlsx")

#receive table everyday
df_r <- NULL
for(i in 1:15){
  cat("i = ", i, "\n")
  df <- NULL
  for(j in 1:10000){
    #cat("j = ", j, "\n")
    ndf <- df_receive_all[[j]][[i]]
    ndf$white <- ifelse(ndf$white == 0,1,0)
    df <- rbind(data.frame(ndf),df)
  }
  
  
  df2 <- ddply(df,.(penn,age_12_40,age_41_60,age_61_75,age_75_more,white,new),summarize,n=sum(n))
  df2$n <- df2$n/10000
  df_r[[i]] <- df2
}

library(openxlsx)
sheetnames <- paste0("Sheet", seq_along(df_r))
names(df_r) <- sheetnames
write.xlsx(df_r,"df_r.xlsx")

#id frequency in group_give
id_all <- NULL
for (i in 1:10000) {
  cat("i = ", i, "\n")
  id_all <- c(id_all,group_give_all[[i]]$pat_enc_csn_id)
}
df1 <- data.frame(table(id_all))
writexl::write_xlsx(df1,"id_give.xlsx")

#the subgroup analysis of g_give
cohort_new[is.na(cohort_new$dead),]$dead <- 0
cohort_new$dead <- as.character(cohort_new$dead)
cohort_new[cohort_new$dead == "1",]$dead <- "deceased"
cohort_new[cohort_new$dead == "0",]$dead <- "alive"
new <- data.frame(pat_enc_csn_id=adi$pat_enc_csn_id,s2 =adi$`Pittsburgh-PS`,date=adi$date)
new$"date" <- as.Date(new$"date",format = "%Y-%m-%d")
n_all <- o1 <- s1 <- os1 <- m1 <- n1 <- r_o1 <- r_s1 <- r_os1 <- r_comor1 <- r_n1 <- r_all1 <- o2 <- s2 <- os2 <- m2 <- n2 <- r_o2 <- r_s2 <- r_os2 <- r_comor2 <- r_n2 <- r_all2 <- o3 <- s3 <- os3 <- m3 <- n3 <- r_o3 <- r_s3 <- r_os3 <- r_comor3 <- r_n3 <- r_all3 <- o4 <- s4 <- os4 <- m4 <- n4 <- r_o4 <- r_s4 <- r_os4 <- r_comor4 <- r_n4 <- r_all4 <- rna <- roa <- rsa <- rosa <- rcoa <- rep(0,10000)

cohort_new[is.na(cohort_new$essential),]$essential <- 0
cohort_new[is.na(cohort_new$social),]$social <- 0
for(i in 1:10000){
  cat("i = ", i, "\n")
  df_all <- group_give_all[[i]]
  n_all[i] <- nrow(df_all)
  df_all_id <- df_all$pat_enc_csn_id
  df_all$s2 <- 0
  for (j in seq_along(df_all_id)) {
    iid <- df_all[df_all$pat_enc_csn_id == df_all_id[j],]
    df_all[df_all$pat_enc_csn_id == df_all_id[j],]$s2 <- na.omit(new[new$pat_enc_csn_id == df_all_id[j] & new$date == iid$date,]$s2)[1]
  }
  
  
  #1 allocation point
  df1_id <- df_all[df_all$s2 == 1,]$pat_enc_csn_id
  #occupation only
  occu_id <- cohort_new[cohort_new$essential == 1 & cohort_new$social == 0 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  occu_id <- occu_id[!is.na(occu_id)]
  o1[i] <- sum(df1_id %in% occu_id)
  id1 <- df1_id[df1_id %in% occu_id]
  #social only
  so_id <- cohort_new[cohort_new$social == 1 & cohort_new$essential == 0 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  so_id <- so_id[!is.na(so_id)]
  s1[i] <- sum(df1_id %in% so_id)
  id2 <- df1_id[df1_id %in% so_id]
  #occupation & social
  occu_so_id <- cohort_new[cohort_new$essential == 1 & cohort_new$social == 1 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  occu_so_id <- occu_so_id[!is.na(occu_so_id)]
  os1[i] <- sum(df1_id %in% occu_so_id)
  id3 <- df1_id[df1_id %in% occu_so_id]
  #comorbidity
  comor_id <- cohort_new[cohort_new$severe_comor == 1 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  comor_id <- comor_id[!is.na(comor_id)]
  m1[i] <- sum(df1_id %in% comor_id)
  id4 <- df1_id[df1_id %in% comor_id]
  #none
  none_id <- df1_id[!(df1_id %in% c(occu_id,so_id,occu_so_id,comor_id))]
  n1[i] <- sum(!(df1_id %in% c(occu_id,so_id,occu_so_id,comor_id)))
  #survival rate
  r_o1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_s1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_os1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_comor1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df1_id,]$dead)["alive"]/length(df1_id)
  
  #2 allocation points
  df2_id <- df_all[df_all$s2 == 2,]$pat_enc_csn_id
  #occupation only
  o2[i] <- sum(df2_id %in% occu_id)
  id1 <- df2_id[df2_id %in% occu_id]
  #social only
  s2[i] <- sum(df2_id %in% so_id)
  id2 <- df2_id[df2_id %in% so_id]
  #occupation & social
  os2[i] <- sum(df2_id %in% occu_so_id)
  id3 <- df2_id[df2_id %in% occu_so_id]
  #comorbidity
  m2[i] <- sum(df2_id %in% comor_id)
  id4 <- df2_id[df2_id %in% comor_id]
  #none
  none_id <- df2_id[!(df2_id %in% c(occu_id,so_id,occu_so_id,comor_id))]
  n2[i] <- sum(!(df2_id %in% c(occu_id,so_id,occu_so_id,comor_id)))
  #survival rate
  r_o2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_s2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_os2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_comor2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df2_id,]$dead)["alive"]/length(df2_id)
  
  #3 allocation points
  df3_id <- df_all[df_all$s2 == 3,]$pat_enc_csn_id
  #occupation only
  o3[i] <- sum(df3_id %in% occu_id)
  id1 <- df3_id[df3_id %in% occu_id]
  #social only
  s3[i] <- sum(df3_id %in% so_id)
  id2 <- df3_id[df3_id %in% so_id]
  #occupation & social
  os3[i] <- sum(df3_id %in% occu_so_id)
  id3 <- df3_id[df3_id %in% occu_so_id]
  #comorbidity
  m3[i] <- sum(df3_id %in% comor_id)
  id4 <- df3_id[df3_id %in% comor_id]
  #none
  none_id <- df3_id[!(df3_id %in% c(occu_id,so_id,occu_so_id,comor_id))]
  n3[i] <- sum(!(df3_id %in% c(occu_id,so_id,occu_so_id,comor_id)))
  #survival rate
  r_o3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_s3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_os3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_comor3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df3_id,]$dead)["alive"]/length(df3_id)
  
  #4 allocation points
  df4_id <- df_all[df_all$s2 == 4,]$pat_enc_csn_id
  #occupation only
  o4[i] <- sum(df4_id %in% occu_id)
  id1 <- df4_id[df4_id %in% occu_id]
  #social only
  s4[i] <- sum(df4_id %in% so_id)
  id2 <- df4_id[df4_id %in% so_id]
  #occupation & social
  os4[i] <- sum(df4_id %in% occu_so_id)
  id3 <- df4_id[df4_id %in% occu_so_id]
  #comorbidity
  m4[i] <- sum(df4_id %in% comor_id)
  id4 <- df4_id[df4_id %in% comor_id]
  #none
  none_id <- df4_id[!(df4_id %in% c(occu_id,so_id,occu_so_id,comor_id))]
  n4[i] <- sum(!(df4_id %in% c(occu_id,so_id,occu_so_id,comor_id)))
  #survival rate
  r_o4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_s4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_os4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_comor4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df4_id,]$dead)["alive"]/length(df4_id)
  
  #severe
  na <- df_all_id[!(df_all_id %in% c(occu_id,so_id,occu_so_id,comor_id))]
  oa <- df_all_id[df_all_id %in% occu_id]
  sa <- df_all_id[df_all_id %in% so_id]
  osa <- df_all_id[df_all_id %in% occu_so_id]
  coa <- df_all_id[df_all_id %in% comor_id]
  rna[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% na,]$dead)["alive"]/length(na)
  roa[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% oa,]$dead)["alive"]/length(oa)
  rsa[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% sa,]$dead)["alive"]/length(sa)
  rosa[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% osa,]$dead)["alive"]/length(osa)
  rcoa[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% coa,]$dead)["alive"]/length(coa)
  
}

day_n <- NULL
day_per <- rep(0,13)
names(day_per) <- 0:12
for (i in 1:10000) {
  cat("i = ", i, "\n")
  day_n <- c(day_n,group_give_all[[i]]$intubated_day)
  per <- c(table(group_give_all[[i]]$intubated_day)/length(group_give_all[[i]]$intubated_day),day_per)
  day_per <- tapply(per,names(per),sum)
}
df1 <- data.frame(table(day_n)/10000)
order = day_per[order(as.numeric(names(day_per)))]
df1$per <- order[!order == 0]
writexl::write_xlsx(df1,"distribution.xlsx")










