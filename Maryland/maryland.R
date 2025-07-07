library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(tibble)
library(data.table)

#import data from excel
sofa <- read_excel("daily_values 20230912 share.xlsx",skip = 1)
cohort2 <- read_excel("cohort results 20230912 share.xlsx",sheet = "Sheet1", skip = 1)

#select important variables
sofa_new <- sofa %>% dplyr::select("pat_enc_csn_id",pat_id, date, "Maryland-psN","SOFA Vtot")
sofa_new <- sofa_new[!is.na(sofa_new$pat_id),]
colnames(sofa_new)[4] <- "maryland"
colnames(sofa_new)[5] <- "compare"
sofa_new$date <- as.Date(sofa_new$date, format = "%Y-%m-%d")
cohort_new <- cohort2 %>% dplyr::select("pat_enc_csn_id","intubation_date","extubation_date","pat_id","914","ever_on_ecmo","ecmo_start_time","race","ethnicity","age (y)","sex","eMaryland...657","m**Maryland...666","pregnant...607")
cohort_new <- cohort_new[!is.na(cohort_new$intubation_date),]
cohort_new$"intubation_date" <- as.Date(cohort_new$"intubation_date",format = "%Y-%m-%d")
cohort_new$"extubation_date" <- as.Date(cohort_new$"extubation_date",format = "%Y-%m-%d")
cohort_new$"ecmo_start_time" <- as.Date(cohort_new$"ecmo_start_time",format = "%Y-%m-%d")
#788010911
sofa_new[sofa_new$pat_enc_csn_id == 788010911,]$maryland <- 0

#788268315
sofa_new[sofa_new$pat_enc_csn_id == 788268315,]$maryland <- 0

#787331860
sofa_new[sofa_new$pat_enc_csn_id == 787331860,]$maryland <- 0

#788170310
sofa_new[sofa_new$pat_enc_csn_id == 788170310 & sofa_new$date == "2020-04-06",]$maryland <- 0

#788107440
sofa_new[sofa_new$pat_enc_csn_id == 788107440 & sofa_new$date == "2020-04-05",]$maryland <- 0

sofa_new <- na.omit(sofa_new)

#variable names
colnames(cohort_new)[10] <- "age"
colnames(cohort_new)[5] <- "dead"
colnames(cohort_new)[12] <- "exclusion"
colnames(cohort_new)[13] <- "severe_comor"
colnames(cohort_new)[14] <- "pregnant"

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
#2205 in total
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

#Z8294446
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
E0 <- sofa2$maryland == "EMPTY" & sofa2$intubated_day == 0
p0 <- sofa2[E0,]
t0 <- table(p0$pat_id)
p1 <- names(t0)[t0 == 1]
for (i in seq_along(p1)) {
  a <- nrow(sofa2[sofa2$pat_id == p1[i],])
  if(a > 1){
    sofa2[sofa2$pat_id == p1[i], "maryland"][1,] <- sofa2[sofa2$pat_id == p1[i], "maryland"][2,]
  }
}

#replace the remaining EMPTY as 0
sofa2$"maryland"[sofa2$"maryland" == "EMPTY"] <- "0"
sofa2$"maryland" <- as.numeric(sofa2$"maryland")

#use the value of day1 for day0
#ONLY FOR PATIENTS WITH SOFA AS EMPTY ON DAY0
E0 <- sofa2$"compare" == "EMPTY" & sofa2$intubated_day == 0
p0 <- sofa2[E0,]
t0 <- table(p0$pat_id)
p1 <- names(t0)[t0 == 1]
for (i in seq_along(p1)) {
  a <- nrow(sofa2[sofa2$pat_id == p1[i],])
  if(a > 1){
    sofa2[sofa2$pat_id == p1[i], "compare"][1,] <- sofa2[sofa2$pat_id == p1[i], "compare"][2,]
  }
}

#replace the remaining EMPTY as 0
sofa2$"compare"[sofa2$"compare" == "EMPTY"] <- "0"
sofa2$"compare" <- as.numeric(sofa2$"compare")

#a new column from 1-4
sofa2 <- sofa2 %>% add_column(g_com = NA)
for (i in 1:nrow(sofa2)) {
  if(sofa2[i,]$compare <= 8) {
    sofa2[i,]$g_com <- 1
  } else if(sofa2[i,]$compare >= 9 & sofa2[i,]$compare <= 11) {
    sofa2[i,]$g_com <- 2
  } else if(sofa2[i,]$compare >= 12 & sofa2[i,]$compare <= 14) {
    sofa2[i,]$g_com <- 3
  } else if(sofa2[i,]$compare > 14) {
    sofa2[i,]$g_com <- 4
  }
}


#time trial
sofa <- as.numeric(sofa2$maryland)
intu <- sofa2$intubated_day
sofa2$maryland_adjusted <- sofa2$maryland
for (i in 1:nrow(sofa2)) {
  if(intu[i] == 1){ #day1
    if(sofa[i]>sofa[i-1]){
      sofa2$maryland_adjusted[i] <- sofa[i] + 1
    }
  } else if(intu[i] == 2){ #day2
    if(sofa[i]>sofa[i-1]){
      sofa2$maryland_adjusted[i] <- sofa[i] + 1
    }
  } else if(intu[i] == 5){ #day5
    if(sofa[i]>sofa[i-3]){
      sofa2$maryland_adjusted[i] <- sofa[i] + 1
    } 
    if(sofa[i]==sofa[i-5]){
      sofa2$maryland_adjusted[i] <- sofa[i] + 1
    }
  } else if(intu[i] > 5 & (intu[i]%%2==1)){ #odd days after day5
    if(sofa[i]>sofa[i-2]){
      sofa2$maryland_adjusted[i] <- sofa[i] + 1
    }
  } 
}

#add adjusted+ in cohort_new
sofa2 <- sofa2 %>% add_column(adjust = 0)
for (i in 1:nrow(sofa2)) {
  sofa2[i,]$adjust <- ifelse(sofa2[i,]$maryland_adjusted > sofa2[i,]$maryland,1,0)
}

#add white
intu <- sofa2$intubated_day
sofa2 <- sofa2 %>% add_column(white = NA)
for (i in 1:nrow(sofa2)) {
  if(intu[i] %in% c(0,1,2,5)){ #every 72 hours
    sofa2$"white"[i] = 1
  } else if(intu[i] > 5 & (intu[i]%%2==1)) {
    sofa2$"white"[i] = 1
  } else {
    sofa2$"white"[i] = 0
  }
}

#add 0-50 in cohort_new
cohort_new[cohort_new$pat_enc_csn_id == 787668565,]$age <- 15
cohort_new <- cohort_new %>% add_column(age_0_50 = 0)
cohort_new[(cohort_new$age >= 0 & cohort_new$age < 50),]$age_0_50 <- 1
#add 12-40 in sofa2
sofa2 <- sofa2 %>% add_column(age_0_50 = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_0_50 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_0_50
}

#add 50-70 in cohort_new
cohort_new <- cohort_new %>% add_column(age_50_70 = 0)
cohort_new[(cohort_new$age >= 50 & cohort_new$age < 70),]$age_50_70 <- 1
#add 50-70 in sofa2
sofa2 <- sofa2 %>% add_column(age_50_70 = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_50_70 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_50_70
}

#add 70-85 in cohort_new
cohort_new <- cohort_new %>% add_column(age_70_85 = 0)
cohort_new[(cohort_new$age >= 70 & cohort_new$age <= 85),]$age_70_85 <- 1
#add 70-85 in sofa2
sofa2 <- sofa2 %>% add_column(age_70_85 = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_70_85 <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_70_85
}

#add 85+ in cohort_new
cohort_new <- cohort_new %>% add_column(age_85_more = 0)
cohort_new[(cohort_new$age > 85),]$age_85_more <- 1
#add 85+ in sofa2
sofa2 <- sofa2 %>% add_column(age_85_more = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$age_85_more <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$age_85_more
}

#add pregnancy+ in sofa2
sofa2 <- sofa2 %>% add_column(pregnant = NA)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$pregnant <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$pregnant
}
sofa2$pregnant[is.na(sofa2$pregnant)] <- 0

#for patients who meet the exclusion criteria
cohort_new$exclusion[is.na(cohort_new$exclusion)] <- 0
sofa2 <- sofa2 %>% add_column(exclusion = 0)
for (i in 1:nrow(sofa2)) {
  pid <- sofa2[i,]$pat_enc_csn_id
  sofa2[i,]$exclusion <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$exclusion
}
sofa2$exclusion <- sofa2$exclusion -1
sofa2[sofa2$exclusion == -1,]$exclusion <- 1

#now the big dataset is `sofa2`

#first_day
b <- unique(sofa2$date)
d <- b[order(b)]
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

sofa2$white <- factor(sofa2$white)
sofa2$age_0_50 <- factor(sofa2$age_0_50, levels = c(1,0))
sofa2$age_50_70 <- factor(sofa2$age_50_70, levels = c(1,0))
sofa2$age_70_85 <- factor(sofa2$age_70_85, levels = c(1,0))
sofa2$age_85_more <- factor(sofa2$age_85_more, levels = c(1,0))
sofa2$exclusion <- factor(sofa2$exclusion, levels = c(1,0))

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
df_receive_all <- NULL
for (r in 1:10000) {
  
  cat("seed = ", r, "\n")
  set.seed(r)
  sofa3 <- sofa2
  df <- vector("list", 110)
  group_give <- NULL
  group_receive <- NULL
  v <- rep(0,length(d))
  df_receive <- NULL
  
  for (i in seq_along(d)) { #for each day
    
    sdf <- sofa3 %>% filter(date == d[i])
    v[i] <- length(unique(sdf$pat_id))
    
    if(nrow(sdf) > 250){
      rank <- sdf %>% frankv(c("exclusion","white","maryland_adjusted","age_0_50","age_50_70","age_70_85","age_85_more"),ties.method="random")
      sdf$rank <- rank
      #patients who received ventilators
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
      group_give <- rbind(group_give,sdf[sdf$date == d[i] & sdf$pat_enc_csn_id %in% remove_all,])
      
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
      group_receive <- rbind(group_receive,sofa3[sofa3$date == d[i] & sofa3$pat_enc_csn_id %in% new_groupb,])
      
      #remove patients who are not allocated
      sofa3 <- sofa3[!(sofa3$pat_enc_csn_id %in% remove_all),] 
      sdf2 <- sofa3 %>% filter(date == d[i])
      v[i] <- length(sdf2$pat_enc_csn_id) 
      last_date[r] <- d[i]
    }
    
    sdf2 <- sofa3 %>% filter(date == d[i])
    # sdf2$new <- 0
    # sdf2[sdf2$"day2" == 0,]$new <- 1
    sdf2$n <- rep(1,nrow(sdf2))
    options(dplyr.summarise.inform = FALSE)
    df_receive[[i]] <- sdf2 %>% group_by(exclusion,maryland_adjusted,age_0_50,age_50_70,age_70_85,age_85_more,white,new) %>% dplyr::summarize(n=n())
    #sdf_all_id <- c(sdf_all_id,sdf2[sdf2$new == 1,]$pat_enc_csn_id)
      
      
  }
  
  df_receive_all[[r]] <- df_receive
  df_sum[[r]] <- df
  start_date[r] <- d[v==250][1]
  total[r] <- as.numeric(last_date[r]-start_date[r]+1)
  v_all[[r]] <- v
  group_give_all[[r]] <- group_give
  group_receive_all[[r]] <- group_receive
  sofa3_all[[r]] <- sofa3
    
    
  }
  
  
file_name <- paste0("ventilator_maryland",".RData")
save(start_date,last_date,total,v_all,group_give_all,group_receive_all,df_sum,sofa3_all,df_receive_all,file = file_name)



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
  df2 <- ddply(df,.(exclusion,maryland_adjusted,age_0_50,age_50_70,age_70_85,age_85_more,white,new),summarize,n=sum(n))
  df2$n <- df2$n/10000
  df_give[[i]] <- df2
}

library(openxlsx)
sheetnames <- paste0("Sheet", seq_along(df_give))
names(df_give) <- sheetnames
write.xlsx(df_give,"df_give.xlsx")

#receive table everyday
df_r <- NULL
for(i in 91:105){
  cat("i = ", i, "\n")
  df <- NULL
  for(j in 1:10000){
    #cat("j = ", j, "\n")
    ndf <- df_receive_all[[j]][[i]]
    #ndf$white <- ifelse(ndf$white == 0,1,0)
    df <- rbind(data.frame(ndf),df)
  }
  
  df2 <- ddply(df,.(exclusion,maryland_adjusted,age_0_50,age_50_70,age_70_85,age_85_more,white,new),summarize,n=sum(n))
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
order = day_per[order(as.numeric(names(day_per)))]*100/length(group_give_all)
df1$per <- order[!order == 0]
writexl::write_xlsx(df1,"distribution.xlsx")

#the subgroup analysis of g_give
cohort_new[is.na(cohort_new$dead),]$dead <- 0
cohort_new$dead <- as.character(cohort_new$dead)
cohort_new[cohort_new$dead == "1",]$dead <- "deceased"
cohort_new[cohort_new$dead == "0",]$dead <- "alive"
n_all <- adj1 <- pre1 <- m1 <- am1 <- n1 <- r_adj1 <- r_pre1 <- r_m1 <- r_am1 <- r_n1 <- r_all1 <- adj2 <- pre2 <- m2 <- am2 <- n2 <- r_adj2 <- r_pre2 <- r_m2 <- r_am2 <- r_n2 <- r_all2 <- adj3 <- pre3 <- m3 <- am3 <- n3 <- r_adj3 <- r_pre3 <- r_m3 <- r_am3 <- r_n3 <- r_all3 <- adj4 <- pre4 <- m4 <- am4 <- n4 <- r_adj4 <- r_pre4 <- r_m4 <- r_am4 <- r_n4 <- r_all4 <- adja <- prea <- ma <- ama <- na <- rna <- radja <- rprea <- rma <- rama <- rep(0,10000)

for(i in 1:10000){
  cat("i = ", i, "\n")
  df_all <- group_give_all[[i]]
  df_all_id <- group_give_all[[i]]$pat_enc_csn_id
  n_all[i] <- nrow(df_all)
  
  #1 allocation point
  df1_id <- df_all[df_all$g_com== 1,]$pat_enc_csn_id
  comor_id <- cohort_new[cohort_new$severe_comor == 1 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  comor_id <- comor_id[!is.na(comor_id)]
  
  #adjusted only
  adj_id <- df_all[df_all$g_com== 1 & df_all$adjust == 1,]$pat_enc_csn_id
  id1 <- setdiff(adj_id,comor_id)
  adj1[i] <- length(id1)
  #pregnant only
  id2 <- df_all[df_all$g_com== 1 & df_all$adjust == 0 & df_all$pregnant == 1,]$pat_enc_csn_id
  pre1[i] <- length(id2)
  #comorbidity
  s1 <- setdiff(comor_id,adj_id)
  m1[i] <- sum(df1_id %in% s1)
  id3 <- df1_id[df1_id %in% s1]
  #adj & comorbidity
  am_id <- comor_id[comor_id %in% adj_id]
  am1[i] <- sum(df1_id %in% am_id)
  id4 <- df1_id[df1_id %in% am_id]
  #none
  none_id <- df1_id[!(df1_id %in% c(id1,id2,id3,id4))]
  n1[i] <- length(none_id)
  #survival rate
  r_adj1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_pre1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_m1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_am1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df1_id,]$dead)["alive"]/length(df1_id)
  
  #2 allocation point
  df2_id <- df_all[df_all$g_com== 2,]$pat_enc_csn_id
  #adjusted only
  adj_id <- df_all[df_all$g_com== 2 & df_all$adjust == 1,]$pat_enc_csn_id
  id1 <- setdiff(adj_id,comor_id)
  adj2[i] <- length(id1)
  #pregnant only
  id2 <- df_all[df_all$g_com== 2 & df_all$adjust == 0 & df_all$pregnant == 1,]$pat_enc_csn_id
  pre2[i] <- length(id2)
  #comorbidity
  s1 <- setdiff(comor_id,adj_id)
  m2[i] <- sum(df2_id %in% s1)
  id3 <- df2_id[df2_id %in% s1]
  #adj & comorbidity
  am_id <- comor_id[comor_id %in% adj_id]
  am2[i] <- sum(df2_id %in% am_id)
  id4 <- df2_id[df2_id %in% am_id]
  #none
  none_id <- df2_id[!(df2_id %in% c(id1,id2,id3,id4))]
  n2[i] <- length(none_id)
  #survival rate
  r_adj2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_pre2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_m2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_am2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df2_id,]$dead)["alive"]/length(df2_id)
  
  #3 allocation point
  df3_id <- df_all[df_all$g_com== 3,]$pat_enc_csn_id
  #adjusted only
  adj_id <- df_all[df_all$g_com== 3 & df_all$adjust == 1,]$pat_enc_csn_id
  id1 <- setdiff(adj_id,comor_id)
  adj3[i] <- length(id1)
  #pregnant only
  id2 <- df_all[df_all$g_com== 3 & df_all$adjust == 0 & df_all$pregnant == 1,]$pat_enc_csn_id
  pre3[i] <- length(id2)
  #comorbidity
  s1 <- setdiff(comor_id,adj_id)
  m3[i] <- sum(df3_id %in% s1)
  id3 <- df3_id[df3_id %in% s1]
  #adj & comorbidity
  am_id <- comor_id[comor_id %in% adj_id]
  am3[i] <- sum(df3_id %in% am_id)
  id4 <- df3_id[df3_id %in% am_id]
  #none
  none_id <- df3_id[!(df3_id %in% c(id1,id2,id3,id4))]
  n3[i] <- length(none_id)
  #survival rate
  r_adj3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_pre3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_m3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_am3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df3_id,]$dead)["alive"]/length(df3_id)
  
  #4 allocation point
  df4_id <- df_all[df_all$g_com== 4,]$pat_enc_csn_id
  #adjusted only
  adj_id <- df_all[df_all$g_com== 4 & df_all$adjust == 1,]$pat_enc_csn_id
  id1 <- setdiff(adj_id,comor_id)
  adj4[i] <- length(id1)
  #pregnant only
  id2 <- df_all[df_all$g_com== 4 & df_all$adjust == 0 & df_all$pregnant == 1,]$pat_enc_csn_id
  pre4[i] <- length(id2)
  #comorbidity
  s1 <- setdiff(comor_id,adj_id)
  m4[i] <- sum(df4_id %in% s1)
  id3 <- df4_id[df4_id %in% s1]
  #adj & comorbidity
  am_id <- comor_id[comor_id %in% adj_id]
  am4[i] <- sum(df4_id %in% am_id)
  id4 <- df4_id[df4_id %in% am_id]
  #none
  none_id <- df4_id[!(df4_id %in% c(id1,id2,id3,id4))]
  n4[i] <- length(none_id)
  #survival rate
  r_adj4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id1,]$dead)["alive"]/length(id1)
  r_pre4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id2,]$dead)["alive"]/length(id2)
  r_m4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id3,]$dead)["alive"]/length(id3)
  r_am4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% id4,]$dead)["alive"]/length(id4)
  r_n4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% none_id,]$dead)["alive"]/length(none_id)
  r_all4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df4_id,]$dead)["alive"]/length(df4_id)
  
  
  #severe
  adja_all <- df_all[df_all$adjust == 1,]$pat_enc_csn_id
  adja <- setdiff(adja_all,comor_id)
  prea <- df_all[df_all$adjust == 0 & df_all$pregnant == 1,]$pat_enc_csn_id
  s1 <- setdiff(comor_id,adja_all)
  ma <- df_all_id[df_all_id %in% s1]
  ama <- df_all_id[df_all_id %in% comor_id[comor_id %in% adja_all]]
  na <- df_all_id[!(df_all_id %in% c(adja,prea,ma,ama))]
  radja[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% adja,]$dead)["alive"]/length(adja)
  rprea[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% prea,]$dead)["alive"]/length(prea)
  rma[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% ma,]$dead)["alive"]/length(ma)
  rama[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% ama,]$dead)["alive"]/length(ama)
  rna[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% na,]$dead)["alive"]/length(na)
  
}






