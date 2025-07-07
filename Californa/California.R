library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(tibble)
library(data.table)

#import data from excel
allo <- read_excel("daily_values 20230118 share.xlsx",sheet = "DailyValues",skip = 1)
cohort <- read_excel("cohort results 20230118 share.xlsx",sheet = "Sheet1", skip = 1)

#select important variables
allo_new <- allo %>% dplyr::select("pat_enc_csn_id",pat_id, date, "Cali-psN")
allo_new <- allo_new[!is.na(allo_new$pat_id),]
allo_new$date <- as.Date(allo_new$date, format = "%Y-%m-%d")
cohort_new <- cohort %>% dplyr::select("pat_enc_csn_id","intubation_date","extubation_date","pat_id","ever_on_ecmo","ecmo_start_time","race","ethnicity","age (y)","sex","Critical Occupation...601","pregnant...607","immediate post-operative care of complex surgical patiets...604","patients receviing solid organ transplants...605","date...606","eCali...721","914","m*Cali...730","m**Cali...741")
cohort_new <- cohort_new[!is.na(cohort_new$intubation_date),]
cohort_new$"intubation_date" <- as.Date(cohort_new$"intubation_date",format = "%Y-%m-%d")
cohort_new$"extubation_date" <- as.Date(cohort_new$"extubation_date",format = "%Y-%m-%d")
cohort_new$"ecmo_start_time" <- as.Date(cohort_new$"ecmo_start_time",format = "%Y-%m-%d")


#variable names
colnames(cohort_new)[9] <- "age"
colnames(cohort_new)[11] <- "worker"
colnames(cohort_new)[12] <- "pregnant"
colnames(cohort_new)[13] <- "non_transplant"
colnames(cohort_new)[14] <- "transplant"
colnames(cohort_new)[15] <- "surgery_date"
colnames(cohort_new)[16] <- "blue"
colnames(cohort_new)[17] <- "dead"
colnames(cohort_new)[18] <- "moderate_comor"
colnames(cohort_new)[19] <- "severe_comor"
cohort_new$surgery_date <- as.Date(cohort_new$surgery_date,format = "%Y-%m-%d")

#select patients that ever on ecmo
ecmo <- cohort_new[which(cohort_new$ever_on_ecmo == TRUE),]
#for patients that ever on ecmo, remove rows with dates on and after ecmo
id_ecmo <- unique(allo_new$pat_id)[unique(allo_new$pat_id) %in% ecmo$pat_id]
#patients who ever on ecmo
list_ecmo <- lapply(id_ecmo, function(i){
  edate <- ecmo$ecmo_start_time[ecmo$pat_id == i]
  df <- allo_new[allo_new$pat_id == i,]
  df <- df[df$date < edate,]
})
allo2 <- allo_new[!allo_new$pat_id %in% id_ecmo,] #patients who are never on ecmo
for (i in seq_along(list_ecmo)) {
  allo2 <- rbind(list_ecmo[[i]],allo2)
}

#788010911
allo2[allo2$pat_enc_csn_id == 788010911,]$`Cali-psN` <- 0

#788268315
allo2[allo2$pat_enc_csn_id == 788268315,]$`Cali-psN` <- 0

#787331860
allo2[allo2$pat_enc_csn_id == 787331860,]$`Cali-psN` <- 0

#Z4356816
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 788339050,]
cohort_new[cohort_new$pat_enc_csn_id == 786664746,]$extubation_date <- as.Date("2020-04-09",format = "%Y-%m-%d")
allo2[allo2$date == "2020-04-09" & allo2$pat_id == "Z4356816",]$pat_enc_csn_id <- 786664746


#remove rows with NA in allo2
allo2 <- na.omit(allo2)

#patients who have intubated for several times
patients <- unique(allo2$pat_id)
all_patients <- patients[patients %in% cohort_new$pat_id]
allo2 <- allo2[allo2$pat_id %in% all_patients,]
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
allo2[allo2$pat_id == "Z3280540",]$pat_enc_csn_id <- 782413361


#Z3396672
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 782427976,]


#Z3991010
cohort_new[cohort_new$pat_enc_csn_id == 784132634,]$extubation_date <- as.Date("2020-03-06",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 785875951,]

#Z4084619
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 787203795,]

#Z4356816
cohort_new[cohort_new$pat_enc_csn_id == 788339050,]$intubation_date <- as.Date("2020-04-09",format = "%Y-%m-%d")

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
allo2 <- allo2[!allo2$pat_enc_csn_id == 790942699,]

#Z8294445
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 790136689,]

#Z8233127
cohort_new[cohort_new$pat_enc_csn_id == 785303551,]$extubation_date <- as.Date("2020-03-20",format = "%Y-%m-%d")
cohort_new <- cohort_new[!cohort_new$pat_enc_csn_id == 786915976,]

#update more2
a2 <- table(cohort_new$pat_id) > 1
more2 <- names(table(cohort_new$pat_id))[a2]


#first_day
b <- unique(allo2$date)
d <- b[order(b)]
origin_survive <- rep(0,length(d))
for (i in seq_along(d)) {
  small_df <- allo2 %>% filter(date == d[i])
  origin_survive[i] <- nrow(small_df)
}
df3 <- data.frame(date = d, number = origin_survive)
the_first_date <- df3$date[df3$number > 250][1] #"2020-03-31"


#add a column "intubated_day" in allo2
#patients who enrolled once
allo2 <- allo2 %>% add_column(intubated_day = NA)
#patients who intubated once
n_patients <- all_patients[!all_patients %in% more2]
for (i in n_patients) {
  a <- nrow(allo2[allo2$pat_id == i & allo2$date >= "2020-03-31",])
  if(a>1){
    allo2[allo2$pat_id == i & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
  } else if(a==1){
    allo2[allo2$pat_id == i & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0
  } else {
    allo2 <- allo2
  }
}

#2 or 3
allo2[allo2$pat_id == "Z1906509" & allo2$date == "2020-03-31",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 786016195 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 786016195 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 791550882 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 791550882 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 788269246 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 788269246 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789245736 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789245736 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 791120280 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 791120280 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z2656846" & allo2$date == "2020-04-12",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 788644652 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 788644652 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789649061 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789649061 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 790462332 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 790462332 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z2901070" & allo2$date == "2020-06-04",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 789566487 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789566487 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z2948019" & allo2$date == "2020-06-03",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 783121775 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 783121775 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 792667499 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 792667499 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z3338372" & allo2$date == "2020-04-06",][,"intubated_day"] <- 0
allo2[allo2$pat_id == "Z3338372" & allo2$date == "2020-05-04",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 790119175 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 790119175 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 790036591 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 790036591 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 790986190 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 790986190 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_id == "Z4356816" & allo2$date >= "2020-03-31",])
allo2[allo2$pat_id == "Z4356816" & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 791953941 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 791953941 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z4871793" & allo2$date == "2020-05-04",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 787687798 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 787687798 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 791007947 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 791007947 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 791207654 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 791207654 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787673627 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 787673627 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z6223718" & allo2$date == "2020-06-21",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 787224307 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 787224307 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 794089108 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 794089108 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 793664768 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 793664768 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787237409 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 787237409 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 793126852 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 793126852 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 788101006 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 788101006 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789266937 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789266937 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789274599 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789274599 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 793608455 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 793608455 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789378735 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789378735 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
allo2[allo2$pat_id == "Z7746201" & allo2$date == "2020-06-04",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 788161858 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 788161858 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 786079332 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 786079332 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789265934 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789265934 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 793199394 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 793199394 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 794693333 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 794693333 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787879524 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 787879524 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789979608 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789979608 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 790638199 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 790638199 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 788609048 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 788609048 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787682207 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 787682207 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 793379339 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 793379339 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789453091 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789453091 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 788994407 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 788994407 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 792806263 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 792806263 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 789599382 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 789599382 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 791231030 & allo2$date >= "2020-03-31",])
allo2[allo2$pat_enc_csn_id == 791231030 & allo2$date >= "2020-03-31",][,"intubated_day"] <- 0:(a-1)



#patients with gaps
a <- nrow(allo2[allo2$pat_enc_csn_id == 787589003 & allo2$date >= "2020-04-08",])
allo2[allo2$pat_enc_csn_id == 787589003 & allo2$date >= "2020-04-08",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787201513 & allo2$date >= "2020-04-09",])
allo2[allo2$pat_enc_csn_id == 787201513 & allo2$date >= "2020-04-09",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 786808306 & allo2$date >= "2020-04-13",])
allo2[allo2$pat_enc_csn_id == 786808306 & allo2$date >= "2020-04-13",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 785992482 & as.character(allo2$date) %in% c("2020-04-07","2020-04-08"),])
allo2[allo2$pat_enc_csn_id == 785992482 & as.character(allo2$date) %in% c("2020-04-07","2020-04-08"),][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 785992482 & allo2$date >= "2020-04-12",])
allo2[allo2$pat_enc_csn_id == 785992482 & allo2$date >= "2020-04-12",][,"intubated_day"] <- 0:(a-1)
sofa_gap <- allo2[allo2$pat_enc_csn_id == 786079332, ]
allo2[allo2$pat_enc_csn_id == 786079332 & allo2$date == "2020-04-14",][,"intubated_day"] <- 0
allo2[allo2$pat_enc_csn_id == 788093316 & allo2$date == "2020-04-15",][,"intubated_day"] <- 0
a <- nrow(allo2[allo2$pat_enc_csn_id == 787235801 & allo2$date >= "2020-04-14",])
allo2[allo2$pat_enc_csn_id == 787235801 & allo2$date >= "2020-04-14",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787507206 & allo2$date >= "2020-04-11",])
allo2[allo2$pat_enc_csn_id == 787507206 & allo2$date >= "2020-04-11",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787021423 & allo2$date >= "2020-04-10",])
allo2[allo2$pat_enc_csn_id == 787021423 & allo2$date >= "2020-04-10",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 788329900 & allo2$date >= "2020-04-18",])
allo2[allo2$pat_enc_csn_id == 788329900 & allo2$date >= "2020-04-18",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787685061 & allo2$date >= "2020-04-19",])
allo2[allo2$pat_enc_csn_id == 787685061 & allo2$date >= "2020-04-19",][,"intubated_day"] <- 0:(a-1)
a <- nrow(allo2[allo2$pat_enc_csn_id == 787651330 & allo2$date >= "2020-04-16",])
allo2[allo2$pat_enc_csn_id == 787651330 & allo2$date >= "2020-04-16",][,"intubated_day"] <- 0:(a-1)

allo2 <- allo2[!is.na(allo2$intubated_day),]

#add pregnancy
allo2 <- allo2 %>% add_column(pregnant = NA)
cohort_new[is.na(cohort_new$pregnant),]$pregnant <- 0
for (i in 1:nrow(allo2)) {
  pid <- allo2[i,]$pat_enc_csn_id
  allo2[i,]$pregnant <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$pregnant
}
allo2$pregnant <- as.numeric(allo2$pregnant)

#add critical care worker
allo2 <- allo2 %>% add_column(worker = NA)
cohort_new[is.na(cohort_new$worker),]$worker <- 0
for (i in 1:nrow(allo2)) {
  pid <- allo2[i,]$pat_enc_csn_id
  allo2[i,]$worker <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$worker
}
allo2$worker <- as.numeric(allo2$worker)
allo2[allo2$worker == 1 & allo2$intubated_day == 3,]$`Cali-psN` <- allo2[allo2$worker == 1 & allo2$intubated_day == 3,]$`Cali-psN`-4

wdf <- allo2[allo2$worker == 1,]
for(i in 1:nrow(wdf)){
  if(wdf$intubated_day[i] %% 3 == 0){
    if((wdf$intubated_day[i] %/% 3) > 1){
      id <- wdf[i,]$pat_enc_csn_id
      allo2[allo2$pat_enc_csn_id == id & allo2$intubated_day == wdf$intubated_day[i],]$`Cali-psN` <- allo2[allo2$pat_enc_csn_id == id & allo2$intubated_day == wdf$intubated_day[i],]$`Cali-psN`-2
    }
  } else {
    allo2 <- allo2
  }
}


#add blue
allo2 <- allo2 %>% add_column(blue = NA)
cohort_new[is.na(cohort_new$blue),]$blue <- 0
for (i in 1:nrow(allo2)) {
  pid <- allo2[i,]$pat_enc_csn_id
  allo2[i,]$blue <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$blue
}
allo2$blue <- as.numeric(allo2$blue)

#exempt
#worker
allo2 <- allo2 %>% add_column(exempt = 0)
allo2[allo2$worker==1 & allo2$intubated_day <3,]$exempt <- 1
wid <- unique(allo2[allo2$worker == 1,]$pat_enc_csn_id)
for(i in wid){
  a <- nrow(allo2[allo2$pat_enc_csn_id == i & allo2$intubated_day > 2,])
  if(a>1){
    allo2[allo2$pat_enc_csn_id == i & allo2$intubated_day <= 2,]$intubated_day <- -9999
    allo2[allo2$pat_enc_csn_id == i & allo2$intubated_day > 2,][,"intubated_day"] <- 0:(a-1)
  } else if(a==1){
    allo2[allo2$pat_enc_csn_id == i & allo2$intubated_day <= 2,]$intubated_day <- -9999
    allo2[allo2$pat_enc_csn_id == i & allo2$intubated_day > 2,][,"intubated_day"] <- 0
  } else {
    allo2[allo2$pat_enc_csn_id == i & allo2$intubated_day <= 2,]$intubated_day <- -9999
  }
}

#non-transplant surgery
allo2 <- allo2 %>% add_column(non_transplant = 0)
cohort_new[is.na(cohort_new$"non_transplant"),]$"non_transplant" <- 0
for (i in 1:nrow(allo2)) {
  pid <- allo2[i,]$pat_enc_csn_id
  allo2[i,]$"non_transplant" <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$"non_transplant"
}
allo2$"non_transplant" <- as.numeric(allo2$"non_transplant")
cohort_new[cohort_new$pat_enc_csn_id == 787609604,]$surgery_date <- as.Date("2020-03-12",format = "%Y-%m-%d")
ntid <- unique(allo2[allo2$non_transplant == 1,]$pat_enc_csn_id)
for(i in ntid){
  sd <- cohort_new[cohort_new$pat_enc_csn_id == i,]$"surgery_date"
  range <- seq(as.Date(sd),by="day",length.out=5)
  allo2[allo2$pat_enc_csn_id == i & allo2$date %in% range,]$exempt <- 1
  a <- nrow(allo2[allo2$pat_enc_csn_id == i & allo2$date > tail(range,n=1),])
  if(a>1){
    allo2[allo2$pat_enc_csn_id == i & allo2$date > tail(range,n=1),][,"intubated_day"] <- 0:(a-1)
    allo2[allo2$pat_enc_csn_id == i & allo2$date <= tail(range,n=1),][,"intubated_day"] <- -9999
  } else if(a==1){
    allo2[allo2$pat_enc_csn_id == i & allo2$date > tail(range,n=1),][,"intubated_day"] <- 0
    allo2[allo2$pat_enc_csn_id == i & allo2$date <= tail(range,n=1),][,"intubated_day"] <- -9999
  } else {
    allo2[allo2$pat_enc_csn_id == i & allo2$date <= tail(range,n=1),][,"intubated_day"] <- -9999
  }
}

#786434358
allo2[allo2$pat_enc_csn_id == 786434358,]$intubated_day <- 0:1
#788502996
allo2[allo2$pat_enc_csn_id == 788502996,]$intubated_day <- 0


#transplant surgery
allo2 <- allo2 %>% add_column(transplant = 0)
cohort_new[is.na(cohort_new$"transplant"),]$"transplant" <- 0
for (i in 1:nrow(allo2)) {
  pid <- allo2[i,]$pat_enc_csn_id
  allo2[i,]$"transplant" <- cohort_new[cohort_new$pat_enc_csn_id == pid,]$"transplant"
}
allo2$"transplant" <- as.numeric(allo2$"transplant")
tid <- unique(allo2[allo2$transplant == 1,]$pat_enc_csn_id)
for(i in tid){
  sd <- cohort_new[cohort_new$pat_enc_csn_id == i,]$"surgery_date"
  range <- seq(as.Date(sd),by="day",length.out=10)
  allo2[allo2$pat_enc_csn_id == i & allo2$date %in% range,]$exempt <- 1
  a <- nrow(allo2[allo2$pat_enc_csn_id == i & allo2$date > tail(range,n=1),])
  if(a>1){
    allo2[allo2$pat_enc_csn_id == i & allo2$date > tail(range,n=1),][,"intubated_day"] <- 0:(a-1)
    allo2[allo2$pat_enc_csn_id == i & allo2$date <= tail(range,n=1),][,"intubated_day"] <- -9999
  } else if(a==1){
    allo2[allo2$pat_enc_csn_id == i & allo2$date > tail(range,n=1),][,"intubated_day"] <- 0
    allo2[allo2$pat_enc_csn_id == i & allo2$date <= tail(range,n=1),][,"intubated_day"] <- -9999
  } else {
    allo2[allo2$pat_enc_csn_id == i & allo2$date <= tail(range,n=1),][,"intubated_day"] <- -9999
  }
}


#pregnancy
#allo2[allo2$pregnant == 1,]$exempt <- 1

#add group
allo2 <- allo2 %>% add_column(group = NA)
allo4 <- allo2[allo2$exempt == 1,]
allo4[allo4$exempt == 1,]$group <- "violet"
allo3 <- allo2[allo2$exempt == 0,]
allo_score <- as.numeric(allo3$`Cali-psN`)
intu <- allo3$intubated_day
exem <- allo3$exempt
aid <- allo3$pat_enc_csn_id
for (i in 1:nrow(allo3)) {
  if(intu[i]==0){ #initial day
    id <- aid[i]
    if(allo_score[i] <= 3){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day <= 2,]$group <- "red"
    } else if(allo_score[i]>=4 & allo_score[i]<=6){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day <= 2,]$group <- "orange"
    } else if(allo_score[i]>=7 & allo_score[i]<=8){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day <= 2,]$group <- "yellow"
    }
  }
  if(intu[i]==3){ #72 hours
    id <- aid[i]
    if(allo_score[i] <= 3){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day %in% c(3,4,5),]$group <- "red"
    } else if(allo_score[i]>=4 & allo_score[i]<=6){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day %in% c(3,4,5),]$group <- "orange"
    } else if((allo_score[i]>=7 & allo_score[i]<=8)|(allo_score[i]-allo_score[i-3]>=3)){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day%in% c(3,4,5), ]$group <- "yellow"
    }
  }

  if((intu[i]>3)&(intu[i]%%3==0)){
    id <- aid[i]
    if(allo_score[i] <= 3){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day %in% c(intu[i],intu[i]+1,intu[i]+2),]$group <- "red"
    } else if(allo_score[i]>=4 & allo_score[i]<=6){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day %in% c(intu[i],intu[i]+1,intu[i]+2),]$group <- "orange"
    } else if((allo_score[i]>=7 & allo_score[i]<=8)|(allo_score[i]-allo_score[i-3]>=2)){
      allo3[allo3$pat_enc_csn_id == id & allo3$intubated_day %in% c(intu[i],intu[i]+1,intu[i]+2),]$group <- "yellow"
    }
  }

}
allo2 <- rbind(allo4,allo3)

#remove rows with NA in allo2
allo2 <- na.omit(allo2)

allo2[allo2$blue == 1,]$group <- "blue"
allo2[allo2$blue == 0,]$blue <- 2
allo2 <- allo2 %>% add_column(violet = 2)
allo2[allo2$group == "violet",]$violet <- 1
allo2 <- allo2 %>% add_column(red = 2)
allo2[allo2$group == "red",]$red <- 1
allo2 <- allo2 %>% add_column(orange = 2)
allo2[allo2$group == "orange",]$orange <- 1
allo2 <- allo2 %>% add_column(yellow = 2)
allo2[allo2$group == "yellow",]$yellow <- 1

allo2 <- allo2 %>% add_column(day2 = 0)
allo2$day2 <- allo2$intubated_day
sid <- unique(allo2[allo2$intubated_day == -9999,]$pat_enc_csn_id)
for(i in sid){
  a <- nrow(allo2[allo2$pat_enc_csn_id == i,])
  if(a>1){
    allo2[allo2$pat_enc_csn_id == i,][,"day2"] <- 0:(a-1)
  } else if(a==1){
    allo2[allo2$pat_enc_csn_id == i,][,"day2"] <- 0
  }
}


file_name <- paste0("date",".RData")
save(d,df3,file = file_name)

file_name2 <- paste0("dataset",".RData")
save(allo2, cohort_new, file = file_name2)


last_date <- as.Date(character(0))
total <- rep(0,10000)
v_all <- NULL
group_give_all <- NULL
group_receive_all <- NULL
allo3_all <- NULL
df_receive_all <- NULL
d2 <- d[91:306]
for (r in 1:10000) {

  cat("seed = ", r, "\n")
  set.seed(r)
  allo3 <- allo2
  group_give <- NULL
  group_receive <- NULL
  v <- rep(0,length(d2))
  df_receive <- NULL

  sdf_all_id <- NULL
  for (i in 1:15) { #for each day
    sdf <- allo3 %>% filter(date == d2[i])
    v[i] <- length(unique(sdf$pat_id))

    if(nrow(sdf) > 250){

        rank <- sdf %>% frank("violet","red","orange","yellow","blue",ties.method="random")
        sdf$rank <- rank

        #patients who received ventilators
        rank_id <- sdf$pat_enc_csn_id[order(sdf$rank)]
        y <- rank_id[1:250]

        #patients who are allocated ventilators are on their day0
        potential_b <- NULL
        for (m in y) {
          single <- sdf[sdf$pat_enc_csn_id == m,]$"day2"
          if(single == 0){
            potential_b <- c(potential_b,m) #part of y and on day0
          }
        }

        remove <- rank_id[!(rank_id %in% y)]
        #group_give information
        group_give <- rbind(group_give,allo3[allo3$date == d2[i] & allo3$pat_enc_csn_id %in% remove,])

        #group_receive information
        set.seed(r)
        give <- NULL
        for (z in remove) {
          single <- sdf[sdf$pat_enc_csn_id == z,]$"day2"
          if(single != 0){
            give <- c(give,z) #patients not on day 0 and in remove
          }
        }
        new_groupb <- sample(potential_b, length(give))
        group_receive <- rbind(group_receive,allo3[allo3$date == d2[i] & allo3$pat_enc_csn_id %in% new_groupb,])


        #remove patients who are not allocated
        allo3 <- allo3[!(allo3$pat_enc_csn_id %in% remove),]
        sdf2 <- allo3 %>% filter(date == d2[i])
        v[i] <- length(sdf2$pat_enc_csn_id)
        last_date[r] <- d2[i]
    }

    sdf2 <- allo3 %>% filter(date == d2[i])
    sdf2$new <- 0
    sdf2[sdf2$"day2" == 0,]$new <- 1
    sdf2$n <- rep(1,nrow(sdf2))
    df_receive[[i]] <- sdf2 %>% group_by(group,new) %>% dplyr::summarize(n=n())
    sdf_all_id <- c(sdf_all_id,sdf2[sdf2$new == 1,]$pat_enc_csn_id)

  }

  for (i in 16:216) { #for each day
    sdf <- allo3 %>% filter(date == d2[i])
    v[i] <- length(unique(sdf$pat_id))
  }



  df_receive_all[[r]] <- df_receive
  v_all[[r]] <- v
  group_give_all[[r]] <- group_give
  group_receive_all[[r]] <- group_receive
  allo3_all[[r]] <- allo3

}

file_name <- paste0("ventilator_carlifornia2",".RData")
save(last_date,v_all,group_give_all,group_receive_all,allo3_all,df_receive_all,file = file_name)

new_red7 <- rep(0,10000)
for(i in 1:10000){
  new_red7[i] <- df_receive_all[[i]][[8]][df_receive_all[[i]][[8]]$group == "red" & df_receive_all[[i]][[8]]$new == 1,]$n + nrow(group_give_all[[i]][group_give_all[[i]]$red == 1 & group_give_all[[i]]$day2 == 0 & group_give_all[[i]]$date == "2020-04-07",])
}


library(writexl)
dfid <- data.frame(unique(allo2[allo2$date >= "2020-03-31" & allo2$date <= "2020-04-14",]$pat_enc_csn_id))
write_xlsx(dfid,"id_before.xlsx")


id_all <- NULL
for (i in 1:10000) {
  cat("i = ", i, "\n")
  id_all <- c(id_all,group_give_all[[i]]$pat_enc_csn_id)
}
df1 <- data.frame(table(id_all))
writexl::write_xlsx(df1,"id_give.xlsx")

df_r <- NULL
for(i in 1:15){
  df <- NULL
  for(j in 1:10000){
    cat("j = ", j, "\n")
    df <- rbind(data.frame(df_receive_all[[j]][[i]]),df)
  }


  df2 <- ddply(df,.(group,new),summarize,n=sum(n))
  df2$n <- df2$n/10000
  df_r[[i]] <- df2
}

library(openxlsx)
sheetnames <- paste0("Sheet", seq_along(df_r))
names(df_r) <- sheetnames
write.xlsx(df_r,"df_r.xlsx")



day_n <- NULL
day_per <- rep(0,13)
names(day_per) <- 0:12
for (i in 1:10000) {
  cat("i = ", i, "\n")
  day_n <- c(day_n,group_give_all[[i]]$day2)
  per <- c(table(group_give_all[[i]]$day2)/length(group_give_all[[i]]$day2),day_per)
  day_per <- tapply(per,names(per),sum)
}
df1 <- data.frame(table(day_n)/10000)
df1$per <- day_per[order(as.numeric(names(day_per)))]
writexl::write_xlsx(df1,"distribution.xlsx")

n1 <- n2 <- n3 <- n4 <- n5 <- n6 <- n7 <- n8 <- n9 <- n10 <- rep(0,10000)
for(i in 1:10000){
  cat("i = ", i, "\n")
  daf <- group_give_all[[i]]
  v1 <- daf[daf$group == "violet" & daf$day2 == 0,]$pat_enc_csn_id
  outcome_all1 <- as.character(rep(0,length(v1)))
  for (j in seq_along(v1)) {
    outcome_all1[j] <- cohort_new[cohort_new$pat_enc_csn_id == v1[j],]$"dead"
  }
  n1[i] <- table(outcome_all1)[1]/length(v1)

  v2 <- daf[daf$group == "violet" & daf$day2 != 0,]$pat_enc_csn_id
  outcome_all2 <- as.character(rep(0,length(v2)))
  for (j in seq_along(v2)) {
    outcome_all2[j] <- cohort_new[cohort_new$pat_enc_csn_id == v2[j],]$"dead"
  }
  n2[i] <- table(outcome_all2)[1]/length(v2)

  v3 <- daf[daf$group == "red" & daf$day2 == 0,]$pat_enc_csn_id
  outcome_all3 <- as.character(rep(0,length(v3)))
  for (j in seq_along(v3)) {
    outcome_all3[j] <- cohort_new[cohort_new$pat_enc_csn_id == v3[j],]$"dead"
  }
  n3[i] <- table(outcome_all3)["alive"]/length(v3)

  v4 <- daf[daf$group == "red" & daf$day2 != 0,]$pat_enc_csn_id
  outcome_all4 <- as.character(rep(0,length(v4)))
  for (j in seq_along(v4)) {
    outcome_all4[j] <- cohort_new[cohort_new$pat_enc_csn_id == v4[j],]$"dead"
  }
  n4[i] <- table(outcome_all4)["alive"]/length(v4)

  v5 <- daf[daf$group == "orange" & daf$day2 != 0,]$pat_enc_csn_id
  outcome_all5 <- as.character(rep(0,length(v5)))
  for (j in seq_along(v5)) {
    outcome_all5[j] <- cohort_new[cohort_new$pat_enc_csn_id == v5[j],]$"dead"
  }
  n5[i] <- table(outcome_all5)["alive"]/length(v5)

  v6 <- daf[daf$group == "orange" & daf$day2 == 0,]$pat_enc_csn_id
  outcome_all6 <- as.character(rep(0,length(v6)))
  for (j in seq_along(v6)) {
    outcome_all6[j] <- cohort_new[cohort_new$pat_enc_csn_id == v6[j],]$"dead"
  }
  n6[i] <- table(outcome_all6)["alive"]/length(v6)

  v7 <- daf[daf$group == "yellow" & daf$day2 != 0,]$pat_enc_csn_id
  outcome_all7 <- as.character(rep(0,length(v7)))
  for (j in seq_along(v7)) {
    outcome_all7[j] <- cohort_new[cohort_new$pat_enc_csn_id == v7[j],]$"dead"
  }
  n7[i] <- table(outcome_all7)["alive"]/length(v7)

  v8 <- daf[daf$group == "yellow" & daf$day2 == 0,]$pat_enc_csn_id
  outcome_all8 <- as.character(rep(0,length(v8)))
  for (j in seq_along(v8)) {
    outcome_all8[j] <- cohort_new[cohort_new$pat_enc_csn_id == v8[j],]$"dead"
  }
  n8[i] <- table(outcome_all8)["alive"]/length(v8)

  v9 <- daf[daf$group == "blue" & daf$day2 != 0,]$pat_enc_csn_id
  outcome_all9 <- as.character(rep(0,length(v9)))
  for (j in seq_along(v9)) {
    outcome_all9[j] <- cohort_new[cohort_new$pat_enc_csn_id == v9[j],]$"dead"
  }
  n9[i] <- table(outcome_all9)["alive"]/length(v9)

  v10 <- daf[daf$group == "blue" & daf$day2 == 0,]$pat_enc_csn_id
  outcome_all10 <- as.character(rep(0,length(v10)))
  for (j in seq_along(v10)) {
    outcome_all10[j] <- cohort_new[cohort_new$pat_enc_csn_id == v10[j],]$"dead"
  }
  n10[i] <- table(outcome_all10)["alive"]/length(v10)

}

n3[is.na(n3)] <- 0

df_give <- NULL
for(i in 91:105){
  cat("i = ", i, "\n")
  df <- NULL
  for(j in 1:10000){
    cat("j = ", j, "\n")
    ndf <- group_give_all[[j]]
    df <- rbind(data.frame(ndf[ndf$date == d[i],]),df)
  }

  df$new <- 0
  df[df$"day2" == 0,]$new <- 1
  df$n <- rep(1,nrow(df))
  df2 <- ddply(df,.(group,new),summarize,n=sum(n))
  df2$n <- df2$n/10000
  df_give[[i]] <- df2
}

library(openxlsx)
sheetnames <- paste0("Sheet", seq_along(df_give))
names(df_give) <- sheetnames
write.xlsx(df_give,"df_give.xlsx")


#remove
count1 = 0
for(i in 91:105){
  s_df <- df_give[[i]]
  count1 = count1 + sum(s_df$n[s_df$new == 1])
}
#receive
count2 = 0
for(i in 1:15){
  s_df <- df_r[[i]]
  count2 = count2 + sum(s_df$n[s_df$new == 1])
}

count1+count2


new <- data.frame(pat_enc_csn_id=allo$pat_enc_csn_id,s2 =allo$`Cali-ps`,date=allo$date)
new$"date" <- as.Date(new$"date",format = "%Y-%m-%d")
m1 <- s1 <- n1 <- r1 <- m2 <- s2 <- n2 <- r2 <- m3 <- s3 <- n3 <- r3 <- m4 <- s4 <- n4 <- r4 <- rm <- rs <- rn <- n_all <- rep(0,1000)
#give_orange_old
for(i in 1:1000){
  cat("i = ", i, "\n")
  df_all <- group_give_all[[i]]
  n_all[i] <- nrow(df_all)
  #afo <- merge(afo,new,by=c("pat_enc_csn_id","date"))
  #df_all <- afo[afo$group == "orange" & afo$day2 != 0,]
  df_all_id <- df_all$pat_enc_csn_id
  df_all$s2 <- 0
  for (j in seq_along(df_all_id)) {
    iid <- df_all[df_all$pat_enc_csn_id == df_all_id[j],]
    if(iid$day2 %% 3 == 0){
      df_all[df_all$pat_enc_csn_id == df_all_id[j],]$s2 <- na.omit(new[new$pat_enc_csn_id == df_all_id[j] & new$date == iid$date,]$s2)[1]
    } else if(iid$day2 %% 3 == 1){
      df_all[df_all$pat_enc_csn_id == df_all_id[j],]$s2 <- na.omit(new[new$pat_enc_csn_id == df_all_id[j] & new$date == (iid$date-1),]$s2)[1]
    } else if(iid$day2 %% 3 == 2){
      df_all[df_all$pat_enc_csn_id == df_all_id[j],]$s2 <- na.omit(new[new$pat_enc_csn_id == df_all_id[j] & new$date == (iid$date-2),]$s2)[1]
    }
  }


  #1 allocation point
  df1_id <- df_all[df_all$s2 == 1,]$pat_enc_csn_id
  mo_id <- cohort_new[cohort_new$moderate_comor == 1 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  mo_id <- mo_id[!is.na(mo_id)]
  m1[i] <- sum(df1_id %in% mo_id)
  se_id <- cohort_new[cohort_new$severe_comor == 1 & cohort_new$intubation_date < "2020-04-15" & cohort_new$extubation_date > "2020-03-30",]$pat_enc_csn_id
  se_id <- se_id[!is.na(se_id)]
  s1[i] <- sum(df1_id %in% se_id)
  n1[i] <- sum(!(df1_id %in% c(mo_id,se_id)))
  r1[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df1_id,]$dead)["alive"]/length(df1_id)

  #2 allocation points
  df2_id <- df_all[df_all$s2 == 2,]$pat_enc_csn_id
  m2[i] <- sum(df2_id %in% mo_id)
  s2[i] <- sum(df2_id %in% se_id)
  n2[i] <- sum(!(df2_id %in% c(mo_id,se_id)))
  r2[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df2_id,]$dead)["alive"]/length(df2_id)

  #3 allocation points
  df3_id <- df_all[df_all$s2 == 3,]$pat_enc_csn_id
  m3[i] <- sum(df3_id %in% mo_id)
  s3[i] <- sum(df3_id %in% se_id)
  n3[i] <- sum(!(df3_id %in% c(mo_id,se_id)))
  r3[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df3_id,]$dead)["alive"]/length(df3_id)

  #4 allocation points
  df4_id <- df_all[df_all$s2 == 4,]$pat_enc_csn_id
  m4[i] <- sum(df4_id %in% mo_id)
  s4[i] <- sum(df4_id %in% se_id)
  n4[i] <- sum(!(df4_id %in% c(mo_id,se_id)))
  r4[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% df4_id,]$dead)["alive"]/length(df4_id)

  #severe
  ma <- df_all_id[df_all_id %in% mo_id]
  sa <- df_all_id[df_all_id %in% se_id]
  na <- df_all_id[!df_all_id %in% c(mo_id,se_id)]
  rm[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% ma,]$dead)["alive"]/length(ma)
  rs[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% sa,]$dead)["alive"]/length(sa)
  rn[i] <- table(cohort_new[cohort_new$pat_enc_csn_id %in% na,]$dead)["alive"]/length(na)

}

r1[is.na(r1)] <- 0
dcrease_all[i] <- length(unlist(dcrease))
increase_point_all[i] <- length(unlist(increase_point))
decrease_point_all[i] <- length(unlist(decrease_point))


all <- last_eavl <- same_all <- same2_all <- increase_all <-decrease_all <- increase_point_all <- decrease_point_all <- day1_all <- day2_all <- group_all <- rep(0,1000)
for(i in 1:1000){
  cat("i = ", i, "\n")
  df_all <- group_give_all[[i]]
  last_eavl[i] <- mean(df_all$day2 %% 3)
  same <- increase <- decrease <- increase_point <- decrease_point <- same2 <- day1 <- day2 <- group <- NULL
  for(j in 1:nrow(df_all)){
    day <- df_all[j,]$day2
    if(day %% 3 == 0){
      same[[j]] <- df_all[j,]$pat_enc_csn_id
      group[[j]] <- 1
    } else if(day %% 3 == 1){
      id <- df_all[j,]$pat_enc_csn_id
      day1[[j]] <- id
      date <- df_all[j,]$date
      point1 <- df_all[j,]$`Cali-psN`
      point2 <- allo2[allo2$pat_enc_csn_id == id & allo2$date == (date-1),]$`Cali-psN`
      g <- df_all[j,]$group
      group[[j]] <- as.numeric(allo2[allo2$pat_enc_csn_id == id & allo2$date == (date-1),]$group == g)
      if(id == 787844664){
        point2 <- 1
        group[[j]] <- 1
      }
      diff_point <- point1 - point2
      if(diff_point > 0){
        increase[[j]] <- id
        increase_point[[j]] <- diff_point
      } else if(diff_point < 0){
        decrease[[j]] <- id
        decrease_point[[j]] <- diff_point
      } else if(diff_point == 0){
        same2[[j]] <- id
      }
    } else if(day %% 3 == 2){
      id <- df_all[j,]$pat_enc_csn_id
      day2[[j]] <- id
      date <- df_all[j,]$date
      point1 <- df_all[j,]$`Cali-psN`
      point2 <- allo2[allo2$pat_enc_csn_id == id & allo2$date == (date-2),]$`Cali-psN`
      g <- df_all[j,]$group
      group[[j]] <- as.numeric(allo2[allo2$pat_enc_csn_id == id & allo2$date == (date-2),]$group == g)
      diff_point <- point1 - point2
      if(diff_point > 0){
        increase[[j]] <- id
        increase_point[[j]] <- diff_point
      } else if(diff_point < 0){
        decrease[[j]] <- id
        decrease_point[[j]] <- diff_point
      } else if(diff_point == 0){
        same2[[j]] <- id
      }
    } 
      
  }
  same_all[i] <- length(unlist(same))
  day1_all[i] <- length(unlist(day1))
  day2_all[i] <- length(unlist(day2))
  increase_all[i] <- length(unlist(increase))
  decrease_all[i] <- length(unlist(decrease))
  same2_all[[i]] <- length(unlist(same2))
  increase_point_all[i] <- mean(unlist(increase_point))
  decrease_point_all[i] <- mean(unlist(decrease_point))
  group_all[[i]] <- mean(unlist(group))

}



sapply(group_give_all,nrow)


n_worker <- rep(0,length(group_give_all))
for(i in 1:length(group_give_all)){
  df <- group_give_all[[i]]
  n_worker[i] <- nrow(df[df$worker == 1,])
}



