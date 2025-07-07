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
cohort_new <- cohort %>% dplyr::select("pat_enc_csn_id","intubation_time","intubation_date","extubation_date","pat_id","ever_on_ecmo","ecmo_start_time","race","ethnicity","age (y)","sex","914")
cohort_new <- cohort_new[!is.na(cohort_new$intubation_time),]
cohort_new$"intubation_date" <- as.Date(cohort_new$"intubation_date",format = "%Y-%m-%d")
cohort_new$"extubation_date" <- as.Date(cohort_new$"extubation_date",format = "%Y-%m-%d")
cohort_new$"ecmo_start_time" <- as.Date(cohort_new$"ecmo_start_time",format = "%Y-%m-%d")

#variable names
colnames(cohort_new)[10] <- "age"
colnames(cohort_new)[12] <- "dead"

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
the_first_date <- df3$date[df3$number > 250][1]



allo3 <- allo2
group_give <- NULL
group_receive_new <- NULL
v <- rep(0,length(d))
receive_old <- receive_new <- give <- rep(0,length(d))

for(i in 1:length(d)){ #for each day
  sdf <- allo3 %>% filter(date == d[i])
  v[i] <- length(unique(sdf$pat_id))

  if(nrow(sdf) > 250){
    ptid <- sdf$pat_enc_csn_id
    adf <- cohort_new[cohort_new$pat_enc_csn_id %in% ptid,]
    receive_old[i] <- nrow(adf[adf$intubation_date < d[i],])
    extra_number <- 250-receive_old[i]

    #how many new patients
    adf2 <- adf[adf$intubation_date >= d[i],]
    order_id <- adf2[order(adf2$intubation_time),]$pat_enc_csn_id
    #first_come first_serve
    #group_receive_new
    receive_id <- order_id[1:extra_number]
    receive_new[i] <- length(receive_id)
    group_receive_new <- rbind(group_receive_new,allo3[allo3$date == d[i] & allo3$pat_enc_csn_id %in% receive_id,])

    #remove patients who are not allocated
    remove <- order_id[!(order_id %in% receive_id)]
    #group_give
    give[i] <- length(remove)
    group_give <- rbind(group_give,allo3[allo3$date == d[i] & allo3$pat_enc_csn_id %in% remove,])
    allo3 <- allo3[!(allo3$pat_enc_csn_id %in% remove),]
    sdf2 <- allo3 %>% filter(date == d[i])
    v[i] <- length(sdf2$pat_enc_csn_id)
  }
}
start_date <-  as.Date(d[v==250][1],format = ("%Y-%m-%d"))
last_date <- as.Date(d[v==250][length(d[v==250])],format = ("%Y-%m-%d"))

new <- data.frame(pat_enc_csn_id=cohort_new$pat_enc_csn_id,date=cohort_new$intubation_date,time=cohort_new$intubation_time)
group_give <- merge(group_give,new,by=c("pat_enc_csn_id","date"))


file_name <- paste0("FCFS",".RData")
save(start_date,last_date,receive_old,receive_new,give,group_receive_new,group_give,v,cohort_new,d,
     file = file_name)



