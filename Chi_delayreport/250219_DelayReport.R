---
title: "Delay Report"
output: html_document
date: "03/05/2025"
---
########
install.packages("dplyr")
library(dplyr)
library(stringr)

welcome_call_raw <- read.csv("C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250401_SERVES-Welcome&Call_DATA_w_sample.csv")
add_com_raw <- read.csv("C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250401_SERVES-AddCommunication_DATA_w_sample.csv")

welcome_call_sample <- read.csv("C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250401_SERVES-Welcome&Call_DATA_sampleonly.csv")
add_com_sample <- read.csv("C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250401_SERVES-AddCommunication_DATA_sampleonly.csv")

sample_id_welcome_call <- unique(welcome_call_sample$record_id)
sample_id_add_com <- unique(add_com_sample$record_id)
welcome_call_raw <- subset(welcome_call_raw, !(record_id %in% sample_id_welcome_call))
add_com_raw <- subset(add_com_raw, !(record_id %in% sample_id_add_com))

add_com_raw <- add_com_raw %>%
  mutate(r_text1_date = NA,
         .before = "r_text1_time")

add_com_raw <- add_com_raw %>%
  mutate(r_text2_date = NA,
         .before = "r_text2_time")

add_com_raw <- add_com_raw %>%
  mutate(r_email_date = NA,
         .before = "r_email_time")

add_com_raw <- add_com_raw %>%
  mutate(r_call_date = NA,
         .before = "r_call_time")

welcome_call_raw$welcome_date <- as.Date(welcome_call_raw$welcome_date)
welcome_call_raw$datecall <- as.Date(welcome_call_raw$datecall)
welcome_call_raw$shortsurveydate <- as.Date(welcome_call_raw$shortsurveydate)
welcome_call_raw$caps_date <- as.Date(welcome_call_raw$caps_date)
# welcome_call_raw$timecall <- strptime(welcome_call_raw$timecall)
# welcome_call_raw$shortsurveytime <- strptime(welcome_call_raw$shortsurveytime)
# welcome_call_raw$caps_time <- strptime(welcome_call_raw$caps_time)

text1_date <- str_split_fixed(add_com_raw$r_text1_time, " ",2)
add_com_raw$r_text1_date <- as.Date(text1_date[,1], format="%Y-%m-%d")
add_com_raw$r_text1_time <- text1_date[,2]

text2_date <- str_split_fixed(add_com_raw$r_text2_time, " ",2)
add_com_raw$r_text2_date <- as.Date(text2_date[,1], format="%Y-%m-%d")
add_com_raw$r_text2_time <- text2_date[,2]

email_date <- str_split_fixed(add_com_raw$r_email_time, " ",2)
add_com_raw$r_email_date <- as.Date(email_date[,1], format="%Y-%m-%d")
add_com_raw$r_email_time <- email_date[,2]

call_date <- str_split_fixed(add_com_raw$r_call_time, " ",2)
add_com_raw$r_call_date <- as.Date(call_date[,1], format="%Y-%m-%d")
add_com_raw$r_call_time <- call_date[,2]

glimpse(welcome_call_raw)
glimpse(add_com_raw)

welcome_set <- subset(subset(welcome_call_raw, redcap_repeat_instrument == "welcome_packet"), select = c(record_id:welcome_packet_complete))
recruit_call_set <- subset(subset(welcome_call_raw, redcap_repeat_instrument == "recruitment_call"), select = c(record_id:redcap_repeat_instance,datecall:recruitment_call_complete))
result_set_1 <- subset(subset(welcome_call_raw, redcap_repeat_instrument == ""), select = c(record_id:redcap_repeat_instance, recruit_consent_complete___1:recruitment_result_complete))
recruit_text_set <- subset(add_com_raw, select = c(record_id:redcap_repeat_instance, recruit_text___1:moved_to_studyarm))
result_set_2 <- subset(add_com_raw, select = c(record_id:redcap_repeat_instance, recruit_consent_complete___1:recruitment_result_complete))

recruit_text_set <- recruit_text_set %>%
  filter(!is.na(r_text1_date) | !is.na(r_email_date) | !is.na(r_call_date))

result_set <- subset(result_set_1)
for (i in 1:nrow(result_set_1)) {
  for (j in 1:nrow(result_set_2)) {
    if (!(result_set_2[j,"record_id"] %in% unique(result_set$record_id))) {
      result_set[nrow(result_set)+1, ] <- result_set_2[j, ]
    }
  }
}

welcome_row <- nrow(welcome_set) #359
recruit_call_row <- nrow(recruit_call_set) #570
result_row <- nrow(result_set) #361
recruit_text_row <- nrow(recruit_text_set) #60

summary(welcome_set) # Max repeat = 3
summary(recruit_call_set) # Max repeat = 7
summary(recruit_text_set)
summary(result_set)

unique_id_welcome_call <- unique(welcome_call_raw$record_id) #357
unique_id_welcome <- unique(welcome_set$record_id) #331
unique_id_call <- unique(recruit_call_set$record_id) #281
unique_id_text <- unique(recruit_text_set$record_id) #60
unique_id_result <- unique(result_set$record_id) #361

col_names <- c(colnames(welcome_set),c(colnames(recruit_call_set)[-(1:4)]))
call_recruit_report <- data.frame(matrix(nrow = 0, ncol=length(col_names)))
colnames(call_recruit_report)<- col_names

for (i in 1:recruit_call_row) {
  for (j in 1: welcome_row){
    if (welcome_set[j,"record_id"] == recruit_call_set[i,"record_id"]){
      if (welcome_set[j,"redcap_repeat_instance"] == recruit_call_set[i,"redcap_repeat_instance"]){
        call_recruit_report[nrow(call_recruit_report)+1, ] <- c(welcome_set[j,],recruit_call_set[i,5:111])
      } else if (welcome_set[j,"redcap_repeat_instance"] > call_recruit_report[nrow(call_recruit_report),"redcap_repeat_instance"] & recruit_call_set[i+1,"redcap_repeat_instance"] != welcome_set[j,"redcap_repeat_instance"]) {
        call_recruit_report[nrow(call_recruit_report)+1, ] <- c(welcome_set[j,],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      }
    }
  }
  if (recruit_call_set[i,"redcap_repeat_instance"] > call_recruit_report[nrow(call_recruit_report),"redcap_repeat_instance"] & recruit_call_set[i,"record_id"] %in% call_recruit_report$record_id){
    call_recruit_report[nrow(call_recruit_report)+1, ] <- c(recruit_call_set[i,1:4],NA,NA,NA,NA,NA,NA,NA,recruit_call_set[i,5:111])
  } 
}

unique_id_recruit <- unique(call_recruit_report$record_id) #272

call_recruit_report$welcome_date <- as.Date(call_recruit_report$welcome_date)
call_recruit_report$datecall <- as.Date(call_recruit_report$datecall)

call_recruit_report <- call_recruit_report %>%
  mutate(delay_days = NA,
         .after = "redcap_repeat_instance")

for (i in 1:nrow(call_recruit_report)){
  if ( !is.na(call_recruit_report[i,"welcome_date"]) & !is.na(call_recruit_report[i,"datecall"]) ) {
    delay <- abs(difftime(call_recruit_report[i,"welcome_date"], call_recruit_report[i,"datecall"], units = "days"))
    if (i < nrow(call_recruit_report)) {
      if (call_recruit_report[i,"record_id"] == call_recruit_report[i+1,"record_id"]){
        welcomedate <- call_recruit_report[i,"welcome_date"]
        date_call <- call_recruit_report[i,"datecall"]
      }
      else{
        welcomedate <- NA
        date_call <- NA
      }
    }
  }
  else if (is.na(call_recruit_report[i,"welcome_date"]) | is.na(call_recruit_report[i,"datecall"]) ){
    if (is.na(call_recruit_report[i,"welcome_date"])){
      delay <- abs(difftime(welcomedate, call_recruit_report[i,"datecall"], units = "days"))
    }
    else if(is.na(call_recruit_report[i,"datecall"])){
      delay <- abs(difftime(call_recruit_report[i,"welcome_date"], call_recruit_report[i,"datecall"], units = "days"))
    }
  }
  call_recruit_report[i,"delay_days"] <- delay
} 

call_recruit_report <- call_recruit_report %>%
  mutate(moved_to_studyarm = NA,
         .after = "recruitment_call_complete")

call_recruit_report <- call_recruit_report %>%
  mutate(reason_notparticipating = NA,
         .after = "moved_to_studyarm")

call_recruit_report <- call_recruit_report %>%
  mutate(why_not = NA,
         .after = "reason_notparticipating")

for (i in 1:nrow(call_recruit_report)) {
  for (j in 1:result_row) {
    if (call_recruit_report[i,"record_id"] == result_set[j,"record_id"]) {
      call_recruit_report[i,"moved_to_studyarm"] <- result_set[j,"moved_to_studyarm"]
      call_recruit_report[i,"reason_notparticipating"] <- result_set[j,"reason_notparticipating"]
      call_recruit_report[i,"why_not"] <- result_set[j,"why_not"]
    }
  }
}

add_com_colnames <- c(colnames(welcome_set),c(colnames(recruit_text_set)[-(1:4)]))
add_com_recruit_report <- data.frame(matrix(nrow = 0, ncol=length(add_com_colnames)))
colnames(add_com_recruit_report)<- add_com_colnames

for (i in 1:nrow(recruit_text_set)){
  for (j in 1:welcome_row) {
    if (welcome_set[j,"record_id"] == recruit_text_set[i,"record_id"]){
      if (welcome_set[j,"redcap_repeat_instance"] == 1) {
        add_com_recruit_report[nrow(add_com_recruit_report)+1, ] <- c(welcome_set[j,],recruit_text_set[i,5:50])
      } else if (welcome_set[j,"redcap_repeat_instance"] > add_com_recruit_report[(nrow(add_com_recruit_report) - 1),"redcap_repeat_instance"]) {
        add_com_recruit_report[nrow(add_com_recruit_report)+1, ] <- c(welcome_set[j,],recruit_text_set[i,5:50])
      }
    }
  }
}

add_com_recruit_report$r_text1_date <- as.Date(add_com_recruit_report$r_text1_date)
add_com_recruit_report$r_text2_date <- as.Date(add_com_recruit_report$r_text2_date)
add_com_recruit_report$r_email_date <- as.Date(add_com_recruit_report$r_email_date)
add_com_recruit_report$r_call_date <- as.Date(add_com_recruit_report$r_call_date)
add_com_recruit_report$welcome_date <- as.Date(add_com_recruit_report$welcome_date)

unique_id_add_com <- unique(add_com_recruit_report$record_id)

both_com <- unique(unique_id_add_com,unique_id_recruit)

add_com_recruit_report <- add_com_recruit_report %>%
  mutate(text1_delay = difftime(add_com_recruit_report$r_text1_date,add_com_recruit_report$welcome_date, units = "days"),
         .before = "r_text1_date")
add_com_recruit_report <- add_com_recruit_report %>%
  mutate(text2_delay = difftime(add_com_recruit_report$r_text2_date,add_com_recruit_report$welcome_date, units = "days"),
         .before = "r_text2_date")
add_com_recruit_report <- add_com_recruit_report %>%
  mutate(delay_from_text1 = abs(difftime(add_com_recruit_report$r_text1_date,add_com_recruit_report$r_text2_date, units = "days")),
         .before = "text2_delay")
add_com_recruit_report <- add_com_recruit_report %>%
  mutate(email_delay = difftime(add_com_recruit_report$r_email_date, add_com_recruit_report$welcome_date, units = "days"),
         .before = "r_email_date")
add_com_recruit_report <- add_com_recruit_report %>%
  mutate(call_delay = difftime(add_com_recruit_report$r_call_date, add_com_recruit_report$welcome_date, units = "days"),
         .before = "r_call_date")

add_com_recruit_report <- add_com_recruit_report %>%
  mutate(r_text_reply = ifelse(add_com_recruit_report$r_text_reply___1 == 1|add_com_recruit_report$r_text_reply___2 == 1|add_com_recruit_report$r_text_reply___3 == 1, 1, 0),
         .before = "r_text_reply___1")

col_result <- colnames(result_set)
no_welcome_recruit <- data.frame(matrix(nrow = 0, ncol=length(col_result)))
colnames(no_welcome_recruit)<- col_result
count=0
for (i in 1: length(unique_id_result)) { 
  if (!(result_set[i,"record_id"] %in% unique_id_welcome) && !(result_set[i,"record_id"] %in% unique_id_recruit)){
    count = count + 1
    no_welcome_recruit[nrow(no_welcome_recruit)+1, ] <- c(result_set[i,])
    count = count +1
  }
}

no_welcome <- data.frame(matrix(nrow = 0, ncol=length(col_result)))
colnames(no_welcome)<- col_result
count=0
for (i in 1: length(unique_id_result)) {
  if (!(result_set[i,"record_id"] %in% unique_id_welcome)){
    count = count + 1
    no_welcome[nrow(no_welcome)+1, ] <- c(result_set[i,])
  }
}

no_call_w_welcome <- data.frame(matrix(nrow = 0, ncol=length(col_result)))
colnames(no_call_w_welcome)<- col_result
count_1 = 0
for (i in 1: length(unique_id_welcome)) {
  if (!(welcome_set[i,"record_id"] %in% unique_id_recruit)){
    count_1 = count_1 + 1
    no_call_w_welcome[nrow(no_call_w_welcome)+1, ] <- c(result_set[i,])
  }
}

delay_colname <- c("record_id","redcap_repeat_instance","how_sent","number_welcome_packet","welcome_date","receive_mail","receive_email","datecall","timecall","delay_days","delay_from_pre_call","callresult","interested","partcipateyn","text1_date","text1_time","text1_delay","text2_date","text2_time","text2_delay","delay_from_text1","text_participateyn","email_date","email_time","email_delay","r_call_date","r_call_time","r_call_delay","participate_studyarm","reason_not_participate","why_not")

delay_report <- subset(call_recruit_report, select = c('record_id','redcap_repeat_instance','how_sent','number_welcome_packet','welcome_date','receive_mail','receive_email','datecall','timecall','delay_days','callresult','interested','participateyn','moved_to_studyarm','reason_notparticipating','why_not'))

delay_report <- delay_report %>%
  mutate(delay_from_previous = delay_days,
         .after = "delay_days")
for (i in 1:nrow(delay_report)) {
  if (delay_report[i,"redcap_repeat_instance"] > 1) {
    delay_report[i,"delay_from_previous"] <- abs(difftime(delay_report[i,'datecall'],delay_report[(i-1),'datecall'],units = "days"))
  }
}

write.csv(welcome_set,"C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250226_SERVES-WelcomeReport.csv")
write.csv(result_set,"C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250226_SERVES-ResultReport.csv")
write.csv(recruit_call_set,"C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250226_SERVES-Recruit_Call_Report.csv")
write.csv(recruit_text_set,"C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250326_SERVES-Recruit_Text_Report.csv")

write.csv(call_recruit_report,"C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250304_SERVES-Recruit_Report.csv")

write.csv(delay_report,"C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250318_SERVES-Delay_Report.csv")

########

cor(add_com_raw[,c("r_text_reply___1","r_text_reply___2","r_text_reply___3","moved_to_studyarm")], use = 'complete.obs')

cor(delay_report[,c("delay_days","callresult","participateyn","moved_to_studyarm")], use = "pairwise.complete.obs")

delay_table <- table(delay_report$callresult,delay_report$participateyn, delay_report$moved_to_studyarm)

chisq.test(data.frame(delay_table))

model <- lm(delay_days~callresult + participateyn + moved_to_studyarm, data = delay_report)
summary(model)

aov_model <- aov(delay_days~as.factor(moved_to_studyarm), data = delay_report)
summary(aov_model)

delay_prop <- prop.table(delay_table)
delay_per <- round((delay_prop*100),2)

delay_per <- data.frame(delay_per) 

delay_per$Var1 <- ifelse(delay_per$Var1 == 1, "No answer, unable to leave voicemail",
                         ifelse(delay_per$Var1 == 2, "No answer, did not leave voicemail",
                                ifelse(delay_per$Var1 == 3, "No answer, left a voicemail",
                                       ifelse(delay_per$Var1 == 4, "Answered, participant not available",
                                              ifelse(delay_per$Var1 ==5, "Answered, but need to call them back at a better time",
                                                     ifelse(delay_per$Var1 ==6, "Answered, speaking to participant", "Participant called back"))))))
delay_per$Var2 <- ifelse(delay_per$Var2 == 1, "Yes","No")
delay_per$Var3 <- ifelse(delay_per$Var3 == 1, "Yes","No")

delay_per$Freq <- as.numeric(delay_per$Freq)

colnames(delay_per)[colnames(delay_per) == 'Var1'] <- 'callresult'
colnames(delay_per)[colnames(delay_per) == 'Var2'] <- 'participation_response_during_call'
colnames(delay_per)[colnames(delay_per) == 'Var3'] <- 'moved_to_studyarm'
colnames(delay_per)[colnames(delay_per) == 'Freq'] <- 'Per'

summary(lm(delay_days ~ callresult, data=call_recruit_report))
summary(lm(callresult ~ moved_to_studyarm, data=call_recruit_report))
summary(lm(delay_days ~ moved_to_studyarm, data=call_recruit_report))

summary(lm(text1_delay~r_text_reply___1, data=add_com_recruit_report))
summary(lm(text2_delay~r_text_reply___2, data=add_com_recruit_report))

summary(lm(text1_delay~moved_to_studyarm, data=add_com_recruit_report))
summary(lm(text2_delay~moved_to_studyarm, data=add_com_recruit_report))

summary(lm(r_text_reply~moved_to_studyarm, data=add_com_recruit_report))

summary(lm(email_delay~moved_to_studyarm, data=add_com_recruit_report))

call_recruit_summary <- table(call_recruit_report$callresult,call_recruit_report$delay_days, call_recruit_report$moved_to_studyarm, useNA = "always")

delay_summary <- data.frame(table(delay_report$delay_days, delay_report$moved_to_studyarm, useNA = "always"))
colnames(delay_summary)[colnames(delay_summary) == 'Var1'] <- 'delay_days'
colnames(delay_summary)[colnames(delay_summary) == 'Var2'] <- 'moved_to_studyarm'

first_contact <- delay_report %>% filter(redcap_repeat_instance == 1)
delay_first_summary <- data.frame(table(first_contact$delay_days, first_contact$moved_to_studyarm, useNA = "always"))
colnames(delay_first_summary)[colnames(delay_first_summary) == 'Var1'] <- 'delay_days'
colnames(delay_first_summary)[colnames(delay_first_summary) == 'Var2'] <- 'moved_to_studyarm'

follow_contact <- delay_report %>% filter(redcap_repeat_instance > 1)
delay_follow_summary <- data.frame(table(follow_contact$delay_days, follow_contact$moved_to_studyarm, useNA = "always"))
colnames(delay_follow_summary)[colnames(delay_follow_summary) == 'Var1'] <- 'delay_days'
colnames(delay_follow_summary)[colnames(delay_follow_summary) == 'Var2'] <- 'moved_to_studyarm'

delay_report <- delay_report %>%
  mutate(avg_delay_days = round(delay_days/redcap_repeat_instance,0),
         .after = "delay_from_previous")

avg_delay_colnames <- c(c(colnames(delay_report)[-(3:11)]))
avg_delay <- data.frame(matrix(nrow = length(unique_id_recruit), ncol=length(avg_delay_colnames)))
colnames(avg_delay) <- avg_delay_colnames
avg_delay <- avg_delay %>%
  mutate(timecall=NA,
         .after = redcap_repeat_instance)

avg_delay$record_id <- c(unique_id_recruit)

for (i in 1:length(avg_delay$record_id)) {
  for (j in 1:length(delay_report$record_id)){
    max_repeat = 0;
    if (avg_delay[i,"record_id"] == delay_report[j,"record_id"]) {
      if (delay_report[j,"redcap_repeat_instance"] > max_repeat) {
        max_repeat = delay_report[j,"redcap_repeat_instance"]
        avg_delay[i,] <- c(delay_report[j,1:2],delay_report[j, 9], delay_report[j, 12:18])
      }
    }
  }
}

avg_delay$temp_moved_to_studyarm <- ifelse(avg_delay$moved_to_studyarm == 1, 1,
                                           ifelse(is.na(avg_delay$moved_to_studyarm), 0, 0))

avg_delay_summary <- data.frame(table(avg_delay$avg_delay_days, avg_delay$moved_to_studyarm, useNA = "always"))
colnames(avg_delay_summary)[colnames(avg_delay_summary) == 'Var1'] <- 'delay_days'
colnames(avg_delay_summary)[colnames(avg_delay_summary) == 'Var2'] <- 'moved_to_studyarm'

temp_avg_delay_summary <- data.frame(table(avg_delay$avg_delay_days, avg_delay$temp_moved_to_studyarm))
colnames(temp_avg_delay_summary)[colnames(temp_avg_delay_summary) == 'Var1'] <- 'delay_days'
colnames(temp_avg_delay_summary)[colnames(temp_avg_delay_summary) == 'Var2'] <- 'moved_to_studyarm'

add_com_summary <- subset(add_com_recruit_report, select = c(record_id, redcap_repeat_instrument, redcap_repeat_instance, how_sent, welcome_date, explanation, text1_delay, r_text1_date, r_text1_time, delay_from_text1, text2_delay, r_text2_date, r_text2_time, r_text_reply, r_contactus___1, r_contactus___2, r_contactus___3, email_delay, r_email_date, r_email_time, call_delay, r_call_date, r_call_time, recruit_consent_complete___1, recruit_consent_complete___2, moved_to_studyarm))

add_com_summary <- add_com_summary %>%
  mutate(datecall = NA,
         .after = "explanation")

add_com_summary <- add_com_summary %>%
  mutate(timecall = NA,
         .after = "datecall")

add_com_summary <- add_com_summary %>%
  mutate(delay_days = NA,
         .after = "timecall")

add_com_summary <- add_com_summary %>%
  mutate(delay_from_previous = NA,
         .after = "delay_days")

add_com_summary <- add_com_summary %>%
  mutate(avg_delay_days= NA,
         .after = "delay_from_previous")

add_com_summary <- add_com_summary %>%
  mutate(callresult= NA,
         .after = "avg_delay_days")

add_com_summary <- add_com_summary %>%
  mutate(interested= NA,
         .after = "callresult")

add_com_summary <- add_com_summary %>%
  mutate(participateyn = NA,
         .after = "interested")
for (i in 1:nrow(delay_report)) {
  for (j in 1:nrow(add_com_summary)) {
    if (delay_report[i, "record_id"] == add_com_summary[j, "record_id"]) {
      if (delay_report[i, "redcap_repeat_instance"] == add_com_summary[j, "redcap_repeat_instance"]) {
        add_com_summary[j, "datecall"] <- delay_report[i, "datecall"]
        add_com_summary[j, "timecall"] <- delay_report[i, "timecall"]
        add_com_summary[j, "delay_days"] <- delay_report[i, "delay_days"]
        add_com_summary[j, "delay_from_previous"] <- delay_report[i, "delay_from_previous"]
        add_com_summary[j, "avg_delay_days"] <- delay_report[i, "avg_delay_days"]
        add_com_summary[j, "callresult"] <- delay_report[i, "callresult"]
        add_com_summary[j, "interested"] <- delay_report[i, "interested"]
        add_com_summary[j, "participateyn"] <- delay_report[i, "participateyn"]
      }
    }
  }
}

add_com_summary$datecall <- as.Date(add_com_summary$datecall)

call_recruit_without_other_com <- subset(avg_delay, !(record_id %in% unique(add_com_summary$record_id)))

move_on_w_com <- sum(add_com_summary$moved_to_studyarm == 1, na.rm=TRUE) / nrow(add_com_summary) *100

move_on_without_com <- sum(call_recruit_without_other_com$moved_to_studyarm == 1, na.rm=TRUE) / nrow(call_recruit_without_other_com) *100

########
library(ggplot2)
library(corrplot)

# jpeg(file = "C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250426_DelayReportGraphst.jpg")
pdf("C:/Users/Admin/Box/Chi_Long Survey/Chi_delay report/250426_DelayReportGraphst.pdf")
#par(mar=c(1,1,1,1))

ggplot(delay_per, aes(x = moved_to_studyarm, y = Per, group = callresult)) +
  geom_line(aes(color = callresult), size = 1.2) +
  geom_point(aes(color = callresult), size = 3) +
  labs(
    title = "Call Results Across Study Arms",
    x = "Study Arm", y = "Frequency (%)"
  ) + theme_minimal()

corrplot(cor(delay_report[,c("delay_days","callresult","participateyn","moved_to_studyarm")], use = "pairwise.complete.obs"),method = "circle", order = "hclust",hclust.method = "ward.D", addrect = 2,rect.col = 3,rect.lwd = 3)

corrplot(cor(avg_delay[,c("avg_delay_days","callresult","participateyn","moved_to_studyarm")], use = "pairwise.complete.obs"),method = "circle", order = "hclust",hclust.method = "ward.D", addrect = 2,rect.col = 3,rect.lwd = 3)

ggplot(delay_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_line(aes(color = moved_to_studyarm), size = 1.2) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  labs(
    title = "How much participants join vs refuse each delay days",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(delay_first_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_line(aes(color = moved_to_studyarm), size = 1.2) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  labs(
    title = "How much participants join vs refuse each delay days for 1st reach out",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(delay_follow_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_line(aes(color = moved_to_studyarm), size = 1.2) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  labs(
    title = "How much participants join vs refuse each delay days for other reach out",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(avg_delay_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_line(aes(color = moved_to_studyarm), size = 1.2) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  labs(
    title = "How much participants join vs refuse each average delay days",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(avg_delay_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "How much participants join vs refuse each average delay days",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(temp_avg_delay_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_line(aes(color = moved_to_studyarm), size = 1.2) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  labs(
    title = "How much participants join vs refuse each average delay days (NA and 0 combine)",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(temp_avg_delay_summary, aes(x = delay_days, y = Freq, group = moved_to_studyarm)) +
  geom_point(aes(color = moved_to_studyarm), size = 3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "How much participants join vs refuse each average delay days(NA and 0 combine)",
    x = "days", y = "participants"
  ) + theme_minimal()

ggplot(call_recruit_report, aes(x = delay_days, y = callresult)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of delay days on call result",
       x = "delay times (days)",
       y = "Call result(no answer -> call back)")

ggplot(call_recruit_report, aes(x = delay_days, y = moved_to_studyarm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of delay days on joining study arm",
       x = "delay times (days)",
       y = "moved to studyarm (1: Yes, 0: No)")

ggplot(call_recruit_report, aes(x = callresult, y = moved_to_studyarm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of call result on joining study arm",
       x = "Call result(no answer -> call back)",
       y = "moved to studyarm (1: Yes, 0: No)")

ggplot(add_com_recruit_report, aes(x = text1_delay, y = moved_to_studyarm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of delay days for 1st text on joining study arm",
       x = "delay times (days)",
       y = "moved to studyarm (1: Yes, 0: No)")

ggplot(add_com_recruit_report, aes(x = text2_delay, y = moved_to_studyarm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of delay days for 2nd text on joining study arm",
       x = "delay times (days)",
       y = "moved to studyarm (1: Yes, 0: No)")

ggplot(add_com_recruit_report, aes(x = text2_delay, y = moved_to_studyarm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of delay days for 2nd text on joining study arm",
       x = "delay times (days)",
       y = "moved to studyarm (1: Yes, 0: No)")

ggplot(add_com_recruit_report, aes(x = r_text_reply, y = moved_to_studyarm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of text reply on joining study arm",
       x = "text reply (1:Yes, 3: No)",
       y = "moved to studyarm (1: Yes, 0: No)")

boxplot(delay_days~callresult, data = delay_report)
boxplot(delay_from_previous~callresult, data = delay_report)

first_contact <- delay_report %>% filter(redcap_repeat_instance == 1)
boxplot(delay_days~callresult, data = first_contact)
follow_contact <- delay_report %>% filter(redcap_repeat_instance > 1)
boxplot(delay_days~callresult, data = follow_contact)

dev.off()