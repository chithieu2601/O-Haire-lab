install.packages("ggplot2")
install.packages("lattice")
library(readr)
library(dplyr)
library(tidyr)
library(xlsx)
library(caret)
library(tidyverse)
library(ggplot2)
library(data.table)

# Note: The file directory can change depends on where file is stored
data <- read.csv("C:/Users/Acer/Box/Chi_VR12/221004_Qualtrics_wide_warrior_DEIDENTIFIED_VR12.csv")

#### EDA
summary(data) # View data with important information (mean, sd, distribution)

glimpse(data) # View data structure

View(data) # View data table

data %>%  # Group participants 
  group_by(servicedog_or_waitlist) %>%
  summarize(percent = 100 * n() / nrow(data))

data %>% count(servicedog_or_waitlist) # Count participants

# Preprocess the input from raw data
# For VR_1: reverse the input and keep NA value
data$VR_1_base <- ifelse(data$VR_1_base == 1, 5,
                    ifelse(data$VR_1_base == 2, 4,
                      ifelse(data$VR_1_base == 3, 3,
                        ifelse(data$VR_1_base == 4, 2,
                          ifelse(data$VR_1_base ==5, 1, NA)))))

data$VR_1_follow <- ifelse(data$VR_1_follow == 1, 5,
                         ifelse(data$VR_1_follow == 2, 4,
                                ifelse(data$VR_1_follow == 3, 3,
                                     ifelse(data$VR_1_follow == 4, 2, 
                                          ifelse(data$VR_1_follow ==5, 1, NA)))))

# Reverse VR_2 input and keep NA value
data$VR_2_1_base <- ifelse(data$VR_2_1_base == 1, 3,
                           ifelse(data$VR_2_1_base == 2, 2,
                                  ifelse(data$VR_2_1_base == 3, 1, data$VR_2_1_base)))

data$VR_2_1_follow <- ifelse(data$VR_2_1_follow == 1, 3,
                             ifelse(data$VR_2_1_follow == 2, 2,
                                    ifelse(data$VR_2_1_follow == 3, 1, NA)))

data$VR_2_2_base <- ifelse(data$VR_2_2_base == 1, 3,
                           ifelse(data$VR_2_2_base == 2, 2,
                                  ifelse(data$VR_2_2_base == 3, 1, NA)))

data$VR_2_2_follow <- ifelse(data$VR_2_2_follow == 1, 3,
                             ifelse(data$VR_2_2_follow == 2, 2,
                                     ifelse(data$VR_2_2_follow == 3, 1, NA)))

# Decreasing by 1 for VR_6 input
data$VR_6_1_base <-data$VR_6_1_base-1
data$VR_6_1_follow <- data$VR_6_1_follow -1

data$VR_6_2_base <-data$VR_6_2_base-1
data$VR_6_2_follow <- data$VR_6_2_follow -1

data$VR_6_3_base <-data$VR_6_3_base - 1
data$VR_6_3_follow <- data$VR_6_3_follow -1

# Reverse VR_6 input and keep all NA value
data$VR_6_1_base <- ifelse(data$VR_6_1_base == 1, 6,
                           ifelse(data$VR_6_1_base == 2, 5,
                                  ifelse(data$VR_6_1_base == 3, 4,
                                         ifelse(data$VR_6_1_base == 4, 3,
                                                ifelse(data$VR_6_1_base == 5, 2, 
                                                       ifelse(data$VR_6_1_base == 6, 1, NA))))))

data$VR_6_1_follow <- ifelse(data$VR_6_1_follow == 1, 6,
                           ifelse(data$VR_6_1_follow == 2, 5,
                                  ifelse(data$VR_6_1_follow == 3, 4,
                                         ifelse(data$VR_6_1_follow == 4, 3,
                                                ifelse(data$VR_6_1_follow == 5, 2, 
                                                       ifelse(data$VR_6_1_follow == 6, 1, NA))))))

data$VR_6_2_base <- ifelse(data$VR_6_2_base == 1, 6,
                           ifelse(data$VR_6_2_base == 2, 5,
                                  ifelse(data$VR_6_2_base == 3, 4,
                                         ifelse(data$VR_6_2_base == 4, 3,
                                                ifelse(data$VR_6_2_base == 5, 2, 
                                                       ifelse(data$VR_6_2_base == 6, 1, NA))))))

data$VR_6_2_follow <- ifelse(data$VR_6_2_follow == 1, 6,
                             ifelse(data$VR_6_2_follow == 2, 5,
                                    ifelse(data$VR_6_2_follow == 3, 4,
                                           ifelse(data$VR_6_2_follow == 4, 3,
                                                  ifelse(data$VR_6_2_follow == 5, 2, 
                                                         ifelse(data$VR_6_2_follow == 6, 1, NA))))))

data$VR_6_3_base <- ifelse(data$VR_6_3_base == 1, 6,
                           ifelse(data$VR_6_3_base == 2, 5,
                                  ifelse(data$VR_6_3_base == 3, 4,
                                         ifelse(data$VR_6_3_base == 4, 3,
                                                ifelse(data$VR_6_3_base == 5, 2, 
                                                       ifelse(data$VR_6_3_base == 6, 1, NA))))))

data$VR_6_3_follow <- ifelse(data$VR_6_3_follow == 1, 6,
                             ifelse(data$VR_6_3_follow == 2, 5,
                                    ifelse(data$VR_6_3_follow == 3, 4,
                                           ifelse(data$VR_6_3_follow == 4, 3,
                                                  ifelse(data$VR_6_3_follow == 5, 2, 
                                                         ifelse(data$VR_6_3_follow == 6, 1, NA))))))
# Reverse VR_7 input and keep NA value
data$VR_7_base <- ifelse(data$VR_7_base == 1, 5,
                         ifelse(data$VR_7_base == 2, 4,
                                ifelse(data$VR_7_base == 3, 3,
                                       ifelse(data$VR_7_base == 4, 2,
                                              ifelse(data$VR_7_base ==5, 1, NA)))))

data$VR_7_follow <- ifelse(data$VR_7_follow == 1, 5,
                           ifelse(data$VR_7_follow == 2, 4,
                                  ifelse(data$VR_7_follow == 3, 3,
                                          ifelse(data$VR_7_follow == 4, 2, 
                                                 ifelse(data$VR_7_follow ==5, 1, NA)))))


#### Calculating each criteria 
# Transform the data of base phase
data$gh1x_base <- ifelse(data$VR_1_base == 1, 100,
                         ifelse(data$VR_1_base == 2, 85,
                                ifelse(data$VR_1_base == 3, 60,
                                       ifelse(data$VR_1_base == 4, 35, 
                                              ifelse(data$VR_1_base ==5, 0, NA)))))

data$pf02x_base <- (data$VR_2_1_base - 1) * 50
data$pf04x_base <- (data$VR_2_2_base - 1) * 50
data$rp2x_base <- (5 - data$VR_3_1_base) * 25
data$rp3x_base <- (5 - data$VR_3_2_base) * 25
data$re2x_base <- (5 - data$VR_4_1_base) * 25
data$re3x_base <- (5 - data$VR_4_2_base) * 25
data$bp2x_base <- (5 - data$VR_5_base) * 25
data$mh3x_base <- (6 - data$VR_6_1_base) * 20
data$vt2x_base <- (6 - data$VR_6_2_base) * 20
data$mh4x_base <- (data$VR_6_3_base - 1) * 20
data$sf2x_base <- (data$VR_7_base - 1) * 25

# Transform the data of follow phase
data$gh1x_follow <- ifelse(data$VR_1_follow == 1, 100,
                           ifelse(data$VR_1_follow == 2, 85,
                                  ifelse(data$VR_1_follow == 3, 60,
                                         ifelse(data$VR_1_follow == 4, 35, 
                                                ifelse(data$VR_1_follow ==5, 0, NA)))))

data$pf02x_follow <- (data$VR_2_1_follow - 1) * 50
data$pf04x_follow <- (data$VR_2_2_follow - 1) * 50
data$rp2x_follow <- (5 - data$VR_3_1_follow) * 25
data$rp3x_follow <- (5 - data$VR_3_2_follow) * 25
data$re2x_follow <- (5 - data$VR_4_1_follow) * 25
data$re3x_follow <- (5 - data$VR_4_2_follow) * 25
data$bp2x_follow <- (5 - data$VR_5_follow) * 25
data$mh3x_follow <- (6 - data$VR_6_1_follow) * 20
data$vt2x_follow <- (6 - data$VR_6_2_follow) * 20
data$mh4x_follow <- (data$VR_6_3_follow - 1) * 20
data$sf2x_follow <- (data$VR_7_follow - 1) * 25

#### Calculate key output: PCS and MCS
# Calculate PCS and MCS value for base phase
data$PCS_base <- round(0.0782523751 * data$gh1x_base + 0.065064013 * data$pf02x_base + 0.0748361349 * data$pf04x_base +
                         0.0716978312 * data$rp2x_base + 0.0741541386 * data$rp3x_base + (-0.057598263) * data$re2x_base +
                         (-0.032268941) * data$re3x_base + 0.1339749098 * data$bp2x_base + (-0.042411864) * data$mh3x_base +
                         0.0299689621 * data$vt2x_base + (-0.053362399) * data$mh4x_base + 0.0046096779 * data$sf2x_base + 21.046859741,5)

data$MCS_base <- round(-0.000915925 * data$gh1x_base + -0.035498649 * data$pf02x_base + -0.03157714 * data$pf04x_base +
                         -0.0251735 * data$rp2x_base + -0.024652228 * data$rp3x_base + 0.1266860962 * data$re2x_base +
                         0.0808723569 * data$re3x_base + -0.024371371 * data$bp2x_base + 0.1094084978 * data$mh3x_base +
                         0.0694271326 * data$vt2x_base + 0.1493788958 * data$mh4x_base + 0.1085734367 * data$sf2x_base + 12.66204834,5)

# Calculate PCS and MCS value for follow-up phase
data$PCS_follow <- round(0.0782523751 * data$gh1x_follow + 0.065064013 * data$pf02x_follow + 0.0748361349 * data$pf04x_follow +
                           0.0716978312 * data$rp2x_follow + 0.0741541386 * data$rp3x_follow + (-0.057598263) * data$re2x_follow +
                           (-0.032268941) * data$re3x_follow + 0.1339749098 * data$bp2x_follow + (-0.042411864) * data$mh3x_follow +
                           0.0299689621 * data$vt2x_follow + (-0.053362399) * data$mh4x_follow + 0.0046096779 * data$sf2x_follow + 21.046859741,5)

data$MCS_follow <- round(-0.000915925 * data$gh1x_follow + -0.035498649 * data$pf02x_follow + -0.03157714 * data$pf04x_follow +
                           -0.0251735 * data$rp2x_follow + -0.024652228 * data$rp3x_follow + 0.1266860962 * data$re2x_follow +
                           0.0808723569 * data$re3x_follow + -0.024371371 * data$bp2x_follow + 0.1094084978 * data$mh3x_follow +
                           0.0694271326 * data$vt2x_follow + 0.1493788958 * data$mh4x_follow + 0.1085734367 * data$sf2x_follow + 12.66204834,5)

# Round function for future used            
round_df <- function(df,digits){
  numeric_columns <- sapply(df,mode) == 'numeric'
  df[numeric_columns] <- round(df[numeric_columns],digits)
  df
}

#### Storing criteria
# Coping criteria base phase to new database
scoring <- subset(data, select = c(NEWSTUDYID:servicedog_or_waitlist))
scoring <- cbind(scoring,subset(data,select= c(gh1x_base:MCS_base)))

# Eliminate transform columns in data 
data <- subset(data, select = -c(gh1x_base:sf2x_base))

# Coping criteria follow phase to new database
scoring <- cbind(scoring,subset(data, select = c(gh1x_follow:MCS_follow)))

# Eliminate transform columns
data<-subset(data, select = -c(gh1x_follow:sf2x_follow))

#### Calculate mean value and standard deviation value
# Calculate mean value for base phase
gh1x_base_mean <- round(mean(scoring$gh1x_base,na.rm = TRUE),digits=2)
pf02x_base_mean <- round(mean(scoring$pf02x_base,na.rm = TRUE),digits=2)
pf04x_base_mean <- round(mean(scoring$pf04x_base,na.rm = TRUE),digits=2)
rp2x_base_mean <- round(mean(scoring$rp2x_base,na.rm = TRUE),digits=2)
rp3x_base_mean <- round(mean(scoring$rp3x_base,na.rm = TRUE),digits=2)
re2x_base_mean <- round(mean(scoring$re2x_base,na.rm = TRUE),digits=2)
re3x_base_mean <- round(mean(scoring$re3x_base,na.rm = TRUE),digits=2)
bp2x_base_mean <- round(mean(scoring$bp2x_base,na.rm = TRUE),digits=2)
mh3x_base_mean <- round(mean(scoring$mh3x_base,na.rm = TRUE),digits=2)
vt2x_base_mean <- round(mean(scoring$vt2x_base,na.rm = TRUE),digits=2)
mh4x_base_mean <- round(mean(scoring$mh4x_base,na.rm = TRUE),digits=2)
sf2x_base_mean <- round(mean(scoring$sf2x_base,na.rm = TRUE),digits=2)

# Calculate mean value for follow phase
gh1x_follow_mean <- round(mean(scoring$gh1x_follow,na.rm = TRUE),digits=2)
pf02x_follow_mean <- round(mean(scoring$pf02x_follow,na.rm = TRUE),digits=2)
pf04x_follow_mean <- round(mean(scoring$pf04x_follow,na.rm = TRUE),digits=2)
rp2x_follow_mean <- round(mean(scoring$rp2x_follow,na.rm = TRUE),digits=2)
rp3x_follow_mean <- round(mean(scoring$rp3x_follow,na.rm = TRUE),digits=2)
re2x_follow_mean <- round(mean(scoring$re2x_follow,na.rm = TRUE),digits=2)
re3x_follow_mean <- round(mean(scoring$re3x_follow,na.rm = TRUE),digits=2)
bp2x_follow_mean <- round(mean(scoring$bp2x_follow,na.rm = TRUE),digits=2)
mh3x_follow_mean <- round(mean(scoring$mh3x_follow,na.rm = TRUE),digits=2)
vt2x_follow_mean <- round(mean(scoring$vt2x_follow,na.rm = TRUE),digits=2)
mh4x_follow_mean <- round(mean(scoring$mh4x_follow,na.rm = TRUE),digits=2)
sf2x_follow_mean <- round(mean(scoring$sf2x_follow,na.rm = TRUE),digits=2)

# Calculate mean value of each criteria group by participants
scoring_mean <- scoring %>%
  group_by(servicedog_or_waitlist) %>%
  summarize(mean_gh1x_base=mean(gh1x_base, na.rm = TRUE), mean_gh1x_follow=mean(gh1x_follow, na.rm = TRUE),
            mean_pf02x_base=mean(pf02x_base, na.rm = TRUE), mean_pf02x_follow=mean(pf02x_follow, na.rm = TRUE),
            mean_pf04x_base=mean(pf04x_base, na.rm = TRUE), mean_pf04x_follow=mean(pf04x_follow, na.rm = TRUE),
            mean_rp2x_base=mean(rp2x_base, na.rm = TRUE), mean_rp2x_follow=mean(rp2x_follow, na.rm = TRUE),
            mean_rp3x_base=mean(rp3x_base, na.rm = TRUE), mean_rp3x_follow=mean(rp3x_follow, na.rm = TRUE),
            mean_re2x_base=mean(re2x_base, na.rm = TRUE), mean_re2x_follow=mean(re2x_follow, na.rm = TRUE),
            mean_re3x_base=mean(re3x_base, na.rm = TRUE), mean_re3x_follow=mean(re3x_follow, na.rm = TRUE),
            mean_bp2x_base=mean(bp2x_base, na.rm = TRUE), mean_bp2x_follow=mean(bp2x_follow, na.rm = TRUE),
            mean_mh3x_base=mean(mh3x_base, na.rm = TRUE), mean_mh3x_follow=mean(mh3x_follow, na.rm = TRUE),
            mean_vt2x_base=mean(vt2x_base, na.rm = TRUE), mean_vt2x_follow=mean(vt2x_follow, na.rm = TRUE),
            mean_mh4x_base=mean(mh4x_base, na.rm = TRUE), mean_mh4x_follow=mean(mh4x_follow, na.rm = TRUE),
            mean_sf2x_base=mean(sf2x_base, na.rm = TRUE), mean_sf2x_follow=mean(sf2x_follow, na.rm = TRUE))

# Calculate mean value and stand deviation value of PCS and MCS
PCS_base_mean <- round(mean(data$PCS_base, na.rm = TRUE),2)
PCS_base_sd <- round(sd(data$PCS_base, na.rm = TRUE),2)

PCS_follow_mean <- round(mean(data$PCS_follow, na.rm = TRUE),2)
PCS_follow_sd <- round(sd(data$PCS_follow, na.rm = TRUE),2)

MCS_base_mean <- round(mean(data$MCS_base, na.rm = TRUE),2)
MCS_base_sd <- round(sd(data$MCS_base, na.rm = TRUE),2)

MCS_follow_mean <- round(mean(data$MCS_follow, na.rm = TRUE),2)
MCS_follow_sd <- round(sd(data$MCS_follow, na.rm = TRUE),2)

# Calculate mean value and standard deviation value group by participants
mean_std <- data %>%
  group_by(servicedog_or_waitlist) %>%
  summarize(mean_PCS_base=mean(PCS_base, na.rm = TRUE), sd_PCS_base=sd(PCS_base, na.rm = TRUE), 
            mean_PCS_follow=mean(PCS_follow, na.rm = TRUE), sd_PCS_follow=sd(PCS_follow, na.rm = TRUE),
            mean_MCS_base=mean(MCS_base, na.rm = TRUE), sd_MCS_base=sd(MCS_base, na.rm = TRUE), 
            mean_MCS_follow=mean(MCS_follow, na.rm = TRUE), sd_MCS_follow=sd(MCS_follow, na.rm = TRUE))

mean_std <- round_df(mean_std,2)

# Extracting database to csv files
write.csv(data,"C:/Users/Acer/Box/Chi_VR12/Output/OutputFile1_241021.csv", row.names=FALSE)

write.csv(scoring,"C:/Users/Acer/Box/Chi_VR12/Output/scoringFile_241021.csv", row.names=FALSE)

write.csv(mean_std,"C:/Users/Acer/Box/Chi_VR12/Output/mean_std_outputFile_ 241021.csv", row.names=FALSE )


#### Statistic tesing using t-test (two-side t-test and paired t-test)
# Determine the effects of participants groups with PCS and MCS calculation
t_test_PCS_base <- t.test(PCS_base ~ servicedog_or_waitlist, data = data)

print(t_test_PCS_base)

t_test_MCS_base <- t.test(MCS_base ~ servicedog_or_waitlist, data = data)

print(t_test_MCS_base)

t_test_PCS_follow <- t.test(PCS_follow ~ servicedog_or_waitlist, data = data)

print(t_test_PCS_follow)

t_test_MCS_follow <- t.test(MCS_follow ~ servicedog_or_waitlist, data = data)

print(t_test_MCS_follow)

# Determine relationship significance of differences in PCS and MCS scores
paired_t_test_PCS <- t.test(data$PCS_base, data$PCS_follow, paired = TRUE)

print(paired_t_test_PCS)

paired_t_test_MCS <- t.test(data$MCS_base, data$MCS_follow, paired = TRUE)

print(paired_t_test_PCS)

#### Preprocess data for visualization
# Calculate frequency of each criteria
gh1x_base_freq <- scoring %>%
  group_by(gh1x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

gh1x_follow_freq <- scoring %>%
  group_by(gh1x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

pf02x_base_freq <- scoring %>%
  group_by(pf02x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

pf02x_follow_freq <- scoring %>%
  group_by(pf02x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

pf04x_base_freq <- scoring %>%
  group_by(pf04x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

pf04x_follow_freq <- scoring %>%
  group_by(pf04x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

sf2x_base_freq <- scoring %>%
  group_by(sf2x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

sf2x_follow_freq <- scoring %>%
  group_by(sf2x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

rp2x_base_freq <- scoring %>%
  group_by(rp2x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

rp2x_follow_freq <- scoring %>%
  group_by(rp2x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

rp3x_base_freq <- scoring %>%
  group_by(rp3x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

rp3x_follow_freq <- scoring %>%
  group_by(rp3x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

re2x_base_freq <- scoring %>%
  group_by(re2x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

re2x_follow_freq <- scoring %>%
  group_by(re2x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

re3x_base_freq <- scoring %>%
  group_by(re3x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

re3x_follow_freq <- scoring %>%
  group_by(re3x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

bp2x_base_freq <- scoring %>%
  group_by(bp2x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

bp2x_follow_freq <- scoring %>%
  group_by(bp2x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

mh3x_base_freq <- scoring %>%
  group_by(mh3x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

mh3x_follow_freq <- scoring %>%
  group_by(mh3x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

mh4x_base_freq <- scoring %>%
  group_by(mh4x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

mh4x_follow_freq <- scoring %>%
  group_by(mh4x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

vt2x_base_freq <- scoring %>%
  group_by(vt2x_base) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

vt2x_follow_freq <- scoring %>%
  group_by(vt2x_follow) %>%
  summarize(n=n())%>%
  mutate(freq = round(n/sum(n)*100,2))

# Turn NA value to "NA" in scoring
scoring$gh1x_base[is.na(scoring$gh1x_base)] <- "NA"
scoring$pf02x_base[is.na(scoring$pf02x_base)] <- "NA"
scoring$pf04x_base[is.na(scoring$pf04x_base)] <- "NA"
scoring$rp2x_base[is.na(scoring$rp2x_base)] <- "NA"
scoring$rp3x_base[is.na(scoring$rp3x_base)] <- "NA"
scoring$re2x_base[is.na(scoring$re2x_base)] <- "NA"
scoring$re3x_base[is.na(scoring$re3x_base)] <- "NA"
scoring$bp2x_base[is.na(scoring$bp2x_base)] <- "NA"
scoring$mh3x_base[is.na(scoring$mh3x_base)] <- "NA"
scoring$vt2x_base[is.na(scoring$vt2x_base)] <- "NA"
scoring$mh4x_base[is.na(scoring$mh4x_base)] <- "NA"
scoring$sf2x_base[is.na(scoring$sf2x_base)] <- "NA"

#Turn NA value to "NA" in scoring
scoring$gh1x_follow[is.na(scoring$gh1x_follow)] <- "NA"
scoring$pf02x_follow[is.na(scoring$pf02x_follow)] <- "NA"
scoring$pf04x_follow[is.na(scoring$pf04x_follow)] <- "NA"
scoring$rp2x_follow[is.na(scoring$rp2x_follow)] <- "NA"
scoring$rp3x_follow[is.na(scoring$rp3x_follow)] <- "NA"
scoring$re2x_follow[is.na(scoring$re2x_follow)] <- "NA"
scoring$re3x_follow[is.na(scoring$re3x_follow)] <- "NA"
scoring$bp2x_follow[is.na(scoring$bp2x_follow)] <- "NA"
scoring$mh3x_follow[is.na(scoring$mh3x_follow)] <- "NA"
scoring$vt2x_follow[is.na(scoring$vt2x_follow)] <- "NA"
scoring$mh4x_follow[is.na(scoring$mh4x_follow)] <- "NA"
scoring$sf2x_follow[is.na(scoring$sf2x_follow)] <- "NA"


#### Visualization

library(ggplot2)

# Extarct graphs to pdf files
pdf(file = "C:/Users/Acer/Box/Chi_VR12/Output/Graph_Data_241021.pdf")

# PCS and MCS distribution
hist(data$PCS_base,
     main = 'Distribution of PCS (base)',
     xlab = 'Values')
hist(data$PCS_follow,
     main = 'Distribution of PCS (follow)',
     xlab = 'Values')
hist(data$MCS_base,
     main = 'Distribution of MCS (base)',
     xlab = 'Values')
hist(data$MCS_follow,
     main = 'Distribution of MCS (follow)',
     xlab = 'Values')

# Relationship matrix for PCS and MCS 
par(mar = c(5, 5, 4, 2)+0.1)

pairs(~PCS_base + MCS_base + PCS_follow + MCS_follow,col = factor (data$servicedog_or_waitlist), data = data,
      main = "Relationship of PCS and MCS")

# Distribution PCS and MCS by participants groups
ggplot(data, aes(x = servicedog_or_waitlist, y = PCS_base, fill = servicedog_or_waitlist)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  labs(title = "PCS base Scores by Group", x = "Group", y = "PCS Base") + 
  theme_minimal()

ggplot(data, aes(x = servicedog_or_waitlist, y = PCS_follow, fill = servicedog_or_waitlist)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  labs(title = "PCS follow Scores by Group", x = "Group", y = "PCS Follow") + 
  theme_minimal()

ggplot(data, aes(x = servicedog_or_waitlist, y = MCS_base, fill = servicedog_or_waitlist)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  labs(title = "MCS base Scores by Group", x = "Group", y = "MCS Base") + 
  theme_minimal()

ggplot(data, aes(x = servicedog_or_waitlist, y = MCS_follow, fill = servicedog_or_waitlist)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  labs(title = "MCS follow Scores by Group", x = "Group", y = "MCS Follow") + 
  theme_minimal()

mean_std_long <- mean_std %>%
  pivot_longer(cols = -servicedog_or_waitlist, names_to = "variable", values_to = "value")

# Graphs for general mean and standard deviation of PCS and MCS 
ggplot(mean_std_long, aes(x = servicedog_or_waitlist, y = value, fill = servicedog_or_waitlist)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Mean and Standard deviation Valuesof PCS and MCS by group of participations", x = "Groups of participations", y = "Value")

# Individuals graphs for PCS and MCS
# Mean values
# PCS: Physical Component Summary
ggplot(mean_std, aes(x = servicedog_or_waitlist, y = mean_PCS_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean PCS (baseline)", x = "Group of participations", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = PCS_base_mean, color="blue")

ggplot(mean_std, aes(x=servicedog_or_waitlist, y=mean_PCS_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean PCS (follow-up)", x = "Group of participations", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = PCS_follow_mean, color="blue")

# MCS: Mental Component Summary
ggplot(mean_std, aes(x=servicedog_or_waitlist, y=mean_MCS_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean MCS (baseline)", x = "Group of participations", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = MCS_base_mean, color="blue")

ggplot(mean_std, aes(x=servicedog_or_waitlist, y=mean_MCS_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean MCS (follow-up)", x = "Group of participations", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = MCS_follow_mean, color="blue")

# Standard deviation values
# PCS: Physical Component Summary
ggplot(mean_std, aes(x=servicedog_or_waitlist, y=sd_PCS_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standard deviation of PCS (baseline)", x = "Group of participations", y = "Std Value")+
  geom_line() +
  geom_hline(yintercept = PCS_base_sd, color="blue")

ggplot(mean_std, aes(x=servicedog_or_waitlist, y=sd_PCS_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standard deviation of PCS (follow-up)", x = "Group of participations", y = "Std Value")+
  geom_line() +
  geom_hline(yintercept = PCS_follow_sd, color="blue")

# MCS: Mental Component Summary
ggplot(mean_std, aes(x=servicedog_or_waitlist, y=sd_MCS_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standard deviation of MCS (baseline)", x = "Group of participations", y = "Std Value")+
  geom_line() +
  geom_hline(yintercept = MCS_base_sd, color="blue")

ggplot(mean_std, aes(x=servicedog_or_waitlist, y=sd_MCS_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standard deviation of MCS (follow-up)", x = "Group of participations", y = "Std Value")+
  geom_line() +
  geom_hline(yintercept = MCS_follow_sd, color="blue")

# Percentage of replies
# General health
ggplot(scoring, aes(x = gh1x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "General Health (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = gh1x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "General Health (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

# Functioning
# Physical functioning
ggplot(scoring, aes(x = pf02x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Moderate activities (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = pf02x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Moderate activities (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = pf04x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Climbing several flights of stairs (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = pf04x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Climbing several flights of stairs (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

# Social functioning
ggplot(scoring, aes(x = sf2x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "How much time health interferes with social activities (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))
ggplot(scoring, aes(x = sf2x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "How much time health interferes with social activities (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

# Role limitation
# Due to Physical problems
ggplot(scoring, aes(x = rp2x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Accomplished less than you would like due to Physical issues (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = rp2x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Accomplished less than you would like due to Physical issues (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = rp3x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Limitations in kind of work or activities due to Physical issues (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = rp3x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Limitations in kind of work or activities due to Physical issues (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

#Due to Emotional problems
ggplot(scoring, aes(x = re2x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Accomplished less than you would like due to Emotional issues (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = re2x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Accomplished less than you would like due to Emotional issues (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = re3x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Limitations in kind of work or activities due to Emotional issues (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = re3x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Limitations in kind of work or activities due to Emotional issues (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

# Pain interferes
ggplot(scoring, aes(x = bp2x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "How much pain interferes with normal work (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = bp2x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "How much pain interferes with normal work (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

# Mental health
ggplot(scoring, aes(x = mh3x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Felt calm and peaceful (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = mh3x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Felt calm and peaceful (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = mh4x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Felt down and blue (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = mh4x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Felt down and blue (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

# Vitality
ggplot(scoring, aes(x = vt2x_base, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Have a lot energy (baseline)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))

ggplot(scoring, aes(x = vt2x_follow, fill = servicedog_or_waitlist)) +
  geom_bar(position = "fill") +
  labs(title = "Have a lot energy (follow-up)", y = "Portion") +
  scale_fill_manual(values = c("Service Dog" = "#FF6699", "Waitlist" = "lightgreen"))


# Scoring graphs
# General graphs for all items in the survey 
ggplot(scoring_long, aes(x = servicedog_or_waitlist, y = value, fill = servicedog_or_waitlist)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Mean Values of 12 items in the survey ", x = "Group of participations", y = "Mean Value")

# Individuals graphs for each
# General health
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_gh1x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for general health (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = gh1x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_gh1x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for general health (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = gh1x_follow_mean, color="blue")

# Functioning
# Physical functioning
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_pf02x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for moderate activities (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = pf02x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_pf02x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for moderate activities (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = pf02x_follow_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_pf04x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for climibing serval flights of stairs (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = pf04x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_pf04x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for climibing serval flights of stairs (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = pf04x_follow_mean, color="blue")

# Social functioning
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_sf2x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for how much time heal thinterferes with social activities (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = sf2x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_sf2x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for how much time health interferes with social activities (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = sf2x_follow_mean, color="blue")

# Role limitations
# Physical issues
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_rp2x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for accomplishing less than you would like due to Physical issues (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = rp2x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_rp2x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for accomplishing less than you would like due to Physical issues (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = rp2x_follow_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_rp3x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for limitation in the kind of work or activities due to Physical issues (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = rp3x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_rp3x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for limitation in the kind of work or activities due to Physical issues (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = rp3x_follow_mean, color="blue")

# Emotional issues             
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_re2x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for accomplishing less than you would like due to Emotional issues (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = re2x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_re2x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for accomplishing less than you would like due to Emotional issues (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = re2x_follow_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_re3x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for didn't do work or activities as carefully as usual due to Emotional issues (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = re3x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_re3x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for didn't do work or activitiesas carefully as usual due to Emotional issues (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = re3x_follow_mean, color="blue")

# Pain interferes
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_bp2x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for how much pain interferes with normal work (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = bp2x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_bp2x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for how much pain interferes with normal work (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = bp2x_follow_mean, color="blue")

# Mental health
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_mh3x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for feeling calm and peaceful (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = mh3x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_mh3x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for feeling calm and peaceful (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = mh3x_follow_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_mh4x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for feeling down and blue (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = mh4x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_mh4x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for feeling down and blue (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = mh4x_follow_mean, color="blue")

# Vitality
ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_vt2x_base, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for having a lot energy (baseline)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = vt2x_base_mean, color="blue")

ggplot(scoring_mean, aes(x=servicedog_or_waitlist, y=mean_vt2x_follow, fill = servicedog_or_waitlist))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean value for having lot of energy (follow-up)", x = "Group", y = "Mean Value")+
  geom_line() +
  geom_hline(yintercept = vt2x_follow_mean, color="blue")

# Pie graphs for each individuals
# General health
ggplot(gh1x_base_freq, aes(x = "", y = freq, fill = gh1x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for general health (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(gh1x_follow_freq, aes(x = "", y = freq, fill = gh1x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response pecentage for general health (follow-up)", x = NULL, y = NULL, fill = "Response percentage")

# Functioning
# Physical functioning
ggplot(pf02x_base_freq, aes(x = "", y = freq, fill = pf02x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for moderate activities (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(pf02x_follow_freq, aes(x = "", y = freq, fill = pf02x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for moderate activities (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(pf04x_base_freq, aes(x = "", y = freq, fill = pf04x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for climibing serval flights of stairs (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(pf04x_follow_freq, aes(x = "", y = freq, fill = pf04x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for climibing serval flights of stairs (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

# Social Function
ggplot(sf2x_base_freq, aes(x = "", y = freq, fill = sf2x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for how much time health interferes with social activities (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(sf2x_follow_freq, aes(x = "", y = freq, fill = sf2x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for how much time health interferes with social activities (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

# Role limitations
# Physical issues
ggplot(rp2x_base_freq, aes(x = "", y = freq, fill = rp2x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for accomplishing less than you would like due to Physical issues (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(rp2x_follow_freq, aes(x = "", y = freq, fill = rp2x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for accomplishing less than you would like due to Physical issues (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(rp3x_base_freq, aes(x = "", y = freq, fill = rp3x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for limitation in the kind of work or activities due to Physical issues (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(rp3x_follow_freq, aes(x = "", y = freq, fill = rp3x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for limitation in the kind of work or activities due to Physical issues (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

# Emotional issues
ggplot(re2x_base_freq, aes(x = "", y = freq, fill = re2x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for accomplishing less than you would like due to Emotional issues (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(re2x_follow_freq, aes(x = "", y = freq, fill = re2x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for accomplishing less than you would like due to Emotional issues (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(re3x_base_freq, aes(x = "", y = freq, fill = re3x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for didn't do work or activities as carefully as usual due to Emotional issues (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(re3x_follow_freq, aes(x = "", y = freq, fill = re3x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for didn't do work or activities as carefully as usual due to Emotional issues (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

# Pain interferes
ggplot(bp2x_base_freq, aes(x = "", y = freq, fill = bp2x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for how much pain interferes with normal work (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(bp2x_follow_freq, aes(x = "", y = freq, fill = bp2x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for how much pain interferes with normal work (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

# Mental health
ggplot(mh3x_base_freq, aes(x = "", y = freq, fill = mh3x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for feeling calm and peaceful (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(mh3x_follow_freq, aes(x = "", y = freq, fill = mh3x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for feeling calm and peaceful (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(mh4x_base_freq, aes(x = "", y = freq, fill = mh4x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for feeling down and blue (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(mh4x_follow_freq, aes(x = "", y = freq, fill = mh4x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for feeling down and blue (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

# Vitality
ggplot(vt2x_base_freq, aes(x = "", y = freq, fill = vt2x_base)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for having lot of energy (baseline)", x = NULL, y = NULL, fill = "Response percentage") 

ggplot(vt2x_follow_freq, aes(x = "", y = freq, fill = vt2x_follow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) +
  labs(title = "Response percentage for having lot of energy (follow-up)", x = NULL, y = NULL, fill = "Response percentage") 

dev.off()

