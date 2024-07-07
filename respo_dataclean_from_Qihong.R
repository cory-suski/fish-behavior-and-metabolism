rm(list = ls()) 

# Win direction
setwd("C:/Users/aeschne2/Desktop/January 2022 Respirometry/Golden Shiner Resp/100% CAWS")
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/January 2022 Respirometry/Golden Shiner Resp/Control")
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/January 2022 Respirometry/Silver Carp Resp/100% CAWS")
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/January 2022 Respirometry/Golden Shiner Resp/100% CAWS")
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/January 2022 Respirometry/Silver Carp Resp/Control")

# Package
library(dplyr)
library(rlang)
library(readr)
avoidance_assay <- read_csv("avoidance assay water quality.csv")
View(avoidance_assay)

#temperature stats--------------------------------------------
# export csv file
#GS CAWS
GS_caws_waterquality = rbind(GS_caws_1,GS_caws_2,GS_caws_3)
rownames(GS_caws_waterquality) = seq(length=nrow(GS_caws_waterquality))
write.csv(GS_caws_waterquality, "GS_caws_waterquality.csv")

GS_caws_waterquality %>%
  dplyr::select(c("AMB.temp.N.1.avg.temp...C.")) %>%
  summarise_all(list(
    "sample_size" = ~ sum(!is.na(.)), # counts only cells with data (i.e., sample size)
    "mean" = ~ mean(., na.rm = TRUE),
    "sd" = ~ sd(., na.rm = TRUE),
    "se" = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
    "median" = ~ median(., na.rm = TRUE),
    "variance" = ~ var(., na.rm = TRUE),
    "min" = ~ min(., na.rm = TRUE),
    "max" = ~ max(., na.rm = TRUE),
    "5th" = ~ quantile(., .05, na.rm = TRUE),
    "25th" = ~ quantile(., .25, na.rm = TRUE),
    "50th" = ~ quantile(., 0.5, na.rm = TRUE),
    "75th" = ~ quantile(., 0.75, na.rm = TRUE),
    "95th" = ~ quantile(., .95, na.rm = TRUE)
  )) %>%
  write.csv(., file = "GS_CAWS_waterquality_statistics.csv")

#GS Control
GS_control_waterquality = rbind(GS_control_1,GS_control_2,GS_control_3)
rownames(GS_control_waterquality) = seq(length=nrow(GS_control_waterquality))
write.csv(GS_control_waterquality, "GS_control_waterquality.csv")

GS_control_waterquality %>%
  dplyr::select(c("AMB.temp.N.1.avg.temp...C.")) %>%
  summarise_all(list(
    "sample_size" = ~ sum(!is.na(.)), # counts only cells with data (i.e., sample size)
    "mean" = ~ mean(., na.rm = TRUE),
    "sd" = ~ sd(., na.rm = TRUE),
    "se" = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
    "median" = ~ median(., na.rm = TRUE),
    "variance" = ~ var(., na.rm = TRUE),
    "min" = ~ min(., na.rm = TRUE),
    "max" = ~ max(., na.rm = TRUE),
    "5th" = ~ quantile(., .05, na.rm = TRUE),
    "25th" = ~ quantile(., .25, na.rm = TRUE),
    "50th" = ~ quantile(., 0.5, na.rm = TRUE),
    "75th" = ~ quantile(., 0.75, na.rm = TRUE),
    "95th" = ~ quantile(., .95, na.rm = TRUE)
  )) %>%
  write.csv(., file = "GS_control_waterquality_statistics.csv")

#SC CAWS
SC_CAWS_waterquality = rbind(SC_caws_1,SC_caws_2,SC_caws_3)
rownames(SC_CAWS_waterquality) = seq(length=nrow(SC_CAWS_waterquality))
write.csv(SC_CAWS_waterquality, "SC_CAWS_waterquality.csv")

SC_CAWS_waterquality %>%
  dplyr::select(c("AMB.temp.N.1.avg.temp...C.")) %>%
  summarise_all(list(
    "sample_size" = ~ sum(!is.na(.)), # counts only cells with data (i.e., sample size)
    "mean" = ~ mean(., na.rm = TRUE),
    "sd" = ~ sd(., na.rm = TRUE),
    "se" = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
    "median" = ~ median(., na.rm = TRUE),
    "variance" = ~ var(., na.rm = TRUE),
    "min" = ~ min(., na.rm = TRUE),
    "max" = ~ max(., na.rm = TRUE),
    "5th" = ~ quantile(., .05, na.rm = TRUE),
    "25th" = ~ quantile(., .25, na.rm = TRUE),
    "50th" = ~ quantile(., 0.5, na.rm = TRUE),
    "75th" = ~ quantile(., 0.75, na.rm = TRUE),
    "95th" = ~ quantile(., .95, na.rm = TRUE)
  )) %>%
  write.csv(., file = "SC_CAWS_waterquality_statistics.csv")

#SC Control
SC_control_waterquality = rbind(SC_control_1,SC_control_2,SC_control_3)
rownames(SC_control_waterquality) = seq(length=nrow(SC_control_waterquality))
write.csv(SC_control_waterquality, "SC_control_waterquality.csv")

SC_control_waterquality %>%
  dplyr::select(c("AMB.temp.N.1.avg.temp...C.")) %>%
  summarise_all(list(
    "sample_size" = ~ sum(!is.na(.)), # counts only cells with data (i.e., sample size)
    "mean" = ~ mean(., na.rm = TRUE),
    "sd" = ~ sd(., na.rm = TRUE),
    "se" = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
    "median" = ~ median(., na.rm = TRUE),
    "variance" = ~ var(., na.rm = TRUE),
    "min" = ~ min(., na.rm = TRUE),
    "max" = ~ max(., na.rm = TRUE),
    "5th" = ~ quantile(., .05, na.rm = TRUE),
    "25th" = ~ quantile(., .25, na.rm = TRUE),
    "50th" = ~ quantile(., 0.5, na.rm = TRUE),
    "75th" = ~ quantile(., 0.75, na.rm = TRUE),
    "95th" = ~ quantile(., .95, na.rm = TRUE)
  )) %>%
  write.csv(., file = "SC_control_waterquality_statistics.csv")



options(scipen = 999)
# Function for correcting for background resp
bkgd <- function(test, TW, pre, post){
  test$bg1 <- seq(pre[1], post[1], length=nrow(test))
  test$bg2 <- seq(pre[2], post[2], length=nrow(test))
  test$bg3 <- seq(pre[3], post[3], length=nrow(test))
  test$bg4 <- seq(pre[4], post[4], length=nrow(test))
  
  test$MO1 <- (test[,5] - test$bg1)
  test$MO2 <- (test[,12] - test$bg2)
  test$MO3 <- (test[,19] - test$bg3)
  test$MO4 <- (test[,26] - test$bg4)
  
  # final df
  output = test[,c(1,3,7,14,21,28,40:43)]
  return(output)
} 

# Function for MMR and SMR 
SMR = function(test){
  # lowest 10% for SMR
  SMR1 <- test %>%
    select(MO1, CH.1.R.2) %>%
    filter(CH.1.R.2 >= 0.9) %>%
    arrange(MO1) %>%
    top_frac(n=-0.1, MO1) %>% 
    colMeans(na.rm = TRUE)
  SMR2 <- test %>%
    select(MO2, CH.2.R.2) %>%
    filter(CH.2.R.2 >= 0.9) %>%
    arrange(MO2) %>%
    top_frac(n=-0.1, MO2) %>% 
    colMeans(na.rm = TRUE)
  SMR3 <- test %>%
    select(MO3, CH.3.R.2) %>%
    filter(CH.3.R.2 >= 0.9) %>%
    arrange(MO3) %>%
    top_frac(n=-0.1, MO3) %>% 
    colMeans(na.rm = TRUE)
  SMR4 <- test %>%
    select(MO4, CH.4.R.2) %>%
    filter(CH.4.R.2 >= 0.9) %>%
    arrange(MO4) %>%
    top_frac(n=-0.1, MO4) %>% 
    colMeans(na.rm = TRUE)
  
  SMR = c(SMR1[1], SMR2[1], SMR3[1], SMR4[1])
  MR = c("Chamber1", "Chamber2", "Chamber3", "Chamber4")
  df =  data.frame(MR, SMR)
  
  return(df=df)
}




############################################## 
# SMR

# Golden Shiner CAWS
# 1

GS_caws_1 <- read.csv("020422CAWsGoldenShiner.txt", 
                   sep = "\t", skip = 12, header = T, fileEncoding="latin1")
GS_caws_TW_1 = c(8, 9, 8, 9)
GS_caws_pre_1 = c(0.2, 0.1, 0.1, 0.2)
GS_caws_post_1 = rep(0.3, 4)

GS_caws_bkgd_1 = bkgd(GS_caws_1, GS_caws_TW_1, GS_caws_pre_1, GS_caws_post_1)
GS_caws_SMR_1 = SMR(GS_caws_bkgd_1)

# 2
GS_caws_2 <- read.csv("020522CAWsGoldenShiner.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
GS_caws_TW_2 = c(6, 7, 7, 6)
GS_caws_pre_2 = c(0.2, 0.1, 0.1, 0.2)
GS_caws_post_2 = c(0.3, 0.2, 0.2, 0.2)

GS_caws_bkgd_2 = bkgd(GS_caws_2, GS_caws_TW_2, GS_caws_pre_2, GS_caws_post_2)
GS_caws_SMR_2 = SMR(GS_caws_bkgd_2)

# 3
GS_caws_3 <- read.csv("020722CAWsGoldenShiner.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
GS_caws_TW_3 = c(8, 6, 6, 7)
GS_caws_pre_3 = c(0.2, 0.2, 0.1, 0.3)
GS_caws_post_3 = c(0.2, 0.2, 0.1, 0.2)

GS_caws_bkgd_3 = bkgd(GS_caws_3, GS_caws_TW_3, GS_caws_pre_3, GS_caws_post_3)
GS_caws_SMR_3 = SMR(GS_caws_bkgd_3)

# export csv file
GS_caws_SMR = rbind(GS_caws_SMR_1,GS_caws_SMR_2,GS_caws_SMR_3)
rownames(GS_caws_SMR) = seq(length=nrow(GS_caws_SMR))
write.csv(GS_caws_SMR, "GS_caws_SMR.csv")


##############################################################

# Golden Shiner Control
# 1
GS_control_1 <- read.csv("012722controlGoldenShiner.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
GS_control_TW_1 = c(6, 7, 6, 7)
GS_control_pre_1 = c(0.3, 0.3, 0.4, 0.5)
GS_control_post_1 = c(0.3, 0.3, 0.4, 0.4)

GS_control_bkgd_1 = bkgd(GS_control_1, GS_control_TW_1, GS_control_pre_1, GS_control_post_1)
GS_control_SMR_1 = SMR(GS_control_bkgd_1)

# 2
GS_control_2 <- read.csv("020122controlGoldenShiner.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
GS_control_TW_2 = c(8, 7, 7, 7)
GS_control_pre_2 = c(0.2, 0.1, 0.2, 0.2)
GS_control_post_2 = c(0.3, 0.1, 0.3, 0.3)

GS_control_bkgd_2 = bkgd(GS_control_2, GS_control_TW_2, GS_control_pre_2, GS_control_post_2)
GS_control_SMR_2 = SMR(GS_control_bkgd_2)

# 3
GS_control_3 <- read.csv("020222controlGoldenShiner.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
GS_control_TW_3 = c(7, 6, 7, 6)
GS_control_pre_3 = c(0.2, 0.1, 0.2, 0.2)
GS_control_post_3 = c(0.3, 0.2, 0.2, 0.3)

GS_control_bkgd_3 = bkgd(GS_control_3, GS_control_TW_3, GS_control_pre_3, GS_control_post_3)
GS_control_SMR_3 = SMR(GS_control_bkgd_3)

#

#Binding data sets

SC_caws_SMR = rbind(GS_control_SMR_1,GS_control_SMR_2,GS_control_SMR_3)
rownames(GS_control_SMR) = seq(length=nrow(GS_control_SMR))
write.csv(GS_control_SMR, "GS_control_SMR.csv")

#############################################

# Silver Carp CAWS
# 1
SC_caws_1 <- read.csv("020322CAWsSilverCarp.txt", 
                         sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_caws_TW_1 = c(15, 16, 15, 11)
SC_caws_pre_1 = c(0.1, 0.1, 0.1, 0.1)
SC_caws_post_1 = c(0.3, 0.3, 0.3, 0.3)

SC_caws_bkgd_1 = bkgd(SC_caws_1, SC_caws_TW_1, SC_caws_pre_1, SC_caws_post_1)
SC_caws_SMR_1 = SMR(SC_caws_bkgd_1)

# 2
SC_caws_2 <- read.csv("020622CAWsSilverCarp.txt", 
                         sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_caws_TW_2 = c(15, 19, 10, 15)
SC_caws_pre_2 = c(0.2, 0.2, 0.1, 0.2)
SC_caws_post_2 = c(0.2, 0.2, 0.2, 0.2)

SC_caws_bkgd_2 = bkgd(SC_caws_2, SC_caws_TW_2, SC_caws_pre_2, SC_caws_post_2)
SC_caws_SMR_2 = SMR(SC_caws_bkgd_2)

# 3
SC_caws_3 <- read.csv("020822CAWsSilverCarp.txt", 
                         sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_caws_TW_3 = c(14, 16, 18, 14)
SC_caws_pre_3 = c(0.2, 0.2, 0.1, 0.2)
SC_caws_post_3 = c(0.2, 0.2, 0.2, 0.3)

SC_caws_bkgd_3 = bkgd(SC_caws_3, SC_caws_TW_3, SC_caws_pre_3, SC_caws_post_3)
SC_caws_SMR_3 = SMR(SC_caws_bkgd_3)

# 4
SC_caws_4 <- read.csv("020922CAWsSilverCarp.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_caws_TW_4 = c(9, 21, 16, 15)
SC_caws_pre_4 = c(0.3, 0.2, 0.2, 0.2)
SC_caws_post_4 = c(0.3, 0.3, 0.3, 0.3)

SC_caws_bkgd_4 = bkgd(SC_caws_4, SC_caws_TW_4, SC_caws_pre_4, SC_caws_post_4)
SC_caws_SMR_4 = SMR(SC_caws_bkgd_4)

#Binding data sets

SC_caws_SMR = rbind(SC_caws_SMR_1,SC_caws_SMR_2,SC_caws_SMR_3,SC_caws_SMR_4)
rownames(SC_caws_SMR) = seq(length=nrow(SC_caws_SMR))
write.csv(SC_caws_SMR, "SC_caws_SMR.csv")

##################################################

# Silver Carp Control
# 1
SC_control_1 <- read.csv("012422controlSilver.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_control_TW_1 = c(15, 11, 14, 12)
SC_control_pre_1 = c(0.0, 0.1, 0.1, 0.1)
SC_control_post_1 = c(0.1, 0.1, 0.2, 0.0)

SC_control_bkgd_1 = bkgd(SC_control_1, SC_control_TW_1, SC_control_pre_1, SC_control_post_1)
SC_control_SMR_1 = SMR(SC_control_bkgd_1)

# 2
SC_control_2 <- read.csv("012522controlSilver.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_control_TW_2 = c(15, 13, 15, 13)
SC_control_pre_2 = c(0.0, 0.2, 0.2, 0.0)
SC_control_post_2 = c(0.3, 0.3, 0.3, 0.3)

SC_control_bkgd_2 = bkgd(SC_control_2, SC_control_TW_2, SC_control_pre_2, SC_control_post_2)
SC_control_SMR_2 = SMR(SC_control_bkgd_2)

# 3
SC_control_3 <- read.csv("012622controlSilver.txt", 
                      sep = "\t", skip = 12, header = T, fileEncoding="latin1")
SC_control_TW_3 = c(15, 11, 15, 17)
SC_control_pre_3 = c(0.2, 0.2, 0.3, 0.3)
SC_control_post_3 = c(0.4, 0.4, 0.5, 0.5)

SC_control_bkgd_3 = bkgd(SC_control_3, SC_control_TW_3, SC_control_pre_3, SC_control_post_3)
SC_control_SMR_3 = SMR(SC_control_bkgd_3)

SC_control_SMR = rbind(SC_control_SMR_1,SC_control_SMR_2,SC_control_SMR_3)
rownames(SC_control_SMR) = seq(length=nrow(SC_control_SMR))
write.csv(SC_control_SMR, "SC_control_SMR.csv")


# 2 Data seems not good enough???
# F_MMR2 <- read.csv("Fathead minnow MMR 3 6-3-2021.txt", 
#                    sep = "\t", skip = 12, header = T)
# F_TW2 = c(1.65, 1.48, 1.62, 1.16)
# F_pre2 = c(259.5, 284.9, 287.6, 293.1)
# F_preTW2 = rep(1,4)
# F_post2 = c(170.2, 108.0, 77.5, 69.6)
# F_postTW2 = rep(1,4)
# F_bkgd2 = bkgd(F_MMR2, F_TW2, F_pre2, F_preTW2, F_post2, F_postTW2)
# F_MMR_SMR2 = MMR_SMR(F_bkgd2)

# 3
F_MMR3 <- read.csv("Fathead minnow MMR 1 6-1-2021.txt", 
                   sep = "\t", skip = 12, header = T)
F_TW3 = c(2.71, 1.71, 1.49, 1.18)
F_pre3 = c(13.3, 1.5, 0, 2.1)
F_preTW3 = rep(10,4)
F_post3 = c(830.4, 629.0, 892.0, 710.7)
F_postTW3 = rep(1,4)
F_bkgd3 = bkgd(F_MMR3, F_TW3, F_pre3, F_preTW3, F_post3, F_postTW3)
F_MMR_SMR3 = MMR_SMR(F_bkgd3)

F_MMR_SMR = rbind(F_MMR_SMR1,F_MMR_SMR3)
write.csv(F_MMR_SMR, "fathead_MMRSMR.csv")


# Largemouth bass
# 1
L_MMR1 <- read.csv("Largemouth bass MMR 6-21-2021.txt", 
                   sep = "\t", skip = 12, header = T)
L_TW1 = c(219, 166, 190, 231)
L_pre1 = c(0, 0, 0, 0)
L_preTW1 = rep(200,4)
L_post1 = c(0.2, 0, 0, 0)
L_postTW1 = rep(200,4)
L_bkgd1 = bkgd(L_MMR1, L_TW1, L_pre1, L_preTW1, L_post1, L_postTW1)
L_MMR_SMR1 = MMR_SMR(L_bkgd1)

# 2
L_MMR2 <- read.csv("Largemouth bass MMR 6-22-2021.txt", 
                   sep = "\t", skip = 12, header = T)
L_TW2 = c(208, 157, 180, 155)
L_pre2 = c(0, 0, 0, 0)
L_preTW2 = rep(200,4)
L_post2 = c(7.7, 4.6, 9.7, 10.9)
L_postTW2 = c(208, 157, 180, 155)
L_bkgd2 = bkgd(L_MMR2, L_TW2, L_pre2, L_preTW2, L_post2, L_postTW2)
L_MMR_SMR2 = MMR_SMR(L_bkgd2)

L_MMR_SMR = rbind(L_MMR_SMR1,L_MMR_SMR2)
write.csv(L_MMR_SMR, "largemouth_MMRSMR.csv")

# Walleye
# 1 $$ No.4 died
W_MMR1 <- read.csv("Walleye MMR 12-2-2021.txt", 
                   sep = "\t", skip = 12, header = T)
W_TW1 = c(55, 45, 40, 44)
W_pre1 = c(1.6, 3.4, 4, 4)
W_preTW1 = rep(100,4)
W_post1 = c(3.3, 4.5, 6.4, 4.3)
W_postTW1 = rep(100,4)
W_bkgd1 = bkgd_noKG(W_MMR1, W_TW1, W_pre1, W_preTW1, W_post1, W_postTW1)
W_MMR_SMR1 = MMR_SMR(W_bkgd1)

# 2
W_MMR2 <- read.csv("Walleye MMR 12-5-2021.txt", 
                   sep = "\t", skip = 12, header = T)
W_TW2 = c(34, 38, 47, 85)
W_pre2 = c(3, 3.1, 3.3, 2.5)
W_preTW2 = rep(100,4)
W_post2 = c(4.2, 4.3, 4.4, 3.7)
W_postTW2 = rep(100,4)
W_bkgd2 = bkgd_noKG(W_MMR2, W_TW2, W_pre2, W_preTW2, W_post2, W_postTW2)
W_MMR_SMR2 = MMR_SMR(W_bkgd2)

W_MMR_SMR = rbind(W_MMR_SMR1,W_MMR_SMR2)
write.csv(W_MMR_SMR, "walleye_MMRSMR.csv")

##############################################
# RMR, heatwave, post-heatwave, and predator-prey interaction 

# Fathead minnow

# heatwave with largemouth bass
# 1 $$#2 chamber had many bubbles
F_hwlb_1 <- read.csv("Fathead minnow heatwave with predator 6-10-2021.txt", 
                   sep = "\t", skip = 12, header = T)
F_hwlb_TW1 = c(1.57, 1.24, 1.29, 1.17)
F_hwlb_pre1 = c(5.8, 118.3, 23.8, 21.4)
F_hwlb_preTW1 = rep(1,4)
F_hwlb_post1 = c(178.2, 260.2, 196.1, 211.3)
F_hwlb_postTW1 = rep(1,4)
F_hwlb_bkgd1 = bkgd(F_hwlb_1, F_hwlb_TW1, F_hwlb_pre1, 
                   F_hwlb_preTW1, F_hwlb_post1, F_hwlb_postTW1)
F_hwlb_bkgd1$MO2 = NA


# 2
F_hwlb_2 <- read.csv("Fathead minnow heatwave with predator 6-13-2021.txt", 
                  sep = "\t", skip = 12, header = T)
F_hwlb_TW2 = c(2.09, 1.42, 2.51, 2.38)
F_hwlb_pre2 = c(0, 34.9, 0, 18.6)
F_hwlb_preTW2 = rep(1,4)
F_hwlb_post2 = c(230.5, 190.8, 152.3, 187.9)
F_hwlb_postTW2 = rep(1,4)
F_hwlb_bkgd2 = bkgd(F_hwlb_2, F_hwlb_TW2, F_hwlb_pre2, 
                   F_hwlb_preTW2, F_hwlb_post2, F_hwlb_postTW2)


F_hwlb = rbind(MO2_pred_prey(F_hwlb_bkgd1, 40, c("1_1","1_2","1_3","1_4")),
               MO2_pred_prey(F_hwlb_bkgd2, 41, c("2_1","2_2","2_3","2_4")))
write.csv(F_hwlb, "fathead_heatwave_largemouth.csv")
F_timeline_hwlb = timeline(F_hwlb_bkgd1, F_hwlb_bkgd2, 40, 41)
write.csv(F_timeline_hwlb, "timeline_fathead_heatwave_largemouth.csv")

# heatwave with walleye
# 1
F_hww_1 <- read.csv("Fathead minnow heatwave with walleye predator 12-9-2021.txt", 
                     sep = "\t", skip = 12, header = T)
F_hww_TW1 = c(2.15, 1.81, 1.8, 1.61)
F_hww_pre1 = c(0.9, 0.8, 0.9, 0.8)
F_hww_preTW1 = rep(1,4)
F_hww_post1 = c(1.1, 1.1, 1.2, 1.1)
F_hww_postTW1 = rep(1,4)
F_hww_bkgd1 = bkgd_noKG(F_hww_1, F_hww_TW1, F_hww_pre1, 
                    F_hww_preTW1, F_hww_post1, F_hww_postTW1)

# 2
F_hww_2 <- read.csv("Fathead minnow heatwave with walleye predator 12-10-2021.txt", 
                     sep = "\t", skip = 12, header = T)
F_hww_TW2 = c(1.99, 1.37, 1.93, 2.02)
F_hww_pre2 = c(0.8, 0.8, 0.8, 0.8)
F_hww_preTW2 = rep(1,4)
F_hww_post2 = c(1.1, 1.1, 1.1, 1)
F_hww_postTW2 = rep(1,4)
F_hww_bkgd2 = bkgd_noKG(F_hww_2, F_hww_TW2, F_hww_pre2, 
                    F_hww_preTW2, F_hww_post2, F_hww_postTW2)


F_hww = rbind(MO2_pred_prey(F_hww_bkgd1, 42, c("1_1","1_2","1_3","1_4")),
               MO2_pred_prey(F_hww_bkgd2, 42, c("2_1","2_2","2_3","2_4")))
write.csv(F_hww, "fathead_heatwave_walleye.csv")
F_timeline_hww = timeline(F_hww_bkgd1, F_hww_bkgd2, 42, 42)
write.csv(F_timeline_hww, "timeline_fathead_heatwave_walleye.csv")

# no-heatwave with largemouth bass
# 1
F_nohwlb_1 <- read.csv("Fathead minnow no heatwave with predator 6-11-2021.txt", 
                     sep = "\t", skip = 12, header = T)
F_nohwlb_TW1 = c(1.71, 1.78, 1.93, 1.79)
F_nohwlb_pre1 = c(0, 61.9, 0, 27.8)
F_nohwlb_preTW1 = rep(1,4)
F_nohwlb_post1 = c(71.6, 91.6, 54.2, 87.1)
F_nohwlb_postTW1 = rep(1,4)
F_nohwlb_bkgd1 = bkgd(F_nohwlb_1, F_nohwlb_TW1, F_nohwlb_pre1, 
                    F_nohwlb_preTW1, F_nohwlb_post1, F_nohwlb_postTW1)

# 2
F_nohwlb_2 <- read.csv("Fathead minnow no heatwave with predator 6-12-2021.txt", 
                     sep = "\t", skip = 12, header = T)
F_nohwlb_TW2 = c(1.5, 1.76, 1.83, 2.18)
F_nohwlb_pre2 = c(0, 74.6, 0, 0)
F_nohwlb_preTW2 = rep(1,4)
F_nohwlb_post2 = c(78.5, 183.1, 82.3, 119.3)
F_nohwlb_postTW2 = rep(1,4)
F_nohwlb_bkgd2 = bkgd(F_nohwlb_2, F_nohwlb_TW2, F_nohwlb_pre2, 
                    F_nohwlb_preTW2, F_nohwlb_post2, F_nohwlb_postTW2)


F_nohwlb = rbind(MO2_pred_prey(F_nohwlb_bkgd2, 42, c("1_1","1_2","1_3","1_4")),
              MO2_pred_prey(F_nohwlb_bkgd1, 42, c("2_1","2_2","2_3","2_4")))
write.csv(F_nohwlb, "fathead_no_heatwave_largemouth.csv")
F_timeline_nohwlb = timeline(F_nohwlb_bkgd1, F_nohwlb_bkgd2, 42, 42)
write.csv(F_timeline_nohwlb, "timeline_fathead_no_heatwave_largemouth.csv")

# no-heatwave with walleye
# 1
F_nohww_1 <- read.csv("Fathead minnow no heatwave with walleye predator 12-7-2021.txt", 
                     sep = "\t", skip = 12, header = T)
F_nohww_TW1 = c(2, 2.27, 1.43, 3.64)
F_nohww_pre1 = c(1.2, 1.3, 1.3, 1.2)
F_nohww_preTW1 = rep(1,4)
F_nohww_post1 = c(1.0, 1.2, 1.3, 1.2)
F_nohww_postTW1 = rep(1,4)
F_nohww_bkgd1 = bkgd_noKG(F_nohww_1, F_nohww_TW1, F_nohww_pre1, 
                    F_nohww_preTW1, F_nohww_post1, F_nohww_postTW1)

# 2
F_nohww_2 <- read.csv("Fathead minnow no heatwave with walleye predator 12-8-2021.txt", 
                     sep = "\t", skip = 12, header = T)
F_nohww_TW2 = c(1.4, 1.68, 2.01, 1.43)
F_nohww_pre2 = c(0.9, 0.9, 1, 0.9)
F_nohww_preTW2 = rep(1,4)
F_nohww_post2 = c(1.1, 1.1, 1.1, 1.1)
F_nohww_postTW2 = rep(1,4)
F_nohww_bkgd2 = bkgd_noKG(F_nohww_2, F_nohww_TW2, F_nohww_pre2, 
                    F_nohww_preTW2, F_nohww_post2, F_nohww_postTW2)


F_nohww = rbind(MO2_pred_prey(F_nohww_bkgd1, 37, c("1_1","1_2","1_3","1_4")),
                 MO2_pred_prey(F_nohww_bkgd2, 34, c("2_1","2_2","2_3","2_4")))
write.csv(F_nohww, "fathead_no_heatwave_walleye.csv")
F_timeline_nohww = timeline(F_nohww_bkgd1, F_nohww_bkgd2, 37, 34)
write.csv(F_timeline_nohww, "timeline_fathead_no_heatwave_walleye.csv")

# largemouth bass

# heatwave with fathead minnow
# 1
L_hw_1 <- read.csv("Largemouth bass heatwave with prey 6-26-2021.txt", 
                     sep = "\t", skip = 12, header = T)
L_hw_TW1 = c(193, 202, 248, 243)
L_hw_pre1 = c(7.8, 7.3, 4.8, 12.6)
L_hw_preTW1 = rep(200,4)
L_hw_post1 = c(6.4, 5.8, 8.8, 7.1)
L_hw_postTW1 = rep(200,4)
L_hw_bkgd1 = bkgd(L_hw_1, L_hw_TW1, L_hw_pre1, 
                    L_hw_preTW1, L_hw_post1, L_hw_postTW1)

# 2
L_hw_2 <- read.csv("Largemouth bass heatwave with prey 6-27-2021.txt", 
                     sep = "\t", skip = 12, header = T)
L_hw_TW2 = c(249, 163, 176, 242)
L_hw_pre2 = c(0, 0, 0, 0)
L_hw_preTW2 = rep(200,4)
L_hw_post2 = c(9.7, 8.0, 9.3, 6.4)
L_hw_postTW2 = rep(200,4)
L_hw_bkgd2 = bkgd(L_hw_2, L_hw_TW2, L_hw_pre2, 
                    L_hw_preTW2, L_hw_post2, L_hw_postTW2)


L_hw = rbind(MO2_pred_prey(L_hw_bkgd1, 41, c("1_1","1_2","1_3","1_4")),
                MO2_pred_prey(L_hw_bkgd2, 39, c("2_1","2_2","2_3","2_4")))
write.csv(L_hw, "largemouth_heatwave_fathead.csv")
L_hw_bkgd22 = L_hw_bkgd2[-nrow(L_hw_bkgd2),]
L_timeline_hwlb = timeline(L_hw_bkgd1, L_hw_bkgd22, 41, 39)
write.csv(L_timeline_hwlb, "timeline_largemouth_heatwave_minnow.csv")

# no-heatwave with fathead minnow
# 1
L_nohw_1 <- read.csv("Largemouth bass no heatwave with prey 6-23-2021.txt", 
                   sep = "\t", skip = 12, header = T)
L_nohw_TW1 = c(249, 194, 228, 250)
L_nohw_pre1 = c(16.4, 19.3, 17.6, 15.5)
L_nohw_preTW1 = rep(200,4)
L_nohw_post1 = c(5.6, 4.1, 0, 2.7)
L_nohw_postTW1 = rep(200,4)
L_nohw_bkgd1 = bkgd(L_nohw_1, L_nohw_TW1, L_nohw_pre1, 
                  L_nohw_preTW1, L_nohw_post1, L_nohw_postTW1)

# 2
L_nohw_2 <- read.csv("Largemouth bass no heatwave with prey 6-24-2021.txt", 
                   sep = "\t", skip = 12, header = T)
L_nohw_TW2 = c(238, 197, 183, 185)
L_nohw_pre2 = c(0, 0, 0, 0)
L_nohw_preTW2 = rep(200,4)
L_nohw_post2 = c(3.4, 2.2, 1.9, 2.7)
L_nohw_postTW2 = rep(200,4)
L_nohw_bkgd2 = bkgd(L_nohw_2, L_nohw_TW2, L_nohw_pre2, 
                  L_nohw_preTW2, L_nohw_post2, L_nohw_postTW2)


L_nohw = rbind(MO2_pred_prey(L_nohw_bkgd1, 46, c("1_1","1_2","1_3","1_4")),
             MO2_pred_prey(L_nohw_bkgd2, 39, c("2_1","2_2","2_3","2_4")))
write.csv(L_nohw, "largemouth_no_heatwave_fathead.csv")
L_timeline_nohwlb = timeline(L_nohw_bkgd1, L_nohw_bkgd2, 46, 39)
write.csv(L_timeline_nohwlb, "timeline_largemouth_no_heatwave_minnow.csv")

# walleye

# heatwave with fathead minnow
# 1 $$$no fish in chamber #1 and #3
W_hw_1 <- read.csv("Walleye heatwave with prey 12-4-2021.txt", 
                   sep = "\t", skip = 12, header = T)
W_hw_TW1 = c(47, 33, 64, 78)
W_hw_pre1 = c(1.8, 2.6, 1.3, 2)
W_hw_preTW1 = rep(100,4)
W_hw_post1 = c(3.6, 3.4, 3.1, 4.1)
W_hw_postTW1 = rep(100,4)
W_hw_bkgd1 = bkgd_noKG(W_hw_1, W_hw_TW1, W_hw_pre1, 
                  W_hw_preTW1, W_hw_post1, W_hw_postTW1)

# 2
W_hw_2 <- read.csv("Walleye heatwave with prey 12-6-2021.txt", 
                   sep = "\t", skip = 12, header = T)
W_hw_TW2 = c(0, 102, 47, 0)
W_hw_pre2 = c(2.8, 2.8, 2.8, 3.6)
W_hw_preTW2 = rep(100,4)
W_hw_post2 = c(3.1, 3.2, 2.5, 3.1)
W_hw_postTW2 = c(0, 102, 47, 0)
W_hw_bkgd2 = bkgd_noKG(W_hw_2, W_hw_TW2, W_hw_pre2, 
                  W_hw_preTW2, W_hw_post2, W_hw_postTW2)


W_hw = rbind(MO2_pred_prey(W_hw_bkgd1, 39, c("1_1","1_2","1_3","1_4")),
               MO2_pred_prey(W_hw_bkgd2, 43, c("2_1","2_2","2_3","2_4")))
write.csv(W_hw, "walleye_heatwave_fathead.csv")
W_timeline_hwlb = timeline(W_hw_bkgd1, W_hw_bkgd2, 39, 43)
write.csv(W_timeline_hwlb, "timeline_walleye_heatwave_minnow.csv")

# no-heatwave with fathead minnow
# 1 $$$ #2 and #4 died overnight
W_nohw_1 <- read.csv("Walleye non-heatwave predator-prey interaction 11-30-2021 new.txt", 
                     sep = "\t", skip = 12, header = T)
W_nohw_TW1 = c(51, 41, 46, 25)
W_nohw_pre1 = c(3.4, 11.9, 10, 7.9)
W_nohw_preTW1 = rep(100,4)
W_nohw_post1 = c(2.7, 6.2, 6.1, 6)
W_nohw_postTW1 = rep(100,4)
W_nohw_bkgd1 = bkgd_noKG(W_nohw_1, W_nohw_TW1, W_nohw_pre1, 
                    W_nohw_preTW1, W_nohw_post1, W_nohw_postTW1)

# 2
W_nohw_2 <- read.csv("Walleye non-heatwave with prey 12-1-2021.txt", 
                     sep = "\t", skip = 12, header = T)
W_nohw_TW2 = c(30, 41, 59, 21)
W_nohw_pre2 = c(1, 3.3, 3.2, 3.6)
W_nohw_preTW2 = rep(100,4)
W_nohw_post2 = c(3.2, 5.6, 4.9, 5.1)
W_nohw_postTW2 = rep(100,4)
W_nohw_bkgd2 = bkgd_noKG(W_nohw_2, W_nohw_TW2, W_nohw_pre2, 
                    W_nohw_preTW2, W_nohw_post2, W_nohw_postTW2)


W_nohw = rbind(MO2_pred_prey(W_nohw_bkgd1, 40, c("1_1","1_2","1_3","1_4")),
               MO2_pred_prey(W_nohw_bkgd2, 31, c("2_1","2_2","2_3","2_4")))
write.csv(W_nohw, "walleye_no_heatwave_fathead.csv")
W_timeline_nohwlb = timeline(W_nohw_bkgd1, W_nohw_bkgd2, 40, 31)
write.csv(W_timeline_nohwlb, "timeline_walleye_no_heatwave_minnow.csv")
