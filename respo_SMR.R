
# Setting stuff up --------------------------------------------------------
rm(list = ls()) 

# Win direction
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/January 2022 Respirometry")
getwd()

getwd()

# package
library(MuMIn)
library(lmerTest)
library(emmeans)
library(multcompView)
library(stringr)
library(grid)
library(gridExtra)
library(tidyverse)
library(tidyverse)
library(readr)
library(installr)
library(readxl)
library(car)
library(ggplot2)
library(dplyr)

SMR_mastersheet <- read_excel("SMR_mastersheet.xlsx", 
                              col_types = c("text", "text", "numeric", 
                                            "text", "text", "numeric", "numeric", 
                                            "date"))
View(SMR_mastersheet)

updateR() #will check for new R updates

packageStatus() #checks whether or not there are any packages that need to be updated
update.packages(checkBuilt = TRUE) #will update all packages
packageVersion("dplyr") #one method for checking version of a package
getNamespaceVersion("ggplot2") #alternate method for checking version of a package
citation("dplyr") #provides the citation for a package
citation() #Citation for R

sessionInfo() #shows R version, packages, & other useful stuff (useful for stats sections of papers)
version #will provide current version of R only (sessionInfo is more useful for papers)

# Data inspection ---------------------------------------------------------
glimpse(SMR_mastersheet)
head(SMR_mastersheet)
summary(SMR_mastersheet)


# Analyses ----------------------------------------------------------------

TW_aov <- aov(TW ~ Species, data = SMR_mastersheet)
summary(TW_aov)
anova(TW_aov)
Anova(TW_aov)





## SC analyses and plots ---------------------------------------------------

SC_only <- SMR_mastersheet %>%
  filter(Species == "SC")
glimpse(SC_only)
summary(SC_only)
View(SC_only)

SC_CAWS_only<- SC_only %>%
  filter(Treatment == "CAWS")

SC_Control_only <- SC_only %>%
  filter(Treatment == "Control")

TW_aov_SC <- aov(TW ~ Treatment, data = SC_only)
Anova(TW_aov_SC, type = "3")

delete <- aov(SMR ~ Chamber, data = SC_only)
Anova(delete)

date_aov_SC <- aov(SMR ~ Date, data=SC_only)
Anova(date_aov_SC, type = "2")

SMR_aov_SC <- aov(SMR ~ Treatment + TW, data = SC_only)
SMR_log <- aov(log(SMR)~ Treatment + log(TW), data = SC_only)
SMR_no_log <- aov(SMR ~ Treatment + TW, data = SC_only)
summary(SMR_aov_SC)$r.squared
Anova(SMR_aov_SC, type = "3")
Anova(SMR_log, type = "3")
plot(SMR_aov_SC)
plot(SMR_log)
TukeyHSD(SMR_aov_SC)
shapiro.test(residuals(SMR_aov_SC))
shapiro.test(residuals(SMR_log))
hist(SMR_aov_SC$residuals, main = "Residual Histogram")
boxplot(SMR_aov_SC$residuals, main = "Residual Boxplot")
ols_test_normality(SMR_aov_SC)
residualPlot(SMR_log)

leveneTest(SMR ~ Treatment + TW, data = SC_only)

no_log <- lm((SMR ~ TW), data = SC_only)
summary(no_log)
Anova(no_log)
plot(no_log)
summary(no_log)$r.squared

TW2 <- TW^2

linear <- lm((SMR ~  TW), data = SC_Control_only)
poly <- lm(SMR ~ poly(TW, 2, raw = TRUE), data = SC_Control_only)
summary(linear)
summary(poly)

log <- lm((log(SMR) ~ log(TW)), data = SC_only)
summary(log)
Anova(log)
plot(log)

AIC(no_log, log)

ggplot(SC_only, aes(log(TW),log(SMR))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x,2))

ggplot(SC_only, aes(TW,SMR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x,2))


ggplot(SC_CAWS_only, aes(TW, SMR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x,2))

ggplot(SC_Control_only, aes(TW, (SMR*TW))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x,2))
 

GS_only %>%
  group_by(Treatment) %>%
  dplyr::select(c("SMR")) %>%
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
  write.csv(., file = "GS_respo_statistics.csv")

plot(cooks.distance(SMR_aov_SC))
op <- par(mfrow = c(2, 2), mar = c(5, 4, 2, 2))
plot(SMR_log, add.smooth = FALSE, which = 1)
E <- resid(SMR_log)
hist(E, xlab = "Residuals", main = "")
plot(SC_only$TW, E, xlab = "TW", ylab = "Residuals")

model <- lm(log(SMR) ~ log(TW), data = SC_only)
SC_only$residuals <- model$resid
SC_only

ggplot(SC_only,aes(TW, residuals))+ 
  geom_point()+ 
  geom_smooth(method = "loess")

ggplot(SC_only,aes(log(TW), log(SMR)))+ 
  geom_point()+ 
  geom_smooth(method = "loess")

aov_SC_length <- aov(TW ~ Treatment, data = SC_only)
Anova(aov_SC_length)
plot(aov_SC_length)


TukeyHSD(SMR_aov_SC)

plot(SMR_aov_SC)

ggplot(SC_only, aes(Treatment, SMR)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  xlab("Treatment") +
  labs(y = expression(bold(mgO[2] / hr)), face = "bold") +
  theme_classic() +
  theme(plot.title = element_blank(), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.75),show.legend = FALSE)


ggsave("SC_respo_boxplot.jpg", dpi = 1000)

## GS analyses and plots ---------------------------------------------------

GS_only <- SMR_mastersheet %>%
  filter(Species == "GS", SMR != "NA")
summary(GS_only)
View(GS_only)

Date_aov_GS <- aov(SMR ~ Date, data = GS_only)
Anova(Date_aov_GS)

TW_aov_GS <- aov(SMR ~ TW, data = GS_only)
Anova(TW_aov_GS, type = "3")

plot(Date_aov_GS)

SMR_aov_GS <- aov(log(SMR) ~ Treatment + log(TW), data = GS_only)
summary(SMR_aov_GS)
Anova(SMR_aov_GS, type = "3")
plot(SMR_aov_GS)
residualPlot(SMR_aov_GS)

plot(cooks.distance(SMR_aov_GS))
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(SMR_aov_GS, add.smooth = FALSE, which = 1)
E <- resid(SMR_aov_GS)
hist(E, xlab = "Residuals", main = "")
plot(GS_only$TW, E, xlab = "TW", ylab = "Residuals")

ggplot(GS_only, aes(group=Date, Date, SMR)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  theme_classic() 


aov_GS_length <- aov(TL ~ Treatment, data = GS_only)
Anova(aov_GS_length)
plot(aov_GS_length)

ggplot(GS_only, aes(Treatment, SMR)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("mgO2 / hr") +
  theme_classic() +
  theme(plot.title = element_blank(), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="chartreuse3", position=position_dodge(width=0.75),show.legend = FALSE)


# water quality data----------------------------------------------
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/January 2022 Respirometry")
getwd()

library(readr)
avoidance_assay <- read_csv("avoidance assay water quality.csv")
View(avoidance_assay)

avoidance_assay %>%
  group_by(Species, Treatment) %>%
  dplyr::select(c("Temp_before", "pH_before", "O2_before")) %>%
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
  write.csv(., file = "activity_water_quality_statistics.csv")

# combining plots in panel form ------------------------------------------


SC_respo_panel <- ggplot(SC_only, aes(Treatment, SMR)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("mgO2 / hr") +
  theme_classic() +
  labs(title = "Silver Carp") +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="chartreuse3", position=position_dodge(width=0.75),show.legend = FALSE)

GS_respo_panel <- ggplot(GS_only, aes(Treatment, SMR)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("mgO2 / hr") +
  theme_classic() +
  labs(title = "Golden Shiner") +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="chartreuse3", position=position_dodge(width=0.75),show.legend = FALSE)

respo_figure <- arrangeGrob(SC_respo_panel, GS_respo_panel, nrow = 2)
ggsave(filename = "respo_figure.jpg", respo_figure, width = 14, height = 12, dpi = 600)

##paired boxplot-----------------------------------------------------------
SMR_1 <- SMR_mastersheet %>%
  filter(SMR != "NA")
View(SMR_1)

SMR_1$Species <- factor(SMR_1$Species, levels = c("SC", "GS"), labels = c("Silver Carp", "Golden Shiner"))

respo_figure <- SMR_1 %>%
  ggplot(aes(Species, SMR, fill = factor(Treatment))) +
  geom_boxplot(lwd=1.5,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE, width=.8) +
  scale_fill_manual(values=c("white", "grey")) +
  scale_y_continuous(breaks = seq(0, 1.5, .5), limits = c(0,1.5)) +
  labs(y = expression(bold(mgO[2] / hr)), face = "bold") +
  xlab("Treatment") +
  annotate(geom = "text", x=.8, y=1.4, label = "*", size = 18, color = "red") +
  theme_classic() +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face = "bold", colour="black", size=30),
        axis.title.x = element_text(face = "bold", colour="black", size=30),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 1.3, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=20),
        axis.text.x = element_text(face="bold", colour="black", size=20),
        axis.line = element_line(size = 1.3),
        axis.text = element_text(size = 16.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 20),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.8),show.legend = FALSE)

respo_figure

ggsave(filename = "respo_figure.jpg", respo_figure, width = 12, height = 10, dpi = 600)


#length tables and SMR means-----------------------------------

SC_only %>%
  group_by(Treatment) %>%
  select(c("TL")) %>%
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
  write.csv(., file = "SC_respo_statistics.csv")

GS_only %>%
  group_by(Treatment) %>%
  select(c("TL")) %>%
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
  write.csv(., file = "GS_respo_statistics.csv")

SC_only %>%
  group_by(Treatment) %>%
  dplyr::select(c("SMR")) %>%
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
  write.csv(., file = "SC_respo_main_statistics.csv")

GS_only %>%
  group_by(Treatment) %>%
  dplyr::select(c("SMR")) %>%
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
  write.csv(., file = "GS_respo_main_statistics.csv")

# junk --------------------------------------------------------------------



SC_SMR <- readxl("SMR_mastersheet.xls")

SC_SMR$MR <- as.factor(F_MS$MR)
F_MS$MR <- factor(F_MS$MR, 
                  levels = c("SMR", "MMR"))
F_MS_MR <- lmer(data = F_MS, 
                MO2 ~ MR + (1|ID)) 
F_MS_MR2 <- lmer(data = F_MS, 
                 MO2 ~ MR + (1|ID)+
                   (1|Date)+
                   (1|TL)) 
AIC(F_MS_MR, F_MS_MR2)
anova(F_MS_MR)
r.squaredGLMM(lmer(data = F_MS, MO2 ~ MR + (1|ID)))

F_MS_em = emmeans(F_MS_MR, 
                  specs = pairwise ~ MR, 
                  adjust = "tukey")#####emmeans

F = ggplot(F_MS, aes(y=MO2, x=MR)) +
  geom_boxplot(aes(fill = MR), alpha = 0.1) +
  geom_line(aes(group=ID), linetype=3) +
  geom_point(aes(color=MR), size=2, alpha = 0.9)+
  theme_classic()+
  scale_color_manual(values = c("SMR" = "seagreen3", 
                                "MMR" = "orangered"))+
  scale_fill_manual(values = c("SMR" = "seagreen3", 
                               "MMR" = "orangered"))+
  scale_y_continuous(limits=c(0,1200))+
  theme(axis.text = element_text(size=16, color = "black"),
        axis.title=element_text(size=16,face="bold"), 
        title = element_text(size=12, face="bold"),
        legend.position="none")+
  xlab("")+ylab("MO2")+
  ggtitle("Fathead minnow")


# largemouth
L_MS <- read.csv("largemouth_MMRSMR.csv")
L_MS$MR <- as.factor(L_MS$MR)
L_MS$MR <- factor(L_MS$MR, 
                  levels = c("SMR", "MMR"))
L_MS_MR <- lmer(data = L_MS, 
                MO2 ~ MR + (1|ID)) 
L_MS_MR2 <- lmer(data = L_MS, 
                 MO2 ~ MR + (1|ID)+
                   (1|Date)+
                   (1|TL)) 
AIC(L_MS_MR, L_MS_MR2)
anova(L_MS_MR)
r.squaredGLMM(lmer(data = L_MS, MO2 ~ MR + (1|ID)))

L_MS_em = emmeans(L_MS_MR, 
                  specs = pairwise ~ MR, 
                  adjust = "tukey")#####emmeans

L = ggplot(L_MS, aes(y=MO2, x=MR)) +
  geom_boxplot(aes(fill = MR), alpha = 0.1) +
  geom_line(aes(group=ID), linetype=3) +
  geom_point(aes(color=MR), size=2, alpha = 0.9)+
  theme_classic()+
  scale_color_manual(values = c("SMR" = "seagreen3", 
                                "MMR" = "orangered"))+
  scale_fill_manual(values = c("SMR" = "seagreen3", 
                               "MMR" = "orangered"))+
  scale_y_continuous(limits=c(0,400))+
  theme(axis.text = element_text(size=16, color = "black"),
        axis.title=element_text(size=16,face="bold"), 
        title = element_text(size=12, face="bold"),
        legend.position="none")+
  xlab("")+ylab("MO2")+
  ggtitle("Largemouth bass")


# walleye
W_MS <- read.csv("walleye_MMRSMR.csv")
W_MS$MR <- as.factor(W_MS$MR)
W_MS$MR <- factor(W_MS$MR, 
                  levels = c("SMR", "MMR"))
W_MS_MR <- lmer(data = W_MS, 
                MO2 ~ MR + (1|ID)) 
W_MS_MR2 <- lmer(data = W_MS, 
                 MO2 ~ MR + (1|ID)+
                   (1|Date)+
                   (1|TL)) 
AIC(W_MS_MR, W_MS_MR2)
anova(W_MS_MR)
r.squaredGLMM(lmer(data = W_MS, MO2 ~ MR + (1|ID)))

W_MS_em = emmeans(W_MS_MR, 
                  specs = pairwise ~ MR, 
                  adjust = "tukey")#####emmeans

W = ggplot(W_MS, aes(y=MO2, x=MR)) +
  geom_boxplot(aes(fill = MR), alpha = 0.1) +
  geom_line(aes(group=ID), linetype=3) +
  geom_point(aes(color=MR), size=2, alpha = 0.9)+
  theme_classic()+
  scale_color_manual(values = c("SMR" = "seagreen3", 
                                "MMR" = "orangered"))+
  scale_fill_manual(values = c("SMR" = "seagreen3", 
                               "MMR" = "orangered"))+
  scale_y_continuous(limits=c(0,700))+
  theme(axis.text = element_text(size=16, color = "black"),
        axis.title=element_text(size=16,face="bold"), 
        title = element_text(size=12, face="bold"),
        legend.position="none")+
  xlab("")+ylab("MO2")+
  ggtitle("Walleye")

grid.arrange(F, L, W, ncol=1)
