
# Setting stuff up --------------------------------------------------------
rm(list = ls()) 

# Win direction
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/Data Sheets and Ethovision Data for UMESC")
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
library(dplyr)
library(ggplot2)
library(psych)
packageVersion("psych")

Avoidance_Data_Number_of_Fish_that_Shuttled <- read_excel("Avoidance Data - Number of Fish that Shuttled.xlsx")

library(readr)
Avoidance_Data_Number_of_Fish_that_Shuttled <- read_csv("Avoidance Data - Number of Fish that Shuttled.csv")
View(Avoidance_Data_Number_of_Fish_that_Shuttled)

did_fish_shuttle <- Avoidance_Data_Number_of_Fish_that_Shuttled

View(did_fish_shuttle)

updateR() #will check for new R updates

packageStatus() #checks whether or not there are any packages that need to be updated
update.packages(checkBuilt = TRUE) #will update all packages


packageVersion("dplyr") #one method for checking version of a package
getNamespaceVersion("ggplot2") #alternate method for checking version of a package
citation("dplyr") #provides the citation for a package
citation() #Citation for R

sessionInfo() #shows R version, packages, & other useful stuff (useful for stats sections of papers)
version #will provide current version of R only (sessionInfo is more useful for papers)

# Data with factors

library(readr)
avoidance_data_compiled <- read_csv("C:/Users/aeschne2/Desktop/avoidance data compiled.csv", 
                                    col_types = cols(Species = col_factor(levels = c("SVC", 
                                                                                     "GOS")), Treatment = col_factor(levels = c("CAWS", 
                                                                                                                                "Control")), Shuttled = col_factor(levels = c("Yes", 
                                                                                                                                                                              "No"))))
summary(avoidance_data_compiled)

View(avoidance_data_compiled)
glimpse(avoidance_data_compiled)

SC_shuttle <- avoidance_data_compiled %>%
  filter(Trial != "5", Trial != "6", Species != "GOS")
View(SC_shuttle)
summary(SC_shuttle)

# Data inspection for all shuttle data-----------------------------------------
glimpse(avoidance_data_compiled)
head(avoidance_data_compiled)
summary(avoidance_data_compiled)

## Data cleaning for SC shuttle data-------------------------------------------


SC_shuttle <- avoidance_data_compiled %>%
  filter(Trial != "5", Trial != "6", Species != "GOS")
View(SC_shuttle)

summary(SC_shuttle)
glimpse(SC_shuttle)

## Data cleaning for GS shuttle data -------------------------------------------

GS_shuttle <- avoidance_data_compiled %>%
  filter(Species != "SVC")
View(GS_shuttle)

## Analyses for shuttle data --------------------------------------------------

SC_glm <- glm(Shuttled ~ Treatment, data = SC_shuttle, family = binomial)
summary(SC_glm)
anova(SC_glm)
Anova(SC_glm)
plot(SC_glm)


SC_glm_T <- glm(Shuttled ~ Treatment + Trial, data = SC_shuttle, family = binomial)

AIC(SC_glm,SC_glm_T)
anova(SC_glm,SC_glm_T)
Anova(SC_glm_T)


GS_glm <- glm(Shuttled ~ Treatment, data = GS_shuttle, family = binomial)
summary(GS_glm)
Anova(GS_glm)
plot(GS_glm)

## Plots for shuttle data --------------------------------------

SC_shuttle_panel <- ggplot(SC_shuttle, aes(x=Treatment, fill = Shuttled)) +
  geom_bar(aes(y=..count../8), position = "dodge", colour = "black") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  xlab("Treatment") +
  ylab("Number of Fish") +
  theme_classic() +
  labs(title = "Silver Carp") +
  theme(plot.title = element_text(vjust = -.1, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(1.2,0.9),#first number is left/right axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())

GS_shuttle_panel <- ggplot(GS_shuttle, aes(Treatment, fill = Shuttled)) +
  geom_bar(aes(y=..count../8), position = "dodge", colour = "black") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  xlab("Treatment") +
  ylab("") +
  theme_classic() +
  labs(title = "Golden Shiner") +
  theme(plot.title = element_text(vjust = -.1, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(1.2,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())

did_fish_shuttle_figure <- ggarrange(ncol=2, nrow=1, SC_shuttle_panel, GS_shuttle_panel)
ggsave(filename = "Did_Fish_Shuttle.jpg", did_fish_shuttle_figure, width = 14, height = 12, dpi = 600)


## data cleaning for shuttle frequency data ------------------------------

library(readr)
frequency_data_compiled <- read_csv("Finalized Data Sheets/Shuttle Frequency Data compiled.csv", 
                                            col_types = cols(Species = col_factor(levels = c("GOS", 
                                                                                             "SVC")), Treatment = col_factor(levels = c("CAWS", 
                                                                                                                                        "Control")), `Times Shuttled` = col_integer()))
View(frequency_data_compiled)

SC_frequency <- frequency_data_compiled %>%
  filter(Trial != "5", Species != "GOS")
View(SC_frequency)
glimpse(SC_frequency)

GS_frequency <- frequency_data_compiled %>%
  filter(Species != "SVC")
View(GS_frequency)


## analyses for SC and GS frequency --------------------------

SC_frequency_glm <- glm(Frequency ~ Treatment, data = SC_frequency, family = poisson)
summary(SC_frequency_glm)
Anova(SC_frequency_glm)

GS_frequency_glm <- glm(Frequency ~ Treatment, data = GS_frequency, family = poisson)
summary(GS_frequency_glm)
anova(GS_frequency_glm)
Anova(GS_frequency_glm)



## figure panels for SC and GS data

SC_frequency_boxplot <- ggplot(SC_frequency, aes(Treatment, Frequency)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Shuttle Frequency") +
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

GS_frequency_boxplot <- ggplot(GS_frequency, aes(Treatment, Frequency)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Shuttle Frequency") +
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

shuttle_frequency_figure <- arrangeGrob(SC_frequency_boxplot, GS_frequency_boxplot, ncol=3)
ggsave(filename = "Shuttle Frequency.jpg", shuttle_frequency_figure, width = 14, height = 12, dpi = 600)

## latency data --------------------------------------

library(readr)
latency_data <- Avoidance_Assay_In_Zone_Data <- read_csv("Finalized Data Sheets/Avoidance Assay In Zone Data.csv")
View(latency_data)

SC_latency <- latency_data %>%
  filter(Species != "GS", Fish_ID != "SC5")
View(SC_latency)
glimpse(SC_latency)

aov_SC_latency <- aov(Latency_to_shuttle ~ Treatment, data = SC_latency)
Anova(aov_SC_latency)
plot(aov_SC_latency)

SC_latency_boxplot <- ggplot(SC_latency, aes(Treatment, Latency_to_shuttle)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Latency to Shuttle") +
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
        aspect.ratio = 5 / (1 + sqrt(2)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="chartreuse3", position=position_dodge(width=0.75),show.legend = FALSE)

GS_latency <- latency_data %>%
  filter(Species != "SC")
View(GS_latency)
glimpse(GS_latency)

aov_GS_latency <- aov(Latency_to_shuttle ~ Treatment, data = GS_latency)
Anova(aov_GS_latency)
plot(aov_GS_latency)

GS_latency_boxplot <- ggplot(GS_latency, aes(Treatment, Latency_to_shuttle)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Latency to Shuttle") +
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
        aspect.ratio = 5 / (1 + sqrt(2)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="chartreuse3", position=position_dodge(width=0.75),show.legend = FALSE)

shuttle_latency_figure <- arrangeGrob(SC_latency_boxplot, GS_latency_boxplot, ncol=3)
ggsave(filename = "Shuttle Latency.jpg", shuttle_latency_figure, width = 14, height = 12, dpi = 600)

## time spent on receiving side ------------------------------------------
library(readr)
Avoidance_Assay_Time_Spent_on_Each_Side_data <- read_csv("Finalized Data Sheets/Avoidance Assay Time Spent on Each Side data.csv")
View(Avoidance_Assay_Time_Spent_on_Each_Side_data)

receiving_data <- Avoidance_Assay_Time_Spent_on_Each_Side_data
View(receiving_data)

SC_receiving <- receiving_data %>%
  filter(Species != "GOS", Fish_ID != "SC5")
View(SC_receiving)
glimpse(SC_receiving)

aov_SC_receiving <- aov(Time_Spent_Receving_Side ~ Treatment, data = SC_receiving)
Anova(aov_SC_receiving)
plot(aov_SC_receiving)

GS_receiving <- receiving_data %>%
  filter(Species != "SVC")
View(GS_receiving)

aov_GS_receiving <- aov(Time_Spent_Receving_Side ~ Treatment, data = GS_receiving)
Anova(aov_GS_receiving)
plot(aov_GS_receiving)

## anova for avoidance lengths -------------------------------------------

library(readr)
latency_data <- Avoidance_Assay_In_Zone_Data <- read_csv("Finalized Data Sheets/Avoidance Assay In Zone Data.csv")
View(latency_data)

SC_length <- latency_data %>%
  filter(Species != "GS", Fish_ID != "SC5")
View(SC_length)

aov_SC_length <- aov(Length ~ Treatment, data = SC_length)
Anova(aov_SC_length)

GS_length <- latency_data %>%
  filter(Species != "SC")
View(GS_length)

aov_GS_length <- aov(Length ~ Treatment, data = GS_length)
Anova(aov_GS_length)

##table of data

# buckets data ------------------------------------------

## total distance and velocity analyses for SC ----------------------


Ethovision_data <- read_csv("Finalized Data Sheets/Ethovision distance and velocity.csv")
View(Ethovision_data)

SC_distance <- Ethovision_data %>%
  filter(Species != "GS")
View(SC_distance)

aov_SC_distance <- aov(Total_Distance ~ Treatment, data = SC_distance)
Anova(aov_SC_distance)
plot(aov_SC_distance)

aov_SC_velocity <- aov(Average_Velocity ~ Treatment, data = SC_distance)
Anova(aov_SC_velocity)
plot(aov_SC_velocity)

aov_SC_wallhug <- aov(Wall_hugging_time ~ Treatment, data = SC_distance)
Anova(aov_SC_wallhug)
plot(aov_SC_wallhug)

SC_glm_rotations <- glm(Total_Rotations ~ Treatment, data = SC_distance, family = poisson)
summary(SC_glm_rotations)
anova(SC_glm_rotations)
Anova(SC_glm_rotations)
plot(SC_glm_rotations)

## SC bucket plots ------------------------------

SC_distance_boxplot <- ggplot(SC_distance, aes(Treatment, Total_Distance)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Total Distance (m)") +
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

SC_wallhug_boxplot <- ggplot(SC_distance, aes(Treatment, Wall_hugging_time)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Total Time spent Wall-hugging (s)") +
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

SC_wallhug_boxplot

SC_rotations_boxplot <- ggplot(SC_distance, aes(Treatment, Total_Rotations)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Total Number of Rotations") +
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

## paired boxplot for wallhugging -------------------------------------

wallhug <- Ethovision_data %>%
  filter(Treatment != "NA")


Wallhug_boxplot <- ggplot(wallhug, aes(x=factor(Species, level=c("SC", "GS")), Wall_hugging_time, fill = Treatment)) +
  geom_boxplot(width = .6, lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  xlab("Species") +
  ylab("Total Time spent Wall-hugging (s)") +
  scale_fill_manual(values=c("white", "grey85")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,600), breaks = seq(0,600,100)) +
  scale_x_discrete(labels = c("Silver Carp", "Golden Shiner")) +
  theme_classic() +
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
        legend.position = c(.95,.95),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.25, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.6),show.legend = FALSE)

Wallhug_boxplot

##paired boxplots for rotations---------------------

rotations <- Ethovision_data %>%
  filter(Treatment != "NA")


rotations_boxplot <- ggplot(rotations, aes(x=factor(Species, level=c("SC", "GS")), Total_Rotations, fill = Treatment)) +
  geom_boxplot(width = .6, lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  xlab("Species") +
  ylab("Total Number of Rotations") +
  scale_fill_manual(values=c("white", "grey85")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,60), breaks = seq(0,60,10)) +
  scale_x_discrete(labels = c("Silver Carp", "Golden Shiner")) +
  theme_classic() +
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
        legend.position = c(.95,.95),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.25, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.6),show.legend = FALSE)

rotations_boxplot

## total distance and velocity analyses for GS --------------------

GS_only <- Ethovision_data %>%
  filter(Species != "SC", Fish_ID != "GS18", Fish_ID != "GS25")
View(GS_only)

aov_GS_distance <- aov(Total_Distance ~ Treatment, data = GS_only)
Anova(aov_GS_distance)
plot(aov_GS_distance)

aov_GS_velocity <- aov(Average_Velocity ~ Treatment, data = GS_only)
Anova(aov_GS_velocity)
plot(aov_GS_velocity)

aov_GS_wallhug <- aov(Wall_hugging_time ~ Treatment, data = GS_only)
Anova(aov_GS_wallhug)
plot(aov_GS_wallhug)

GS_glm_rotations <- glm(Total_Rotations ~ Treatment, data = GS_only, family = poisson)
summary(GS_glm_rotations)
anova(GS_glm_rotations)
Anova(GS_glm_rotations)
plot(GS_glm_rotations)

# Ethovision plots -------------------------------


GS_wallhug_boxplot <- ggplot(GS_only, aes(Treatment, Wall_hugging_time)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Total Time spent Wall-hugging (s)") +
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

SC_wallhug_boxplot

wallhug_figure <- arrangeGrob(SC_wallhug_boxplot, GS_wallhug_boxplot, ncol=2)
wallhug_figure

ggsave(filename = "Wallhugging.jpg", wallhug_figure, width = 14, height = 12, dpi = 600)

# unrelated stuff -----------------------------------

TW_aov <- aov(TW ~ Species, data = SMR_mastersheet)
summary(TW_aov)
anova(TW_aov)
Anova(TW_aov)

#shuttle data reanalysis ---------------------------------------------------
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/UMESC Data Analysis")
getwd()

library(readr)
shuttle_data <-read_csv("Shuttle Data_Scored by Amy.csv", 
                                       col_types = cols(Date = col_factor(levels = c("02_16_22","02_07_22","02_09_22","02_06_22","02_08_22","02_01_22","02_05_22","02_04_22","02_02_22","02_10_22"
)), Shuttled = col_factor(levels = c("Yes", 
                                                                                         "No"))))
View(shuttle_data)
glimpse(shuttle_data)

SC_shuttle <- shuttle_data %>%
  filter(Species != "GS")
View(SC_shuttle)

SC_glm <- glm(Shuttled ~ Treatment, data = SC_shuttle, family = binomial)
summary(SC_glm)
anova(SC_glm)
Anova(SC_glm)
plot(SC_glm)

did_fish_shuttle <- ggplot(SC_shuttle, aes(x=Treatment, fill = Shuttled)) +
  geom_bar(aes(y=..count../8), position = "dodge", colour = "black") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  scale_fill_manual(values=c("white", "grey")) +
  xlab("Treatment") +
  ylab("Proportion of Fish") +
  theme_classic() +
  labs(title = "", fill = "Did fish shuttle?") +
  theme(plot.title = element_text(vjust = -.1, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.875,.9),#first number is left/right axis from 0-1
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())

ggsave(filename = "did_fish_shuttle.jpg", did_fish_shuttle, width = 12, height = 10, dpi = 600)

GS_shuttle <- shuttle_data %>%
  filter(Species != "SC")
View(GS_shuttle)

GS_glm <- glm(Shuttled ~ Treatment, data = GS_shuttle, family = binomial)
summary(GS_glm)
anova(GS_glm)
Anova(GS_glm)
plot(GS_glm)

ggplot(GS_shuttle, aes(x=Treatment, fill = Shuttled)) +
  geom_bar(aes(y=..count../8), position = "dodge", colour = "black") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  xlab("Treatment") +
  ylab("Proportion of Fish") +
  theme_classic() +
  labs(title = "Golden Shiner", fill = "Did fish shuttle?") +
  theme(plot.title = element_text(vjust = -.1, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(1,0.9),#first number is left/right axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())

GS_shuttle %>%
  group_by(Treatment) %>%
  select(c("Frequency", "Latency", "Time_spent_receiving")) %>%
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
  write.csv(., file = "GS_shuttle_statistics.csv")


##avoidance length data ----------------------------------------------
aov_SC_length <- aov(Length_cm ~ Treatment, data = SC_shuttle)
Anova(aov_SC_length)
plot(aov_SC_length)

aov_GS_length <- aov(Length_cm ~ Treatment, data = GS_shuttle)
Anova(aov_GS_length)
plot(aov_GS_length)

SC_shuttle %>%
  group_by(Treatment) %>%
  select(c("Length_cm")) %>%
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
  write.csv(., file = "SC_shuttle_length_statistics.csv")



GS_shuttle %>%
  group_by(Treatment) %>%
  select(c("Length_cm")) %>%
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
  write.csv(., file = "GS_shuttle_length_statistics.csv")



## analyses for SC and GS frequency --------------------------

SC_frequency <- shuttle_data %>%
  filter(Species != "GS")
View(SC_frequency)
glimpse(SC_frequency)

SC_freq_CAWS <- shuttle_data %>%
  filter(Species != "GS", Treatment != "Control")
View(SC_freq_CAWS)

SC_freq_Control <- shuttle_data %>%
  filter(Species != "GS", Treatment != "CAWS")
View(SC_freq_Control)

GS_frequency <- shuttle_data %>%
  filter(Species != "SC")
View(GS_frequency)

GS_freq_CAWS <- shuttle_data %>%
  filter(Species != "SC", Treatment != "Control")
View(GS_freq_CAWS)

GS_freq_Control <- shuttle_data %>%
  filter(Species != "SC", Treatment != "CAWS")
View(GS_freq_Control)

SC_frequency_glm <- glm(Frequency ~ Treatment, data = SC_frequency, family = poisson)
summary(SC_frequency_glm)
Anova(SC_frequency_glm)
plot(SC_frequency_glm)

ggplot(SC_freq_CAWS, aes(x=Length_cm, y = Frequency)) +
  geom_point()

ggplot(SC_freq_Control, aes(x=Length_cm, y = Frequency, fill = Treatment)) +
  geom_point()


GS_frequency_glm <- glm(Frequency ~ Treatment + Length_cm, data = GS_frequency, family = poisson)
summary(GS_frequency_glm)
anova(GS_frequency_glm)
Anova(GS_frequency_glm)

GS_freq_CAWS_glm <- glm(Frequency ~ Date, data = GS_freq_CAWS, family = poisson)
summary(GS_freq_CAWS_glm)
anova(GS_freq_CAWS_glm)
Anova(GS_freq_CAWS_glm)

ggplot(GS_freq_CAWS, aes(x=Length_cm, y = Frequency, fill = Treatment)) +
  geom_point()

ggplot(GS_freq_Control, aes(x=Length_cm, y = Frequency, fill = Treatment)) +
  geom_point()

GS_frequency_aov <- aov(GS_frequency_glm)
plot(GS_frequency_glm)
visreg(GS_frequency_glm)
qqnorm(GS_frequency_glm)
shapiro.test(residuals(GS_frequency_glm))

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(GS_frequency_glm, add.smooth = FALSE, which = 1)
E <- resid(GS_frequency_glm)
hist(E, xlab = "Residuals", main = "")
plot(GS_frequency$Treatment, E, xlab = "Treatment", ylab = "Residuals")
par(op)
residualPlots(GS_frequency_glm)
marginalModelPlots(GS_frequency_glm)
influenceIndexPlot(GS_frequency_glm)

par(mfrow = c(2, 2))
plot((distance))

rot_emmeans <- emmeans(GS_frequency_glm)
pairs((freq_emmeans))
pwpp(freq_emmeans)

## analyses for SC and GS latency ----------------------------

SC_latency <- shuttle_data %>%
  filter(Species != "GS")
View(SC_latency)

aov_SC_latency <- aov(Latency ~ Treatment, data = SC_latency)
Anova(aov_SC_latency)
plot(aov_SC_latency)

SC_latency_boxplot <- ggplot(SC_latency, aes(Treatment, Latency)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  xlab("Treatment") +
  ylab("Latency to Shuttle (s)") +
  theme_classic() +
  labs(title = "") +
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
               color="black", position=position_dodge(width=0.75),show.legend = FALSE)

GS_latency <- shuttle_data %>%
  filter(Species != "SC")

aov_GS_latency <- aov(Latency ~ Treatment, data = GS_latency)
Anova(aov_GS_latency)
plot(aov_GS_latency)

## analyses for SC and GS receiving side --------------------------------------

SC_receiving <- shuttle_data %>%
  filter(Species != "GS")

SC_receiving_boxplot <- ggplot(SC_receiving, aes(Treatment, Time_spent_receiving)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Latency to Shuttle") +
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
        aspect.ratio = 5 / (1 + sqrt(2)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="chartreuse3", position=position_dodge(width=0.75),show.legend = FALSE)

aov_SC_receiving <- aov(Time_spent_receiving ~ Treatment, data = SC_receiving)
Anova(aov_SC_receiving)
plot(aov_SC_receiving)

GS_receiving <- shuttle_data %>%
  filter(Species != "SC")

aov_GS_receiving <- aov(Time_spent_receiving ~ Treatment, data = GS_receiving)
Anova(aov_GS_receiving)
plot(aov_GS_receiving)



#residual analyses by group taken from 2.3.6.1 of Zuur et al. mixed model book
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(GS_frequency_glm, add.smooth = FALSE, which = 1)
E <- resid(GS_frequency_glm)
hist(E, xlab = "Residuals", main = "")
plot(GS_frequency$Frequency, E, xlab = "Frequency", ylab = "Residuals")
plot(No_Long$Location, E, xlab = "Location", ylab = "Residuals")
par(op)

#check residuals for AS_Two_Way - run both lines of code together
par(mfrow = c(2, 2))
plot(lm(GS_frequency_glm))

# Diagnostic plots through car package
avPlots(GS_frequency_glm)
qqPlot(GS_frequency_glm, id.n=3)
outlierTest(GS_frequency_glm)
influenceIndexPlot(GS_frequency_glm)
influencePlot(GS_frequency_glm)

# run these 4 lines of code together, also from car package
par(mfrow = c(2, 2))
crPlots(GS_frequency_glm)
influencePlot(GS_frequency_glm)
residualPlot(GS_frequency_glm)
hist(resid(GS_frequency_glm))



##main statistics-------------------------------

GS_shuttle %>%
  group_by(Treatment) %>%
  dplyr::select(c("Time_spent_receiving")) %>%
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
  write.csv(., file = "GS_shuttle_receiving_statistics.csv")

#ethovsion data reanalysis -----------------------------------------------------

setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/UMESC Data Analysis")
getwd()

library(readr)
general_behavior_data_revised <- read_csv("general_behavior_data_revised.csv")
View(general_behavior_data_revised)
glimpse(general_behavior_data_revised)


buckets_data <- general_behavior_data_revised %>%
  filter(Removal == "No")
View(buckets_data)

SC_buckets <- buckets_data %>%
  filter(Species != "GS")
View(SC_buckets)
data <- SC_buckets[ , c("Total_Distance", "Average_Velocity", "Wall_hugging_time", "Total_Rotations", "Turn_angle", "Angular_velocity")] #to check to see if response variables are correlated or not
cor(data)

GS_buckets <- buckets_data %>%
  filter(Species != "SC")
View(GS_buckets)
data <- GS_buckets[ , c("Total_Distance", "Average_Velocity", "Wall_hugging_time", "Total_Rotations", "Turn_angle", "Angular_velocity")]
cor(data)

t.test(Length_cm ~ Treatment, data = SC_buckets)
## Silver carp --------------------------------------------


aov_SC_distance <- aov(Total_Distance ~ Treatment, data = SC_buckets)
anova(aov_SC_distance)
Anova(aov_SC_distance, type = "3")
plot(aov_SC_distance)
residualPlots(aov_SC_distance)

ggplot(SC_buckets, aes(Treatment, Total_Distance)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_SC_velocity <- aov(Average_Velocity ~ Treatment, data = SC_buckets)
Anova(aov_SC_velocity, type = "3")
anova(aov_SC_velocity)
plot(aov_SC_velocity)

ggplot(SC_buckets, aes(Treatment, Average_Velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_SC_turnangle <- aov(Turn_angle ~ Treatment, data = SC_buckets)
Anova(aov_SC_turnangle, type = "3")
anova(aov_SC_turnangle)
plot(aov_SC_turnangle)

ggplot(SC_buckets, aes(Treatment, Turn_angle)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_SC_anglevelocity <- aov(Angular_velocity ~ Treatment, data = SC_buckets)
Anova(aov_SC_anglevelocity, type = "3")
anova(aov_SC_anglevelocity)
plot(aov_SC_anglevelocity)

ggplot(SC_buckets, aes(Treatment, Angular_velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_SC_min <- aov(Min_velocity ~ Treatment, data = SC_buckets)
Anova(aov_SC_min)
plot(aov_SC_min)

ggplot(SC_buckets, aes(Treatment, Min_velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.6),show.legend = FALSE)

aov_SC_max <- aov(Max_velocity ~ Treatment, data = SC_buckets)
Anova(aov_SC_max)
plot(aov_SC_max)

ggplot(SC_buckets, aes(Treatment, Max_velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.6),show.legend = FALSE)

aov_SC_wallhug <- aov(Wall_hugging_time ~ Treatment, data = SC_buckets)
anova(aov_SC_wallhug)
Anova(aov_SC_wallhug, type = "3")
plot(aov_SC_wallhug)


SC_glm_rotations <- glm(Total_Rotations ~ Treatment, data = SC_buckets, family = poisson)
summary(SC_glm_rotations)
anova(SC_glm_rotations)
Anova(SC_glm_rotations, type = "3")
plot(SC_glm_rotations)
hist(SC_glm_rotations$residuals, main = "Residual Histogram")
shapiro.test(residuals(SC_glm_rotations))

library(MASS)

nb_rotations <- glm.nb(Total_Rotations ~ Treatment, data = SC_buckets,
                  control = glm.control(maxit=10000))
summary(nb_rotations)
Anova(nb_rotations, type = "3")
plot(nb_rotations)



summary(SC_buckets)
glimpse(SC_buckets)

aov_SC_length <- aov(Length_cm ~ Treatment, data = SC_buckets)
Anova(aov_SC_length, type = "3")
anova(aov_SC_length)
plot(aov_SC_length)

###statistics for table--------------------------------

SC_buckets %>%
  group_by(Treatment) %>%
  dplyr::select(c("Turn_angle")) %>%
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
  write.csv(., file = "SC_turnangle_statistics.csv")

GS_buckets %>%
  group_by(Treatment) %>%
  dplyr::select(c("Turn_angle")) %>%
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
  write.csv(., file = "GS_turnangle_statistics.csv")

###post hoc rotations--------------------------------------------

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(SC_glm_rotations, add.smooth = FALSE, which = 1)
E <- resid(SC_glm_rotations)
hist(E, xlab = "Residuals", main = "")
plot(SC_buckets$Treatment, E, xlab = "Treatment", ylab = "Residuals")
par(op)
residualPlots(SC_glm_rotations)
marginalModelPlots(SC_glm_rotations)
influenceIndexPlot(SC_glm_rotations)

par(mfrow = c(2, 2))
plot((distance))

rot_emmeans <- emmeans(SC_glm_rotations)
pairs((rot_emmeans))
pwpp(rot_emmeans)

###-------------------------------------------------------------

aov_SC_length <- aov(Length_cm ~ Treatment, data = SC_buckets)
Anova(aov_SC_length)
plot(aov_SC_length)

SC_rotations_boxplot <- ggplot(SC_buckets, aes(Treatment, Total_Rotations)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  xlab("Treatment") +
  ylab("Total Number of Rotations") +
  theme_classic() +
  annotate(geom = "text", x=1, y=55, label = "*", size = 18, color = "red") +
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
               color="black", position=position_dodge(width=0.75),show.legend = FALSE)

SC_wallhug_boxplot <- ggplot(SC_buckets, aes(Treatment, Wall_hugging_time)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter() +
  xlab("Treatment") +
  ylab("Total Number of Rotations") +
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

## Golden shiner --------------------------------------------


aov_GS_distance <- aov(Total_Distance ~ Treatment, data = GS_buckets)
Anova(aov_GS_distance, type = "3")
plot(aov_GS_distance)

aov_GS_velocity<- aov(Average_Velocity ~ Treatment, data = GS_buckets)
Anova(aov_GS_velocity)
plot(aov_GS_velocity)

aov_GS_min <- aov(Min_velocity ~ Treatment, data = GS_buckets)
Anova(aov_GS_min)
plot(aov_GS_min)

ggplot(GS_buckets, aes(Treatment, Min_velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_GS_max <- aov(Max_velocity ~ Treatment, data = GS_buckets)
Anova(aov_GS_max)
plot(aov_GS_max)

ggplot(GS_buckets, aes(Treatment, Max_velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_GS_turnangle <- aov(Turn_angle ~ Treatment, data = GS_buckets)
Anova(aov_GS_turnangle, type = "3")
anova(aov_GS_turnangle)
plot(aov_GS_turnangle)

ggplot(GS_buckets, aes(Treatment, Turn_angle)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_GS_anglevelocity <- aov(Angular_velocity ~ Treatment, data = GS_buckets)
Anova(aov_GS_anglevelocity, type = "3")
anova(aov_GS_anglevelocity)
plot(aov_GS_anglevelocity)

ggplot(SC_buckets, aes(Treatment, Angular_velocity)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE) +
  geom_jitter()

aov_GS_wallhug <- aov(Wall_hugging_time ~ Treatment, data = GS_buckets)
Anova(aov_GS_wallhug)
plot(aov_GS_wallhug)

GS_glm_rotations <- glm(Total_Rotations ~ Treatment + Length_cm, data = GS_buckets, family = poisson)
summary(GS_glm_rotations)
anova(GS_glm_rotations)
Anova(GS_glm_rotations, type = "3")
plot(GS_glm_rotations)

ggplot(GS_buckets, aes(as.character(Length_cm), Total_Rotations, color = Treatment)) +
  geom_boxplot()
  

aov_GS_length <- aov(Length_cm ~ Treatment, data = GS_buckets)
Anova(aov_GS_length)
plot(aov_GS_length)

GS_wallhug_boxplot <- ggplot(GS_buckets, aes(Date, Wall_hugging_time, fill = Treatment)) +
  geom_boxplot(lwd=0.75,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE)

##rotations figure ----------------------------------------------------

buckets_data$Species <- factor(buckets_data$Species, levels = c("SC", "GS"), labels = c("Silver Carp", "Golden Shiner"))

rotations_figure <- buckets_data %>%
  ggplot(aes(Species, Total_Rotations, fill = factor(Treatment))) +
  geom_boxplot(lwd=1.3,color="black", outlier.size = 1.5, outlier.fill = "black", na.rm=TRUE, width=.6) +
  annotate(geom = "text", x=1.15, y=79, label = "*", size = 18, color = "red") +
  scale_fill_manual(labels = c("CAWS", "Control"), values=c("white", "grey")) +
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0,80)) +
  xlab("Species") +
  ylab("Number of Rotations") +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 1.3, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=16),
        axis.line = element_line(size = 1.3),
        axis.text = element_text(size = 16.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 16),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.6),show.legend = FALSE)

rotations_figure

ggsave(filename = "rotations_figure.jpg", rotations_figure, width = 12, height = 10, dpi = 600)

##length tables --------------------------------------------------------

SC_buckets %>%
  group_by(Treatment) %>%
  dplyr::select(c("Total_Rotations")) %>%
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
  write.csv(., file = "SC_buckets_rotations_statistics.csv")

GS_buckets %>%
  group_by(Treatment) %>%
  select(c("Length_cm")) %>%
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
  write.csv(., file = "GS_buckets_length_statistics.csv")

#buckets binned data analysis-----------------------------------------------
##SC--------------------------------

setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/UMESC Data Analysis")
getwd()

library(readr)
SC_data_binned <- read_csv("SC_data_binned.csv")
data <- SC_data_binned[ , c("Total_distance", "Avg_velocity", "Wallhug_time", "Rotations", "Turn_angle", "Angular_velocity")] #to check to see if response variables are correlated or not
cor(data)
cor.test(data, method = "spearman")
View(SC_data_binned)
glimpse(SC_data_binned)

###SC PCA --------------------------------------

SC.pca <-(SC_data_binned[,c(4,6,13)])
SC.pca.id <- (SC_data_binned[,c(1,2,3,14)])

cor_matrix <- cor(SC.pca)
cortest.bartlett(cor_matrix, n = nrow(SC.pca))
cor_matrix

a.fit <- princomp(SC.pca, cor=TRUE)

summary(a.fit) # print variance accounted for
loadings(a.fit) # pc loadings
plot(a.fit) # scree plot

library(psych)
fit <- principal(SC.pca, nfactors=1, residuals = F, rotate="varimax")

summary(fit)
loadings(fit)
KMO(SC.pca)

scoresframe2<-as.data.frame(fit$scores)

SC.pca.score <- cbind(SC.pca.id, scoresframe2)
View(SC.pca.score)
##rbind this to the id dataframe

ggplot(SC.pca.score, aes(x = as.factor(Time_bin), y = PC1, fill = Treatment)) +
  geom_boxplot() +
  geom_smooth(method="lm", se=FALSE, color="black") +
  theme_classic()

a<- ggplot(SC.pca.score, aes(x= Treatment, y= PC1))+ geom_jitter()
a
###lmer---------------------------------------

data <- SC_data_binned[ , c("Total_distance", "Avg_velocity", "Wallhug_time", "Rotations")]
cor(data)

distance <- lmer(log(Total_distance) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(distance)
Anova(distance, type = "3")
anova(distance)
plot(distance)
r.squaredGLMM(distance)
vif(distance)

PC1.lmer <- lmer(rank(PC1) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC.pca.score)
summary(PC1.lmer)
Anova(PC1.lmer, type = "3")
r.squaredGLMM(PC1.lmer)
plot(PC1.lmer)
PC1_emmeans <- emmeans(PC1.lmer, ~Treatment*Time_bin)
pairs((PC1_emmeans))
pwpp(PC1_emmeans)

SC_pc1_boxplot <- ggplot(SC.pca.score) +
  geom_boxplot(aes(x = as.character(Time_bin), y = PC1, fill = Treatment), lwd = 1) +
  theme_classic() +
  xlab("Time (minutes)") +
  ylab("PC1 Score") +
  scale_fill_manual(values=c("white", "grey")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  annotate(geom = "text", x=1.825, y=1.5, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=2.825, y=1, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=3.825, y=1.25, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=4.825, y=.85, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=5.825, y=.85, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=6.825, y=.4, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=7.825, y=0.5, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=8.825, y=0.6, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=9.825, y=0.6, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=10.175, y=2.5, label = "+", size = 14, color = "red") +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 1.3, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=16),
        axis.line = element_line(size = 1.3),
        axis.text = element_text(size = 16.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 16),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  stat_summary(aes(x = as.character(Time_bin), y = PC1, fill = Treatment), fun=mean, geom="point", shape=5, size=2, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.7),show.legend = FALSE)

ggsave(filename = "binned_PC1_figure.jpg", SC_pc1_boxplot, width = 12, height = 10, dpi = 600)

 distance_sd <- lmer(rank(Distance_sd) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(distance_sd)
Anova(distance_sd)
plot(distance_sd)

#residual analyses by group taken from 2.3.6.1 of Zuur et al. mixed model book
op <- par(mfrow = c(3, 2), mar = c(5, 4, 2, 2))
plot(PC1.lmer, add.smooth = FALSE, which = 1)
E <- resid(PC1.lmer)
hist(E, xlab = "Residuals", main = "")
plot(SC.pca.score$Treatment, E, xlab = "Treatment", ylab = "Residuals")
plot(SC.pca.score$Time_bin, E, xlab = "Time bin", ylab = "Residuals")
par(op)

plot(PC1.lmer, type = c("p", "smooth")) 
plot(distance,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))
plot(SC.pca.score,resid(.,type="pearson")~PC1,
          type=c("p","smooth"))
qqnorm(residuals(PC1.lmer))
residualPlots(PC1.lmer)
shapiro.test(PC1.lmer)

par(mfrow = c(2, 2))
plot((PC1.lmer))

TukeyHSD(distance)
r.squaredGLMM(distance)
coef(summary(distance))
coef(distance)


distance_emmeans <- emmeans(distance, ~Treatment*Time_bin)
pairs((distance_emmeans))
pwpp(distance_emmeans)


velocity <- lmer(log(Avg_velocity) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(velocity)
Anova(velocity)
plot(velocity)

min_velocity <- lmer(log(Min_velocity) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(min_velocity)
Anova(min_velocity)
plot(min_velocity)

ggplot(SC_data_binned) +
  geom_boxplot(aes(x = as.character(Time_bin), y = Min_velocity, fill = Treatment), lwd = 1) +
  theme_classic()

max_velocity <- lmer(log(Max_velocity) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(max_velocity)
Anova(max_velocity)
plot(max_velocity)

sd_velocity <- lmer(rank(Velocity_sd) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(sd_velocity)
Anova(sd_velocity)
plot(sd_velocity)

ggplot(SC_data_binned) +
  geom_boxplot(aes(x = as.character(Time_bin), y = Max_velocity, fill = Treatment), lwd = 1) +
  theme_classic()

wallhug_lmer <- lmer(rank(Wallhug_time) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(wallhug_lmer)
Anova(wallhug_lmer, type = "3")
r.squaredGLMM(wallhug_lmer)
plot(wallhug_lmer)



rotations_lmer <- lmer(log(Rotations) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(rotations_lmer)
Anova(rotations_lmer)
plot(rotations_lmer)

turn.angle <- lmer(log(Turn_angle) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = SC_data_binned)
summary(turn.angle)
Anova(turn.angle, type = "3")
anova(turn.angle)
r.squaredGLMM(turn.angle)
plot(turn.angle)

ggplot(SC_data_binned) +
  geom_boxplot(aes(x = as.factor(Time_bin), y = Turn_angle, fill = Treatment), lwd = 1) +
  theme_classic()



##distance plot---------------------------

distance_binned_boxplot <- ggplot(SC_data_binned) +
  geom_boxplot(aes(x = as.character(Time_bin), y = Total_distance, fill = Treatment), lwd = 1) +
  theme_classic() +
  xlab("Time (minutes)") +
  ylab("Distance Traveled (cm)") +
  scale_fill_manual(values=c("white", "grey")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  annotate(geom = "text", x=3.825, y=490, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=4.825, y=350, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=5.825, y=370, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=6.825, y=330, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=7.825, y=370, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=8.825, y=335, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=9.825, y=370, label = "*", size = 18, color = "red") +
  annotate(geom = "text", x=9.175, y=590, label = "+", size = 14, color = "red") +
  annotate(geom = "text", x=10.175, y=740, label = "+", size = 14, color = "red") +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 1.3, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=16),
        axis.line = element_line(size = 1.3),
        axis.text = element_text(size = 16.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 16),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
stat_summary(aes(x = as.character(Time_bin), y = Total_distance, fill = Treatment), fun=mean, geom="point", shape=5, size=2, stroke=2, na.rm=TRUE,
             color="black", position=position_dodge(width=0.7),show.legend = FALSE)

ggsave(filename = "binned_distance_figure.jpg", distance_binned_boxplot, width = 12, height = 10, dpi = 600)

velocity_binned_boxplot <- ggplot(SC_data_binned) +
  geom_boxplot(aes(x = as.character(Time_bin), y = Avg_velocity, fill = Treatment), lwd = 1) +
  theme_classic() +
  xlab("Time (minutes)") +
  ylab("Average Velocity (cm/s)") +
  scale_fill_manual(values=c("white", "grey")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 1.3, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=16),
        axis.line = element_line(size = 1.3),
        axis.text = element_text(size = 16.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 16),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())

ggsave(filename = "binned_velocity_figure.jpg", velocity_binned_boxplot, width = 12, height = 10, dpi = 600)

##GS-----------------------------------

setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/UMESC Data Analysis")
getwd()

library(readr)
GS_data_binned <- read_csv("GS_data_binned.csv")
View(GS_data_binned)
data <- GS_data_binned[ , c("Total_distance", "Avg_velocity", "Wallhug_time", "Rotations", "Turn_angle", "Angular_velocity")]
cor(data)

distance_GS <- lmer(rank(Total_distance) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(distance_GS)
Anova(distance_GS)
anova(distance_GS)
plot(distance_GS)
TukeyHSD(distance_GS)
r.squaredGLMM(distance_GS)
distance_emmeans_GS <- emmeans(distance_GS, ~Treatment*Time_bin)
pairs((distance_emmeans_GS))
pwpp(distance_emmeans_GS)

ggplot(GS_data_binned) +
  geom_boxplot(aes(x = as.character(Time_bin), y = Total_distance, fill = Treatment), lwd = 1) +
  theme_classic()



Min_velocity <- lmer(rank(Min_velocity) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(Min_velocity)
Anova(Min_velocity)
plot(Min_velocity)

Max_velocity <- lmer(rank(Max_velocity) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(Max_velocity)
Anova(Max_velocity)
plot(Max_velocity)

velocity <- lmer(rank(Avg_velocity) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(velocity)
Anova(velocity)
plot(velocity)

wallhug_lmer <- lmer(rank(Wallhug_time) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(wallhug_lmer)
Anova(wallhug_lmer, type = "3")
r.squaredGLMM(wallhug_lmer)
plot(wallhug_lmer)

rotations_lmer <- lmer(rank(Rotations) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(rotations_lmer)
Anova(rotations_lmer)
plot(rotations_lmer)

turnangle_lmer <- lmer(log(Turn_angle) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS_data_binned)
summary(turnangle_lmer)
Anova(turnangle_lmer, type = "3")
r.squaredGLMM(turnangle_lmer)
plot(turnangle_lmer)

velocity_binned_boxplot <- ggplot(GS_data_binned) +
  geom_boxplot(aes(x = as.character(Time_bin), y = Avg_velocity, fill = Treatment), lwd = 1) +
  theme_classic() +
  xlab("Time (minutes)") +
  ylab("Average Velocity (cm/s)") +
  scale_fill_manual(values=c("white", "grey")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  theme(plot.title = element_text(vjust = -2, hjust = 0.5, face="bold", colour="black", size=14), 
        axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 1.3, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=16),
        axis.line = element_line(size = 1.3),
        axis.text = element_text(size = 16.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 16),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())

###GS PCA --------------------------------------

GS.pca <-(GS_data_binned[,c(4,5,9)])
GS.pca.id <- (GS_data_binned[,c(1,2,3,13)])

cor_matrix <- cor(GS.pca)
cortest.bartlett(cor_matrix, n = nrow(GS.pca))
cor_matrix

a.fit <- princomp(GS.pca, cor=TRUE)

summary(a.fit) # print variance accounted for
loadings(a.fit) # pc loadings
plot(a.fit) # scree plot

library(psych)
fit <- principal(GS.pca, nfactors=1, residuals = F, rotate="varimax")

summary(fit)
loadings(fit)
KMO(GS.pca)

scoresframe2<-as.data.frame(fit$scores)

GS.pca.score <- cbind(GS.pca.id, scoresframe2)
View(GS.pca.score)
##rbind this to the id dataframe

ggplot(GS.pca.score, aes(x = as.factor(Time_bin), y = PC1, fill = Treatment)) +
  geom_boxplot() +
  geom_smooth(method="lm", se=FALSE, color="black") +
  theme_classic()

a<- ggplot(GS.pca.score, aes(x= Treatment, y= PC1))+ geom_jitter()
a

PC1.lmer <- lmer(rank(PC1) ~ as.factor(Treatment) * as.factor(Time_bin) + (1|Fish_ID), data = GS.pca.score)
summary(PC1.lmer)
Anova(PC1.lmer, type = "3")
r.squaredGLMM(PC1.lmer)
plot(PC1.lmer)
PC1_emmeans <- emmeans(PC1.lmer, ~Treatment*Time_bin)
pairs((PC1_emmeans))
pwpp(PC1_emmeans)

# Post hoc test -----------------------------------------------------------

library(emmeans)
summary(glht(distance, linfct=mcp (GROUP = "Tukey")))


# multiple comparisons (i.e., post hoc test) Base R
TukeyHSD(aov_SC_latency)

# multiple comparisons (i.e., post hoc test) using emmeans for mixed models
binned_distance_emmeans <- emmeans(distance, ~ Treatment * Time_bin, adjust = "tukey")
contrast(binned_distance_emmeans, "eff", by = "Time_bin")
pairs(binned_distance_emmeans)
pwpp(binned_distance_emmeans)


CTMax_Emmeans <- emmeans(distance, conf.level = .95)
pairs(CTMax_Emmeans)
plot(CTMax_Emmeans, comparisons = TRUE)
pwpp(CTMax_Emmeans)
pwpm(CTMax_Emmeans)
emmeans(CTMax_Emmeans, list(pairwise ~ Sample_type), adjust = "tukey")
multcomp::cld(CTMax_Emmeans, alpha = 0.05, Letters = LETTERS) # display letters thru multcomp

# Summary_statistics ------------------------------------------------------
Avg_CT %>%
  group_by(Sample_type) %>%
  select(c("Avg_CT")) %>%
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
  write.csv(., file = "Avg_CT_statistics.csv")
#water quality ------------------------------------------------------
setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/UMESC Data Analysis/Water Quality")
getwd()

library(readr)
activity_assay <- read_csv("activity assay water quality.csv")
View(activity_assay)

activity_assay %>%
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

SC <- activity_assay %>%
  filter(Species != "GS")
View(SC)

SC %>%
  group_by(Treatment) %>%
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
  write.csv(., file = "SC_activity_water_quality_statistics.csv")

GS <- activity_assay %>%
  filter(Species != "SC")
View(GS)

GS %>%
  group_by(Treatment) %>%
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
  write.csv(., file = "GS_activity_water_quality_statistics.csv")

#avoidance water quality----------------------------------------

setwd("C:/Users/aeschne2/Box/2020_Carp_Contaminants_Proposal/UMESC Data Analysis/Water Quality")
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
