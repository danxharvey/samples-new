# **********************************************************
# Please be aware to update the working directory on line 23
# Results on linear regression may differ slightly as seed
# did not set properly
# **********************************************************

# Section 1 - Prepare R --------------------------------------------------

# Define required packages for analysis
packages = c("tidyverse", "dplyr", "car", "ggplot2", "ggthemes", "ggpubr", "rstatix", "DescTools", "emmeans")
# Check and install if not already installed
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
# Check all packages are installed
search()

# Set working directory
dir <- "C:/Users/admin/Desktop/Stats/Assignment/wd"
setwd(dir)
# getwd()

# End of Section 1 - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 2 - Data Load & Cleanse ----------------------------------------

# Load assignment data into R
library(readxl)
file <- "Athlete Data.xlsx"
data <- as.data.frame(read_excel(file))

# Check the structure of the data matches the XLS file
str(data)
names(data)
head(data)
tail(data)
# Variable    Description
# Sport:      Sport
# Sex:        male or female
# Ht:         Height in cm
# Wt:         Weight in kg
# LBM:        Lean body mass
# RCC:        Red cell count
# WCC:        White cell count
# Hc:         Hematocrit
# Hg:         Hemoglobin
# Ferr:       Plasma ferritin concentration
# BMI:        Body mass index = weight/height2
# SSF:        Sum of skin folds
# %Bfat:      % body fat

# Rename the `%Bfat` column for ease of coding
data <-data %>% rename(Bfat=`%Bfat`)
data$Sex <- as.factor(data$Sex)
data$Sport <- as.factor(data$Sport)

# Precursory check (should be 102 males and 100 females)
nrow(data)
sum(data$Sex == "male")
sum(data$Sex == "female")
# Check for NAs in the dataset
sum(is.na(data))  # None found

# End of Section 2 - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 3 - Data Exploration -------------------------------------------

# Breakdown of athletes by sport
library(gridExtra)
library(ggplot2)
library(ggthemes)
# Number of athletes per sport
data %>% ggplot(aes(x=Sex, fill=Sex, )) +
  geom_bar(position = position_dodge(), color="black") +
  scale_fill_manual(values=c("#999999", "#E69F00")) + 
  facet_grid(.~Sport) +
  xlab("") + ylab("Athlete count") +
  ggtitle("Breakdown of athletes per sport") +
  theme_stata() +
  scale_x_discrete(labels=c("fem", "male"))


# Boxplots of response variables by Sport and Sex
library(grid)
sportsexRCC <- data %>% ggplot(aes(Sex, RCC)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Red Cell Count") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexWCC <- data %>% ggplot(aes(Sex, WCC)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("White Cell Count") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexHc <- data %>% ggplot(aes(Sex, Hc)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Haematocrit") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexHg <- data %>% ggplot(aes(Sex, Hg)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Haemoglobin") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexFerr <- data %>% ggplot(aes(Sex, Ferr)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Plasma Ferritin Conc.") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexBMI <- data %>% ggplot(aes(Sex, BMI)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Body Mass Index") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexSSF <- data %>% ggplot(aes(Sex, SSF)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Sum of Skin Folds") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexBfat <- data %>% ggplot(aes(Sex, Bfat)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Bodyfat %") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexLBM <- data %>% ggplot(aes(Sex, LBM)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Lean Body Mass") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexHt <- data %>% ggplot(aes(Sex, Ht)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Height") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
sportsexWt <- data %>% ggplot(aes(Sex, Wt)) + geom_boxplot() + labs(x="", y="") + facet_grid(.~Sport) + ggtitle("Weight") + theme(plot.title = element_text(size = 10, face="plain", hjust = "0.5")) + scale_x_discrete(labels=c("f", "m"))
# Arrange into a compact display format
grid.arrange(sportsexHt, sportsexWt, sportsexLBM, sportsexSSF, nrow=2, ncol=2, top=textGrob("Boxplot of response variables by Sport and Sex", gp=gpar(fontsize=16,font=3)))
grid.arrange(sportsexBMI, sportsexBfat, sportsexRCC, sportsexWCC, nrow=2, ncol=2)
grid.arrange(sportsexHc, sportsexHg, sportsexFerr, nrow=2, ncol=2)
# Clean workspace as the plots are heavy on the system
rm(sportsexRCC, sportsexWCC, sportsexHc, sportsexHg, sportsexFerr, sportsexBMI, sportsexSSF, sportsexBfat, sportsexLBM, sportsexHt, sportsexWt)


# Create function to return 7 number summary (5 number summary plus mean and sd)
SevenNumSum <- function(x){
  
  # Sort data  
  x <- sort(x)
  
  # Calculate positions of required values for mean, lower & upper quartiles
  pos_median <- (length(x)+1)/2
  pos_lq <- (length(x)+1)/4
  pos_uq <- 3*(length(x)+1)/4
  
  # Calculate lower quartile
  lq <- ifelse(pos_lq%%floor(pos_lq) == 0, 
               x[pos_lq],
               x[floor(pos_lq)] + (pos_lq%%floor(pos_lq))*(x[pos_lq+1]-x[pos_lq]))
  
  # Calculate median
  median <- ifelse(pos_median%%floor(pos_median) == 0,
                   x[pos_median],
                   (x[pos_median]+x[pos_median+1])/2)
  
  # Calculate upper quartile
  uq <- ifelse(pos_uq%%floor(pos_uq) == 0,
               x[pos_uq],
               x[floor(pos_uq)] + (pos_uq%%floor(pos_uq))*(x[pos_uq+1]-x[pos_uq]))
  
  # Create 5 number summary vector
  return(c("Min"=min(x), "LQ"=lq, "Median"=median, "UQ"=uq, "Max"=max(x), "Mean"=mean(x), "SD"=sd(x)))
}
# Repeat for each summary required
# x <- data$Ferr[data$Sex=="male"]
x <- data$Wt
SevenNumSum(x)


# *****************************************************************
# THE IQR function creates the dataset of outlier boundaries
# for each response variable.  The following outliers function
# uses the results of the IWR function to identify which values
# are outliers.  The dataset factors are only defined once below
# *****************************************************************
# Function to calculate IQR
# The dataset to run on is defined after the function definition
IQR <- function(x) {

  # Sort data  
  x <- sort(x)
  
  # Calculate positions of lower & upper quartiles
  pos_lq <- (length(x)+1)/4
  pos_uq <- 3*(length(x)+1)/4
  
  # Calculate lower quartile
  lq <- ifelse(pos_lq%%floor(pos_lq) == 0, 
               x[pos_lq],
               x[floor(pos_lq)] + (pos_lq%%floor(pos_lq))*(x[pos_lq+1]-x[pos_lq]))
  
  # Calculate upper quartile
  uq <- ifelse(pos_uq%%floor(pos_uq) == 0,
               x[pos_uq],
               x[floor(pos_uq)] + (pos_uq%%floor(pos_uq))*(x[pos_uq+1]-x[pos_uq]))
  
  # Calculate inter-quartile range
  iqr <- abs(uq-lq)
  
  # Return a vector of quartiles plus ranges
  return(c(lowext=lq-3*iqr, lowmild=lq-1.5*iqr, lq=lq, iqr=iqr, uq=uq, upmild=uq+1.5*iqr, upext=uq+3*iqr))
}
# Create vector of quartiles for the response variables
datIQR <- data # %>% filter(Sex=="male")
resIQR <- sapply(datIQR[3:13], IQR)
resIQR


# Identify the outliers
# Uses datIQR variable as above - Outliers uses IQR data from above function
outliers <- function(x){

  # Define data to use ************** CHANGE FACTORS HERE
  dat <- datIQR %>% select(x+2)

  # Test which values violate outlier boundaries
  outle <- dat <= resIQR[1,x]
  outlm <- dat <= resIQR[2,x] & dat > resIQR[1,x]
  outum <- dat >= resIQR[6,x] & dat < resIQR[7,x]
  outue <- dat >= resIQR[7,x]
  
  # Return values that violate boundaries 
  resle <- dat[outle]    # Returns values that are lower extreme outliers
  reslm <- dat[outlm]    # Returns values that are lower mild outliers
  resum <- dat[outum]    # Returns values that are upper mild outliers
  resue <- dat[outue]    # Returns values that are upper extreme outliers
  
  return(c(LoExtr=resle, LoMild=reslm, upMild=resum, UpExt=resue))
}
# Generate list of outliers
sapply(seq(1, 11, 1), outliers)

# End of Section 3 - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 4.1 - Test differences by Sex ----------------------------------

# Perform two-tailed, two sample t-test on the difference in population means
# between male and female athletes for each response variable
# Prepare data
data_male <- data %>% filter(Sex=="male")
data_female <- data %>% filter(Sex=="female")
# Set var.equal = FALSE to use Welch's test
t.test(data_male$RCC, data_female$RCC, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$WCC, data_female$WCC, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$Hc, data_female$Hc, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$Hg, data_female$Hg, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$Ferr, data_female$Ferr, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$BMI, data_female$BMI, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$SSF, data_female$SSF, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$Bfat, data_female$Bfat, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$LBM, data_female$LBM, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$Ht, data_female$Ht, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male$Wt, data_female$Wt, alternative="two.sided", paired=FALSE, var.equal = FALSE)
# Only WCC is H0, rest are very strongly H1 as per expectation


# Create a dataset excluding outliers at the total dataset level
# We have not proven there is any differences so we should calculate outliers
# as if the null hypothesis is the same

# Re-run outlier calculations to be sure
datIQR <- data
resIQR <- sapply(datIQR[3:13], IQR)
sapply(seq(1, 11, 1), outliers)
# Create dataset
data_no_outliers <- data %>% filter((RCC > resIQR[2,1] & RCC < resIQR[6,1])) %>%
  filter((WCC > resIQR[2,2] & WCC < resIQR[6,2])) %>%
  filter((Hc > resIQR[2,3] & Hc < resIQR[6,3])) %>%
  filter((Hg > resIQR[2,4] & Hg < resIQR[6,4])) %>%
  filter((Ferr > resIQR[2,5] & Ferr < resIQR[6,5])) %>%
  filter((BMI > resIQR[2,6] & BMI < resIQR[6,6])) %>%
  filter((SSF > resIQR[2,7] & SSF < resIQR[6,7])) %>%
  filter((Bfat > resIQR[2,8] & Bfat < resIQR[6,8])) %>%
  filter((LBM > resIQR[2,9] & LBM < resIQR[6,9])) %>%
  filter((Ht > resIQR[2,10] & Ht < resIQR[6,10])) %>%
  filter((Wt > resIQR[2,11] & Wt < resIQR[6,11]))

# Repeat the tests
data_male2 <- data_no_outliers %>% filter(Sex=="male")
data_female2 <- data_no_outliers %>% filter(Sex=="female")
# Set var.equal = FALSE to use Welch's test
t.test(data_male2$RCC, data_female2$RCC, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$WCC, data_female2$WCC, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$Hc, data_female2$Hc, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$Hg, data_female2$Hg, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$Ferr, data_female2$Ferr, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$BMI, data_female2$BMI, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$SSF, data_female2$SSF, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$Bfat, data_female2$Bfat, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$LBM, data_female2$LBM, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$Ht, data_female2$Ht, alternative="two.sided", paired=FALSE, var.equal = FALSE)
t.test(data_male2$Wt, data_female2$Wt, alternative="two.sided", paired=FALSE, var.equal = FALSE)
# Only WCC is H0, rest are very strongly H1

# End of Section 4.1 - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 4.2 - Test differences by Sport ---------------------------------

# Perform a one-way ANOVA on the data

# Check homogeneity
data %>% levene_test(RCC~Sport)
data %>% levene_test(WCC~Sport)
data %>% levene_test(Hc~Sport)
data %>% levene_test(Hg~Sport)
data %>% levene_test(Ferr~Sport)
data %>% levene_test(BMI~Sport)
data %>% levene_test(SSF~Sport)
data %>% levene_test(Bfat~Sport)
data %>% levene_test(LBM~Sport)
data %>% levene_test(Ht~Sport)
data %>% levene_test(Wt~Sport)
# Test failed.  Check normality and then perform Welch's ANOVA.

# QQ Plot for normality test (n > 50) this is preferred to Shapiro-Wilk
q1 <- ggqqplot(residuals(lm(RCC~Sport, data=data))) + ggtitle("RCC") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q2 <- ggqqplot(residuals(lm(WCC~Sport, data=data))) + ggtitle("WCC") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q3 <- ggqqplot(residuals(lm(Hc~Sport, data=data))) + ggtitle("Hc") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q4 <- ggqqplot(residuals(lm(Hg~Sport, data=data))) + ggtitle("Hg") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q5 <- ggqqplot(residuals(lm(sqrt(Ferr)~Sport, data=data))) + ggtitle("Ferr") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q6 <- ggqqplot(residuals(lm(BMI~Sport, data=data))) + ggtitle("BMI") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q7 <- ggqqplot(residuals(lm(sqrt(SSF)~Sport, data=data))) + ggtitle("SSF") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q8 <- ggqqplot(residuals(lm(Bfat~Sport, data=data))) + ggtitle("Bfat") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q9 <- ggqqplot(residuals(lm(LBM~Sport, data=data))) + ggtitle("LBM") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q10 <- ggqqplot(residuals(lm(Ht~Sport, data=data))) + ggtitle("Ht") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
q11 <- ggqqplot(residuals(lm(Wt~Sport, data=data))) + ggtitle("Wt") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
grid.arrange(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, nrow=3, ncol=4, top=textGrob("QQ Plots of response variables to test for normality", gp=gpar(fontsize=16,font=3)))

# Welch ANOVA
data %>% welch_anova_test(RCC ~ Sport)
data %>% welch_anova_test(WCC ~ Sport)
data %>% welch_anova_test(Hc ~ Sport)
data %>% welch_anova_test(Hg ~ Sport)
data %>% welch_anova_test(sqrt(Ferr) ~ Sport)
data %>% welch_anova_test(BMI ~ Sport)
data %>% welch_anova_test(sqrt(SSF) ~ Sport)
data %>% welch_anova_test(Bfat ~ Sport)
data %>% welch_anova_test(LBM ~ Sport)
data %>% welch_anova_test(Ht ~ Sport)
data %>% welch_anova_test(Wt ~ Sport)

# Pairwise t-test with Bonferonni
data %>% pairwise_t_test(RCC ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(WCC ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(Hc ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(Hg ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(Ferr ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(BMI ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(SSF ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(Bfat ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(LBM ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(Ht ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)
data %>% pairwise_t_test(Wt ~ Sport, pool.sd = FALSE, p.adjust.method = "bonferroni", detailed= TRUE, conf.level = 0.95, alternative = "two.sided") %>% filter(p.adj < 0.05) %>% select(df, statistic, group1, group2, conf.low, conf.high, p.adj)

# End of Section 4.2 - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 4.3 - Test differences by Sex & Sport ---------------------------

# Perform a two-way ANOVA on the data

# Compute both in turn
dat <- data
# dat <- data_no_outliers

# Compute Shapiro-Wilk test of normality
shapiro.test(residuals(lm(RCC ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(WCC ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Hc ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Hg ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Ferr ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(BMI ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(SSF ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Bfat ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(LBM ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Ht ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Wt ~ Sex*Sport, data=dat)))

# Transform data and find best fit using no outliers data
dat <- data_no_outliers
shapiro.test(residuals(lm(RCC ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(WCC ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Hc ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Hg ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(sqrt(Ferr) ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(BMI ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(sqrt(SSF) ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(sqrt(Bfat) ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(LBM ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Ht ~ Sex*Sport, data=dat)))
shapiro.test(residuals(lm(Wt ~ Sex*Sport, data=dat)))


# Check for normality with QQ plot
library(gridExtra)
dat <- data_no_outliers
qq1 <- ggqqplot(residuals(lm(RCC ~ Sex*Sport, data=dat))) + ggtitle("RCC") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq2 <- ggqqplot(residuals(lm(WCC ~ Sex*Sport, data=dat))) + ggtitle("WCC") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq3 <- ggqqplot(residuals(lm(Hc ~ Sex*Sport, data=dat))) + ggtitle("Hc") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq4 <- ggqqplot(residuals(lm(Hg ~ Sex*Sport, data=dat))) + ggtitle("Hg") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq5 <- ggqqplot(residuals(lm(sqrt(Ferr) ~ Sex*Sport, data=dat))) + ggtitle("Ferr") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq6 <- ggqqplot(residuals(lm(BMI ~ Sex*Sport, data=dat))) + ggtitle("BMI") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq7 <- ggqqplot(residuals(lm(sqrt(SSF) ~ Sex*Sport, data=dat))) + ggtitle("SSF") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq8 <- ggqqplot(residuals(lm(sqrt(Bfat) ~ Sex*Sport, data=dat))) + ggtitle("Bfat") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq9 <- ggqqplot(residuals(lm(LBM ~ Sex*Sport, data=dat))) + ggtitle("LBM") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq10 <- ggqqplot(residuals(lm(Ht ~ Sex*Sport, data=dat))) + ggtitle("Ht") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
qq11 <- ggqqplot(residuals(lm(Wt ~ Sex*Sport, data=dat))) + ggtitle("Wt") + labs(x="Theoretical Distribution", y="Sample Distribution") + theme(plot.title = element_text(size = 16, face="plain", hjust = "0.5"))
grid.arrange(qq1, qq2, qq3, qq4, qq5, qq6, qq7, qq8, qq9, qq10, qq11, nrow=3, ncol=4, top=textGrob("QQ Plots of response variables to test for normality", gp=gpar(fontsize=16,font=3)))


# Check for homogeneity of variance
dat <- data_no_outliers
dat %>% levene_test(RCC ~ Sex*Sport)
dat %>% levene_test(WCC ~ Sex*Sport)
dat %>% levene_test(Hc ~ Sex*Sport)
dat %>% levene_test(Hg ~ Sex*Sport)
dat %>% levene_test(sqrt(Ferr) ~ Sex*Sport)
dat %>% levene_test(BMI ~ Sex*Sport)
dat %>% levene_test(sqrt(SSF) ~ Sex*Sport)
dat %>% levene_test(sqrt(Bfat) ~ Sex*Sport)
dat %>% levene_test(LBM ~ Sex*Sport)
dat %>% levene_test(Wt ~ Sex*Sport)
dat %>% levene_test(Ht ~ Sex*Sport)


# Mixed data, we will continue with 2 way ANOVA as it is quite robust
# and the data is human data and generally assumed to be normal.
nrow(data_no_outliers)
# 2 Way ANOVA
summary(aov(RCC ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(WCC ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(Hc ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(Hg ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(sqrt(Ferr) ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(BMI ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(sqrt(SSF) ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(sqrt(Bfat) ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(LBM ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(Ht ~ Sex+Sport+Sex*Sport, data=data_no_outliers))
summary(aov(Wt ~ Sex+Sport+Sex*Sport, data=data_no_outliers))


# Post hoc tests for main effect on WCC variable
model <- lm(WCC ~ Sex*Sport, data = data_no_outliers)
data_no_outliers %>% group_by(Sex) %>% anova_test(WCC ~ Sport, error = model)


# Significant Pairwise test for where exactly differences are
# Ferr, BMI, SSF, Bfat
library(emmeans)
data_no_outliers %>% group_by(Sex) %>% emmeans_test(Ferr ~ Sport, p.adjust.method = "bonferroni") %>% filter(p.adj < 0.05)
data_no_outliers %>% group_by(Sex) %>% emmeans_test(BMI ~ Sport, p.adjust.method = "bonferroni") %>% filter(p.adj < 0.05)
data_no_outliers %>% group_by(Sex) %>% emmeans_test(SSF ~ Sport, p.adjust.method = "bonferroni") %>% filter(p.adj < 0.05)
data_no_outliers %>% group_by(Sex) %>% emmeans_test(Bfat ~ Sport, p.adjust.method = "bonferroni") %>% filter(p.adj < 0.05)

# End of Section 4.3 - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 5 - Fit multiple regression model to predict iron ferritin levels -----

# Define test and training set on 80/20 split
set.seed(2)
index <- order(data$Ferr)
set <- sample(index, 202*0.2)
train <- data[-set, ]# %>% filter(Sex=="male")
test <- data[set, ]# %>% filter(Sex=="male")
# Check bias
train %>% group_by(Sex) %>% summarise(athletes = length(Sex)) # 47.5/52.5
test %>% group_by(Sex) %>% summarise(athletes = length(Sex)) # 40/60

# Check pairs for any signs of relationships and collinearity
pairs(train[3:13])

# Check correlation matrix
attach(train)
cor(train[3:13])
detach(train)

# Create a baseline linear model for analysis
# baseline <- lm(data=data, Ferr ~ RCC+WCC+Hc+Hg+BMI+SSF+Bfat+LBM+Ht+Wt)
baseline <- lm(data=train, Ferr ~ RCC+WCC+Hc+Hg+BMI+SSF+Bfat+LBM+Ht+Wt)
summary(baseline)
# Baseline model explains 19% of the variation
# Taking a look at the baseline residuals
baseline.res <- resid(baseline)


# Plot predicted against residuals
plot(predict.lm(baseline), baseline.res, xlab="Fitted Values", ylab="Baseline Residuals")
abline(h=0)
# Plot QQ against residuals for normality
qqnorm(baseline.res, ylab="Iron Ferritin residual quantiles", main="Normal QQ Plot")
qqline(baseline.res, lty=2)
# Plot residuals in order of observation to test independence
plot(seq(1, length(baseline.res), 1), baseline.res, xlab="Observation Order", ylab="Residual", type="b",
     main="Residuals by order on observation")
abline(h=0, lty=2)

attach(train)
# Check dependent variables v independent variables for a linear relationship
plot(RCC, baseline.res, xlab="RCC", ylab="Baseline Residuals")
abline(h=0)
plot(WCC, baseline.res, xlab="WCC", ylab="Baseline Residuals")
abline(h=0)
plot(Hc, baseline.res, xlab="Hc", ylab="Baseline Residuals")
abline(h=0)
plot(Hg, baseline.res, xlab="Hg", ylab="Baseline Residuals")
abline(h=0)
plot(SSF, baseline.res, xlab="SSF", ylab="Baseline Residuals")
abline(h=0)
plot(Bfat, baseline.res, xlab="Bfat", ylab="Baseline Residuals")
abline(h=0)
plot(BMI, baseline.res, xlab="BMI", ylab="Baseline Residuals")
abline(h=0)
plot(LBM, baseline.res, xlab="LBM", ylab="Baseline Residuals")
abline(h=0)
plot(Ht, baseline.res, xlab="Ht", ylab="Baseline Residuals")
abline(h=0)
plot(Wt, baseline.res, xlab="Wt", ylab="Baseline Residuals")
abline(h=0)
detach(train)


# Fit predicted values against the observed values
plot(train$Ferr, predict.lm(baseline), xlab="Iron Ferritin Levels", ylab="Predicted Values",
    main="Plot of baseline model for predicting iron ferritin")
abline(0, 1, lty=2)
# The plot shows that this is not a good model


# Check Variance Inflated Factor for multicollinearity
VIF(baseline)   # High degrees of multicollinearity


# Perform a drop1 test
# basedrop <- lm(data=data, Ferr ~ RCC+WCC+Hc+Hg+BMI+SSF+Bfat+LBM+Ht+Wt)
# Iterate here from full model above - remove lowest F value
basedrop <- lm(data=train, Ferr ~ RCC+WCC+Hc+Hg+BMI+SSF+Bfat+LBM+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# View summary
summary(basedrop)

# Refine model with drop1 methodology
# Remove RCC
basedrop <- lm(data=train, Ferr ~ WCC+Hc+Hg+BMI+SSF+Bfat+LBM+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove LBM
basedrop <- lm(data=train, Ferr ~ WCC+Hc+Hg+BMI+SSF+Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove BMI
basedrop <- lm(data=train, Ferr ~ WCC+Hc+Hg+SSF+Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove SSF
basedrop <- lm(data=train, Ferr ~ WCC+Hc+Hg+Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove WCC
basedrop <- lm(data=train, Ferr ~ Hc+Hg+Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove Hc
basedrop <- lm(data=train, Ferr ~ Hg+Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove WCC
basedrop <- lm(data=train, Ferr ~ Hg+Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# Remove Hg
basedrop <- lm(data=train, Ferr ~ Bfat+Ht+Wt)
with(train, {
  drop1(basedrop, test="F")
})
# View summary
summary(basedrop)
basedrop$coefficients


# Model is skewed and not really centred around zero.
# Model covers 19% of the variance
# Various log transformations occurred but had no effect on the model
predmodel <- lm(data=train, Ferr ~ Bfat+Ht+Wt)

# Check model fit
# Fit predicted values against the observed values
plot(train$Ferr, predict.lm(predmodel), xlab="Iron Ferritin Levels", ylab="Predicted Values")
abline(0, 1, lty=2)


# Look at predictions
# sample size
n <- length(test$Ferr)
Ferr <- test$Ferr

attach(test)
# Find predicted values using model developed using training data
FerrPred <- 307.5102 - 2.1874*Bfat - 2.0229*Ht + 2.1946*Wt

# plot predicted versus actual observations
plot(Ferr, FerrPred,xlim=c(0,250),ylim=c(0,250))
abline(0,1,lty=2)

# find SStot, the total variation of the test set observations
SStot <- sum((Ferr)^2)-(sum(Ferr))^2/n

# and SSres - note this is from the difference between the predictions
# and the actual. So we are finding the total variation of the residuals
FerrPredDiff<- FerrPred - Ferr
SSres <- sum(FerrPredDiff^2) - (sum(FerrPredDiff))^2/n

# So explained variation is 1 - the total variation in the residuals / total
# Gives us an amount of variance in our residuals
R2 <- 1 - SSres/SStot
R2

detach(test)


# Plot original values against training data
plot(train$Ferr, predmodel$fitted.values, col="#B2B2B2",
     main="Plot of observed iron ferritin levels (training set)",
     xlim=c(0, 175),ylim=c(0, 175),
     xlab="Observed iron ferritin values (training set)", ylab="Fitted iron ferritin values")
abline(0,1, lty=1)


# Now predicted against new test observations
plot(Ferr, FerrPred,
     main="Plot of predicted versus observed iron ferritin levels",
     xlim=c(0, 250), ylim=c(0, 250), col="#0612F8",
     xlab="Observed iron ferritin values (test set)", ylab="Predicted iron ferritin values")
abline(0,1,lty=1)

# and now both on same plot
plot(train$Ferr, predmodel$fitted.values, col="#B2B2B2",
     main="Plot of predicted versus observed data for iron ferritin",
     xlim=c(0, 250),ylim=c(0, 250),
     xlab="Observed iron ferritin values", ylab="Predicted iron ferritin levels (ng/ml)")
points(Ferr, FerrPred, col="#0612F8", pch=3)
legend(x=0,y=250,c("Observed","Predicted"),cex=.8,col=c("#B2B2B2","#0612F8"),pch=c(1,3))
abline(0,1, lty=1)

# End of Section 5 - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Section 6 - Additional Insights -----------------------------------------------------


# Calculate means of WCC by Sex and Sport
data %>% group_by(Sex) %>% summarise(mean = mean(WCC), sd = sd(WCC))
data %>% group_by(Sex, Sport) %>% summarise(mean = mean(WCC), sd = sd(WCC))


# Number of athletes with low haemoglobin levels
data %>% filter(Sex=="male" & Hg < 14)
data %>% filter(Sex=="female" & Hg < 12)
# 2 of each sex = 2%


# Number of athletes outside of normal ferritin levels
FerrLowMale <- data %>% filter(Sex=="male" & Ferr < 20)
FerrLowFemale <- data %>% filter(Sex=="female" & Ferr < 20)
FerrHighMale <- data %>% filter(Sex=="male" & Ferr > 200)
FerrHighFemale <- data %>% filter(Sex=="female" & Ferr > 120)

# Athlese under TeamUSA diagnosis levels
FerrLowMale <- data %>% filter(Sex=="male" & Ferr < 40)
FerrLowFemale <- data %>% filter(Sex=="female" & Ferr < 40)


