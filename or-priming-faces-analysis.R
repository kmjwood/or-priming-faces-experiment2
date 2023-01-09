## Title: or-priming-faces analysis
## Author: Tina Seabrooke, Katie Wood 

## Set up ----
rm(list=ls())

## Load libraries
library(plyr)    # provides 'ddply'
library(Rmisc)   # provides 'summarySE'
library(ez)      # provides 'ezANOVA'
library(ggplot2) # proves 'ggplot'
library(tidyverse) 
library(BayesFactor) # provides 'BayesFactor'
library(pwr)     # calculates power

## Read data files
data <- read.csv("or-priming-faces-data.csv", stringsAsFactors = F)

## Load custom functions
source('or-priming-faces-functions.R')

## Remove scientific notation
options(scipen = 999)

## Training ----

## Subset discriminative training data
training <- subset(data, Running == 'Training')

## Mean training scores per participant and stimulus
training <- ddply(
  training,
  c('Subject', 'Stimulus'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum),
  meanRT = mean(RT)
)

## Test

## Subset test data where participants performed a response at the right time
test <- subset(data, Running == 'Test' & Discard == 'False')


## Percent correct per condition
overallacc <- ddply(
  test,
  c('Congruency', 'Load'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)


## Percent correct per participant and condition
acc <- ddply(
  test,
  c('Subject', 'Congruency', 'Load'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Factorise columns for ezANOVA
cols <- c("Subject", "Congruency", "Load")
acc[cols] <- lapply(acc[cols], factor)

## Test Accuracy ANOVA (all participants)
acc.ez <- ezANOVA(
  data     = acc,
  wid      = Subject,
  dv       = percentAcc,
  within   = .(Congruency, Load),
  detailed = T,
  type     = 3
)
print('Test Accuracy ANOVA: Congruency x Load')
print(acc.ez)

print('Mean, SEMs for main effect of Congruency')
summarySE(acc, measurevar = 'percentAcc',groupvars = 'Congruency')


## Percent correct per participant across all conditions
testacc <- ddply(
  test,
  c('Subject', 'Age', 'Sex'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Descriptive stats for participant sample 

print('Mean, SEMs for age and gender for all participants')
summarySE(testacc, measurevar = 'Age')

print('Mean, SEMs for age and gender for all participants')
summarySE(testacc, measurevar = 'Age','Sex')


## Exclude participants >=80% correct on test trials
testacc <- subset(testacc, percentAcc < 80)

## remove participants that are in the ‘testacc’ data.frame 
## (i.e. those that scored ## under 80%)
test <- test[!(test$Subject %in% testacc$Subject),]

## Percent correct per condition (excluding below 80% acc)
overallhighacc <- ddply(
  test,
  c('Congruency', 'Load'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Percent correct per participant (excluding below 80% acc)
hightestacc <- ddply(
  test,
  c('Subject','Age', 'Sex'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Rerun descriptive stats for participant sample (excluding below 80% accuracy)

print('Mean, SEMs for age and gender for all participants')
summarySE(hightestacc, measurevar = 'Age')

print('Mean, SEMs for age and gender for all participants')
summarySE(hightestacc, measurevar = 'Age','Sex')


## Percent correct per participant and condition (excluding below 80% acc)
highacc <- ddply(
  test,
  c('Subject','Congruency', 'Load'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Factorise columns for ezANOVA
cols <- c("Subject", "Congruency", "Load")
highacc[cols] <- lapply(highacc[cols], factor)

## Test accuracy ANOVA (participants who scored ##over 80% only)
highacc.ez <- ezANOVA(
  dat      = highacc,
  wid      = Subject,
  dv       = percentAcc,
  within   = .(Congruency, Load),
  detailed = T,
  type     = 3)
print('Test accuracy ANOVA: Congruency x Load repeated measures ANOVA')
print('DV = percent correct')
print(highacc.ez)

print('Mean, SEMs for main effect of Congruency')
summarySE(highacc, measurevar = 'percentAcc',groupvars = 'Congruency')


## Bayes Factor analysis ------------------------------------------------

## Factorise columns for anovaBF

highacc$Congruency <- factor(highacc$Congruency)
highacc$Load <- factor(highacc$Load)
highacc$Subject <- factor(highacc$Subject)

library(BayesFactor)

anovaBF(formula = percentAcc ~ Congruency + Load + Subject,
        data = data.frame(highacc),
        whichRandom = "Subject")

highacc %>% group_by(Congruency) %>% summarise(mean(percentAcc))




## List of pairwise comparisons for t-tests
pairwise <- list(highacc)

## Function for pairwise comparisons
tests <- function (p){
  ## t-test
  tt <- t.test(percentAcc ~ Congruency, paired = T, data = p)
  ## Cohen's d_z
  dz <- p[,c("Subject", "Congruency", "percentAcc")]  
  colnames(dz) <- c('id', 'cond', 'dv')
  cd <- cohen.dz(dz)
  ret <- list(tt, cd)
}

## Apply function to each item in list
print("Pairwise comparisons")
lapply(pairwise, tests)


library(pwr)     # calculates power

## Power test (replace d value with calculated cohen d (or mean d)
pwr.t.test(d=0.47,n=47,sig.level=0.05,type="paired",alternative="two.sided")


## Graphs ----

# Renaming factor levels dplyr
library(dplyr)
highacc$Distraction <- recode_factor(highacc$Load, "Load" = "Distraction", 
                                "No Load" = "No distraction")

## Accuracy graph

## No distraction conditions
## Convert long data to wide data
nd <- highacc[highacc$Distraction == "No distraction", 
              c('Subject', 'Congruency', 'percentAcc')]
nd <- reshape(nd, direction = "wide", timevar = "Congruency",idvar = "Subject")
nd <- nd[,2:3]
colnames(nd) <- c("Congruent", "Incongruent")

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
nd <- cm.ci(nd)

## Distraction conditions
## Convert long data to wide data
d <- highacc[highacc$Distraction == "Distraction", 
             c('Subject', 'Congruency', 'percentAcc')]
d <- reshape(d, direction = "wide", timevar = "Congruency",idvar = "Subject")
d <- d[,2:3]
colnames(d) <- c("Congruent", "Incongruent")

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
d <- cm.ci(d)

## Combine
percentAcc <- cbind(rep(c("No Distraction", "Distraction"), each = 2), 
                    rbind(nd, d))
colnames(percentAcc) <- c("Distraction", "Congruency", "lower", "av", "upper")

## Define Figure
highaccplot <- 
  ggplot(percentAcc, aes(x = Distraction, y = av, fill = Congruency))+
  geom_bar(position=position_dodge(),
           stat="identity",
           colour="black",
           size=1.5)+
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = .3,
    size = 1.5,
    position = position_dodge(.9))+
  scale_x_discrete("Distraction Condition")+
  scale_y_continuous("Mean Accuracy (%)", expand=c(0,0), 
                     breaks=seq(90, 100, by=2))+
  coord_cartesian(ylim=c(90,101))+
  theme_APA

## Grayscale colour scheme
highaccplot <- highaccplot + scale_fill_grey(start = .3, end = .7)
print(highaccplot)

ggsave(filename = "highaccplot.jpg", plot = highaccplot, width = 15, 
       height = 15, units = "cm")


## Reaction times ----

## Subset correct test trials
hightestRT <- subset(test, Acc == 1)

## Mean RTs per condition 
## (already excluded accuarcy ## below 80% in acc analysis)
highoveralltestRT <- ddply(
  hightestRT,
  c('Congruency', 'Load'),
  summarise,
  meanRT = mean(RT)
)

## Mean RTs per participant and condition 
## (already excluded accuarcy ## below 80% in acc analysis)
hightestRT <- ddply(
  hightestRT,
  c('Subject', 'Congruency', 'Load'),
  summarise,
  meanRT = mean(RT)
)

## Remove participants that had no correct responses in at least one condition
temp <- as.data.frame(table(hightestRT$Subject))
temp <- temp[temp$Freq == 4,]
testRT <- subset(hightestRT, Subject %in% temp$Var1)

## Factorise columns for ezANOVA
cols <- c("Subject", "Congruency", "Load")
hightestRT[cols] <- lapply(hightestRT[cols], factor)

## Test RT ANOVA (already excluded accuarcy ## below 80% in acc analysis)
highrt.ez <- ezANOVA(
  data     = hightestRT,
  wid      = Subject,
  dv       = meanRT,
  within   = .(Congruency, Load),
  detailed = T,
  type     = 3
)
print('RT ANOVA: Congruency x Load')
print(highrt.ez)

print('Mean, SEMs for main effect of Load')
summarySE(hightestRT, measurevar = 'meanRT', groupvars = 'Load')

print('Mean, SEMs for main effect of Congruency')
summarySE(hightestRT, measurevar = 'meanRT', groupvars = 'Congruency')

print('Means, SEMs per condition')
summarySE(hightestRT, measurevar = 'meanRT', groupvars = c('Load', 
                                                           'Congruency'))


## Bayes Factor analysis ------------------------------------------------

## Factorise columns for anovaBF

hightestRT$Congruency <- factor(hightestRT$Congruency)
hightestRT$Load <- factor(hightestRT$Load)
hightestRT$Subject <- factor(hightestRT$Subject)

library(BayesFactor)

anovaBF(formula = meanRT ~ Congruency + Load + Subject,
        data = data.frame(hightestRT),
        whichRandom = "Subject")

hightestRT %>% group_by(Congruency) %>% summarise(mean(meanRT))


## List of pairwise comparisons for t-tests
pairwise <- list(hightestRT)

## Function for pairwise comparisons
loadtest <- function (p){
  ## t-test
  tt <- t.test(meanRT ~ Load, paired = T, data = p)
  ## Cohen's d_z
  dz <- p[,c("Subject", "Load", "meanRT")]  
  colnames(dz) <- c('id', 'cond', 'dv')
  cd <- cohen.dz(dz)
  ret <- list(tt, cd)
}

## Apply function to each item in list
print("Pairwise comparisons")
lapply(pairwise, loadtest)


library(pwr)     # provides sample size needed for power

## Power test (replace d value with calculated cohen d (or mean d)
pwr.t.test(d=0.63,n=47,sig.level=0.05,type="paired",alternative="two.sided")



## RT graph

# Renaming factor levels dplyr
library(dplyr)
hightestRT$Distraction <- recode_factor(hightestRT$Load, "Load" = "Distraction", 
                                     "No Load" = "No distraction")

## No distraction conditions
## Convert long data to wide data
nd <- hightestRT[hightestRT$Distraction == "No distraction", 
                 c('Subject', 'Congruency', 'meanRT')]
nd <- reshape(nd, direction = "wide", timevar = "Congruency",idvar = "Subject")
nd <- nd[,2:3]
colnames(nd) <- c("Congruent", "Incongruent")

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
nd <- cm.ci(nd)

## Distraction conditions
## Convert long data to wide data
d <- hightestRT[hightestRT$Distraction == "Distraction", 
                c('Subject', 'Congruency', 'meanRT')]
d <- reshape(d, direction = "wide", timevar = "Congruency",idvar = "Subject")
d <- d[,2:3]
colnames(d) <- c("Congruent", "Incongruent")

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
d <- cm.ci(d)

## Combine
rtmeans <- cbind(rep(c("No Distraction", "Distraction"), each = 2), rbind(nd, d))
colnames(rtmeans) <- c("Distraction", "Congruency", "lower", "av", "upper")

## Define Figure
highrtplot <- 
  ggplot(rtmeans, aes(x = Distraction, y = av, fill = Congruency))+
  geom_bar(position=position_dodge(),
           stat="identity",
           colour="black",
           size=1.5)+
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = .3,
    size = 1.5,
    position = position_dodge(.9))+
  scale_x_discrete("Distraction Condition")+
  scale_y_continuous("Mean Reaction Time (ms)", expand=c(0,0))+
  coord_cartesian(ylim=c(450,750))+
  theme_APA

## Grayscale colour scheme
highrtplot <- highrtplot + scale_fill_grey(start = .3, end = .7)
print(highrtplot)

ggsave(filename = "highrtplot.jpg", plot = highrtplot, width = 15, 
       height = 15, units = "cm")


