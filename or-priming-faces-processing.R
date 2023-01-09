## Title: or-priming-faces processing
## Author: Tina Seabrooke

## Set up ----
rm(list=ls())

## Load libraries
library(rprime) # Provides 'read_eprime'
library(plyr)   # Provides 'revalue'

## Get data ----
data <- data.frame()

## Get files
filenames <- list.files(path =  "data", pattern = ".txt", full.names = T)

## Loop through all files 
for (p in 1:length(filenames)){
  ppt_data <- read_eprime(filenames[p])
  ppt_data <- FrameList(ppt_data)
  ppt_data <- filter_out(ppt_data, "Running", values = "BlockList")
  ppt_data <- to_data_frame(ppt_data)
  ppt_data <- ppt_data[-nrow(ppt_data),] # remove last (useless) row of df
  
  ## Get demographic info
  subj  <- ppt_data$Subject[1]
  expt  <- ppt_data$Experiment[1]
  sex   <- ppt_data$Sex[1]
  age   <- ppt_data$Age[1]
  stimCon <- ppt_data$StimCondition[1]
  outCon  <- ppt_data$OutcomeCondition[1]
  respCon <- ppt_data$ResponseCondition[1]
  
  ppt_data <- ppt_data[-1, c(
    'Running', 'Stimulus', 'Outcome', 'Stim.ACC', 'Stimulus.RT',
    'Points', 'Congruency', 'Load', 'Discard', 
    'OutcomeResp', 'DelayResp', 'FaceResp', 'Response.RT', 'Response.ACC',
    'Resp'
  )]
  
  ppt_data <- data.frame('Subject'=subj, 'Experiment'=expt, 'Sex'=sex, 
                         'Age'=age, 'StimCond'= stimCon, 'OutcomeCond'=outCon,
                         'RespCond'=respCon, ppt_data)
  
  ## Collapse across training and test columns
  ppt_data[ppt_data$Running == "Training",
           c("Response.RT", "Response.ACC")] <-
    ppt_data[ppt_data$Running == "Training",
             c("Stimulus.RT", "Stim.ACC")]
  
  ppt_data[ppt_data$Running == "PracticeTraining",
           c("Response.RT", "Response.ACC")] <-
    ppt_data[ppt_data$Running == "PracticeTraining",
             c("Stimulus.RT", "Stim.ACC")]
  
  ## Remove duplicate columns
  remove <- which(colnames(ppt_data) %in%
                    c("Stimulus.RT", "Stim.ACC"))
  ppt_data <- subset(ppt_data, select=-remove)
  
  ## Combine data
  data <- rbind(data, ppt_data)
  
  
}

## Summarise error types on test
data$Error[data$Running == 'Test' & data$OutcomeResp == 'Yes'
           ] <- 'OutcomeResp'
data$Error[data$Running == 'PracticeTest' & data$OutcomeResp == 'Yes'
           ] <- 'OutcomeResp'

data$Error[data$Running == 'Test' & data$DelayResp == 'Yes'
           ] <- 'DelayResp'
data$Error[data$Running == 'PracticeTest' & data$DelayResp == 'Yes'
           ] <- 'DelayResp'

data$Error[data$Running == 'Test' & data$Discard == 'True' & (
  data$Resp == 'R1' | data$Resp == 'R2')] <- 'TooSlow'
data$Error[data$Running == 'PracticeTest' & data$Discard == 'True' & (
  data$Resp == 'R1' | data$Resp == 'R2')] <- 'TooSlow'

data$Error[data$Running == 'Test' & data$FaceResp == 'Yes'] <- 'FaceResp'
data$Error[data$Running == 'PracticeTest' & data$FaceResp == 'Yes'
           ] <- 'FaceResp'

drop <- c("OutcomeResp","DelayResp", "FaceResp")
data <- data[,!(names(data) %in% drop)]

## Re-order columns
data <- data[,c(1:8,12:13,9:10,17,15,16,11,14,18)]

colnames(data)[which(colnames(data) %in%
                       c("Response.RT", "Response.ACC"))] <- c("RT", "Acc")

write.csv(data, 'or-priming-faces-data.csv', row.names = F)

