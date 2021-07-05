library(meta)
library(metafor)
library(readxl)
library(dplyr)

setwd('/Users/khanh94/Downloads')

#Read the data
data <- read_excel('Complete_Data.xlsx')

#Columns to be selected
selected <- c('Study ID',
              'Title',
              "Country in which the study was conducted",
              'Fluid Type',
              'Number of Markers',
              'CA19-9 or Novel',
              'Biomarker Name',
              'Biomarker Type',
              'Cohorts', 'Number patients in PDAC cohort', 'AUC', 'Calculated SE', 
              'na', 'nn', 'Q1', 'Q2'
)

#Filter the data and change the datatypes of the relevant columns
data2 <- data %>% 
  filter(AUC > 0.5) %>%
  filter(`CA19-9 or Novel` == 'CA19-9') %>%
  select(which(colMeans(is.na(.)) < 0.25)) %>% 
  select(selected) %>% 
  mutate_at('Number of Markers', as.numeric)

#Convert all string columns to factors
data2_factor <- data2 %>% 
  mutate_if(sapply(data2, is.character), as.factor)

#Generate the metaanalysis
m.gen_results <- metagen(TE = data2_factor$AUC,
                     seTE = data2_factor$"Calculated SE",
                     lower = Q1,
                     upper = Q2,
                     data = data2_factor,
                     studlab = data2_factor$"Study ID",
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     title = "Biomarker MetaAnalysis")

  #select(where(is.double))

#data_nonzeroAUC <- data[which(data$AUC > 0),]
