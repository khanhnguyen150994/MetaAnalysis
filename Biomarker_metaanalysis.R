if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")

library(meta)
library(dmetar)
library(metafor)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('/Users/khanh94/Documents/MetaAnalysis')

#Read the data
data <- read_excel('Complete_Data.xlsx')

#Columns to be selected
selected <- c('Study ID',
              'Title',
              "Country in which the study was conducted",
              'Fluid Type',
              'Corresponding Author Name', 
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
  filter(`Corresponding Author Name` != 'NA') %>%
  select(which(colMeans(is.na(.)) < 0.25)) %>% 
  select(selected) %>% 
  mutate_at('Number of Markers', as.numeric) 

#Convert all string columns to factors
data2_CA19 <- data2 %>% 
  mutate_if(sapply(data2, is.character), as.factor)

#Generate the metaanalysis for the markers containing CA19-9 only
m.gen_results <- metagen(TE = data2_CA19$AUC,
                     seTE = data2_CA19$"Calculated SE",
                     lower = Q1,
                     upper = Q2,
                     data = data2_CA19,
                     studlab = data2_CA19$"Study ID",
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     title = "Biomarker MetaAnalysis")


#Set the IDs for the rows in R and calculate the within group variance
data2_CA19$es.id <- as.factor(rownames(data2_CA19))
data2_CA19_variance <- as.data.frame(apply(data2_CA19[,12], 2, function(x) tapply(x, data2_CA19$`Study ID`, var)))
colnames(data2_CA19_variance)[1] <- "AUC_var"

#Prepare the dataframe for multilevel metaanalysis with within group variance
data2_CA19_variance[is.na(data2_CA19_variance)] = 0.0000000001
data2_CA19_variance$'Study ID' <- rownames(data2_CA19_variance)
data2_CA19_meta <- left_join(data2_CA19, data2_CA19_variance)
colnames(data2_CA19_meta)[1] <- 'Study_ID'
colnames(data2_CA19_meta)[5] <- 'Author'


#Perform the multilevel metaanalysis based on title and new ID
full.model <- rma.mv(yi = AUC, 
                     V = AUC_var, 
                     slab = Author, 
                     data = data2_CA19_meta,
                     random = ~ 1 | Author/es.id, 
                     test = "t",
                     method = "REML")


i2 <- var.comp(full.model)
plot(i2)

#Perform the comparison with a two-level model
l3.removed <- rma.mv(yi = AUC, 
                     V = AUC_var, 
                     slab = Author,
                     data = data2_CA19_meta,
                     random = ~ 1 | Author/es.id, 
                     test = "t", 
                     method = "REML",
                     sigma2 =  c(0, NA))

summary(l3.removed)
anova(full.model, l3.removed)

#Perform subgroup analyses in three-level models to assess moderators
mod.model <- rma.mv(yi = AUC, V = AUC_var, 
                    slab = Author, data = data2_CA19_meta,
                    random = ~ 1 | Author/es.id, 
                    test = "t", method = "REML",
                    mods = ~ `Biomarker Type`)

summary(mod.model)