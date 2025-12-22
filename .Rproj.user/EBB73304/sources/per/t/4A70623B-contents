---
  title: "Wine Quality (Red) — Step 1 & Step 2"
author: "Shruti Pashine"
date: "`r format(Sys.Date(), '%d %B %Y')`"
output: html_document
---
  
  \`\`\`{r setup, include=FALSE} knitr::opts_chunk\$set( echo = TRUE, message = FALSE, warning = FALSE )

# Install once if needed:

# install.packages("here")

library(here) \# Path to the dataset

data_path \<- here("data", "winequality-red.csv")

# Load dataset (semicolon-separated)

wine \<- read.csv2(data_path) dim(wine) nrow(wine) ncol(wine)

str(wine) summary(wine) head(wine)

#Checking Missing Values colSums(is.na(wine))

#Distribution of original quality scores table(wine$quality)
prop.table(table(wine$quality))

#Histogram of Quality hist( wine\$quality, main = "Distribution of Wine Quality Scores", xlab = "Quality", col = "lightblue", breaks = 10 )

#Data Type str(wine\$quality)

wine$quality  <- as.numeric(as.character(wine$quality)) wine$alcohol  <- as.numeric(as.character(wine$alcohol))

str(wine$quality)
str(wine$alcohol)

#Boxplot: Alchol Vs Quality boxplot( alcohol \~ quality, data = wine, main = "Alcohol Content vs Wine Quality", xlab = "Quality", ylab = "Alcohol" )

#Data Cleaning wine$quality_binary <- ifelse(wine$quality \>= 7, 1, 0)

wine$quality_binary <- factor(
  wine$quality_binary, levels = c(0, 1), labels = c("Poor", "Premium") )

#Check class Distribution table(wine$quality_binary)
prop.table(table(wine$quality_binary))

#Remove original quality variable (keep only engineered target) wine_clean \<- wine wine_clean\$quality \<- NULL str(wine_clean) summary(wine_clean)

#Save Clean Dataset \# Create data folder if it doesn't exist

dir.create(here("data"), showWarnings = FALSE)

# Save cleaned dataset

write.csv( wine_clean, here("data", "winequality-red-clean.csv"), row.names = FALSE )
