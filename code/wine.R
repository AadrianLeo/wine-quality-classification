
## Wine Quality Project - Data Preparation Script

## Step 0 — Package setup

## Run this line once manually if "here" is not installed:
install.packages("here")

library(here)
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

## Step 1 — Load the raw data

# Path to the dataset (red wine quality)
data_path <- here("data/winequality-red.csv")

# Load the dataset (semicolon-separated file)
wine <- read.csv2(data_path)

## Basic structure and sanity checks
dim(wine)
str(wine)
nrow(wine)
ncol(wine)
summary(wine)
head(wine)

# Check missing values per column
colSums(is.na(wine))

# Distribution of original quality scores
table(wine$quality)
prop.table(table(wine$quality))

hist(
  wine$quality,
  main = "Distribution of Wine Quality Scores",
  xlab = "Quality",
  col = "lightblue",
  breaks = 10
)

# Ensure numeric types for key variables
wine$quality <- as.numeric(as.character(wine$quality))
wine$alcohol <- as.numeric(wine$alcohol)

boxplot(
  alcohol ~ quality,
  data = wine,
  main = "Alcohol Content vs Wine Quality",
  xlab = "Quality",
  ylab = "Alcohol"
)

str(wine)
wine$quality_factor <- as.factor(wine$quality)
str(wine)

## Step 2 — Data cleaning & target engineering

# 1. Create binary target variable
#    Premium (1): quality >= 7
#    Poor    (0): quality <= 6
wine$quality_binary <- ifelse(wine$quality >= 7, 1, 0)

# Convert target to factor (classification task)
wine$quality_binary <- factor(
  wine$quality_binary,
  levels = c(0, 1),
  labels = c("Poor", "Premium")
)

# 2. Check class distribution
table(wine$quality_binary)
prop.table(table(wine$quality_binary))

# 3. Remove original quality variable
wine_clean <- wine
wine_clean$quality <- NULL

numeric_cols <- c(
  "fixed.acidity", "volatile.acidity", "citric.acid",
  "residual.sugar", "chlorides",
  "free.sulfur.dioxide", "total.sulfur.dioxide",
  "density", "pH", "sulphates", "alcohol"
)

wine_clean[numeric_cols] <- lapply(wine_clean[numeric_cols], as.numeric)
# 4. Final structure check
str(wine_clean)
summary(wine_clean)

## Step 3 — Save cleaned dataset to a new CSV file

# Create data folder if it doesn't exist
dir.create(here("data"), showWarnings = FALSE)

# Save cleaned wine dataset
write.csv(
  wine_clean,
  here("data", "winequality-red-clean.csv"),
  row.names = FALSE
)

## Step 4 — Additional quality checks

# Check missing values per column in cleaned data
colSums(is.na(wine_clean))

# Number of duplicate rows
sum(duplicated(wine_clean))

str(wine_clean)

boxplot(
  wine_clean$alcohol,
  main = "Boxplot of Alcohol (Outlier Check)",
  ylab = "Alcohol"
)

# Before vs after cleaning
dim(wine)        # original

dim(wine_clean)  # cleaned

summary(wine)
summary(wine_clean)

# Original quality distribution
table(wine$quality)

# Binary target distribution
table(wine_clean$quality_binary)
prop.table(table(wine_clean$quality_binary))

# View all duplicated rows (excluding first occurrence)
duplicate_rows <- wine_clean[duplicated(wine_clean), ]
nrow(duplicate_rows)
head(duplicate_rows)

# Row indices of duplicated rows
which(duplicated(wine_clean))


head(wine_clean)




#______EDA___________________

summary(wine_clean)
# wine_clean$quality_binary <- as.factor(wine_clean$quality_binary)

str(wine_clean)
#All chemical predictor variables were converted from character to numeric to enable proper analysis and model training. 
#The target variable quality_binary was converted to a factor with two levels, "Poor" and "Premium", for classification.
#___________________________________

#sum(duplicated(wine_clean))
#wine_clean <- unique(wine_clean)

#sum(duplicated(wine_clean)) 
#nrow(wine_clean)   

#checking and removing duplicate values.

#___________________________________
table(wine_clean$quality_binary)
prop.table(table(wine_clean$quality_binary))

library(ggplot2)
ggplot(wine_clean, aes(x = quality_binary)) +
  geom_bar(fill = "orange") +
  ggtitle("Distribution of Wine Quality") +
  xlab("Quality") + ylab("Count")

#The target variable quality_binary shows a strong class imbalance. 
#Out of 1599 wines, 1382 (86%) are labeled Poor, while only 217 (14%) are labeled Premium. 
#This indicates that Premium wines are less frequent,
#which should be considered when evaluating model performance.

#____________________________
summary(wine_clean)




#____________________________
library(ggplot2)
library(dplyr)
library(corrplot)

#Pairplot / Correlation plot
#__________________________
library(corrplot)
cor_mat <- cor(wine_clean[, numeric_cols])
corrplot(cor_mat, method = "color", type = "upper")



par(mfrow = c(2, 3)) 

# Density vs Fixed Acidity
r1 <- round(cor(wine_clean$`fixed.acidity`, wine_clean$density), 2)
plot(wine_clean$`fixed.acidity`, wine_clean$density,
     main = paste("Density vs Fixed Acidity (r =", r1, ")"),
     xlab = "Fixed Acidity", ylab = "Density",
     col = "orange", pch = 16)

# Citric Acid vs Fixed Acidity
r2 <- round(cor(wine_clean$`fixed.acidity`, wine_clean$`citric.acid`), 2)
plot(wine_clean$`fixed.acidity`, wine_clean$`citric.acid`,
     main = paste("Citric Acid vs Fixed Acidity (r =", r2, ")"),
     xlab = "Fixed Acidity", ylab = "Citric Acid",
     col = "orange", pch = 16)

# pH vs Fixed Acidity
r3 <- round(cor(wine_clean$`fixed.acidity`, wine_clean$pH), 2)
plot(wine_clean$`fixed.acidity`, wine_clean$pH,
     main = paste("pH vs Fixed Acidity (r =", r3, ")"),
     xlab = "Fixed Acidity", ylab = "pH",
     col = "brown", pch = 16)

# Alcohol vs Density
r4 <- round(cor(wine_clean$density, wine_clean$alcohol), 2)
plot(wine_clean$density, wine_clean$alcohol,
     main = paste("Alcohol vs Density (r =", r4, ")"),
     xlab = "Density", ylab = "Alcohol",
     col = "brown", pch = 16)

# Alcohol vs Fixed Acidity
r5 <- round(cor(wine_clean$`fixed.acidity`, wine_clean$alcohol), 2)
plot(wine_clean$`fixed.acidity`, wine_clean$alcohol,
     main = paste("Alcohol vs Fixed Acidity (r =", r5, ")"),
     xlab = "Fixed Acidity", ylab = "Alcohol",
     col = "blue", pch = 16)

# Alcohol vs Citric Acid
r6 <- round(cor(wine_clean$`citric.acid`, wine_clean$alcohol), 2)
plot(wine_clean$`citric.acid`, wine_clean$alcohol,
     main = paste("Alcohol vs Citric Acid (r =", r6, ")"),
     xlab = "Citric Acid", ylab = "Alcohol",
     col = "blue", pch = 16)


par(mfrow = c(1, 1)) 
#______________________________________

#pH distribution
hist(wine_clean$pH,
     main = "Distribution of pH in Red Wine",
     xlab = "pH",
     col = "lightblue",
     border = "black",
     breaks = 15)  # number of bins

#___________________________________________
#alcohol distribution
hist(wine_clean$alcohol,
     main = "Distribution of Alcohol Content",
     xlab = "Alcohol (%)",
     col = "lightgreen",
     border = "black",
     breaks = 15)

#___________________________________________
#Pie chart of quality_factor 
q_table <- table(wine_clean$quality_factor)
q_percent <- round(prop.table(q_table) * 100, 1)
labels <- paste(names(q_table), "-", q_percent, "%")


pie(q_table,
    labels = labels,
    main = "Wine Quality Score Distribution",
    col = rainbow(length(q_table)))
#____________________________________________
#Box plot of quality factor vs alcohol
boxplot(alcohol ~ quality_factor,
        data = wine_clean,
        main = "Alcohol Content by Wine Quality",
        xlab = "Wine Quality Score",
        ylab = "Alcohol (%)",
        col = "lightblue")
#___________________________________________

#Groupby Quality vs other columns


cols <- c("fixed.acidity", "volatile.acidity", "citric.acid",
          "residual.sugar", "chlorides", "free.sulfur.dioxide",
          "total.sulfur.dioxide", "density", "pH",
          "sulphates", "alcohol")


agg_df <- wine_clean %>%
  group_by(quality_factor) %>%
  summarise(
    across(all_of(cols), mean, na.rm = TRUE),
    .groups = "drop"
  )

agg_long <- agg_df %>%
  pivot_longer(
    cols = all_of(cols),
    names_to = "Variable",
    values_to = "Frequency"
  )

ggplot(
  agg_long,
  aes(
    x = factor(quality_factor),
    y = Frequency,
    fill = Variable
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  
  labs(
    title = "Grouped Bar Chart of Chemical Variables by Wine Quality",
    x = "Quality Factor",
    y = "Total Frequency / Amount"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

#_____________________

# Get unique quality factors
qualities <- sort(unique(wine_clean$quality_factor))
n <- length(qualities)

# Set up plotting grid (e.g., 2 rows, 3 columns for 6 qualities)
par(mfrow = c(2, 3),      # adjust depending on number of qualities
    mar = c(4, 4, 2, 1))  # margins: bottom, left, top, right

# Loop over each quality factor
for(q in qualities) {
  subset_df <- wine_clean[wine_clean$quality_factor == q, ]
  
  # Scatter plot
  plot(subset_df$total.sulfur.dioxide, subset_df$free.sulfur.dioxide,
       main = paste("Quality", q),
       xlab = "Total SO₂",
       ylab = "Free SO₂",
       pch = 19, col = "darkgreen",
       cex = 0.7)
  
  # Calculate and add correlation
  r <- cor(subset_df$free.sulfur.dioxide, subset_df$total.sulfur.dioxide)
  legend("topleft", legend = paste("r =", round(r, 2)), bty = "n")
}

# wine$quality_factor <- as.factor(wine$quality)

# Predictor variables will be standardized before training SVM, as SVM is sensitive to feature scaling
# The dataset shows a strong class imbalance, therefore performance evaluation will rely on ROC-AUC and F1-score rather than accuracy alone.


wine_model <- wine_clean
wine_model$quality_factor <- NULL



