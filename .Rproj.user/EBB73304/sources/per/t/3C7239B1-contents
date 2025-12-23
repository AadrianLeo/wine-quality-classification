## Wine Quality Project - Data Preparation Script

## Step 0 — Package setup

## Run this line once manually if "here" is not installed:
## install.packages("here")

library(here)

## Step 1 — Load the raw data

# Path to the dataset (red wine quality)
data_path <- here("data", "winequality-red.csv")

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

