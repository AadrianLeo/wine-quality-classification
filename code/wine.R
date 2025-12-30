
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




#EDA

# 1. Disttibution of wine quality
library(ggplot2)
ggplot(wine_clean, aes(x = quality_binary)) +
  geom_bar(fill = "orange") +
  ggtitle("Distribution of Wine Quality") +
  xlab("Quality") + ylab("Count")


library(corrplot)

# 2. Correlation plot
library(corrplot)
cor_mat <- cor(wine_clean[, numeric_cols])
corrplot(cor_mat, method = "color", type = "upper")


# 3.  Density vs Fixed Acidity
par(mfrow = c(2, 3)) 
r1 <- round(cor(wine_clean$`fixed.acidity`, wine_clean$density), 2)
plot(wine_clean$`fixed.acidity`, wine_clean$density,
     main = paste("Density vs Fixed Acidity (r =", r1, ")"),
     xlab = "Fixed Acidity", ylab = "Density",
     col = "steelblue", pch = 16)

# Citric Acid vs Fixed Acidity
r2 <- round(cor(wine_clean$`fixed.acidity`, wine_clean$`citric.acid`), 2)
plot(wine_clean$`fixed.acidity`, wine_clean$`citric.acid`,
     main = paste("Citric Acid vs Fixed Acidity (r =", r2, ")"),
     xlab = "Fixed Acidity", ylab = "Citric Acid",
     col = "steelblue", pch = 16)

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
     col = "pink", pch = 16)

# Alcohol vs Citric Acid
r6 <- round(cor(wine_clean$`citric.acid`, wine_clean$alcohol), 2)
plot(wine_clean$`citric.acid`, wine_clean$alcohol,
     main = paste("Alcohol vs Citric Acid (r =", r6, ")"),
     xlab = "Citric Acid", ylab = "Alcohol",
     col = "pink", pch = 16)

par(mfrow = c(1, 1)) 


# 4. pH distribution
hist(wine_clean$pH,
     main = "Distribution of pH in Red Wine",
     xlab = "pH",
     col = "lightblue",
     border = "black",
     breaks = 15)  # number of bins


# 5. alcohol distribution
hist(wine_clean$alcohol,
     main = "Distribution of Alcohol Content",
     xlab = "Alcohol (%)",
     col = "lightgreen",
     border = "black",
     breaks = 15)


# 6. Pie chart of quality_factor 
q_table <- table(wine_clean$quality_factor)
q_percent <- round(prop.table(q_table) * 100, 1)
labels <- paste(names(q_table), "-", q_percent, "%")


pie(q_table,
    labels = labels,
    main = "Wine Quality Score Distribution",
    col = rainbow(length(q_table)))



# 7. Groupby Quality vs other columns


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
  
  
  scale_fill_manual(values = c(
    "fixed.acidity" = "steelblue",
    "volatile.acidity" = "orange",
    "citric.acid" = "green",
    "residual.sugar" = "red",
    "chlorides" = "purple",
    "free.sulfur.dioxide" = "brown",
    "total.sulfur.dioxide" = "pink",
    "density" = "grey",
    "pH" = "yellow",
    "sulphates" = "cyan",
    "alcohol" = "darkblue"
  )) +
  
  
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
par(mfrow = c(1, 1)) 


# 8. Box plot of quality factor vs alcohol
boxplot(alcohol ~ quality_factor,
        data = wine_clean,
        main = "Alcohol Content by Wine Quality",
        xlab = "Wine Quality Score",
        ylab = "Alcohol (%)",
        col = "lightblue")

wine_model <- wine_clean
wine_model$quality_factor <- NULL



