## Logistic Regression Model (ML1) — Wine Quality
## Uses cleaned data produced in your wine.Rmd

# Packages
install.packages(c("here","caret","pROC","dplyr", "tidyverse"))
library(here)
library(caret)
library(pROC)
library(tidyverse) 
library(dplyr)

set.seed(123)

# 1) Load cleaned data
data_path <- here("data/winequality-red-clean.csv")
wine_model <- read.csv(data_path)

# Ensure target is factor with correct positive class
wine_model$quality_binary <- factor(wine_model$quality_binary,
                                    levels = c("Poor", "Premium"))

# Drop quality_factor if it exists (you already do this)
if ("quality_factor" %in% names(wine_model)) {
  wine_model$quality_factor <- NULL
}

# Sanity checks
stopifnot(all(sapply(wine_model %>% select(-quality_binary), is.numeric)))
stopifnot(!anyNA(wine_model))

# 2) Train/Validation/Test Split (60/20/20)
idx_train <- createDataPartition(wine_model$quality_binary, p = 0.60, list = FALSE)
train <- wine_model[idx_train, ]
temp  <- wine_model[-idx_train, ]

idx_valid <- createDataPartition(temp$quality_binary, p = 0.50, list = FALSE)
valid <- temp[idx_valid, ]
test  <- temp[-idx_valid, ]

# 3) Fit Logistic Regression (GLM binomial)
log_model <- glm(quality_binary ~ .,
                 data = train,
                 family = binomial)

summary(log_model)

# 4) Predict probabilities
valid_prob <- predict(log_model, newdata = valid, type = "response")
test_prob  <- predict(log_model, newdata = test,  type = "response")

# 5) Choose threshold using Validation set (Youden’s J from ROC)
roc_valid <- roc(valid$quality_binary, valid_prob, levels = c("Poor","Premium"), direction = "<")
best <- coords(roc_valid, x = "best", best.method = "youden", transpose = FALSE)
best_thresh <- as.numeric(best["threshold"])
best_thresh

# 6) Class predictions using chosen threshold
valid_pred <- factor(ifelse(valid_prob >= best_thresh, "Premium", "Poor"),
                     levels = c("Poor","Premium"))
test_pred  <- factor(ifelse(test_prob  >= best_thresh, "Premium", "Poor"),
                     levels = c("Poor","Premium"))

# 7) Evaluation (Validation)
confusionMatrix(valid_pred, valid$quality_binary, positive = "Premium")

auc_valid <- auc(roc_valid)
auc_valid

# 8) Final Evaluation (TEST ONLY) — this is what you report for comparison
roc_test <- roc(test$quality_binary, test_prob, levels = c("Poor","Premium"), direction = "<")
auc_test <- auc(roc_test)
auc_test

confusionMatrix(test_pred, test$quality_binary, positive = "Premium")

# 9) Optional: ROC plot (test)
plot(roc_test, main = "Logistic Regression ROC (Test Set)")

