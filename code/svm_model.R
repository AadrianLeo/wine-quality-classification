## SVM Model (ML2) — Wine Quality (Radial Kernel)
## Uses cleaned data produced in your wine.Rmd

# Packages
# If needed, install packages once in the R console, e.g.:
install.packages(c("here","caret","pROC","dplyr","kernlab"))
library(here)
library(caret)
library(pROC)
library(dplyr)
library(kernlab)  # required for svmRadial in caret

set.seed(123)

# 1) Load cleaned data
data_path <- here("data/winequality-red-clean.csv")
wine_model <- read.csv(data_path)

# Ensure target is factor with correct positive class
wine_model$quality_binary <- factor(wine_model$quality_binary,
                                    levels = c("Poor", "Premium"))

# Drop quality_factor if it exists
if ("quality_factor" %in% names(wine_model)) {
  wine_model$quality_factor <- NULL
}

# Sanity checks
stopifnot(all(sapply(wine_model %>% select(-quality_binary), is.numeric)))
stopifnot(!anyNA(wine_model))

# 2) Train/Validation/Test Split (60/20/20) — same split logic as logistic
idx_train <- createDataPartition(wine_model$quality_binary, p = 0.60, list = FALSE)
train <- wine_model[idx_train, ]
temp  <- wine_model[-idx_train, ]

idx_valid <- createDataPartition(temp$quality_binary, p = 0.50, list = FALSE)
valid <- temp[idx_valid, ]
test  <- temp[-idx_valid, ]

# 3) Fit SVM (Radial) with CV tuning on TRAIN only
# SVM is sensitive to scaling -> preProcess centers/scales predictors
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

svm_grid <- expand.grid(
  sigma = c(0.005, 0.01, 0.02, 0.05),
  C     = c(0.5, 1, 2, 5, 10)
)

svm_model <- train(
  quality_binary ~ .,
  data = train,
  method = "svmRadial",
  preProcess = c("center", "scale"),
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = svm_grid
)

svm_model
svm_model$bestTune

# 4) Predict probabilities
valid_prob <- predict(svm_model, newdata = valid, type = "prob")[, "Premium"]
test_prob  <- predict(svm_model, newdata = test,  type = "prob")[, "Premium"]

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

# 8) Final Evaluation (TEST ONLY)
roc_test <- roc(test$quality_binary, test_prob, levels = c("Poor","Premium"), direction = "<")
auc_test <- auc(roc_test)
auc_test

confusionMatrix(test_pred, test$quality_binary, positive = "Premium")

# 9) Optional: ROC plot (test)
plot(roc_test, main = "SVM (Radial) ROC (Test Set)")
