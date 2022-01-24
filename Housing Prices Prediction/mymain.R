# load necessary packages
library(readr)
library(glmnet)
library(caret)
library(tibble)
library(dplyr)
library(xgboost)


#================================================#
## Process Training Data
train = read.csv("train.csv", stringsAsFactors = FALSE)

# remove uninterpretable and imbalanced variables
train = subset(train, select = -c(Street, Utilities, Condition_2, Roof_Matl, Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF, Pool_Area, Longitude, Latitude))

# winsorize
winsor_var = c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")

quan_value = 0.95
for(var in winsor_var){
  tmp = train[, var]
  myquan = quantile(tmp, probs = quan_value, na.rm = TRUE)
  tmp[tmp > myquan] = myquan
  train[, var] = tmp
}


# setting up train matrices
train.x = subset(train, select = -c(PID, Sale_Price))
train.y = log(train$Sale_Price)

train.x$Garage_Yr_Blt[is.na(train.x$Garage_Yr_Blt)] = 0

categorical.vars <- colnames(train.x)[which(sapply(train.x,
                                                   function(x)
                                                     mode(x) == "character"))]

train.matrix <- train.x[,!colnames(train.x) %in% categorical.vars,
                        drop = FALSE]

n.train <- nrow(train.matrix)
for (var in categorical.vars) {
  mylevels <- sort(unique(train.x[, var]))
  m <- length(mylevels)
  m <- ifelse(m > 2, m, 1)
  tmp.train <- matrix(0, n.train, m)
  col.names <- NULL
  for (j in 1:m) {
    tmp.train[train.x[, var] == mylevels[j], j] <- 1
    col.names <- c(col.names, paste(var, '_', mylevels[j], sep = ''))
  }
  colnames(tmp.train) <- col.names
  train.matrix <- cbind(train.matrix, tmp.train)
}


#=================================================#

## Process Testing Data
test = read.csv("test.csv",  stringsAsFactors = FALSE)
test_y = read.csv("test_y.csv")

# remove uninterpretable and imbalanced variables
test = subset(test, select = -c(Street, Utilities, Condition_2, Roof_Matl, Heating, Pool_QC, Misc_Feature, Low_Qual_Fin_SF, Pool_Area, Longitude, Latitude))

# winsorize
winsor_var = c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")

quan_value = 0.95
for(var in winsor_var){
  tmp = test[, var]
  myquan = quantile(tmp, probs = quan_value, na.rm = TRUE)
  tmp[tmp > myquan] = myquan
  test[, var] = tmp
}

# setting up test matrices
test.x = subset(test, select = -c(PID))

test.x$Garage_Yr_Blt[is.na(test.x$Garage_Yr_Blt)] = 0

categorical.vars <- colnames(test.x)[which(sapply(test.x,
                                                  function(x)
                                                    mode(x) == "character"))]

test.matrix <- test.x[,!colnames(test.x) %in% categorical.vars,
                      drop = FALSE]

n.test <- nrow(test.matrix)
for (var in categorical.vars) {
  mylevels <- sort(unique(test.x[, var]))
  m <- length(mylevels)
  m <- ifelse(m > 2, m, 1)
  tmp.test <- matrix(0, n.test, m)
  col.names <- NULL
  for (j in 1:m) {
    tmp.test[test.x[, var] == mylevels[j], j] <- 1
    col.names <- c(col.names, paste(var, '_', mylevels[j], sep = ''))
  }
  colnames(tmp.test) <- col.names
  test.matrix <- cbind(test.matrix, tmp.test)
}

#----------------------------------------------------------#

# Drop or add columns between train and test matrices

missing_col_train = setdiff(colnames(test.matrix), colnames(train.matrix))
missing_col_test = setdiff(colnames(train.matrix), colnames(test.matrix))


train.matrix[missing_col_train] = 0

test.matrix[missing_col_test] = 0
test.matrix = test.matrix[colnames(train.matrix)]

dim(train.matrix)
dim(test.matrix)


#===================================================#

# MODEL 1 - LINEAR MODEL

set.seed(12345)

start_time <- Sys.time()

cv_out = cv.glmnet(as.matrix(train.matrix), train.y, alpha = 1)


sel_vars = predict(cv_out, type="nonzero", 
                    s = cv_out$lambda.min)[[1]]

cv_out = cv.glmnet(as.matrix(train.matrix[, sel_vars]), 
                    train.y, alpha = 0)

Ytest_pred = exp(predict(cv_out, s = cv_out$lambda.min, 
              newx = as.matrix(test.matrix[, sel_vars])))

end_time <- Sys.time()
time_elapse = end_time - start_time

print(paste0("time elapse: ", time_elapse))


mysubmission1 = data.frame(PID = test[ ,"PID"], Sale_Price = Ytest_pred)
colnames(mysubmission1) = c("PID", "Sale_Price")
write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE)


# MODEL 2 - XGBOOST

start_time2 <- Sys.time()

xgb.model <- xgboost(data = as.matrix(train.matrix), 
                    label = train.y, max_depth = 6,
                     eta = 0.05, nrounds = 5000,
                     subsample = 0.5,
                     verbose = FALSE)

model_2_pred = exp(predict(xgb.model, newdata = as.matrix(test.matrix)))

end_time2 <- Sys.time()

time_elapse2 = end_time2 - start_time2
print(paste0("time elapse: ", time_elapse2))

mysubmission2 = data.frame(PID = test[ ,"PID"], Sale_Price = model_2_pred)
colnames(mysubmission2) = c("PID", "Sale_Price")
write.table(mysubmission2, file = "mysubmission2.txt", sep = ",", row.names = FALSE)



