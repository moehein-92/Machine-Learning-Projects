library(readr)

data = read.csv("Ames_data.csv", stringsAsFactors = FALSE)
testIDs = read.table("project1_testIDs.dat")


for (j in 1:10){
  
  cat("======Split=====", j, "================", '\n')
  
  train <- data[-testIDs[,j], ]
  test <- data[testIDs[,j], ]
  test_y <- test[, c(1, 83)]
  test <- test[, -83]
  
  write.csv(train,"train.csv",row.names=FALSE)
  write.csv(test, "test.csv",row.names=FALSE)
  write.csv(test_y,"test_y.csv",row.names=FALSE)
  
  #run mymain.r
  source("mymain.R")
  
  test.y = read.csv('test_y.csv')
  names(test.y)[2] = "True_Sale_Price"
  
  pred = read.csv('mysubmission1.txt')
  pred = merge(pred, test.y, by = "PID")
  rmse1 = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  
  print(paste0("model - 1 RMSE: ", rmse1))
  
  pred = read.csv('mysubmission2.txt')
  pred = merge(pred, test.y, by = "PID")
  rmse2 = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  
  print(paste0("model - 2 RMSE: ", rmse2))
  
}



