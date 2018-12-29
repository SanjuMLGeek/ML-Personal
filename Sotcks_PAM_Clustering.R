################################### PAM ####################################################

# Function to check whether package is installed
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 

packages <- list("sparklyr","aws.s3","ISLR", "dplyr", "cluster", "Rtsne", "cluster","lme4","plyr","magrittr","purrr","bindrcpp","zoo","RCurl","jsonlite","tibble","xts","devtools","quantmod","rJava","mailR","htmlTable","caret","AppliedPredictiveModeling","forecast","e1071","TTR","DMwR","neuralnet","caTools")

for (i in packages){
  # check if package "hydroGOF" is installed
  if (!is.installed(i)){
    install.packages(i)
    print(paste0("Package-",i,"-Now-Installed"))
  }
  else {
    
    print(paste0("Package-",i,"-Is Already Installed"))
  }
}

install.packages("aws.s3")

install.packages('RCurl') 

library(sparklyr) # for S3
library(aws.s3) # for S3
library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(plyr)
library(magrittr)
library(purrr)
library(bindrcpp)
library(zoo)
library(RCurl)
library(jsonlite)
library(tibble)
library(xts)
library(devtools)
library(quantmod)
library(rJava)
library(mailR)
library(htmlTable)
library(lubridate)
library(caret)
library(forecast)
library(AppliedPredictiveModeling)
library(e1071)
library(TTR)
library(DMwR)
library(neuralnet)
library(caTools)

set.seed(101)


if (exists('combined_stock_option_final')){
  print("combined_stock_option_final Data Frame Exists")
  combined_stock_option_final_no_na <- na.omit(combined_stock_option_final)
  write.csv(combined_stock_option_final_no_na,"/Users/sanjaykukadiya/Desktop/stocks/Outputs/combined_stock_option_final_no_na.csv",row.names=FALSE, na="")
  
  
} else {
  print(" combined_stock_option_final Data Frame Doesn't Exists")
  
  output_folder <- "/Users/sanjaykukadiya/Desktop/stocks/Outputs"
  setwd(output_folder)
  print(getwd())
  
  combined_stock_option_final_file <- list.files(pattern = 'combined_stock_option_final.csv')
  combined_stock_option_temp <- do.call(rbind,lapply(combined_stock_option_final_file,read.csv))
  combined_stock_option_temp$actdate <- as.Date(as.character(combined_stock_option_temp$actdate), format = "%Y-%m-%d")
  combined_stock_option_temp$signal <- as.factor(combined_stock_option_temp$signal)
  
  combined_stock_option_final <- combined_stock_option_temp
  combined_stock_option_final_no_na <- na.omit(combined_stock_option_final)
  write.csv(combined_stock_option_final_no_na,"/Users/sanjaykukadiya/Desktop/stocks/Outputs/combined_stock_option_final_no_na.csv",row.names=FALSE, na="")
  
} 


max_stock_date <- as.Date("2018-12-24")
year_old_date <- max_stock_date - as.difftime(365,units = "days")
set_100_train_date <- max_stock_date - as.difftime(100,units = "days")

# Remove stock name and date before clustering

trainPAM <- combined_stock_option_final_no_na %>% filter(actdate >= set_100_train_date) %>% select(underlying,actdate,Open,High,Low,Close,Volume,Change,Daily_raw_put_call_ratio,SMA_10_put_call_ratio,Total_roll_avg_30,Call_roll_avg_30,Put_roll_avg_30,SMA200,SMA120,SMA80,SMA50,SMA20,SMA10,RSI200,RSI120,RSI80,RSI50,RSI20,RSI10,ATR,MACD,OBV,BB_SMA,BB_PctB)
glimpse(trainPAM)

# Calculate Grower Distance
gower_dist <- daisy(trainPAM[,c(-1,-2)],metric = "gower",type = list(logratio = 3))


# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair

trainPAM[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),arr.ind = TRUE)[1, ], ]
trainPAM[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),arr.ind = TRUE)[1, ], ]

#Calculate Width
sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


# run Actual PAM Algorithm

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- trainPAM %>%
  mutate(cluster = factor(pam_fit$clustering)) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


# t-Distributed Stochastic Neighbourhood embedding
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = college_clean$name)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
