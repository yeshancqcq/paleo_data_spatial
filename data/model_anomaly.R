library(plyr)
library(readr)

model_data <- read_csv("Feng_model_new.csv")
View(model_data)

df <- data.frame(FID=1:nrow(model_data), area=model_data$area,weight=model_data$a_weight,PageName=model_data$PageName, a_base=NA)

for(row in 1:nrow(model_data)){
  # In the raw data: t8000 is the 85th column and t12000 is the 125th column
  sum <- 0
  count <- 0
  for(col in 85:125){
    sum <- sum + model_data[row,col]
    count <- count + 1
  }
  mean <- sum/count
  df$a_base[row] <- mean
  cat("Processing row ", row, " of 2592; ")
}
df$a_base <- vapply( df$a_base, paste, collapse = ", ", character(1L))

output_df <- model_data

for(row in 1:nrow(output_df)){
  for(col in 6:ncol(output_df)){
    output_df[row,col]<-model_data[row,col] - df$a_base[row]
    cat("Processing row ", row, " of 2592; ")
  }
}

write.csv(output_df, file = "Feng_model_anomaly.csv")
