library(readr)
TS <- read_csv("TS.csv", col_names = FALSE)
View(TS)

df_ts <- data.frame(pagenumber=1:2592, pagename="")

# Looping through the csv file (output from Panoply)
temp <- array()
time <- 22000
for (row in 1:nrow(TS)){
  if(is.na(TS[row,1])==0){
    for(col in 1:72){
      temp <- c(temp,TS[row,col])
    }
  } else{
    temp=temp[-1]
    df_ts$new <- temp
    df_ts$new <- vapply(df_ts$new, paste, collapse = ", ", character(1L))
    colnames(df_ts)[ncol(df_ts)] = toString(time)
    cat("finishing ", time,"; ")
    time <- time - 100
    temp <- array()
  }
}
# Adding the last column t = 100 ka
temp=temp[-1]
df_ts$new <- temp
df_ts$new <- vapply(df_ts$new, paste, collapse = ", ", character(1L))
colnames(df_ts)[ncol(df_ts)] = toString(time)
cat("finishing ", time,"; ")

write.csv(df_ts, file = "Feng_model_new.csv")
