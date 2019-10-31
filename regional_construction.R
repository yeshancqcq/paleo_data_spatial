library(readr)
library(ggplot2)
library(stats)

model_anomaly <- read_csv("Feng_model_anomaly.csv")
model_anomaly <- model_anomaly[-1]
model_data <- model_anomaly
proxy_data <- read_csv("proxy_anomaly.csv")

proxy_region <- data.frame(
  (matrix(vector(), 220, 22, dimnames=list(
    c(), 
    c("time",
      "new_zealand", 
      "greenland", 
      "n_atlantic", 
      "s_atlantic", 
      "indonesia",
      "japan",
      "oregon",
      "alaska",
      "china",
      "congo",
      "mediterranean",
      "antarctica",
      "arabean_sea",
      "gulf_of_mexico",
      "e_pacific",
      "s_australia",
      "philippines",
      "s_america",
      "iceland",
      "scandinavia",
      "w_africa"
      )
    ))
   )
)

model_region <- data.frame(
  (matrix(vector(), 220, 22, dimnames=list(
    c(), 
    c("time",
      "new_zealand", 
      "greenland", 
      "n_atlantic", 
      "s_atlantic", 
      "indonesia",
      "japan",
      "oregon",
      "alaska",
      "china",
      "congo",
      "mediterranean",
      "antarctica",
      "arabean_sea",
      "gulf_of_mexico",
      "e_pacific",
      "s_australia",
      "philippines",
      "s_america",
      "iceland",
      "scandinavia",
      "w_africa"
    )
  ))
  )
)

#time
model_region$time = seq(100,22000,100)

#new zealand AA70, AA72,  AB71, AC70
x=4
#model
temp <- data.frame(matrix(vector(),220,6))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "AA70" ||model_data$PageName[i] =="AA72"||model_data$PageName[i] =="AB71"||model_data$PageName[i] =="AC70"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$new_zealand[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,6))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "AA70" ||proxy_data$PageName[i] =="AA72"||proxy_data$PageName[i] =="AB71"||proxy_data$PageName[i] =="AC70"){
    for(j in 1:220){
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", proxy_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  proxy_region$new_zealand[i]<-temp[i,x+2]
}

#greenland C28, D29
x=2
#model
temp <- data.frame(matrix(vector(),220,4))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "C28" ||model_data$PageName[i] =="D29"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$greenland[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,4))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  #here: model and proxy doesn't matter
  if(proxy_data$PageName[i] == "C28" ||proxy_data$PageName[i] =="D29"){
    for(j in 1:220){
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", proxy_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  proxy_region$greenland[i]<-temp[i,x+2]
}

# Japan J65, k65, L64, L62 and M62
x=5
#model
temp <- data.frame(matrix(vector(),220,7))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "J65" ||
     model_data$PageName[i] =="K65" ||
     model_data$PageName[i] =="L62" ||
     model_data$PageName[i] =="L64" ||
     model_data$PageName[i] =="M62" 
     ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$japan[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,7))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "J65" ||
     proxy_data$PageName[i] =="K65" ||
     proxy_data$PageName[i] =="L62" ||
     proxy_data$PageName[i] =="L64" ||
     proxy_data$PageName[i] =="M62" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$japan[i]<-temp[i,x+2]
}

# mediterrean K34, K35, K36, K39, K40, L43, M43 and J40
x = 8
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "K34" ||
     model_data$PageName[i] =="K35" ||
     model_data$PageName[i] =="K36" ||
     model_data$PageName[i] =="K39" ||
     model_data$PageName[i] =="K40" ||
     model_data$PageName[i] =="L43" ||
     model_data$PageName[i] =="M43" ||
     model_data$PageName[i] =="J40"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$mediterranean[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "K34" ||
     proxy_data$PageName[i] =="K35" ||
     proxy_data$PageName[i] =="K36" ||
     proxy_data$PageName[i] =="K39" ||
     proxy_data$PageName[i] =="K40" ||
     proxy_data$PageName[i] =="L43" ||
     proxy_data$PageName[i] =="M43" ||
     proxy_data$PageName[i] =="J40" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$mediterranean[i]<-temp[i,x+2]
}

# Alaska E3, E5, E9, F3 and F9
x=5
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "E3" ||
     model_data$PageName[i] =="E5" ||
     model_data$PageName[i] =="E9" ||
     model_data$PageName[i] =="F3" ||
     model_data$PageName[i] =="F9" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$alaska[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "E3" ||
     proxy_data$PageName[i] =="E5" ||
     proxy_data$PageName[i] =="E9" ||
     proxy_data$PageName[i] =="F3" ||
     proxy_data$PageName[i] =="F9" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$alaska[i]<-temp[i,x+2]
}

# north atlantic J25, J27, K22, K30, L21 and L25
x = 6
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "J25" ||
     model_data$PageName[i] =="J27" ||
     model_data$PageName[i] =="K22" ||
     model_data$PageName[i] =="K30" ||
     model_data$PageName[i] =="L21" ||
     model_data$PageName[i] =="L25" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$n_atlantic[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "J25" ||
     proxy_data$PageName[i] =="J27" ||
     proxy_data$PageName[i] =="K22" ||
     proxy_data$PageName[i] =="K30" ||
     proxy_data$PageName[i] =="L21" ||
     proxy_data$PageName[i] =="L25"  
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$n_atlantic[i]<-temp[i,x+2]
}

#iceland E33, F32, F33, G31, G34 and H34
x = 6
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "E33" ||
     model_data$PageName[i] =="E32" ||
     model_data$PageName[i] =="F33" ||
     model_data$PageName[i] =="G31" ||
     model_data$PageName[i] =="G34" ||
     model_data$PageName[i] =="H34" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$iceland[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "E33" ||
     proxy_data$PageName[i] =="E32" ||
     proxy_data$PageName[i] =="F33" ||
     proxy_data$PageName[i] =="G31" ||
     proxy_data$PageName[i] =="G34" ||
     proxy_data$PageName[i] =="H34"  
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$iceland[i]<-temp[i,x+2]
}

# philippines Q59, Q61, Q62 and N60
x = 4
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "Q59" ||
     model_data$PageName[i] =="Q61" ||
     model_data$PageName[i] =="Q62" ||
     model_data$PageName[i] =="N60" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$philippines[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "Q59" ||
     proxy_data$PageName[i] =="Q61" ||
     proxy_data$PageName[i] =="Q62" ||
     proxy_data$PageName[i] =="N60"   
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$philippines[i]<-temp[i,x+2]
}

# Indonesia S60, T57, t59, T60, T63, U61 and U62
x = 7
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "S60" ||
     model_data$PageName[i] =="T57" ||
     model_data$PageName[i] =="T59" ||
     model_data$PageName[i] =="T60" ||
     model_data$PageName[i] =="T63" ||
     model_data$PageName[i] =="U61" ||
     model_data$PageName[i] =="U62" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$indonesia[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "S60" ||
     proxy_data$PageName[i] =="T57" ||
     proxy_data$PageName[i] =="T59" ||
     proxy_data$PageName[i] =="T60" ||
     proxy_data$PageName[i] =="T63" ||
     proxy_data$PageName[i] =="U61" ||
     proxy_data$PageName[i] =="U62"  
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$indonesia[i]<-temp[i,x+2]
}

# antarctica AG37, AG61, AH44 and AH58
x = 4
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "AG37" ||
     model_data$PageName[i] =="AG61" ||
     model_data$PageName[i] =="AH44" ||
     model_data$PageName[i] =="AH58"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$antarctica[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "AG37" ||
     proxy_data$PageName[i] =="AG61" ||
     proxy_data$PageName[i] =="AH44" ||
     proxy_data$PageName[i] =="AH58" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$antarctica[i]<-temp[i,x+2]
}

# South America AA22 and  Y22
x = 2
#model
#change here
temp <- data.frame(matrix(vector(),220,4))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "AA22" ||
     model_data$PageName[i] =="Y22" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$s_america[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,4))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "AA22" ||
     proxy_data$PageName[i] =="Y22"  
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$s_america[i]<-temp[i,x+2]
}


# eastern pacific S19, S20, Q20, R18, R19
x = 5
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "S19" ||
     model_data$PageName[i] =="S20" ||
     model_data$PageName[i] =="Q20" ||
     model_data$PageName[i] =="R18" ||
     model_data$PageName[i] =="R19"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$e_pacific[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "S19" ||
     proxy_data$PageName[i] =="S20" ||
     proxy_data$PageName[i] =="Q20" ||
     proxy_data$PageName[i] =="R18" ||
     proxy_data$PageName[i] =="R19"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$e_pacific[i]<-temp[i,x+2]
}

# Caribbean / GofMexico M18, M19, P21, P23 and P24
x = 5
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "M18" ||
     model_data$PageName[i] =="M19" ||
     model_data$PageName[i] =="P21" ||
     model_data$PageName[i] =="P23" ||
     model_data$PageName[i] =="P24"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$gulf_of_mexico[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "M18" ||
     proxy_data$PageName[i] =="M19" ||
     proxy_data$PageName[i] =="P21" ||
     proxy_data$PageName[i] =="P23" ||
     proxy_data$PageName[i] =="P24"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$gulf_of_mexico[i]<-temp[i,x+2]
}

# Oregon offshore J11 and J12
x = 2
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "J11" ||
     model_data$PageName[i] =="J12" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$oregon[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "J11" ||
     proxy_data$PageName[i] =="J12" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$oregon[i]<-temp[i,x+2]
}

# scandinavia G38, G39, E38, E40, E41, D42 and C39
x = 7
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "G38" ||
     model_data$PageName[i] =="G39" ||
     model_data$PageName[i] =="E38" ||
     model_data$PageName[i] =="E40" ||
     model_data$PageName[i] =="E41" ||
     model_data$PageName[i] =="D42" ||
     model_data$PageName[i] =="C39" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$scandinavia[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "G38" ||
     proxy_data$PageName[i] =="G39" ||
     proxy_data$PageName[i] =="E38" ||
     proxy_data$PageName[i] =="E40" ||
     proxy_data$PageName[i] =="E41" ||
     proxy_data$PageName[i] =="D42" ||
     proxy_data$PageName[i] =="C39" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$scandinavia[i]<-temp[i,x+2]
}

# w africa R38, T39 and V39
x=3
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "R38" ||
     model_data$PageName[i] =="T39" ||
     model_data$PageName[i] =="V39"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$w_africa[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "R38" ||
     proxy_data$PageName[i] =="T39" ||
     proxy_data$PageName[i] =="V39"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$w_africa[i]<-temp[i,x+2]
}

# arabean sea P47 and P48
x = 2
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "P47" ||
     model_data$PageName[i] =="P48" 
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$arabean_sea[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "P47" ||
     proxy_data$PageName[i] =="P48" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$arabean_sea[i]<-temp[i,x+2]
}

# congo T42
x = 1
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "T42"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$congo[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "T42" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$congo[i]<-temp[i,x+2]
}

# china L59
x = 1
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "L59"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$china[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "L59" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$china[i]<-temp[i,x+2]
}

# s australia Z64
x = 1
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "Z64"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$s_australia[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "Z64" 
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$s_australia[i]<-temp[i,x+2]
}

# S atlantic X39, AA38 and AB38
x = 3
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] == "X39" ||
     model_data$PageName[i] =="AA38" ||
     model_data$PageName[i] =="AB38"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_region$s_atlantic[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] == "X39" ||
     proxy_data$PageName[i] =="AA38" ||
     proxy_data$PageName[i] =="AB38"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$s_atlantic[i]<-temp[i,x+2]
}

write.csv(proxy_region,"proxy_region.csv")
write.csv(model_region,"model_region.csv")
