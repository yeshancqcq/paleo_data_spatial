x = paste("~/Downloads/lipd/all/",filenames[2],sep="")
D = readLipd(path = toString(x))

setwd("~/Documents/github/paleo_data_spatial")

library(lipdR)

setwd("~/Downloads/lipd/all")

filenames <- list.files(path = "~/Downloads/lipd/lipd_data", pattern=NULL, all.files=FALSE,
                        full.names=FALSE)

metadata = data.frame(id = 1:length(filenames),
                      lat = NA,
                      lon = NA,
                      sitename = NA,
                      filename = NA,
                      elevation = NA,
                      country = NA,
                      gcmd_location = NA,
                      author = NA,
                      pub_year = NA,
                      title = NA,
                      journal = NA)

for(i in 1:length(filenames)){
  x = paste("~/Downloads/lipd/lipd_data/",filenames[i],sep="")
  D = readLipd(path = toString(x))
  metadata$lat[i] = D[["geo"]][["latitude"]]
  metadata$lon[i] = D[["geo"]][["longitude"]]
  metadata$elevation[i] = D[["geo"]][["elevation"]]
  metadata$sitename[i] = D[["geo"]][["siteName"]]
  if(length(D[["geo"]][["countryOcean"]])>0){
    metadata$country[i] = D[["geo"]][["countryOcean"]]
  }
  if(length(D[["geo"]][["gcmdLocation"]])>0){
    metadata$gcmd_location[i] = D[["geo"]][["gcmdLocation"]]
  }
  metadata$filename[i] = toString(filenames[i])
  if(length(D[["pub"]][[1]][["author"]][[1]][["name"]])>0){
    metadata$author[i] = D[["pub"]][[1]][["author"]][[1]][["name"]]
  }
  if(length(D[["pub"]][[1]][["year"]])>0){
    metadata$pub_year[i] = D[["pub"]][[1]][["year"]]
  }
  if(length(D[["pub"]][[1]][["title"]])>0){
    metadata$title[i] = D[["pub"]][[1]][["title"]]
  }
  if(length(D[["pub"]][[1]][["journal"]])>0){
    metadata$journal[i] = D[["pub"]][[1]][["journal"]]
  }
  cat("finishing", i, "of 455","\n")
}
write.csv(metadata, file="~/Documents/github/paleo_data_spatial/data/lipd_metadata.csv",fileEncoding="UTF-8")
