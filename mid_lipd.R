setwd("~/Documents/github/paleo_data_spatial")

library(lipdR)

setwd("~/Downloads/lipd/midfiles")

filenames <- list.files(path = "~/Downloads/lipd/midfiles", pattern=NULL, all.files=FALSE,
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

#teststring = paste("~/Downloads/lipd/lipd_data/",filenames[2],sep="")

for(i in 1:length(filenames)){
  x = paste("~/Downloads/lipd/midfiles/",filenames[i],sep="")
  D = readLipd(path = toString(x))
  #level 1
  if(length(D[["paleoData"]][[1]][["measurementTable"]])==1){
    if(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]]) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]]))){
      paleodata = data.frame(age = D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])
    } else if (
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])) &&
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]]))
    ){
      paleodata = data.frame(depth = D[["paleoData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]],
                             age = D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])
    }  else {
      paleodata = data.frame(depth = NA)
    }
  } else if(length(D[["paleoData"]][[1]][["measurementTable"]])==2){
    if(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["depth"]][["values"]]) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]]))){
      paleodata = data.frame(age = D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])
    } else if (
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])) &&
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]]))
    ){
      paleodata = data.frame(depth = D[["paleoData"]][[1]][["measurementTable"]][[2]][["depth"]][["values"]],
                             age = D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])
    }  else {
      paleodata = data.frame(depth = NA)
    }
  } else {
    paleodata = data.frame(depth = NA)
  }
  
  #chron
  len = length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]][["values"]])
  if(len>0){
    output <- data.frame(id = 1:len, 
                         age_type = NA, 
                         depth = NA,
                         depth_unit = " ",
                         age = NA,
                         age_err = NA, 
                         age_unit = " ",
                         age_high = NA,
                         age_high_unit = " ",
                         age_low = NA,
                         age_low_unit = " ",
                         reservoir_age = NA, 
                         reservoir_age_unit = " ",
                         lab_id = NA,
                         sd = NA,
                         sd_unit = " ",
                         material = NA,
                         thickness = NA,
                         thickness_unit = " ",
                         notes = NA)
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]])>0){
      output$depth = D[["chronData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]]
      output$depth_unit = toString(D[["chronData"]][[1]][["measurementTable"]][[1]][["depth"]][["units"]])
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["depthMid"]][["values"]])>0){
      output$depth = D[["chronData"]][[1]][["measurementTable"]][[1]][["depthMid"]][["values"]]
      output$depth_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["depthMid"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]][["values"]])>0){
      output$age_type = D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["labID"]][["values"]])>0){
      output$lab_id = D[["chronData"]][[1]][["measurementTable"]][[1]][["labID"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]])>0){
      output$age = D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]]
      if(length( D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["units"]])>0){
        output$age_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["units"]]
      }
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["ageHi"]][["values"]])>0){
      output$age_high = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageHi"]][["values"]]
      output$age_high_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageHi"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["ageLow"]][["values"]])>0){
      output$age_low = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageLow"]][["values"]]
      output$age_low_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageLow"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["thickness"]][["values"]])>0){
      output$thickness = D[["chronData"]][[1]][["measurementTable"]][[1]][["thickness"]][["values"]]
      output$thickness_unit[1] = D[["chronData"]][[1]][["measurementTable"]][[1]][["thickness"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["1SD"]][["values"]])>0){
      output$sd = D[["chronData"]][[1]][["measurementTable"]][[1]][["1SD"]][["values"]]
      output$sd_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["1SD"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["SD"]][["values"]])>0){
      output$sd = D[["chronData"]][[1]][["measurementTable"]][[1]][["SD"]][["values"]]
      output$sd_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["SD"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["material"]][["values"]])>0){
      output$material = D[["chronData"]][[1]][["measurementTable"]][[1]][["material"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["measurmentMaterial"]][["values"]])>0){
      output$material = D[["chronData"]][[1]][["measurementTable"]][[1]][["measurmentMaterial"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["notes"]][["values"]])>0){
      output$notes = D[["chronData"]][[1]][["measurementTable"]][[1]][["notes"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["ageError"]][["values"]])>0){
      output$age_err = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageError"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["reservoirAge"]][["values"]])>0){
      output$reservoir_age = D[["chronData"]][[1]][["measurementTable"]][[1]][["reservoirAge"]][["values"]]
      output$reseervoir_age_unit = toString(D[["chronData"]][[1]][["measurementTable"]][[1]][["reservoirAge"]][["units"]])
    }
    
  } else {
    output <- data.frame(id = 1, 
                         age_type = NA, 
                         depth = NA,
                         depth_unit = NA,
                         age = NA,
                         age_err = NA, 
                         age_unit = NA,
                         material = NA)
  }
  
  #meta
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
  
  save_dir = paste("~/Downloads/lipd/mid2csv/",filenames[i],".csv",sep="")
  write.csv(paleodata, file=toString(save_dir),fileEncoding="UTF-8")
  save_dir2 = paste("~/Downloads/lipd/mid_chron/",filenames[i],"_chronData.csv",sep="")
  write.csv(output, file=toString(save_dir2),fileEncoding="UTF-8")
  cat("finishing", i, "of 121","\n")
}

write.csv(metadata, file="~/Documents/github/paleo_data_spatial/data/lipd_metadata_mid.csv",fileEncoding="UTF-8")
