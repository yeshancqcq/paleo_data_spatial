# install homebrew and ffmpeg first

library(magick)
library(animation)
#setwd("~/Documents/GitHub/paleo_data_spatial/anomaly_diff_map")
#frames <- paste0("/Users/apple/Documents/GitHub/paleo_data_spatial/anomaly_diff_map/", seq(100, 22000, by = 100), ".jpg")
frames <- paste0(seq(100, 22000, by = 100), ".jpg")
saveVideo({
  for(img in frames){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }  
},
video.name = "model-proxy.mp4")
