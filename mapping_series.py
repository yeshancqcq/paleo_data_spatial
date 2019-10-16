
import arcpy  
from arcpy import env  
  
# Set workspace  
arcpy.env.workspace = "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/diff_raster"

# Define the mxd file to use
mxd = arcpy.mapping.MapDocument("C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/paleo_process1.mxd")

# Hook into the data frame where you want to add the layer  
df  = arcpy.mapping.ListDataFrames(mxd)[0]  

# Get list of raster datasets  
rasterList = arcpy.ListRasters("*", "All")

# Set the symbol layer
symbolLyr = "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/symbol.lyr"

# Loop through all rasters
for raster in rasterList:

    print ("Starting the processing " + raster)

    titleAge = str(raster[1:])

    #arcpy.ApplySymbologyFromLayer_management(raster, "symbol")
    
    # Set layer name of this raster
    rasterName = str("this_layer")

    # Create the raster layer
    lyr = arcpy.MakeRasterLayer_management(raster, rasterName).getOutput(0)

    # Add this layer to data frame
    arcpy.mapping.AddLayer(df, lyr)

    layerToUpdate = arcpy.mapping.ListLayers(mxd, "this_layer", df)[0] 

    # Add title to the map
    titleStr = str("Global Data-Model Temperature Anomaly Differences at " + titleAge +" Years BP")
    titleItem = arcpy.mapping.ListLayoutElements(mxd, "TEXT_ELEMENT", "title")[0]
    titleItem.text = titleStr 

    # Apply the symbology of the "symbol" layer to this layer
    arcpy.ApplySymbologyFromLayer_management(layerToUpdate, "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/symbol.lyr")

    # Set the save directory of the map
    mapstr = str("C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/anomaly_diff_map/" + titleAge + ".jpg")

    # Export map
    arcpy.mapping.ExportToJPEG(mxd, mapstr)

    # Remove this raster layer
    arcpy.mapping.RemoveLayer(df, lyr)







    

    print ("finishing " + raster +";")
