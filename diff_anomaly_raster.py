import arcpy
#from arcpy import env
#from arcpy.sa import *
arcpy.CheckOutExtension("spatial")
arcpy.env.overwriteOutput = True

arcpy.env.workspace = "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial"
polygon = "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/data/diff_anomaly.shp"

allFields = arcpy.ListFields("C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/data/diff_anomaly_new.shp", {"None"}, {"Double"})

for field in allFields:

    fieldName = str(field.name)

    print ("Starting the processing " + fieldName)

    if fieldName != "FID" and fieldName != "Shape" and fieldName != "PageName":

        raster = str("C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/diff_raster/" + fieldName)

        # polygon to raster

        arcpy.PolygonToRaster_conversion(polygon, fieldName, raster, 'CELL_CENTER', 'NONE', '5')

        print (fieldName +" finished; ")

       


    
