import arcpy
#from arcpy import env
#from arcpy.sa import *
arcpy.CheckOutExtension("spatial")
arcpy.env.overwriteOutput = True

arcpy.env.workspace = "C:/Users/Shan Ye/Desktop/paleoclimate"

allFields = arcpy.ListFields("C:/Users/Shan Ye/Desktop/paleoclimate/paleoclimate/anomaly_new_sim.shp", {"None"}, {"Double"})

points = "C:/Users/Shan Ye/Desktop/paleoclimate/paleoclimate/anomaly_new_sim.shp"

refGrid = "C:/Users/Shan Ye/Desktop/paleoclimate/paleoclimate/newGrid_8_12ka.shp"

cellSize = 2000

for field in allFields:

    fieldName = str(field.name)

    print ("Starting the processing " + fieldName)

    if fieldName != "FID" and fieldName != "Shape":

        raster = str("C:/Users/Shan Ye/Desktop/paleoclimate/raster/" + fieldName + "Stat")

        table = str("C:/Users/Shan Ye/Desktop/paleoclimate/spatialGrid/ZonalSt_" + fieldName)

        # point to raster

        arcpy.PointToRaster_conversion(points, fieldName, raster, "MEAN", "", "0.61")

        print ("point to raster done")

        # zonal statistics

        arcpy.gp.ZonalStatisticsAsTable_sa(refGrid, "PageName", raster, table, "DATA", "MEAN")

        print ("zonal stats done")

        # join table

        arcpy.JoinField_management(refGrid, "PageName", table, "PageName", "MEAN")

        print ("join table done")

        # add a field in grid with the name of fieldName

        arcpy.AddField_management(refGrid, fieldName, "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

        print ("add field done")

        # use field calculator to transfer joined field to the new field

        arcpy.CalculateField_management(refGrid, fieldName, "[MEAN]", "VB", "")

        print ("field calculation done")

        # delete the joined field

        arcpy.DeleteField_management(refGrid, "MEAN")

        print ("delete field done")

        print ("Ending the processing " + fieldName)


    
