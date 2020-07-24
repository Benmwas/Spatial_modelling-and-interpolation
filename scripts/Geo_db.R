
library(sf)
library(sp)
library(odbc)
library(DBI)
library(rpostgis)
library(RPostgreSQL)


load(file = "inputData/2020-07-21 - input data for test GeoSpatial Data Scientist.RData")
sort(unique(odbcListDrivers()[[1]]))


#Set database connection
conn <- dbConnect(drv = "PostgreSQL", host = "localhost", dbname = "spatial_projects", 
                  user = "postgres", password = "*****")

#Store pointDF to postGIS (dbname = spatial_projects)
pgInsert(conn, "po_data", po, new.id = "Id_1")

