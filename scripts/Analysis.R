install.packages("automap")
library(devtools)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tmap)
library(tibble)
library(rgdal)
library(spatstat)
library(gstat)
library(automap)

load(file = "inputData/2020-07-21 - input data for test GeoSpatial Data Scientist.RData")
#check metadata
head(gadm)
head(po)
nrow(po)
str(po, max.level = 2)
print(po)
print(gadm)
summary(po)

#Check CRS
proj4string(gadm)
st_crs(po)
st_crs(gadm)
class(po)
class(gadm)

#Convert sp to sf
data_sf <- st_as_sf(po)
data2_sf <- st_as_sf(gadm)

#Reproject to utm
st_transform(data_sf, crs ="+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" )
st_transform(data2_sf, crs = "+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"  )

#Correlation between OM and PH
cor(data_sf$pH, data_sf$OM)

#Check missing values
hist(data_sf$pH)
table(is.na(data_sf$pH))
#Quick visuals
tm_shape(data2_sf)+
  tm_polygons(alpha = 0.5)+
  tm_shape(data_sf)+
  tm_dots(col = "red", size = 0.2)

#Create a dataset for pH and OM variables
data_pH <- select(data_sf, -OM)
as.data.frame(data_pH)

data_Om <- select(data_sf, -pH)
as.data.frame(data_Om)

#Convert the datasets created to sp objects
data_pH_sp <- as(data_pH, Class = "Spatial")
data_Om_sp <- as(data_Om, Class = "Spatial")

gadm_sp <- as(data2_sf, Class = "Spatial")

spTransform(gadm_sp, CRS(data_pH_sp))


#Individual variable plots--Points distribution
spplot(data_pH_sp[], "pH")
spplot(data_Om_sp[], "OM")

#Fit a simple linear model. >>Response variables depend on coordinates>> check significance of variables
coordnames(data_pH_sp)

lm <- lm(pH ~ coords.x1 + coords.x2, as.data.frame(data_pH_sp))
summary(lm)  #all variablles are statistically significant

lm2 <- lm(OM ~coords.x1 + coords.x2, as.data.frame(data_Om_sp))
summary(lm2) 


##SPATIAL INTRPOLATION 
#Fit a VARIOGRAM model 
 

#Plot a spatial trend variogram

vgm_pH <- variogram(pH ~ coords.x1 + coords.x2 , data = data_pH_sp)
plot(vgm_pH)

vgm_OM <- plot(variogram(OM ~ 1, data_Om_sp))
plot(vgm_OM)

#Fit a variogram model for pM data

nugget <- 0.20
psill <-  0.5
range <-  15

v_model_pH <- fit.variogram(
  vgm_pH, 
  model = vgm(
    model = "Gau",
    nugget = nugget,
    psill = psill,
    range = range,
    kappa = 0.5
  )
)

plot(vgm_pH, model = v_model_pH)
print(v_model_pH)

#Fit a variogram model for OM data
nugget <- 0.6
psill <-  0.2
range <-  15

v_model_OM <- fit.variogram(
  vgm_OM, 
  model = vgm(
    model = "Gau",
    nugget = nugget,
    psill = psill,
    range = range,
    kappa = 0.5
  )
)

plot(vgm_OM, model = v_model_OM)
print(v_model_OM)

#KRIGING

pH_pred <- krige(pH ~ 1, data_pH_sp , newdata = gadm_sp, model = v_model_pH)

#Plot predicted variables on a map
spplot(pH_pred, zcol = "var1.pred")
names(pH_pred)

#Try auto Kriging

predict_auto_pH <- autoKrige(pH ~ 1, input_data = data_pH_sp, new_data = gadm_sp )

#Ooops! ERROR in Interpolation. I have to reproject new_data


#Create a data grid from the polygon DataFrame object
grd <- spsample(gadm_sp, n=5000, type="regular")
grd_pixels= as(grd, "SpatialPixels")
plot(grd_pixels)

#Try to Interpolate on grided datasets:

pH_pred_grid <- krige(pH ~ 1, data_pH_sp , newdata = grd_pixels, model = v_model_pH)
names(pH_pred_grid)

#Auto kriging
predict_auto_pH <- autoKrige(pH ~ 1, input_data = data_pH_sp, new_data = pH_pred_grid )

#Create a map of the Interpoled points
spplot(pH_pred_grid, zcol = "var1.pred")

spplot(pH_pred_grid, "var1.pred", do.log = TRUE,
       key.space=list(x = 0.1, y = 0.95, corner = c(0, 1)),
       scales=list(draw = TRUE))

#Save the Predictions as df and export
Predicted_xlsx <- as.data.frame(pH_pred_grid)
write.xlsx(Predicted_xlsx, "Predicted_pH_values.xlsx")



#Save the Spatial polygonDF (sf) as shapefile

st_write(data2_sf, "gadm.shp", driver="ESRI Shapefile")  

#Import the shapefile

gadm_shp <- st_read("outputData/gadm.shp")

#Interpolate on the polygon (.shp), both manual and auto krige
pH_pred <- krige(pH ~ 1, data_pH_sp , newdata = gadm_shp, model = v_model_pH)

predict_auto_pH <- autoKrige(pH ~ 1, input_data = data_pH_sp, new_data = gadm_sp )







