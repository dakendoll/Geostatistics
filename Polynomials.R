#################################################
##Spatial Interpolation with Polynomial Trends
# Define the 1st order polynomial equation

f.1 <- as.formula(value ~ X + Y) 

# Add X and Y to P
ozone.mean.spdf$X <- coordinates(ozone.mean.spdf)[,1]
ozone.mean.spdf$Y <- coordinates(ozone.mean.spdf)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=ozone.mean.spdf)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, SC.AirBasin.t)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted Ozone \n(in ppm)") +
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)



# Define the 2nd order polynomial equation
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Add X and Y to P
ozone.mean.spdf$X <- coordinates(ozone.mean.spdf)[,1]
ozone.mean.spdf$Y <- coordinates(ozone.mean.spdf)[,2]

# Run the regression model
lm.2 <- lm( f.2, data=ozone.mean.spdf)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- mask(r, SC.AirBasin.t)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,
            title="Predicted Ozone \n(in ppm)") +
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
