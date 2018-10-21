#################################################
##Spatial Interpolation with Kriging

f.1 <- as.formula(value ~ X + Y) 
var.smpl <- variogram(f.1, ozone.mean.spdf, cloud = FALSE) #, cutoff=1000000, width=89900)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=1.4e-05, model="Sph", range=15, nugget=0))
plot(var.smpl, dat.fit)


# Define the trend model
f.1 <- as.formula(value ~ X + Y) 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, ozone.mean.spdf, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, SC.AirBasin.t)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted Ozone \n(in ppm)") +
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, SC.AirBasin.t)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \n(in squared ppm)") +tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, SC.AirBasin.t)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map \n(in ppm)") +tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)