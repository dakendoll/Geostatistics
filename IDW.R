#################################################
##Spatial Interpolation with IDW

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(ozone.mean.spdf, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

proj4string(grd) <- proj4string(SC.monitor.t)
P.idw <- gstat::idw(value ~ 1, ozone.mean.spdf, newdata=grd, idp=2.0)
r       <- raster(P.idw)
r.m     <- mask(r, SC.AirBasin.t)

tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted Ozone \n(in ppp)") + 
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#################################################
# Leave-one-out validation routine
IDW.out <- vector(length = length(ozone.mean.spdf))
for (i in 1:length(ozone.mean.spdf)) {
  IDW.out[i] <- idw(value ~ 1, ozone.mean.spdf[-i,], ozone.mean.spdf[i,], idp=0.5)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ ozone.mean.spdf$value, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ ozone.mean.spdf$value), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - ozone.mean.spdf$value)^2) / length(ozone.mean.spdf))


#################################################
# Implementation of a jackknife technique to estimate a confidence interval at each unsampled point.
# Create the interpolated surface
img <- gstat::idw(value~1, ozone.mean.spdf, newdata=grd, idp=2.0)
n   <- length(ozone.mean.spdf)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(value~1, ozone.mean.spdf[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Texas
r <- raster(img.sig, layer="v")
r.m <- mask(r, SC.AirBasin.t)

# Plot the map
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval \n(in ppm)") +
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


