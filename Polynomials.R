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
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted Ozone \n(in ppm)") +
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)



# Define the 2nd order polynomial equation
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

#f.2 <- as.formula(Precip_in ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))
#f.2.poly <- lm(noisy.y ~ poly(q,3))
#model <- lm(noisy.y ~ x + I(X^2) + I(X^3))

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
  tm_raster(n=10, palette="RdBu", midpoint = NA,
            title="Predicted Ozone \n(in ppm)") +
  tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

summary(lm.1)
summary(lm.2)
confint(lm.1, level=0.95)
confint(lm.2, level=0.95)
plot(fitted(lm.1),residuals(lm.1),xlab="fitted",ylab="residuals",main=expression(1^st~Order~Polynomial~Regression~Model))
plot(fitted(lm.2),residuals(lm.2),xlab="fitted",ylab="residuals",main=expression(2^nd~Order~Polynomial~Regression~Model))
