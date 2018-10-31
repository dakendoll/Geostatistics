#################################################
##Spatial Interpolation with Kriging

# Define the trend model, previously done in Polynomials.R
f.1 <- as.formula(value ~ X + Y) 
var.smpl <- variogram(f.1, ozone.mean.spdf, cloud = FALSE) #, cutoff=1000000, width=89900)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=1.4e-05, model="Sph", range=15, nugget=0))
plot(var.smpl, dat.fit)

dat.fit1  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.4e-05, model="Sph", range=15, nugget=0))
#less good
dat.fit2  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.3e-05, model="Sph", range=10, nugget=0))
dat.fit3  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.5e-05, model="Sph", range=25, nugget=0))
dat.fit4  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.5e-05, model="Sph", range=35, nugget=0))
dat.fit5  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.4e-05, model="Exp", range=15, nugget=0))
dat.fit6  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.5e-05, model="Exp", range=25, nugget=0))
#less good
dat.fit7  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.3e-05, model="Exp", range=10, nugget=0))
dat.fit8  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.6e-05, model="Exp", range=15, nugget=0))
dat.fit9  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill=1.4e-05, model="Gau", range=15, nugget=0))
dat.fit10  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                            vgm(psill=1.5e-05, model="Gau", range=25, nugget=0))
dat.fit11  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                            vgm(psill=1.3e-05, model="Gau", range=10, nugget=0))
dat.fit12  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                            vgm(psill=1.6e-05, model="Gau", range=20, nugget=0))

pdf("kriging_semivariograms.pdf")
#png("myplot.png", units = 'in', res = 300)
#par(mfrow=c(3,4))
op<-par(no.readonly=TRUE)
par(op)
par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(3,4))
plot(var.smpl, dat.fit1, main="Spherical Model: Sill = 1.4e-05 & Range = 15")
plot(var.smpl, dat.fit2, main="Spherical Model: Sill = 1.3e-05 & Range = 10")
plot(var.smpl, dat.fit3, main="Spherical Model: Sill = 1.5e-05 & Range = 25")
plot(var.smpl, dat.fit4, main="Spherical Model: Sill = 1.5e-05 & Range = 35")
plot(var.smpl, dat.fit5, main="Exponential Model: Sill = 1.4e-05 & Range = 15")
plot(var.smpl, dat.fit6, main="Exponential Model: Sill = 1.5e-05 & Range = 25")
plot(var.smpl, dat.fit7, main="Exponential Model: Sill = 1.3e-05 & Range = 10")
plot(var.smpl, dat.fit8, main="Exponential Model: Sill = 1.6e-05 & Range = 15")
plot(var.smpl, dat.fit9, main="Gaussian Model: Sill = 1.4e-05 & Range = 15")
plot(var.smpl, dat.fit10, main="Gaussian Model: Sill = 1.5e-05 & Range = 25")
plot(var.smpl, dat.fit11, main="Gaussian Model: Sill = 1.3e-05 & Range = 10")
plot(var.smpl, dat.fit12, main="Gaussian Model: Sill = 1.6e-05 & Range = 20")

dev.off()
text(locator(1), "sill = 1.4e-05\nrange = 15")
# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, ozone.mean.spdf, grd, dat.fit)
dat.krg <- krige( f.1, ozone.mean.spdf, grd, dat.fit4)
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