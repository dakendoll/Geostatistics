#################################################
##Spatial Interpolation with Thiessen Polygons

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(ozone.mean.spdf)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(ozone.mean.spdf)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, ozone.mean.spdf, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(SC.AirBasin.t,th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col="value", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted Ozone \n(in ppm)") +
  tm_legend(legend.outside=TRUE)
