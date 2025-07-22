##############################################################
# RE: Calculate percent green cover for turf grass plots
#       -> requires shape file of plot boundaries
#       -> requires UAV captured orthomosaic
#       -> plots and ortho must be in same coordinate system 
#            (e.g. NC Stateplane, NAD83, feet - EPSG 2264)
# Date: July 2025
##############################################################
library(tidyr)
library(sf)
library(terra)
library(ggplot2)

### set local data paths
plot_filepath = "./data/PlotBoundary.shp"
orth_filepath = "./data/LW_07102025_RGB.tif"

### read in the plot boundaries and UAV ortho 
plots <- vect(plot_filepath)
ortho <- rast(orth_filepath)

### print summary info 
print(plots)
print(ortho)

### Plot the shapefile (simple plot)
plotRGB(ortho, r = 1, g = 2, b = 3, stretch = "lin", main = "Rob is Amazing")
plot(plots, add = TRUE, border = "black", lwd = 2, col = NA)

### Extract the plot areas of the imagery
# -> Cropping before masking reduces the processing area
crop_raster <- crop(ortho, plots)
ortho_mask <- mask(crop_raster, plots)

### Plot to make sure it worked
plotRGB(ortho_mask, r = 1, g = 2, b = 3, stretch = "lin", main = "This is Super Amazing")
plot(plots, add = TRUE, border = "black", lwd = 2, col = NA)


### Convert from RGB to HSV color space
hsv_ortho <- colorize(ortho_mask, to = "hsv")

### Plot up the 3 new color bands
par(mfrow = c(1, 3))
plot(hsv_ortho$hue, main = "HSV - Hue Component", col = hcl.colors(100, "viridis"))
plot(hsv_ortho$saturation, main = "HSV - Saturation Component", col = gray.colors(100))
plot(hsv_ortho$value, main = "HSV - Value Component", col = gray.colors(100))
par(mfrow = c(1, 1))

### Get the pixel values to examine at the histograms (remove na's)
pix_val <- values(hsv_ortho, dataframe=TRUE)
pix_val <- na.omit(pix_val)

### Pivot the data to long for so we can plot using facet_wrap
pix_val_long <- pix_val %>%
  pivot_longer(
    cols = c(hue, saturation, value), 
    names_to = "Variable",                 
    values_to = "Value"                    
  )

### plot the distribution of pixel vales for each HSV band
ggplot(pix_val_long, aes(x = Value)) + 
  geom_histogram(binwidth = 0.005, fill = "steelblue", color = "black") + 
  facet_wrap(~ Variable, ncol = 1) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  theme_minimal()


### Threshold the 'hue' band. Hue values typically range from 0 to 1
# Make a copy of the hue band
hue_band <- hsv_ortho$hue      


# Define the threshold range
min_hue <- 0.2
max_hue <- 0.5


### Select the threshold values  
# Create a new raster from the hue values that will be used to store just the turfgrass pixels
# and Set pixels outside the desired range to NA
turf_binary <- hue_band

### remove the values that are not turfgrass (assign them NA)
turf_binary[hue_band < min_hue | hue_band > max_hue] <- NA

### do the same, but this time remove the turfgrass, keep the soil 
soil_binary <- hue_band
soil_binary[hue_band >= min_hue & hue_band <= max_hue] <- NA



### plot up the threshold turfgrass areas over the original RGB image
zlim_vals <- quantile(values(turf_binary), probs = c(0.05, 0.95), na.rm = TRUE)
plotRGB(ortho_mask, r = 1, g = 2, b = 3, stretch = "lin", main = "This is Super Amazing")
plot(turf_binary, add=TRUE, zlim=zlim_vals, col = hcl.colors(100, "viridis"))
plot(plots, add = TRUE, border = "black", lwd = 2, col = NA)
#plot(soil_binary, add = TRUE, main = "HSV - Hue Component", col = 'brown')


### calculate the area withing each zone (i.e. plot) that is turfgrass 
raster_info <- cellSize(turf_binary)
zone_vals <- extract(c(turf_binary, raster_info), plots, cells = TRUE, na.rm = TRUE)
clean_zone_vals <- na.omit(zone_vals)
tab_area <- aggregate(area ~ ID, data = clean_zone_vals, sum)
tab_area$area = round(tab_area$area,2)


### join the data back to the plots shapefile
plots$turf <- NA
plots$turf[tab_area$ID] <- tab_area$area


### calculate the area of each plot (defaults t m2)
plots$area_m2 <- expanse(plots, unit = "m")


### calculate PGC
plots$pgc = round(((plots$turf / plots$area_m2) * 100),0)


## plot the PGC on the map
text(plots, labels=plots$pgc, font=2, cex=0.7, col="white")


### write out the results
writeRaster(turf_binary, "./data/turf_raster.tif", filetype="GTiff", overwrite=TRUE)
writeVector(plots, "./data/plots_pgc.shp", filetype = "ESRI Shapefile", overwrite = TRUE)



