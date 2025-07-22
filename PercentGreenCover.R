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

#### set data paths
plot_filepath = "./data/PlotBoundary.shp"
orth_filepath = "./data/LW_07102025_RGB.tif"

### read in the datasets
plots <- vect(plot_filepath)
ortho <- rast(orth_filepath)

### print summary info 
print(plots)
print(ortho)

### Plot the shapefile (simple plot)
plotRGB(ortho, r = 1, g = 2, b = 3, stretch = "lin", main = "Rob is Amazing")
plot(plots, add = TRUE, border = "black", lwd = 2, col = NA)

### Extract out just the plots
## -> Cropping first reduces the processing area
crop_raster <- crop(ortho, plots)
ortho_mask <- mask(crop_raster, plots)

### Plot to make sure it worked
plotRGB(ortho_mask, r = 1, g = 2, b = 3, stretch = "lin", main = "Rob is Super Amazing")
plot(plots, add = TRUE, border = "black", lwd = 2, col = NA)


### Convert from RGB to HSV color space
hsv_ortho <- colorize(ortho_mask, to = "hsv")

### Plot up the 3 new color bands
par(mfrow = c(1, 3))
plot(hsv_ortho$hue, main = "HSV - Hue Component", col = hcl.colors(100, "viridis"))
plot(hsv_ortho$saturation, main = "HSV - Saturation Component", col = gray.colors(100))
plot(hsv_ortho$value, main = "HSV - Value Component", col = gray.colors(100))
par(mfrow = c(1, 1))

### Get the pixel values to look at the histograms
pix_val <- values(hsv_ortho, dataframe=TRUE)
pix_val <- na.omit(pix_val)





pix_val_long <- pix_val %>%
  pivot_longer(
    cols = c(hue, saturation, value), # Specify the columns you want to pivot
    names_to = "Variable",                 # Name of the new column to store the original column names
    values_to = "Value"                    # Name of the new column to store the values
  )


ggplot(pix_val_long, aes(x = Value)) + 
  geom_histogram(binwidth = 0.005, fill = "steelblue", color = "black") + 
  facet_wrap(~ Variable, ncol = 1) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  theme_minimal()




### Threshold the 'hue' band. Hue values typically range from 0 to 1
hue_band <- hsv_ortho$hue      # Make a copy of the hue band

# Define the threshold range
min_hue <- 0.2
max_hue <- 0.5


# Create a new raster object for the thresholded hue
# and Set pixels outside the desired range to NA
turf_binary <- hue_band
turf_binary[hue_band < min_hue | hue_band > max_hue] <- NA
#turf_binary[!is.na(turf_binary)] <- 1

soil_binary <- hue_band
soil_binary[hue_band > min_hue & hue_band < max_hue] <- NA


#plotRGB(ortho_mask, r = 1, g = 2, b = 3, stretch = "lin", main = "Rob is Super Amazing")
par(mfrow = c(2, 1))
plot(turf_binary, main = "HSV - Hue Component", col = hcl.colors(100, "viridis"))
plot(soil_binary, main = "HSV - Hue Component", col = 'brown')
par(mfrow = c(1, 1))

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


### write out the results
writeRaster(turf_binary, "./data/out_ras.tif", filetype="GTiff", overwrite=TRUE)
writeVector(plots, "./data/plot_pgc.shp", filetype = "ESRI Shapefile", overwrite = TRUE)



