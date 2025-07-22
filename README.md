# UAV-PGC (Percent Green Cover)
Calculate Percent Green Cover (PGC) of turfgrass plots from UAV imagery using a 
simple threshold method. 

- Converts RGBV image into HSV, then uses hue band and user-specified threshold 
values to identify the area of turgrass within plots (PGC)

![image info](./resources/pgc.png)

## Required Packages
- install.packages(tidyr)
- install.packages(ggplot2)
- install.packages(sf)
- install.packages(terra)


## Required Inputs
- UAV Image (geotiff)
- shapefile that defines the boundary of the turfgrass plots

## Assumptions
- both inputs are in the same projected coordinate system

## user specified parameters
- the threshold values for green turfgrass (assumed 0.2 to 0.5)

