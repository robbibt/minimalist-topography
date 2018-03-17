
# Install forked version of ggridges which has been modified to include a white outline around lines
library("devtools")
install_github("robbibt/ggridges")

# Import libraries
library("raster")
library("tidyverse")
library("zoo")
library("ggridges")

# GDAL warp call:
# C:\Anaconda2\Lib\site-packages\osgeo\gdalwarp -cutline C:\Geography\GIS_data\cb_2016_us_nation_5m\mainland_usa_5m.shp -crop_to_cutline -t_srs "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" C:\Geography\GIS_data\sdat_10003_1_20180316_053558638.tif C:\Geography\GIS_data\output.tif


##########
# Set up #
##########

# Import DEM raster
clipped_raster = raster("raw_data/output.tif")

# Plotting parameters
plot_spacing = 18  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.6  # exponent factor for elevations (small values = less difference between low and high)
plot_mult = 1.5  # multiplication factor for elevations (high values = higher line heights)
plot_smoothing = 8  # How many x-axis points to smooth using rolling averages

# Output file path
output_file = "output_data/test.png"


############
# Analysis #
############

# Set areas of low elevation to NA
clipped_raster[clipped_raster < 1] = NA
plot(clipped_raster)

# Set up sequence of raster rows to extract
y_vals = seq(1, nrow(clipped_raster), plot_spacing)
all_list = vector("list", length(y_vals))

# For each selected row
for (i in 1:length(y_vals)) {

  # Create dataframe with latitude of selected row, distance along x axis and ridgeline heights
  y_df = data.frame(y = -y_vals[i],
                    x = 1:ncol(clipped_raster),
                         
                    # For improved visual impact in low elevation areas, use exponent
                    height = (clipped_raster[y_vals[i], ]) ^ plot_exp * plot_mult) %>% 
    
              # For smoother lines, apply a rolling mean to height values
              mutate(height = rollapplyr(height, width = plot_smoothing, FUN = mean, partial=TRUE))
  
  # Append results to list
  all_list[[i]] = y_df

}

# Combine results for each row into single dataframe
all_df = bind_rows(all_list) 


############
# Plotting #
############

# Plot resulting data using ridgeline plots
output_plot = ggplot(data = all_df) + 
  geom_ridgeline(aes(x = x, y = y, height = height, group = y), fill = "white", size = 0.45) +
  coord_cartesian() +  
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.background = element_blank(), 
        panel.background = element_blank())

# Export to file
ggsave(output_file, output_plot, dpi = 600, width = 40, height = 25, type = "cairo-png")
