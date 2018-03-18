
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

# Input DEM raster file path
input_raster = "raw_data/EU.tif"
output_file = "output_data/EU.png"

# Plotting parameters, USA
plot_spacing = 18  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.58  # exponent factor for elevations (small values = less difference between low and high)
plot_mult = 1.65  # multiplication factor for elevations (high values = higher line heights)
plot_smoothing = 20  # How many x-axis points to smooth using rolling averages

# # Plotting parameters, Mexico
# plot_spacing = 4  # select every xth y-axis row for plotting (small values = finely spaced lines) 
# plot_exp = 0.8  # exponent factor for elevations (small values = less difference between low and high)
# plot_mult = 0.05  # multiplication factor for elevations (high values = higher line heights)
# plot_smoothing = 4  # How many x-axis points to smooth using rolling averages

# Plotting parameters, Australia
plot_spacing = 18  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.83  # exponent factor for elevations (small values = less difference between low and high)
plot_mult = 0.3  # multiplication factor for elevations (high values = higher line heights)
plot_smoothing = 13  # How many x-axis points to smooth using rolling averages

# Plotting parameters, Canada
plot_spacing = 26  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.7  # exponent factor for elevations (small values = less difference between low and high)
plot_mult = 0.7  # multiplication factor for elevations (high values = higher line heights)
plot_smoothing = 18  # How many x-axis points to smooth using rolling averages

# Plotting parameters, Europe
plot_spacing = 15  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.67  # exponent factor for elevations (small values = less difference between low and high)
plot_mult = 0.4  # multiplication factor for elevations (high values = higher line heights)
plot_smoothing = 16  # How many x-axis points to smooth using rolling averages


############
# Analysis #
############

# Import raster and set areas of low elevation to NA
dem = raster(input_raster)
dem[dem < 1] = 0
dem[is.na(dem)] = 0
plot(dem)





# Set up sequence of raster rows to extract
y_vals = seq(3, nrow(dem), plot_spacing)
all_list = vector("list", length(y_vals))

# For each selected row
for (i in 1:length(y_vals)) {

  # Create dataframe with latitude of selected row, distance along x axis and ridgeline heights
  y_df = data.frame(y = -y_vals[i],
                    x = 1:ncol(dem),
                    dem_height = dem[y_vals[i], ]) %>% 
                         
              # For improved visual impact in low elevation areas, use exponent
              mutate(height = (dem_height) ^ plot_exp * plot_mult) %>% 
    
              # For smoother lines, apply a rolling mean to height values
              # mutate(height = height) %>% 
              mutate(height = rollapply(height, width = plot_smoothing, FUN = mean, partial=TRUE,  align = "center")) %>% 
              mutate(height = ifelse(dem_height > 0, height, -1))
  
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
  coord_fixed() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.background = element_blank(), 
        panel.background = element_blank())

# Export to file
# ggsave(output_file, output_plot, dpi = 508*1.5, width = 100/1.5, height = 70.478/1.5, units = "cm", type = "cairo-png")
ggsave(output_file, output_plot, dpi = 508*1.5, heigh = 100/1.5, width = 70.478/1.5, units = "cm", type = "cairo-png")





# Convert to points
dem_points = rasterToPoints(dem) %>% 
  as_data_frame() %>% 
  # arrange(x, y) %>% 
  # slice(seq(1, nrow(.), 500)) %>% 
  mutate(height = (EU) ^ plot_exp * 500)



dem_points2 = dem_points %>% 
  filter(y %in% unique(y)[seq(1, length(unique(y)), 50)]) %>% 
  mutate(height = ifelse(height > 0, height, -1))


test_plot = ggplot(data = dem_points2) + 
  geom_ridgeline(aes(x = x, y = y, height = height, group = y), fill = "white", size = 0.45) +
  coord_fixed() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.background = element_blank(), 
        panel.background = element_blank())

ggsave(output_file, test_plot, dpi = 200, heigh = 25, width = 15, units = "cm", type = "cairo-png")




