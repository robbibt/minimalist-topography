
# Install forked version of ggridges which has been modified to include a white outline around lines
library("devtools")
install_github("robbibt/ggridges")

# Import libraries
library("raster")
library("tidyverse")
library("zoo")
library("ggridges")
library("scales")


##########
# Set up #
##########

# Input DEM raster file path
input_raster = "raw_data/AUS.tif"
output_file = "output_data/AUS.png"

# Plotting parameters, USA
plot_spacing = 18  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.65  # exponent factor for elevations (small values = less difference between low and high)
plot_max = 8  # maximum height in rows to scale the highest line (high values = more crossing lines)
plot_smoothing = 14  # How many x-axis points to smooth using rolling averages

# Plotting parameters, Europe
plot_spacing = 16  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.65  # exponent factor for elevations (small values = less difference between low and high)
plot_max = 5  # maximum height in rows to scale the highest line (high values = more crossing lines)
plot_smoothing = 14  # How many x-axis points to smooth using rolling averages

# # Plotting parameters, Mexico
# plot_spacing = 4  # select every xth y-axis row for plotting (small values = finely spaced lines) 
# plot_exp = 0.8  # exponent factor for elevations (small values = less difference between low and high)
# plot_max = 5  # maximum height in rows to scale the highest line (high values = more crossing lines)
# plot_smoothing = 4  # How many x-axis points to smooth using rolling averages

# Plotting parameters, Australia
plot_spacing = 18  # select every xth y-axis row for plotting (small values = finely spaced lines) 
plot_exp = 0.83  # exponent factor for elevations (small values = less difference between low and high)
plot_max = 7  # maximum height in rows to scale the highest line (high values = more crossing lines)
plot_smoothing = 13  # How many x-axis points to smooth using rolling averages

# # Plotting parameters, Canada
# plot_spacing = 26  # select every xth y-axis row for plotting (small values = finely spaced lines) 
# plot_exp = 0.7  # exponent factor for elevations (small values = less difference between low and high)
# plot_max = 5  # maximum height in rows to scale the highest line (high values = more crossing lines)
# plot_smoothing = 18  # How many x-axis points to smooth using rolling averages


############
# Analysis #
############

# Import raster and set areas of low elevation to NA
dem = raster(input_raster)
plot(dem)

# Convert to dataframe of points
dem_points = as_data_frame(rasterToPoints(dem)) 
colnames(dem_points) = c("x", "y", "elevation")

# Convert elevation data to relative heights
dem_df = dem_points %>% 

  # Extract every nth unique Y value (i.e. row)
  filter(y %in% unique(y)[ c( rep(FALSE, plot_spacing), TRUE ) ]) %>% 
  
  # For improved visual impact in low elevation areas, use exponent
  mutate(height = ifelse(elevation > 0, elevation ^ plot_exp, NA)) %>% 
  
  # For each row, apply smoothing window to reduce noise
  group_by(y) %>% 
  mutate(height = rollapply(height, width = plot_smoothing, FUN = mean, partial = TRUE,  align = "center")) %>%  
  
  # Finally, stretch between 0 and maximum height in rows, then remove 0 elevation lines
  ungroup() %>% 
  mutate(height = rescale(height, to = c(0, plot_max * plot_spacing * yres(dem))))


############
# Plotting #
############

dem_plot = ggplot(data = dem_df) + 
  geom_ridgeline(aes(x = x, y = y, height = height, group = y), fill = "white", size = 0.45) +
  coord_fixed() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.background = element_blank(), 
        panel.background = element_blank())

# ggsave(output_file, dem_plot, dpi = 508*2, height = 100/2, width = 70.478/2, units = "cm", type = "cairo-png")
ggsave(output_file, dem_plot, dpi = 508*2, width = 100/2, height = 70.478/2, units = "cm", type = "cairo-png")
