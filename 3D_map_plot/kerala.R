library(tidyverse)
library(sf)
library(rayshader)

# Read Shapefile
s.sf <- st_read("final_combined.shp")

# plot
kk=ggplot(s.sf)+
        geom_sf(aes(fill = Active))+
        scale_fill_gradient(low = "#f5af19", high = "#f12711")+
        ggtitle("Active Cases")

plot_gg(kk, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))

#change res
plot_gg(kk, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.4, theta = 330, phi = 40)

# added attributes
par(mfrow = c(1, 1))
plot_gg(kk, width = 5, height = 4, scale = 300, raytrace = FALSE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.4, theta = 330, phi = 20,  max_error = 0.01, verbose = TRUE)

#Save plot as png
render_snapshot('kk1.png')

Sys.sleep(0.2)
render_highquality(samples = 400, aperture=30, light = FALSE, ambient = TRUE,focal_distance = 1700,
                   obj_material = rayrender::dielectric(attenuation = c(1,1,0.3)/200), 
                   ground_material = rayrender::diffuse(checkercolor = "grey80",sigma=90,checkerperiod = 100))

