library(ggplot2)
library(rayshader)
ggdiamonds = ggplot(diamonds) +
        stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)), 
                        geom = "polygon", n = 100, bins = 10, contour = TRUE) +
        facet_wrap(clarity~.) +
        scale_fill_viridis_c(option = "A")

par(mfrow = c(1, 2))

plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot(clear = TRUE)


mtplot = ggplot(mtcars) + 
        geom_point(aes(x = mpg, y = disp, color = cyl)) + 
        scale_color_continuous(limits = c(0, 8))

par(mfrow = c(1, 2))
plot_gg(mtplot, width = 3.5, raytrace = FALSE, preview = TRUE)

plot_gg(mtplot, width = 3.5, multicore = TRUE, windowsize = c(800, 800), 
        zoom = 0.85, phi = 35, theta = 30, sunangle = 225, soliddepth = -100)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)


a = data.frame(x = rnorm(20000, 10, 1.9), y = rnorm(20000, 10, 1.2))
b = data.frame(x = rnorm(20000, 14.5, 1.9), y = rnorm(20000, 14.5, 1.9))
c = data.frame(x = rnorm(20000, 9.5, 1.9), y = rnorm(20000, 15.5, 1.9))
data = rbind(a, b, c)

#Lines
pp = ggplot(data, aes(x = x, y = y)) +
        geom_hex(bins = 20, size = 0.5, color = "black") +
        scale_fill_viridis_c(option = "C")

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)


par(mfrow = c(1, 1))
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.4, theta = 330, phi = 40)
Sys.sleep(0.2)
render_depth(focus = 0.68, focallength = 200,clear=TRUE)

par(mfrow = c(1, 1))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.4, theta = 330, phi = 20,  max_error = 0.01, verbose = TRUE)


Sys.sleep(0.2)
render_highquality(samples = 400, aperture=30, light = FALSE, ambient = TRUE,focal_distance = 1700,
                   obj_material = rayrender::dielectric(attenuation = c(1,1,0.3)/200), 
                   ground_material = rayrender::diffuse(checkercolor = "grey80",sigma=90,checkerperiod = 100))
