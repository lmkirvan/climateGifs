#ul <- "https://www.ngdc.noaa.gov/mgg/inundation/sandy/data/tiles/zip19/ncei19_n41x00_w074x25_2015v1.tif"
#ul <- curl::curl_download(ul, "ul.tif")
#ur <- "https://www.ngdc.noaa.gov/mgg/inundation/sandy/data/tiles/zip19/ncei19_n41x00_w074x00_2015v1.tif"
#ur <- curl::curl_download(ur, "ur.tif")
#ll <- "https://www.ngdc.noaa.gov/mgg/inundation/sandy/data/tiles/zip19/ncei19_n40x75_w074x25_2015v1.tif"
#ll <- curl::curl_download(ll, "ll.tif") 
#lr <- "https://www.ngdc.noaa.gov/mgg/inundation/sandy/data/tiles/zip19/ncei19_n40x75_w074x00_2015v1.tif"
#lr <- curl::curl_download(lr, "lr.tif") 

library(rayshader)
library(tidyverse)
library(rgl)
rgl::rgl.useNULL()
#options(rgl.useNULL = TRUE)
#options(rgl.printRglwidget = TRUE)
#Sys.setenv(DISPLAY=":7")

get_squares <- function(le, mat){
  mat[le[[1]][1]:le[[1]][2], le[[2]][1]:le[[2]][2]]
}

get_matrix_squares <- function(m, rows_out = 100, cols_out = 100){             
  nrows <- nrow(m)
  ncols <- ncol(m)
  
  r_rounded <- trunc(seq(1, nrows, length.out = rows_out + 1))
  c_rounded <- trunc(seq(1, ncols, length.out = cols_out + 1))
  
  r_rounded_shifted <- r_rounded[-1]
  c_rounded_shifted <- c_rounded[-1]
  
  r_rounded <- r_rounded[-length(r_rounded)]
  c_rounded <- c_rounded[-length(c_rounded)]
  
  c_pairs <- purrr::map2(c_rounded, c_rounded_shifted, ~c(.x,.y))
  r_pairs <- purrr::map2(r_rounded, r_rounded_shifted, ~c(.x,.y))
  
  purrr::cross(list(r_pairs, c_pairs))
}

aggregate_matrix <- function(m, rows_out = 500, cols_out = 500, fun = min, ...){
  matrix_squares <- get_matrix_squares(m, rows_out, cols_out)
  
  values <- purrr::map(matrix_squares, get_squares, mat = m) %>% 
    purrr::map_dbl(~fun(., ...))
  
  matrix(values, rows_out, cols_out, byrow = T)
  
}

ll <- raster::raster("ll.tif") %>% 
  raster::as.matrix() %>% 
  aggregate_matrix(300, 300, fun = min, na.rm = T)

ul <- raster::raster("ul.tif") %>% 
  raster::as.matrix() %>% 
  aggregate_matrix(300, 300, fun = min, na.rm = T)

ur <- raster::raster("ur.tif") %>% 
  raster::as.matrix() %>% 
  aggregate_matrix(300, 300, fun = min, na.rm = T)

lr <- raster::raster("lr.tif") %>% 
  raster::as.matrix() %>% 
  aggregate_matrix(300, 300, fun = min, na.rm = T)

top <- cbind(ul, ll)
bottom <- cbind(ur, lr)
values <- rbind(top, bottom)

shadow <- ray_shade(values, zscale=27.04)
amb <- ambient_shade(values,zscale=27.04)

rgl::rgl.open()

values %>% 
  sphere_shade(zscale=91.936,texture = "imhof1") %>% 
  add_shadow(shadow, 0.7) %>%
  add_shadow(amb) %>%
  plot_3d(
    values, 
    zscale=27.04,
    fov=0,
    theta=-45,
    phi=45,
    windowsize=c(1000,800),
    zoom=0.75,
    water=TRUE, 
    waterdepth = 0, 
    wateralpha = 0.2,
    watercolor = "#20b2aa",
    waterlinecolor = "white",
    waterlinealpha = 0.7) 

render_label(values, "Staten Island", x = 125, y = 75, z = 50)
render_label(values, "Manhattan", x = 310, y = 300, z = 50)
render_label(values, "Bronx", x = 400, y = 375, z = 50)
render_label(values, "Brooklyn", x = 290, y = 160, z = 50)
render_label(values, "Queens", x = 400, y = 300, z = 50)  
render_label(values, "Your New York", 520, 520, 75, 
            linewidth = 0, textsize = 2, alpha = 0)

render_snapshot('snapshot.png')

rgl::snapshot3d(filename = "temp.png")
