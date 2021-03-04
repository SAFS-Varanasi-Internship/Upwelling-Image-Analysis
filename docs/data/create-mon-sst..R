file <- 'data/hawaii_soest_d124_2bb9_c935-5-27-48-80-1979-01-01-2020-06-01.csv'
aspect_ratio <- c(89,129)
lat_range <- c(7,13)
long_range <- c(72,78)
mon.sst <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                      lat_range=lat_range,
                      long_range=long_range)$dat