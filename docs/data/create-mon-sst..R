# URL 1979-2020 http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_d124_2bb9_c935.htmlTable?sst[(2021-02-01T00:00:00Z):1:(2021-02-01T00:00:00Z)][(7):1:(13)][(72):1:(78)]
# URL 1950-1978 http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_26fe_f2c9_ed3b.htmlTable?sst[(1950-01-01T00:00:00Z):1:(1978-12-01T00:00:00Z)][(7):1:(13)][(72):1:(78)]
file <- 'data/hawaii_soest_d124_2bb9_c935-5-27-48-80-1979-01-01-2020-06-01.csv'
aspect_ratio <- c(89,129)
lat_range <- c(7,13)
long_range <- c(72,78)
mon.sst <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                      lat_range=lat_range,
                      long_range=long_range)$dat