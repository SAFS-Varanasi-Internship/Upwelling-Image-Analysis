# URL 1979-2020 http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_d124_2bb9_c935.htmlTable?sst[(2021-02-01T00:00:00Z):1:(2021-02-01T00:00:00Z)][(7):1:(13)][(72):1:(78)]
# URL 1950-1978 http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_26fe_f2c9_ed3b.htmlTable?sst[(1950-01-01T00:00:00Z):1:(1978-12-01T00:00:00Z)][(7):1:(13)][(72):1:(78)]
file <- 'data/hawaii_soest_d124_2bb9_c935-5-27-48-80-1979-01-01-2020-06-01.csv'
aspect_ratio <- c(89,129)
lat_range <- c(7,13)
long_range <- c(72,78)
mon.sst <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                      lat_range=lat_range,
                      long_range=long_range)$dat


file <- 'data/hawaii_soest_26fe_f2c9_ed3b-7-13-72-78-1950-01-01-1978-12-01.csv'
lat_range <- c(7,13)
long_range <- c(72,78)
aspect_ratio <- c(1+(lat_range[2]-lat_range[1])/0.25,1+(long_range[2]-long_range[1])/0.25)
out <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                      lat_range=lat_range,
                      long_range=long_range)
save(out, file="data/SEAS-monthly-sst-1950-1978.RData")

datafile <- file.path(here::here(), "data", "SEAS-monthly-sst-1950-1978.RData")
load(datafile)
dat <- out$dat
dat.clean <- out$dat.clean
pos.loc <- out$pos.loc
datafile <- file.path(here::here(), "data", "SEAS-monthly-sst-1979-2020.RData")
load(datafile)
dat <- rbind(dat, out$dat[c(-1,-2),])
dat.clean <- rbind(dat.clean, out$dat.clean[c(-1,-2),])
out <- list(dat=dat, dat.clean=dat.clean, pos.loc=pos.loc)
save(out, file="data/SEAS-monthly-sst.RData")

# IOSST Daily data
getdata("hawaii_soest_afc8_9785_907e", pars="sst", lat=c(7,13)+0.125, lon=c(72,78)+0.125, 
        date=c("1981-09-01","1981-12-31"))
for(i in 1983:2015){
  cat(i, " ")
  getdata("hawaii_soest_afc8_9785_907e", pars="sst", lat=c(7,13)+0.125, lon=c(72,78)+0.125, 
        date=c(paste0(i,"-01-01"), paste0(i,"-12-31")))
}
# IOSST Daily data 2016+
for(i in 2016:2019){
  cat(i, " ")
  getdata("hawaii_soest_330b_094e_ca45", pars="sst", 
          altitude=0, alt.name="LEV",
          lat=c(7,13)+0.125, lon=c(72,78)+0.125, 
          date=c(paste0(i,"-01-01"), paste0(i,"-12-31")))
}
# Weird problem with 2020
i = 2020
cat(i, " ")
getdata("hawaii_soest_330b_094e_ca45", pars="sst", 
        altitude=0, alt.name="LEV",
        lat=c(7,13)+0.125, lon=c(72,78)+0.125, 
        date=c(paste0(i,"-01-01"), paste0(i,"-02-27")))
getdata("hawaii_soest_330b_094e_ca45", pars="sst", 
        altitude=0, alt.name="LEV",
        lat=c(7,13)+0.125, lon=c(72,78)+0.125, 
        date=c(paste0(i,"-02-28"), paste0(i,"-12-31")))

i <- 1981
file <- paste0("data/hawaii_soest_afc8_9785_907e-7.125-13.125-72.125-78.125-", i, "-09-01-", i, "-12-31.csv")
lat_range <- c(7,13)+0.125
long_range <- c(72,78)+0.125
aspect_ratio <- c(1+(lat_range[2]-lat_range[1])/0.25,1+(long_range[2]-long_range[1])/0.25)
out <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                  lat_range=lat_range,
                  long_range=long_range)
dat <- out$dat
dat.clean <- out$dat.clean
pos.loc <- out$pos.loc
for(i in 1982:2015){
  file <- paste0("data/hawaii_soest_afc8_9785_907e-7.125-13.125-72.125-78.125-", i, "-01-01-", i, "-12-31.csv")
  out <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                    lat_range=lat_range,
                    long_range=long_range)
  dat <- rbind(dat, out$dat[c(-1,-2),])
  dat.clean <- rbind(dat.clean, out$dat.clean[c(-1,-2),])
}
for(i in 2016:2019){
  file <- paste0("data/hawaii_soest_330b_094e_ca45-7.125-13.125-72.125-78.125-", i, "-01-01-", i, "-12-31.csv")
  out <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                    lat_range=lat_range,
                    long_range=long_range, has.alt=TRUE)
  dat <- rbind(dat, out$dat[c(-1,-2),])
  dat.clean <- rbind(dat.clean, out$dat.clean[c(-1,-2),])
}
i <- 2020
file <- paste0("data/hawaii_soest_330b_094e_ca45-7.125-13.125-72.125-78.125-", i, "-01-01-", i, "-02-27.csv")
out <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                  lat_range=lat_range,
                  long_range=long_range, has.alt=TRUE)
dat <- rbind(dat, out$dat[c(-1,-2),])
dat.clean <- rbind(dat.clean, out$dat.clean[c(-1,-2),])
file <- paste0("data/hawaii_soest_330b_094e_ca45-7.125-13.125-72.125-78.125-", i, "-02-28-", i, "-12-31.csv")
out <- processCSV(file=file, aspect_ratio=aspect_ratio, 
                  lat_range=lat_range,
                  long_range=long_range, has.alt=TRUE)
dat <- rbind(dat, out$dat[c(-1,-2),])
dat.clean <- rbind(dat.clean, out$dat.clean[c(-1,-2),])

out <- list(dat=dat, dat.clean=dat.clean, pos.loc=pos.loc)
save(out, file="data/SEAS-daily-sst-1981-2020.RData")



