library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(raster)
library(sf)
library(plotly)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

peta_dunia <- map_data('world')
head(peta_dunia)

#load data pengguna internet
internet_user <- read.csv("https://raw.githubusercontent.com/sherlyprastica/digital_society_vs_unemployment_analysis/main/persebaran_pengguna_internet_fix_202203161536.csv")
internet_user <- internet_user %>% mutate(persentase_2015 = (`X2015_fix`/jumlah_penduduk_2015)*100, persentase_2016 = (`X2016_fix`/jumlah_penduduk_2016)*100, persentase_2017 = (`X2017_fix`/jumlah_penduduk_2017)*100, persentase_2018 = (`X2018_fix`/jumlah_penduduk_2018)*100, persentase_2019 = (`X2019_fix`/jumlah_penduduk_2019)*100, persentase_2020 = (`X2020_fix`/jumlah_penduduk_2020)*100)
internet_user <- internet_user %>% rename(NAME_1=ï..NAME_1)

#show map all world
ggplot(peta_dunia, aes(x=long, y=lat, group=group)) + geom_polygon(fill='lightgray', colour='white')

#show indonesian map
peta_indonesia <- map_data('world', region = 'Indonesia')
indonesia_map <- ggplot(peta_indonesia, aes(x=long, y=lat, group=group)) + geom_polygon(fill='lightgray', colour='white')
# Indonesia terletak antara 6° LU - 11° LS dan 95° BT - 141° BT.
indonesia_map <- ggplot(peta_dunia, aes(x=long, y=lat, group=group)) + geom_polygon(fill='lightgray', colour='white') + xlim(95, 141) + ylim(-11, 6)
#memotong berdasar provinsi
indonesia1 <- getData('GADM', country='IDN', level=1)
ind1 <- readRDS('gadm36_IDN_1_sp.rds')
ind1A <- fortify(ind1)
warna <- rainbow(length(unique(ind1A$id)))
ggplot(peta_indonesia, aes(x=long, y=lat, group=group)) + geom_polygon(data=ind1A, aes(x=long, y=lat, group=group, fill=id), color='grey')
ind1@data$id <- rownames(ind1@data)
ind1B <- plyr::join(ind1A, ind1@data, by="id")
ind2B <- plyr::join(ind1B, internet_user, by="NAME_1")
viz_map <- ggplot(peta_indonesia, aes(x=long, y=lat, group=group)) + geom_polygon(data=ind2B, aes(x=long, y=lat, group=group, fill=`persentase_2020`), color='grey') + labs(title="Peta Persebaran Pengguna Internet di Indonesia pada Tahun 2020")
viz_map <- ggplotly(viz_map)
viz_map