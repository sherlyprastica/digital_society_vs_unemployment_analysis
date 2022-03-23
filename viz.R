library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(plotly)

#----------------------------------------------#
#Data Penduduk Indonesia dan Pengguna Internet
jumlah_penduduk <- read_xlsx('../jumlah_penduduk/jumlah_penduduk_fix.xlsx')

jumlah_penduduk <- jumlah_penduduk %>% select(Tahun:Pengguna_Internet_Percent) %>% mutate(percent_internet = Pengguna_Internet_Percent*100) %>% mutate(internet_user_percentage = sprintf("%1.1f%%", percent_internet))
jumlah_penduduk <- jumlah_penduduk %>% select(Tahun, Jumlah_Penduduk, Pengguna_Internet, internet_user_percentage)
sepuluh_tahun_trakhir <- jumlah_penduduk %>% filter(between(Tahun,2011, 2020))
head(jumlah_penduduk)

#Grafik internet user terhadap populasi penduduk indonesia
viz <- ggplot(data = sepuluh_tahun_trakhir, aes(Tahun)) + scale_x_continuous(breaks = c(2011:2020)) + geom_col(aes(y=Jumlah_Penduduk), color="black", fill="lightblue") 
viz <- viz + geom_text(aes(y=Jumlah_Penduduk, label=Jumlah_Penduduk), position=position_dodge(width=1), vjust=-1)
viz <- viz + geom_col(aes(y=Pengguna_Internet), color="black", fill="black")
viz <- viz + scale_y_continuous(sec.axis = sec_axis(~., name = "User Internet"))
viz <- viz + geom_text(aes(y=Pengguna_Internet, label=sprintf("%1.1f%%", (Pengguna_Internet/Jumlah_Penduduk)*100)), position=position_dodge(width=1), vjust=-1)
viz <- viz + labs(x="Tahun", y="Jumlah Penduduk", title="Banyak Pengguna Internet Terhadap Populasi Masyarakat Indonesia pada Tahun 2011-2020")
viz <- viz + theme(axis.text.x = element_text(hjust = 0.5, size=15), plot.title = element_text(hjust = 0.5, vjust = 1))
viz <- ggplotly(viz)


#grafik Korelasi populasi penduduk indonesia dengan pertambahan internet user
viz2 <- ggplot(data = jumlah_penduduk, mapping = aes(x = Jumlah_Penduduk, y = Pengguna_Internet)) + geom_point(color="black", size=2)
viz2 <- viz2 + geom_line(mapping = aes(x = Jumlah_Penduduk, y = Pengguna_Internet), data = jumlah_penduduk, color="black", size=1)
viz2 <- viz2 + labs(x="Jumlah Penduduk", y="Jumlah Pengguna Internet", title="Apakah Pertambahan Jumlah Penduduk Mempengaruhi Pertambahan Penggunaan Internet ?")
viz2 <- viz2 + theme(axis.text.x = element_text(hjust = 0.5), axis.text.y.left = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, vjust = 1), plot.caption = element_text(size = 8))
viz2 <- ggplotly(viz2)

#-----------------------------------#
#Data internet user berdasarkan umur
user_internet_umur <- read_xlsx('../data persebaran internet/pengguna internet berdasarkan umur.xlsx')
user_internet_umur <- user_internet_umur %>% select(Umur, mean)
View(user_internet_umur)

#pie chart pengguna internet berdasar umur (persen)
library(scales)
library(ggrepel)

#setting position of label
df <- user_internet_umur %>% mutate(csum = rev(cumsum(rev(mean))), pos = mean/1 + lead(csum, 1),pos = if_else(is.na(pos), mean/3, pos))

#blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

viz3 <- ggplot(data=user_internet_umur, aes(x="", y=mean, fill=Umur)) 
viz3 <- viz3 + geom_bar(width = 1, stat = "identity")
viz3 <- viz3 + coord_polar("y", start=0)
viz3 <- viz3 + geom_label_repel(data = df, aes(y = pos, label = sprintf("%1.1f%%", mean)), size = 8, nudge_x = 1, show.legend = FALSE)
viz3 <- viz3 + theme(axis.text.x=element_blank()) + blank_theme
viz3 <- viz3 + labs(title = "Rata-rata Pengguna Internet Berdasarkan Umur") + theme(plot.title = element_text(hjust = 0.5, vjust = 1))
viz3

#---------------------------------#
#grafik pengangguran
pengangguran <- read_xlsx("../data pengangguran/fix_pengangguran.xlsx")
names(pengangguran) <- make.names(names(pengangguran))
pengangguran <- pengangguran %>% filter(Jumlah.Pengangguran > 0)
mean_pengangguran <- pengangguran %>% select(Tahun, Jumlah.Pengangguran) %>% mutate(rata_pengangguran = mean(Jumlah.Pengangguran))
head(pengangguran)

viz4 <- ggplot(data=pengangguran, aes(x=Tahun, y=Jumlah.Pengangguran)) + geom_line() + geom_line(data=mean_pengangguran, aes(y=rata_pengangguran), color="red")
viz4 <- viz4 + labs(y="Jumlah Pengangguran", title="Grafik Jumlah Pengangguran di Indonesia Tahun 1990-2020") + theme(plot.title = element_text(hjust = 0.5, vjust = 1))
viz4 <- ggplotly(viz4)

#grafik korelasi jumlah pengguna internet dengan jumlah pengangguran
viz5 <- ggplot(data=pengangguran, aes(y=pengguna_internet, x=Jumlah.Pengangguran)) + geom_point(size=2)
viz5 <- viz5 + labs(x="Jumlah Pengangguran", y="Pengguna Internet", title="Korelasi Antara Jumlah Pengangguran dengan Jumlah Pengguna Internet") + theme(plot.title = element_text(hjust = 0.5, vjust = 1))
viz5 <- ggplotly(viz5)

#grafik pengangguran berdasarkan umur
pengangguran_umur <- read_xlsx("../data pengangguran/pengangguran_umur.xlsx")
pengangguran_umur <- pengangguran_umur %>% arrange(umur)
#View(pengangguran_umur)

viz6 <- ggplot(data = pengangguran_umur, aes(umur)) + geom_col(aes(y=tahun_2020), color="black", fill="lightblue") 
viz6 <- viz6 + geom_text(aes(y=tahun_2020, label=tahun_2020), position=position_dodge(width=1), vjust=-1)
viz6 <- viz6 + labs(x="Umur", y="Persentase", title="Persentase Penduduk Belum Bekerja atau Pengangguran Berdasarkan Umur Tahun 2020")
viz6 <- viz6 + theme(axis.text.x = element_text(hjust = 0.5, size=15), plot.title = element_text(hjust = 0.5, vjust = 1))
viz6 <- ggplotly(viz6)
