library(shiny)
library(dplyr)
library(readxl)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(scales)
library(ggrepel)
library(tidyverse)
library(tidyr)
library(raster)
library(sf)
library(rsconnect)
require(maps)
require(viridis)
theme_set(
  theme_void()
)


#load data--------------------------------------------------------------------------------------------
jumlah_penduduk <- read_xlsx('./jumlah_penduduk_fix.xlsx')
sepuluh_tahun_trakhir <- jumlah_penduduk %>% filter(between(`Tahun`,2011, 2020))
#head(jumlah_penduduk)

pengangguran <- read_xlsx("./fix_pengangguran.xlsx")
names(pengangguran) <- make.names(names(pengangguran))
pengangguran <- pengangguran %>% filter(Jumlah.Pengangguran > 0)
mean_pengangguran <- pengangguran %>% dplyr::select(`Tahun`, Jumlah.Pengangguran) %>% mutate(rata_pengangguran = mean(Jumlah.Pengangguran))


user_internet_umur <- read_xlsx('./pengguna internet berdasarkan umur.xlsx')
user_internet_umur <- user_internet_umur %>% dplyr::select(`Umur`, mean)
#View(user_internet_umur)

#peta_dunia <- map_data('world')
#head(peta_dunia)

#load data pengguna internet
internet_user <- read_xlsx("./persebaran_pengguna_internet_fix_202203161536.xlsx")
internet_user <- internet_user %>% mutate(persentase_2015 = (`2015_fix`/jumlah_penduduk_2015)*100, persentase_2016 = (`2016_fix`/jumlah_penduduk_2016)*100, persentase_2017 = (`2017_fix`/jumlah_penduduk_2017)*100, persentase_2018 = (`2018_fix`/jumlah_penduduk_2018)*100, persentase_2019 = (`2019_fix`/jumlah_penduduk_2019)*100, persentase_2020 = (`2020_fix`/jumlah_penduduk_2020)*100)
#internet_user <- internet_user %>% rename(NAME_1=?..NAME_1)
#View(internet_user)

#grafik pengangguran berdasarkan umur
pengangguran_umur <- read_xlsx("./pengangguran_umur.xlsx")
pengangguran_umur <- pengangguran_umur %>% arrange(`umur`)

#grafik pengangguran
pengangguran <- read_xlsx("./fix_pengangguran.xlsx")
names(pengangguran) <- make.names(names(pengangguran))
pengangguran <- pengangguran %>% filter(Jumlah.Pengangguran > 0)
mean_pengangguran <- pengangguran %>% dplyr::select(`Tahun`, Jumlah.Pengangguran) %>% mutate(rata_pengangguran = mean(Jumlah.Pengangguran))


#setting for viz theme-----------------------------------------------------------------------------------------------------
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

#shiny ------------------------------------------------------------------------------------------
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Digital SOciety vs. Unemployment Cases in Indonesia", titleWidth = 1000),
  dashboardSidebar(
    menuItem("Article", tabName = "background", icon = icon("dashboard")),
    menuItem("Graph", icon = icon("bar-chart-o"), startExpanded = TRUE,
             menuSubItem("Data about Internet User", tabName = "digital"),
             menuSubItem("Data about Unemployment Cases", tabName = "unemployment"),
             menuSubItem("Correlation", tabName = "correlation")
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 10, background="yellow",
        h1("Digital Society vs Unemployment Cases in Indonesia"),
        h3("Apakah benar Digital Society di Indonesia mempengaruhi kasus pengangguran di Indonesia ?"),
        h5("Let's check for more information"),
        h6("\n Data Source : bps.go.id, Asosiasi Penyelenggara Jasa Internet Indonesia, data.worldbank.org, databooks.katadata.co.id")
      )
    ),
    tabItems(
      tabItem("background",
        h1("Memasuki Era Society 5.0 dan Persiapan SDM Indonesia Menghadapi Digital Society"),
        h3("Perkembangan teknologi ke arah serba digital  saat ini semakin pesat. Pada era digital seperti ini, manusia secara umum memiliki gaya hidup baru yang tidak bisa dilepaskan dari perangkat yang serba elektronik. Peran penting teknologi inilah yang membawa peradaban manusia memasuki era digital. Era digital telah membawa berbagai perubahan yang baik sebagai dampak positif yang bisa digunakan sebaik-baiknya. Namun dalam waktu yang bersamaan, era digital juga membawa banyak dampak negatif, sehingga menjadi tantangan baru dalam kehidupan manusia di era digital ini. Tantangan pada era digital telah pula masuk ke dalam berbagai bidang seperti politik, ekonomi, sosial budaya, pertahanan, keamanan, dan teknologi informasi itu sendiri."),
        h3("Sedikit pendahuluan, Digital Society  adalah kerangka berpikir mendasar antara interaksi manusia dengan teknologi yang bertujuan untuk membuat kehidupan yang lebih baik, jadi bisa dibilang Society 5.0 adalah solusi dari revolusi industri 4.0, jika pada revolusi industri 4.0 menggaungkan kecerdasan buatan maka di era Society 5.0 adalah bagaimana manusia bisa memanfaatkan teknologi tersebut guna menuju kehidupan yang lebih baik di masa mendatang.
           Lalu apakah kita sudah siap menghadapi era tersebut? Garis besarnya adalah kesiapan yang kurang dari pemerintah
           Indonesia dalam menghadapi Era Society 5.0, lalu tingginya angka pengangguran  di Indonesia dan yang terakhir kekeliruan masyarakat Indonesia dalam menggunakan teknologi digital."),
        h2("Bersama dengan itu, data yang diambil dari Badan Pusat Statistik melaporkan jumlah pengangguran periode Agustus 2020 mengalami peningkatan sebanyak 2,67 juta orang. Dengan demikian, jumlah angkatan kerja di Indonesia yang menganggur menjadi sebanyak 9,77 juta orang. Angka tersebut sudah masuk kedalam kategori cukup banyak. Dengan angka pengangguran yang cukup tinggi bukan tidak mungkin Indonesia sulit beradaptasi dengan era Society 5.0."),
        h3("Jika dilihat dari alasan yang dipaparkan diatas dan melonjaknya angka pengangguran yang terjadi di Indonesia itu bisa saja terjadi karena kurangnya daya saing SDM Indonesia, seharusnya pemerintah mengevaluasi proses pembelajaran dari tingkat yang paling rendah hingga ke tingkat perguruan tinggi, dan mengubah kurikulum yang ada lalu disesuaikan dengan kebutuhan pasar, agar terciptanya SDM yang kompetitif dan mumpuni guna menghadapi Era Society 5.0. Bisa juga dengan mengadakan pelatihan kerja yang mengarah kepada kesiapan SDM Indonesia untuk memahami dan menguasai segala aspek yang dibutuhkan oleh SDM Indonesia dalam era digital ini, guna mengurangi angka pengangguran yang ada di Indonesia.
           Tetapi, fakta yang terjadi hari ini pemerintah kurang mengedukasi dan mengevaluasi apa yang saat ini terjadi dan apa yang akan kita hadapi, pemerintah belum mengambil langkah banyak dalam mempersiapkan SDM yang kompetitif untuk era ini sehingga lulusan dari pendidikan di Indonesia kurang dicari dan kurang diminati karena tidak sesuai dengan kebutuhan pasar saat ini. Lalu banyaknya masyarakat Indonesia yang kurang bisa mengoperasikan teknologi digital yang ada karena keterbatasan pemahaman, dan kurangnya kesiapan sehingga menimbulkan angka pengangguran yang cukup tinggi.
           Menyinggung masalah tingginya angka pengangguran yang terjadi di Indonesia maka pembahasan ini tidak bisa dipisahkan dengan ketersediaan lapangan pekerjaan yang ada. Timbul pertanyaan, di era Society 5.0 yang akan kita hadapi apa sebenarnya pekerjaan yang relevan dan banyak dibutuhkan? Dilansir dari akun instagram ditjen dikti ada beberapa pekerjaan yang cocok di era Society 5.0, seperti Web Developer, App Developer, SEO (Search Engine Optimisation), Content Creator, dan juga Social Media Specialist. Jika ditelisik kembali pekerjaan yang disebutkan adalah pekerjaan yang berhubungan dengan teknologi informasi, maka pendidikan yang mengarah kesana dan juga pengembangan SDM untuk pemahaman tentang teknologi informasi bisa menjadi senjata ampuh bagi Indonesia untuk bisa menekan angka pengangguran dan semakin siap menghadapi era Society 5.0.
           Walau begitu, sebenarnya Indonesia masih memiliki peluang, bahkan sangat besar. Asal kita terus meningkatkan Digital
           Skill yang diperlukan untuk bersaing di era Society 5.0, meningkatkan digital skill ini harus dilakukan di banyak sektor, dan fokus ke masyarakat juga tak kalah penting, bukan hanya fokus ke teknologinya saja. Kita tidak boleh takut pekerjaan akan digantikan oleh robot atau mesin, karena tetap manusia lah fokusnya. Pada era ini yang akan bisa tetap survive adalah bukan mereka yang kuat dan juga bukan mereka yang pintar, tetapi mereka yang akan tetap bisa survive adalah mereka yang bisa beradaptasi.
           Ada banyak yang bisa kita jadikan contoh dalam menghadapi Society 5.0 salah satunya adalah Negara Estonia, di sana, pemerintah memanfaatkan teknologi digital untuk membantu dan memudahkan warga negaranya mengakses berbagai fasilitas penting seperti kesehatan, pendidikan, dan lainnya. Semua itu demi mewujudkan kehidupan masyarakat yang lebih baik lagi untuk hari ini, dan yang akan datang.
           "),
        h4("source : https://www.kompasiana.com/riorohmi/606e6fae8ede4847cb29b3f2/memasuki-era-society-5-0-dan-persiapan-sdm-indonesia-menghadapi-digital-society 
           ")
      ),
      tabItem("digital",
              h3("Graph of Digital Society Data"),
              box(plotlyOutput("plot1"), width = 20),
              box(imageOutput("map"), width = 20),
              box(plotOutput("plot2"), width = 20)
      ),
      tabItem("unemployment",
              h3("Graph of Unemployment Cases"),
              box(plotlyOutput("plot3"), width = 20),
              box(selectInput(
                "feature2", "Data :", c("Kondisi Pengangguran di Indonesia","Pengangguran di Indonesia Berdasarkan Umur")
              ), width = 20, background="yellow")
      ),
      tabItem("correlation",
              h3("Graph of correlation of the Data"),
              box(plotlyOutput("plot4"), width = 20),
              box(selectInput(
                "feature3", "Data :", c("Korelasi Antara Banyaknya Jumlah Penduduk dengan Jumlah Pengguna Internet","Korelasi Antara Banyaknya Pengguna Internet dengan Banyaknya Penduduk yang Pengangguran")
              ), width = 20, background="yellow")
      )
  )
)
)

server <- function(input,output){
  observe({
      output$plot1 <- renderPlotly({
        viz <- ggplot(data = sepuluh_tahun_trakhir, aes(`Tahun`)) + scale_x_continuous(breaks = c(2011:2020)) + geom_col(aes(y=Jumlah_Penduduk), color="black", fill="lightblue") 
        viz <- viz + geom_text(aes(y=Jumlah_Penduduk, label=Jumlah_Penduduk), position=position_dodge(width=1), vjust=-1)
        viz <- viz + geom_col(aes(y=Pengguna_Internet), color="black", fill="black")
        viz <- viz + scale_y_continuous(sec.axis = sec_axis(~., name = "User Internet"))
        viz <- viz + geom_text(aes(y=Pengguna_Internet, label=sprintf("%1.1f%%", (Pengguna_Internet/Jumlah_Penduduk)*100)), position=position_dodge(width=1), vjust=-1)
        viz <- viz + labs(x="Tahun", y="Jumlah Penduduk", title="Banyak Pengguna Internet Terhadap Populasi Masyarakat Indonesia pada Tahun 2011-2020")
        viz <- viz + theme(axis.text.y = element_text(hjust = 0.5, size=8), axis.text.x = element_text(hjust = 0.5, size=15), plot.title = element_text(hjust = 0.5, vjust = 1))
        viz <- ggplotly(viz)
        viz
      })

      output$plot2 <- renderPlot({
        viz3 <- ggplot(data=user_internet_umur, aes(x="", y=mean, fill=`Umur`)) 
        viz3 <- viz3 + geom_bar(width = 1, stat = "identity")
        viz3 <- viz3 + coord_polar("y", start=0)
        viz3 <- viz3 + geom_label_repel(data = df, aes(y = pos, label = sprintf("%1.1f%%", mean)), size = 8, nudge_x = 1, show.legend = FALSE)
        viz3 <- viz3 + theme(axis.text.x=element_blank()) + blank_theme
        viz3 <- viz3 + labs(title = "Rata-rata Pengguna Internet Berdasarkan Umur") + theme(plot.title = element_text(hjust = 0.5, vjust = 1))
        viz3
      })
    
      output$map <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./fix', paste('fix', input$n, '.png', sep='')))
        # Return a list containing the filename
        list(src = "./fix.png")
        }, deleteFile = FALSE)
  
  })
  
  observe({
  if(input$feature2 == "Kondisi Pengangguran di Indonesia"){
    output$plot3 <- renderPlotly({
      viz4 <- ggplot(data=pengangguran, aes(x=`Tahun`, y=Jumlah.Pengangguran)) + geom_line() + geom_line(data=mean_pengangguran, aes(y=rata_pengangguran), color="red")
      viz4 <- viz4 + labs(y="Jumlah Pengangguran", title="Grafik Jumlah Pengangguran di Indonesia Tahun 1990-2020") + theme(plot.title = element_text(hjust = 0.5, vjust = 1))
      viz4 <- viz4 + theme(axis.text.y = element_text(hjust = 0.5, size=8), axis.text.x = element_text(hjust = 0.5, size=15), plot.title = element_text(hjust = 0.5, vjust = 1)) 
      viz4 <- ggplotly(viz4)
      viz4
    })
  }
  if(input$feature2 == "Pengangguran di Indonesia Berdasarkan Umur"){
    output$plot3 <- renderPlotly({
      viz6 <- ggplot(data = pengangguran_umur, aes(`umur`)) + geom_col(aes(y=tahun_2020), color="black", fill="lightblue") 
      viz6 <- viz6 + geom_text(aes(y=tahun_2020, label=tahun_2020), position=position_dodge(width=1), vjust=-1)
      viz6 <- viz6 + labs(x="Umur", y="Persentase", title="Persentase Penduduk Belum Bekerja atau Pengangguran Berdasarkan Umur Tahun 2020")
      viz6 <- viz6 + theme(axis.text.x = element_text(hjust = 0.5, size=15), plot.title = element_text(hjust = 0.5, vjust = 1))
      viz6 <- ggplotly(viz6)
      viz6
    })
  }
  })
  
  observe({
    if(input$feature3 == "Korelasi Antara Banyaknya Jumlah Penduduk dengan Jumlah Pengguna Internet"){
      output$plot4 <- renderPlotly({
        viz2 <- ggplot(data = jumlah_penduduk, mapping = aes(x = Jumlah_Penduduk, y = Pengguna_Internet)) + geom_point(color="black", size=2)
        viz2 <- viz2 + geom_line(mapping = aes(x = Jumlah_Penduduk, y = Pengguna_Internet), data = jumlah_penduduk, color="black", size=1)
        viz2 <- viz2 + labs(x="Jumlah Penduduk", y="Jumlah Pengguna Internet", title="Apakah Pertambahan Jumlah Penduduk Mempengaruhi Pertambahan Penggunaan Internet ?")
        viz2 <- viz2 + theme(axis.text.x = element_text(hjust = 0.5), axis.text.y.left = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, vjust = 1), plot.caption = element_text(size = 8))
        viz2 <- viz2 + geom_line(mapping = aes(x = Jumlah_Penduduk, y = Pengguna_Internet), data = jumlah_penduduk, stat = "smooth", method = "lm")
        viz2 <- ggplotly(viz2)
        viz2
      })
    }
    if(input$feature3 == "Korelasi Antara Banyaknya Pengguna Internet dengan Banyaknya Penduduk yang Pengangguran"){
      output$plot4 <- renderPlotly({
        viz5 <- ggplot(data=pengangguran, aes(y=pengguna_internet, x=Jumlah.Pengangguran)) + geom_point(size=2)
        viz5 <- viz5 + labs(x="Jumlah Pengangguran", y="Pengguna Internet", title="Korelasi Antara Jumlah Pengangguran dengan Jumlah Pengguna Internet") + theme(plot.title = element_text(hjust = 0.5, vjust = 1))
        viz5 <- viz5 + geom_line(mapping = aes(y=pengguna_internet, x=Jumlah.Pengangguran), data = pengangguran, stat = "smooth", method = "lm")
        viz5 <- ggplotly(viz5)
        viz5
      })
    }
  })
}

shinyApp(ui, server)
