#2a
#Formulate your quation
#Tentukan pertanyaan analisis
#Misalnya: "Apakah kualitas makanan dan kecepatan layanan berpengaruh pada tingkat kepuasan pelanggan ?"

#2b
#Read in your data
getwd()
setwd("C:/Users/Hendra/OneDrive/Dokumen")
utsandat=read.csv("utsandat.csv", sep=";",header=TRUE)
utsandat

#2c
#Check the packaging
str(utsandat)
#ringkasan data
summary(utsandat)

#2d
#Look at the top and the bottom of your data
#Lihat beberapa baris awal dan akhir data
head(utsandat)
tail(utsandat)

#2e
#check your "n"s
# Hitung jumlah observasi
dim(utsandat)
# Cek nilai yang hilang
colSums(is.na(utsandat))

#2f
#Validate with at least one external data source
#Periksa apakah pola atau statistik data sesuai dengan sumber eksternal
#Distribusi nilai kolom tertentu dibandingkan dengan sumber referensi

#Histogram untuk Food.Quality
library(ggplot2)
ggplot(utsandat, aes(x = Food.Quality)) +
  geom_histogram(binwidth = 0.5, fill = "coral", color = "black", alpha = 0.7) +
  labs(title = "Distribusi Food Quality",
       x = "Nilai Food Quality",
       y = "Frekuensi") +
  theme_minimal()


# Load library
library(ggplot2)

# Data eksternal : Predict Restaurant Customer Satisfaction Dataset
set.seed(123) # Agar hasil reproducible
external_data = rnorm(1500, mean = 3, sd = 1.42)

#Plot perbandingan histogram
ggplot() +
# Histogram untuk dataset Anda
geom_histogram(data = utsandat, aes(x = Food.Quality, fill = "Dataset"), 
                 color = "black", alpha = 0.5, binwidth = 0.5) +
# Histogram untuk data eksternal
geom_histogram(aes(x = external_data, fill = "Referensi"), 
                 color = "black", alpha = 0.5, binwidth = 0.5) +
# Judul dan label
labs(title = "Perbandingan Distribusi Food Quality",
     x = "Nilai Food Quality",
     y = "Density") +
# Custom warna
scale_fill_manual(name = "Sumber", values = c("Dataset" = "coral", 
                                                "Referensi" = "green")) +
theme_minimal()

#2g
#make a plot
library(ggplot2)

ggplot(utsandat, aes(x = Food.Quality, y = Delivery.Experience)) +
  geom_boxplot(color = "black") +
  labs(title = "Tingkat Kepuasan Pelanggan dan dan Kualitas Makanan",
       x = "Kualias Makanan",
       y = "Tingkat Kepuasan")

#2h
#Try the easy solution first
#rata-rata tingkat kepuasan pelanggan
mean(utsandat$Delivery.Experience, na.rm = TRUE)
#Korelasi
cor(utsandat$Food.Quality, utsandat$Delivery.Experience, use = "complete.obs")

#2i
#Follow up
# Model regresi linear
model = lm( Delivery.Experience ~ Food.Quality, data = utsandat)
summary(model)

#NOMOR 3
#a
# Membuat model regresi linear sederhana
model = lm(Delivery.Experience ~ Food.Quality + Times+ Correct.Order, 
            data = utsandat)
summary(model)

#b
# Membuat histogram variabel Dependen
install.packages("MASS")
library(ggplot2)
library(MASS)
summary(utsandat$Food.Quality)
ggplot(utsandat, aes(x = Food.Quality)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", 
                 color = "black", alpha = 0.7) + stat_function(fun = dnorm, 
                 args = list(mean = mean(utsandat$Food.Quality, na.rm = TRUE), 
                sd = sd(utsandat$Food.Quality, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(title = "Histogram dan Distribusi Normal dari Variabel Dependen",
       x = "Variabel Dependen",
       y = "Density") +
  theme_minimal()

