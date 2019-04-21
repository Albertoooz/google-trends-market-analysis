#zadanie 2 
library(tidyverse)
library(lubridate)

library(officer)

library(magrittr)


data <- read.csv(file="https://stooq.pl/q/d/l/?s=wig&i=d",sep=",",dec=".",header=T,stringsAsFactors=F)
data[is.na(data)] <- 0
write.table(data,file="data.csv",sep=";",dec=".",row.names=F)

data$Roznica=data$Zamkniecie-data$Otwarcie

data$roznicamaxmin=data$Najwyzszy-data$Najnizszy

data$dzientygodnia <- wday(as.Date(data$Data),label=T)

data$dzienmiesiaca <- as.POSIXlt(data$Data)$mday

data$Data <- as.Date(data$Data)



jpeg(file="Wykres1.jpeg", width  =  4 * 240, height  =  4 * 240)
layout(matrix(c(1, 2, 3, 4, 5,5), 3, 2, byrow  =  TRUE))
graphics::plot(y = data$Otwarcie,
               x = data$Data,
               xlab = "Data", ylab = "", type = "h",
               main = "Otwarcie",col="darkgoldenrod4",
               col.lab = "darkgoldenrod4", col.sub = "darkgoldenrod4", las = 2)
graphics::box("figure", col = "darkgoldenrod4", lwd = 1) 
graphics::plot(y = data$Zamkniecie,
               x = data$Data,
               xlab = "Data", ylab = "", type = "h",
               main = "Zamkniecie",col="darkgoldenrod4",
               col.lab = "darkgoldenrod4", col.sub = "darkgoldenrod4", las = 2)
graphics::box("figure", col = "darkgoldenrod4", lwd = 1) 
graphics::plot(y = data$Najnizszy,
               x = data$Data,
               xlab = "Data", ylab = "", type = "h",
               main = "Najnizsza cena danego dnia",col="darkgoldenrod4",
               col.lab = "darkgoldenrod4", col.sub = "darkgoldenrod4", las = 2)
graphics::box("figure", col = "darkgoldenrod4", lwd = 1) 
graphics::plot(y = data$Najwyzszy,
               x = data$Data,
               xlab = "Data", ylab = "", type = "h",
               main = "Najwyzsza cena danego dnia",col="darkgoldenrod4",
               col.lab = "darkgoldenrod4", col.sub = "darkgoldenrod4", las = 2)
graphics::box("figure", col = "darkgoldenrod4", lwd = 1) 

graphics::plot(y = data$Wolumen,
               x = data$Data,
               xlab = "Data", ylab = "", type = "h",
               main = "Wolumen",col="darkgoldenrod4",
               col.lab = "darkgoldenrod4", col.sub = "darkgoldenrod4", las = 2)
graphics::box("figure", col = "darkgoldenrod4", lwd = 1) 
dev.off()

### zadanie 2.2
#usuwam dane przed 200-11-17 ze względu na brak różnic między Cenami zamknięcia otwarcia max i min
data2 <- data[data$Data >= '2000-11-17',]
jpeg(file="Wykres2.jpeg", width  =  4 * 240, height  =  4 * 240)

layout(matrix(c(1, 2), 1, 2, byrow  =  TRUE))
graphics::hist(x = data2$Roznica, breaks = 53, main = "Histogram różnic cen",xlab="Różnica",ylab="Procent %", col = "darkgoldenrod4",col.lab = "darkgoldenrod4", freq = T)
graphics::box("figure", col = "darkgoldenrod4", lwd = 1)
graphics::grid(lty = 2, col = "beige")


graphics::hist(x = data2$Roznica, breaks = 53, main = "Histogram różnic cen",ylab="Wartosc",xlab="Różnica",col.lab = "darkgoldenrod4", col = "darkgoldenrod4", freq = F)
graphics::grid(lty = 2, col = "beige")
graphics::box("figure", col = "darkgoldenrod4", lwd = 1)

dev.off()
###zadanie 2.3

data2$dzien_tygodnia <- factor(lubridate::wday(data2$Data, week_start = 1), levels = c(1, 2, 3, 4, 5),
                           labels = c("Pon", "Wt", "Śr", "Czw", "Pi"), ordered = TRUE)
jpeg(file="Wykres3.jpeg", width  =  2 * 240, height  =  2 * 240)
layout(matrix(1, 1, 1, byrow  =  TRUE))
graphics::boxplot(roznicamaxmin~dzien_tygodnia,data=data2,col=c("darkgoldenrod1","darkgoldenrod2","darkgoldenrod3","darkgoldenrod","darkgoldenrod4"),
                  main = "Wykres boxplot względem dni tygodnia",xlab="Dzień tygodnia",ylab="Wartość",col.lab = "darkgoldenrod4")
graphics::grid(lty = 2, col = "beige")
graphics::box("figure", col = "darkgoldenrod4", lwd = 1)
dev.off()
jpeg(file="Wykres4.jpeg", width  =  2 * 240, height  =  2 * 240)
layout(matrix(1, 1, 1, byrow  =  TRUE))
graphics::boxplot(roznicamaxmin~dzienmiesiaca,data=data2,col=c("darkgoldenrod1","darkgoldenrod2","darkgoldenrod3","darkgoldenrod","darkgoldenrod4"),
                  main = "Wykres boxplot względem dni miesiaca",xlab="Dzień tygodnia",ylab="Wartość",col.lab = "darkgoldenrod4")
graphics::grid(lty = 2, col = "beige")
graphics::box("figure", col = "darkgoldenrod4", lwd = 1)
dev.off()

##zadanie 2.4



data3 <- data2%>%
  select(Zamkniecie,Otwarcie,Najwyzszy,Najnizszy,Wolumen)

podsumowanie3 <- round(do.call(cbind, lapply(data3,summary)),0)

podsumowanie3 <- as.data.frame(podsumowanie3)
podsumowanie3$Typ[1:6] <- c("Min","1st Qu","Median","Mean","3rd Qu","Max")
podsumowanie3 <- podsumowanie3%>%
  select(Typ,Zamkniecie,Otwarcie,Najwyzszy,Najnizszy,Wolumen)



##tworzenie prezentacji
my_presentation <- read_pptx() 


my_presentation <- officer::add_slide(my_presentation, layout = "Title and Content", master = "Office Theme")
my_presentation <- officer::ph_with_text(my_presentation, type = "title", str = "Zadanie 2")
my_presentation <- officer::ph_with_text(my_presentation, type = "ftr", str = "Name")
my_presentation <- officer::ph_with_text(my_presentation, type = "sldNum", str = "Slajd 1")
my_presentation <- officer::ph_with_text(my_presentation, str = paste("1. Treść raportu:", "\n", 
                                                                      "1.1. Ogólną postać szeregów czasowych cen i wolumenu", "\n",
                                                                      "1.2. Histogram różnic kursów zamknięcia i otwarcia", "\n",
                                                                      "1.3. Boxplot różnic kursów max i min dla typów dni", "\n",
                                                                      "1.4. Tabelę z podstawowymi statystykami pozycyjnymi danych"), type = "body")
# Slajd (2)
my_presentation <- officer::add_slide(my_presentation, layout = "Title and Content", master = "Office Theme")
my_presentation <- officer::ph_with_text(my_presentation, type = "title", str = "Zadanie 2.1")
my_presentation <- officer::ph_with_text(my_presentation, type = "ftr", str = "Name")
my_presentation <- officer::ph_with_text(my_presentation, type = "sldNum", str = "Slajd 2")
my_presentation <- officer::ph_with_img(my_presentation, src = "Wykres1.jpeg")
unlink("Wykres1.jpeg")

#slaid (3)
my_presentation <- officer::add_slide(my_presentation, layout = "Title and Content", master = "Office Theme")
my_presentation <- officer::ph_with_text(my_presentation, type = "title", str = "Zadanie 2.2")
my_presentation <- officer::ph_with_text(my_presentation, type = "ftr", str = "Name")
my_presentation <- officer::ph_with_text(my_presentation, type = "sldNum", str = "Slajd 3")
my_presentation <- officer::ph_with_img(my_presentation, src = "Wykres2.jpeg")
unlink("Wykres2.jpeg")

#slaid (4)
my_presentation <- officer::add_slide(my_presentation, layout = "Title and Content", master = "Office Theme")
my_presentation <- officer::ph_with_text(my_presentation, type = "title", str = "Zadanie 2.3")
my_presentation <- officer::ph_with_text(my_presentation, type = "ftr", str = "Name")
my_presentation <- officer::ph_with_text(my_presentation, type = "sldNum", str = "Slajd 4")
my_presentation <- officer::ph_with_img(my_presentation, src = "Wykres3.jpeg")
unlink("Wykres3.jpeg")

#slaid (5)
my_presentation <- officer::add_slide(my_presentation, layout = "Title and Content", master = "Office Theme")
my_presentation <- officer::ph_with_text(my_presentation, type = "title", str = "Zadanie 2.3 ciąg dalszy")
my_presentation <- officer::ph_with_text(my_presentation, type = "ftr", str = "Name")
my_presentation <- officer::ph_with_text(my_presentation, type = "sldNum", str = "Slajd 5")
my_presentation <- officer::ph_with_img(my_presentation, src = "Wykres4.jpeg")
unlink("Wykres4.jpeg")

# Slajd (6)
my_presentation <- officer::add_slide(my_presentation, layout = "Title and Content", master = "Office Theme")
my_presentation <- officer::ph_with_text(my_presentation, type = "title", str = "Zadanie 2.4")
my_presentation <- officer::ph_with_text(my_presentation, type = "ftr", str = "Name")
my_presentation <- officer::ph_with_text(my_presentation, type = "sldNum", str = "Slajd 6")
my_presentation <- officer::ph_with_table(my_presentation, type = "body", value = podsumowanie3)

print(my_presentation, target = "Name.pptx") 
