library(likert)

dane <- read.table("D://Studies//Sem_6//ADA//Data_analysis_studies//data//ankieta.csv", sep=";")
dane <- dane[-1, ]

names(dane) <- c("DZIAŁ", "STAŻ", "CZY_KIER", "PYT_1", "PYT_2", "PYT_3", "PŁEĆ", "WIEK")

#print(head(dane))

## ZADANIE 2
df <- dane
df$PYT_1 <- factor(df$PYT_1, levels = c(-2, -1, 0, 1, 2),
                     labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "nie mam zdania", "zgadzam się", "zdecydowanie się zgadzam"))

df$CZY_KIER <- factor(df$CZY_KIER, levels = c("Nie", "Tak"))

# a) działanie funkcji summary

# bez grupowania (w całości)

likert_pyt_1 <- likert(df[,"PYT_1", drop=FALSE])

summary_pyt_1 <- summary(likert_pyt_1)
print(summary_pyt_1)

# z grupowaniem (wedlug bycia kierownikiem)
likert_pyt_1_czy_kier <- likert(df[, "PYT_1", drop = FALSE], grouping = df$CZY_KIER)

summary_pyt_1_czy_kier <- summary(likert_pyt_1_czy_kier)
print(summary_pyt_1_czy_kier)

# b) plots

# wykresy typu "bar", "heat" i "density" bez grupowania
plot(likert_pyt_1, type = "bar", centered = FALSE)
plot(likert_pyt_1, type = "heat", centered = FALSE)
plot(likert_pyt_1, type = "density")

# wykresy typu "bar", "heat" i "density" z grupowaniem
plot(likert_pyt_1_czy_kier, type = "bar", centered = FALSE)
plot(likert_pyt_1_czy_kier, type = "density")

## ZADANIE 3

# bez zwracania
ind <- sample(nrow(dane), 1/10 * nrow(dane), replace = FALSE)
print(dane[ind,])

# ze zwracaniem
ind2 <- sample(nrow(dane), 1/10 * nrow(dane), replace = TRUE)
print(dane[ind2,])


## ZADANIE 6
#Napisz funkcje do wyznaczania realizacji przedziału ufnosci 
#Cloppera-Pearsona. ´Niech argumentem wejsciowym bedzie 
#poziom ufnosci, liczba sukcesów i liczba prób lub ´
#poziom ufnosci i wektor danych 
#(funkcja powinna obsługiwać oba przypadki). 

clopper_pearson <- function(p_ufnosci, args_list){
  if (is.vector(args_list[[2]])){  # jeśli pojawi się wektor to obsługujemy 1 przypadek
    dane <- args_list[[2]] # pobranie wektora danych
    s <- sum(dane == "TAK") # liczba sukcesów 
    n <- length(dane) # liczba wszystkich prób

  } else if (length(args_list) == 3) {  # trzy elementy oznaczają 2 przypadek
    s <- args_list[[2]] # liczba sukcesów, podanych na sztywno
    n <- args_list[[3]] # liczba prób
    
  } else { # ewentualne wyeliminowanie błędów
    return("Niepoprawna liczba argumentów.")
  }
  
  # wyznaczenie przedziału ufności
  start_przedzial <- qbeta(1/2-p_ufnosci/2, s, n-s+1)
  koniec_przedzial <- qbeta(1/2+p_ufnosci/2, s+1, n-s)
    
  return(paste("[", start_przedzial, ", ", koniec_przedzial, "]", sep = ""))
}

#print(clopper_pearson(95/100, list(1, 100)))
#print(clopper_pearson(95/100, c("NIE", "TAK", "TAK")))


## ZADANIE 7

# tworzenie nowych zmiennych
# Tworzenie nowej kolumny CZY_ZADOW

#dane$PYT_2 <- dane$V5
#dane$PYT_3 <- dane$V6

# Utwórz kolumnę CZY_ZADOW i przypisz wartości na podstawie wartości w kolumnie PYT_2


dane$CZY_ZADW <- ifelse(dane$PYT_2 %in% c(1, 2), "TAK", ifelse(dane$PYT_2 %in% c(-1, -2), "NIE", NA))

dane$CZY_ZADW_2 <- ifelse(dane$PYT_3 %in% c(1, 2), "TAK", ifelse(dane$PYT_3 %in% c(-1, -2), "NIE", NA))

# Wyświetlanie wyników dla CZY_ZADW
#cat("Przedział ufności (zadowolenie z wynagrodzenia w 1 okresie):\n")
print(clopper_pearson(0.9, dane$CZY_ZADW))

# Wyświetlanie wyników dla CZY_ZADW_2
#cat("Przedział ufności (zadowolenie z wynagrodzenia w 2 okresie):\n")
print(clopper_pearson(0.9, dane$CZY_ZADW_2))

## ZADANIE 11

n <- length(dane$PŁEĆ) #n=200
print(n)

#a
dane_kobiet <- dane$PŁEĆ[dane$PŁEĆ == "K"] # liczba kobiet wynosi 71
liczba_kobiet <- length(dane_kobiet)
print(liczba_kobiet)

library(binom)
test1 <- binom.test(liczba_kobiet , n , p=0.5, alternative = "two.sided", conf.level = 0.95)
print(test1)

#b
dane_zadowolonych <- dane$PYT_1[dane$PYT_1 >0] #ilosc zadowolonych qynosi 129
liczba_zadowolonych <- length(dane_zadowolonych)
test2 <- binom.test(liczba_zadowolonych , n , p=0.7, alternative = "greater", conf.level = 0.95)
print(test2)

#c
dane_menadzerskie <- dane$PŁEĆ[dane$DZIAŁ == "DK"]
n_menadzerskie <- length(dane_menadzerskie)
dane_menadzerskie_kobiety <- length(dane_menadzerskie[dane_menadzerskie == "K"])
dane_menadzerskie_faceci <- length(dane_menadzerskie[dane_menadzerskie == "M"])
x <- c(dane_menadzerskie_kobiety , dane_menadzerskie_faceci)
nn <- c(n_menadzerskie, n_menadzerskie)
test3 <- prop.test(x , nn , alternative = "t" , correct = FALSE)
test4 <- prop.test(x , nn , alternative = "t" , correct = TRUE)
print(test3) #p-value = 2.2e-16
print(test4) #p-value = 3.861e-16

#d
dane_kobiety <- dane$PYT_1[dane$PŁEĆ == "K"]
dane_faceci <- dane$PYT_1[dane$PŁEĆ == "M"]
liczba_kobiety <- length(dane_kobiety)
liczba_facetow <- length(dane_faceci)
dane_kobiet_zadowolenie <-  length(dane_kobiety[dane_kobiety > 0])
dane_facet_zadowolenie <-  length(dane_faceci[dane_faceci > 0])
xx <- c(dane_kobiet_zadowolenie , dane_facet_zadowolenie)
nnn <- c(liczba_kobiety , liczba_facetow)

test5 <- prop.test(xx , nnn , alternative = "t" , correct = FALSE)
test6 <- prop.test(xx , nnn , alternative = "t" , correct = TRUE)
print(test5) #p-value = 0.7098
print(test6) #p-value = 0.8277

#e
dane_hr <- dane$PŁEĆ[dane$DZIAŁ == "HR"]
liczba_kobiety_hr <- length(dane_hr[dane_hr == "K"])
liczba_faceci_hr <- length(dane_hr[dane_hr == "M"])
ilosc_hr <- length(dane_hr)
xxx <- c(liczba_kobiety_hr , liczba_faceci_hr)
nnnn <- c(ilosc_hr , ilosc_hr)

test7 <- prop.test(xxx , nnnn , alternative = "less" , correct = FALSE)
test8 <- prop.test(xxx , nnnn , alternative = "less" , correct = TRUE)
print(test7) #p-value = 2.579e-09
print(test8) #p-value = 1.148e-08

