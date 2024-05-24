

## ZADANIE 1

library(binom)
X <- c(14, 17,40,100,29)
n <- 200
wektor_prawd <- X/n 
stopnie_zadowolenia <- c("bardzo niezadowolony", "niezadowolony", "nie ma zdania", "zadowolony", "bardzo zadowolony")

tab_exact <- binom.confint(x = X, n = 200, conf.level = 0.95, 
              methods = c("exact"))

tab_asympt <- binom.confint(x = X, n = 200, conf.level = 0.95, 
                         methods = c("asymptotic"))

wyniki_1 <- data.frame(stopnie_zadowolenia,
wektor_prawd,tab_exact$lower, tab_exact$upper, tab_asympt$lower, tab_asympt$upper )

colnames(wyniki_1) <- c("St. zadowolenia", "Prawd.stopnia zadowolenia", 
"Dolna granica prz. ufności (exact)", "Górna granica prz. ufności (exact)",
"Dolna granica prz. ufności (asymptotic)", "Górna granica prz. ufności (asymptotic)")
print(wyniki_1)


## ZADANIE 5
 
# W tym zadaniu weryfikujemy czy płeć ma istotny wpływ na obejmowanie stanowiska kierowniczego.

# Działanie testu Fishera: badamy niezależność zmiennych
# przyjęta hipoteza zerowa to H0: zmienne są niezależne
# Jeśli wartość p-wartość < p.istotności: odrzucamy hipotezę czyli zmienne są zależne.
# Jeśli wartość p-wartość > p.istotności: przyjmujemy hipotezę, zmienne są niezależne.

dane <- read.table("D://Studies//Sem_6//ADA//Data_analysis_studies//data//ankieta.csv", sep=";")
dane <- dane[-1, ]
names(dane) <- c("DZIAŁ", "STAŻ", "CZY_KIER", "PYT_1", "PYT_2", "PYT_3", "PŁEĆ", "WIEK")

kolumna_czy_kier <- dane$CZY_KIER
kolumna_plec <- dane$PŁEĆ
print(testowane_dane)

wynik_testu <- fisher.test(kolumna_czy_kier, kolumna_plec, conf.level = 0.95)

print(wynik_testu)

# p-value = 0.6659 czyli zmienne są niezależne, co oznacza że dla naszych danych i przyjętego poziomu istotności
# płeć nie ma istotnego wpływu, zatem możemy wnioskować że prawdopodobie ˙ nstwo tego, ze na stanowisku kierowniczym pracuje kobieta jest ˙
#równe prawdopodobienstwu tego, ze na stanowisku kierowniczym pracuje mezczyzna


# ZADANIE 6
dane <- read.table("D://Studies//Sem_6//ADA//Data_analysis_studies//data//ankieta.csv", sep=";")
dane <- dane[-1, ]
names(dane) <- c("DZIAŁ", "STAŻ", "CZY_KIER", "PYT_1", "PYT_2", "PYT_3", "PŁEĆ", "WIEK")

p_istotnosci <- 0.05

dane$WIEK <- as.numeric(dane$WIEK)

print(dane)

dane$WIEK_KAT <- cut(dane$WIEK,breaks=c(0,35,45,55,Inf),
labels=c("do 35 lat", "36-45 lat", "46-55 lat", "powyżej 55 lat"), include.lowest = TRUE)

# H0: zajmowanie stanowiska kierowniczego zależy od wieku
fisher_test_1 <- fisher.test(dane$CZY_KIER, dane$WIEK_KAT, conf.level = 1-p_istotnosci)
print(fisher_test_1)
# nie zalezy, bo p-value =  0.7823 > 0.05

# H0: zajmowanie stanowiska kierowniczego zalezy od stażu pracy
fisher_test_2 <- fisher.test(dane$CZY_KIER, dane$STAŻ, conf.level = 1-p_istotnosci)
print(fisher_test_2)
# zależy, bo p-value = 6.538e-05 < 0.05

# H0: zadowolenie z wynagrodzenia w pierwszym badanym okresie zalezy od zajmowanego stanowiska
fisher_test_3 <- fisher.test(dane$CZY_KIER, dane$PYT_2, conf.level = 1-p_istotnosci)
print(fisher_test_3)
# zależy bo p-value = 0.0443 < 0.05

# H0: zadowolenie z wynagrodzenia w pierwszym badanym okresie zalezy od stażu
fisher_test_4 <- fisher.test(dane$STAŻ, dane$PYT_2, conf.level = 1-p_istotnosci)
print(fisher_test_4)
# zależy bo p-value = 0.01069 < 0.05

# H0: zadowolenie z wynagrodzenia w pierwszym badanym okresie zalezy od płci
fisher_test_5 <- fisher.test(dane$PŁEĆ, dane$PYT_2, conf.level = 1-p_istotnosci)
print(fisher_test_5)
# nie zależy bo p-value =  0.4758 > 0.05

# H0: zadowolenie z wynagrodzenia w pierwszym badanym okresie zalezy od wieku
fisher_test_6 <- fisher.test(dane$WIEK_KAT, dane$PYT_2, conf.level = 1-p_istotnosci, workspace = 10^6)
print(fisher_test_6)
# nie zależy bo p-value =  0.3194 > 0.05

print(paste("P-wartość dla hipotezy 1:", fisher_test_1$p.value))
print(paste("P-wartość dla hipotezy 2:", fisher_test_2$p.value))
print(paste("P-wartość dla hipotezy 3:", fisher_test_3$p.value))
print(paste("P-wartość dla hipotezy 4:", fisher_test_4$p.value))
print(paste("P-wartość dla hipotezy 5:", fisher_test_5$p.value))
print(paste("P-wartość dla hipotezy 6:", fisher_test_6$p.value))

# ZADANIE 8
# Test chi-kwadrat służy do zbadania niezależności między danymi z dwóch kategorii poprzez analizę tabeli kontyngencji. 
# Przyjmujemy hipoteze zerową H0: Dane są niezależne (wartości z wierszy i kolumn tabeli kontyngencji są niezależne). 
# Wynik testu zwraca p-wartość, jeżeli owa wartość jest większa od przyjętego poziomu istotności to wówczas przyjmujemy H0 (dane są niezależne). 

tabela_kontyngencji <- table(dane$PYT_2, dane$CZY_KIER) 

poprawny_uklad <- c('-2', '-1', '1', '2')
tabela_kontyngencji <- tabela_kontyngencji[poprawny_uklad,]
rownames(tabela_kontyngencji) <- c('bardzo niezadowolony', 'niezadowolony', 'zadowolony', 'bardzo zadowolony')

test_chi2_pwart <- chisq.test(table)$p.val
print(tabela_kontyngencji)
print(paste("P-wartość dla  hipotezy: ", test_chi2_pwart))

n_wierszy <- nrow(tabela_kontyngencji)
n_kolumn <- ncol(tabela_kontyngencji)

assocplot(tabela_kontyngencji, col = c("cyan3","coral1"), space = 0.3,
          main = "Zależność zadowolenia z wynagrodzenia od zajmowanego stanowiska", 
          xlab = "Poziom zadowolenia z wynagrodzenia", ylab = "Czy pracownik jest kierownikiem")


# Tabela porownania zad. 8 z zad. 6
p_istotnosci <- c(0.01, 0.05)
wyniki_chi2 <- c(test_chi2_pwart, test_chi2_pwart)
wyniki_fisher <- c(fisher_test_3$p.value, fisher_test_3$p.value)
tab_porownanie_6_8 <- data.frame(p_istotnosci, wyniki_chi2, wyniki_fisher)
a <- ifelse(test_chi2_pwart > 0.01, "TAK", "NIE")
b <- ifelse(test_chi2_pwart > 0.05, "TAK", "NIE")
cc <- ifelse(fisher_test_3$p.value> 0.01,"TAK", "NIE")za
d <- ifelse(fisher_test_3$p.value> 0.05, "TAK", "NIE")
zmienne_niezależne_chi2 <- c(a, b)
zmienne_niezależne_fisher <- c(cc, d)
tab_porownanie_6_8 <- data.frame(p_istotnosci, wyniki_chi2, wyniki_fisher, zmienne_niezależne_chi2, zmienne_niezależne_fisher)
print(tab_porownanie_6_8)
# p-wartość wyszła poniżej przyjętych poziomów istotności (0.01 oraz 0.05) zatem dane są od siebie zależne
# (zadowolenie z wynagrodzenia ˙w 1 badanym okresie zależy od zajmowanego stanowiska)
# jest to zgodne z zadaniem 6
# Oczekiwana wartość to szerokość pudełek, a wartość uzyskana to wysokość pudełek.

# w grupie  niezadowolonych: oczekiwalibyśmy że bedzie mniej osob na stanowisku kierowniczym
# w grupie bardzo niezadowolonych: jest tyle ile oczekiwaliśmy 
# w grupie zadowolonych: jest znacznie wiecej kierowników niż bysmy oczekiwali, za to dużo mniej osób na niekierowniczym stanowisku niż byśmy myśleli
# w grupie bardzo zadowolonych: jest wiecej nie-kierowników oraz mniej kierowników niz bysmy przypusczali

# ZADANIE 11

zgony_rak <- c(0.00140, 0.00010)
zgony_serce <- c(0.00669, 0.00413)

tab_1 <- matrix(c(zgony_rak, zgony_serce), nrow = 2, byrow = TRUE)
rownames(tab_1) <- c("Palący", "Niepalący")
colnames(tab_1) <- c("Rak płuc", "Choroba serca")
print(tab_1)

odsetek_raka_palacych <- tab_1["Palący", "Rak płuc"]
odsetek_raka_niepalacych <- tab_1["Niepalący", "Rak płuc"]
odsetek_serca_palacych <- tab_1["Palący", "Choroba serca"]
odsetek_serca_niepalacych <- tab_1["Niepalący", "Choroba serca"]

# Różnica proporcji
roz_prop_raka <- odsetek_raka_palacych - odsetek_raka_niepalacych
roz_prop_serca <- odsetek_serca_palacych - odsetek_serca_niepalacych

# Ryzyko względne
ryz_wzgl_raka <- odsetek_raka_palacych / odsetek_raka_niepalacych
ryz_wzgl_serca <- odsetek_serca_palacych / odsetek_serca_niepalacych

# Iloraz szans
iloraz_szans_raka <- (odsetek_raka_palacych / (1 - odsetek_raka_palacych)) / (odsetek_raka_niepalacych / (1 - odsetek_raka_niepalacych))
iloraz_szans_serca <- (odsetek_serca_palacych / (1 - odsetek_serca_palacych)) / (odsetek_serca_niepalacych / (1 - odsetek_serca_niepalacych))

# Wyświetlenie wyników
cat("Różnica proporcji dla raka płuc:", roz_prop_raka, "\n")
cat("Różnica proporcji dla choroby serca:", roz_prop_serca, "\n")
cat("Ryzyko względne dla raka płuc:", ryz_wzgl_raka, "\n")
cat("Ryzyko względne dla choroby serca:", ryz_wzgl_serca, "\n")
cat("Iloraz szans dla raka płuc:", iloraz_szans_raka, "\n")
cat("Iloraz szans dla choroby serca:", iloraz_szans_serca, "\n")
# silniejszy zwiazek wg ilorazu szans mają od raka płuc bo jest wartość dalsza od jedynki
# silniejszy zwiazek wg ryzyka wzglednego mają od raka płuc (wyższa wartość)
# silnijeszy zwiazek wg roznic proporcji maja od choroby serca (wyższa różnica)

# ZADANIE 12

# TABELA WYNIKÓW SMIERTELNOSCI W WYPADKACH

zgony_bez_pasow <- c(1085, 55623)
zgony_z_pasami <- c(703, 441239)

tab_smiertelnosci <- matrix(c(zgony_bez_pasow, zgony_z_pasami), nrow = 2, byrow = TRUE)
rownames(tab_smiertelnosci) <- c("Bez pasów", "Z pasami")
colnames(tab_smiertelnosci) <- c("Śmiertelny", "Nieśmiertelny")
print(tab_smiertelnosci)


# a) WAR.PRAWD. SMIERCI dla tych, którzy uzyli/nie uzyli pasa bezpieczenstwa

prawd_smierci_bez_pasow <- tab_smiertelnosci["Bez pasów", "Śmiertelny"] / 
  (tab_smiertelnosci["Bez pasów", "Śmiertelny"] + tab_smiertelnosci["Bez pasów", "Nieśmiertelny"])

prawd_smierci_z_pasami <- tab_smiertelnosci["Z pasami", "Śmiertelny"] / 
  (tab_smiertelnosci["Z pasami", "Śmiertelny"] + tab_smiertelnosci["Z pasami", "Nieśmiertelny"])

print(prawd_smierci_bez_pasow) # prawd. smierci gdy nie uzyli pasa to 0.0191331
print(prawd_smierci_z_pasami) # prawd. smierci gdy  uzyli pasa  0.001590706
print(paste("Warunkowe prawdopodobieństwo śmierci dla tych, którzy nie użyli pasa bezpieczenstwa:", prawd_smierci_bez_pasow))
print(paste("Warunkowe prawdopodobieństwo śmierci dla tych, którzy użyli pasa bezpieczenstwa:", prawd_smierci_z_pasami))

# widzimy że prawd smierci z pasami jest mniejsze niz bez pasów 

# b) WAR.PRAWD. UZYCIA PASA dla tych, ktorzy umarli/przezyli 

prawd_pasy_przezycie <- tab_smiertelnosci["Z pasami", "Nieśmiertelny"] / 
  (tab_smiertelnosci["Z pasami", "Nieśmiertelny"] + tab_smiertelnosci["Bez pasów", "Nieśmiertelny"])

prawd_pasy_smierc <- tab_smiertelnosci["Z pasami", "Śmiertelny"] / 
  (tab_smiertelnosci["Z pasami", "Śmiertelny"] + tab_smiertelnosci["Bez pasów", "Śmiertelny"])

print(prawd_pasy_smierc) # prawd. uzycia pasa gdy umarli to 0.3931767
print(prawd_pasy_przezycie) # prawd. uzycia pasa gdy przezyli to  0.8880514
print(paste("Warunkowe prawdopodobieństwo użycia pasów dla tych, którzy nie przeżyli:", prawd_pasy_smierc))
print(paste("Warunkowe prawdopodobieństwo użycia pasów dla tych, którzy przeżyli:", prawd_pasy_przezycie))

# mamy wieksze prawdopodobienstwo uzycia pasa dla wypadkow niesmiertelnych


# c) Jaki jest najbardziej naturalny wybór dla zmiennej objaśnianej w tym badaniu? Dla takiego
# wyboru wyznacz i zinterpretuj różnicę proporcji, ryzyko względne oraz iloraz szans.
# Dlaczego wartości ryzyka względnego i ilorazu szans przyjmują zbliżone wartości?

roz_proporcji_1 <- prawd_smierci_z_pasami - prawd_smierci_bez_pasow

ryz_wzgl_1 <- prawd_smierci_z_pasami/prawd_smierci_bez_pasow

iloraz_szans_pasy <- prawd_smierci_z_pasami/(1-prawd_smierci_z_pasami)
iloraz_szans_bez_pasow <- prawd_smierci_bez_pasow/(1-prawd_smierci_bez_pasow)

iloraz_szans_1 <- iloraz_szans_pasy / iloraz_szans_bez_pasow

roz_proporcji_2 <- prawd_pasy_smierc - prawd_pasy_smierc

ryz_wzgl_2 <- prawd_pasy_smierc/prawd_pasy_przezycie

iloraz_szans_smierc <- prawd_pasy_smierc/(1-prawd_pasy_smierc)
iloraz_szans_przezycie <- prawd_pasy_przezycie/(1-prawd_pasy_przezycie)

iloraz_szans_2 <- iloraz_szans_smierc / iloraz_szans_przezycie

result <- data.frame(
  Przypadek = c("Z pasami vs Bez pasów", "Śmiertelne vs Nieśmiertelne"),
  Różnica_proporcji = c(roz_proporcji_1, roz_proporcji_2),
  RR = c(ryz_wzgl_1, ryz_wzgl_2),
  OR = c(iloraz_szans_1, iloraz_szans_2)
)


print(result)


# Wyjaśnienie podobieństwa wartości RR i OR ( z chatu)
# Wartości RR i OR są zbliżone, gdy prawdopodobieństwo zdarzenia (w tym przypadku śmierci) jest małe.
# Wynika to z matematycznych właściwości ilorazu szans i ryzyka względnego, gdzie iloraz szans zbliża 
# się do ryzyka względnego w miarę zmniejszania się prawdopodobieństwa zdarzenia.


# objasniająca (x) to z pasami i bez pasow 
# objaśniana (y) to śmiertelny i niesmiertelny 

roz_prop_pasy <- tab_smiertelnosci["Bez pasów", ] - tab_smiertelnosci["Z pasami", ]

ryz_wzgl_pasy <-  tab_smiertelnosci["Bez pasów", ] / tab_smiertelnosci["Z pasami", ]

iloraz_szans_bez_pasow <- tab_smiertelnosci["Bez pasów", ]  / (1 - tab_smiertelnosci["Bez pasów", ] )
iloraz_szans_z_pasami <- tab_smiertelnosci["Z pasami", ]  / (1 - tab_smiertelnosci["Z pasami", ] )
iloraz_szans_pasy <- iloraz_szans_bez_pasow / iloraz_szans_z_pasami

result <- data.frame(
  Różnica_proporcji = roz_prop_pasy,
  Ryzyko_względne = ryz_wzgl_pasy,
  iloraz_szans = iloraz_szans_pasy
)


print(result)

# Analiza ryzyka względnego wskazuje, że prawd. śmiertelnego wypadku dla osób bez pasów jest 1.5433855 większe niż dla tych, 
# którzy zapięli pasy. Natomiast stosunek osób w nieśmiertelnych wypadkach między grupą mającą pasy a tą bez nich wynosi 0.1260609.

# Analiza ilorazu szans pokazuje, że szansa na śmiertelny wypadek dla osób bez pasów wynosi 0.9994987, 
# podczas gdy dla osób zapiętych pasami wynosi 1.0159995 dla wypadków nieśmiertelnych.


# Iloraz szans zbliżony do 1 dla obu grup wskazuje na to, że stosunek szans na śmierć w wypadku 
# nie różni się znacząco między osobami, które używają pasów bezpieczeństwa, a tymi, którzy tego nie robią.

# Podsumowując, z przeprowadzonej analizy wnioskujemy że używanie pasów w celu zwiększenia ryzyka śmierci 
# w wypadku samochodowym jest zasadne.


# ZADANIE 13

library(DescTools)
help("GoodmanKruskalTau")
dane <- read.table("D://Studies//Sem_6//ADA//Data_analysis_studies//data//ankieta.csv", sep=";")
dane <- dane[-1, ]
names(dane) <- c("DZIAŁ", "STAŻ", "CZY_KIER", "PYT_1", "PYT_2", "PYT_3", "PŁEĆ", "WIEK")

dane$CZY_ZADW <- ifelse(dane$PYT_2 %in% c(1, 2), "TAK", ifelse(dane$PYT_2 %in% c(-1, -2), "NIE", NA))
print(dane)

# NIEZALEZNOŚĆ: gamma od -1 do 1 oraz tau od 0 do 1
# ZALEŻNOŚĆ IDEALNA: gamma równa 1 oraz tau równe 1  

# a) Zadowolenie z wynagrodzenia w pierwszym badanym okresie i zajmowane stanowisko
# Są to zmienne nominalne, zatem liczymy współczynnik tau dla nich.

print(paste("Współczynnik Tau dla zadowolenia z wynagrodzenia w 1 badanym okresie i zajmowanego stanowiska: ",GoodmanKruskalTau(dane$CZY_KIER, dane$CZY_ZADW))) 
# wsp. tau wynosi 0.0004091802 który jest blisko 0 czyli zmienne są niezależne

# b) Zadowolenie z wynagrodzenia w pierwszym badanym okresie i staz pracy
# Są to zmienne porządkowe, zatem liczymy współczynnik gamma. 

print(paste("Współczynnik Gamma dla zadowolenia z wynagrodzenia w 1 badanym okresie i stażu pracy: ", GoodmanKruskalGamma(dane$PYT_2, dane$STAŻ)) )
# wsp. gamma wynosi 0.08170353 zatem niezaleznosc 

# c) Zajmowane stanowisko i staz pracy
# dane należy ustalić by były jednego typu, zatem zmieniamy STAŻ na zmienną nominalną 

dane$STAŻ_CZY_KROTKI <- ifelse(dane$STAŻ <= 1, "Tak", "Nie")

print(paste("Współczynnik Tau dla zajmowanego stanowiska i stażu pracy:", GoodmanKruskalTau(dane$CZY_KIER, dane$STAŻ_CZY_KROTKI)))
# wsp. tau wynosi 0.02701617 zatem jest to niewielka zależnosc miedzy zmiennymi

# ___________________________________________________________________________________

