# Zadanie 1
# Prawdopodobieństwa z zadania
# H0 - wybór siedliska nie zależy od okresu.
# H1 - wybór siedliska zależy od okresu.
Pb <- 0.26
Pb_dopelnienie <- 1-Pb
Pab <- 0.4
Pa_b_dopelnienie <- 0.76
Pa_dopelnienie_b <- 1-Pa_b
N = 110

# Wartości grup n przed i po 2000:
n_przed_2000 <- N*Pb
n_po_2000 <- N - n_przed_2000

# Wartości do tabelii kontygencji
O11 <- N*(Pab*Pb)
O21 <- n_przed_2000 - O11
O12 <- N*(Pa_b_dopelnienie*Pb_dopelnienie)
O22 <- n_po_2000 - O12

wektor_do_kond <- c(O11, O12, O21, O22)

# Tworzę tabelę kontygencji z wcześniej przygotowanego wektora
kondyg <- matrix(wektor_do_kond, nrow=2, ncol=2, byrow=TRUE)

# Obliczam p-value za pomocą funkcji pchisq i sumuję ze wzoru
p_value <- sum(1-pchisq(kondyg, df=1))

# Wykonuję test chisq.test
chisq.test(kondyg, correct=F)

# Na podstawie obliczonego przeze mnie p-value mogę określić, że wybór siedliska zależał od okresu, 
# skutkując odrzuceniem H0 przy poziomie istotności 0,05. Ponadto wartość p-value przy chisq.text była jeszcze niższa niż ta obliczona przeze mnie.

# Zadanie 2




