# Gerekli K??t??phaneler
library(tidyverse)
library(ggplot2)
library(corrplot)

# 1. Veri Y??kleme ve Temizleme --------------------------------------------

data("mtcars")

#VER??SET??N?? G??R??NT??LEME
mtcars

head(mtcars)

cat("Eksik veri say??s??:", sum(is.na(mtcars)), "\n")

# Yeni de??i??kenler olu??turma
mtcars <- mtcars %>% 
  mutate(
    hp_per_cyl = hp/cyl, # Silindir ba????na beygir g??c??
    wt_per_hp = wt/hp    # Beygir g??c?? ba????na a????rl??k
  )

# 2. Ke??ifsel Veri Analizi ------------------------------------------------
# ??zet istatistikler
cat("\n??zet ??statistikler:\n")
print(summary(mtcars))

# De??i??ken yap??s??
cat("\nVeri Yap??s??:\n")
str(mtcars)

# 3. G??rselle??tirmeler ----------------------------------------------------
# Grafik 1: A????rl??k-Yak??t Verimlili??i ??li??kisi
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Ara?? A????rl?????? ve Yak??t Verimlili??i ??li??kisi",
       x = "A????rl??k (1000 lbs)",
       y = "Mil/Galon") +
  theme_minimal()

# Grafik 2: Silindir Say??s??na G??re Yak??t Verimlili??i
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Silindir Say??s??na G??re Yak??t Verimlili??i",
       x = "Silindir Say??s??",
       y = "Mil/Galon")+
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Grafik 3: De??i??kenler Aras?? Korelasyon
cor_matrix <- cor(mtcars)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.7)

# 4. Regresyon Analizi ----------------------------------------------------
# Model: Yak??t verimlili??ini tahmin etme
model <- lm(mpg ~ wt + hp + factor(cyl), 
           data = mtcars)

# Model ??zeti
cat("\nRegresyon Model ??zeti:\n")
print(summary(model))

# Model varsay??mlar??n??n kontrol??
par(mfrow = c(2,2))
plot(model)

# 5. ??leri Analiz: Otomatik-Manuel Kar????la??t??rmas?? ------------------------
ggplot(mtcars, aes(x = factor(am), y = mpg, fill = factor(am))) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2) +
  labs(title = "Vites   Yak??t Verimlili??i Kar????la??t??r??lmas??")
       x = "Vites Turu:(0: Otomatik, 1: Manuel),
       y = MPG:("Mil/Galon") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_minimal()

# 6. Sonu?? Tablosu --------------------------------------------------------
sonuc_tablosu <- mtcars %>% 
  select(
    MPG = mpg,
    Silindir = cyl,
    Agirlik = wt,
    Beygir = hp,
    Vites=am,
  ) %>% 
  arrange(desc(MPG))

cat("\n??lk 5 G??zlem:\n")
print(head(sonuc_tablosu, 5))



