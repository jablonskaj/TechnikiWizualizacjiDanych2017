library(dplyr)
library(readxl)
library(ggplot2)

# zaladowanie i obrobka danych
dane = readxl::read_excel("jedzeniex.xlsx")

# interesujace nas kategorie
najczestsze = c("pieczywo", "wędliny", "warzywa", "owoce", "ziemniaki", "jogurty")

# braki w danych specjalnie umieszczone dla nowej daty gdzie nie bylo badania
dane <- filter(dane, dane$Rodzaj %in% najczestsze | is.na(dane$Rodzaj))

# przezwanie etykiety
dane$Czas_label <- ifelse(dane$Czas_label=='IX 2015', 'IX 2015 (brak danych)', dane$Czas_label)

# sztuczne nadanie brakom danych z roku 2015 odpowiednich wartosci i etykiet - spojnosc legend itd.
dane$Rodzaj <- ifelse(is.na(dane$Rodzaj), "pieczywo", dane$Rodzaj)
dane$Rodzaj <- factor(x = dane$Rodzaj, levels = najczestsze)
dane$Kod <- ifelse(is.na(dane$Wartosc), 1, 0)
dane$Wartosc <- ifelse(is.na(dane$Wartosc), 999, dane$Wartosc)


# kod do wyrysowania grafiki
p <- ggplot(dane, aes(x = Czas_group, y = Wartosc, na.rm = F, colour = Rodzaj)) +
  geom_point(show_guide = FALSE, size = 2) +
  geom_line(data = filter(dane, Kod==0), lwd = 1.05) +
  scale_y_continuous(limits = c(10, 65), breaks = seq(10, 60, 10), 
                     labels = paste0(seq(10, 60, 10), rep(" %", 6))) +
  scale_x_continuous(breaks = 1:6, labels = unique(dane$Czas_label)) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = -10, hjust = 0)) +
  labs(x = "Data badania", y = "% osób wyrzucających dane produkty", 
       title = "6 najczęściej wyrzucanych przez Polaków produktów spożywczych") +
  geom_vline(xintercept = 4, col = "white", lwd = 1.1)

# zapis
ggsave("wykres_kamil.png", plot = p, device = "png")


  
  