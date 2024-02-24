library(remotes)
library(climate)
library(dplyr)

# Zgłobice wodowskaz
zglobice <- hydro_imgw_daily(
  2012:2023,
  coords = FALSE,
  station = "ZGŁOBICE",
  col_names = "polish",
  allow_failure = FALSE,
)

zglobice_all <- hydro_imgw_daily(
  1951:2023,
  coords = FALSE,
  station = "ZGŁOBICE",
  col_names = "polish",
  allow_failure = FALSE,
)

# usuwanie zbędnych kolumn
library(dplyr)
zglobice_all <-
  zglobice_all %>% select(
    -"Nazwa rzeki/jeziora",
    -"Kod stacji",
    -"Nazwa stacji",
    -"Grubosc lodu [cm]", -"Kod zjawiska lodowego (slownik ponizej)",
    -"Procent udzialu zjawiska lodowego [mnoznik *10; np. 3 oznacza 30% udzialu zjawisk lodowych]", -"Kod zarastania (slownik ponizej)"
  )

# zmiana nazw kolumn
colnames(zglobice_all) <-
  c(
    "rok_hydro",
    "msc_hydro",
    "dzien",
    "stan",
    "Q",
    "temp",
    "m_kalendarz"
  )

# dodawanie kolumny rok kalendarzowy
zglobice_all$rok_kalendarzowy <-
  ifelse(zglobice_all$msc_hydro %in% 1:2,
    zglobice_all$rok_hydro - 1,
    zglobice_all$rok_hydro
  )

column_order <- c("rok_hydro", "msc_hydro", "rok_kalendarzowy", "m_kalendarz", "dzien", "stan", "Q", "temp")
zglobice2 <- zglobice_all %>% select(all_of(column_order))

rownames(zglobice2) <- NULL

# najpierw sklejamy dane
names(zglobice2)
date <- paste(zglobice2$rok_kalendarzowy, zglobice2$m_kalendarz, zglobice2$dzien,
  sep = "-"
)
date

# następnie nadajemy format daty
date <- as.Date(date, format = "%Y-%m-%d")

# następnie dodajemy informację o dacie do tabeli

zglobice2 <- data.frame(zglobice2, date)

zglobice3 <-
  zglobice2 %>% select(
    -"rok_hydro",
    -"msc_hydro",
    -"rok_kalendarzowy",
    -"m_kalendarz",
    -"dzien",
    -"temp"
  )

column_order <- c("date", "stan", "Q")
zglobice3 <- zglobice3 %>% select(all_of(column_order))

# sprawdzamy czy jest ok
str(zglobice3)

zglobice_q <-
  zglobice3 %>% select(
    -"stan"
  )

any(is.na(zglobice_q$Q_filled))

library(zoo)
zglobice_q$Q_filled <- na.approx(zglobice_q$Q)

zglobice_q <-
  zglobice_q %>% select(
    -"Q"
  )
colnames(zglobice_q)[colnames(zglobice_q) == "Q_filled"] <- "discharge"


# dane te możemy zapisać na dysku
write.table(zglobice_q, file = "zglobice_q_1951_2023.txt", sep = "\t", row.names = FALSE)

write.csv(zglobice_q, file = "zglobice_q_1951_2023.csv", row.names = FALSE)



