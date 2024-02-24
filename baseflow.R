# k_lewy_pas <- c(1.43e-3, 3.00e-3, 5.10e-4, 2.73e-3, 8.60e-4, 1.25e-3, 7.17e-4)
# mean(k)

library(lfstat)
library(dplyr)
library(ggplot2)

# 1951 - 2022
baseflow_1951_2023 <- read.table('Out_zglobice_q_1951_2022.txt', sep = '\t', header = T)
baseflow_1951_2023 <- baseflow_1951_2023 %>% select(-"X")
round(mean(baseflow_1951_2023$BFI.Index, na.rm = T), 2)
round(median(baseflow_1951_2023$BFI.Index, na.rm = T), 2)

all_discharge_1951_2023 <- sum(baseflow_1951_2023$Discharge[baseflow_1951_2023$Baseflow > 0], na.rm = TRUE)
all_baseflow_1951_2023 <- sum(baseflow_1951_2023$Baseflow, na.rm = T)
round(all_baseflow_1951_2023 / all_discharge_1951_2023, 2)

baseflow_1951_2023$baseflow_r <- baseflow(baseflow_1951_2023$Discharge)

# 2012 - 2022
baseflow_2012_2022 <- read.table('Out_zglobice_q_2012_2022.txt', sep = '\t', header = T)
baseflow_2012_2022 <- baseflow_2012_2022 %>% select(-"X")
round(mean(baseflow_2012_2022$BFI.Index, na.rm = T), 2)
round(median(baseflow_2012_2022$BFI.Index, na.rm = T), 2)

all_discharge_2012_2022 <- sum(baseflow_2012_2022$Discharge[baseflow_2012_2022$Baseflow > 0], na.rm = TRUE)
all_baseflow_2012_2022 <- sum(baseflow_2012_2022$Baseflow, na.rm = T)
round(all_baseflow_2012_2022 / all_discharge_2012_2022, 2)

baseflow_2012_2022$baseflow_r <- baseflow(baseflow_2012_2022$Discharge)


ggplot(baseflow_2012_2022) +
  theme_minimal() +
  labs(y = 'Flow (m3/s)') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 'bold', size = 10, color = 'black', family = 'sans'),
    axis.text.x = element_text(face = 'bold', size = 10, angle = 90, hjust = 0.5, vjust = 0.5, color = 'black', family = 'sans'), 
    axis.text.y = element_text(face = 'bold', size = 10, color = 'black', family = 'sans'),
    axis.ticks = element_line(color = 'black')
  ) + 
  scale_x_datetime(labels = scales::date_format('%m-%Y'), breaks = '1 year') +
  geom_line(aes(x = as.POSIXct(Date), y = Discharge), color = 'black', linewidth = 0.5) +
  geom_line(aes(x = as.POSIXct(Date), y = baseflow_r), color = 'red', linewidth = 1)


all_baseflow_1951_2023 / all_discharge_1951_2023
all_baseflow_2012_2022 / all_discharge_2012_2022

round(colSums(baseflow_2012_2022[, c("Discharge", "baseflow_r")], na.rm = TRUE), 0)








