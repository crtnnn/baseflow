library(lfstat)
library(dplyr)
library(ggplot2)

# 1970 - 2022
dunajec_1970_2022 <- read.csv('zglobice_q_1970_2022.csv')
dunajec_1970_2022$baseflow <- baseflow(dunajec_1970_2022$discharge)
all_discharge_1970_2022 <- sum(dunajec_1970_2022$discharge[dunajec_1970_2022$baseflow > 0], na.rm = TRUE)
all_baseflow_1970_2022 <- sum(dunajec_1970_2022$baseflow, na.rm = T)
ratio_1970_2022 <- round(all_baseflow_1970_2022 / all_discharge_1970_2022, 3)

round(colSums(dunajec_1970_2022[, c("discharge", "baseflow")], na.rm = TRUE), 0)

# 2012 - 2022
dunajec_2012_2022 <- read.csv('zglobice_q_2012_2022.csv')
dunajec_2012_2022$baseflow <- baseflow(dunajec_2012_2022$discharge)
all_discharge_2012_2022 <- sum(dunajec_2012_2022$discharge[dunajec_2012_2022$baseflow > 0], na.rm = TRUE)
all_baseflow_2012_2022 <- sum(dunajec_2012_2022$baseflow, na.rm = T)
ratio_2012_2022 <- round(all_baseflow_2012_2022 / all_discharge_2012_2022, 3)

round(colSums(dunajec_2012_2022[, c("discharge", "baseflow")], na.rm = TRUE), 0)

# Plot
ggplot(dunajec_2012_2022) +
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
  geom_line(aes(x = as.POSIXct(date), y = discharge), color = 'black', linewidth = 0.5) +
  geom_line(aes(x = as.POSIXct(date), y = baseflow), color = 'red', linewidth = 1)










