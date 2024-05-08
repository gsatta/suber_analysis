################################################################################

#                                 SPAD

################################################################################

# Load the necessary packages
library(xml2); library(dplyr); library(tidyr); library(ggplot2);

# Load the dataframes
spad_0<- readxl::read_excel("./FLUO_SPAD/SPAD.xlsx")

# convert in long format the dataframe
spad_long <- pivot_longer(spad_0, 
                          cols = colnames(spad_0)[6:13],
                          names_to = "Date", 
                          values_to = "Value")

spad_long <- na.omit(spad_long)

# Select only the the columns ID_plant, Date and Fv/Fm
spad <- spad_long %>% 
  select(Codes, inoculum, treatment, Date, Value)

# Calcola la deviazione standard per ogni combinazione di inoculum e treatment
spad_summary <- spad %>%
  group_by(inoculum, treatment, Date) %>%
  summarise(mean_spad = mean(Value), sd_spad = sd(Value))

# Plot per ogni combinazione di inoculum e treatment con la barra della deviazione standard
spad_plot <- ggplot(spad_summary, aes(x = Date, y = mean_spad, group = interaction(inoculum, treatment), color = interaction(inoculum, treatment))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_spad - sd_spad, ymax = mean_spad + sd_spad), width = 0.2) + 
  labs(x = "Date", y = "Mean spad", color = "Inoculum & Treatment", title = "Mean spad over Time with Standard Deviation") +
  theme_bw() +
  theme(legend.position = "bottom")


# Save the graphs in png
ggsave("./GRAPHS/spad_plot.png", plot = spad_plot, width = 8, height = 6, dpi = 300)



