################################################################################

#                                 SPAD

################################################################################

# Load the necessary packages
library(xml2); library(dplyr); library(tidyr); library(ggplot2); library(scales)

# Load the dataframes
spad_0<- readxl::read_excel("./FLUO_SPAD/SPAD.xlsx")

# convert in long format the dataframe
spad_long <- pivot_longer(spad_0, 
                          cols = colnames(spad_0)[6:13],
                          names_to = "Date", 
                          values_to = "Value")

spad_long$Date <- as.Date(spad_long$Date, format = "%d-%m-%Y")

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
  theme_minimal() +  
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "10 days", date_labels = "%Y-%m-%d") 

spad_plot

# Save the graphs in png
ggsave("./GRAPHS/spad_plot.png", plot = spad_plot, width = 8, height = 6, dpi = 300)

################################################################################

# Convert to factor the columns inoculum and treatments
spad$inoculum <- as.factor(spad$inoculum)
spad$treatment <- as.factor(spad$treatment)

# Fittiamo il modello
mod <- lm(Value ~ inoculum + treatment, data = spad)

# Visual check dei dati
plot(mod, which = 1)

plot(mod, which = 2)

# aggiungiamo colonna dei residui
spad <- spad %>%
  mutate(residuals = residuals(mod))

# Controllare la normalità dei residui: shapiro test
shapiro.test(mod$residuals)

# 	Shapiro-Wilk normality test
# 
# data:  mod$residuals
# W = 0.96608, p-value = 0.0005786

################################### ANOVA ######################################

# Esegui l'analisi della varianza (ANOVA)
anova_result <- aov(Value ~ inoculum + treatment + inoculum:treatment, data = spad)

summary(anova_result)

# Df Sum Sq Mean Sq F value Pr(>F)  
# inoculum             1    142  141.94   6.522 0.0116 *
#   treatment            1      0    0.26   0.012 0.9124  
# inoculum:treatment   1      4    3.69   0.170 0.6811  
# Residuals          156   3395   21.76                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Non c'è divverenza significativa tra i treatments (pvalue = 0.9124), 
# non c'è nell'interazione tra inoculum:treatment , invece 
# c'è tra gli inoculum (pvalue 0.0116)


################################# TukeyHSD #####################################


# Esegui il test di Scheffé (LSD) per confronti multipli
lsd_result <- TukeyHSD(anova_result)

# Mostra i risultati del test di Scheffé (LSD)
print(lsd_result)

# > # Mostra i risultati del test di Scheffé (LSD)
#   > print(lsd_result)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Value ~ inoculum + treatment + inoculum:treatment, data = spad)
# 
# $inoculum
# diff       lwr        upr     p adj
# PC-NI -1.88375 -3.340746 -0.4267537 0.0116124 # Ci sono differenze tra l'PC e NI
# 
# $treatment
# diff       lwr      upr     p adj
# T-NT 0.08125 -1.375746 1.538246 0.9124298 # Non ci sono differenze
# 
# $`inoculum:treatment`
# diff       lwr      upr     p adj                     # non ci sono differenze
# PC:NT-NI:NT -1.5800 -4.288974 1.128974 0.4312026
# NI:T-NI:NT   0.3850 -2.323974 3.093974 0.9827710
# PC:T-NI:NT  -1.8025 -4.511474 0.906474 0.3127312
# NI:T-PC:NT   1.9650 -0.743974 4.673974 0.2392427
# PC:T-PC:NT  -0.2225 -2.931474 2.486474 0.9965580
# PC:T-NI:T   -2.1875 -4.896474 0.521474 0.1586047


# Visualizza graficamente i risultati del test di Scheffé (LSD)
plot(lsd_result)


























