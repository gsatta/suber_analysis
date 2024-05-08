################################################################################

#                           Fluorescenza

################################################################################

# Load the necessary packages
library(xml2); library(dplyr); library(tidyr); library(ggplot2);

# Load the dataframes
fluo_0 <- readxl::read_excel("./FLUO_SPAD/Fluometer_quercus_seconda_prova.xlsx")

fluo_0$Date <- as.Date(fluo_0$Date, format = "%d-%m-%Y")

# View the dataframes structure
str(fluo_0) # Devo delezionare solo ID_plant, Date  e Fv/Fm

# Select only the the columns ID_plant, Date and Fv/Fm
fluo <- fluo_0 %>% 
  select(ID_plant, inoculum, treatment, Date, `Fv/Fm`)

# Calcola la deviazione standard per ogni combinazione di inoculum e treatment
fluo_summary <- fluo %>%
  group_by(inoculum, treatment, Date) %>%
  summarise(mean_FvFm = mean(`Fv/Fm`), sd_FvFm = sd(`Fv/Fm`))

# Plot per ogni combinazione di inoculum e treatment con la barra della deviazione standard
fluo_plot <- ggplot(fluo_summary, aes(x = Date, y = mean_FvFm, group = interaction(inoculum, treatment), color = interaction(inoculum, treatment))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_FvFm - sd_FvFm, ymax = mean_FvFm + sd_FvFm), width = 0.2) + 
  labs(x = "Date", y = "Mean Fv/Fm", color = "Inoculum & Treatment", title = "Mean Fv/Fm over Time with Standard Deviation") +
  theme_minimal() +  
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "10 days", date_labels = "%Y-%m-%d") 

fluo_plot

# Save the graphs in png
ggsave("./GRAPHS/fluo_plot.png", plot = fluo_plot, width = 8, height = 6, dpi = 300)

# boxplot della produzione per la varietà
boxplot(`Fv/Fm` ~ inoculum + treatment, data=fluo, col=c("red","green","pink","yellow"))

################################################################################

# Convert to factor the columns inoculum and treatments
fluo$inoculum <- as.factor(fluo$inoculum)
fluo$treatment <- as.factor(fluo$treatment)

# Fittiamo il modello
mod <- lm(`Fv/Fm` ~ inoculum + treatment + inoculum:treatment , data = fluo)

# Visual check dei dati
plot(mod, which = 1)

plot(mod, which = 2)

# aggiungiamo colonna dei residui
fluo <- fluo %>%
  mutate(residuals = residuals(mod))

# Controllare la normalità dei residui: shapiro test
shapiro.test(mod$residuals)

# Shapiro-Wilk normality test
# 
# data:  mod$residuals
# W = 0.82293, p-value = 1.209e-12

# Esegui l'analisi della varianza (ANOVA)
anova_result <- aov(`Fv/Fm` ~ inoculum + treatment + inoculum:treatment, data = fluo)

summary(anova_result)

# Df  Sum Sq  Mean Sq F value
# inoculum             1 0.00664 0.006644   5.773
# treatment            1 0.00239 0.002395   2.081
# inoculum:treatment   1 0.00075 0.000753   0.654
# Residuals          156 0.17952 0.001151        
# Pr(>F)  
# inoculum           0.0174 *
#   treatment          0.1511  
# inoculum:treatment 0.4199  
# Residuals                  
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Non c'è divverenza significativa 

# Esegui il test di Scheffé (LSD) per confronti multipli
lsd_result <- TukeyHSD(anova_result)

# Mostra i risultati del test di Scheffé (LSD)
print(lsd_result)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = `Fv/Fm` ~ inoculum + treatment + inoculum:treatment, data = fluo)
# 
# $inoculum
# diff         lwr          upr     p adj
# PC-NI -0.0128875 -0.02348237 -0.002292633 0.0174481 # Ci sono differenze tra gli inoculi
# 
# $treatment
# diff         lwr         upr     p adj
# T-NT -0.0077375 -0.01833237 0.002857367 0.1511478
# 
# $`inoculum:treatment`
# diff        lwr           upr     p adj
# PC:NT-NI:NT -0.008550 -0.0282489  0.0111488963 0.6733471
# NI:T-NI:NT  -0.003400 -0.0230989  0.0162988963 0.9698821
# PC:T-NI:NT  -0.020625 -0.0403239 -0.0009261037 0.0362874 # Questo è significativo... differenza tra PC-T e NI-NT 
# NI:T-PC:NT   0.005150 -0.0145489  0.0248488963 0.9049623
# PC:T-PC:NT  -0.012075 -0.0317739  0.0076238963 0.3862346
# PC:T-NI:T   -0.017225 -0.0369239  0.0024738963 0.1093765


# I p values indicano che ci sono delle differenze significative tra 
# gli inoculum e non tra i trattamenti

library(multcompView)

# Visualizza graficamente i risultati del test di Scheffé (LSD)
plot(lsd_result)

