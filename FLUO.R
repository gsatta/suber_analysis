################################################################################

#                           Fluorescenza

################################################################################

# Load the necessary packages
library(xml2); library(dplyr); library(tidyr); library(ggplot2);

# Load the dataframes
fluo_0 <- readxl::read_excel("./FLUO_SPAD/Fluometer_quercus_seconda_prova.xlsx")

fluo_0 <- na.omit(fluo_0)

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
  theme_bw() +
  theme(legend.position = "bottom")

# Save the graphs in png
ggsave("./GRAPHS/fluo_plot.png", plot = fluo_plot, width = 8, height = 6, dpi = 300)



# 
fluo$inoculum <- as.factor(fluo$inoculum)
fluo$treatment <- as.factor(fluo$treatment)


# Fittiamo il modello
mod <- lm(`Fv/Fm` ~ inoculum + treatment, data = fluo)

# Visual check dei dati
plot(mod, which = 1)

plot(mod, which = 2)

# aggiungiamo colonna dei residui
fluo <- fluo %>%
  mutate(residuals = residuals(mod))

# Controllare la normalità dei residui: shapiro test
shapiro.test(mod$residuals)

# Esegui l'analisi della varianza (ANOVA)
anova_result <- aov(`Fv/Fm` ~ inoculum + treatment, data = fluo)


summary(anova_result)

# Df  Sum Sq  Mean Sq F value Pr(>F)  
# inoculum      1 0.00664 0.006644   5.786 0.0173 *
#   treatment     1 0.00239 0.002395   2.086 0.1507  
# Residuals   157 0.18027 0.001148                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Non c'è divverenza significativa tra i tretamnet (pvalue = 0.15), invece 
# c'è tra gli inoculum (pvalue 0.017)




# Esegui il test di Scheffé (LSD) per confronti multipli
lsd_result <- TukeyHSD(anova_result)

# Mostra i risultati del test di Scheffé (LSD)
print(lsd_result)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = `Fv/Fm` ~ inoculum + treatment, data = fluo)
# 
# $inoculum
# diff         lwr          upr    p adj
# PC-NI -0.0128875 -0.02347016 -0.002304843 0.017319
# 
# $treatment
# diff         lwr         upr     p adj
# T-NT -0.0077375 -0.01832016 0.002845157 0.1506873


























spad_0 <- readxl::read_excel("./FLUO_SPAD/SPAD.xlsx")
str(spad_0)
