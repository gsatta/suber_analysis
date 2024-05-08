################################################################################

#                           Fluorescenza

################################################################################

# Load the necessary packages
library(xml2); library(dplyr); library(tidyr); library(ggplot2); library(car)

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

# Calcola l'interazione tra inoculum e treatment
fluo$interaction <- interaction(fluo$inoculum, fluo$treatment)

# Convert to factor the columns inoculum and treatments
fluo$inoculum <- as.factor(fluo$inoculum)
fluo$treatment <- as.factor(fluo$treatment)
fluo$interaction <- as.factor(fluo$interaction)

# Trasforma la variabile di interazione in numerico assegnando un valore specifico a ciascuna categoria
fluo$interaction_numeric <- as.numeric(factor(fluo$interaction, levels = unique(fluo$interaction)))

# Trova tutte le possibili interazioni
interactions <- unique(fluo$interaction)

# Inizializza un vettore per memorizzare i risultati
shapiro_results <- vector("list", length(interactions))

# Esegui il test di normalità Shapiro-Wilk per ciascuna interazione
for (i in 1:length(interactions)) {
  shapiro_results[[i]] <- shapiro.test(fluo$`Fv/Fm`[fluo$interaction == interactions[i]])
}

# Stampa i risultati
names(shapiro_results) <- interactions
print(shapiro_results)

# > print(shapiro_results)
# $NI.NT
# 
# Shapiro-Wilk normality test
# 
# data:  fluo$residuals[fluo$interaction == interactions[i]]
# W = 0.85995, p-value = 0.0001583
# 
# 
# $NI.T
# 
# Shapiro-Wilk normality test
# 
# data:  fluo$residuals[fluo$interaction == interactions[i]]
# W = 0.90094, p-value = 0.002037
# 
# 
# $PC.NT
# 
# Shapiro-Wilk normality test
# 
# data:  fluo$residuals[fluo$interaction == interactions[i]]
# W = 0.81468, p-value = 1.391e-05
# 
# 
# $PC.T
# 
# Shapiro-Wilk normality test
# 
# data:  fluo$residuals[fluo$interaction == interactions[i]]
# W = 0.7576, p-value = 9.888e-07

# I residui non seguono una distribuzione normale

# Esegui il test di Levene
leveneTest(`Fv/Fm` ~ inoculum:treatment, data = fluo)

# > leveneTest(`Fv/Fm` ~ inoculum:treatment, data = fluo)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.9364 0.4246
#       156  


# Le varianze tra i gruppi delle interazioni non sono significativamente diverse.
# Si rispetta l'assunzione della omoschedasticità'

################################
# Visto che si rispetta l'omoschedasticità, ma non la normalità dei gruppi, non si
# può fare anova, ma si possono fare altre analisi.

# Proviamo a eseguire il test di Kruskal-Wallis con il formato corretto
kruskal.test(`Fv/Fm` ~ interaction, data = fluo)

# > kruskal.test(`Fv/Fm` ~ interaction, data = fluo)
# 
# Kruskal-Wallis rank sum test
# 
# data:  Fv/Fm by interaction
# Kruskal-Wallis chi-squared = 9.3474, df = 3,
# p-value = 0.02501

# c'è almeno una differenza significativa tra le mediane dei gruppi dell'interazione
# Quindi si possono fare i test post hoc

library(exactRankTests)

# Esegui un test di Wilcoxon per confronti multipli (test di Steel)
wilcox.exact(fluo$`Fv/Fm`, fluo$interaction_numeric, alternative ="two.sided", paired = F)

# ci sono differenze significative nei valori di Fv/Fm tra i diversi gruppi definiti dalla variabile di interazione numerica

# Carica il pacchetto
library(dunn.test)

# Esegui il test di Dunn
dunn.test_fluo <- dunn.test(fluo$`Fv/Fm`, fluo$interaction, method = "bonferroni")

# > dunn.test(fluo$`Fv/Fm`, fluo$interaction, method = "bonferroni")
# Kruskal-Wallis rank sum test
# 
# data: x and group
# Kruskal-Wallis chi-squared = 9.3474, df = 3, p-value = 0.03
# 
# 
# Comparison of x by group                            
# (Bonferroni)                                  
# Col Mean-|
#   Row Mean |      NI.NT       NI.T      PC.NT
# ---------+---------------------------------
#   NI.T |   0.877358
#      1.0000
# -|
#   PC.NT |   1.250265   0.372907
# -|     0.6336     1.0000
# -|
#   PC.T |   2.974812   2.097453   1.724546
# -|    0.0088*     0.1079     0.2538
# 
# alpha = 0.05
# Reject Ho if p <= alpha/2

# Crea un dataframe per i risultati
dunn_df <- data.frame(
  Comparison = dunn.test_fluo$comparisons,
  P_adjusted = dunn.test_fluo$P.adjusted
)

# Ordina il dataframe per valori p corretti crescenti
dunn_df <- dunn_df[order(dunn_df$P_adjusted), ]

# Crea un grafico a barre con ggplot2
barplot <- ggplot(dunn_df, aes(x = Comparison, y = P_adjusted)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(P_adjusted, 3)), vjust = -0.5, size = 3, color = "black") +  # Aggiunge i valori di P_adjusted all'interno delle barre
  labs(title = "Dunn test results - fluorimeter",
       x = "Groups pairs",
       y = "P value adjusted") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Orienta le etichette sull'asse x
  geom_hline(yintercept = 0.05, color = "red")  # Aggiunge una linea orizzontale rossa a y = 0.05

# Visualizza il grafico
print(barplot)
























# PARTE NON UTILIZZABILE VISTE LE ASSUNZIONI NON RISPETTATE

# ################################## ANOVA #######################################
# 
# # Esegui l'analisi della varianza (ANOVA)
# anova_result <- aov(`Fv/Fm` ~ inoculum + treatment + inoculum:treatment, data = fluo)
# 
# summary(anova_result)
# 
# # Df  Sum Sq  Mean Sq F value
# # inoculum             1 0.00664 0.006644   5.773
# # treatment            1 0.00239 0.002395   2.081
# # inoculum:treatment   1 0.00075 0.000753   0.654
# # Residuals          156 0.17952 0.001151        
# # Pr(>F)  
# # inoculum           0.0174 *
# #   treatment          0.1511  
# # inoculum:treatment 0.4199  
# # Residuals                  
# # ---
# #   Signif. codes:  
# #   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# # Non c'è differenza significativa 
# 
# 
# ################################# TukeyHSD #####################################
# 
# # Esegui il test di Scheffé (LSD) per confronti multipli
# lsd_result <- TukeyHSD(anova_result)
# 
# # Mostra i risultati del test di Scheffé (LSD)
# print(lsd_result)
# 
# # Tukey multiple comparisons of means
# # 95% family-wise confidence level
# # 
# # Fit: aov(formula = `Fv/Fm` ~ inoculum + treatment + inoculum:treatment, data = fluo)
# # 
# # $inoculum
# # diff         lwr          upr     p adj
# # PC-NI -0.0128875 -0.02348237 -0.002292633 0.0174481 # Ci sono differenze tra gli inoculi
# # 
# # $treatment
# # diff         lwr         upr     p adj
# # T-NT -0.0077375 -0.01833237 0.002857367 0.1511478
# # 
# # $`inoculum:treatment`
# # diff        lwr           upr     p adj
# # PC:NT-NI:NT -0.008550 -0.0282489  0.0111488963 0.6733471
# # NI:T-NI:NT  -0.003400 -0.0230989  0.0162988963 0.9698821
# # PC:T-NI:NT  -0.020625 -0.0403239 -0.0009261037 0.0362874 # Questo è significativo... differenza tra PC-T e NI-NT 
# # NI:T-PC:NT   0.005150 -0.0145489  0.0248488963 0.9049623
# # PC:T-PC:NT  -0.012075 -0.0317739  0.0076238963 0.3862346
# # PC:T-NI:T   -0.017225 -0.0369239  0.0024738963 0.1093765
# 
# 
# # I p values indicano che ci sono delle differenze significative tra 
# # gli inoculum e non tra i trattamenti
# 
# library(multcompView)
# 
# plot(lsd_result)
# 
# 
