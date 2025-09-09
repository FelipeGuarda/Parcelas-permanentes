library(tidyverse)
library(readxl)
library(lme4)

#Cargar datos
treeStruc <- read_excel("G:\\Shared drives\\FMA-G\\C-CONSERVACIÓN\\C1-BOSQUE PEHUEN\\C1.6_ESTUDIOS BP\\UACH 2023-2024 (RIcardo Moreno)\\Monitoreo parcelas permanentes\\Entrega Ricardo 02 Sept 2025\\Datos y Scripts\\treeStruc_2024-4.xlsx", sheet = "Sheet1")


####data fixing
treeStruc$DAP <- as.numeric(treeStruc$DAP)
treeStruc$dap1 <- as.numeric(treeStruc$dap1)
treeStruc$parc_elev <- as.numeric(treeStruc$parc_elev)
treeStruc$agno <- as.integer(treeStruc$agno)


############  ############  ############  ############
###########Analisis estructura############

#estructura gral

ggplot(treeStruc, aes(as.numeric(DAP))) +
  geom_histogram(binwidth = 10) +
  theme_bw()


# estructura x parc entre agnos
ggplot(treeStruc, aes(as.numeric(DAP))) +
  geom_histogram(binwidth = 10) +
  facet_grid(parc_elev ~ agno, scales = 'free_y' ) + #scales = 'free_y'
  theme_bw()


# estructura x parc entre agnos --clasif x E° sanitario
ggplot(treeStruc, aes(as.numeric(DAP), fill= estado_sanitario)) +
  geom_histogram(binwidth = 10) +
  facet_grid(parc_elev ~ agno, scales = 'free_y' ) + #scales = 'free_y'
  theme_bw()


#
ggplot(treeStruc, aes(factor(agno), as.numeric(DAP))) +
    geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  facet_wrap(parc_elev~. , scales = 'free_y')



ggplot(treeStruc, aes(factor(nombre), as.numeric(DAP), 
                      colour = as.factor(agno) )) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) #+
  facet_grid(parc_elev~. , scales = 'free_y')



###ANoVA - modelo  aditivo
anova_A <- aov(as.numeric(DAP) ~  parc_elev + agno, treeStruc) #parc_elev
summary(anova_A) 

par(mfrow = c(2,2))
plot(anova, ask = FALSE)  


  
   
###ANoVA - modelo con interaccion
anova_I <- aov(as.numeric(DAP) ~  parc_elev * agno, treeStruc) #parc_elev
summary(anova_I) 

par(mfrow = c(2,2))
plot(anova_I, ask = FALSE)  




###calcular promedios
with(treeStruc,expr = tapply(as.numeric(DAP),  parc_elev, mean, na.rm = TRUE))
with(treeStruc,expr = tapply(as.numeric(DAP),  parc_elev, sd, na.rm = TRUE))

with(treeStruc,expr = tapply(as.numeric(DAP),  agno, mean, na.rm = TRUE))
with(treeStruc,expr = tapply(as.numeric(DAP),  agno, sd, na.rm = TRUE))



####################################################################
####compare growth rate between 2014-2018 & 2018-2024 periods#######

TOgrowthRate1<- treeStruc %>% select( agno,parc_elev, arb_ID, dap1) %>% 
  mutate(row = row_number()) %>%                            #create artificially a variable/column to keep all the datas
  pivot_wider(names_from= agno,                             #modify the table to a wide format
              names_prefix = "yr_" , 
              values_from= dap1, 
              values_fill = 0) %>% 
  select(-row) %>%                                          #remove to group and summarize table
  group_by(parc_elev, arb_ID) %>% 
  summarise(sum14=sum(yr_2014),
            sum18=sum(yr_2018),
            sum24=sum(yr_2024))  %>% 
  mutate(growthRate1= (.data[['sum18']] - .data[['sum14']])/4,  #calculate difference - consider yrs between 
         growthRate2= (.data[['sum24']] - .data[['sum18']])/6)



#preparing to plot in ggplot

TOgrowthRate3<- TOgrowthRate1 %>%                           # TOgrowthRate1 puede ser cambiado xTOgrowthRate2 para evaluar tasa crec/agno
  select(parc_elev, growthRate1, growthRate2) %>% 
  pivot_longer(!parc_elev, names_to = "growthRate", values_to = "count") 


TOgrowthRate3 %>% 
  filter(between(parc_elev, 930, 1200)) %>%   #just to exclude recent plots and compare the growth rate between 2 periods 
  
  ggplot() +
  geom_boxplot(aes(as.factor(parc_elev), count, fill= growthRate)) +
  scale_fill_brewer(palette = "YlGnBu") +
  ylim(0,1.2) +
  theme_bw()



ggplot(TOgrowthRate3, aes(count, fill= growthRate , colour=growthRate)) +
geom_density(alpha = 0.3) + 
  xlim(-3,3) +
theme_bw()


###ANoVA - modelo  aditivo

anova_GR_ad <- aov(count ~  parc_elev + growthRate, TOgrowthRate3) #parc_elev
summary(anova_GR_ad) 

par(mfrow = c(2,2))
plot(anova_GR_ad, ask = FALSE)  


###ANoVA - modelo con interaccion
anova_GR_I <- aov(count ~  parc_elev * growthRate, TOgrowthRate3) #parc_elev
summary(anova_GR_I) 

par(mfrow = c(2,2))
plot(anova_I, ask = FALSE) 
              

############
library(lme4)

growth.glmm <- TOgrowthRate3 %>% mutate(growth= count^2) %>% 
  filter(growth > 0)

#data visualizaiton
ggplot(growth.glmm, aes(growth, fill= growthRate , colour=growthRate)) +
  geom_density(alpha = 0.3) + 
  xlim(-3,3) +
  theme_bw()


#glmm function
glmm.Growth <- glmer(growth ~  growthRate +(1| parc_elev), 
                     data=growth.glmm, 
                     family = poisson
                     )

print(summary(glmm.Growth), correlation=FALSE)
coef(glmm.Growth)               #A function that extracts the fixed effect coefficients








##################################################################
#####analisis plantulas
##################################################################

db_Plan <- read_excel("db_Plan&sppAsoc.xlsx")
View(db_Plan)

db_Plan$Plántula <- as.numeric(db_Plan$Plántula)
db_Plan$`Altura  (cm)` <- as.numeric(db_Plan$`Altura  (cm)`)



############average seedling per plot
# Calculate mean and standard deviation
  db_Plan %>%
    group_by(Fecha, Parcela, Ubicación) %>%
    summarise(sumPLT= sum(Plántula, na.rm=TRUE)) %>% 
    group_by(Parcela, Fecha) %>% 
    summarise(mean_plnt = mean(sumPLT, na.rm = TRUE),
            sd_plnt = sd(sumPLT, na.rm = TRUE)) %>% 

# Plot
ggplot( aes(x = as.factor(Parcela), y = mean_plnt)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_plnt - sd_plnt, ymax = mean_plnt + sd_plnt),
                width = 0.4, position = position_dodge(0.9)) +
    facet_grid(Fecha~.) +
  labs(title = "",
       x = "Plot elavation",
       y = "amount of seedling (mean + sd)")+
  theme_bw()


  
  ############  ############  ############
  ############average seedling per spp per plot
  
  # Calculate mean and standard deviation
  db_Plan %>%
    group_by(Fecha, Parcela, Ubicación, spp) %>%
    summarise(sumPLT= sum(Plántula, na.rm=TRUE)) %>% 
    group_by(Parcela,spp, Fecha) %>% 
    summarise(mean_plnt = mean(sumPLT, na.rm = TRUE),
              sd_plnt = sd(sumPLT, na.rm = TRUE)) %>% 
    filter(!(spp  %in% c( "N. sp", "n.i.", "Sin regeneración"))) %>% 
    
    # Plot
    ggplot( aes(x = as.factor(Parcela), y = mean_plnt)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_plnt - sd_plnt, ymax = mean_plnt + sd_plnt),
                  width = 0.4, position = position_dodge(0.9)) +
    facet_grid(spp~ Fecha) +
    labs(title = "",
         x = "Plot elavation (masl)",
         y = "seedling amount (mean + sd)")+
    theme_bw()
  

  ############  ############  ############
  ############average advanced seedling per spp per plot
  
  # Calculate mean and standard deviation
  db_Plan %>%
    group_by(Fecha, Parcela, Ubicación, spp) %>%
    summarise(sumADV= sum(reg_avanzada, na.rm=TRUE)) %>% 
    group_by(Parcela,spp, Fecha) %>% 
    summarise(mean_Aplnt = mean(sumADV, na.rm = TRUE),
              sd_Aplnt = sd(sumADV, na.rm = TRUE)) %>% 
    filter(!(spp  %in% c( "N. sp", "n.i.", "Sin regeneración"))) %>% 
    
    # Plot
    ggplot( aes(x = as.factor(Parcela), y = mean_Aplnt)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_Aplnt - sd_Aplnt, ymax = mean_Aplnt + sd_Aplnt),
                  width = 0.4, position = position_dodge(0.9)) +
    facet_grid(spp ~ Fecha, scales = 'free_y') +
    labs(title = "",
         x = "Plot elavation (masl)",
         y = "seedling amount (mean + sd)")+
    theme_bw()