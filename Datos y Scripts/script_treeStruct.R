#Cargar libreria en uso

library(tidyverse)
library(ggpubr)
library(readxl)
library(lmerTest)
library(glmmTMB)

#Cargar datos estructura arborea

treeStruc <- read_excel("treeStruc_2024-3.xlsx", sheet = "Sheet1")

#ajustar datos
treeStruc$DAP = as.numeric(treeStruc$DAP)
treeStruc$dap1 = as.numeric(treeStruc$dap1)
treeStruc$parc_elev = as.numeric(treeStruc$parc_elev)
treeStruc$agno = as.integer(treeStruc$agno)



############################################################
#diametric distribution x year

ggplot(treeStruc, aes(as.numeric(DAP))) +
  geom_histogram(binwidth = 10) +
  facet_grid(. ~ agno, scales = 'free_y' ) + #scales = 'free_y'
  theme_bw()


#diametric distribution along gradient 
ggplot(treeStruc, aes(as.numeric(DAP), fill=spp)) +
  geom_histogram(binwidth = 10) +
  facet_grid(parc_elev ~ agno, scales = 'free_y' ) + #scales = 'free_y'
  labs(x = "Diametric distribution (cm)", y = "Trees frecuency") +
  xlim(0,250) +
  labs(fill="Species") +
  theme_bw()
  
 #diametric distribution struct deevelop
  ggplot(treeStruc, aes(as.numeric(DAP))) +
    geom_histogram(binwidth = 10) +
    facet_grid(estruct ~ agno, scales = 'free_y' ) + #scales = 'free_y'
    labs(x = "Diametric distribution (cm)", y = "Trees frecuency") 
    xlim(0,200)
  theme_bw()


 #diametric distribution struct deevelop and species
 treeStruc %>% 
   filter(nombre %in% c("Rauli", "Coihue", "Lenga", "Roble", "Araucaria")) %>% 
   
   ggplot(aes(as.numeric(DAP), fill= nombre)) +
    geom_histogram(binwidth = 10) +
    facet_grid(estruct ~ agno, scales = 'free_y' ) + #scales = 'free_y'
    labs(x = "Diametric distribution (cm)", y = "Trees frecuency") +
  xlim(0,200) +
  theme_bw()
  
  
  
  
  

############################################################
#DAP variation - mean comparison between yrs

ggplot(treeStruc, aes(factor(agno), as.numeric(DAP))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  facet_wrap(parc_elev~. , scales = 'free_y')


ggplot(treeStruc, aes(factor(agno), as.numeric(DAP))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  facet_grid(parc_elev~spp , scales = 'free_y')


############################################################
###ANoVA - modelo  aditivo
anova_A <- aov(as.numeric(DAP) ~  parc_elev + agno, treeStruc) #parc_elev
summary(anova_A) 

par(mfrow = c(2,2))
plot(anova, ask = FALSE)  

##no signficative result


###ANoVA - modelo con interaccion
anova_I <- aov(as.numeric(DAP) ~  parc_elev * agno, treeStruc) #parc_elev
summary(anova_I) 

par(mfrow = c(2,2))
plot(anova_I, ask = FALSE) 
dev.off()


############################################################
###calcular promedios
with(treeStruc,expr = tapply(as.numeric(DAP),  parc_elev, mean, na.rm = TRUE))
with(treeStruc,expr = tapply(as.numeric(DAP),  parc_elev, sd, na.rm = TRUE))

with(treeStruc,expr = tapply(as.numeric(DAP),  agno, mean, na.rm = TRUE))
with(treeStruc,expr = tapply(as.numeric(DAP),  agno, sd, na.rm = TRUE))




############################################################
############################################################
####compare growth rate between 2014-2018 & 2018-2024 periods#######

TOgrowthRate1<- treeStruc %>% select( agno,parc_elev, arb_ID, DAP) %>% 
  mutate(row = row_number()) %>%                            #create artificially a variable/column to keep all the datas
  pivot_wider(names_from= agno,                             #modify the table to a wide format
              names_prefix = "yr_" , 
              values_from= DAP, 
              values_fill = 0) %>% 
  select(-row) %>%                                          #remove to group and summarise table
  group_by(parc_elev, arb_ID) %>% 
  summarise(sum14=sum(yr_2014),
            sum18=sum(yr_2018),
            sum24=sum(yr_2024))  %>% 
  mutate(growthRate1= .data[['sum18']] - .data[['sum14']],  #calculate diference
         growthRate2= .data[['sum24']] - .data[['sum18']])




#pero!!!
#mean growth rate per yr
TOgrowthRate2<- treeStruc %>% select( agno,parc_elev, arb_ID, nombre, DAP) %>% 
  mutate(row = row_number()) %>%                            #create artificially a variable/column to keep all the datas
  pivot_wider(names_from= agno,                             #modify the table to a wide format
              names_prefix = "yr_" , 
              values_from= DAP, 
              values_fill = 0) %>% 
  select(-row) %>%                                          #remove to group and summarize table
  group_by(parc_elev, arb_ID, nombre) %>% 
  summarise(sum14=sum(yr_2014),
            sum18=sum(yr_2018),
            sum24=sum(yr_2024), .groups = "drop")  %>% 
  mutate(gr18_14= (.data[['sum18']] - .data[['sum14']])/4,  #calculate difference between 4 yrs
         gr24_18= (.data[['sum24']] - .data[['sum18']])/5)  #calculate difference between 5 yrs (considering growthperiod, not calendary yrs)

write.csv(TOgrowthRate2, "gr.data.csv")

#preparing to plot in ggplot

TOgrowthRate3<- TOgrowthRate2 %>%                           # arb_ID,TOgrowthRate1 puede ser cambiado xTOgrowthRate2 para evaluar tasa crec/agno
  select(parc_elev, gr18_14, gr24_18) %>% 
  pivot_longer(!parc_elev, names_to = "growthRate", values_to = "count") 

TOgrowthRate3 %>% 
  filter(between(parc_elev, 930, 1200)) %>%   #just to exclude recent plots and compare the growth rate between 2 periods 
  filter(count >= 0) %>% 
  filter(count <=5) %>% 
  ggplot(aes(as.factor(parc_elev), count, fill= growthRate)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "YlGnBu", labels=c('2014-2018', '2018-2024')) +
  ylim(0,5) +
  labs(x = "Plots elavation (m a.s.l)", y = "Groth rate (cm/yr)") +
  guides(fill=guide_legend("Periods")) +
  theme_bw() +
  stat_compare_means(method="wilcox.test", label = "p", paired = FALSE) #cannot use until R version is updated


TOgrowthRate3 %>% 
  filter(between(parc_elev, 930, 1200)) %>%
  write.csv("gr.data.test.csv")



###testing the full dataset
TOgrowthRate3 %>% 
  filter(between(parc_elev, 920, 1200)) %>%
  filter(count >= 0) %>% 
ggplot(aes(growthRate, count)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  geom_jitter(alpha=0.3) +
  facet_wrap(parc_elev~.) +   #faceting per plot if necesary
  ylim(0,3) +
  labs(x = "Periodos de crecimiento", y = "Tasa de crecimiento (cm)") +
  theme_bw() +
  stat_compare_means() #Wilcox.test by default, highly different when considering the whole dataset, but within plot NO


compare_means()



gr.model.data= TOgrowthRate2 %>% 
  filter(between(parc_elev, 920, 1200)) %>%
  #select(parc_elev, arb_ID, nombre, gr18_14, gr24_18) %>% 
  #filter(nombre %in% c("Rauli", "Coihue", "Lenga", "Roble")) %>% 
  pivot_longer(cols = c(gr18_14, gr24_18), 
               names_to = "growth_period", 
               values_to = "growth_rate") %>% 
  filter(growth_rate >= 0) %>% 
  filter(growth_rate <4)

gr.model.data$parc_elev = as.factor(gr.model.data$parc_elev)
gr.model.data$arb_ID = as.factor(gr.model.data$arb_ID)
gr.model.data$nombre = as.factor(gr.model.data$nombre)
gr.model.data$growth_period = as.factor(gr.model.data$growth_period)

write.csv(gr.model.data, "gr.model.data.csv")

#plot data distribution
gr.model.data %>%
  ggplot(aes(growth_rate, fill=growth_period)) +
  geom_density(alpha=0.4) +
  facet_grid(parc_elev~nombre) + 
  theme_bw()






#model 1 
gr.model <- glmer(growth_rate ~ growth_period * parc_elev + (1 | arb_ID) + (1 | nombre), #nombre y arb como factor aleatorio
              data = gr.model.data,
              family=poisson())
gr.model
  summary(gr.model)
  
  anova(gr.model)
  
#model 2
  gr.model2 <- lmer(growth_rate ~ growth_period * parc_elev + (1 | nombre), 
                   data = gr.model.data)
  
  summary(gr.model2)
  
  

#gr.model3--with glm
gr.model3 <- glm(growth_rate ~ growth_period + parc_elev , 
                    data = gr.model.data,
                 family= poisson)
  
  summary(gr.model3)

#gr.model4--with glm
  gr.model4 <- glm(growth_rate ~ growth_period * parc_elev , 
                   data = gr.model.data,
                   family= poisson)
  
  summary(gr.model4)
  par(mfrow= c(2, 2))
  plot(gr.model5, ask= FALSE)
  
  
  #gaussian model
  gr.model4.4 <- glm(growth_rate ~ growth_period * parc_elev , 
                   data = gr.model.data,
                   family= gaussian())
  
  summary(gr.model4.4)
  par(mfrow= c(2, 2))
  plot(gr.model4.4, ask= FALSE)
  
  
 #################################################################### 
#gr.model5--with glm
  gr.model5 <- glm(growth_rate ~ growth_period * parc_elev * nombre, 
                   data = gr.model.data,
                   family= gaussian)
  
  summary(gr.model5)
  par(mfrow= c(2, 2))
  plot(gr.model5, ask= FALSE)
  
  dev.off()
  
glm.Gaus.Null <- glm(growth_rate ~ 1, data = gr.model.data, family = gaussian())
summary(glm.Gaus.Null)

anova(gr.model5, glm.Gaus.Null , test ="F" )
  


#gr.model6--with glm
  gr.model6 <- glm(growth_rate ~ growth_period * parc_elev *nombre, 
                   data = gr.model.data,
                   family= poisson())
  
  summary(gr.model6)
  par(mfrow= c(2, 2))
  plot(gr.model6, ask= FALSE)
  
  dev.off()

  
  glm.Pois.Null <- glm(growth_rate ~ 1, data = gr.model.data, 
                       family = poisson)
  summary(glm.Pois.Null)
  
  anova(gr.model5, glm.Pois.Null , test ="F" )
  
  anova(gr.model6, gr.model5)
  

  
 
  
  #model7 with glmm
  library(glmmTMB)
  gr.model7<- glmmTMB(growth_rate ~ growth_period * nombre + (1 | parc_elev), 
                  data = gr.model.data,
                  family= gaussian)
  
  summary(gr.model7)
  par(mfrow= c(2, 2))
  plot(gr.model7)
  
  dev.off()

###visual comparison of growth rate

ggplot(gr.model.data, aes(growth_period, growth_rate)) +
  geom_boxplot() +
  geom_jitter(alpha=0.3, width = 0.2) +
  stat_summary(fun.y=mean, geom="point") +
  ylim(0,2) +
  stat_compare_means(label = "p", label.y =1.8) + #pero con wilcox.text
  theme_bw()


#faceting data by species name
ggplot(gr.model.data, aes(growth_period, growth_rate)) +
  geom_boxplot() +
  geom_jitter(alpha=0.3, width = 0.2) +
  stat_summary(fun.y=mean, geom="point") +
  ylim(0,2) +
  facet_grid(nombre~.) +   
  stat_compare_means(label = "p", label.y =1.8) + #pero con wilcox.text
  theme_bw()


#faceting data by plot elevation
ggplot(gr.model.data, aes(growth_period, growth_rate)) +
  geom_boxplot() +
  geom_jitter(alpha=0.3, width = 0.2) +
  stat_summary(fun.y=mean, geom="point") +
  ylim(0,2) +
  facet_grid(parc_elev~.) +   
  stat_compare_means(label = "p", label.y =1.8) + #pero con wilcox.text
  theme_bw()



#faceting data by species name and plot elevation
ggplot(gr.model.data, aes(growth_period, growth_rate)) +
  geom_boxplot() +
  geom_jitter(alpha=0.3, width = 0.2) +
  stat_summary(fun.y=mean, geom="point") +
  ylim(0,2) +
  facet_grid(nombre~parc_elev) +   #parc_elev
  stat_compare_means(label = "p", label.y =1.8) + #pero con wilcox.text
  theme_bw()


ggplot(gr.model.data, aes(growth_period, growth_rate)) +
  geom_boxplot() +
  geom_jitter(alpha=0.3, width = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"))




####################################################################
####################################################################

################################################################
################  health status   ################


#diametric distribution - health status
ggplot(treeStruc, aes(as.numeric(DAP), fill= estado_sanitario)) +
  geom_histogram(binwidth = 10) +
  facet_grid(parc_elev ~ agno, scales = 'free_y' ) + #scales = 'free_y'
  theme_bw()


treeStruc %>%
  select(agno, parc_elev, estado_sanitario) %>% 
  group_by(agno, parc_elev, estado_sanitario) %>% 
  summarize(suma = n()) %>% 
 
ggplot( aes(fill= estado_sanitario, as.factor(agno), suma)) +
  geom_bar(position="fill", stat="identity") +
  facet_grid(parc_elev~.)


  
  ############################################################  
  #vision gral  
  ggplot(treeStruc, aes(as.factor(agno), fill=estado_sanitario)) +
    geom_bar() +
    theme_bw() +
    labs(x = "Año medición", y = "cantidad árboles", fill = "Estado sanitario")  +
    scale_fill_manual(values=c("darkgreen",
                               "lightgreen",
                                "orange", 
                                "darkgrey")) 

  
  ggplot(treeStruc, aes(as.factor(agno), fill=estado_sanitario)) +
    geom_bar(position="fill") +
    theme_bw() +
    labs(x = "Año medición", y = "Proporción", fill = "Estado sanitario") +
    scale_fill_manual(values=c("darkgreen",
                               "lightgreen",
                               "orange", 
                               "darkgrey")) 
  
  
  ggplot(treeStruc, aes(as.factor(agno), fill=estado_sanitario)) +
    geom_bar(position="fill") +
    theme_bw() +
    labs(x = "Año medición", y = "Proporción", fill = "Estado sanitario") +
    facet_grid(estruct~.) +
    scale_fill_manual(values=c("darkgreen",
                               "lightgreen",
                               "orange", 
                               "darkgrey")) 



###nivel de especies
treeStruc %>% 
  filter(spp=="Nothofagus alpina") %>%   #nombre spp puede ser reemplazado
  
  ggplot( aes(as.factor(agno), fill=estado_sanitario)) +
  geom_bar(position="fill") +
  theme_bw() +
  labs(x = "Año medición", y = "Proporción", fill = "Estado sanitario") +
  scale_fill_manual(values=c("darkgreen",
                             "lightgreen",
                             "orange", 
                             "darkgrey")) 
  

####################################################################
####################################################################

#variacion interanual de cantidad arb vivos/muertos en las dist. parcelas

treeStruc %>% 
  select(agno, parc_elev, E_vida) %>% 
  filter(parc_elev %in% c("940", "968", "1050", "1100", "1190", "1300")) %>% 
  group_by(agno, parc_elev, E_vida) %>%
  summarise(n = n()) %>% 

ggplot(aes(as.factor(agno),n , color= E_vida, group=E_vida)) +
  geom_line(size=1, linetype = "dashed", arrow=arrow()) +
  geom_point(size=3) + 
  facet_wrap(parc_elev~., scale = "free") +
  theme_bw() +
  labs(x="years", y= "Number of trees", color="")
  

####################################################################
####################################################################

#variacion interanual de area basal en las dist. parcelas

treeStruc %>% 
  select(agno, parc_elev, A_basal) %>% 
  filter(parc_elev %in% c("940", "968", "1050", "1100", "1190", "1300")) %>% 
  group_by(agno, parc_elev) %>%
  summarise(sum = sum(A_basal), .groups ="drop") %>% 
  
  
  ggplot() +
  geom_bar(aes(factor(agno), sum , fill= factor(agno)),
  stat= "identity", position="dodge") + 
  facet_grid(.~parc_elev) +
  theme_bw() +
  labs(x="Plot elevation", y= "Basal area (m^2/plot)", fill='Years') +
  scale_fill_brewer(palette="Dark2")

#test NO diference :)
AB.data<- treeStruc %>% 
  select(agno, parc_elev, A_basal) %>% 
  group_by(agno, parc_elev) %>%
  summarise(sum = sum(A_basal), .groups ="drop") %>% 
  complete(parc_elev = unique(parc_elev),
           agno = unique(agno),
           fill = list(basal_area = 0)) 

anovaAB<-lm(sum ~ factor(agno) * parc_elev, AB.data)
anova(anovaAB)



#compare to Mean Cuadratic Diameter
treeStruc %>%
  group_by(parc_elev, agno) %>%
  summarise(
    diam_cuadratico = sqrt(mean(DAP^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(diam_promedio, diam_cuadratico),
               names_to = "tipo_diametro",
               values_to = "valor") %>% 
  filter(tipo_diametro=='diam_cuadratico') %>%
  filter(parc_elev %in% c("940", "968", "1050", "1100", "1190", "1300")) %>% 
  
ggplot() +
  geom_bar(aes(factor(agno), valor, fill = factor(agno)), 
           stat= "identity", position = "dodge") +
  facet_grid(.~parc_elev) +
  labs(x = "Plot elavation", y = "Quadratic Mean Diameter (cm)" ,
       fill = "Years") +
  theme_bw() +
  ylim(0,80) +
  scale_fill_brewer(palette="Dark2")


#compare Mean Diameter ->do not use, cause DMC is more sensible and have more biological sence
treeStruc %>%
  group_by(parc_elev, agno) %>%
  summarise(
    diam_promedio = mean(DAP, na.rm = TRUE),
    diam_sd = sd(DAP / sqrt(n()), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(upper = diam_promedio + diam_sd,
          lower = pmax(diam_promedio - diam_sd,0)) %>% 
  filter(parc_elev %in% c("940", "968", "1050", "1100", "1190", "1300")) %>% 
  
  ggplot() +
  geom_bar(aes(factor(agno), diam_promedio, fill= factor(agno)), stat = "identity") +
  geom_errorbar(aes(x = factor(agno), ymin = lower, ymax = upper), 
                position=position_dodge(1.9)) + 
  facet_grid(.~parc_elev) +
  labs(x = "", y = "Mean diameter (cm)", fill = "Years") +
  theme_bw() +
  #ylim(10,200) +
  scale_fill_brewer(palette="Dark2")




treeStruc %>%  
  filter(parc_elev %in% c("940", "968", "1050", "1100", "1190", "1300")) %>% 

ggplot(aes(factor(agno), DAP, fill= factor(agno))) +
  geom_boxplot() +
  geom_jitter(alpha=0.2) +
  facet_grid(.~parc_elev) +
  labs(x = "", y = "Mean diameter (cm)", fill = "Years") +
  theme_bw() +
  ylim(5,150) +
  scale_fill_brewer(palette="Dark2")
