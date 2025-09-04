#cargar paquetes usados
library(tidyverse)
library(readxl)
library(lme4)
library(emmeans)
library(rioja)
library(vegan)



############  ############  ########################  ############  ############
############diversity of species in the plots

sppAsoc <- read_excel("db_Plan_sppAsoc_2024-3.xlsx", 
                      sheet = "div_flr")
View(sppAsoc)



##############################################################
#######     RIQUEZA PROMEDIO 

##alternativa
sppAsoc %>%
  group_by(fecha,parcela, cuadrante) %>%
  filter(cuad_selec == 1 & 
           GrowthForm == "understory") %>%
  tally() %>%
  mutate(riq = n) %>%  
  
  
  ggplot(aes(as.factor(fecha), riq )) + 
  geom_boxplot(varwidth = 0.3) +
  geom_jitter(stat = "identity",  alpha = 0.4, size = 2) +
  labs(title = "",
       x = "Plot elavation (masl)",
       y = "species richness") +
  ylim(2.0,15) +
  theme_bw() +
  labs(x = "Años", y = "Número de especies")



##############################################################
##spp richness changes per plot 
sppAsoc %>%
  group_by(fecha, parcela, cuadrante) %>%
  filter(cuad_selec == 1 & 
           GrowthForm == "understory") %>% 
  tally() %>%
  mutate(riq = n) %>% 
  summarise(riq.prom = mean(riq),
            riq.sd =sd(riq),
            .groups = "drop") %>% 
  
  
  ggplot(aes(as.factor(fecha), riq.prom)) + 
  geom_point(stat = "identity",  alpha = 0.7, size = 3) +
  geom_errorbar(aes(ymin = riq.prom - riq.sd, ymax = riq.prom + riq.sd),
                width = 0.4, position = position_dodge(0.05)) +
  facet_grid(parcela~., scales = "free") +
  labs(title = "",
       x = "Year",
       y = "Mean number of species") +
 
  theme_bw()



##### testing significant changes
library(lme4)

rich.dat= sppAsoc %>%
  group_by(fecha, parcela, cuadrante) %>%
  filter(cuad_selec == 1 & 
           GrowthForm == "understory") %>% 
  tally() %>%
  mutate(riq = n) %>% 
  
  anova(riq ~ fecha * parcela)


 
 rich.model.glm = glm( n ~ fecha  * parcela, 
   data = rich.dat, 
   family = poisson(link = "identity"))

summary(rich.model.glm)

  
# Plot residuals
par(mfrow=c(2,2))
plot(rich.model.glm)

# Check residual deviance
deviance(rich.model.glm) / df.residual(rich.model.glm)


#Use estimated marginal means to compare years within each elevation
library(emmeans)
emmeans(rich.model.glm, pairwise ~ year | elevation)



####################################################################################

sppAsoc %>%
  group_by(fecha, parcela, cuadrante) %>%
  filter(cuad_selec == 1 & 
           GrowthForm == "understory") %>% 
  tally() %>%
  mutate(riq = n)  %>% 
  
  
  ggplot(aes(fecha , riq, color=as.factor(parcela))) + 
  geom_jitter() +
  geom_smooth(se= FALSE) +
  labs(title = "",
       x = "Year",
       y = "Species richness") +
  
  theme_bw()

#ylim(0,15) +



sppAsoc %>% 
  group_by(fecha, parcela, spp) %>%
  filter( spp %in% c("Chusquea coleou", "Drymis andina", "Myrceugenia chrysocarpa")) %>% 
  
  ggplot(aes(parcela, fill=spp,  colour=spp, )) +
  geom_density(alpha = 0.3) +
  theme_bw()



##############################################################
#######     variacion abundancia spp en el gradiente


sppAsoc %>% 
  group_by(fecha, parcela, spp) %>%
  filter( spp %in% c("Chusquea_culeou", "Drimys_andina", "Myrceugenia_chrysocarpa")) %>% 
  filter(cuad_selec == "1") %>% 
  ggplot(aes(fecha, as.numeric(cobertura))) +
  geom_point(alpha=0.2) +
  geom_smooth(se=TRUE) +
  facet_grid(parcela~spp, scales = "free") +
  theme_bw() +
  labs(x = "Años", y = "Cobertura relativa (%)") 

sppAsoc$cobertura = as.numeric(sppAsoc$cobertura)

############ alternativa graf
summary_stat-> sppAsoc %>%
  select(fecha, parcela, cuad_selec, GrowthForm, spp, cobertura) %>% 
  filter(cuad_selec == 1 & 
           GrowthForm == "understory") %>% 
  select(fecha, parcela, spp, cobertura) %>% 
  group_by(fecha, parcela) %>%
  summarise(Mean = mean(cobertura, na.rm = TRUE),
            SD = sd(cobertura, na.rm = TRUE),
            .groups = 'drop')
  

sppAsoc %>% 
  group_by(fecha, parcela, spp) %>%
  filter( spp %in% c("Myrceugenia_chrysocarpa")) %>% # ,   "Drimys_andina""Chusquea_culeou"
  filter(cuad_selec == "1") %>% 
  ggplot(aes(as.factor(fecha), as.numeric(cobertura))) +
  geom_jitter(alpha=0.4, width = 0.2) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="blue", alpha=0.8, size= 0.9) +
  facet_grid(parcela~., scales = "free_y") +
  theme_bw() +
  labs(x = "Años", y = "Cobertura relativa (%)")





sppAsoc %>% 
  group_by(fecha, parcela, spp) %>%
  filter( spp %in% c("Berberis trigona", "Osmorhiza berteroi", "Alstroemeria aurea")) %>% 
  
  ggplot(aes(parcela, cobertura, colour=spp )) +
  geom_point() +
  geom_smooth(se=TRUE) +
  facet_grid(fecha~.) +
  theme_bw()

#curvas no son expresadas por que especies seleccionandas no forman un continuo a lo largo del gradeinte
#sucede lo mismo con cualquier otra especie









##############################################################
#######     analisis composicion  
library(rioja)
library(vegan)
library(tidyverse)

mat1<-sppAsoc %>%
  filter(cuad_selec == "1") %>% 
  filter(GrowthForm == "understory") %>% 
  dplyr::select(fecha, parcela, cuadrante, spp, cobertura) %>%
  slice(-c(284, 283, 281,282)) %>%    #slice repeated measures in the dataset
  spread(key= spp, value = cobertura) 
 

######Ordintation with NMDS

nmds_1<- mat1[,4:34]  #spp
nmds_2<- mat1[,1:3]   #var.expl
nmds_2$cuadrante<- as.factor(nmds_2$cuadrante)


nmds_1<- as.data.frame(sapply(nmds_1, as.numeric)) %>% 
  replace(., is.na(.), 0) 


bp.nmds<- metaMDS(nmds_1, distance= "bray", k=2) 
bp.nmds

#obtain sites scores 
scores<- bp.nmds$points
scores

#obtain species scores 
spp.scores<- bp.nmds$species
spp.scores<- as.data.frame(spp.scores, optional=TRUE)

##combine dataset with spp and dataset with date and elevation
pNMDS <-cbind(scores, nmds_2) 
pNMDS<- as.data.frame(sapply(pNMDS, as.numeric))


#nmds por cada parcela
 ggplot(pNMDS, aes(x=MDS1, y=MDS2, color= as.factor(parcela), fill= as.factor(parcela)))+
  geom_point(size=3) +
  stat_ellipse(geom="polygon", level=0.8, alpha= 0.2)+
   theme_bw()
 
 
#nmds por distintos agnos
 ggplot(pNMDS, aes(x=MDS1, y=MDS2, color= as.factor(fecha), fill= as.factor(fecha)))+
   geom_point(size=3) +
   stat_ellipse(geom="polygon", level=0.95, alpha=0.2)+
   theme_bw()  

 
#################################
 # same nmds as before but adding the names
 ggplot(pNMDS, aes(x=MDS1, y=MDS2, 
                   color= as.factor(parcela), 
                   fill= as.factor(parcela)
        )) +
   geom_point(size=3) +
   stat_ellipse(geom="polygon", level=0.8, alpha= 0.2)+
      geom_point(data= spp.scores,
              aes(MDS1, MDS2),
              inherit.aes = FALSE
              ) +  
   geom_text(data=spp.scores,
             aes(x= MDS1, y=MDS2),
             inherit.aes = FALSE,
             label=row.names(spp.scores)) +
   theme_bw()
 
 ##por agno
 ggplot(pNMDS, aes(x=MDS1, y=MDS2, 
                   color= as.factor(fecha), 
                   fill= as.factor(fecha)
 )) +
   geom_point(size=3) +
   stat_ellipse(geom="polygon", level=0.8, alpha= 0.2)+
   geom_point(data= spp.scores,
              aes(MDS1, MDS2),
              inherit.aes = FALSE
   ) +  
   geom_text(data=spp.scores,
             aes(x= MDS1, y=MDS2),
             inherit.aes = FALSE,
             label=row.names(spp.scores)) +
   theme_bw()
 
#####testing significance 
# interaction
m0<-adonis2(nmds_1~ fecha*parcela, data= nmds_2)
m0
#w.o factor 
adonis2(nmds_1~ fecha+parcela, data= nmds_2)

#both cases

#######################################################
###########Procustes analysis 
#matrix for survey in 2014

dat1<- mat1 %>% 
  filter(fecha =='2014') %>% 
  filter(between(parcela, 930,1200)) %>% #cause only comparable plot resurveyed
  filter(!row_number() %in% c(2, 5)) %>%  #removed files only cause not surveyed in 2024. 
  unite("plot", parcela:cuadrante) %>%
  select(-c(fecha)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot") 

dat1[]<- as.data.frame(sapply(dat1, as.numeric))  %>% 
  replace(., is.na(.), 0) 
dat1

#matrix for survey in 2018
dat2<- mat1 %>% 
  filter(fecha =='2018') %>% 
  filter(between(parcela, 930,1200)) %>%  #cause only comparable plot resurveyed
  filter(!row_number() %in% c(2, 5)) %>%  #removed files only cause not surveyed in 2024. 
  unite("plot", parcela:cuadrante) %>%
  select(-c(fecha)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot") 

dat2[]<- as.data.frame(sapply(dat2, as.numeric)) %>% 
  replace(., is.na(.), 0) 
dat2

#matrix for survey in 2024
dat3<- mat1 %>% 
  filter(fecha =='2024') %>% 
  filter(between(parcela, 930,1200)) %>%  #cause only comparable plot resurveyed
  unite("plot", parcela:cuadrante) %>%
  select(-c(fecha)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot") 

dat3[]<- as.data.frame(sapply(dat3, as.numeric)) %>% 
  replace(., is.na(.), 0) 
dat3


# Perform NMDS for each dataset
nmds1 <- metaMDS(dat1)
nmds2 <- metaMDS(dat2)
nmds3 <- metaMDS(dat3)

# species vectors (centroids)
species_scores1 <- scores(nmds1, display = "species")
species_scores2 <- scores(nmds2, display = "species")
species_scores3 <- scores(nmds3, display = "species")


##################
##################
# Perform Procrustes analysis between dat1 and 2
pro1_2 <- procrustes(nmds1$points, nmds2$points)

# Perform Procrustes analysis between dat1 and 3
pro1_3 <- procrustes(nmds1$points, nmds3$points)

# Perform Procrustes analysis between dat2 and 3
pro2_3 <- procrustes(nmds2$points, nmds3$points)

pro1_2.spp <- procrustes(nmds1$points, nmds2$points)
plot(pro1_2.spp, kind= 1) 
text(pro1_2.spp)
           
########################################################################
#####ploting results #####
par(mfrow=c (1,1))
par(mfrow= c(1,3))

windows()

#############
# Plot the Procrustes results for dataset 1 and 2

plot(pro1_2, main = "2014 vs 2018", kind = 1, cex.lab=0.7, cex.main= 0.7, cex.axis=0.8)
text(pro1_2, display = "target", col="black", pos=4, cex=0.6)


# Plot species for dataset 1 
points(species_scores1, col = "blue", pch = 16)
text(species_scores1, labels = rownames(species_scores1), col = "blue", pos = 3)

# Plot species for dataset 2 
points(species_scores2, col = "red", pch = 17, cex=0.3)
text(species_scores2, labels = rownames(species_scores2),cex=0.6, col = "red", pos = 4)


plot(pro1_2, main = "2014 vs 2018", kind = 2)


#############
# Plot the Procrustes results for dataset 2 and 3
plot(pro2_3, main = "2018 vs 2024", cex.lab=0.7, cex.main= 0.7, cex.axis=0.8)
text(pro2_3, display = "target", col="black", pos=4, cex=0.6)

# Plot species for dataset 2 
points(species_scores2, col = "blue", pch = 16)
text(species_scores2, labels = rownames(species_scores2), col = "blue", pos = 3)

# Plot species for dataset 2 
points(species_scores3, col = "red", pch = 17, cex=0.3)
text(species_scores3, labels = rownames(species_scores3),cex=0.6, col = "red", pos = 4)

#plot residuals
plot(pro2_3, main = "2018 vs 2024", kind = 2)


# Plot the Procrustes results for dataset 1 and 3
plot(pro1_3, main = "2014 vs 2024", cex.lab=0.7, cex.main= 0.7, cex.axis=0.8)
text(pro1_3, display = "target", col="black", pos=4, cex=0.6)

points(species_scores1, col = "darkgray", pch = 16, cex=0.3)
text(species_scores1, labels = rownames(species_scores1), cex = 0.5, col = "darkgray", pos = 3)

# Plot species for dataset 2 
points(species_scores3, col = "red", pch = 17, cex=0.3)
text(species_scores3, labels = rownames(species_scores3),cex=0.6, col = "red", pos = 4)

#plot residuals
plot(pro1_3, main = "2014 vs 2024", kind = 2)




#######################################################################################
# Perform Procrustes permutation test between dataset 1 and 2

protest1_2 <- protest(nmds1$points, nmds2$points)
protest1_2
summary(protest1_2)

# Perform Procrustes permutation test between dataset 1 and 3
protest1_3 <- protest(nmds1$points, nmds3$points)
protest1_3
summary(protest1_3)

# Perform Procrustes permutation test between dataset 2 and 3
protest2_3 <- protest(nmds2$points, nmds3$points)
protest2_3
summary(protest2_3)







################################################################################################
######CCA 
########################
#test to evaluate rows and columns
chisq.test(nmds_1)       #sign difference btw. rows and cols


bp.cca<- cca(sqrt(nmds_1))  #scale by sqrt downweight dominant spp
eigenvals(bp.cca)
#Site scores:  
summary(bp.cca)$sites %>% round(3) 
#Site spp:  
summary(bp.cca)$species %>% round(3) 


screeplot(bp.cca, bstick = TRUE, type = "lines")   #6 significant axes



bp.dist<-vegdist(nmds_1, "bray")  #dissimilarity 0-1, close to 1 they are completely different
bp.clust<-chclust(bp.dist, method="conslink")
plot(bp.clust)
clust<-cutree(bp.clust, 4)
bstick(clust)


plot(bp.cca, scaling = 2)








