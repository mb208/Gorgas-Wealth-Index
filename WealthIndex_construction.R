'''
This file performs what is a factor analysis, very similar (but different to PCA). THey are 
both use in the construction of Wealth Indices. It is done as describe by the DHS method.

Factor analysis and PCA takes your data set and spits out variables that are called componenets. These
are linear combinations of the variables in data set and are uncorrelated with each other. Constructing 
a wealth index uses the first component which has the highest variance. The values of this component 
for the weights for the indicator variables. To construct the index we sum these over the observations.
DHS asks to do this for a composite dataset that contains vairables that apply to each area. Then for
speficic regions with their spefic variables. The it asks to use regression the area scores onto the c
composite one. Finally we use the constant and the regressional coefficent to construct the national score.
'''


# Packages Installed
#install.packages('XML')
#install.packages('rtf')
# install.packages('ggplot2')
#install.packages(c("psych","GPArotation","MASS"))
#install.packages("FactoMineR")
rm(list = ls())
setwd("/Users/mgbrooks208/Dropbox/NHS_gorgas")
library(XML)
library('tidyr')
library('dplyr')
library(data.table)
library('psych')
library("ggplot2")
library(xtable)
library(rtf)


data <- read.csv('Survey_data_cat.csv',header = TRUE,  stringsAsFactors=FALSE)
key <- read.csv('key.csv',header = TRUE, stringsAsFactors = FALSE)
sample_weights <- read.csv("/Users/mgbrooks208/Dropbox/NHS_gorgas/FEsexual.csv")
key$X <- NULL
rownames(key)<- NULL
key$sex <- data$sex
data <- tbl_df(data)
rownames(data)<- NULL
data$X <- NULL



# We need to combine the Aread_ID - SEGM_ID so that we can match observations with the correct weights.
# By looking through the key Carlos gave me I see it goes Area, Dist, Corr, Prov, Seg, so we I will multiple these
# numbers by powers of 10 to form the same numbers with in the data set
na_id <- 0 
n= 1
for(i in 1:dim(key)[1]){
  if(any(is.na(key[i,1:9]))){
    na_id[n] <- i 
    n = n+1
  }
}
data <- data[c(-1*na_id),]
key <- key[c(-1*na_id),]
dato_llave <- c(0)

dato_llave <- key[,1]*10^10 + key[,2]*10^8 + key[,3]*10^6 + key[,4]*10^4 + key[,5]
non_matches <- which(!(dato_llave %in% sample_weights$LLAVE_UPM))
# I found 52 non-matches. Since its so few compared to the 11,095 responses 
# Using the corrections Carlos gave me I replace the incorrect inputs.
corrections <- read.csv("52Cases_After_Changes_v1.csv")
labels <- names(corrections)
key[non_matches,labels] <- corrections
dato_llave[non_matches] <- key[non_matches,1]*10^10 + key[non_matches,2]*10^8 + 
                           key[non_matches,3]*10^6 + key[non_matches,4]*10^4 + key[non_matches,5]
dato_llave <- dato_llave[-1*non_matches]
key <- key[-1*non_matches,]
data <- data[-1*non_matches,]

# Here we make modifications to the keys so that they can be matched.
for (i in 1:length(data$sex)){
  if (data$sex[i] == 1){
  data$sex[i] <-  "H"
    }
  else {
    data$sex[i] <- "M"
  }
}
for( x in 1:length(dato_llave)){
  dato_llave[x] <- paste(dato_llave[x],data$sex[x],sep="_")
}
for(x in 1:dim(sample_weights)[1]){
  sample_weights$LLAVE_UPM[x] <- paste(sample_weights$LLAVE_UPM[x],
                                       sample_weights$SEXO_first[x],
                                       sep = "_")
}
weights <- 0 
for (i in 1:length(dato_llave)){
  indice <- which(sample_weights$LLAVE_UPM == dato_llave[i])
  weights[i] <- sample_weights$FE[indice]
}
data[,"sex"] <- NULL
## I am assuming that these weights were developed by dividing the
## population by the sample population so when we scale our wealth scores we will 
## multiply these by the the inverse of the weight (1/weights).



# Here I create a function that creates the quinties of data and uses characters (for example "20-40"),
# to portray them. We use the quantile function (which is portions of 25% of the data, but modify it so
# that each portion is 20% of the data. The cuts assigns factors (character label) based on the quintile.
ApplyQuintiles <- function(x,y) {
  quintiles = quantile(y, probs = seq(0,1,by = 0.20))
  cut(x, breaks = quintiles, 
      labels = c("0-20","20-40","40-60","60-80","80-100"), include.lowest = T)
}

AREA_ID <- key$AREA_ID
PROV_ID <- key$PROV_ID
# This selects the key to attach the quntiles and scores that I sent to Carlos
data <- select(data, -1*c(h20_other, san_other,cook_other,floo_other,ceil_other,wall_oth,K_oth))
data[,"HH118_02"] <- NULL
## If you are familiar with Stata, you know that you can assign labels to variables. We cant do this in R.
## But I create a vector of the labels that we will need for tables later.
var_labels <- c("El tiempo toma recoger agua", "Si se comparte el servicario sanitario",  "Si se tiene una televisión",
                "Si se tiene un teléfono fijo", "Si se tiene un reifrigerador/nevera", "Si se tiene estufa","Si se tienen lavadora",
                "Si se tiene una despensa", "Si se tiene cuarto utilizado sólo para cocinar", "Si se tiene un reloj de mano",
                "Si se tienen una motocicleta", "Si se tiene una carreta de animales", "Si se tiene un carro o camión",
                "Si se tiene bote con motor", "Cuantas hectáres de tierra agrícola se poseen", "Si se tienen ganado de engorde",
                "Si se tienen ganado de leche", "Si se tienen caballos, burros, mulas","Si se tienen cabras",
                "Si se tienen ovejas", "Si tienen aves de corral", "Si se tiene cuenta de bancaría",
                "Si es dueño/a de una casa", "Si es dueño/a de tierra","Si se usa agua potable", "Si se usa un pozo",
                "Si se usan agua lluvia", "Si se usan carro cisterna", "Si se usan agua de superficie", "Si se usan agua embotellada",
                "Si se usan inodoro","Si se usan letrina", " Si sin servicio sanitario","Si se utilizan electridad para cocinar",
                "Si se utilizan gas para cocinar","Si se utilizan kerosene para cocinar","Si se utilizan carbón para cocinar",
                "Si se utilizan cosas naturales para cocinar", "No se cocinan en el hogar", "Si se tienen piso natural",
                "Si tienen piso terminado", "Si tienen techo natural", "Si tienen techo terminado",
                "Si se tienen paredes naturales", "Si se tienen paredes rudimentarias","Si se tienen paredes terminadas",
                "Si la cocina está en casa","Si la cocina está en estructura separada","Si se cocinan al aire libre",
                "Si menos que cuatro personas por cada dormitorio")
# We create data sets for each are.
URB <- data %>% filter(AREA_ID == 1) %>% select(-1*c(HH104,HH118_04,HH118_06,HH120,HH122A:HH122F, numb_ppl))
RUR <- data %>% filter(AREA_ID == 2) %>% select(-1*c(HH118_06,HH123,numb_ppl))
INDG <- data  %>% filter(AREA_ID == 3)  %>% select(-1*c(HH118_04,HH123,numb_ppl))

#Composite Analysis
comp <- data %>% select(-1*c(HH104,HH118_04,HH118_06,HH120,HH122A:HH123, numb_ppl))
comp_means = list()
for (i in names(comp)){
  comp_means[i] <- sum(comp[[i]],na.rm = T)/length(comp[[i]])
}
comp_var <- c()
for (i in names(comp)){
  comp_var[i] <- sum((comp[[i]] - comp_means[[i]])^2)/(length(comp[[i]])-1)
}

comp_sd <- sqrt(comp_var)
comp_df <- tbl_df(data.frame(names = names(comp),Vars = comp_var, SD = comp_sd))
comp_sorted <- comp_df %>% arrange(desc(Vars), desc(SD))
# The dataframe I just created has the variances and SDs of the varialbes in the common variable dataset. I did this to 
# look at the variables with the highest variance and to if any variabels have no variance. In the second case we would need to 
# delete these variables.
inds <- which( names(data)  %in% names(comp))
stats_comp = describe(comp)
desc_comp = data.frame( " " = var_labels[inds],"Mean" = round(stats_comp[,"mean"],2),
                                        "Std Deviation" = round(stats_comp[,"sd"],2))
# fa() performs the factor anaylsis. fa()$loadings[,] extracts the first component (the weights).
comp.fa <- fa(comp,rotate = "varimax")
comp_pesas <- comp.fa$loadings[,]

comp_pc = data.frame(" " = var_labels[inds], "Component" = round(comp_pesas,3))

# The apply function works like a for loop. This sums the operations that are weight by our analysis.
comp$scores <- apply(comp, MARGIN = 1,function(x){ sum(x*comp_pesas)})
comp_adj_scores <- comp$scores/weights
# We create the quintiles
comp$Quintiles <- sapply(comp_adj_scores, function(x) ApplyQuintiles(x,comp_adj_scores))

# We look look at the average ownership of an asset per quintile 
comp_meansbyquintiles <- list()
for (i in names(comp)[1:dim(comp)[2]-1]){
  comp_meansbyquintiles[[i]] <-tapply(comp[[i]],comp$Quintiles, mean)
}

comp_meansbyquintiles["scores"] <- NULL
comp_IN_u <- mean(comp_adj_scores)
comp_IN_sd <- sd(comp_adj_scores)
comp_plot = ggplot(data=comp, aes(x = comp_adj_scores, fill = Quintiles)) +
  geom_histogram(bins = 50, position = "identity",) + 
  labs(title = "Composite Wealth Index", x = "HH Wealth Index Score", y="") +
  annotate("text",x = c(0.7,.7), y = c(200,300),label = c("Std. Dev = ", "Mean = ")) +
  annotate("text",x = c(1,1), y = c(200,300), 
  label = c(paste(round(comp_IN_sd,digits = 3)),paste(round(comp_IN_u,digits = 3)))) +
  theme_classic()
comp_plot                  

# Urban Analysis
URB_means <- apply(URB, MARGIN = 2, function(x){ sum(x)/length(x)})
n = dim(URB)[1]
URB_vars <- sapply(1:length(URB),  function(x){(1/(n))*sum((URB[,x]-URB_means[[x]])^2)})
URB_sd <- sqrt(URB_vars)
UVAR_df <- tbl_df(data.frame(names = names(URB),Vars =URB_vars, SD = URB_sd))
UVAR_sorted <- UVAR_df %>% arrange(desc(Vars), desc(SD))
URB[,names(URB)[which(URB_vars == 0)]] <- NULL
# Which should check again to see if these variables now area specific have zero variance.
URB_del_var <-c()
for (i in names(URB)){
 if(all(URB[[i]] == 0)){
   URB_del_var <- c(URB_del_var,i)
   URB[,i] <- NULL
 }
}
inds <- which( names(data)  %in% names(URB))
stats_URB = describe(URB)
desc_URB = data.frame( " " = var_labels[inds],"Mean" = round(stats_URB[,"mean"],2),
                       "Std. Deviation" = round(stats_URB[,"sd"],2))
URB.fa <- fa(URB,nfactors = 1,rotate = "varimax")
URB_pesas <- URB.fa$loadings[,]

URB_pc = data.frame(" " = var_labels[inds], "Component" = round(URB_pesas,3))

URB$scores <- apply(URB, MARGIN = 1,function(x){ sum(x*URB_pesas)})
urb_adj_scores <- URB$scores*(1/weights[which(AREA_ID == 1)])
URB$Quintiles <- sapply(urb_adj_scores, function(x) ApplyQuintiles(x,urb_adj_scores))
urb_IN_sd <- sd(urb_adj_scores)
urb_IN_u <- mean(urb_adj_scores)
URB_meansbyquintiles <- list()
for (i in names(URB)[1:(dim(URB)[2]-2)]){
  URB_meansbyquintiles[[i]] <-tapply(URB[[i]],URB$Quintiles, mean)
}
sd.text = paste("Std. Dev"," = ",round(urb_IN_sd,digits = 4),sep = "")
mean.text = paste("Mean"," = ",round(urb_IN_u,digits = 4),sep = "")
png('Urb_index.png')
urb_plot = ggplot(data=URB, aes(x = urb_adj_scores, fill = Quintiles)) +
  geom_histogram(bins = 50, position = "identity") + 
  labs(title = "Urban Wealth Index", x = "HH Wealth Index Score", y="") +
  annotate("text",x = c(.15,.15), y = c(125,165),label = c(sd.text, mean.text)) +
  theme_classic()
urb_plot                      
dev.off()
inds <- which(names(data) %in%  names(URB)[1:(dim(URB)[2]-2)])
URB_meansbyquintiles["scores"]<- NULL
k <- length(URB_meansbyquintiles)
URB_asset_avg <- data.frame("Indices" = var_labels[inds], "Lowest" = rep(0,k),"Second" = rep(0,k),
                            "Middle" = rep(0,k), "Fourth" = rep(0,k), "Highest" = rep(0,k),
                            "Average"= rep(0,k),row.names = NULL)
# Creates a dataframe containing information on the average asset ownership per quintile. THis 
# will be turned into a fancy table.
for (i in 1:length(URB_meansbyquintiles)){
  if (URB_meansbyquintiles[[i]][[1]] > 0 & URB_meansbyquintiles[[i]][[1]] < 1){
    URB_asset_avg$Lowest[i] <- round(100*URB_meansbyquintiles[[i]][[1]], digits = 1) 
    URB_asset_avg$Second[i] <- round(100*URB_meansbyquintiles[[i]][[2]] , digits = 1)
    URB_asset_avg$Middle[i] <- round(100*URB_meansbyquintiles[[i]][[3]], digits = 1)
    URB_asset_avg$Fourth[i] <- round(100*URB_meansbyquintiles[[i]][[4]], digits = 1)
    URB_asset_avg$Highest[i] <- round(100*URB_meansbyquintiles[[i]][[5]], digits = 1) 
    URB_asset_avg$Average[i] <- round(sum(URB_asset_avg[i,2],URB_asset_avg[i,3],URB_asset_avg[i,4],
                                          URB_asset_avg[i,5],URB_asset_avg[i,6])/5, digits = 1)
  }
  else {
    URB_asset_avg$Lowest[i] <- round(URB_meansbyquintiles[[i]][[1]], digits = 1) 
    URB_asset_avg$Second[i] <- round(URB_meansbyquintiles[[i]][[2]] , digits = 1)
    URB_asset_avg$Middle[i] <- round(URB_meansbyquintiles[[i]][[3]], digits = 1)
    URB_asset_avg$Fourth[i] <- round(URB_meansbyquintiles[[i]][[4]], digits = 1)
    URB_asset_avg$Highest[i] <- round(URB_meansbyquintiles[[i]][[5]], digits = 1) 
    URB_asset_avg$Average[i] <- round(sum(URB_asset_avg[i,2],URB_asset_avg[i,3],URB_asset_avg[i,4],
                                          URB_asset_avg[i,5],URB_asset_avg[i,6])/5, digits = 1)
  }
}

# Rural analysis
RUR_means <- apply(RUR, MARGIN = 2, function(x){ sum(x)/length(x)})
n = dim(RUR)[1]
RUR_vars <- sapply(1:length(RUR),  function(x){(1/(n))*sum((RUR[,x]-RUR_means[[x]])^2)})
RUR_sd <- sqrt(RUR_vars)
RVAR_df <- tbl_df(data.frame(names = names(RUR),Vars =RUR_vars, SD = RUR_sd))
RVAR_sorted <- UVAR_df %>% arrange(desc(Vars), desc(SD))
RUR_del_var <- c()
which(RUR_vars == 0)
which(RUR_sd == 0)
for (i in names(RUR)){
  if(all(RUR[[i]] == 0)){
    RUR_del_var <- c(RUR_del_var,i)
    RUR[,i] <- NULL
  }
}

inds <- which( names(data)  %in% names(RUR))
stats_RUR = describe(RUR)
desc_RUR = data.frame( " " = var_labels[inds],"Mean" = round(stats_RUR[,"mean"],2),
                       "Std. Deviation" = round(stats_RUR[,"sd"],2))
RUR.fa <- fa(RUR,rotate = "varimax")
RUR_pesas <- RUR.fa$loadings[,]
RUR_pc = data.frame(" " = var_labels[inds], "Component" = round(RUR_pesas,3))

RUR$scores <- apply(RUR, MARGIN = 1,function(x){ sum(x*RUR_pesas)})
rur_adj_scores <- RUR$scores*(1/weights[which(AREA_ID == 2)])
ru_IN_sd <- sd(rur_adj_scores)
ru_IN_u <- mean(rur_adj_scores)
RUR$Quintiles <- sapply(rur_adj_scores, function(x) ApplyQuintiles(x,rur_adj_scores))

RUR_meansbyquintiles <- list()
for (i in names(RUR)[1:dim(RUR)[2]-1]){
  RUR_meansbyquintiles[[i]] <-tapply(RUR[[i]],RUR$Quintiles, mean)
}
sd.text = paste("Std. Dev"," = ",round(ru_IN_sd,digits = 4),sep = "")
mean.text = paste("Mean"," = ",round(ru_IN_u,digits = 4),sep = "")
png('RUR_index.png')
rur_plot = ggplot(data=RUR, aes(x = rur_adj_scores, fill = Quintiles)) +
  geom_histogram(bins = 50, position = "identity") + 
  labs(title = "Rural Wealth Index", x = "HH Wealth Index Score", y="") +
  annotate("text",x = c(-.4,-.4), y = c(125,165),label = c(sd.text, mean.text)) +
  theme_classic()
rur_plot                      
dev.off()
inds <- which(names(data) %in%  names(RUR)[1:(dim(RUR)[2]-2)])
RUR_meansbyquintiles["scores"]<- NULL
k <- length(RUR_meansbyquintiles)
RUR_asset_avg <- data.frame("Indices" = var_labels[inds], "Lowest" = rep(0,k),"Second" = rep(0,k),
                             "Middle" = rep(0,k), "Fourth" = rep(0,k), "Highest" = rep(0,k),
                             "Average"= rep(0,k),row.names = NULL)

for (i in 1:length(RUR_meansbyquintiles)){
  if (RUR_meansbyquintiles[[i]][[1]] > 0 & RUR_meansbyquintiles[[i]][[1]] < 1){
    RUR_asset_avg$Lowest[i] <- round(100*RUR_meansbyquintiles[[i]][[1]], digits = 1) 
    RUR_asset_avg$Second[i] <- round(100*RUR_meansbyquintiles[[i]][[2]] , digits = 1)
    RUR_asset_avg$Middle[i] <- round(100*RUR_meansbyquintiles[[i]][[3]], digits = 1)
    RUR_asset_avg$Fourth[i] <- round(100*RUR_meansbyquintiles[[i]][[4]], digits = 1)
    RUR_asset_avg$Highest[i] <- round(100*RUR_meansbyquintiles[[i]][[5]], digits = 1) 
    RUR_asset_avg$Average[i] <- round(sum(RUR_asset_avg[i,2],RUR_asset_avg[i,3],RUR_asset_avg[i,4],
                                           RUR_asset_avg[i,5],RUR_asset_avg[i,6])/5, digits = 1)
  }
  else {
    RUR_asset_avg$Lowest[i] <- round(RUR_meansbyquintiles[[i]][[1]], digits = 1) 
    RUR_asset_avg$Second[i] <- round(RUR_meansbyquintiles[[i]][[2]] , digits = 1)
    RUR_asset_avg$Middle[i] <- round(RUR_meansbyquintiles[[i]][[3]], digits = 1)
    RUR_asset_avg$Fourth[i] <- round(RUR_meansbyquintiles[[i]][[4]], digits = 1)
    RUR_asset_avg$Highest[i] <- round(RUR_meansbyquintiles[[i]][[5]], digits = 1) 
    RUR_asset_avg$Average[i] <- round(sum(RUR_asset_avg[i,2],RUR_asset_avg[i,3],RUR_asset_avg[i,4],
                                           RUR_asset_avg[i,5],RUR_asset_avg[i,6])/5, digits = 1)
  }
}
#Indigenous Analysis
INDG_means <- apply(INDG, MARGIN = 2, function(x){ sum(x)/length(x)})
n = dim(INDG)[1]
INDG_vars <- sapply(1:length(INDG),  function(x){(1/(n))*sum((INDG[,x]-INDG_means[[x]])^2)})
INDG_sd <- sqrt(INDG_vars)
INDG_df <- tbl_df(data.frame(names = names(INDG),Vars =INDG_vars, SD = INDG_sd))
INVAR_sorted <- INDG_df %>% arrange(desc(Vars), desc(SD))
INDG[names(INDG)[which(INDG_vars == 0)]] <- NULL
INDG_del_var <- c()
for (i in names(INDG)){
  if(all(INDG[[i]] == 0)){
    INDG_del_var <- c(INDG_del_var,i)
    INDG[,i] <- NULL
  }
}
inds <- which( names(data)  %in% names(INDG))
stats_INDG = describe(INDG)
desc_INDG = data.frame( " " = var_labels[inds],"Mean" = round(stats_INDG[,"mean"],2),
                        "Std Deviation" = round(stats_INDG[,"sd"],2))
INDG.fa <- fa(INDG,rotate = "varimax")
INDG_pesas <- INDG.fa$loadings[,]

INDG_pc = data.frame(" " = var_labels[inds], "Component" = round(INDG_pesas,3))

INDG$scores <- apply(INDG, MARGIN = 1,function(x){ sum(x*INDG_pesas)})
INDG_adj_scores <- INDG$scores*(1/weights[which(AREA_ID == 3)])
INDG_IN_sd <- sd(INDG_adj_scores)
INDG_IN_u <- mean(INDG_adj_scores)
INDG$Quintiles <- sapply(INDG_adj_scores, function(x) ApplyQuintiles(x,INDG_adj_scores))
INDG_meansbyquintiles <- list()
for (i in names(INDG)[1:dim(INDG)[2]-1]){
  INDG_meansbyquintiles[[i]] <-tapply(INDG[[i]],INDG$Quintiles, mean)
}
sd.text = paste("Std. Dev"," = ",round(INDG_IN_sd,digits = 4),sep = "")
mean.text = paste("Mean"," = ",round(INDG_IN_u,digits = 4),sep = "")
png('INDG_index.png')
ing_plot = ggplot(data=INDG, aes(x = INDG_adj_scores, fill = Quintiles)) +
  geom_histogram(bins = 45, position = "identity") + 
  labs(title = "Indigenous Wealth Index", x = "HH Wealth Index Score", y="") +
  annotate("text",x = c(.4,.4), y = c(125,165),label = c(sd.text, mean.text)) +
  theme_classic()
ing_plot                    
dev.off()
inds <- which( names(data)  %in% names(INDG)[1:(dim(INDG)[2]-2)])
INDG_meansbyquintiles["scores"]<- NULL
k <- length(INDG_meansbyquintiles)
INDG_asset_avg <- data.frame("Indices" = var_labels[inds], "Lowest" = rep(0,k),"Second" = rep(0,k),
                             "Middle" = rep(0,k), "Fourth" = rep(0,k), "Highest" = rep(0,k),
                             "Average"= rep(0,k),row.names = NULL)

for (i in 1:length(INDG_meansbyquintiles)){
  if (INDG_meansbyquintiles[[i]][[1]] > 0 & INDG_meansbyquintiles[[i]][[1]] < 1){
    INDG_asset_avg$Lowest[i] <- round(100*INDG_meansbyquintiles[[i]][[1]], digits = 1) 
    INDG_asset_avg$Second[i] <- round(100*INDG_meansbyquintiles[[i]][[2]] , digits = 1)
    INDG_asset_avg$Middle[i] <- round(100*INDG_meansbyquintiles[[i]][[3]], digits = 1)
    INDG_asset_avg$Fourth[i] <- round(100*INDG_meansbyquintiles[[i]][[4]], digits = 1)
    INDG_asset_avg$Highest[i] <- round(100*INDG_meansbyquintiles[[i]][[5]], digits = 1) 
    INDG_asset_avg$Average[i] <- round(sum(INDG_asset_avg[i,2],INDG_asset_avg[i,3],INDG_asset_avg[i,4],
                                           INDG_asset_avg[i,5],INDG_asset_avg[i,6])/5, digits = 1)
  }
  else {
    INDG_asset_avg$Lowest[i] <- round(INDG_meansbyquintiles[[i]][[1]], digits = 1) 
    INDG_asset_avg$Second[i] <- round(INDG_meansbyquintiles[[i]][[2]] , digits = 1)
    INDG_asset_avg$Middle[i] <- round(INDG_meansbyquintiles[[i]][[3]], digits = 1)
    INDG_asset_avg$Fourth[i] <- round(INDG_meansbyquintiles[[i]][[4]], digits = 1)
    INDG_asset_avg$Highest[i] <- round(INDG_meansbyquintiles[[i]][[5]], digits = 1) 
    INDG_asset_avg$Average[i] <- round(sum(INDG_asset_avg[i,2],INDG_asset_avg[i,3],INDG_asset_avg[i,4],
                                           INDG_asset_avg[i,5],INDG_asset_avg[i,6])/5, digits = 1)
  }
}


# Here we regress Urban scores onto the composite score
urbscor <- lm(comp$scores[which(AREA_ID == 1)] ~ URB$scores)
summary(urbscor)
Urb_coefs = data.frame("Model" = c("Constant","Urban Wealth"),
                       "Estimates" = c(urbscor[[1]][[1]],urbscor[[1]][[2]]),
                       "Std.Error" = c(0.0277,0.0061),
                       "t" = c(55.38 ,169.76),
                       "Sig." = c(0.001,0.001)
)
# Check significance of the coefficients 
rurscor <- lm(comp$scores[which(AREA_ID == 2)] ~ RUR$scores)
summary(rurscor)
Rur_coefs = data.frame("Model" = c("Constant","Rural Wealth"),
                       "Estimates" = c(rurscor[[1]][[1]],rurscor[[1]][[2]]),
                       "Std.Error" = c(0.0277,0.0061),
                       "t" = c(55.38 ,169.76),
                       "Sig." = c(0.001,0.001)
                       )

ingscor <-lm(comp$scores[which(AREA_ID == 3)] ~ INDG$scores)
summary(ingscor)
Indg_coefs = data.frame("Model" = c("Constant","Indigenous Wealth"),
                       "Estimates" = c(ingscor[[1]][[1]],ingscor[[1]][[2]]),
                       "Std.Error" = c(0.03187,0.0115),
                       "t" = c(-13.45 ,86.70),
                       "Sig." = c(0.001,0.001)
)
combscor <- rep(0,dim(data)[1])
# We then use the constant coefficient and regressional coefficient to get the combined score.
combscor[which(AREA_ID == 1)] <- (urbscor[[1]][[1]] + urbscor[[1]][[2]]*URB$scores) 
combscor[which(AREA_ID == 2)] <- (rurscor[[1]][[1]] + rurscor[[1]][[2]]*RUR$scores) 
combscor[which(AREA_ID == 3)]  <- (ingscor[[1]][[1]] + ingscor[[1]][[2]]*INDG$scores)



data$scores <- combscor/weights
data$Quintiles <- sapply(data$scores, function(x) ApplyQuintiles(x,data$scores))
Assetquintiles <- list()

for (i in names(data)[1:dim(data)[2]-1]){
  Assetquintiles[[i]] <-tapply(data[[i]],data$Quintiles, mean)
}
Assetquintiles["scores"] <- NULL
Assetquintiles["numb_ppl"] <- NULL
combscor <- combscor/weights
IN_u <- mean(data$scores)
IN_sd <- sd(data$scores)

## Using a key Carlos gave me I am attaching information from the wealth index and sending it back to him.
key$quintiles <- data$Quintiles
key$scores <- data$scores
write.csv(x = key, file = "HH_quintiles.csv")

sd.text = paste("Std. Dev"," = ",round(IN_sd,digits = 3),sep = "")
mean.text = paste("Mean"," = ",round(IN_u,digits = 3),sep = "")
png('nacional.png')
p = ggplot(data=data, aes(x = scores, fill = Quintiles)) +
  geom_histogram(bins = 60, position = "identity") + 
  labs(title = "National Wealth Index", x = "HH Wealth Index Score", y="") +
  annotate("text",x = c(.6,.6), y = c(250,350),label = c(sd.text, mean.text)) +
  theme_classic()
p
dev.off()
k <- length(Assetquintiles)
asset_avg <- data.frame("Indices" = var_labels, "Lowest" = rep(0,k),"Second" = rep(0,k), "Middle" = rep(0,k), "Fourth" = rep(0,k), "Highest" = rep(0,k), "Average"= rep(0,k),row.names = NULL)

for (i in 1:length(Assetquintiles)){
  if (Assetquintiles[[i]][[1]] > 0 & Assetquintiles[[i]][[1]] < 1){
  asset_avg$Lowest[i] <- round(100*Assetquintiles[[i]][[1]], digits = 1) 
  asset_avg$Second[i] <- round(100*Assetquintiles[[i]][[2]] , digits = 1)
  asset_avg$Middle[i] <- round(100*Assetquintiles[[i]][[3]], digits = 1)
  asset_avg$Fourth[i] <- round(100*Assetquintiles[[i]][[4]], digits = 1)
  asset_avg$Highest[i] <- round(100*Assetquintiles[[i]][[5]], digits = 1) 
  asset_avg$Average[i] <- round(sum(asset_avg[i,2],asset_avg[i,3],asset_avg[i,4],asset_avg[i,5],asset_avg[i,6])/5, digits = 1)
}
else {
  asset_avg$Lowest[i] <- round(Assetquintiles[[i]][[1]], digits = 1) 
  asset_avg$Second[i] <- round(Assetquintiles[[i]][[2]] , digits = 1)
  asset_avg$Middle[i] <- round(Assetquintiles[[i]][[3]], digits = 1)
  asset_avg$Fourth[i] <- round(Assetquintiles[[i]][[4]], digits = 1)
  asset_avg$Highest[i] <- round(Assetquintiles[[i]][[5]], digits = 1) 
  asset_avg$Average[i] <- round(sum(asset_avg[i,2],asset_avg[i,3],asset_avg[i,4],asset_avg[i,5],asset_avg[i,6])/5, digits = 1)
  }
}

doc <- RTF("Tablas.doc", width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Common")
addTable(doc,desc_comp,c(2.85,.55,.8))
addHeader(doc,"Common Component 1")
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addTable(doc,comp_pc,c(2.85,.8))
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Urban")
addTable(doc,desc_URB,c(2.85,.55,.8),font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Urban Component 1")
addTable(doc,URB_pc,c(2.85,.8),font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Urban Asset Averages")
addTable(doc,URB_asset_avg, col.widths = c(2.85,.57,.57,.57,.57,.57,.57), font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Rural")
addTable(doc,desc_RUR,c(2.85,.55,.8),font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Rural Component 1")
addTable(doc,RUR_pc,c(2.85,.8), font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Rural Asset Averages")
addTable(doc,RUR_asset_avg, col.widths = c(2.85,.57,.57,.57,.57,.57,.57),font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Indigenous")
addTable(doc,desc_INDG,c(2.85,.65,.8), font.size = 8)
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Indigenous Component 1")
addTable(doc,INDG_pc,c(2.85,.8))
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Indigenous Asset Averages")
addTable(doc,INDG_asset_avg, col.widths = c(2.85,.57,.57,.57,.57,.57,.57))
addPageBreak(doc, width=8.5,height=11,font.size = 8, omi = c(1,1,1,1))
addHeader(doc,"Combined Scores")
addTable(doc, Urb_coefs, font.size = 8)
addNewLine(doc, n=1)
text = paste("Combined score", " = ", round(urbscor[[1]][[1]],3), " + " ,
             round(urbscor[[1]][[2]],3), "*Urban score", sep = "")
addText(doc, text)
addNewLine(doc, n=14)
addTable(doc, Rur_coefs, font.size = 8)

addNewLine(doc, n=1)
text = paste("Combined score", " = ", round(rurscor[[1]][[1]],3),
             " + " ,round(rurscor[[1]][[2]],3), "*Rural score", sep = "")
addText(doc, text)
addNewLine(doc, n=14)
addTable(doc, Indg_coefs, font.size = 8)

text = paste("Combined score", " = ", round(ingscor[[1]][[1]],3),
             " + " ,round(ingscor[[1]][[2]],3), "*Indigenous score", sep = "")
addText(doc, text)
addPageBreak(doc, width=8.5,height=11,font.size = 10, omi = c(1,1,1,1))
addHeader(doc,"National Wealth Index")
addPng(doc,"nacional.png",width = 4, height = 2)
done(doc)









