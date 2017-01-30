rm(list = ls())
setwd("/Users/mgbrooks208/Dropbox/NHS_gorgas")
library(XML)
library('tidyr')
library('dplyr')
library('plyr')
library(data.table)

#This R file prepares the data for the PCA/Factor analysis to be used. It turns questionaire codes
#into binary variables, and removes the unecessary variables.

men <- read.csv('HOMBRE_FINAL.csv',header = TRUE,  stringsAsFactors=FALSE)
women <-read.csv('N_MUJER.csv', header = TRUE,stringsAsFactors=FALSE)
Women <- tbl_df(women)
Men <- tbl_df(men)
#View(Men)
## House variables used are HH_101 - HH_127
## Male variables used are H_612 - H_613
## Female variables use are M_823 - M_824

## Creating a variable for the number of members in the house
## First we select the names categories and turn them into their own data sets.
M_NOMBRES <- select(Men,NOMBRE_01:NOMBRE_30)
W_NOMBRES <- select(Women,NOMBRE_01:NOMBRE_30)
Sys.setlocale(locale="C")
## I had to do because there were instances of strings that strsplit didn't recognize
## I do not know if this has to do with the data coming from CS Pro. I guess the
## standard locale for R is "C" at least in the US. It solved the error though
## I create a function that detects blanks and turns them into NAs so they aren't dectected as strings
## when counting the number of names.
 isB <-function(x){
  if (all(strsplit(x,'[ +]')[[1]] == "")){
    return(TRUE)}
  else {(return(FALSE))}
}
# Here we change all the blanks into NAs for both the MUJERE and HOMBRE data sets.
W_NOMBRES <- apply(W_NOMBRES, MARGIN = c(1,2), function(x){
  if(is.na(x)){return(x)}
  else if (isB(x)){ return(NA)}
  else(return(x))
}
)

W_NOMBRES <- tbl_df(data.frame(W_NOMBRES))
M_NOMBRES <- apply(M_NOMBRES, MARGIN = c(1,2), function(x){
  if(is.na(x)){return(x)}
  else if (isB(x)){ return(NA)}
  else(return(x))
}
)
# The opperation turned the data into a matrix so we need to put it back into a datarame.
M_NOMBRES <- tbl_df(data.frame(M_NOMBRES))

## This put NAs in the empty input spaces. I will now sum over !is.na() for row
## values because TRUE = 1 and FALSE = 0 so this will give me the number of 
## household members
M_numb_ppl <- apply(M_NOMBRES, MARGIN = 1, function(x){sum(!is.na(x))})
W_numb_ppl <- apply(W_NOMBRES, MARGIN = 1, function(x){sum(!is.na(x))})


# Here were collect the variables that we need.
varM <-select(Men,c(PROV_ID:TENC_ID,HH102:HH127,H612,H613))
varM <-select(varM, -c(HH106,HH106_X_1,HH107A,HH111_A,HH112_A,HH114A,HH115A,HH125))
varM$numb_ppl <- M_numb_ppl
varM$sex <- 1 
varW <-select(Women,c(PROV_ID:TENC_ID,HH102:HH127,M823,M824))
varW <-select(varW, -c(HH106,HH106_X_1,HH107A,HH111_A,HH112_A,HH114A,HH115A,HH125))
varW$numb_ppl <- W_numb_ppl
varW$sex <- 0
# Remove the unecesssary data sets
rm(list = c('M_NOMBRES','W_NOMBRES','men','women'))
# We must ensure that all the variables are title the same before combining the two data sets.
names(varM)[which(names(varM) == 'H612'|names(varM) == 'H613')] <- c("H_owner","L_owner")
names(varW)[which(names(varW) == 'M823'|names(varW) == 'M824')] <- c("H_owner","L_owner")
# For some reason HH116 in the HOMBRE data set was loaded as characters so we need to coerce 
# it to integers to combine the two data sets.
varM_cl <- sapply(1:length(names(varM)),  function(x){ class(varM[[x]])})
varW_cl <- sapply(1:length(names(varM)),  function(x){ class(varW[[x]])})
M_char <- names(varM)[which(varM_cl == "character")]
#[1] "HH102A"    "HH116"     "HH116A"    "HH125_A"   "HH125_B"   "HH125_X_1" "HH127"   
W_char <-names(varW)[which(varW_cl == "character")]
#[1] "HH102A"    "HH116A"    "HH125_B"   "HH125_X_1"
# HH102A, HH116A, HH125_X_1 will be removed because they are actually characters but we must convert
# the others into integers for the anaylsis 
varM$HH127 <- varM$HH127 %>% as.integer()
varM$HH116 <- varM$HH116 %>% as.integer()
varM$HH125_A <- varM$HH125_A %>% as.integer()
varM$HH125_B <- varM$HH125_B %>% as.integer()
varW$HH125_B <- varW$HH125_B %>% as.integer()

# Again here we use do.call to combine the two datasets into one large one.
data <- do.call('rbind',list(varW,varM))
data <- tbl_df(data)
## Here we delete the variables that are only characters and provide no use for the analysis.
del_vars = c('HH102A',"HH116A","HH125_X_1")
data[,del_vars] <- list(NULL)

## Now check for logical inconistencies. The means questions that are dependent on each other but
## have answer that contradict each other.
which((data$HH102 == 11 | data$HH102 == 12 | data$HH102 == 41) & (data$HH104 > 0))
# Some people who were suppose to skip 104 answered it so we set these to 0>
data[which((data$HH102 == 11 | data$HH102 == 12 | data$HH102 == 41) & (data$HH104 > 0)),"HH104"] <- 0
which((data$HH103 == 1 | data$HH102 == 2) & (data$HH104 > 0))

which(data$HH107 == 31 & data$HH109 > 0)
which(data$HH107 == 31 & data$HH108 == 1)
# none
bad_inds <- which(data$HH108 == 2 & data$HH109 > 0)
#data[which(data[bad_inds,"HH109"] == 1),"HH109"] <- 0

bad_inds <- which((data$HH112 == 2 | data$HH112 == 3 | data$HH112 == 6)&(data$HH113 == 1))
data[bad_inds,"HH113"] <- 2

data[which(data$HH119 == 2 & data$HH120 > 0 ),"HH120"] <- 0
start <- which(names(data) == "HH122A")
end <- which(names(data) == "HH122F")
names = names(data)[start:end]
sums <- apply(data[,names],MARGIN = 1, function(x) sum(is.na(x)))             
bad_inds <- which(data$HH121 == 2 & sums < 6)
data[bad_inds,"HH121"] <- 1
## I am giving more validity to then answering with a number the ammount of animals 
## then answering yes or no to question HH12

# Answering 2 in HH121 implies an answer of 0 in 122 for all variables
inds = which(data[,'HH121'] == 2)
data[inds,c("HH122A","HH122B","HH122C","HH122D","HH122E","HH122F")] <- 0


## I take the means of question 120 so I can replace the NO SABE values with the mean.
## I also repalce 950 with 95 becuase 950 includes huge outliers.
data[which(data$HH120 == 950), "HH120"] <- 95
data[which(data$HH120 == 9998), "HH120"] <-998
data[which(data$HH120 == 98), "HH120"] <-998
data[which(data$HH120 >998),"HH120"] <-95
# Using regular expressions to find inputs greater than 998 
verz <- sapply(data$HH120[which(data$HH120 != 998)], function(x) grepl(pattern = "*(99|98)+8", paste(x)))
data$HH120[which(data$HH120 < 998 & data$HH120 > 95)] <- 95
summary(data$HH120[which(!is.na(data$HH120)& !data$HH120 == 998)] )
# There is 186 answer of 95 which is 1.6% of the observations. Most of the responses are 0 so I could fill
# the no sabe response with 0 but we should fill these with the trimmed mean of the response (NAs and 998 excluded)
# since 0 are very common I will only trim the end. It is common to trim 5% in total so I will trim 2.5% fromt he
# higher end. That is length(data$HH120)*.025 
land_means <- tapply(data$HH120[-1*(which(data$HH120 == 998))],data$DIST_ID[-1*(which(data$HH120 == 998))], 
                     function(x) {
                       n = length(x)*.025
                       mean(sort(x,decreasing = TRUE)[-1*c(1:n)],na.rm = TRUE)
                     })
inds = which(data$HH120 == 998)
for (i in inds){
  data[i,"HH120"] <- land_means[paste(data$DIST_ID[i])]
}

## I will do this a similar process for question 104, I use 988 because it came up as an answer I think people misentered 998
inds = which(!(data$HH102 == 11)&!(data$HH102 == 12)&!(data$HH102 == 41)&(data$HH103 == 3)&!(data$HH104 == 998)&!(data$HH104 == 988))
water_means <- tapply(data$HH104[-1*(inds)],data$DIST_ID[-1*(inds)], 
                      function(x) {
                        n = length(x)*.025
                        mean(sort(x,decreasing = TRUE)[-1*c(1:n)],na.rm = TRUE)
                      })
inds = which(data$HH104 == 998 | data$HH104 == 988)
for (i in inds){
  data[i,"HH104"] <- land_means[paste(data$DIST_ID[i])]
}



labels <- c()
for (i in c("A","B","C","D","E","F")){
  labels <- c(labels,paste("HH122",i, sep = ''))
}

for(i in labels){
  if (length(which(data[,i] == 98)) >0){
    data[which(data[,i] == 98),i] <- 0}}
## Here we break variables contain information on types through codes,(ie water soucrce is indicated
## through 11,12,...71) into binary variables per type
## water source


data <- data.table(data)
# Land and Home owner ship
data[,HOME := ((H_owner == 1)|(H_owner == 2)|(H_owner == 3))  %%2]
data[,LAND := ((L_owner == 1)|(L_owner == 2)|(L_owner == 3)) %%2]
#water
data[,h20_P := ((HH102 == 11)|(HH102 == 12)|(HH102 == 13)|(HH102 == 14)) %%2]
data[,h20_well := ((HH102 == 21) |(HH102 == 31)|(HH102 == 32))  %%2]
data[,h20_rain := (HH102 == 41) %%2]
data[,h20_truck := (HH102 == 51) %%2]
data[,h20_surf:= (HH102 == 61) %%2]
data[,h20_bot:= (HH102 == 71) %%2]
data[,h20_other:= (HH102 == 96) %%2]
## water sanitasation
data[,san_septank:= ((HH107 == 11)|(HH107 == 12)) %%2]
data[,san_let:= ((HH107 == 13)|(HH107 == 14)|(HH107 == 15)|(HH107 == 21)|(HH107 == 22)|(HH107 == 23))  %%2]
data[,san_NO_serv:= (HH107 == 31) %%2]
data[,san_other:= (HH107 == 96) %%2]
## Combustible use for cooking 
data[,cook_elec:= (HH111 == 1) %%2]
data[,cook_gas:= (HH111 == 2) %%2]
data[,cook_ker:= (HH111 == 3) %%2]
data[,cook_carbon:= (HH111 == 4) %%2]
data[,cook_nat:= ((HH111 == 5)|(HH111 == 6)|(HH111 == 7)) %%2]
data[,cook_NO:= (HH111 == 95) %%2]
data[,cook_other := (HH111 == 96) %%2]
#floors
data[,floo_nat:= ((HH114 == 11)|(HH114 == 21)|(HH114 == 22)) %%2]
data[,floo_term:= ((HH114 == 31)|(HH114 == 32)|(HH114 == 33)|(HH114 == 34)) %%2]
data[,floo_other:= (HH114 == 96) %%2]
# Ceiling
data[,ceil_nat:= ((HH115 == 11)|(HH115 == 21)|(HH115 == 22)|(HH115 == 23)) %%2]
data[,ceil_term:= ((HH115 == 31)|(HH115 == 32)|(HH115 == 33)|(HH115 == 34))  %%2]
data[,ceil_other:= (HH115 == 96) %%2]
#walls
data[,wall_nat:= ((HH116 == 11)|(HH116 == 12)|(HH116 == 13)) %%2]
data[,wall_rud:= ((HH116 == 21)|(HH116 == 22)|(HH116 == 23)|(HH116 == 24)|(HH116 == 25)|(HH116 == 26)|(HH116 == 27)) %%2]
data[,wall_term:= ((HH116 == 31)|(HH116 == 32)|(HH116 == 33)|(HH116 == 34)|(HH116 == 35)|
                     (HH116 == 36)|(HH116 == 37)|(HH116 == 38)) %%2]
data[,wall_oth:= (HH116 == 96) %%2]
# Cooking Space
data[,K_inH:= (HH112 == 1) %%2]
data[,K_Sep_rm:= (HH112 == 2) %%2]
data[,K_Out:= (HH112 == 3) %%2]
data[,K_oth:= (HH112 == 6) %%2]

## Remove the variables that had the survey codes. I am removing HH124-HH127 because it wasn't indentified as 
## as a variable to be used in any of the analysis (URBAN, RURAL, INDIGENOUS)
del_vars <- c(del_vars,c("HH116","HH115","HH114","HH111","HH107","HH102",
                         "HH111","HH112", "HH103","HH105","HH124","HH125","HH126","HH127"))
for (i in c("HH116","HH115","HH114","HH111","HH107","HH102","HH103","HH105",
    "HH112","H_owner","L_owner")){
  data[, grep(paste("^",i,"$",sep=""), colnames(data)):=NULL]
}
## After speaking with Fermina, we decided to remove the following varialbes.
start <- which(names(data) == "HH106_A")
end <- which(names(data) == "HH106_Z")
names = names(data)[start:end]
for (i in names){
data[, grep(paste("^",i,"$",sep=""), colnames(data)):=NULL]
}
start <- which(names(data) == "HH124")
end <- which(names(data) == "HH127")
names = names(data)[start:end]
for (i in names){
  data[, grep(paste("^",i,"$",sep=""), colnames(data)):=NULL]
}
data[, grep(paste("^HH109$",sep=""), colnames(data)):=NULL]
data[, grep(paste("^HH119$",sep=""), colnames(data)):=NULL]
data[, grep(paste("^HH121$",sep=""), colnames(data)):=NULL]
c1 <- c(1,2,4,7)
c2 <- c(10:19,21)
for ( i in c1){
  data[, grep(paste("^HH110_0",i,"$",sep=""), colnames(data)):=NULL]
}
for ( i in c2){
  data[, grep(paste("^HH110_",i,"$",sep=""), colnames(data)):=NULL]
}
## Make binary variable indicating whether or not a household has more than 4 people per room.
data <- tbl_df(data)
data$ppl_per_rm <- data$numb_ppl/data$HH117
data[which(data$ppl_per_rm ==Inf),"ppl_per_rm"] <- 0
data$ppl_per_rm <- ((data$ppl_per_rm <= 4) %%2)
data[,"HH117"] <- NULL
## We change all NA's in the binary variables to 0s
c1 <- which((1:9 %in% c1) == FALSE)

labels = c("HH108")
for (i in c1){
  labels <- c(labels,paste("HH110_0",i, sep = ""))
}
# Because we are only using asset 110_21 (alacena)
labels <- c(labels,"HH110_20")

labels <- c(labels,"HH113")
for (i in 1:6){
  labels <- c(labels,paste("HH118_0",i, sep = ""))
}

labels <- c(labels,"HH123")
begin <- which(names(data) == "HOME")
labels <- c(labels,names(data)[begin:length(names(data))])
data <- as.data.frame(data)
data[,labels] <- apply(data[,labels],MARGIN =2, function(x) x%%2)
nas <- apply(data[,labels],MARGIN =2, function(x){return(which(is.na(x)))})
for (i in 1:length(nas)){
  data[nas[[i]],names(nas[i])] <-0
}







key <- data %>% select(PROV_ID:TENC_ID)
write.csv(x = key, file = "key.csv")
data <- data %>% select(-1*c(PROV_ID:TENC_ID))
means = list()
for (i in names(data)){
  means[i] <- sum(data[[i]],na.rm = T)/length(data[[i]])
}
na_inds <- list()
for(i in names(data)){
  temp <- which(is.na(data[,i]) == T)
  if (length(temp) >0){
    na_inds[[i]] <- c(temp)
  }
}
for (i in names(na_inds)){
  data[na_inds[[i]],i] <- means[[i]]
}
data_var <- c()
for (i in names(data)){
  data_var[i] <- sum((data[[i]] - means[[i]])^2)/(length(data[[i]])-1)
}

data_sd <- sqrt(data_var)

## Variable with 0 are very small variance are constant and don't describe anything so we 
## will remove them.
names(data)[which(data_sd == 0)]
names(data)[which(data_var == 0)]

# These variables both have 0 variance and SD and therefore do not indicate the spread of the discribution well.
if(length(names(data)[which(data_sd == 0)]) >0){
del_vars <- c(del_vars,names(data)[which(data_var == 0)])
data[,names(data)[which(data_var == 0)]] <- list(NULL)
means<- means[-1*which(data_var == 0)] 
pop_sd <- pop_sd[-1*which(data_var == 0)]
pop_var <-pop_var[-1*which(data_var == 0)]
}
names(data[which(data_sd == max(data_sd))])

write.csv(x = data, file = "Survey_data_cat.csv")

