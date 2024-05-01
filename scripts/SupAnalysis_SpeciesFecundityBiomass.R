#supplementary analysis - size and fecundity classes, prep data
#load models
fec <- readRDS("output/brms_fecundity_se.rds")
matf_df <- readRDS("output/SERF_biomass_matureF.rds")

#First calculate total MF biomass at each site
#load dataframe with fish biomass (all fish, 
#just with columns that allow filtering mat 
#females / before sampling by sex ratio)

#data
biomAll <- readRDS("output/SERF_biomass_matureF.rds")

#clean
biomAll <- subset(biomAll, select = c(UniqueSite, UniqueTransect, SampArea, Number, genus_sp, biomass_kg_indiv))
biomAll$UniqueSite <- as.factor(biomAll$UniqueSite)
biomAll$UniqueTransect <- as.factor(biomAll$UniqueTransect)
biomAll <- biomAll %>% group_by(UniqueSite, UniqueTransect, SampArea)%>%
    summarise(sumBiomassKG = sum((biomass_kg_indiv * Number)),
              hectareMultip = (10000/SampArea)) 
biomAll <- biomAll%>% 
    group_by(UniqueSite, UniqueTransect, SampArea)%>%
    summarise(sumBiomassKG = sumBiomassKG * hectareMultip)
biomAll <- unique(biomAll) %>% group_by(UniqueSite, UniqueTransect) %>% 
    summarise(sumBiomassKG = sum(sumBiomassKG)) 
biomAll <- biomAll%>% 
    group_by(UniqueSite)%>%
    summarise(MeanAllFishBiomassKgHa = mean((sumBiomassKG)))
biomAll$UniqueSite <- as.factor(biomAll$UniqueSite)

#data
abundAll <- readRDS("output/SERF_biomass_matureF.rds")

#clean
abundAll <- subset(abundAll, select = c(UniqueSite, UniqueTransect, SampArea, Number, genus_sp, biomass_kg_indiv))
abundAll <- abundAll %>% group_by(UniqueSite, UniqueTransect, SampArea)%>%
  summarise(sumAbundance = sum(Number),
            hectareMultip = (10000/SampArea)) 
abundAll <- abundAll%>% 
  group_by(UniqueSite, UniqueTransect, SampArea)%>%
  summarise(sumAbundance = sumAbundance * hectareMultip)
abundAll <- unique(abundAll) %>% group_by(UniqueSite, UniqueTransect) %>% 
  summarise(sumAbundance = sum(sumAbundance)) 
abundAll <- abundAll%>% 
  group_by(UniqueSite)%>%
  summarise(MeanAllFishAbundanceHA= mean((sumAbundance)))


#what fish are driving high fecundity, low biomass sites
#what percentage of the biomass are non-mature females?
# here, just to explore, multiplied the sex ratio by 
#number of individuals rather than sampling 1000 times
dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
dat$UniqueSite <- as.factor(dat$UniqueSite)

#proportion of the biomass mature individuals 
biom_mature <- readRDS("output/SERF_biomass_matureF.rds")
biom_mature <- biom_mature %>% group_by(UniqueSite, UniqueTransect) %>%
  mutate(number_mature = sum(Number*prob_mat),
            total_number = sum(Number),
            proportion_mature = number_mature/total_number)

biom_mature <- biom_mature %>% group_by(UniqueSite, UniqueTransect) %>%
                                   summarise(number_mature = sum(Number*prob_mat),
                                             total_number = sum(Number),
                                             proportion_mature = number_mature/total_number)
                                            
biom_mature <- biom_mature %>% group_by(UniqueSite) %>% summarise(mean_prop_mature = mean(proportion_mature))
biom_mature$UniqueSite <- as.factor(biom_mature$UniqueSite)
biomPropMature<- inner_join(dat, biom_mature) %>%  inner_join(biomAll)
saveRDS(biomPropMature, "output/proportionMature.rds")


#get distribution of fecundity values, seperate into 3 categories (low, med, high)
allMatF <-readRDS("output/sampledFemales_1000.rds")

matureFilter <- function (allMatF){
  mature <- allMatF %>% mutate(mu2 = ifelse(prob_mat > 0, mu,  0),
                               biomass_kg_indiv2 = ifelse(prob_mat>0, biomass_kg_indiv, 0))
  mature <- filter(mature, mu2 > 0)
  return(mature)
}

mature <- lapply(allMatF, matureFilter)

#add back in total length by merging with original df 
mature <- lapply(mature, function(df) inner_join(df, matf_df))


#split sizes into 3 categories - find the range
sizeQuant <- function(mature){
  quants <- as.data.frame(t(quantile(mature$TotalLengthMidSizeCm, probs = c(.3333, .6666, .9999))))
  return(quants)
}

quants <- lapply(mature, sizeQuant)

#merge all lists into 1 df and get median
mergeListMed <- reduce(quants, rbind)
median(mergeListMed$`33.33%`) #17.5
median(mergeListMed$`66.66%`) # 27.5
median(mergeListMed$`99.99%`) # 132.5

##################################################################
##                        Small (<17.5cm)                       ##
##################################################################
## when you filter, you drop a bunch of values and so can 
## end up with 2 very large fish and 
## the mean per site is skewed, instead, take the proportion 
## of each categ in each transect
## when taking the mean, count all the transects

getTransectTotal <- function(mature){
  mature %>% group_by(UniqueSite) %>% mutate(TotalTransectsPerSite =  n_distinct(UniqueTransect))
}

mature <- lapply(mature, getTransectTotal)

#filter function
smallFilt <- function (mature){
  filter(mature, TotalLengthMidSizeCm <=17.5)
}


small <- lapply(mature, smallFilt) 


#get site average
groupT <- function (size){
  size %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
    summarise(sumBiomassKG = sum((biomass_kg_indiv2)),
              sumFecundity = sum(exp(mu2)),
              hectareMultip = (10000/SampArea))
}


convertHA <- function(MatF){
  MatF %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
    summarise(sumBiomassKG = sumBiomassKG * hectareMultip,
              sumFecundity = sumFecundity * hectareMultip)
}



groupS <- function (sizeMatF){
  
  unique(sizeMatF) %>% group_by(UniqueSite, UniqueTransect, TotalTransectsPerSite) %>% 
    summarise(sumBiomassKG = sum(sumBiomassKG),
              sumFecundity = sum(sumFecundity))
  
}

groupS2 <- function (sizeMatF2){
  
 sizeMatF2 <- sizeMatF2 %>% group_by(UniqueSite)%>%
    summarise(meanBiomassKG = sum(sumBiomassKG)/TotalTransectsPerSite,
              meanFecundity = sum(sumFecundity)/TotalTransectsPerSite)
 unique(sizeMatF2)
}



smallMatF <- lapply(small, groupT)
smallMatF <- lapply(smallMatF, convertHA)
smallMatF <- lapply(smallMatF, groupS)
smallMatF <- lapply(smallMatF, groupS2)

naFind <- function (sizeMatF){
filter(sizeMatF, is.na(meanFecundity))
}


#get median fecundity of all df
#create df
smallMatF <- reduce(smallMatF, full_join, by = "UniqueSite")

#add 0 to any columns with NA
smallMatF[is.na(smallMatF)] <- 0


#get median of columns with "biomass"
UniqueSite <- subset(smallMatF, select = c("UniqueSite"))


smallMatFB<- smallMatF %>% 
  dplyr::select(tidyr::contains("biomass"))
smallMatFB$BiomassMedian <- apply(smallMatFB, 1, median)
smallMatFB <-subset(smallMatFB, select = c("BiomassMedian"))
smallMatFB <- cbind(UniqueSite, smallMatFB)

smallMatFF<- smallMatF %>% 
  dplyr::select(tidyr::contains("fecund"))
smallMatFF$FecundityMedian <- apply(smallMatFF, 1, median)
smallMatFF <-subset(smallMatFF, select = c("FecundityMedian"))
smallMatFF <- cbind(UniqueSite, smallMatFF)

smallMatFMed <- full_join(smallMatFF, smallMatFB)
check <- filter(smallMatFMed, is.na(FecundityMedian))

#save rds
saveRDS(smallMatFMed, "output/SMALL_SIZE_site_fecundity_med1000samp.rds")



##################################################################
##                 Medium (17.5-27.5cm)                       ##
##################################################################

#filter function
medFilt <- function (mature){
  filter(mature, TotalLengthMidSizeCm <=27.5 & TotalLengthMidSizeCm>17.5)
} 


med <- lapply(mature, medFilt)
str(as.factor(med$df_4$UniqueSite)) 


medMatF <- lapply(med, groupT)
medMatF <- lapply(medMatF, convertHA)
medMatF <- lapply(medMatF, groupS)
medMatF <- lapply(medMatF, groupS2)



#get median fecundity of all df
#create df
medMatF <- reduce(medMatF, inner_join, by = "UniqueSite")

#add 0 to any columns with NA
medMatF[is.na(medMatF)] <- 0

#get median of columns with "biomass"
UniqueSite <- subset(medMatF, select = c("UniqueSite"))


medMatFB<- medMatF %>% 
  dplyr::select(tidyr::contains("biomass"))
medMatFB$BiomassMedian <- apply(medMatFB, 1, median)
medMatFB <-subset(medMatFB, select = c("BiomassMedian"))
medMatFB <- cbind(UniqueSite, medMatFB)



medMatFF<- medMatF %>% 
  dplyr::select(tidyr::contains("fecund"))
medMatFF$FecundityMedian <- apply(medMatFF, 1, median)
medMatFF <-subset(medMatFF, select = c("FecundityMedian"))
medMatFF <- cbind(UniqueSite, medMatFF)

medMatFMed <- inner_join(medMatFF, medMatFB)
check <- filter(medMatFMed, is.na(FecundityMedian))

#save rds
saveRDS(medMatFMed, "output/MED_SIZE_site_fecundity_med1000samp.rds")


##################################################################
##                          Large (>27.5cm)                     ##
##################################################################

#filter function
largeFilt <- function (mature){
  filter(mature, TotalLengthMidSizeCm >27.5)
} 

large <- lapply(mature, largeFilt)

largeMatF <- lapply(large, groupT)
largeMatF <- lapply(largeMatF, convertHA)
largeMatF <- lapply(largeMatF, groupS)
largeMatF <- lapply(largeMatF, groupS2)


#get median fecundity of all df
#create df
largeMatF <- reduce(largeMatF, inner_join, by = "UniqueSite")

#add 0 to any columns with NA
largeMatF[is.na(largeMatF)] <- 0

#get median of columns with "biomass"
UniqueSite <- subset(largeMatF, select = c("UniqueSite"))


largeMatFB<- largeMatF %>% 
  dplyr::select(tidyr::contains("biomass"))
largeMatFB$BiomassMedian <- apply(largeMatFB, 1, median)
largeMatFB <-subset(largeMatFB, select = c("BiomassMedian"))
largeMatFB <- cbind(UniqueSite, largeMatFB)



largeMatFF<- largeMatF %>% 
  dplyr::select(tidyr::contains("fecund"))
largeMatFF$FecundityMedian <- apply(largeMatFF, 1, median)
largeMatFF <-subset(largeMatFF, select = c("FecundityMedian"))
largeMatFF <- cbind(UniqueSite, largeMatFF)

largeMatFMed <- inner_join(largeMatFF, largeMatFB)
check <- filter(largeMatFMed, is.na(FecundityMedian))

#save rds
saveRDS(largeMatFMed, "output/LARGE_SIZE_site_fecundity_med1000samp.rds")

#############################################################################################################
#calculate relative biomass of each of the categories
dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")

#smallMatFMed <- readRDS("outptut/SMALL_SIZE_site_fecundity_med1000samp.rds")
#medMatFMed <- readRDS("output/MED_SIZE_site_fecundity_med1000samp.rds")
#largeMatFMed <- readRDS("output/LARGE_SIZE_site_fecundity_med1000samp.rds")


#rename df
names(smallMatFMed) <- c("UniqueSite", "SmallFecundity", "SmallBiomass")
smallMatFMed$UniqueSite <- as.factor(smallMatFMed$UniqueSite)

names(medMatFMed) <-  c("UniqueSite", "MedFecundity", "MedBiomass")
medMatFMed$UniqueSite <- as.factor(medMatFMed$UniqueSite)

names(largeMatFMed) <-  c("UniqueSite", "LargeFecundity", "LargeBiomass")
largeMatFMed$UniqueSite <- as.factor(largeMatFMed$UniqueSite)

#create new df
relDat <- data.frame("UniqueSite" = as.factor(dat$UniqueSite))
relDat <- full_join(relDat, smallMatFMed) %>% full_join(medMatFMed) %>% full_join(largeMatFMed)
relDat[is.na(relDat)] <- 0 
dat$UniqueSite <- as.factor(dat$UniqueSite)
relDat <- inner_join(dat, relDat) 
relDat <- inner_join(relDat, biomAll)

#calculate proportion of biomass each of fecundity categories is 
relDat$SmallProp <- (relDat$SmallBiomass/relDat$MeanAllFishBiomassKgHa)
relDat$MedProp <- (relDat$MedBiomass/relDat$MeanAllFishBiomassKgHa)
relDat$LargeProp <- (relDat$LargeBiomass/relDat$MeanAllFishBiomassKgHa)
relDat$totalFecCatProp <- relDat$SmallProp + relDat$MedProp + relDat$LargeProp 



#plot proportions
ggplot(data = relDat) + 
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = SmallProp), color = "red")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = MedProp),  color = "blue")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = LargeProp), color = "dark green")

#save rds
saveRDS(relDat, "output/SizeProportions_site_fecundity_med1000samp.rds")



#####################################################
#################
#########################################################
#what fish are driving high fecundity, low biomass sites
#get distribution of fecundity values, seperate into 3 categories (low, med, high)

matureFilter <- function (allMatF){
  mature <- allMatF %>% mutate(mu2 = ifelse(prob_mat > 0, mu,  0),
                     biomass_kg_indiv2 = ifelse(prob_mat>0, biomass_kg_indiv, 0))
  mature <- filter(mature, mu2 > 0)
  return(mature)
}

mature <- lapply(allMatF, matureFilter)

#split sizes into 3 categories - find the median quantiles of all df
fecQuant <- function(mature){
  mature <- as.data.frame(t(quantile(mature$mu2, probs = c(.3333, .6666, .9999))))
  return(mature)
}

quants <- lapply(mature, fecQuant)

#merge all lists into 1 df and get median
mergeListMed <- unique(reduce(quants, rbind))
median(mergeListMed$`33.33%`) #10.40224
median(mergeListMed$`66.66%`) #11.83902
median(mergeListMed$`99.99%`) #17.66209



##################################################################
##                        Low fec (<=10.402 log eggs)        ##
##################################################################
#when taking the mean, count all the transects

getTransectTotal <- function(mature){
  mature %>% group_by(UniqueSite) %>% mutate(TotalTransectsPerSite =  n_distinct(UniqueTransect))
}

mature <- lapply(mature, getTransectTotal)

#filter function
lowFilt <- function (mature){
  filter(mature, mu2 <=10.40224)
}


low <- lapply(mature, lowFilt)

#get site average
groupT <- function (level){
  level %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
    summarise(sumBiomassKG = sum((biomass_kg_indiv2)),
              sumFecundity = sum(exp(mu2)),
              sumAbund = n(),
              hectareMultip = (10000/SampArea))
} 


convertHA <- function(MatF){
  MatF %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
    summarise(sumBiomassKG = sumBiomassKG * hectareMultip,
              sumFecundity = sumFecundity * hectareMultip,
              sumAbund = sumAbund * hectareMultip)
}



groupS <- function (levelMatF){
  
  unique(levelMatF) %>% group_by(UniqueSite, UniqueTransect, TotalTransectsPerSite) %>% 
    summarise(sumBiomassKG = sum(sumBiomassKG),
              sumFecundity = sum(sumFecundity),
              sumAbund = sum(sumAbund))
  
}

groupS2 <- function (levelMatF2){
  
  levelMatF2 <- levelMatF2 %>% group_by(UniqueSite)%>%
    summarise(meanBiomassKG = sum(sumBiomassKG)/TotalTransectsPerSite,
              meanFecundity = sum(sumFecundity)/TotalTransectsPerSite,
              meanAbundance = sum(sumAbund)/TotalTransectsPerSite)
  unique(levelMatF2)
}



lowMatF <- lapply(low, groupT)
lowMatF <- lapply(lowMatF, convertHA)
lowMatF <- lapply(lowMatF, groupS)
lowMatF <- lapply(lowMatF, groupS2)

naFind <- function (levelMatF){
  filter(levelMatF, is.na(meanFecundity))
}


#create df
lowMatF <- reduce(lowMatF, full_join, by = "UniqueSite")

#add 0 to any columns with NA
lowMatF[is.na(lowMatF)] <- 0


#get median of columns with "biomass"
UniqueSite <- subset(lowMatF, select = c("UniqueSite"))


lowMatFB<- lowMatF %>% 
  dplyr::select(tidyr::contains("biomass"))
lowMatFB$BiomassMedian <- apply(lowMatFB, 1, median)
lowMatFB <-subset(lowMatFB, select = c("BiomassMedian"))
lowMatFB <- cbind(UniqueSite, lowMatFB)



lowMatFF<- lowMatF %>% 
  dplyr::select(tidyr::contains("fecund"))
lowMatFF$FecundityMedian <- apply(lowMatFF, 1, median)
lowMatFF <-subset(lowMatFF, select = c("FecundityMedian"))
lowMatFF <- cbind(UniqueSite, lowMatFF)


lowMatFA<- lowMatF %>% 
  dplyr::select(tidyr::contains("abund"))
lowMatFA$AbundanceMedian <- apply(lowMatFA, 1, median)
lowMatFA <-subset(lowMatFA, select = c("AbundanceMedian"))
lowMatFA <- cbind(UniqueSite, lowMatFA)

lowMatFMed <- full_join(lowMatFF, lowMatFB)
lowMatFMed <- full_join(lowMatFMed, lowMatFA)
check <- filter(lowMatFMed, is.na(FecundityMedian))

#save rds
saveRDS(lowMatFMed, "output/LOW_FECUNDITY_site_fecundity_med1000samp.rds")


##################################################################
##         Med fec (>10.40224 log eggs < 11.83902 eggs)               ##
##################################################################


#filter function
medFilt <- function (mature){
  filter(mature, mu2 <= 11.83902 & mu2 > 10.40224 )
}


med <- lapply(mature, medFilt)

medMatF <- lapply(med, groupT)
medMatF <- lapply(medMatF, convertHA)
medMatF <- lapply(medMatF, groupS)
medMatF <- lapply(medMatF, groupS2)

naFind <- function (levelMatF){
  filter(levelMatF, is.na(meanFecundity))
}


#create df
medMatF <- reduce(medMatF, full_join, by = "UniqueSite")

#add 0 to any columns with NA
medMatF[is.na(medMatF)] <- 0


#get median of columns with "biomass"
UniqueSite <- subset(medMatF, select = c("UniqueSite"))


medMatFB<- medMatF %>% 
  dplyr::select(tidyr::contains("biomass"))
medMatFB$BiomassMedian <- apply(medMatFB, 1, median)
medMatFB <-subset(medMatFB, select = c("BiomassMedian"))
medMatFB <- cbind(UniqueSite, medMatFB)



medMatFF<- medMatF %>% 
  dplyr::select(tidyr::contains("fecund"))
medMatFF$FecundityMedian <- apply(medMatFF, 1, median)
medMatFF <-subset(medMatFF, select = c("FecundityMedian"))
medMatFF <- cbind(UniqueSite, medMatFF)


medMatFA<- medMatF %>% 
  dplyr::select(tidyr::contains("abund"))
medMatFA$AbundanceMedian <- apply(medMatFA, 1, median)
medMatFA <-subset(medMatFA, select = c("AbundanceMedian"))
medMatFA <- cbind(UniqueSite, medMatFA)

medMatFMed <- full_join(medMatFF, medMatFB)
medMatFMed <- full_join(medMatFMed, medMatFA)
check <- filter(medMatFMed, is.na(FecundityMedian))

#save rds
saveRDS(medMatFMed, "output/MED_FECUNDITY_site_fecundity_med1000samp.rds")



##################################################################
##                        High fec (>66.76eggs)               ##
##################################################################

#filter function
highFilt <- function (mature){
  filter(mature, mu2 > 11.83902 )
}


high <- lapply(mature, highFilt)

highMatF <- lapply(high, groupT)
highMatF <- lapply(highMatF, convertHA)
highMatF <- lapply(highMatF, groupS)
highMatF <- lapply(highMatF, groupS2)

naFind <- function (levelMatF){
  filter(levelMatF, is.na(meanFecundity))
}

#create df
highMatF <- reduce(highMatF, full_join, by = "UniqueSite")

#add 0 to any columns with NA
highMatF[is.na(highMatF)] <- 0


#get median of columns with "biomass"
UniqueSite <- subset(highMatF, select = c("UniqueSite"))


highMatFB<- highMatF %>% 
  dplyr::select(tidyr::contains("biomass"))
highMatFB$BiomassMedian <- apply(highMatFB, 1, median)
highMatFB <-subset(highMatFB, select = c("BiomassMedian"))
highMatFB <- cbind(UniqueSite, highMatFB)



highMatFF<- highMatF %>% 
  dplyr::select(tidyr::contains("fecund"))
highMatFF$FecundityMedian <- apply(highMatFF, 1, median)
highMatFF <-subset(highMatFF, select = c("FecundityMedian"))
highMatFF <- cbind(UniqueSite, highMatFF)


highMatFA<- highMatF %>% 
  dplyr::select(tidyr::contains("abund"))
highMatFA$AbundanceMedian <- apply(highMatFA, 1, median)
highMatFA <-subset(highMatFA, select = c("AbundanceMedian"))
highMatFA <- cbind(UniqueSite, highMatFA)

highMatFMed <- full_join(highMatFF, highMatFB)
highMatFMed <- full_join(highMatFMed, highMatFA)
check <- filter(highMatFMed, is.na(FecundityMedian))

#save rds
saveRDS(highMatFMed, "output/HIGH_FECUNDITY_site_fecundity_med1000samp.rds")


#############################################################################################################
#calculate relative abundance, biomass and fecundity of each of the categories
dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
dat$UniqueSite <- as.factor(dat$UniqueSite)

#rename df
names(lowMatFMed) <- c("UniqueSite", "LowCFecundity", "LowCBiomass", "LowCAbundance")
lowMatFMed$UniqueSite <- as.factor(lowMatFMed$UniqueSite)
names(medMatFMed) <-  c("UniqueSite", "MedCFecundity", "MedCBiomass", "MedCAbundance")
medMatFMed$UniqueSite  <- as.factor(medMatFMed$UniqueSite)
names(highMatFMed) <-  c("UniqueSite", "HighCFecundity", "HighCBiomass", "HighCAbundance")
highMatFMed$UniqueSite <- as.factor(highMatFMed$UniqueSite)

#create new df
relFDat <- data.frame("UniqueSite" = dat$UniqueSite)
relFDat <- dplyr::full_join(relFDat, lowMatFMed) %>% full_join(medMatFMed) %>% full_join(highMatFMed)
relFDat[is.na(relFDat)] <- 0 
relFDat <- inner_join(dat, relFDat) 
relFDat <- inner_join(relFDat, biomAll)
abundAll$UniqueSite <- as.factor(abundAll$UniqueSite)
relFDat <- inner_join(relFDat, abundAll)

relFDat$TotalAbundFec <- relFDat$LowCAbundance + relFDat$MedCAbundance + relFDat$HighCAbundance
relFDat$propAbLowFec <- relFDat$LowCAbundance/relFDat$MeanAllFishAbundanceHA
#fix so max is 1
relFDat$propAbLowFec [relFDat$propAbLowFec > 1] <- 1
relFDat$propAbMedFec <- relFDat$MedCAbundance/relFDat$MeanAllFishAbundanceHA
relFDat$propAbHighFec <- relFDat$HighCAbundance/relFDat$MeanAllFishAbundanceHA


relFDat$propBiomLowFec <- relFDat$LowCBiomass/relFDat$MeanAllFishBiomassKgHa
relFDat$propBiomMedFec <- relFDat$MedCBiomass/relFDat$MeanAllFishBiomassKgHa
relFDat$propBiomHighFec <- relFDat$HighCBiomass/relFDat$MeanAllFishBiomassKgHa


relFDat$TotalFec <- relFDat$LowCFecundity+relFDat$MedCFecundity+relFDat$HighCFecundity
relFDat$propFecLowFec <- relFDat$LowCFecundity/relFDat$TotalFec
relFDat$propFecMedFec <- relFDat$MedCFecundity/relFDat$TotalFec
relFDat$propFecHighFec <- relFDat$HighCFecundity/relFDat$TotalFec


ggplot(data = relFDat) + 
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = propFecLowFec), color = "red")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = propFecMedFec),  color = "blue")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = propFecHighFec), color = "dark green")



ggplot(data = relFDat) + 
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = propAbLowFec), color = "red")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = propAbMedFec),  color = "blue")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = propAbHighFec), color = "dark green")


plot(log(relFDat$MeanAllFishBiomassKgHa+1), log(relFDat$TotalAbundFec+1))
plot(log(relFDat$MeanAllFishBiomassKgHa+1), log(relFDat$MeanAllFishAbundanceHA+1))

ggplot(data = relFDat) + 
  geom_smooth(aes(x = TotalAbundFec, y = LowCAbundance), color = "red")+
  geom_smooth(aes(x = TotalAbundFec, y = MedCAbundance),  color = "blue")+
  geom_smooth(aes(x = TotalAbundFec, y = HighCAbundance), color = "dark green")


ggplot(data = relFDat) + 
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = LowCAbundance), color = "red")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = MedCAbundance),  color = "blue")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = HighCAbundance), color = "dark green")


ggplot(data = relFDat) + 
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = LowCBiomass), color = "red")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = MedCBiomass),  color = "blue")+
  geom_smooth(aes(x = log(MeanAllFishBiomassKgHa+1), y = HighCBiomass), color = "dark green")




#save rds
saveRDS(relFDat, "output/FecundityClassProportions_site_fecundity_med1000samp.rds")



###########

#Relative biomass of small, med, large fish (not just mature fish)
dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
biomAll <- readRDS("output/SERF_biomass_matureF.rds")
biomAll2 <- uncount(biomAll, weights = Number)
quantile(biomAll2$TotalLengthMidSizeCm,  probs = c(.3333, .6666, .9999))


biomAll2 <- biomAll2 %>% group_by(UniqueSite) %>% mutate(TotalTransectsPerSite = n_distinct(UniqueTransect))
smallAll <- biomAll2 %>% filter(TotalLengthMidSizeCm <=12.5)
medAll <- biomAll2 %>% filter(TotalLengthMidSizeCm <=22.5 & TotalLengthMidSizeCm > 12.5)
largeAll <- biomAll2 %>% filter(TotalLengthMidSizeCm > 22.5)


smallAll <- subset(smallAll, select = c(UniqueSite, UniqueTransect, SampArea, genus_sp, biomass_kg_indiv, TotalTransectsPerSite))
medAll <- subset(medAll, select = c(UniqueSite, UniqueTransect, SampArea,  genus_sp, biomass_kg_indiv, TotalTransectsPerSite))
largeAll <- subset(largeAll, select = c(UniqueSite, UniqueTransect, SampArea,  genus_sp, biomass_kg_indiv, TotalTransectsPerSite))


smallAll <- smallAll %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
  summarise(sumBiomassKG = sum((biomass_kg_indiv)),
            hectareMultip = (10000/SampArea)) 
smallAll <- smallAll%>% 
  group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
  summarise(sumBiomassKG = sumBiomassKG * hectareMultip)
smallAll <- unique(smallAll) %>% group_by(UniqueSite, UniqueTransect, TotalTransectsPerSite) %>% 
  summarise(sumBiomassKG = sum(sumBiomassKG)) 
smallAll <- smallAll%>% 
  group_by(UniqueSite)%>%
  summarise(SmallMeanAllFishBiomassKgHa = sum(sumBiomassKG)/TotalTransectsPerSite)
smallAll <- unique(smallAll)
#add 0 to any columns with NA
smallAll[is.na(smallAll)] <- 0



medAll <- medAll %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
  summarise(sumBiomassKG = sum((biomass_kg_indiv)),
            hectareMultip = (10000/SampArea)) 
medAll <- medAll%>% 
  group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
  summarise(sumBiomassKG = sumBiomassKG * hectareMultip)
medAll <- unique(medAll) %>% group_by(UniqueSite, UniqueTransect, TotalTransectsPerSite) %>% 
  summarise(sumBiomassKG = sum(sumBiomassKG)) 
medAll <- medAll%>% 
  group_by(UniqueSite)%>%
  summarise(MedMeanAllFishBiomassKgHa = sum(sumBiomassKG)/TotalTransectsPerSite)
medAll <- unique(medAll)
#add 0 to any columns with NA
medAll[is.na(medAll)] <- 0


largeAll <- largeAll %>% group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
  summarise(sumBiomassKG = sum((biomass_kg_indiv)),
            hectareMultip = (10000/SampArea)) 
largeAll <- largeAll%>% 
  group_by(UniqueSite, UniqueTransect, SampArea, TotalTransectsPerSite)%>%
  summarise(sumBiomassKG = sumBiomassKG * hectareMultip)
largeAll <- unique(largeAll) %>% group_by(UniqueSite, UniqueTransect, TotalTransectsPerSite) %>% 
  summarise(sumBiomassKG = sum(sumBiomassKG)) 
largeAll <- largeAll%>% 
  group_by(UniqueSite)%>%
  summarise(LargeMeanAllFishBiomassKgHa = sum(sumBiomassKG)/TotalTransectsPerSite)
largeAll <- unique(largeAll)
#add 0 to any columns with NA
largeAll[is.na(largeAll)] <- 0


biomAll <- readRDS("output/SERF_biomass_matureF.rds")
biomAll <- subset(biomAll, select = c(UniqueSite, UniqueTransect, SampArea, Number, genus_sp, biomass_kg_indiv))

biomAll <- biomAll %>% group_by(UniqueSite, UniqueTransect, SampArea)%>%
  summarise(sumBiomassKG = sum((biomass_kg_indiv * Number)),
            hectareMultip = (10000/SampArea)) 
biomAll <- biomAll%>% 
  group_by(UniqueSite, UniqueTransect, SampArea)%>%
  summarise(sumBiomassKG = sumBiomassKG * hectareMultip)
biomAll <- unique(biomAll) %>% group_by(UniqueSite, UniqueTransect) %>% 
  summarise(sumBiomassKG = sum(sumBiomassKG)) 
biomAll <- biomAll%>% 
  group_by(UniqueSite)%>%
  summarise(MeanAllFishBiomassKgHa = mean((sumBiomassKG)))

relSizeDat <- full_join(smallAll, medAll)
relSizeDat <- full_join(relSizeDat, largeAll)
relSizeDat <- full_join(relSizeDat, biomAll)
relSizeDat <- inner_join(dat, relSizeDat)

relSizeDat$PropSmallBiom <- relSizeDat$SmallMeanAllFishBiomassKgHa/relSizeDat$MeanAllFishBiomassKgHa
relSizeDat$PropMedBiom <- relSizeDat$MedMeanAllFishBiomassKgHa/relSizeDat$MeanAllFishBiomassKgHa
relSizeDat$PropLargeBiom <- relSizeDat$LargeMeanAllFishBiomassKgHa/relSizeDat$MeanAllFishBiomassKgHa


relSizeDat$PropSmallBiom[is.na(relSizeDat$PropSmallBiom)] <- 0
relSizeDat$PropMedBiom[is.na(relSizeDat$PropMedBiom)] <- 0
relSizeDat$PropLargeBiom[is.na(relSizeDat$PropLargeBiom)] <- 0

saveRDS(relSizeDat, "output/SizeProportionsAllFish_site_fecundity_med1000samp.rds")


