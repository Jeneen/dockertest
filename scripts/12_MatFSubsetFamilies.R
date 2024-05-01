#Load data
allMatF <- readRDS("output/sampledFemales_1000.rds")


##################################################################
##                    All families                               ##
##################################################################

#filter for mature
mature <- lapply(allMatF, function(x) filter(x, prob_mat > 0))

#check unique sites
site_df <- as.data.frame(mature[10])
site_df <- unique(site_df$UniqueSite) #between 1640-1644, dropped if no mat f

#get site average
groupT <- function (mature){
  mature %>% group_by(UniqueSite, UniqueTransect, SampArea)%>%
    dplyr::summarise(sumBiomassKG = sum(biomass_kg_indiv),
                     sumFecundity = sum(exp(mu)),
                     hectareMultip = 10000/SampArea)
}


MatF <- lapply(mature, groupT)


convertHA <- function(MatF){
  MatF %>% group_by(UniqueSite, UniqueTransect, SampArea)%>%
    dplyr::summarise(sumBiomassKG = sumBiomassKG * hectareMultip,
                     sumFecundity = sumFecundity * hectareMultip)
}

MatF <- lapply(MatF, convertHA)



groupS<- function (MatF){
  MatF %>% group_by(UniqueSite)%>%
    dplyr::summarise(meanBiomassKG = mean(sumBiomassKG),
                     sdBiomassKG = sd(sumBiomassKG),
                     meanFecundity = mean(sumFecundity),
                     sdFecundity = sd(sumFecundity))
}

MatF <- lapply(MatF, groupS)


#1000 dataframes, each with unique site level biomass and fecundity
saveRDS(MatF, "output/siteFecundBiomass_1000.rds")




##################################################################
##                          Serranidae                          ##
##################################################################
#filter for mature
mature <- lapply(allMatF, function(x) filter(x, prob_mat > 0))


#filter function
serFilt <- function (mature){
  filter(mature, Family == "Serranidae")
}


ser <- lapply(mature, serFilt)
serMatF <- lapply(ser, groupT)
serMatF <- lapply(serMatF, convertHA)
serMatF <- lapply(serMatF, groupS)


#save rds
saveRDS(serMatF, "output/SERRANIDAE_siteFecundBiomass_1000.rds")



##################################################################
##                          Lutjanidae                          ##
##################################################################

#filter function
lutFilt <- function (mature){
  filter(mature, Family == "Lutjanidae")
}




lut <- lapply(mature, lutFilt)



lutMatF <- lapply(lut, groupT)
lutMatF <- lapply(lutMatF, convertHA)
lutMatF <- lapply(lutMatF, groupS)




#save rds
saveRDS(lutMatF, "output/LUTJANIDAE_siteFecundBiomass_1000.rds")




##################################################################
##                           Scarinae                           ##
##################################################################


#filter function
scarFilt <- function (mature){
  filter(mature, Family == "Scaridae")
}


scar <- lapply(mature, scarFilt)


scarMatF <- lapply(scar, groupT)
scarMatF <- lapply(scarMatF, convertHA)
scarMatF <- lapply(scarMatF, groupS)



#save rds
saveRDS(scarMatF, "output/SCARIDAE_siteFecundBiomass_1000.rds")

