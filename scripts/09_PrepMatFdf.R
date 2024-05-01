#load data
sex <- readRDS("output/extrap_sex_ratio_model.rds")
lm <-  readRDS("output/extrapolated_sp_temp.rds")
site_sst <- readRDS("data/processed/SERF_site_sst.rds")
cinner <- read.csv("data/SERF_SiteData.csv", header = T)
serf <- read.csv("data/SERF_FishSizeData_TL.csv")
ab <- read.csv("data/biomass_ab.csv")


#check missing sites
missing <- filter(cinner, UniqueSite %!in% site_sst$UniqueSite) #no missing sites
missing <- filter(site_sst, meanSST %!in% lm$meanSST)

#add sex ratio and lm to serf site
sex <- subset(sex, select=c(genus_sp, ratio_pred2))
names(sex) <- c("genus_sp", "sex_ratio")
lm <- subset(lm, select = c("genus_sp", "meanSST", "lm_model"))
site_sst <- subset(site_sst, select = c(meanSST, UniqueSite))
str(as.factor(site_sst$UniqueSite))
sex_lm <- inner_join(sex, lm)

# #combine
# sex_lm_site <- merge.data.frame(sex_lm, site_sst)   
sex_lm_site <- inner_join(sex_lm, site_sst)   
sex_lm_site <- unique(sex_lm_site)
str(as.factor(sex_lm_site$UniqueSite)) 

#####################
#check missing data
###################
#filter serf df to only include sites in cinner site df
m_s <-filter(cinner, UniqueSite %!in% serf$UniqueSite)
serf <- filter(serf, UniqueSite %in% cinner$UniqueSite)

#remove column x and genus_sp old
#serf <- serf[,-c(1,33)]

#merge by genus_sp_correct and unique site
serf_all <- merge.data.frame(sex_lm_site, serf, by.x= c("genus_sp", "UniqueSite"), 
                             by.y = c("genus_sp_correct", "UniqueSite"), 
                             all = TRUE)
serf_all <- filter(serf_all, !is.na(UniqueTransect))
str(as.factor(serf_all$UniqueSite)) #1650


#######
#proportion mature (not the final ratio, just to look at possible districution)
serf_all<- serf_all %>% mutate(prob_mat = ifelse(TotalLengthMidSizeCm >= lm_model, 1, 0))
serf_all$prob_matF <- serf_all$prob_mat * serf_all$sex_ratio
serf_dist <- uncount(serf_all, weights =Number)

#multiply proportion by Number
serf_all$NumberMatF <- serf_all$Number * serf_all$prob_matF
#look at distribtion
hist(serf_dist$prob_matF)
#########

########################## calculate biomass ##############################

#clean ab data 
ab$genus_sp <- paste0(ab$Genus, "_", ab$Species)
ab <- subset(ab, select = c(genus_sp, Coeff_a, Coeff_b))

#merge with data
data <- merge.data.frame(serf_all, ab, by = c("genus_sp"), all=TRUE)
data <- filter(data, !is.na(UniqueSite))

#biomass conversions (W = aL^b)
#use totallength mid size cm to calc biomass
data$biomass_kg_matF <- ((data$Coeff_a*data$TotalLengthMidSizeCm^(data$Coeff_b))*data$NumberMatF)/1000
data$biomass_kg_indiv <- ((data$Coeff_a*data$TotalLengthMidSizeCm^(data$Coeff_b)))/1000

#save
saveRDS(data, file = "output/SERF_biomass_matureF.rds")


