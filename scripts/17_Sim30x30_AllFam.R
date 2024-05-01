############################################# Fecundity potential - ALL FISH ###################################################


#import model and data
fec <- readRDS("output/brms_fecundity_se.rds")
dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")


#Explore data
#what percentage of sites are protected?
sites <- unique(data.frame(UniqueSite = dat$UniqueSite, Larger = dat$Larger, Geographic_Basin = dat$Geographic_Basin,
                             Protection = dat$Protection))
prot <- filter(sites, Protection %in% c("Restricted", "UnfishedHigh"))
percentage_prot <- (nrow(prot)/nrow(sites))*100  #35%




#how many sites per geographic basin?
sites %>% group_by(Geographic_Basin) %>% summarise(count = n())
#410 per group would be even
#Geographic_Basin     count
#<chr>                <int>
#1 Central Indo-Pacific   689
#2 Eastern Indo-Pacific   622
#3 Tropical Atlantic       99
#4 Western Indo-Pacific   223





#Create new data - take reference level category and mean continuous vars
cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress',  'sLarger_pop_size', 
  'sReef_fish_landings_per_km2', "LogFecunditySD"), ~ mean(.x, na.rm = TRUE))

cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])


#select all countries/reef cluster/geographic basin/protection to vary
vary <- subset(dat, select = c(Larger, ReefCluster, Geographic_Basin, Protection) )
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))

#posterior without any changes to protection, just standardised vars
posts_nochange <-pred %>% 
  add_predicted_draws(fec,
                      re_formula = NULL,
                      category='response',
                      allow_new_levels = TRUE,
                      seed = 10,
                      ndraws = 1000)





###################################################################################################################
############ 1. ALL FISHED SCENARIO ###########

#change site protection 
newd_fished <- mutate(pred, Protection = "Fished")
newd_fished <- newd_fished %>%
  mutate(ID  = row_number())

#predict for newdata fished area
posts_fished <- newd_fished %>% add_predicted_draws(fec,
                                               re_formula = NULL,
                                               category='response',
                                               allow_new_levels = TRUE,
                                               seed = 10,
                                               ndraws = 1000)




###################################################################################################################
######## 2. 30% UNFISHED SCENARIO ###########

#select 30% of sites and make them fully protected
# set the seed for reproducibility
set.seed(123)

# create a list to store the 100 dataframes
df_list <- list()

# sample 30% of each group 100 times and change to unfished, all other vars at means
for (i in 1:100) {
  df_temp <- newd_fished %>%
    group_by(Geographic_Basin) %>%
    sample_frac(size = 0.3, replace = FALSE) %>%
    mutate(NewProtection = "UnfishedHigh")
  # add the remaining rows back to the dataframe
  df_temp <- full_join(df_temp%>% distinct(), newd_fished %>% anti_join(df_temp) %>% distinct()) %>% 
    mutate(Protection = if_else(is.na(NewProtection), "Fished", "UnfishedHigh"))
  df_list[[i]] <- df_temp
}


# check to make sure it is a 70/30 split
fished_count <- df_list[[1]] %>% filter(Geographic_Basin == "Central Indo-Pacific") %>%
  filter(Protection == "Fished")%>%
  nrow()/689   ###70%
unfished_count <- df_list[[1]] %>% filter(Geographic_Basin == "Tropical Atlantic") %>%
  filter(Protection == "UnfishedHigh")%>%
  nrow()/99   ###30%
#yes



######### Predict 100 x for new data
# create a list to store the 100 dataframes
master <- list()

#run the predicted draws function for each newdf and save in master
#**TAKES A WHILE TO RUN, may need to increase memory capacity (26.3 GB)
for (i in 1:length(df_list)) {
  posts_30unfished <- df_list[[i]]%>% add_predicted_draws(fec,
                                                          re_formula = NULL,
                                                          category='response',
                                                          allow_new_levels = TRUE,
                                                          seed = 10,
                                                          ndraws = 1000)
  master[[i]] <- posts_30unfished
}



#for each df, group by site, reef cluster, country, and protection and take median of predicted draws
TakeMedianDraws30Unfish <- function (master){
  master %>% group_by(ID, ReefCluster, Larger, Geographic_Basin, Protection)%>%
    summarise(medianPrediction = median(.prediction))
}

medianDraws30Unfish <- lapply(master, TakeMedianDraws30Unfish)


#Combine all df in the list to one df
posts_30Unfished  <- Reduce(rbind, medianDraws30Unfish)

#save
saveRDS(posts_30Unfished, "output/medians_posts_30percentUnfished.rda")

#check output
cent_indpacD <- posts_30Unfished %>% filter(Geographic_Basin == "Central Indo-Pacific")
unfished_count <- cent_indpacD %>% filter(Protection == "UnfishedHigh")
20700/68900
#30%







###################################################################################################################
####### 3. ACCURATE REPRESENTATION OF GEOG BASIN UNFISHED SCENARIO ########

#now sample based on actual percentage protected in each region 
#code for extraction available through Iain Caldwell (publication in review,
#extended data table 1)
#https://bioone.org/journals/bioscience/volume-57/issue-7/B570707/Marine-Ecoregions-of-the-World--A-Bioregionalization-of-Coastal/10.1641/B570707.full
#Central Indo-Pacific = South China Sea + Sunda Shelf + Java Transitional + South Kuroshio + Tropical Northwestern Pacific +
                  #     Western Coral Triangle + Eastern Coral Triangle + Sahul Shelf + Northeast Australian Shelf + 
                  #     Northwest Australian Shelf + Tropical Southwestern Pacific + Lord Howe and Norfolk Islands (NA)
cent_indpac <- (0 + 7.84  + 0.0224  + 0 + 3.95 + 1.42 + 0.257 + 0.0153 + 23.9 + 23.5 + 2.46)/11 #5.76%

#Eastern Indo-Pacific = Hawaii + Marshall, Gilbert, and Ellis Islands + Central Polynesia + Southeast Polynesia + Marquesas
                    #   Easter Island (NA)
east_indpac <- (4.86 + 7.68 + 5.75 + 0 + 0 )/5 #3.658

#Tropical Atlantic = Tropical Northwestern Atlantic, North Brazil Shelf, Tropical Southwestern Atlantic,
                    #  St. Helena and Ascension Islands = NA, West African Transition, Gulf of Guinea
trop_atl <- (0.563 + 0 + 2.46 + 0.00907 + 0)/5 ## 0.606414%


#Western Indo-Pacific = Red Sea and Gulf of Aden + Somali/Arabian + Western Indian Ocean + West and South Indian Shelf +
                        # Central Indian Ocean Islands + Bay of Bengal + Andaman
west_indpac <- (0 + 0 + 0.908 + 0 + 9.77 + 0 + 10.2)/7#2.98%

#Geographic_Basin     count
#<chr>                <int>
#1 Central Indo-Pacific   689
#2 Eastern Indo-Pacific   623
#3 Tropical Atlantic       99
#4 Western Indo-Pacific   223


#Central Indo-Pacific 5.76%
#Eastern Indo-Pacific 3.658
#Tropical Atlantic 0.606%
#Western Indo-Pacific 2.98%




######subset
# set the seed for reproducibility
set.seed(123)

# create a list to store the 100 dataframes
df_cent_indpac <- list()
df_east_indpac <- list()
df_trop_atl <- list()
df_west_indpac <- list()


# sample each group 100x (according to percentage of geog basin actually fully protected) times and change to unfished
for (i in 1:100) {
  df_temp <- newd_fished %>%
    filter(Geographic_Basin == "Central Indo-Pacific") %>%
    sample_frac(size = 0.0576, replace = FALSE) %>%
    mutate(NewProtection = "UnfishedHigh")
  # add the remaining rows back to the dataframe
  df_temp <- full_join(df_temp%>% distinct(), newd_fished %>% 
                         filter(Geographic_Basin == "Central Indo-Pacific") %>%
                         anti_join(df_temp) %>% distinct()) %>% 
    mutate(Protection = if_else(is.na(NewProtection), "Fished", "UnfishedHigh"))
  df_cent_indpac[[i]] <- df_temp
}

#check
a <- df_cent_indpac[[2]]
Tunfished_cent_indpac <- filter(df_cent_indpac[[2]], Protection== "UnfishedHigh")
40/689 *100
#okay

for (i in 1:100) {
  df_temp <- newd_fished %>%
    filter(Geographic_Basin == "Eastern Indo-Pacific") %>%
    sample_frac(size = 0.03658, replace = FALSE) %>%
    mutate(NewProtection = "UnfishedHigh")
  # add the remaining rows back to the dataframe
  df_temp <- full_join(df_temp%>% distinct(), newd_fished %>% 
                         filter(Geographic_Basin == "Eastern Indo-Pacific") %>%
                         anti_join(df_temp) %>% distinct()) %>% 
    mutate(Protection = if_else(is.na(NewProtection), "Fished", "UnfishedHigh"))
  df_east_indpac[[i]] <- df_temp
}

#check
Teast_indpac <- df_east_indpac[[5]] %>% filter(Geographic_Basin == "Eastern Indo-Pacific")
Tunfished_east_indpac <- filter(Teast_indpac, Protection== "UnfishedHigh")
23/622 *100
#okay

for (i in 1:100) {
  df_temp <- newd_fished %>%
    filter(Geographic_Basin == "Tropical Atlantic") %>%
    sample_frac(size = 0.00606, replace = FALSE) %>%
    mutate(NewProtection = "UnfishedHigh")
  # add the remaining rows back to the dataframe
  df_temp <- full_join(df_temp%>% distinct(), newd_fished %>% 
                         filter(Geographic_Basin == "Tropical Atlantic") %>%
                         anti_join(df_temp) %>% distinct()) %>% 
    mutate(Protection = if_else(is.na(NewProtection), "Fished", "UnfishedHigh"))
  df_trop_atl[[i]] <- df_temp
}

#check
Ttrop_atl<- df_trop_atl[[1]] %>% filter(Geographic_Basin == "Tropical Atlantic")
Tunfished_trop_atl <- filter(Ttrop_atl, Protection== "UnfishedHigh")
1/99 *100
#okay

for (i in 1:100) {
  df_temp <- newd_fished %>%
    filter(Geographic_Basin == "Western Indo-Pacific") %>%
    sample_frac(size = 0.0298, replace = FALSE) %>%
    mutate(NewProtection = "UnfishedHigh")
  # add the remaining rows back to the dataframe
  df_temp <- full_join(df_temp%>% distinct(), newd_fished %>% 
                         filter(Geographic_Basin == "Western Indo-Pacific") %>%
                         anti_join(df_temp) %>% distinct()) %>% 
    mutate(Protection = if_else(is.na(NewProtection), "Fished", "UnfishedHigh"))
  df_west_indpac[[i]] <- df_temp
}

Twest_indpac<- df_west_indpac[[1]] %>% filter(Geographic_Basin == "Western Indo-Pacific")
Tunfished_west_indpac <- filter(Twest_indpac, Protection== "UnfishedHigh")








######### Predict for new data (100 dataframes for each geog_basin)
# create a list to store the 100 dataframes of each gb
master_cent_indpac <- list()
master_east_indpac <- list()
master_trop_at <- list()
master_west_indpac <- list()

# List of lists to run pred_forMaster on
lists_to_run <-list(df_cent_indpac,
                    df_east_indpac,
                    df_trop_atl,
                    df_west_indpac)
  
  
lists_to_save <- list(master_cent_indpac, 
                      master_east_indpac, 
                      master_trop_at, 
                      master_west_indpac)

file_names <- paste0("output/",
                     c("master_cent_indpac.rda", 
                       "master_east_indpac.rda", 
                       "master_trop_at.rda", 
                       "master_west_indpac.rda"))

#write the function to take the median predicted draw for each location & protection
#for each datafame (x100) of predictions (obs x 1000), 
TakeMedianDraws <- function(df) {
  df %>% group_by(ID, ReefCluster, Larger, Geographic_Basin, Protection) %>%
    summarise(medianPrediction = median(.prediction))
}


#run the predicted draws function for each newdf and save in master
pred_forMaster <- function(df_list, fec, file_names) {
  # Define empty list to store results
  master <- list()
  
  # Loop through data frames and add predicted draws
  for (i in 1:length(df_list)) {
    posts_unfished <- df_list[[i]] %>% 
      add_predicted_draws(fec, re_formula = NULL, category='response', seed = 10,ndraws = 1000,
                          allow_new_levels = TRUE)
    # Store result in master list
    master[[i]] <- posts_unfished
  }
  
  # Calculate median prediction for each group
  medianDraws <- lapply(master, TakeMedianDraws)
  
  # Define empty list to store results
  median <- list()
  
  # Merge data frames in medianDraws
  median[[i]] <- Reduce(rbind, medianDraws)
  
}



# Loop through the lists and file names and run pred_forMaster,
# save each df
#**TAKES A WHILE TO RUN, may need to increase memory capacity
for (i in 1:length(lists_to_run)) {
  result <- pred_forMaster(lists_to_run[[i]], fec, file_names[i])
  saveRDS(result, file=file_names[i])
}
#this produces a list of df of fecundity predictions for representative percentages of GB protected


#have a look at the individual dataframes
a <- df_cent_indpac[[1]]
b <- df_cent_indpac[[1]]%>% filter(Protection == "UnfishedHigh")
40/689

test <- df_cent_indpac[[1]]%>% 
  add_predicted_draws(fec, re_formula = NULL, category='response',seed = 10, ndraws = 1000)
mTest <- TakeMedianDraws(test)

test2 <- df_cent_indpac[[2]]%>% 
  add_predicted_draws(fec, re_formula = NULL, category='response', seed = 10,ndraws = 1000)

test3 <- rbind(test, test2)
test4 <- test3 %>% group_by(Larger, ReefCluster)

mTest2 <- TakeMedianDraws(test2)
list_m <- list(mTest, mTest2)
mRed <- Reduce(full_join, list_m)
##







#rather than taking median for each new df set of predictions,
#combine prediction dataframes

#load the dfs into environment
master_cent_indpac <- readRDS("output/master_cent_indpac.rda")
master_east_indpac <- readRDS("output/master_east_indpac.rda")
master_trop_at <- readRDS("output/master_trop_at.rda")
master_west_indpac <- readRDS("output/master_west_indpac.rda")

posts_30Unfished <- readRDS("output/medians_posts_30percentUnfished.rda")



#Central Indo-Pacific 5.76%
#Eastern Indo-Pacific 3.658
#Tropical Atlantic 0.606%
#Western Indo-Pacific 2.98%


#check they are the right percentages
master_cent_indpac  %>% group_by(Protection) %>% summarise(count = n()) #5.8
4000/(64900+4000)
master_east_indpac  %>% group_by(Protection) %>% summarise(count = n()) #3.7
2300/(59900+2300)
master_trop_at  %>% group_by(Protection) %>% summarise(count = n())#1
master_west_indpac   %>% group_by(Protection) %>% summarise(count = n()) #3.1
# yes


#merge into 1 df
posts_gb <- rbind(master_cent_indpac, master_east_indpac, master_trop_at, master_west_indpac)
#posts_gb <- unique(posts_gb)

###################################################################################################################
#get the percent differences between the median fecundity of each and write a summary df

posts_gb1 <- subset(posts_gb, select = c("ID", "Larger", "ReefCluster", "Geographic_Basin", "medianPrediction"))
names(posts_gb1) <- c("ID","Larger","ReefCluster", "Geographic_Basin", "postsProtectionByGB")
posts_gb1 <- as.data.frame(posts_gb1)
summary <- posts_gb1 %>% group_by(Geographic_Basin) %>% median_qi(postsProtectionByGB)

posts_30Unfished1 <- subset(posts_30Unfished, select = c("ID","Larger", "ReefCluster", "Geographic_Basin", "medianPrediction"))
names(posts_30Unfished1) <- c("ID","Larger","ReefCluster", "Geographic_Basin", "posts30Unfished")
posts_30Unfished1 <- as.data.frame(posts_30Unfished1)
summary2 <- posts_30Unfished1 %>% group_by(Geographic_Basin) %>% summarise(median_posts30Unfished = median(posts30Unfished))
summary2 <- posts_30Unfished1 %>% group_by(Geographic_Basin) %>% median_qi(posts30Unfished)

#exponentiate 
posts_30Unfished1$posts30Unfished <- exp(posts_30Unfished1$posts30Unfished)-1
posts_gb1$postsProtectionByGB <- exp(posts_gb1$postsProtectionByGB)-1


#combine by order
percent <-median_qi(((posts_30Unfished1$posts30Unfished - posts_gb1$postsProtectionByGB)/
                       posts_gb1$postsProtectionByGB)*100)
posts_gb1 <- posts_gb1 %>% group_by(ID, Larger, ReefCluster) %>% arrange(postsProtectionByGB, .by_group = TRUE)
posts_30Unfished1 <- posts_30Unfished1%>% group_by(ID, Larger, ReefCluster) %>% arrange(posts30Unfished, .by_group = TRUE)


posts_30GB <- data.frame(ID=posts_gb1$ID, Larger=posts_gb1$Larger, ReefCluster=posts_gb1$ReefCluster, 
                         Geographic_Basin=posts_gb1$Geographic_Basin, postsGB = posts_gb1$postsProtectionByGB,
                         posts30 = posts_30Unfished1$posts30Unfished)
posts_30GB$ratio <- (posts_30GB$posts30)/(posts_30GB$postsGB)
posts_30GB$percent_diff <- ((posts_30GB$posts30 - posts_30GB$postsGB) / posts_30GB$postsGB) *100
median(na.omit(posts_30GB$percent_diff ))
summary_perc_gain <-na.omit(posts_30GB) %>% group_by(Geographic_Basin) %>%
  summarise(percent_gain = ((median(posts30) - median(postsGB)) / median(postsGB)) *100,
            median_posts30 = median(posts30),
            median_postsGB = median(postsGB),
            median_posts30_log = log(median_posts30+1),
            median_postsGB_log = log(median_postsGB+1),
            difference= (median(posts30) - median(postsGB)),
            difference_log = log(difference + 1))
posts_30GB$all <- "all"
summary_perc_gain2 <-na.omit(posts_30GB) %>% group_by(all) %>%
  summarise(percent_gain = ((median(posts30) - median(postsGB)) / median(postsGB)) *100,
            median_posts30 = median(posts30),
            median_postsGB = median(postsGB),
            median_posts30_log = log(median_posts30+1),
            median_postsGB_log = log(median_postsGB+1),
            difference= (median(posts30) - median(postsGB)),
            difference_log = log(difference + 1)) %>% rename(Geographic_Basin = all)
summary_perc_gain <- rbind(summary_perc_gain, summary_perc_gain2)

#add order to df
posts_30GB<- posts_30GB %>% mutate(orderFig = case_when(
  Geographic_Basin == "Tropical Atlantic" ~ 1,
  Geographic_Basin == "Western Indo-Pacific" ~ 2,
  Geographic_Basin == "Eastern Indo-Pacific" ~ 3,
  Geographic_Basin == "Central Indo-Pacific" ~ 4))

#add percentages
posts_30GB<- posts_30GB %>% mutate(label = case_when(
  Geographic_Basin == "Tropical Atlantic" ~ "Tropical Atlantic \n0.77% to 30% protection",
  Geographic_Basin == "Western Indo-Pacific" ~ "Western Indo-Pacific \n2.98% to 30% protection",
  Geographic_Basin == "Eastern Indo-Pacific" ~ "Eastern Indo-Pacific \n3.66% to 30% protection",
  Geographic_Basin == "Central Indo-Pacific" ~ "Central Indo-Pacific \n5.76% to 30% protection"))


posts_30GB_mean <- posts_30GB %>% group_by(Geographic_Basin, Larger, ReefCluster, ID, label, orderFig) %>% 
  summarise(meanGB = mean(postsGB),
            mean30= mean(posts30),
            percent_diff = (((mean30 - meanGB)/meanGB)*100))

posts_30GB_mean$percent_diff


#save
saveRDS(posts_30GB_mean,  "output/fecundity_potential_mean_GB_allfish.rds")


