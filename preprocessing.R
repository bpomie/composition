# 1 Read in the raw data from individual excel files; 2 Compute the AOI hits; 3 Store as .Rdata for each experiment.

library(readxl)
library(tidyverse)

# TRAINING PHASE ================================================

# Set the path
setwd("~/Documents/GitHub/compositionworking/data/training/")

#Experiment 1
my_files <- list.files(pattern = "^e1t") #Get file names of individual data files
listExcel = lapply(my_files, read_excel) # Read each excel file into a list
de1 = do.call(rbind.data.frame, listExcel) %>% # Concatenate the data in each file into one combined data frame
  mutate(experiment = "Exp1")
rm(listExcel)

#Experiment 2
my_files <- list.files(pattern = "^e2t")
listExcel = lapply(my_files, read_excel)
de2 = do.call(rbind.data.frame, listExcel) %>%
  mutate(experiment = "Exp2")
rm(listExcel)

#Experiment 3
my_files <- list.files(pattern = "^e3t")
listExcel = lapply(my_files, read_excel)
de3 = do.call(rbind.data.frame, listExcel) %>%
  mutate(experiment = "Exp3")
rm(listExcel)

d <- rbind(de1,de2,de3) 
d <- d %>%
  mutate(countID = group_indices_(d, .dots=c("experiment", "ID")))

# Re-code 9999 / 0 to missing data
d[d==9999] <- NA
d[d==0] <- NA

# DEFINE AOIs

# AOIs | labeling
aoileft = c(1,1,500,1080)
aoiright = c(1420,1,1920,1080)
aoicenter= c(900,465,1020,615)
aoilefte3 = c(1,1,500,1200)
aoirighte3 = c(1420,1,1920,1200)
aoicentere3 = c(900,465,1020,615)

# AOIS | word mapping assessment
aoilu = c(375,5,900,520)
aoild = c(375,560,900,1075)
aoiru = c(1020,5,1545,520)
aoird = c(1020,560,1545,1075)
aoilue3 = c(375,5,900,575)
aoilde3 = c(375,625,900,1195)
aoirde3 = c(1020,625,1545,1195)
aoirue3 = c(1020,5,1545,575)

# COMPUTE AOIs

d <- d %>%
  #TRAINING
  mutate(hittrainleft = ifelse(experiment %in% c("Exp1","Exp2"),
                               ifelse(gazex >= aoileft[1] & gazex <= aoileft[3] & gazey >= aoileft[2] & gazey <= aoileft[4],1,0),
                               ifelse(gazex >= aoilefte3[1] & gazex <= aoilefte3[3] & gazey >= aoilefte3[2] & gazey <= aoilefte3[4],1,0))) %>%
  mutate(hittrainright = ifelse(experiment %in% c("Exp1","Exp2"),
                                ifelse(gazex >= aoiright[1] & gazex <= aoiright[3] & gazey >= aoiright[2] & gazey <= aoiright[4],1,0),
                                ifelse(gazex >= aoirighte3[1] & gazex <= aoirighte3[3] & gazey >= aoirighte3[2] & gazey <= aoirighte3[4],1,0))) %>%
  mutate(missingdata = ifelse((is.na(gazex)==1 | is.na(gazey)==1), 1, 0)) %>%
  mutate(locref = ifelse((phase %in% c("handin2","name2","handout2")),
                         ifelse(loclabeled=="l","r","l"),
                         loclabeled)) %>%
  mutate(hittrainreferent = ifelse((locref=="l"&hittrainleft==1|(locref=="r"&hittrainright==1)),1,0)) %>%
  # TEST
  mutate(hittesttg = ifelse(experiment %in% c("Exp1","Exp2"),
                            ifelse((
                              (locationtarget == "Right down"
                               & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
                                (locationtarget == "Right up"
                                 & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
                                (locationtarget == "Left down"
                                 & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
                                (locationtarget == "Left up"
                                 & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])  
                            ),1,0),
                            ifelse((
                              (locationtarget == "Right down"
                               & gazex >= aoirde3[1] & gazex <= aoirde3[3] & gazey >= aoirde3[2] & gazey <= aoirde3[4]) |
                                (locationtarget == "Right up"
                                 & gazex >= aoirue3[1] & gazex <= aoirue3[3] & gazey >= aoirue3[2] & gazey <= aoirue3[4]) |
                                (locationtarget == "Left down"
                                 & gazex >= aoilde3[1] & gazex <= aoilde3[3] & gazey >= aoilde3[2] & gazey <= aoilde3[4]) |
                                (locationtarget == "Left up"
                                 & gazex >= aoilue3[1] & gazex <= aoilue3[3] & gazey >= aoilue3[2] & gazey <= aoilue3[4])  
                            ),1,0))) %>%
  mutate(hittestdt = ifelse(experiment %in% c("Exp1","Exp2"),
                            ifelse((
                              (locationdistractor == "Right down"
                               & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
                                (locationdistractor == "Right up"
                                 & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
                                (locationdistractor == "Left down"
                                 & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
                                (locationdistractor == "Left up"
                                 & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])  
                            ),1,0),
                            ifelse((
                              (locationdistractor == "Right down"
                               & gazex >= aoirde3[1] & gazex <= aoirde3[3] & gazey >= aoirde3[2] & gazey <= aoirde3[4]) |
                                (locationdistractor == "Right up"
                                 & gazex >= aoirue3[1] & gazex <= aoirue3[3] & gazey >= aoirue3[2] & gazey <= aoirue3[4]) |
                                (locationdistractor == "Left down"
                                 & gazex >= aoilde3[1] & gazex <= aoilde3[3] & gazey >= aoilde3[2] & gazey <= aoilde3[4]) |
                                (locationdistractor == "Left up"
                                 & gazex >= aoilue3[1] & gazex <= aoilue3[3] & gazey >= aoilue3[2] & gazey <= aoilue3[4])  
                            ),1,0))) %>%
  mutate(att = ifelse(is.na(gazex) == F | is.na(gazey) == F,1,0)) 

# Compute AOI hits

save(d,file = "../datatraining.RData")

# TEST PHASE ================================================

setwd("~/Documents/GitHub/compositionworking/data/test/")

# EXP1 ====

# Set the path
setwd('~/Documents/GitHub/compositionworking/data/test/')
#Get file names of individual data files
my_files <- list.files(pattern = "^e1_")
# Read each excel file into a list
listExcel = lapply(my_files, read_excel)
# Concatenate the data in each file into one combined data frame
d = do.call(rbind.data.frame, listExcel)
rm(listExcel)

# Recode 9999 / 0 to missing data
d[d==9999] <- NA
d[d==0] <- NA

d <- d %>%
  filter(trial > 6) %>%
  mutate(ID = as.numeric(ID))

# Define AOIs 

aoilu = c(375,5,900,520) 
aoild = c(375,560,900,1075)
aoird = c(1020,560,1545,1075) 
aoiru = c(1020,5,1545,520)
aoicenter= c(900,465,1020,615)


# AOIs 

d <- d %>%
  mutate(tghit = ifelse((
    (locationtarget == "Right down"
     & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
      (locationtarget == "Right up"
       & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
      (locationtarget == "Left down"
       & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
      (locationtarget == "Left up"
       & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])  
  ),1,0)) %>%
  mutate(dthit = ifelse((
    (locationdistractor == "Right down"
     & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
      (locationdistractor == "Right up"
       & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
      (locationdistractor == "Left down"
       & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
      (locationdistractor == "Left up"
       & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
  ),1,0)) %>%
  mutate(centerhit = ifelse(
    (gazex >= aoicenter[1] & gazex <= aoicenter[3] & gazey >= aoicenter[2] & gazey <= aoicenter[4]),
    1,0))

save(d,file = "../datateste1.RData")

# EXP2 ====

# Read in the data

my_files <- list.files(pattern = "^e2_")
listExcel = lapply(my_files, read_excel)
d2 = do.call(rbind.data.frame, listExcel)
rm(listExcel)

exp = 2

# Recode 9999 / 0 to missing data
d2[d2==9999] <- NA
d2[d2==0] <- NA

d2 <- d2 %>%
  filter(trial > 6) %>%
  mutate(ID = as.numeric(ID))

# Define AOIs =====
  aoilu = c(375,5,900,520) #aoilu = c(375,5,900,575)
  aoild = c(375,560,900,1075) #aoild = c(375,625,900,1195)
  aoird = c(1020,560,1545,1075) #aoird = c(1020,625,1545,1195)
  aoiru = c(1020,5,1545,520) #aoiru = c(1020,5,1545,575)
  aoikindl = c(375,200,900,880) #aoikindl = c(375,315,900,885)
  aoikindr = c(1020,200,1545,880)
  aoicenter= c(900,465,1020,615)

  # AOIs Composition
  
  dcompose <- d2 %>%
    filter(trial < 11) %>%
    mutate(tghit = ifelse((
      (locationtarget == "Right down"
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        (locationtarget == "Right up"
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        (locationtarget == "Left down"
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        (locationtarget == "Left up"
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])  
    ),1,0)) %>%
    mutate(dtnumhit = ifelse((
      (locationdistractornumber == "Right down"
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        (locationdistractornumber == "Right up"
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        (locationdistractornumber == "Left down"
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        (locationdistractornumber == "Left up"
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
    ),1,0)) %>%
    mutate(dtkindhit = ifelse((
      (locationdistractorkind == "Right down"
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        (locationdistractorkind == "Right up"
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        (locationdistractorkind == "Left down"
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        (locationdistractorkind == "Left up"
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
    ),1,0)) %>%
    mutate(dtkunrelatedhit = ifelse((
      ((locationdistractorunrelated == "rightdown" | locationdistractorunrelated == "Right down")
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        ((locationdistractorunrelated == "rightup" | locationdistractorunrelated == "Right up")
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        ((locationdistractorunrelated == "leftdown" | locationdistractorunrelated == "Left down")
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        ((locationdistractorunrelated == "leftup" | locationdistractorunrelated == "Left up")
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
    ),1,0)) %>%
    mutate(centerhit = ifelse(
      (gazex >= aoicenter[1] & gazex <= aoicenter[3] & gazey >= aoicenter[2] & gazey <= aoicenter[4]),
      1,0)) %>%
  mutate(att = ifelse(is.na(gazex) == F | is.na(gazey) == F,1,0)) 
  
  # AOIs kind
  
  dkind2 <- d2 %>%
    filter(trial > 10) %>%
    mutate(tghit = ifelse(
      locationtarget == "right" & gazex >= aoikindr[1] & gazex <= aoikindr[3] & gazey >= aoikindr[2] & gazey <= aoikindr[4] |
        locationtarget == "left" & gazex >= aoikindl[1] & gazex <= aoikindl[3] & gazey >= aoikindl[2] & gazey <= aoikindl[4],
      1,0)) %>%
    mutate(dtnumhit = ifelse(
      locationdistractornumber == "right" & gazex >= aoikindr[1] & gazex <= aoikindr[3] & gazey >= aoikindr[2] & gazey <= aoikindr[4] |
        locationdistractornumber == "left" & gazex >= aoikindl[1] & gazex <= aoikindl[3] & gazey >= aoikindl[2] & gazey <= aoikindl[4],
      1,0 )) %>%
    mutate(centerhit = ifelse(
      (gazex >= aoicenter[1] & gazex <= aoicenter[3] & gazey >= aoicenter[2] & gazey <= aoicenter[4]),
      1,0)) %>%
    mutate(dtkindhit = NA) %>%
    mutate(dtkunrelatedhit = NA) %>%
    mutate(att = ifelse(is.na(gazex) == F | is.na(gazey) == F,1,0)) %>%
    mutate(experiment = "Experiment 2")

  save(dcompose,file = "../datateste2.RData")
  #save(dkind,file = "../datakindteste2.RData")

# EXP3 ====

  # Read in the data
  
  my_files <- list.files(pattern = "^e3_")
  listExcel = lapply(my_files, read_excel)
  d3 = do.call(rbind.data.frame, listExcel)
  rm(listExcel)

  
  # Recode 9999 / 0 to missing data
  d3[d3==9999] <- NA
  d3[d3==0] <- NA
  
  d3 <- d3 %>%
    filter(trial > 6) %>%
    mutate(ID = as.numeric(ID))
  
  # Define AOIs 
  aoilu = c(375,5,900,575)
  aoild = c(375,625,900,1195)
  aoird = c(1020,625,1545,1195)
  aoiru = c(1020,5,1545,575)
  aoikindl = c(375,315,900,885)
  aoikindr = c(1020,200,1545,880)
  aoicenter= c(900,525,1020,675)
  
  # AOIs Composition
  
  dcompose <- d3 %>%
    filter(trial < 11) %>%
    mutate(tghit = ifelse((
      (locationtarget == "Right down"
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        (locationtarget == "Right up"
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        (locationtarget == "Left down"
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        (locationtarget == "Left up"
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])  
    ),1,0)) %>%
    mutate(dtnumhit = ifelse((
      (locationdistractornumber == "Right down"
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        (locationdistractornumber == "Right up"
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        (locationdistractornumber == "Left down"
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        (locationdistractornumber == "Left up"
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
    ),1,0)) %>%
    mutate(dtkindhit = ifelse((
      (locationdistractorkind == "Right down"
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        (locationdistractorkind == "Right up"
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        (locationdistractorkind == "Left down"
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        (locationdistractorkind == "Left up"
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
    ),1,0)) %>%
    mutate(dtkunrelatedhit = ifelse((
      ((locationdistractorunrelated == "rightdown" | locationdistractorunrelated == "Right down")
       & gazex >= aoird[1] & gazex <= aoird[3] & gazey >= aoird[2] & gazey <= aoird[4]) |
        ((locationdistractorunrelated == "rightup" | locationdistractorunrelated == "Right up")
         & gazex >= aoiru[1] & gazex <= aoiru[3] & gazey >= aoiru[2] & gazey <= aoiru[4]) |
        ((locationdistractorunrelated == "leftdown" | locationdistractorunrelated == "Left down")
         & gazex >= aoild[1] & gazex <= aoild[3] & gazey >= aoild[2] & gazey <= aoild[4]) |
        ((locationdistractorunrelated == "leftup" | locationdistractorunrelated == "Left up")
         & gazex >= aoilu[1] & gazex <= aoilu[3] & gazey >= aoilu[2] & gazey <= aoilu[4])
    ),1,0)) %>%
    mutate(centerhit = ifelse(
      (gazex >= aoicenter[1] & gazex <= aoicenter[3] & gazey >= aoicenter[2] & gazey <= aoicenter[4]),
      1,0)) %>%
    mutate(att = ifelse(is.na(gazex) == F | is.na(gazey) == F,1,0)) 
  
  # AOIs kind
  
  dkind3 <- d3 %>%
    filter(trial > 10) %>%
    mutate(tghit = ifelse(
      locationtarget == "right" & gazex >= aoikindr[1] & gazex <= aoikindr[3] & gazey >= aoikindr[2] & gazey <= aoikindr[4] |
        locationtarget == "left" & gazex >= aoikindl[1] & gazex <= aoikindl[3] & gazey >= aoikindl[2] & gazey <= aoikindl[4],
      1,0)) %>%
    mutate(dtnumhit = ifelse(
      locationdistractornumber == "right" & gazex >= aoikindr[1] & gazex <= aoikindr[3] & gazey >= aoikindr[2] & gazey <= aoikindr[4] |
        locationdistractornumber == "left" & gazex >= aoikindl[1] & gazex <= aoikindl[3] & gazey >= aoikindl[2] & gazey <= aoikindl[4],
      1,0 )) %>%
    mutate(centerhit = ifelse(
      (gazex >= aoicenter[1] & gazex <= aoicenter[3] & gazey >= aoicenter[2] & gazey <= aoicenter[4]),
      1,0)) %>%
    mutate(dtkindhit = NA) %>%
    mutate(dtkunrelatedhit = NA) %>%
    mutate(att = ifelse(is.na(gazex) == F | is.na(gazey) == F,1,0)) %>%
    mutate(experiment = "Experiment 3")
  
  save(dcompose,file = "../datateste3.RData")

  # KIND RECOGNITON ====
  dkind <- rbind(dkind2,dkind3)
  
  save(dkind, file = "../datakindrecognition.RData")
  