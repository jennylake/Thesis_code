### jenny lake 
### template for frog work
### july 2018 | reed college | kbott

##### install packages if needed
#install.packages("readxl")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("hms")

update.packages("lubridate")
update.packages("dplyr")

##### load libraries [always]
library(readxl)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(hms)

################################### DATA LOADING

##### read in data
# do this for each frog file. example here is 'Frog 1.xlsx'

# note:: to access help documentation, type 'question mark and thing you're curious about'
# example:: ?read_excel
# it's good to get comfy working with help documentation.

##### this command reads an excel file, tells R not to bring the first row as column 
##### names, and tells it to skip the first four rows it is to read
frog_one <- read_excel("Frog 1.xlsx", col_names = FALSE, skip = 4)
frog_two <- read_excel("Frog 2.xlsx", col_names = FALSE, skip = 4)
frog_three <- read_excel("Frog 3.xlsx", col_names = FALSE, skip = 4)
frog_four <- read_excel("Frog 4.xlsx", col_names = FALSE, skip = 4)
frog_five <- read_excel("Frog 5.xlsx", col_names = FALSE, skip = 4)
frog_six <- read_excel("Frog 6.xlsx", col_names = FALSE, skip = 4)
frog_seven <- read_excel("Frog 7.xlsx", col_names = FALSE, skip = 4)

##### take a look
# do this as often as you need! 
#View(frog_one)
#View(frog_two)
#View(frog_three)
#View(frog_four)
#View(frog_five)
#View(frog_six)
#View(frog_seven)

################################### DATA CLEANING

##### remove first column -- one
frog_one <- frog_one %>%
  select(-X__1)

frog_one <- frog_one %>%
    rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)

#### -- two
frog_two <- frog_two %>%
  select(-X__1)

frog_two <- frog_two %>%
  rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)

#### -- three
frog_three <- frog_three %>%
  select(-X__1)

frog_three <- frog_three %>%
  rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)

#### -- four
frog_four <- frog_four %>%
  select(-X__1)

frog_four <- frog_four %>%
  rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)

#### -- five
frog_five <- frog_five %>%
  select(-X__1)

frog_five <- frog_five %>%
  rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)

#### -- six
frog_six <- frog_six %>%
  select(-X__1)

frog_six <- frog_six %>%
  rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)

#### -- seven
frog_seven <- frog_seven %>%
  select(-X__1)

frog_seven <- frog_seven %>%
  rename(video = X__2, times = X__3 , tankSide = X__4, distCm = X__5, timeSec = X__6, input = X__7)


################################### DATA WRANGLING

########### add variable for frog number AND leading playback
frog_one <- frog_one %>%
  mutate(frog = 1, firstPlayback = "laevis")

frog_two <- frog_two %>%
  mutate(frog = 2, firstPlayback = "laevis")

frog_three <- frog_three %>%
  mutate(frog = 3, firstPlayback = "laevis")

frog_four <- frog_four %>%
  mutate(frog = 4, firstPlayback = "petersii")

frog_five <- frog_five %>%
  mutate(frog = 5, firstPlayback = "petersii")

frog_six <- frog_six %>%
  mutate(frog = 6, firstPlayback = "petersii")

frog_seven <- frog_seven %>%
  mutate(frog = 7, firstPlayback = "petersii")

############ combine all files

frog_all <- bind_rows(frog_one, frog_two, frog_three, frog_four, frog_five, frog_six, frog_seven)

##### remove unnecessary text from video
frog_all <- frog_all %>%
  mutate(video = str_replace(video, "Trial ", ""))
  
##### split 'times' into start and finish times
frog_all <- frog_all %>%
  mutate(timesNew = str_pad(times, 16, side="left")) %>%
  mutate(start = str_sub(timesNew,1,8)) %>%
  mutate(end = str_sub(timesNew,9,-1)) %>%
  mutate(end = str_replace(end,"01","1")) %>%
  mutate(start = str_replace(start, ":000", ":00")) %>%
  select(-times, -timesNew)

##### split 'tankSide' into left and right, minus the extra stuff
frog_all <- frog_all %>%
  mutate (tankSide2 = str_replace(tankSide, " Speaker Half", "")) %>%
  mutate (tankSide = str_replace(tankSide2, "In ", "")) %>%
  select(-tankSide2)

#### format start + end as times
## start by removing string from start times
frog_all <- frog_all %>%
  mutate(start = str_replace(start, "Start", "0:00:00"))

## variables started as characters. need them to be numeric/hms
## then set to periods for easier manipulation (later, more experimental design / nesting)

frog_all <- frog_all %>%
 mutate(start = as.hms(start)) %>%
  mutate(end = as.hms(end)) %>%
  mutate(start = as.period(start)) %>%
  mutate(end = as.period(end))

##### define 'rep' by hour. this goes for all frogs. hour 1-4 = rep 1; 4-8 = rep 2; 8-12 = rep 3
frog_all <- frog_all %>%
  mutate(rep = 
           case_when(start >= "0H 0M 0S" & start < "4H 0M 0S" ~ 1,
                     start >= "4H 0M 0S" & start < "8H 0M 0S" ~ 2,
                     start >= "8H 0M 0S" & start < "12H 0M 0S" ~ 3))


frog_all <- frog_all %>%
  mutate(rep = 
           case_when(start >= "0H 0M 0S" & start < "4H 0M 0S" ~ 1,
                     start >= "4H 0M 0S" & start < "8H 0M 0S" ~ 2,
                     start >= "8H 0M 0S" & start < "12H 0M 0S" ~ 3))

#### calculate where you are within "rep" for each cycle (is this minute 15 of this rep? right now we just have overall time.

frog_all <- frog_all %>%
  mutate(repEnd =
           case_when(rep == 1 ~ end,
                     rep == 2 ~ (end - hours(4)),
                     rep == 3 ~ (end - hours(8))))

######## blocks
frog_all <- frog_all %>%
  mutate(block = 
           case_when(repEnd == "0H 15M 0S" ~ 1,
                     repEnd == "0H 30M 0S" ~ 2,
                     repEnd == "0H 45M 0S" ~ 3,
                     repEnd == "1H 00M 0S" ~ 4,
                     repEnd == "1H 15M 0S" ~ 5,
                     repEnd == "1H 30M 0S" ~ 6,
                     repEnd == "1H 45M 0S" ~ 7,
                     repEnd == "2H 00M 0S" ~ 8,
                     repEnd == "2H 15M 0S" ~ 9,
                     repEnd == "2H 30M 0S" ~ 10,
                     repEnd == "2H 45M 0S" ~ 11,
                     repEnd == "3H 00M 0S" ~ 12,
                     repEnd == "3H 15M 0S" ~ 13,
                     repEnd == "3H 30M 0S" ~ 14,
                     repEnd == "3H 45M 0S" ~ 15,
                     repEnd == "4H 00M 0S" ~ 16))

##### rearrange
frog_all <- frog_all %>%
  select(frog, firstPlayback, video, rep, block, input, start, end, repEnd, tankSide, distCm, timeSec)

##### note on PLAYBACK
## added to original excel document / datafile before import
## now need to clean up

frog_all <- frog_all %>%
  mutate(inputSide = 
           case_when(input == "Laevis (L)" ~ "Left",
                     input == "Laevis (R)" ~ "Right",
                     input == "Petersii (L)" ~ "Left",
                     input == "Petersii (R)" ~ "Right",
                     input == "White Noise 1a (L)" ~ "Left",
                     input == "White Noise 1a (R)" ~ "Right",
                     input == "White Noise 1b (L)" ~ "Left",
                     input == "White Noise 1b (R)" ~ "Right", 
                     input == "Silence Laevis (L)" ~ "Left",
                     input == "Silence Laevis (R)" ~ "Right",
                     input == "Silence Petersii (L)" ~ "Left",
                     input == "Silence Petersii (R)" ~ "Right",
                     input == "Silence WN1a (L)" ~ "Left",
                     input == "Silence WN1a (R)" ~ "Right",
                     input == "Silence WN1b (L)" ~ "Left",
                     input == "Silence WN1b (R)" ~ "Right"))

frog_all <- frog_all %>%
  mutate(inputSound = 
           case_when(input == "Laevis (L)" ~ "Laevis",
                     input == "Laevis (R)" ~ "Laevis",
                     input == "Petersii (L)" ~ "Petersii",
                     input == "Petersii (R)" ~ "Petersii",
                     input == "White Noise 1a (L)" ~ "White Noise 1a",
                     input == "White Noise 1a (R)" ~ "White Noise 1a",
                     input == "White Noise 1b (L)" ~ "White Noise 1b",
                     input == "White Noise 1b (R)" ~ "White Noise 1b",
                     input == "Silence Laevis (L)" ~ "Silence Laevis",
                     input == "Silence Laevis (R)" ~ "Silence Laevis",
                     input == "Silence Petersii (L)" ~ "Silence Petersii",
                     input == "Silence Petersii (R)" ~ "Silence Petersii",
                     input == "Silence WN1a (L)" ~ "Silence WN1a",
                     input == "Silence WN1a (R)" ~ "Silence WN1a",
                     input == "Silence WN1b (L)" ~ "Silence WN1b",
                     input == "Silence WN1b (R)" ~ "Silence WN1b"))

frog_all <- frog_all %>%
  mutate(inputSound = replace_na(inputSound, "Silence"))

frog_all <- mutate(frog_all, video = as.numeric(frog_all$video)) 
frog_all <- mutate(frog_all, timeSec = as.numeric(frog_all$timeSec))

frog_all <- frog_all %>%
  select(frog, firstPlayback, video, rep, block, start, end, repEnd, inputSide, inputSound, tankSide, distCm, timeSec)

################################### CALCULATE PREFERENCE FACTOR
# " A preference factor for the average of the distance near the playback speaker over the total distance for both sides of the tank"

# "12 values per playback per frog"
# 4 calls. 16 per session. 3 videos.
# actually NINE CALLS.

frog_all <- frog_all %>%
  group_by(frog, video, rep, block) %>%
  mutate(totalCmBlock = sum(distCm)) %>%
  mutate(totalSecBlock = sum(timeSec))

### write CSVs to file
View(frog_all)
write_csv(frog_all, "/Users/bottk/Desktop/frog_all.csv")

View(frog_prefs)

#### preference DIFFERENCE and preference FACTORS
frog_prefs <- frog_all %>%
  mutate(sideSame = case_when(inputSide == tankSide ~ "true",
                              inputSide != tankSide ~ "false")) %>%
  mutate(prefDiffCmBlock = sum(case_when(sideSame == "true" ~ distCm*1,
                                         sideSame == "false" ~ distCm*(-1)))) %>%
  mutate(prefDiffTimeBlock = sum(case_when(sideSame == "true" ~ timeSec *1,
                                           sideSame == "false" ~ timeSec*(-1)))) %>%
  group_by(frog, video, rep, block) %>%
  mutate(totalCmBlock = sum(distCm)) %>%
  mutate(totalSecBlock = sum(timeSec)) %>%
  group_by(video,rep,inputSound) %>%
  mutate(prefCm = case_when(inputSide == tankSide ~ distCm / totalCmBlock,
                            is.na(inputSide) ~ distCm / totalCmBlock)) %>%
  mutate(prefSec = case_when(inputSide == tankSide ~ timeSec / totalSecBlock,
                             is.na(inputSide) ~ timeSec / totalSecBlock))
## need to drop prefCm
## can't figure this out
## stopping for now ... had this dialed before dammit
## need to drop the obs that don't have prefCm b/c i think that messes w/ the later math.
## problematic pipe is commented out below.

frog_prefs <- frog_all %>%
  mutate(sideSame = case_when(inputSide == tankSide ~ "true",
                              inputSide != tankSide ~ "false")) %>%
  mutate(prefDiffCmBlock = sum(case_when(sideSame == "true" ~ distCm*1,
                                         sideSame == "false" ~ distCm*(-1)))) %>%
  mutate(prefDiffTimeBlock = sum(case_when(sideSame == "true" ~ timeSec *1,
                                           sideSame == "false" ~ timeSec*(-1)))) %>%
  group_by(frog, video, rep, block) %>%
  mutate(totalCmBlock = sum(distCm)) %>%
  mutate(totalSecBlock = sum(timeSec)) %>%
  group_by(video,rep,inputSound) %>%
  mutate(prefCm = case_when(inputSide == tankSide ~ distCm / totalCmBlock,
                            is.na(inputSide) ~ distCm / totalCmBlock)) %>%
  mutate(prefSec = case_when(inputSide == tankSide ~ timeSec / totalSecBlock,
                             is.na(inputSide) ~ timeSec / totalSecBlock)) %>%
  select(-start, -end, -repEnd) %>%
  filter(!is.na(prefCm)) %>%
  select(frog,firstPlayback,video,rep,inputSound,prefCm,prefSec,prefDiffTimeBlock,prefDiffCmBlock) %>%
  group_by(video,rep,inputSound) %>%
  mutate(prefTotalCm = (sum(prefCm)/2))%>%
  mutate(prefTotalSec = (sum(prefSec)/2)) %>%
  mutate(prefDiffCm = sum(prefDiffCmBlock/2)) %>%
  mutate(prefDiffTime = sum(prefDiffTimeBlock/2)) %>%
  distinct(frog,firstPlayback,video,rep,inputSound,prefTotalCm,prefTotalSec,prefDiffCm,prefDiffTime)

### write CSVs to file
View(frog_prefs)
write_csv(frog_prefs, "/Users/jennylake8/Desktop/frog_prefs.csv")