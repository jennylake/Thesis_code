### jenny lake
### thesis, 2018
### code w k.bott | instructional tech svcs | reed college

### source files should be in shared google drive folder
### code for source files, also in shared folder

### install packages if necessary
install.packages("tidyverse")
install.packages("ggsignif")
install.packages("ggpubr")

### load libraries (always)
library(tidyverse)
library(ggsignif)
library(ggpubr)


### read in data; change filepath as needed
frog_prefs <- read_csv("/Users/jennylake8/Desktop/frog_prefs.csv")
View(frog_prefs)


## right now the way we deal w/ Silence (naive) and 'sidedness' gives us some funny values. these are also fairly rare observations, only one set per frog EVER, so -- we will drop these from our graphic analysis.

## so, removing Silence and adding some step numbers so we can identify observations + sequence.
frog_prefs_graphs <- frog_prefs %>%
  filter(inputSound != "Silence")

## also want to create a video number variable
frog_prefs_graphs <- frog_prefs_graphs %>%
  mutate(video123 = case_when((video == 1 |video == 5 | video == 9 | video == 13 | video == 17 | video == 21 | video == 25) ~ "1",
                              (video == 2 |video == 6 | video == 10 | video == 14 | video == 18 | video == 22 | video == 26) ~ "2",
                              (video == 3 |video == 7 | video == 11 | video == 15 | video == 19 | video == 23 | video == 27) ~ "3",
                              (video == 4 |video == 8 | video == 12 | video == 16 | video == 20 | video == 24 | video == 28) ~ "4"))

## take a look at data
View(frog_prefs_graphs)
View(frog_prefs)
View(frog_all)

#### GRAPHS FOR
#### DISTANCE TRAVELED (cm)
#### broken out by VIDEO and TYPE OF SOUND

####### step one
## prep to graph working w/ average values for avePrefTotalCm
frog_prefs_graphs <- frog_prefs_graphs %>%
  ## across all individuals for the sound
  group_by(inputSound) %>%
  mutate(avePrefTotalCm = mean(prefTotalCm)) %>%
  ## ... within video 
  group_by(video123, inputSound) %>%
  mutate(vidPrefTotalCm = mean(prefTotalCm)) %>%
  ## ... within rep ####tried changing it to sort by rep but it's still showing video hmm -JL
  group_by(video123, inputSound) %>%
  mutate(repPrefTotalCm = mean(prefTotalCm)) %>%
  ## ... for the sound WITHIN frog
  group_by(frog, inputSound) %>%
  mutate(frogAvePrefTotalCm = mean(prefTotalCm)) %>%
  ## ... for the sound WITHIN frog WITHIN video
  group_by(frog, video123, inputSound) %>%
  mutate(frogVideoAvePrefTotalCm = mean(prefTotalCm)) %>%
  ## ... for the sound WITHIN frog WITHIN rep
  group_by(frog, rep, inputSound) %>%
  mutate(frogRepAvePrefTotalCm = mean(prefTotalCm)) 


frog_prefs_graphs2 <- frog_prefs_graphs %>%
  ## across all individuals for the sound
  group_by(inputSound) %>%
  mutate(avePrefTotalCm = mean(prefTotalCm))

frog_prefs_graphs2 <- frog_prefs_graphs2 %>%
  ## ... within video 
  group_by(video123, inputSound) %>%
  mutate(vidPrefTotalCm = mean(prefTotalCm))

## ... within rep 
group_by(video123, inputSound) %>%
  mutate(repPrefTotalCm = mean(prefTotalCm)) %>%
  ## ... for the sound WITHIN frog
  group_by(frog, inputSound) %>%
  mutate(frogAvePrefTotalCm = mean(prefTotalCm)) %>%
  ## ... for the sound WITHIN frog WITHIN video
  group_by(frog, video123, inputSound) %>%
  mutate(frogVideoAvePrefTotalCm = mean(prefTotalCm)) %>%
  ## ... for the sound WITHIN frog WITHIN rep
  group_by(frog, rep, inputSound) %>%
  mutate(frogRepAvePrefTotalCm = mean(prefTotalCm)) 

### Repeating code for preference factor Time - JL
frog_prefs_graphs <- frog_prefs_graphs %>%
  ## across all individuals for the sound
  group_by(inputSound) %>%
  mutate(avePrefTotalSec = mean(prefTotalSec)) %>%
  ## ... within video 
  group_by(video123, inputSound) %>%
  mutate(vidPrefTotalSec = mean(prefTotalSec)) %>%
  ## ... within rep
  group_by(video123, inputSound) %>%
  mutate(repPrefTotalSec = mean(prefTotalSec)) %>%
  ## ... for the sound WITHIN frog
  group_by(frog, inputSound) %>%
  mutate(frogAvePrefTotalSec = mean(prefTotalSec)) %>%
  ## ... for the sound WITHIN frog WITHIN video
  group_by(frog, video123, inputSound) %>%
  mutate(frogVideoAvePrefTotalSec = mean(prefTotalSec)) %>%
  ## ... for the sound WITHIN frog WITHIN rep
  group_by(frog, rep, inputSound) %>%
  mutate(frogRepAvePrefTotalSec = mean(prefTotalSec)) 

####Same for PrefDiff - JL
frog_prefs_graphs <- frog_prefs_graphs %>%
  ## across all individuals for the sound
  group_by(inputSound) %>%
  mutate(avePrefDiffCm = mean(prefDiffCm)) %>%
  ## ... within video 
  group_by(video123, inputSound) %>%
  mutate(vidPrefDiffCm = mean(prefDiffCm)) %>%
  ## ... within rep 
  group_by(video123, inputSound) %>%
  mutate(repPrefDiffCm = mean(prefDiffCm)) %>%
  ## ... for the sound WITHIN frog
  group_by(frog, inputSound) %>%
  mutate(frogAvePrefDiffCm = mean(prefDiffCm)) %>%
  ## ... for the sound WITHIN frog WITHIN video
  group_by(frog, video123, inputSound) %>%
  mutate(frogVideoAvePrefDiffCm = mean(prefDiffCm)) %>%
  ## ... for the sound WITHIN frog WITHIN rep
  group_by(frog, rep, inputSound) %>%
  mutate(frogRepAvePrefDiffCm = mean(prefDiffCm)) 

### Repeating code for preference factor Time - JL
frog_prefs_graphs <- frog_prefs_graphs %>%
  ## across all individuals for the sound
  group_by(inputSound) %>%
  mutate(avePrefDiffTime = mean(prefDiffTime)) %>%
  ## ... within video 
  group_by(video123, inputSound) %>%
  mutate(vidPrefDiffTime = mean(prefDiffTime)) %>%
  ## ... within rep 
  group_by(video123, inputSound) %>%
  mutate(repPrefDiffTime = mean(prefDiffTime)) %>%
  ## ... for the sound WITHIN frog
  group_by(frog, inputSound) %>%
  mutate(frogAvePrefDiffTime = mean(prefDiffTime)) %>%
  ## ... for the sound WITHIN frog WITHIN video
  group_by(frog, video123, inputSound) %>%
  mutate(frogVideoAvePrefDiffTime = mean(prefDiffTime)) %>%
  ## ... for the sound WITHIN frog WITHIN rep
  group_by(frog, rep, inputSound) %>%
  mutate(frogRepAvePrefDiffTime = mean(prefDiffTime)) 

### Making new variable for each time one sound is played
frog_prefs_graphs <- frog_prefs_graphs %>%
  mutate(playbackNumber = 
           case_when(frog == 1 & video == 1 & rep == 1 ~ 1,
                     frog == 1 & video == 1 & rep == 2 ~ 2, 
                     frog == 1 & video == 1 & rep == 3 ~ 3, 
                     frog == 1 & video == 2 & rep == 1 ~ 4, 
                     frog == 1 & video == 2 & rep == 2 ~ 5, 
                     frog == 1 & video == 2 & rep == 3 ~ 6,
                     frog == 1 & video == 3 & rep == 1 ~ 7, 
                     frog == 1 & video == 3 & rep == 2 ~ 8, 
                     frog == 1 & video == 3 & rep == 3 ~ 9, 
                     frog == 1 & video == 4 & rep == 1 ~ 10,
                     frog == 1 & video == 4 & rep == 2 ~ 11,
                     frog == 1 & video == 4 & rep == 3 ~ 12,
                     frog == 2 & video == 5 & rep == 1 ~ 1,
                     frog == 2 & video == 5 & rep == 2 ~ 2, 
                     frog == 2 & video == 5 & rep == 3 ~ 3, 
                     frog == 2 & video == 6 & rep == 1 ~ 4, 
                     frog == 2 & video == 6 & rep == 2 ~ 5, 
                     frog == 2 & video == 6 & rep == 3 ~ 6,
                     frog == 2 & video == 7 & rep == 1 ~ 7, 
                     frog == 2 & video == 7 & rep == 2 ~ 8, 
                     frog == 2 & video == 7 & rep == 3 ~ 9, 
                     frog == 2 & video == 8 & rep == 1 ~ 10,
                     frog == 2 & video == 8 & rep == 2 ~ 11,
                     frog == 2 & video == 8 & rep == 3 ~ 12,
                     frog == 3 & video == 9 & rep == 1 ~ 1,
                     frog == 3 & video == 9 & rep == 2 ~ 2, 
                     frog == 3 & video == 9 & rep == 3 ~ 3, 
                     frog == 3 & video == 10 & rep == 1 ~ 4, 
                     frog == 3 & video == 10 & rep == 2 ~ 5, 
                     frog == 3 & video == 10 & rep == 3 ~ 6,
                     frog == 3 & video == 11 & rep == 1 ~ 7, 
                     frog == 3 & video == 11 & rep == 2 ~ 8, 
                     frog == 3 & video == 11 & rep == 3 ~ 9, 
                     frog == 3 & video == 12 & rep == 1 ~ 10,
                     frog == 3 & video == 12 & rep == 2 ~ 11,
                     frog == 3 & video == 12 & rep == 3 ~ 12,
                     frog == 4 & video == 13 & rep == 1 ~ 1,
                     frog == 4 & video == 13 & rep == 2 ~ 2, 
                     frog == 4 & video == 13 & rep == 3 ~ 3, 
                     frog == 4 & video == 14 & rep == 1 ~ 4, 
                     frog == 4 & video == 14 & rep == 2 ~ 5, 
                     frog == 4 & video == 14 & rep == 3 ~ 6,
                     frog == 4 & video == 15 & rep == 1 ~ 7, 
                     frog == 4 & video == 15 & rep == 2 ~ 8, 
                     frog == 4 & video == 15 & rep == 3 ~ 9, 
                     frog == 4 & video == 16 & rep == 1 ~ 10,
                     frog == 4 & video == 16 & rep == 2 ~ 11,
                     frog == 4 & video == 16 & rep == 3 ~ 12,
                     frog == 5 & video == 17 & rep == 1 ~ 1,
                     frog == 5 & video == 17 & rep == 2 ~ 2, 
                     frog == 5 & video == 17 & rep == 3 ~ 3, 
                     frog == 5 & video == 18 & rep == 1 ~ 4, 
                     frog == 5 & video == 18 & rep == 2 ~ 5, 
                     frog == 5 & video == 18 & rep == 3 ~ 6,
                     frog == 5 & video == 19 & rep == 1 ~ 7, 
                     frog == 5 & video == 19 & rep == 2 ~ 8, 
                     frog == 5 & video == 19 & rep == 3 ~ 9, 
                     frog == 5 & video == 20 & rep == 1 ~ 10,
                     frog == 5 & video == 20 & rep == 2 ~ 11,
                     frog == 5 & video == 20 & rep == 3 ~ 12,
                     frog == 6 & video == 21 & rep == 1 ~ 1,
                     frog == 6 & video == 21 & rep == 2 ~ 2, 
                     frog == 6 & video == 21 & rep == 3 ~ 3, 
                     frog == 6 & video == 22 & rep == 1 ~ 4, 
                     frog == 6 & video == 22 & rep == 2 ~ 5, 
                     frog == 6 & video == 22 & rep == 3 ~ 6,
                     frog == 6 & video == 23 & rep == 1 ~ 7, 
                     frog == 6 & video == 23 & rep == 2 ~ 8, 
                     frog == 6 & video == 23 & rep == 3 ~ 9, 
                     frog == 6 & video == 24 & rep == 1 ~ 10,
                     frog == 6 & video == 24 & rep == 2 ~ 11,
                     frog == 6 & video == 24 & rep == 3 ~ 12,
                     frog == 7 & video == 25 & rep == 1 ~ 1,
                     frog == 7 & video == 25 & rep == 2 ~ 2, 
                     frog == 7 & video == 25 & rep == 3 ~ 3, 
                     frog == 7 & video == 26 & rep == 1 ~ 4, 
                     frog == 7 & video == 26 & rep == 2 ~ 5, 
                     frog == 7 & video == 26 & rep == 3 ~ 6,
                     frog == 7 & video == 27 & rep == 1 ~ 7, 
                     frog == 7 & video == 27 & rep == 2 ~ 8, 
                     frog == 7 & video == 27 & rep == 3 ~ 9, 
                     frog == 7 & video == 28 & rep == 1 ~ 10,
                     frog == 7 & video == 28 & rep == 2 ~ 11,
                     frog == 7 & video == 28 & rep == 3 ~ 12))

View(frog_prefs_graphs)


frog_prefs_graphs <- frog_prefs_graphs %>%
  group_by(playbackNumber, inputSound) %>%
  mutate(frogPlaybackAvePrefTotalCm = mean(prefTotalCm))

frog_prefs_graphs <- frog_prefs_graphs %>%
  group_by(playbackNumber, inputSound) %>%
  mutate(frogPlaybackAvePrefTotalSec = mean(prefTotalSec))

frog_prefs_graphs <- frog_prefs_graphs %>%
  group_by(playbackNumber, inputSound) %>%
  mutate(frogPlaybackAvePrefDiffCm = mean(prefDiffCm))

frog_prefs_graphs <- frog_prefs_graphs %>%
  group_by(playbackNumber, inputSound) %>%
  mutate(frogPlaybackAvePrefDiffTime = mean(prefDiffTime))

## pfDistance
frog_prefs_graphs3 <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  mutate(sd = sd(prefTotalCm),
         n = sum(frog >= 1),
         me = qnorm(0.975)*sd/sqrt(n))

##pftime
frog_prefs_graphs3 <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  mutate(sd = sd(prefTotalSec),
         n = sum(frog >= 1),
         me = qnorm(0.975)*sd/sqrt(n))


###pdiffcm
frog_prefs_graphs3 <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  mutate(sd = sd(prefDiffCm),
         n = sum(frog >= 1),
         me = qnorm(0.975)*sd/sqrt(n))

##pdiffTime
frog_prefs_graphs3 <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  mutate(sd = sd(prefDiffTime),
         n = sum(frog >= 1),
         me = qnorm(0.975)*sd/sqrt(n))

## Use this one..
for_graph2 <- filter(frog_prefs_graphs3, inputSound == "Laevis" | inputSound == "Petersii")

View(for_graph2)

##LINE GRAPHS
### PfDistance
ggplot(for_graph2) + 
  geom_line(aes(x = playbackNumber,
                y = frogPlaybackAvePrefTotalCm,
                group = inputSound,
                colour = inputSound)) +
  scale_size_continuous(name = "Playback") + 
  scale_color_manual(values = c("Red", "Orange"), name = "Playback") + 
  xlab("Repetition") + 
  ylab("Preference Factor Distance") + 
  geom_point(aes(x = playbackNumber,
                 y = frogPlaybackAvePrefTotalCm,
                 group = inputSound,
                 colour = inputSound)) + 
  geom_errorbar(aes(x = playbackNumber,
                    ymin = frogPlaybackAvePrefTotalCm - me,
                    ymax = frogPlaybackAvePrefTotalCm + me,
                    color = inputSound), width = 0.25)  + 
  annotate("text", x=3, y=0.95, label= "*") +
  annotate("text", x=4, y=0.93, label= "*") +
  annotate("text", x=5, y=0.99, label= "**") + 
  annotate("text", x=6, y=0.95, label= "**") +
  annotate("text", x=7, y=0.99, label= "**") +
  annotate("text", x=8, y=0.90, label= "*") + 
  annotate("text", x=9, y=0.88, label= "*")


##pfTime
ggplot(for_graph2) + 
  geom_line(aes(x = playbackNumber, y = frogPlaybackAvePrefTotalSec, group = inputSound, colour = inputSound)) + 
  scale_color_manual(values = c("Red", "Orange"), name = "Playback") +
  xlab("Repetition") + ylab("Preference Factor Time") +
  geom_point(aes(x = playbackNumber, y = frogPlaybackAvePrefTotalSec, group = inputSound, colour = inputSound)) + 
  geom_errorbar(aes(x = playbackNumber,
                    ymin = frogPlaybackAvePrefTotalSec - me,
                    ymax = frogPlaybackAvePrefTotalSec + me,
                    color = inputSound), width = 0.25) + 
  annotate("text", x=3, y=0.92, label= "**") +
  annotate("text", x=4, y=0.93, label= "*") +
  annotate("text", x=5, y=0.93, label= "**") + 
  annotate("text", x=6, y=0.91, label= "*") +
  annotate("text", x=7, y=0.95, label= "**") +
  annotate("text", x=8, y=0.87, label= "*") + 
  annotate("text", x=9, y=0.87, label= "*")

##pdiffcm
ggplot(for_graph2) + 
  geom_line(aes(x = playbackNumber, y = frogPlaybackAvePrefDiffCm, group = inputSound, colour = inputSound)) + 
  scale_color_manual(values = c("Red", "Orange"), name = "Playback") +
  xlab("Repetition") + ylab("Preference Difference Distance (cm)") +
  geom_point(aes(x = playbackNumber, y = frogPlaybackAvePrefDiffCm, group = inputSound, colour = inputSound)) + 
  geom_errorbar(aes(x = playbackNumber,
                    ymin = frogPlaybackAvePrefDiffCm - me,
                    ymax = frogPlaybackAvePrefDiffCm + me,
                    color = inputSound), width = 0.25) +
  annotate("text", x=4, y=2500, label= "*") +
  annotate("text", x=5, y=4500, label= "*") + 
  annotate("text", x=6, y=5000, label= "**") +
  annotate("text", x=7, y=5700, label= "**") +
  annotate("text", x=8, y=4700, label= "*") + 
  annotate("text", x=9, y=3500, label= "*")

###pdfiffTime
ggplot(for_graph2) + 
  geom_line(aes(x = playbackNumber, y = frogPlaybackAvePrefDiffTime, group = inputSound, colour = inputSound)) + 
  scale_color_manual(values = c("Red", "Orange"), name = "Playback") +
  xlab("Repetition") + ylab("Preference Difference Time (seconds)") +
  geom_point(aes(x = playbackNumber, y = frogPlaybackAvePrefDiffTime, group = inputSound, colour = inputSound)) + 
  geom_errorbar(aes(x = playbackNumber,
                    ymin = frogPlaybackAvePrefDiffTime - me,
                    ymax = frogPlaybackAvePrefDiffTime + me,
                    color = inputSound), width = 0.25) + 
  annotate("text", x=3, y=730, label= "**") +
  annotate("text", x=4, y=740, label= "*") +
  annotate("text", x=5, y=800, label= "**") + 
  annotate("text", x=6, y=740, label= "*") +
  annotate("text", x=7, y=800, label= "**") +
  annotate("text", x=8, y=650, label= "*") + 
  annotate("text", x=9, y=710, label= "*")

#### can also do these graphs for average preference factor, or average preference difference
#### will need to repeat the above math w/ the proper groupings for your experimental design / nesting
#### to create boxplots, calls are similar, but "geom_boxplot()" instead of "geom_bar()"

##BOX PLOTS FOR 4 VIDEOS
#### boxplots - any laevis playback any frog within videos across frogs - JL.. use prefTotalCm for non averaged data. and frogVideoAvePrefTotalCm for averaged for each frog first
##### average of PFs for each frog - distribution of 7 values for each video - JL...
##can also facet_wrap(~frog) to get graphs for each frog..

ggplot(data = frog_prefs_graphs, mapping = aes(x = video123, y = frogVideoAvePrefTotalCm, fill= inputSound)) +
  geom_boxplot(position = "dodge") + 
  xlab("Video") + ylab("Preference Factor Distance") +
  annotate("text", x=0.67, y=0.9, label= "*") +
  annotate("text", x=1.67, y=0.94, label= "****") +
  annotate("text", x=2.67, y=0.96, label= "****") +
  annotate("text", x=3.67, y=0.87, label= "*") +
  guides(fill=guide_legend(title="Playback"))
         
ggplot(data = frog_prefs_graphs, mapping = aes(x = video123, y = frogVideoAvePrefTotalSec, fill= inputSound)) +
  geom_boxplot(position = "dodge") + xlab("Video") + ylab("Preference Factor Time") + 
  annotate("text", x=0.67, y=0.8, label= "***") +
  annotate("text", x=1.67, y=0.9, label= "****") +
  annotate("text", x=2.67, y=0.91, label= "****") + guides(fill=guide_legend(title="Playback"))

ggplot(data = frog_prefs_graphs, mapping = aes(x = video123, y = frogVideoAvePrefDiffCm, fill= inputSound)) +
  geom_boxplot(position = "dodge") + xlab("Video") + ylab("Preference Difference Distance (cm)") + 
  annotate("text", x=0.67, y=4000, label= "***") +
  annotate("text", x=1.67, y=4100, label= "****") +
  annotate("text", x=2.67, y=5400, label= "****") +
  annotate("text", x=3.67, y=2500, label= "**") + 
  guides(fill=guide_legend(title="Playback"))

ggplot(data = frog_prefs_graphs, mapping = aes(x = video123, y = frogVideoAvePrefDiffTime, fill= inputSound)) +
  geom_boxplot(position = "dodge") + xlab("Video") + ylab("Preference Difference Time (seconds)") +
  annotate("text", x=0.67, y=530, label= "***") +
  annotate("text", x=1.67, y=690, label= "****") +
  annotate("text", x=2.67, y=750, label= "****") +guides(fill=guide_legend(title="Playback"))

###Write csv to file
write_csv(frog_prefs_graphs, "/Users/jennylake8/Desktop/frog_prefs_graphs.csv")

####all playbacks..FROGAVEPREFTOTALSEC.. averaged per frog first!
##pfdistance
ggplot(data = frog_prefs_graphs,
       mapping = aes(x = inputSound, fill = inputSound,
                     y = frogAvePrefTotalCm)) +
  geom_boxplot(position = "dodge") + xlab("Playback") + 
  ylab("Preference Factor Distance") + geom_text(data = frog_prefs_graphs, aes(x = 1.0, y = 0.85),label = "****")

##pfSec
ggplot(data = frog_prefs_graphs,
       mapping = aes(x = inputSound, fill = inputSound,
                     y = frogAvePrefTotalSec)) +
  geom_boxplot(position = "dodge") + xlab("Playback") + 
  ylab("Preference Factor Time") + geom_text(data = frog_prefs_graphs, aes(x = 1.0, y = 0.78),label = "****")

###pdiffCm
ggplot(data = frog_prefs_graphs,
       mapping = aes(x = inputSound, fill = inputSound,
                     y = frogAvePrefDiffCm)) +
  geom_boxplot(position = "dodge") + xlab("Playback") + 
  ylab("Preference Difference Distance (cm)") + geom_text(data = frog_prefs_graphs, aes(x = 1.0, y = 3900),label = "****")

##pdiffTime
ggplot(data = frog_prefs_graphs,
       mapping = aes(x = inputSound, fill = inputSound,
                     y = frogAvePrefDiffTime)) +
  geom_boxplot(position = "dodge") + xlab("Playback") + 
  ylab("Preference Difference Time (seconds)") + geom_text(data = frog_prefs_graphs, aes(x = 1.0, y = 500),label = "****")


##compare means
###use frogAvePrefTotalCm for data analysis because you want to average by FROG first.. not average of all points.

all_t_tests_pfCm <- compare_means(frogAvePrefTotalCm ~ inputSound,  data = frog_prefs_graphs, ref.group = "Laevis",
              method = "t.test")

all_t_tests_pfSec <- compare_means(frogAvePrefTotalSec ~ inputSound,  data = frog_prefs_graphs, ref.group = "Laevis",
              method = "t.test")

all_t_tests_pdiffCm <- compare_means(frogAvePrefDiffCm ~ inputSound,  data = frog_prefs_graphs, ref.group = "Laevis",
              method = "t.test")

all_t_tests_pdiffSec <- compare_means(frogAvePrefDiffTime ~ inputSound,  data = frog_prefs_graphs, ref.group = "Laevis",
              method = "t.test")

####summary statistics
###use frogAvePrefTotalCm
### for all playbacks
##pfdistance
all_summary_stats_pfCm <- frog_prefs_graphs %>%
  group_by(inputSound) %>%
  summarize(mean = mean(frogAvePrefTotalCm, na.rm = TRUE),
            median = median(frogAvePrefTotalCm, na.rm = TRUE),
            sd = sd(frogAvePrefTotalCm, na.rm = TRUE),
            iqr = IQR(frogAvePrefTotalCm, na.rm = TRUE))

##pfSec
all_summary_stats_pfSec <- frog_prefs_graphs %>%
  group_by(inputSound) %>%
  summarize(mean = mean(frogAvePrefTotalSec, na.rm = TRUE),
            median = median(frogAvePrefTotalSec, na.rm = TRUE),
            sd = sd(frogAvePrefTotalSec, na.rm = TRUE),
            iqr = IQR(frogAvePrefTotalSec, na.rm = TRUE))

##pdiffdistance
all_summary_stats_pdiffCm <- frog_prefs_graphs %>%
  group_by(inputSound) %>%
  summarize(mean = mean(frogAvePrefDiffCm, na.rm = TRUE),
            median = median(frogAvePrefDiffCm, na.rm = TRUE),
            sd = sd(frogAvePrefDiffCm, na.rm = TRUE),
            iqr = IQR(frogAvePrefDiffCm, na.rm = TRUE))

##pdiffTime
all_summary_stats_pdiffSec <- frog_prefs_graphs %>%
  group_by(inputSound) %>%
  summarize(mean = mean(frogAvePrefDiffTime, na.rm = TRUE),
            median = median(frogAvePrefDiffTime, na.rm = TRUE),
            sd = sd(frogAvePrefDiffTime, na.rm = TRUE),
            iqr = IQR(frogAvePrefDiffTime, na.rm = TRUE))

## for 4 videos
##pfCm
videos_summary_pfCm <- frog_prefs_graphs %>%
  group_by(inputSound, video123) %>%
  summarize(mean = mean(frogVideoAvePrefTotalCm, na.rm = TRUE),
            median = median(frogVideoAvePrefTotalCm, na.rm = TRUE),
            sd = sd(frogVideoAvePrefTotalCm, na.rm = TRUE),
            iqr = IQR(frogVideoAvePrefTotalCm, na.rm = TRUE))

##pfSec
videos_summary_pfSec <- frog_prefs_graphs %>%
  group_by(inputSound, video123) %>%
  summarize(mean = mean(frogVideoAvePrefTotalSec, na.rm = TRUE),
            median = median(frogVideoAvePrefTotalSec, na.rm = TRUE),
            sd = sd(frogVideoAvePrefTotalSec, na.rm = TRUE),
            iqr = IQR(frogVideoAvePrefTotalSec, na.rm = TRUE)) 

##pdiffCm
videos_summary_pdiffCm <- frog_prefs_graphs %>%
  group_by(inputSound, video123) %>%
  summarize(mean = mean(frogVideoAvePrefDiffCm, na.rm = TRUE),
            median = median(frogVideoAvePrefDiffCm, na.rm = TRUE),
            sd = sd(frogVideoAvePrefDiffCm, na.rm = TRUE),
            iqr = IQR(frogVideoAvePrefDiffCm, na.rm = TRUE))

##pdiffTime
videos_summary_pdiffTime <- frog_prefs_graphs %>%
  group_by(inputSound, video123) %>%
  summarize(mean = mean(frogVideoAvePrefTotalCm, na.rm = TRUE),
            median = median(frogVideoAvePrefTotalCm, na.rm = TRUE),
            sd = sd(frogVideoAvePrefTotalCm, na.rm = TRUE),
            iqr = IQR(frogVideoAvePrefTotalCm, na.rm = TRUE)) 

##p values from t tests

videos_t_tests_pfCm <- compare_means(frogVideoAvePrefTotalCm ~ inputSound, group.by = "video123", data = frog_prefs_graphs, ref.group = "Laevis",
                                    method = "t.test", paired = FALSE)

videos_t_tests_pfSec <- compare_means(frogVideoAvePrefTotalSec ~ inputSound, group.by = "video123", data = frog_prefs_graphs, ref.group = "Laevis",
                                     method = "t.test", paired = FALSE)

videos_t_tests_pdiffCm <- compare_means(frogVideoAvePrefDiffCm ~ inputSound, group.by = "video123", data = frog_prefs_graphs, ref.group = "Laevis",
                                     method = "t.test", paired = FALSE)

videos_t_tests_pdiffTime <- compare_means(frogVideoAvePrefDiffTime ~ inputSound, group.by = "video123", data = frog_prefs_graphs, ref.group = "Laevis",
                                     method = "t.test", paired = FALSE)

##for 12 reps
##pfCm
reps_summary_pfCm <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  summarize(mean = mean(prefTotalCm, na.rm = TRUE),
            median = median(prefTotalCm, na.rm = TRUE),
            sd = sd(prefTotalCm, na.rm = TRUE))

##pfSec
reps_summary_pfSec <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  summarize(mean = mean(prefTotalSec, na.rm = TRUE),
            median = median(prefTotalSec, na.rm = TRUE),
            sd = sd(prefTotalSec, na.rm = TRUE))

##pdiffCm
reps_summary_pdiffCm <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  summarize(mean = mean(prefDiffCm, na.rm = TRUE),
            median = median(prefDiffCm, na.rm = TRUE),
            sd = sd(prefDiffCm, na.rm = TRUE))

##pdiffTime
reps_summary_pdiffTime <- frog_prefs_graphs %>%
  group_by(inputSound, playbackNumber) %>%
  summarize(mean = mean(prefDiffTime, na.rm = TRUE),
            median = median(prefDiffTime, na.rm = TRUE),
            sd = sd(prefDiffTime, na.rm = TRUE))


###p values from t tests
reps_t_tests_pfCm <- compare_means(prefTotalCm ~ inputSound, group.by = "playbackNumber", data = frog_prefs_graphs, ref.group = "Laevis",
                                          method = "t.test", paired = FALSE)

reps_t_tests_pfSec <- compare_means(prefTotalSec ~ inputSound, group.by = "playbackNumber", data = frog_prefs_graphs, ref.group = "Laevis",
                                   method = "t.test", paired = FALSE)

reps_t_tests_pdiffCm <- compare_means(prefDiffCm ~ inputSound, group.by = "playbackNumber", data = frog_prefs_graphs, ref.group = "Laevis",
                                    method = "t.test", paired = FALSE)

reps_t_tests_pdiffTime <- compare_means(prefDiffTime ~ inputSound, group.by = "playbackNumber", data = frog_prefs_graphs, ref.group = "Laevis",
                                      method = "t.test", paired = FALSE)

#######t tests! just to make sure.. here's another way to do it -JL

###T TESTS FOR ALL PLAYBACKS HERE.
##for all
laevis4 <- filter(frog_prefs_graphs, inputSound == "Laevis")
petersii4 <- filter(frog_prefs_graphs, inputSound == "Petersii")

t.test(laevis4$frogAvePrefTotalCm, petersii4$frogAvePrefTotalCm, paired = FALSE, alternative = "two.sided")

##laevis and petersii by video
laevis4 <- filter(frog_prefs_graphs, video123 == 1, inputSound == "Laevis")
petersii4 <- filter(frog_prefs_graphs, video123 == 1, inputSound == "Petersii")

t.test(laevis4$frogVideoAvePrefTotalCm, petersii4$frogVideoAvePrefTotalCm, paired = FALSE, alternative = "two.sided")

## t tests for playbackNumber.. Jay from R tutoring said this one doesn't but the code for reps above is correct
laevis1 <- filter(frog_prefs_graphs, playbackNumber == 1, inputSound == "Laevis")
petersii1 <- filter(frog_prefs_graphs, playbackNumber == 1, inputSound == "Petersii")
t.test(laevis1$frogPlaybackAvePrefDiffCm, petersii1$frogPlaybackAvePrefDiffCm, paired = FALSE, alternative = "two.sided")

