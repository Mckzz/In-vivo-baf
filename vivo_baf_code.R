install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

head(vivobaf)

# convert data frame to tibble type
baf <- as_tibble(vivobaf)
head(baf)

str(baf)
is.data.frame(baf)

#make single time variable
baf.long <- pivot_longer(baf, 
                         cols=c(`0`, `20`, `70`, `120`), 
                         names_to = "min", 
                         values_to = "area")

head(baf.long)
str(baf.long)

baf.long$min <- as.numeric(baf.long$min)
print(baf.long, n=50)

#making % change column
baf.long.pct <- baf.long %>%
  group_by(treatment, ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  ungroup()

print(baf.long.pct, n=100)
str(baf.long.pct)

baf.long.pct_antpost.mean <- baf.long.pct %>%
  group_by(treatment, larva, min) %>% 
  dplyr::summarise(
    area_mean = mean(area), 
    area.pct.change_mean = mean(area.pct.change)) 

print(baf.long.pct_antpost.mean, n= 48)

# make means and sd
meansd <- baf.long.pct_antpost.mean %>%
  group_by(treatment, min) %>% 
  dplyr::summarise(
    pct.mean = mean(area.pct.change_mean),
    mean = mean(area_mean),
    pct.sd = sd(area.pct.change_mean),
    sd = sd(area_mean)) 

print(meansd, n= 25)

########## plotting  ###########

#plot means/ sd

ggplot(data = meansd, aes(x= min, colour= treatment)) +
  geom_point(aes(y= pct.mean)) +
  geom_line(aes(y= pct.mean)) +
  geom_errorbar(data= meansd,
                aes(x= min, ymin= pct.mean - pct.sd, ymax= pct.mean + pct.sd), group= "treatment",
                width= 2) +
  labs(x = "Min", y = "% change") +
  theme_classic() +
  theme(legend.position = "none")

#### plot the two dataframes together (raw dat)
ggplot(data = anterior, aes(y= area.pct.change , x= min, group= larva, colour= treatment)) +
  geom_line() +
  geom_line(data = posterior, linetype= "dashed") +
  labs(x = "min", y = "% change") +
  theme_classic()


##############################     stats     #############################
end.pct <- baf.long.pct_antpost.mean %>%
  filter(min == 120)

print(end.pct)

# produces a mean for the tratment that is the difference from the intercept (here, the control)
# a specific time point defined by end.pct
mcmod.end.pct <-
  MCMCglmm::MCMCglmm(
    area.pct.change_mean ~ treatment,
    data = end.pct, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.end.pct)

# produces a mean for the tratment that is the difference from the intercept (here, the control)
# taking all points with time into account 
mcmod.pct <-
  MCMCglmm::MCMCglmm(
    area.pct.change_mean ~ treatment * min, random = ~larva,
    data = baf.long.pct_antpost.mean, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.pct)
