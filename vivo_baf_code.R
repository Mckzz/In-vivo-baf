
rm(vivobaf)
rm(baf)

install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)

# convert data frame to tibble type
baf <- as_tibble(vivobaf)
head(baf)

str(baf)
is.data.frame(baf)

#make single time variable
baf.long <- pivot_longer(baf, cols=c(`0`, `20`, `70`, `120`), names_to = "min", values_to = "area")

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

plot(baf.long.pct$area.pct.change~baf.long.pct$min, 
     main= "Bafilomycin",
     type="n", #plot without any lines or points
     ylab="Percent change",
     xlab="Time (min)", 
     bty="n", # turns offbox around the plot
     tcl=0.5 ) # points axis tick marks inwards (negative points them outwards)

# create new vector with two different colours for plotting by each of the two unique sac categories
colours=c("darkgreen", "hotpink")

# loops through the data by unique category of sac (in this case 4X), subsets the data by each unique category and then adds
# a line to the blank plot of area by time that was initialized in the previous section of script and colours them by that factor
# using the colours vector
for (i in 1: length(unique(baf.long.pct$treatment))) {
  x<-subset(baf.long.pct, treatment==unique(baf.long.pct$treatment)[i])
  lines(x$area.pct.change~x$min, col = colours[i])
}

legend(5, 13, legend=c("Bafilomycin", "Control"), col=c("hotpink", "darkgreen"), lty=1, bty = "n")

plot.baf<-baf.long.pct %>%
  group_by(treatment,ant.post) %>%
  as.data.frame() %>%
  plot(area.pct.change~min, type="l", ylab="Percent change",
       xlab="Time (min)", 
       bty="n", # turns offbox around the plot
       tcl=0.5, # points axis tick marks inwards (negative points them outwards)
       data = .)

##nbfxghx