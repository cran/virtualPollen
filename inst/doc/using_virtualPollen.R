## ----setup, warning=FALSE, message=FALSE, echo=FALSE---------------------
#checking if required packages are installed, and installing them if not
list.of.packages <- c("ggplot2", "cowplot", "knitr", "viridis", "tidyr", "formatR", "grid", "devtools", "magrittr", "kableExtra", "viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep=TRUE)

#install virtualPollen if not installed
if(!("virtualPollen" %in% installed.packages())){
  library(devtools)
  install_github("blasbenito/virtualPollen")
}

# source("ecological_memory_functions.R")
library(virtualPollen) #to simulate pollen curves
library(ggplot2) #plotting library
library(cowplot) #plotting library
library(knitr)   #report generation in R
library(viridis) #pretty plotting colors
library(grid)
library(tidyr)
library(formatR)
library(kableExtra) #to fit tables to pdf page size
library(magrittr) #kableExtra requires pipes

options(scipen=999)

#trying to line-wrap code in pdf output
#from https://github.com/yihui/knitr-examples/blob/master/077-wrap-output.Rmd
knitr::opts_chunk$set(echo = TRUE, fig.pos= "h")
opts_chunk$set(tidy.opts=list(width.cutoff=80), tidy=FALSE)


## ---- size="small"-------------------------------------------------------

#sets a state for the generator of pseudo-random numbers
set.seed(1)
#defines the variable "time"
time <- 1:10000
#samples (-1, 0, 1) with replacement 
moves <- sample(x=c(-1, 0, 1), size=length(time), replace=TRUE)
#computes the cumulative sum of moves
random.walk <- cumsum(moves)


## ---- echo=FALSE , fig.height=2.5, fig.width=9, fig.cap="Random walk (a) and its temporal autocorrelation (b)."----

p1 <- ggplot(data=data.frame(Time=time, Value=random.walk), aes(x=Time, y=Value)) + 
  geom_line(color="gray40") + 
  ggtitle("Random walk") +
  xlab("Time (years)")

p2 <- ggplot(data=acfToDf(random.walk, 5000, 50), aes(x=lag, y=acf)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = ci.max), color="red", linetype="dashed") +
  geom_hline(aes(yintercept = ci.min), color="red", linetype="dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  ggtitle("Temporal autocorrelation") +
  xlab("Lag (years)") +
  ylab("Pearson correlation")


plot_grid(p1, p2, labels = c("a", "b"), align = "h")


## ---- size="small"-------------------------------------------------------

#setting a fixed seed for the generator of pseudo-random numbers
set.seed(1)

#generating 5 random numbers in the range [0, 1]
a <- runif(5)

#applying a convolution filter of length 2 and value 1
b <- filter(a, filter=c(1,1), method="convolution", circular=TRUE)


## ---- echo=FALSE , fig.height=2.5, fig.width=9, fig.cap="Original sequence (dashed line) and filtered sequence with filter (solid line)."----

ggplot(data=data.frame(Time=rep(1:length(a), 2), Value=c(a, b), Legend=c(rep("Original (a)", length(a)), rep("Filtered (b)", length(b)))), aes(x=Time, y=Value, group=Legend)) + 
  geom_line(aes(linetype=Legend), color="gray40", size=1) + 
  theme(legend.position="right") + 
  ggtitle("Effect of a convolution filter")

## ---- results="asis", echo=FALSE-----------------------------------------

temp.table <- data.frame(row=1:5, a=round(a, 2), b=round(b, 2))
temp.table$operation <- c("b1 = a1 x f2 + a2 x f1",  "b2 = a2 x f2 + a3 x f1", "b3 = a3 x f2 + a4 x f1" , "b4 = a4 x f2 + a5 x f1", "b5 = a5 x f2 + a6 x f1")
kable(temp.table, caption = "Original sequence (a), filtered sequence (b), and filtering operations. Numbers beside letters a and b represent row numbers, while f1 and f2 represent the values of the convolution filter (both equal to 1 in this case).", booktabs = T) %>% kable_styling(latex_options = c("hold_position", "striped"))


## ---- size="small"-------------------------------------------------------

moves.10 <- filter(moves, filter=rep(1, 10), circular=TRUE)
moves.100 <- filter(moves, filter=rep(1, 100), circular=TRUE)


## ---- fig.height=5, fig.width=9, warning=TRUE, fig.cap="Sequences filtered with different filter lengths: a) Sequence with autocorrelation length equal to 10; b) Temporal autocorrelation of a); c) Sequence with autocorrelation length equal to 100: d) Temporal autocorrelation of c).", echo=FALSE----

p4 <- ggplot(data=data.frame(Time=time, Value=as.vector(moves.10)), aes(x=Time, y=Value)) + 
  geom_line(color="gray40", size=0.5) + 
  ggtitle("Time series") +
  xlab("") +
  theme(axis.line.x=element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))

p5 <- ggplot(data=acfToDf(moves.10, 200, 50), aes(x=lag, y=acf)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = ci.max), color="red", linetype="dashed") +
  geom_hline(aes(yintercept = ci.min), color="red", linetype="dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + 
  ggtitle("Temporal autocorrelation") +
  xlab("") +
  ylab("R-squared") +
  theme(axis.line.x=element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))

p6 <- ggplot(data=data.frame(Time=time, Value=as.vector(moves.100)), aes(x=Time, y=Value)) + 
  geom_line(color="gray40", size=0.5) +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))

p7 <- ggplot(data=acfToDf(moves.100, 200, 50), aes(x=lag, y=acf)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = ci.max), color="red", linetype="dashed") +
  geom_hline(aes(yintercept = ci.min), color="red", linetype="dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  xlab("Lag (years)") +
  ylab("R-squared") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))

plot_grid(p4, p5, p6, p7, labels = c("a", "b", "c", "d"), align = "v", nrow=2)


## ---- size="small"-------------------------------------------------------
moves.5000 <- filter(moves, filter=rep(1, 5000), circular=TRUE)

## ---- fig.height=2.5, fig.width=9, fig.cap="Sequence moves.5000 (a) and its temporal autocorrelation (b). In this case there is a large deviation between the required temporal autocorrelation (5000) and the outcome (2000).", echo=FALSE----

p8 <- ggplot(data=data.frame(Time=time, Value=as.vector(moves.5000)), aes(x=Time, y=Value)) + 
  geom_line(color="gray40", size=0.5) + 
  ggtitle("Time series") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))

p9 <- ggplot(data=acfToDf(moves.5000, 5000, 50), aes(x=lag, y=acf)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = ci.max), color="red", linetype="dashed") +
  geom_hline(aes(yintercept = ci.min), color="red", linetype="dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + 
  ggtitle("Temporal autocorrelation") +
  theme(plot.margin=unit(c(0,0,0,0), "cm")) +
  ylab("R-square")

plot_grid(p8, p9, labels = c("a", "b"), align = "h")


## ---- fig.height=4, fig.width=9, fig.cap="Virtual driver and its temporal autocorrelation. Note that virtual driver B will not be used hereafter to simplify the explanation on the model dynamics."----

driver <- simulateDriverS(
 random.seeds=c(60, 120),
 time=1:10000,
 autocorrelation.lengths = 600,
 output.min=c(0, 0),
 output.max=c(100, 100),
 driver.names=c("A", "B"),
 filename=NULL)


## ---- echo=FALSE---------------------------------------------------------

parameters <- parametersDataframe(rows=1)
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 10000, 1, 0, 50, 10, 0, 0, 600, 0)
parameters[, 2:ncol(parameters)] <- sapply(parameters[, 2:ncol(parameters)], as.numeric)

parameters.t <- data.frame(t(parameters))
parameters.t <- data.frame(parameters.t[-1,])
colnames(parameters.t) <- paste(parameters$label, sep="")
parameters.t$Parameter <- rownames(parameters.t)
rownames(parameters.t) <- NULL
parameters.t <- parameters.t[, c(ncol(parameters.t),1:(ncol(parameters.t)-1))]
#removing last two lines
parameters.t <- parameters.t[c(1:8, 10:11), ]

kable(parameters.t, caption="Parameters of a virtual species. Note that driver.B is ommited in order to simplify the explanation of the model.", booktabs = T) %>% kable_styling(latex_options = c("hold_position", "striped"))


## ---- size="small"-------------------------------------------------------
niche.A <- dnorm(x=0:100, mean=50, sd=10)

## ---- fig.height=2.5, fig.width=6, fig.cap="Ecological niche of the virtual species (blue) against the density (relative availability of values over time) of the driver (gray). Both densities have been scaled in the range [0, 1].", echo=FALSE----

#scaling suitability between 0 and 1
niche.A <- niche.A / max(niche.A)

#getting the density of the driver
driver.A = driver[driver$driver=="A", "value"]
density.driver.A = density(driver.A, from=min(driver.A), to=max(driver.A), n=101, bw=max(driver.A)/100)
density.driver.A.y = (density.driver.A$y - min(density.driver.A$y)) / (max(density.driver.A$y) - min(density.driver.A$y))
driver.A.range = seq(min(driver.A), max(driver.A), length.out = 101)

#dataframe for plot
plot.df = data.frame(Species=rep(paste(parameters[1, "label"], sep=""), 101),
                                Driver=c(rep("Driver A", 101)),
                                Driver.density.x=c(density.driver.A$x),
                                Driver.density.y=c(density.driver.A.y),
                                Value=driver.A.range, 
                                Suitability=niche.A)

ggplot(data=plot.df, aes(x=Value, y=Suitability, group=Species)) + 
    geom_ribbon(data=plot.df, aes(ymin=0, ymax=Driver.density.y), color="gray80", fill="gray80", alpha=0.5) +
    geom_ribbon(data=plot.df, aes(ymin=0, ymax=Suitability), alpha=0.8, colour=NA, fill="#4572A9") +
    geom_line(data=plot.df, aes(x=Value, y=Driver.density.y), color="gray80", alpha=0.5) +
    xlab("Driver values") + 
    ylab("Environmental suitability") + 
    theme(text = element_text(size=12), strip.background = element_rect(fill=NA), panel.spacing = unit(1, "lines"))


## ---- fig.height=5,  fig.width=9, fig.cap="Driver and environmental suitability of the virtual taxa. Burn-in period is highlighted by a gray box in the Environmental suitability panel.", echo=FALSE----

#computing density
density.A <- dnorm(driver[driver$driver == "A", "value"], mean=50, sd=10)

#scaling to [0, 1]
suitability <- density.A / max(density.A)

 burnin.suitability <- jitter(c(rep(1, parameters$maximum.age*5), seq(1, suitability[1], length.out = parameters$maximum.age*5)), amount=max(suitability)/100)
  burnin.suitability[burnin.suitability < 0]<-0
  burnin.suitability[burnin.suitability > 1]<-1
  length.burnin.suitability<-length(burnin.suitability)
  burnin.suitability <- c(burnin.suitability, suitability)

plot.df4 <- data.frame(Time=c(-length.burnin.suitability:-1, 1:(length(suitability))), Suitability=burnin.suitability, Period=c(rep("Burn-in", length.burnin.suitability), rep("Simulation", length(suitability))))

p1 <- ggplot(data=driver[driver$driver == "A", ], aes(x=time, y=value)) + 
  geom_line(size=0.5, color="gray40") + 
  ggtitle("Driver and environmental suitability") + 
  xlab("") + 
  ylab("Driver") +
  coord_cartesian(xlim=c(-500, 10000)) +
  theme(plot.margin = unit(c(0,0,-0.5,0), "cm"), axis.line.x=element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())

p2 <- ggplot(data=plot.df4, aes(x=Time, y=Suitability)) + 
  geom_rect(aes(xmin=min(plot.df4$Time), xmax=0, ymin=0, ymax=Inf), inherit.aes=FALSE, fill="gray90") + 
  geom_line(size=0.5, color="#4572A9") + 
  xlab("Time (years)") + 
  ylab("Suitability") +
  coord_cartesian(xlim=c(-500, 10000))+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1.1, 1))


## ---- fig.height=3, fig.width=6, fig.cap="Biomass vs. age curves resulting from different growth rates for a 400 years life-span.", echo=FALSE----

#objects to store results
growth.rate.vector <- biomass.vector <- age.vector <- vector()

#params
age <- 0:400
maximum.biomass <- 100
growth.rates <- c(0.025, 0.05, 0.1, 0.2, 0.4, 0.8)

#iterating through growth rates
for(growth.rate in growth.rates){
  
biomass.vector <-  c(biomass.vector,maximum.biomass / (1 + maximum.biomass * exp(- growth.rate * age)))

growth.rate.vector <- c(growth.rate.vector, rep(growth.rate, length(age)))

age.vector <- c(age.vector, age)

}

plot.df3 <- data.frame(growth.rate=growth.rate.vector, age=age.vector, biomass=biomass.vector)
plot.df3$growth.rate <- as.factor(plot.df3$growth.rate)

p3 <- ggplot(data=plot.df3, aes(x=age, y=biomass, group=growth.rate, color=growth.rate)) + 
  geom_line(size=1) + 
  scale_color_viridis(option="D", discrete=TRUE, direction=-1) +
  ggtitle("Biomass gain under different growth rates") + 
  ylab("Biomass (relative)") + xlab("Age (years)")

p3

rm(age, age.vector, biomass.vector, growth.rate, growth.rate.vector, growth.rates, p3, plot.df3)


## ---- fig.height=3, fig.width=6, fig.cap="Risk curve defining the probability of removal of a given individual as a function of its fractional age when maximum carrying capacity is reached.", echo=FALSE----

temp.df <- data.frame(Pm=NA, Age=0:100)
temp.df$Pm <- 1 - sqrt(temp.df$Age/100)
temp.df$Age <- temp.df$Age/100
ggplot(data=temp.df, aes(x=Age, y=Pm, color=Pm)) + 
  geom_line(size=1) + 
  scale_color_viridis(option="D", direction=-1) +
  ggtitle("Probability of mortality when carrying capacity is reached") + 
  ylab("Removal probability") + 
  xlab("Age of the individual (as proportion to the maximum age)") + 
  theme(legend.position = "none")
rm(temp.df)


## ---- eval=FALSE, size="small"-------------------------------------------
#  
#  #parameters (1st line in dataframe "parameters")
#  maximum.age <- parameters[1, "maximum.age"]
#  reproductive.age <- parameters[1, "reproductive.age"]
#  growth.rate <- parameters[1, "growth.rate"]
#  carrying.capacity <- parameters[1, "carrying.capacity"]
#  fecundity <- parameters[1, "fecundity"]
#  
#  #reproductive age to proportion
#  reproductive.age <- reproductive.age / maximum.age
#  
#  #years scaled taking maximum.age as reference
#  scaled.year <- 1/maximum.age
#  
#  #vector to store pollen counts
#  pollen.count <- vector()
#  
#  #starting population
#  population <- sample(seq(0, 1, by=scaled.year),
#                      100,
#                      replace=TRUE)
#  
#  #iterating through suitability (once per year)
#  #------------------------------------
#  for(suitability.i in suitability){
#  
#    #AGING -----------------------------------------------
#    population <- population + scaled.year
#  
#    #SENESCENCE ------------------------------------------
#    #1 is the maximum age of ages expressed as proportions
#    population <- population[population < 1]
#  
#    #LOCAL EXTINCTION AND RECOLONIZATION -----------------
#    if (length(population) == 0){
#  
#      #local extinction, replaces population with a seedbank
#      population <- rep(0, floor(100 * suitability.i))
#  
#      #adds 0 to pollen.count
#      pollen.count <- c(pollen.count, 0)
#  
#      #jumps to next iteration
#      next
#    }
#  
#    #PLANT GROWTH ---------------------------------------
#    #biomass of every individual
#    biomass <- maximum.biomass /
#      (1 +  maximum.biomass *
#         exp(- (growth.rate * suitability.i) *
#               (population * maximum.age)
#             )
#       )
#  
#    #SELF-THINNING --------------------------------------
#    #carrying capacity reached
#    while(sum(biomass) > carrying.capacity){
#  
#      #removes a random individual based on risk curve
#      individual.to.remove <- sample(
#        x = length(population),
#        size = 1,
#        replace = TRUE,
#        prob = 1 - sqrt(population) #risk curve
#      )
#  
#      #removing individuals from population and biomass
#      population <- population[-individual.to.remove]
#      biomass <- biomass[-individual.to.remove]
#  
#    }#end of while
#  
#    #REPRODUCTION --------------------------------------
#    #identifyies adult individuals
#    adults <- population > reproductive.age
#  
#    #seeds (vector of 0s)
#    #fractional biomass of adults * fecundity * suitability
#    seeds <- rep(0, floor(sum((biomass[adults]/maximum.biomass) *
#                                fecundity) * suitability.i))
#  
#    #adding seeds to the population
#    population <- c(population, seeds)
#  
#    #POLLEN OUTPUT -------------------------------------
#    #biomass of adults multiplied by suitability
#    pollen.count <- c(pollen.count,
#                      sum(biomass[adults]) * suitability.i)
#  
#  } #end of loop through suitability values
#  

## ---- size="small", fig.height=6.5, fig.width=9, message=TRUE, warning=TRUE, error=TRUE, results="hide", fig.cap="Simulation outcome. Green shades represent different age groups (seedlings, saplings, and adults).", warning=FALSE, message=FALSE----

#simulate population based on parameters
simulation <- simulatePopulation(parameters=parameters[1, ], 
                                 drivers=driver)

#plotting simulation output
plotSimulation(simulation.output=simulation, 
               burnin=FALSE, 
               panels=c("Driver A", 
                        "Suitability", 
                        "Population", 
                        "Pollen"), 
               plot.title="",
               text.size=12,
               line.size=0.4)


## ---- message=TRUE, warning=TRUE, echo=FALSE-----------------------------

parameters[2,] <- c("Species 2", 50, 20, 4, 0.3, 0, 100, 10000, 1, 0, 50, 15, 0, 0, 600, 600)
parameters[3,] <- c("Species 3", 50, 20, 6, 0.4, 0.5, 100, 10000, 1, 0, 50, 20, 0, 0, 600, 600)
parameters[, 2:ncol(parameters)] <- sapply(parameters[, 2:ncol(parameters)], as.numeric)

parameters.t <- data.frame(t(parameters))
parameters.t <- parameters.t[c(2:9, 11:12),]
names(parameters.t) <- paste(parameters$label, sep="")
parameters.t$Parameter <- rownames(parameters.t)
rownames(parameters.t) <- NULL
parameters.t <- parameters.t[, c(ncol(parameters.t),1:(ncol(parameters.t)-1))]

kable(parameters.t, caption="Parameters of the three virtual species.", booktabs = T) %>% kable_styling(latex_options = c("hold_position", "striped"))


## ---- fig.height=5, fig.width=9, fig.cap="Comparison of the pollen abundance and environmental suitability (same in all cases) for the three virtual species shown in Table 2 within the period 5600-6400. Species 2 has a higher fecundity than Species 1 (1 vs 10)", message=FALSE----

#simulating species 2 and 3 of the dataframe parameters
simulation.2 <- simulatePopulation(parameters=parameters, 
                                   species=c(2,3), 
                                   drivers=driver)

#adding the results to the previous simulation
simulation <- c(simulation, simulation.2)
rm(simulation.2)

#plotting the comparison for the time interval between 4000 and 5000.-
compareSimulations(simulation.output=simulation, 
                   species = "all",
                   columns = c("Suitability", "Pollen"), 
                   time.zoom = c(5600, 6400))


## ---- echo=FALSE---------------------------------------------------------
parameters.test <- parametersDataframe(rows=1)
parameters.test[1,] <- c("Test 1", 4, 1, 0.55, 2, 0, 1, 30, 0.5, 0.5, 50, 10, 50, 10, 100, 100)
parameters.test[2,] <- c("Test 2", 3, 1, 0.5, 2, 0, 1, 30, 0.5, 0.5, 50, 10, 50, 10, 100, 100)
parameters.test[, 2:ncol(parameters.test)] <- sapply(parameters.test[, 2:ncol(parameters.test)], as.numeric)

parameters.t <- data.frame(t(parameters.test))
parameters.t <- data.frame(parameters.t[-1,])
names(parameters.t) <- c("Test 1", "Test 2")
parameters.t$Parameter <- rownames(parameters.t)
rownames(parameters.t) <- NULL
parameters.t <- parameters.t[, c(ncol(parameters.t),1:(ncol(parameters.t)-1))]
parameters.t <- parameters.t[1:(nrow(parameters.t)-2),]

kable(parameters.t, caption="Parameters of virtual taxa used to test model limits.")
rm(parameters.t)

## ---- message=FALSE------------------------------------------------------
simulation.test.1 <- simulatePopulation(
  parameters=parameters.test,
  driver.A=jitter(rep(50, 500), amount=4)
  )

## ---- fig.height=3, fig.width=9, fig.cap="Pollen output of virtual taxa Test 1 and Test 2 for a 200 years time-window."----
compareSimulations(simulation.output=simulation.test.1, 
                   columns="Pollen", 
                   time.zoom = c(0, 200))

## ---- echo=FALSE---------------------------------------------------------
parameters.test[3,] <- c("Test 3", 1000, 100, 0.5, 0.05, 0, 100, 10000, 0.5, 0.5, 50, 10, 50, 10, 100, 100)
parameters.test[4,] <- c("Test 4", 1000, 500, 0.5, 0.05, 0, 100, 10000, 0.5, 0.5, 50, 10, 50, 10, 100, 100)
parameters.test[5,] <- c("Test 5", 1000, 900, 0.5, 0.05,0, 100, 10000, 0.5, 0.5, 50, 10, 50, 10, 100, 100)
parameters.test[, 2:ncol(parameters.test)] <- sapply(parameters.test[, 2:ncol(parameters.test)], as.numeric)

parameters.t <- data.frame(t(parameters.test[3:5,]))
parameters.t <- data.frame(parameters.t[-1,])
names(parameters.t) <- c("Test 3", "Test 4", "Test 5")
parameters.t$Parameter <- rownames(parameters.t)
rownames(parameters.t) <- NULL
parameters.t <- parameters.t[, c(ncol(parameters.t),1:(ncol(parameters.t)-1))]
parameters.t <- parameters.t[1:(nrow(parameters.t)-2),]

kable(parameters.t, caption="Parameters of virtual taxa used to test model limits.")
rm(parameters.t)

## ---- message=FALSE------------------------------------------------------
simulation.test.2 <- simulatePopulation(
  parameters=parameters.test,
  species=c(3:5),
  driver.A=jitter(rep(50, 2000), amount=4)
  )

## ---- fig.height=3, fig.width=9, fig.cap="Pollen output of virtual taxa Test 1 and Test 2 for a 200 years time-window."----
compareSimulations(simulation.output=simulation.test.2, 
                   columns="Pollen", 
                   time.zoom = c(0, 800))

## ---- fig.height=3, fig.width=9, fig.cap="Virtual sediment accumulation rate."----
accumulation.rate <- simulateAccumulationRate(seed=140, 
                                              time=1:10000, 
                                              output.min=1, 
                                              output.max=50)

## ---- echo=FALSE---------------------------------------------------------
kable(accumulation.rate[1:20, ], caption="Dataframe resulting from the function to generate virtual accumulation rates. Each group in the grouping column has as many elements as accumulation.rate the given group has.", booktabs = T) %>% kable_styling(latex_options = c("hold_position", "striped"))

## ---- size="small"-------------------------------------------------------
simulation.aggregated <- aggregateSimulation(
  simulation.output=simulation, 
  accumulation.rate=accumulation.rate, 
  sampling.intervals=c(2, 6, 10)
  )

## ---- fig.height=5,  fig.width=9, fig.cap="Effect of applying accumulation rate and different sampling depth intervals to a section of the the annual data produced by the simulation (represented in the Legend by the label Annual). Note that the 10 cm resampling completely misses the whole high-suitability event in the Pollen panel, and barely registers it in the Suitability panel. Inter-decadal variability shown by the Annual data is completely lost even at 1 cm, the higher sampling resolution.", echo=FALSE----

#checking results
temp.list <- simulation.aggregated[1, 1:5]
names(temp.list) <- c("Annual", "1 cm", "2 cm", "6 cm", "10 cm")
compareSimulations(simulation.output=temp.list, columns = c("Suitability","Pollen"), time.zoom=c(5800, 6200))


## ---- fig.height=3, fig.width=6, fig.cap="Histogram of the time differences (in years) between consecutive samples for the outcome of aggregateSimulation when resampled at intervals of 6 centimeters on Species 1. It clearly shows how the data are not organized in regular time intervals, and therefore are unsuitable for analyses requiring regular time lags.", echo=FALSE----

#getting example data sampled at 2cm intervals
simulated.data = simulation.aggregated[[1, 3]]

#checking distribution of differences in age between consecutive samples
hist(simulated.data[2:nrow(simulated.data),"Time"] - simulated.data[1:(nrow(simulated.data)-1),"Time"], main="Age differences between consecutive samples", xlab="Age difference between consecutive samples", col=viridis(12, begin = 0, end=1))


## ---- size="small", eval=FALSE-------------------------------------------
#  
#  #getting example data sampled at 2cm intervals
#  simulated.data = simulation.aggregated[[1, 3]]
#  
#  
#  #span values to be explored
#  span.values=seq(20/nrow(simulated.data),
#                  5/nrow(simulated.data),
#                  by=-0.0005)
#  
#  
#  #plotting the optimization process in real time
#  x11(height=12, width=24)
#  
#  #iteration through candidate span values
#  for(span in span.values){
#  
#    #function to interpolate the data
#    interpolation.function = loess(
#      Pollen ~ Time,
#      data=simulated.data,
#      span=span,
#      control=loess.control(surface="direct"))
#  
#    #plot model fit
#    plot(simulated.data$Pollen, type="l", lwd=2)
#    lines(interpolation.function$fitted, col="red", lwd=2)
#  
#    #if correlation equals 0.9985 loop stops
#    if(cor(interpolation.function$fitted,
#      simulated.data$Pollen) >=  0.9985){break}
#  
#  }
#  
#  #gives time to look at result before closing the plot window
#  Sys.sleep(5)

## ---- warning=FALSE, size="small"----------------------------------------

simulation.interpolated <- toRegularTime(
  x=simulation.aggregated,
  time.column="Time",
  interpolation.interval=10,
  columns.to.interpolate=c("Pollen",
                           "Suitability",
                           "Driver.A")
  )

## ---- echo=FALSE , fig.height=5, fig.width=9, message=TRUE, error=TRUE, warning=TRUE, fig.cap="Data aggregated using virtual accumulation rate and reinterpolated into a regular time grid of 10 years resolution."----

#getting the results for Species 1
temp.list <- simulation.interpolated[1, 1:5]
names(temp.list) <- c("Annual", "1 cm", "2 cm", "6 cm", "10 cm")

#plotting comparison
compareSimulations(simulation.output=temp.list, columns = c("Suitability","Pollen"), time.zoom=c(5800, 6200))


