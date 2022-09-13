###############################################
############# Moderation Demo #################
###############################################

library(lavaan)
library(lavaanPlot)
library(tidyverse)
library(semTools)
library(haven) #read in SPSS data 


data <- read_sav("Data/mediation_data.sav")
View(data)

# mean centre variables using scale()

df <- data %>%
mutate(across(c(read, write, math, science, socst), 
              scale,
              .names = "{col}_c")) 


### Moderation Demo - Observed Variables ###


#Create interaction effect
df <- df %>% 
        mutate(socstXmath = socst_c * math_c)

#Set seed
set.seed(13)

mod_model <- '
  # regressions
  read ~ b1*socst_c
  read ~ b2*math_c
  read ~ b3*socstXmath
  
  # define mean parameter label for centred math for use in simple slopes
  math_c ~ math.mean*1
  
  # define variance parameter label for centred math for use in simple slopes
  math_c ~~ math.var*math_c
  
  # simple slopes for condition effect
  SDbelow := b1 + b3*(math.mean - sqrt(math.var))
  mean := b1 + b3*(math.mean)
  SDabove := b1 + b3*(math.mean + sqrt(math.var))
  '

m1 <- sem(model = mod_model,
            data = df, 
            se = "bootstrap",
            bootstrap = 100)

summary(m1, fit.measures = TRUE)
parameterEstimates(m1, boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE, standardized = FALSE)

##################################
###### Moderation Exercise #######
##################################


### Have a go at running the moderation model again, but instead of using the variable math_c
### Use the binary version himath (0 and 1)
### Tip - you do not need to calculate simple slopes. 
