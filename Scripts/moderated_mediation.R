##################################
#### Moderated Mediation Demo ####
##################################

modmed_model <- '
  # regressions
    read_c ~ a1*socst_c
    read_c ~ a2*math_c
    read_c ~ a3*socstXmath
    science ~ b1*read_c
    science ~ cdash*socst_c
    
  # define mean parameter label for centred math for use in simple slopes
  math_c ~ math.mean*1
  
  # define variance parameter label for centred math for use in simple slopes
  math_c ~~ math.var*math_c
  # index of moderated mediation
    imm := a3*b1    
  # indirect effects conditional on moderator 
    indirect.SDbelow := a1*b1 + a3*-sqrt(math.var)*b1
    indirect.mean := a1*b1 + a3*math.mean*b1
    indirect.SDabove := a1*b1 + a3*sqrt(math.var)*b1
'
m2<- sem(model = modmed_model, data = df, se = "bootstrap", bootstrap = 1000)
summary(m2)