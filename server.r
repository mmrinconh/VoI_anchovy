

library(shiny)

# Define server logic for slider examples
M<-0.075
have1=9.812671284*225*10
#avesurv should be 1/have1
AS<-1/(have1) #1/(have1*0.973) 
#!Carrying capacity in eggs (based Xavier paper)
CC<-2.86*10^12
WindUp<-2
Wind<-runif(10000,2.25,15)
slope<-0.013
logD_mean<-4.6
ages<-c(0,10)
ages<-c(ages[],length(ages[1]:ages[2]))
months<-c(0,11)
months<-c(months[],length(months[1]:months[2]))
years<-c(0,48)
years<-c(years[],length(years[2]:years[1]))
sims<-c(1,200)
sims<-c(sims[],length(sims[2]:sims[1]))
Rel_F<-matrix(1,1000,49)
iniQuant  <- array(NA, dim = c(ages[3],months[3],years[3],sims[3]))
Months<-c("May","June","July","Aug","Sept","Oct", "Nov","Dec","Jan","Feb","March","Apr")
ageX<-c(0:10)
dimnames(iniQuant)<-list(ageClass=ageX, month=Months,year=years[1
                                                                ]:years[2],iter=sims[1]:sims[2])

#Stats
Catch<-array(NA, dim = c(months[3]*years[3],sims[3]))
Catch[]<-0
CatchAnnual<-array(NA, dim = c(years[3],sims[3]))
RecruitsAnnual<-array(NA, dim = c(years[3],sims[3]))
CatchAnnual[]<-0
RecruitsAnnual[]<-0
Trick<-iniQuant 
Trick[]<-0
for(a in 2:5)
{
  Trick[2:11,a,1,1]<-Trick[1:10,a-1,1,1]+1
}
for(a in 6:12)
{
  Trick[1:10,a,1,1]<-Trick[1:10,a-1,1,1]+1
}
Trick[2:11,"May",2,1]<-Trick[1:10,"Apr",1,1]+1
for(a in 2:5)
{
  Trick[2:11,a,2,1]<-Trick[1:10,a-1,2,1]+1
}
for(a in 6:12)
{
  Trick[1:10,a,2,1]<-Trick[1:10,a-1,2,1]+1
}
Trick[7:11,"May",2,1]<-Trick[6:10,"Apr",2,1]+1
Trick[8:11,"June",2,1]<-Trick[7:10,"May",2,1]+1
Trick[9:11,"July",2,1]<-Trick[8:10,"June",2,1]+1
Trick[10:11,"Aug",2,1]<-Trick[9:10,"July",2,1]+1
Trick[11:11,"Sept",2,1]<-Trick[10:10,"Aug",2,1]+1
AgeStructure<-Trick[,,2,1]
###############33AgeStructure

SpawnersAge<-AgeStructure
SpawnersAge[SpawnersAge[]<10]<-0
SpawnersAge[,6:12]<-0
#####################3SpawnersAge
SpawnersWeightAtAge<-SpawnersAge*1.50907-9.2221
SpawnersWeightAtAge[SpawnersWeightAtAge[]<0]<-0
########3SpawnersWeightAtAge
#Number of eggs per one spawner at age
#Growth rate assumed linear with 8.33 g/mo
#Fecundity assumed to be 450 eggs/gram per female
#Fecundity is assumed to be 225 eggs/gram because we assume females are 50% of population
FecAge<-SpawnersWeightAtAge*225
#Spawner Weight at age, assume the growth regression (Ignaci)
#####FecAge
#######sum(FecAge)
#!Average fecundity per fish per spawning event
###########sum(FecAge)/(10+9+8+7+6)

Anchovy<-iniQuant
Anchovy[]<-0
Anchovy[1,1:5,,]<-CC

Mx<-1

y<-1
for(a in 2:5)
{
  Anchovy[2:11,a,y,]<-Anchovy[1:10,a-1,y,]*exp(-Mx)
}
for(a in 6:12)
{
  Anchovy[1:10,a,y,]<-Anchovy[1:10,a-1,y,]*exp(-Mx)
}


for (y in 2:5) {
  Anchovy[2:11,"May",y,]<-Anchovy[1:10,"Apr",y-1,]*exp(-Mx)
  for(a in 2:5)
  {
    Anchovy[2:11,a,y,]<-Anchovy[1:10,a-1,y,]*exp(-Mx)
  }
  for(a in 6:12)
  {
    Anchovy[1:10,a,y,]<-Anchovy[1:10,a-1,y,]*exp(-Mx)
  }
  
}
#!initial population in year five
Anchovy[,,4,1]


##################################################################################
#Period of interest
t0<-12*17
t1<-12*years[3]
y0<-18
y1<-years[3]

#!holding information
AnchovyHCR<-Anchovy
AnchovyEHCR<-Anchovy

CatchHCR<-array(NA, dim = c(months[3]*years[3],sims[3]))
CatchHCR[]<-0
CatchAnnualHCR<-array(NA, dim = c(years[3],sims[3]))
AnchWeightedPrice<-array(0.23, dim = c(years[3],sims[3]))
RecruitsAnnualHCR<-array(NA, dim = c(years[3],sims[3]))
CatchAnnualHCR[]<-0
RecruitsAnnualHCR[]<-0

CatchEHCR<-array(NA, dim = c(months[3]*years[3],sims[3]))
CatchEHCR[]<-0
CatchAnnualEHCR<-array(NA, dim = c(years[3],sims[3]))
RecruitsAnnualEHCR<-array(NA, dim = c(years[3],sims[3]))
CatchAnnualEHCR[]<-0
RecruitsAnnualEHCR[]<-0

#!To look for a correlation between wind in the first half a year and catch in the second
CatchHalfAnnualHCR<-array(NA, dim = c(years[3],sims[3]))
WindHCR<-array(NA, dim = c(years[3],sims[3]))
Revenue_CL<-1  

#Number of years the price is averaged over
Num_Y_Price<-5
#Number of years the harvest is averaged over
Num_Y_Harvest<-5
#!changed to include survival in the first six months dependant on wind and discharge
#AnchPayment<-matrix(NA,ncol=sims[3],nrow=30)
#TrigLevel<-matrix(NA,ncol=sims[3],nrow=30)
#Premium<-0
#AnchInsFund<-matrix(0,ncol=1000,nrow=30)
#PremiumPaidIn<-matrix(0,ncol=1000,nrow=30)
#DividendPaidOut<-matrix(0,ncol=1000,nrow=30)
#Maximum that the lowest the insurance fund should be after 10 years in operation
FundBottom<-0.01
#Annual operating costs in millions of EUROs
AnualOpCosts<-0
#Annual investment interest
InvRate<-0.05
#Borrowing rate if fund falls below zero
BorRate<-0.08











shinyServer(function(input, output) { 
  # Reactive expression to compose a data frame containing all of
  # the values
 ro<-reactive(input$rho)#0.3#0.4
  #D_sd<-reactive(input$sd_Disch)#0.4
#   D_sd<-D_sd()
#   D<-rlnorm(10000,4.6,D_sd)
#   F_D<-dnorm(log(D)-log(100))
F_D<-reactive(dnorm(log(rlnorm(1000,4.6,input$sd_Disch))-log(100)))

output$plot1 <- renderPlot({
 F_D<-F_D()
 ro<-ro()
  hist(ro*F_D, main="Survival rate in months 4 to 6,\naffected by the volume of \nfreshwater discharges", xlim=c(0,1), xlab="Rate")
})
    

output$plot2 <- renderPlot({
  F_D<-F_D()
  ro<-ro()
AS1<-AS/(exp(-M*6)*(ro*median(F_D))^3)#99.5 es la mediana de la distribución para valores de sd entre 0.2 y 0.7
lambda<-log(AS1)/(3*median(-Wind))
hist(exp(-Wind*lambda), main="Survival rate in months 1 to 3, \n affected by extreme wind frequency", xlim=c(0,1), xlab="Rate")
})

sliderValues <- reactive({
  F_D<-F_D()
  ro<-ro()
  AS1<-AS/(exp(-M*6)*(ro*median(F_D))^3)#99.5 es la mediana de la distribución para valores de sd entre 0.2 y 0.7
  lambda<-log(AS1)/(3*median(-Wind))
  #D<-rlnorm(1000,4.6,D_sd)
  # Compose data frame
  data.frame(
    Name = c("rho", "lambda", "sd Disch", "F"),#),
    #"Decimal"),
    #"Range",
    #"Custom Format",
    #"Animation"),
    Value = as.character(c(input$rho,signif(lambda,2),input$sd_Disch,input$FM)),#, 
    # input$decimal
    #paste(input$range, collapse=' '),
    #input$format,
    #input$animation
    #)), 
    stringsAsFactors=FALSE)
}) 

output$values <- renderTable({
  sliderValues()
})

strategy<-reactive({
 F_D<-F_D()
 ro<-ro()
 AS1<-AS/(exp(-M*6)*(ro*median(F_D))^3)#99.5 es la mediana de la distribución para valores de sd entre 0.2 y 0.7
 lambda<-log(AS1)/(3*median(-Wind))
#lambda<-0.15
  FishMortTarget<-input$FM
  D_sd<-input$sd_Disch
 for(s in 1:sims[3])
 {
   FishMort<-FishMortTarget
 for (y in 4:years[3]) {
   #!Target monthly F value for May until Oct 
   #FishMort<-FishMortTarget
   SumWind<-0
   
   #Adults May
   AnchovyHCR[2:11,"May",y,s]<-AnchovyHCR[1:10,"Apr",y-1,s]*exp(-M-FishMortTarget)
   CatchHCR[1+12*(y-1),s]<-sum(AnchovyHCR[1:10,"Apr",y-1,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[2:11,"May",y,s]<-AnchovyEHCR[1:10,"Apr",y-1,s]*exp(-M-FishMort)
   CatchEHCR[1+12*(y-1),s]<-sum(AnchovyEHCR[1:10,"Apr",y-1,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Eggs May
   #Probability of spawning even based on historical data
   x<-runif(1,0,100)
   if(x>=96) p<-4 else 
     if(x<=37) p<-1 else 
       if(x>37 && x<71) p<-2 else p<-3
   AnchovyHCR[1,"May",y,s]<-sum(AnchovyHCR[,"May",y,s]*FecAge[,"May"])*p 
   AnchovyHCR[1,"May",y,s]<-min(AnchovyHCR[1,"May",y,s],CC)
   AnchovyEHCR[1,"May",y,s]<-sum(AnchovyEHCR[,"May",y,s]*FecAge[,"May"])*p 
   AnchovyEHCR[1,"May",y,s]<-min(AnchovyEHCR[1,"May",y,s],CC)
   
   #Adults June
   AnchovyHCR[3:11,"June",y,s]<-AnchovyHCR[2:10,"May",y,s]*exp(-M-FishMortTarget)
   CatchHCR[2+12*(y-1),s]<-sum(AnchovyHCR[2:10,"May",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[3:11,"June",y,s]<-AnchovyEHCR[2:10,"May",y,s]*exp(-M-FishMort)
   CatchEHCR[2+12*(y-1),s]<-sum(AnchovyEHCR[2:10,"May",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Eggs June
   #Probability of spawning even based on historical data
   x<-runif(1,0,100)
   if(x>=96) p<-4 else 
     if(x<=37) p<-1 else 
       if(x>37 && x<71) p<-2 else p<-3
   AnchovyHCR[1,"June",y,s]<-sum(AnchovyHCR[,"June",y,s]*FecAge[,"June"])*p
   AnchovyHCR[1,"June",y,s]<-min(AnchovyHCR[1,"June",y,s],CC)
   AnchovyEHCR[1,"June",y,s]<-sum(AnchovyEHCR[,"June",y,s]*FecAge[,"June"])*p
   AnchovyEHCR[1,"June",y,s]<-min(AnchovyEHCR[1,"June",y,s],CC)
   #Juvenials June
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   SumWind<-SumWind+Wind
   AnchovyHCR[2,"June",y,s]<-AnchovyHCR[1,"May",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[2,"June",y,s]<-AnchovyEHCR[1,"May",y,s]*exp(-lambda*Wind)
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   
   #Adults July
   AnchovyHCR[4:11,"July",y,s]<-AnchovyHCR[3:10,"June",y,s]*exp(-M-FishMortTarget)
   CatchHCR[3+12*(y-1),s]<-sum(AnchovyHCR[3:10,"June",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[4:11,"July",y,s]<-AnchovyEHCR[3:10,"June",y,s]*exp(-M-FishMort)
   CatchEHCR[3+12*(y-1),s]<-sum(AnchovyEHCR[3:10,"June",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Eggs July
   #Probability of spawning even based on historical data
   x<-runif(1,0,100)
   if(x>=96) p<-4 else 
     if(x<=37) p<-1 else 
       if(x>37 && x<71) p<-2 else p<-3
   AnchovyHCR[1,"July",y,s]<-sum(AnchovyHCR[,"July",y,s]*FecAge[,"July"])*p
   AnchovyHCR[1,"July",y,s]<-min(AnchovyHCR[1,"July",y,s],CC)
   AnchovyEHCR[1,"July",y,s]<-sum(AnchovyEHCR[,"July",y,s]*FecAge[,"July"])*p
   AnchovyEHCR[1,"July",y,s]<-min(AnchovyEHCR[1,"July",y,s],CC)
   #Juvenials July
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   SumWind<-SumWind+Wind
   AnchovyHCR[2:3,"July",y,s]<-AnchovyHCR[1:2,"June",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[2:3,"July",y,s]<-AnchovyEHCR[1:2,"June",y,s]*exp(-lambda*Wind)
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   
   #Adults Aug
   AnchovyHCR[5:11,"Aug",y,s]<-AnchovyHCR[4:10,"July",y,s]*exp(-M-FishMortTarget)
   CatchHCR[4+12*(y-1),s]<-sum(AnchovyHCR[4:10,"July",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[5:11,"Aug",y,s]<-AnchovyEHCR[4:10,"July",y,s]*exp(-M-FishMort)
   CatchEHCR[4+12*(y-1),s]<-sum(AnchovyEHCR[4:10,"July",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Eggs Aug
   #Probability of spawning even based on historical data
   x<-runif(1,0,100)
   if(x>=96) p<-4 else 
     if(x<=37) p<-1 else 
       if(x>37 && x<71) p<-2 else p<-3
   AnchovyHCR[1,"Aug",y,s]<-sum(AnchovyHCR[,"Aug",y,s]*FecAge[,"Aug"])*p
   AnchovyHCR[1,"Aug",y,s]<-min(AnchovyHCR[1,"Aug",y,s],CC)
   AnchovyEHCR[1,"Aug",y,s]<-sum(AnchovyEHCR[,"Aug",y,s]*FecAge[,"Aug"])*p
   AnchovyEHCR[1,"Aug",y,s]<-min(AnchovyEHCR[1,"Aug",y,s],CC)
   #Juvenials Aug
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   SumWind<-SumWind+Wind
   AnchovyHCR[2:4,"Aug",y,s]<-AnchovyHCR[1:3,"July",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[2:4,"Aug",y,s]<-AnchovyEHCR[1:3,"July",y,s]*exp(-lambda*Wind)
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   
   #Adults Sept
   AnchovyHCR[6:11,"Sept",y,s]<-AnchovyHCR[5:10,"Aug",y,s]*exp(-M-FishMortTarget)
   CatchHCR[5+12*(y-1),s]<-sum(AnchovyHCR[5:10,"Aug",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[6:11,"Sept",y,s]<-AnchovyEHCR[5:10,"Aug",y,s]*exp(-M-FishMort)
   CatchEHCR[5+12*(y-1),s]<-sum(AnchovyEHCR[5:10,"Aug",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Eggs Sept
   #Eggs Sept
   #Probability of spawning even based on historical data
   x<-runif(1,0,100)
   if(x>=96) p<-4 else 
     if(x<=37) p<-1 else 
       if(x>37 && x<71) p<-2 else p<-3
   AnchovyHCR[1,"Sept",y,s]<-sum(AnchovyHCR[,"Sept",y,s]*FecAge[,"Sept"])*p
   AnchovyHCR[1,"Sept",y,s]<-min(AnchovyHCR[1,"Sept",y,s],CC)
   AnchovyEHCR[1,"Sept",y,s]<-sum(AnchovyEHCR[,"Sept",y,s]*FecAge[,"Sept"])*p
   AnchovyEHCR[1,"Sept",y,s]<-min(AnchovyEHCR[1,"Sept",y,s],CC)
   #Juvenials Sept
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   SumWind<-SumWind+Wind
   AnchovyHCR[2:4,"Sept",y,s]<-AnchovyHCR[1:3,"Aug",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[2:4,"Sept",y,s]<-AnchovyEHCR[1:3,"Aug",y,s]*exp(-lambda*Wind)
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100))
   AnchovyHCR[5,"Sept",y,s]<-AnchovyHCR[4,"Aug",y,s]*ro*F_D
   AnchovyEHCR[5,"Sept",y,s]<-AnchovyEHCR[4,"Aug",y,s]*ro*F_D
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   #Adults Oct
   AnchovyHCR[6:10,"Oct",y,s]<-AnchovyHCR[6:10,"Sept",y,s]*exp(-M-FishMortTarget)
   CatchHCR[6+12*(y-1),s]<-sum(AnchovyHCR[6:10,"Sept",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[6:10,"Oct",y,s]<-AnchovyEHCR[6:10,"Sept",y,s]*exp(-M-FishMort)
   CatchEHCR[6+12*(y-1),s]<-sum(AnchovyEHCR[6:10,"Sept",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Juvenials Oct
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   SumWind<-SumWind+Wind
   AnchovyHCR[1:3,"Oct",y,s]<-AnchovyHCR[1:3,"Sept",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[1:3,"Oct",y,s]<-AnchovyEHCR[1:3,"Sept",y,s]*exp(-lambda*Wind)
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100))
   AnchovyHCR[4:5,"Oct",y,s]<-AnchovyHCR[4:5,"Sept",y,s]*ro*F_D
   AnchovyEHCR[4:5,"Oct",y,s]<-AnchovyEHCR[4:5,"Sept",y,s]*ro*F_D
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   WindHCR[y,s]<-SumWind
   
   #the fishing for the rest of the year depends on previous months' wind conditions
   if (FishMortTarget*{WindUp+5*(SumWind/5-2.25)/(5*(2.25-8.62))} <= FishMortTarget/2) 
   {
     FishMort<-FishMortTarget/2
   }
   else
   {
     FishMort<-FishMortTarget*{WindUp+5*(SumWind/5-2.25)/(5*(2.25-8.62))}
   }
   
   Rel_F[s,y]<- FishMort/FishMortTarget
   
   #Adults Nov
   AnchovyHCR[6:10,"Nov",y,s]<-AnchovyHCR[6:10,"Oct",y,s]*exp(-M)#-FishMortTarget)
   # CatchHCR[7+12*(y-1),s]<-sum(AnchovyHCR[6:10,"Oct",y,s]*(1-exp(-M)-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[6:10,"Nov",y,s]<-AnchovyEHCR[6:10,"Oct",y,s]*exp(-M)#-FishMort)
   # CatchEHCR[7+12*(y-1),s]<-sum(AnchovyEHCR[6:10,"Oct",y,s]*(1-exp(-M)-FishMort)))*FishMort/(FishMort+M)
   #Juvenials 
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   AnchovyHCR[1:2,"Nov",y,s]<-AnchovyHCR[1:2,"Oct",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[1:2,"Nov",y,s]<-AnchovyEHCR[1:2,"Oct",y,s]*exp(-lambda*Wind)
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100))
   AnchovyHCR[3:5,"Nov",y,s]<-AnchovyHCR[3:5,"Oct",y,s]*ro*F_D
   AnchovyEHCR[3:5,"Nov",y,s]<-AnchovyEHCR[3:5,"Oct",y,s]*ro*F_D
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   #Adults Dec
   AnchovyHCR[5:10,"Dec",y,s]<-AnchovyHCR[5:10,"Nov",y,s]*exp(-M)#-FishMortTarget)
   #CatchHCR[8+12*(y-1),s]<-sum(AnchovyHCR[5:10,"Nov",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[5:10,"Dec",y,s]<-AnchovyEHCR[5:10,"Nov",y,s]*exp(-M)#-FishMort)
   # CatchEHCR[8+12*(y-1),s]<-sum(AnchovyEHCR[5:10,"Nov",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Juvenials
   #Number of days in a month with strong easterlies wind
   Wind<-runif(1,2.25,15)
   AnchovyHCR[1,"Dec",y,s]<-AnchovyHCR[1,"Nov",y,s]*exp(-lambda*Wind)
   AnchovyEHCR[1,"Dec",y,s]<-AnchovyEHCR[1,"Nov",y,s]*exp(-lambda*Wind)
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100))
   AnchovyHCR[2:4,"Dec",y,s]<-AnchovyHCR[2:4,"Nov",y,s]*ro*F_D
   AnchovyEHCR[2:4,"Dec",y,s]<-AnchovyEHCR[2:4,"Nov",y,s]*ro*F_D
   #!Adjust Fishing mortality based on wind
   #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   #Adults Jan
   AnchovyHCR[4:10,"Jan",y,s]<-AnchovyHCR[4:10,"Dec",y,s]*exp(-M)#-FishMortTarget)
   #CatchHCR[9+12*(y-1),s]<-sum(AnchovyHCR[4:10,"Dec",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[4:10,"Jan",y,s]<-AnchovyEHCR[4:10,"Dec",y,s]*exp(-M)#-FishMort)
   #CatchEHCR[9+12*(y-1),s]<-sum(AnchovyEHCR[4:10,"Dec",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Juvenials
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100)) 
   AnchovyHCR[1:3,"Jan",y,s]<-AnchovyHCR[1:3,"Dec",y,s]*ro*F_D
   AnchovyEHCR[1:3,"Jan",y,s]<-AnchovyEHCR[1:3,"Dec",y,s]*ro*F_D
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   #Adults Feb
   AnchovyHCR[3:10,"Feb",y,s]<-AnchovyHCR[3:10,"Jan",y,s]*exp(-M)#-FishMortTarget)
   #CatchHCR[10+12*(y-1),s]<-sum(AnchovyHCR[3:10,"Jan",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[3:10,"Feb",y,s]<-AnchovyEHCR[3:10,"Jan",y,s]*exp(-M)#-FishMort)
   # CatchEHCR[10+12*(y-1),s]<-sum(AnchovyEHCR[3:10,"Jan",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Juvenials
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100)) 
   AnchovyHCR[1:2,"Feb",y,s]<-AnchovyHCR[1:2,"Jan",y,s]*ro*F_D
   AnchovyEHCR[1:2,"Feb",y,s]<-AnchovyEHCR[1:2,"Jan",y,s]*ro*F_D
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   #Adults March
   AnchovyHCR[2:10,"March",y,s]<-AnchovyHCR[2:10,"Feb",y,s]*exp(-M-FishMortTarget)
   CatchHCR[11+12*(y-1),s]<-sum(AnchovyHCR[2:10,"Feb",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[2:10,"March",y,s]<-AnchovyEHCR[2:10,"Feb",y,s]*exp(-M-FishMort)
   CatchEHCR[11+12*(y-1),s]<-sum(AnchovyEHCR[2:10,"Feb",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   #Juvenials
   #Modelling monthly discharge
   D<-rlnorm(1,logD_mean,D_sd)
   F_D<-dnorm(log(D)-log(100))
   AnchovyHCR[1,"March",y,s]<-AnchovyHCR[1,"Feb",y,s]*ro*F_D
   AnchovyEHCR[1,"March",y,s]<-AnchovyEHCR[1,"Feb",y,s]*ro*F_D
   #!Adjust Fishing mortality based on discharges
   #F_factor<-max(F_D/dnorm(0),0.85)
   #FishMort<-F_factor*FishMort
   
   #Adults Apr
   AnchovyHCR[1:10,"Apr",y,s]<-AnchovyHCR[1:10,"March",y,s]*exp(-M-FishMortTarget)
   CatchHCR[12+12*(y-1),s]<-sum(AnchovyHCR[1:10,"March",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
   AnchovyEHCR[1:10,"Apr",y,s]<-AnchovyEHCR[1:10,"March",y,s]*exp(-M-FishMort)
   CatchEHCR[12+12*(y-1),s]<-sum(AnchovyEHCR[1:10,"March",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
   m0<-1+12*(y-1)
   m1<-12+12*(y-1)
   CatchAnnualHCR[y,s]<- sum(CatchHCR[m0:m1,s])
   RecruitsAnnualHCR[y,s]<-AnchovyHCR[5,"Nov",y,s]+AnchovyHCR[4,"Dec",y,s]+AnchovyHCR[3,"Jan",y,s]+AnchovyHCR[2,"Feb",y,s]+AnchovyHCR[1,"March",y,s]
   
   CatchAnnualEHCR[y,s]<- sum(CatchEHCR[m0:m1,s])
   RecruitsAnnualEHCR[y,s]<-AnchovyEHCR[5,"Nov",y,s]+AnchovyEHCR[4,"Dec",y,s]+AnchovyEHCR[3,"Jan",y,s]+AnchovyEHCR[2,"Feb",y,s]+AnchovyEHCR[1,"March",y,s]
   
   CatchHalfAnnualHCR[y,s]<- sum(CatchEHCR[(m0+6):m1,s])
 }
}


CC_Y<-CC*5
x<-AnchovyHCR[1,"May",17:years[3],] +
  AnchovyHCR[1,"June",17:years[3],] +
  AnchovyHCR[1,"July",17:years[3],]+
  AnchovyHCR[1,"Aug",17:years[3],] +
  AnchovyHCR[1,"Sept",17:years[3],]


x[x[]<CC_Y]<-0
x[x[]>0]<-1
#sum(x)/((years[3]-16)*sims[3])
Prob_CC_Reached_Y_HCR<- sum(x)/((years[3]-16)*sims[3])
#Look at the proportion of simulations when eggs fall below 10% of CC 

y<-AnchovyHCR[1,"May",17:years[3],] +
  AnchovyHCR[1,"June",17:years[3],] +
  AnchovyHCR[1,"July",17:years[3],]+
  AnchovyHCR[1,"Aug",17:years[3],] +
  AnchovyHCR[1,"Sept",17:years[3],]
y[y[]<0.1*CC_Y]<-1
y[y[]>1]<-0
#sum(y)/((years[3]-16)*sims[3])
Prob_Crash_Y_HCR<-sum(y)/((years[3]-16)*sims[3])

#Look at the proportion of simulations when CC is reached
CC_Y<-CC*5
x<-AnchovyEHCR[1,"May",17:years[3],] +
  AnchovyEHCR[1,"June",17:years[3],] +
  AnchovyEHCR[1,"July",17:years[3],]+
  AnchovyEHCR[1,"Aug",17:years[3],] +
  AnchovyEHCR[1,"Sept",17:years[3],]


x[x[]<CC_Y]<-0
x[x[]>0]<-1
#sum(x)/((years[3]-16)*sims[3])
Prob_CC_Reached_Y_EHCR<- sum(x)/((years[3]-16)*sims[3])
#Look at the proportion of simulations when eggs fall below 10% of CC 

y<-AnchovyEHCR[1,"May",17:years[3],] +
  AnchovyEHCR[1,"June",17:years[3],] +
  AnchovyEHCR[1,"July",17:years[3],]+
  AnchovyEHCR[1,"Aug",17:years[3],] +
  AnchovyEHCR[1,"Sept",17:years[3],]
y[y[]<0.1*CC_Y]<-1
y[y[]>1]<-0
#sum(y)/((years[3]-16)*sims[3])
Prob_Crash_Y_EHCR<-sum(y)/((years[3]-16)*sims[3])

AveMonthlyCatchHCR<-array()
for (s in 1:sims[3]){
  AveMonthlyCatchHCR[s]<-median(CatchHCR[t0:t1,s])
}
#average Quarterly Catches
#median(AveMonthlyCatchHCR*3/10^6)
AveQuartCatchHCR<-median(AveMonthlyCatchHCR*3/10^6)


AveMonthlyCatchEHCR<-array()
for (s in 1:sims[3]){
  AveMonthlyCatchEHCR[s]<-median(CatchEHCR[t0:t1,s])
}
#average Quarterly Catches
#median(AveMonthlyCatchEHCR*3/10^6)
AveQuartCatchEHCR<-median(AveMonthlyCatchEHCR*3/10^6)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#! Insurance calculations are based on 30 years from year 8 to 37
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#!Units in Millions

CatchAnnualX<-CatchAnnualHCR
CatchAnnualX[CatchAnnualX[]>2.0*10^9]<-1
CatchAnnualX[CatchAnnualX[]>1]<-0
#Proportion of times catches are unrealistically high
#sum(CatchAnnualX)/(sims[3]*years[3])

#!Limit catches to a maximum capacity
#CatchAnnual[CatchAnnual[]>2.0*10^9]<-2.0*10^9

AnchYield<-CatchAnnualHCR/10^6
#AnchWeightedPrice<-CatchAnnualHCR
AnchPayment<-matrix(NA,ncol=sims[3],nrow=30)
TrigLevel<-matrix(NA,ncol=sims[3],nrow=30)
#Assume an average Anchovy is worth 23 Euro Cents
#AnchWeightedPrice[]<-0.23
#!DECIDE REVENUE COVERAGE LEVEL
# Revenue_CL<-1  
# 
# #Number of years the price is averaged over
# Num_Y_Price<-5
# #Number of years the harvest is averaged over
# Num_Y_Harvest<-5

#CREATE MATRIX OBJECT TO STORE VALUES


#!EXTRACT ITERATIONS FOR 30 YEARS, FROM year =71 TO 100
for( i in 1:sims[3])
{
  for(y in 1:30)
  {
    j<-y+17
    
    AveRev<-mean(AnchYield[(j-Num_Y_Harvest):(j-1),i]*AnchWeightedPrice[(j-Num_Y_Price):(j-1),i])  
    Trigger<-Revenue_CL* AveRev
    TrigLevel[y,i]<-Trigger
    if (AnchYield[j,i]*AnchWeightedPrice[j,i]<Trigger)
    {
      AnchPayment[y,i]<-(Trigger-AnchYield[j,i]*AnchWeightedPrice[j,i])
    }else
    {                                                           
      AnchPayment[y,i]<-0
    }
  }#end year loop
}#end iteration loop

#################################################
#!Calculate the premiums, insurance fund
Premium<-0
 AnchInsFund<-matrix(0,ncol=1000,nrow=30)
 PremiumPaidIn<-matrix(0,ncol=1000,nrow=30)
 DividendPaidOut<-matrix(0,ncol=1000,nrow=30)
 #Maximum that the lowest the insurance fund should be after 10 years in operation
# FundBottom<-0.01
# #Annual operating costs in millions of EUROs
# AnualOpCosts<-0
# #Annual investment interest
# InvRate<-0.05
# #Borrowing rate if fund falls below zero
# BorRate<-0.08

#Determine the 75th percentile of payments by insurance
#75% de 30*sims[3]*75/100
MaxPayout<-sort(as.vector(AnchPayment))[30*sims[3]*75/100]   #75% de 30000
AnchovyPayment<-AnchPayment
AnchovyPayment[AnchovyPayment[]>MaxPayout]<-MaxPayout

if(MaxPayout>0)
{
  while(min(AnchInsFund[11:30,])<FundBottom)
  {
    Premium<-Premium+1
    AnchInsFund<-matrix(0,ncol=sims[3],nrow=30)
    for( i in 1:sims[3])
    {
      for(y in 1:29)
      {   
        if(AnchInsFund[y,i]<0)
        {
          AnchInsFund[y+1,i]<-AnchInsFund[y,i]*(1+BorRate)+Premium-AnchovyPayment[y,i]-AnualOpCosts
          PremiumPaidIn[y+1,i]<-Premium
        }else
        {
          if(AnchInsFund[y,i]<2*MaxPayout)
          {
            AnchInsFund[y+1,i]<-AnchInsFund[y,i]*(1+InvRate)+Premium-AnchovyPayment[y,i]-AnualOpCosts
            PremiumPaidIn[y+1,i]<-Premium
          }else
          {
            AnchInsFund[y+1,i]<-AnchInsFund[y,i]-AnchovyPayment[y,i]-AnualOpCosts
            PremiumPaidIn[y+1,i]<-0
            DividendPaidOut[y+1,i]<-AnchInsFund[y,i]*InvRate
          }
        }         
      }#end year loop       
    }#end iteration loop      
  }#end while loop
}#end if maxpayout is non-zero

#Reinsurance premium
ReInsPayment<-AnchPayment-AnchovyPayment
#Calculate the expected payouts for re-insurance, and add 25% to cover costs and profits   
ReInsPremium<-mean(apply(ReInsPayment,2,mean))*1.25

#####################################################################################
#!FUND DEVELOPMENT GRATH
#windows(7,5)
#par(mfrow=c(1,1))
# M<-min(AnchInsFund[11:30,]) 
# for( i in 1:1000)
# {
#   for(y in 1:30)
#   {     
#     if (AnchInsFund[y,i]==M) 
#     {
#       Y<-y
#       Iter<-i
#     }
#   } 
# }

# plot(AnchInsFund[,Iter],type="l", lwd=2, main="Insurance Fund", xlab="Years of operation", ylim=c(min(AnchInsFund),2*(MaxPayout+Premium)), ylab="Millions of EUROs", col = "red")
# lines(AnchInsFund[,1000], col = "purple", lwd=2)
# lines(AnchInsFund[,50], col="green", lwd=2)
# lines(AnchInsFund[,150], col="blue", lwd=2)
# abline(h=2*MaxPayout, col = "black", lty = 2) 
# legend("bottomright", "Capped fund limit", lty = 2, col="black", bty ="n")
# legend("topright", paste(sep="", "", Revenue_CL*100,"% CL Revenue policy"),bty ="n")
##################################################################################### 
PremiumHCR<-Premium
AvePremiumHCR<-mean(PremiumPaidIn)
ReInsPremiumHCR<-ReInsPremium
#Average yield to compare to premiums
MeanYieldHCR<-mean(as.vector(AnchYield[18:48,])*0.23)
SdYieldHCR<-sd(as.vector(AnchYield[18:48,])*0.23)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#! Insurance calculations are based on 30 years from year 8 to 37
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#!Units in Millions

CatchAnnualX<-CatchAnnualEHCR
CatchAnnualX[CatchAnnualX[]>2.0*10^9]<-1
CatchAnnualX[CatchAnnualX[]>1]<-0
#Proportion of times catches are unrealistically high
#sum(CatchAnnualX)/(sims[3]*years[3])

#!Limit catches to a maximum capacity
#CatchAnnual[CatchAnnual[]>2.0*10^9]<-2.0*10^9

AnchYield<-CatchAnnualEHCR/10^6
#AnchWeightedPrice<-CatchAnnualEHCR

#Assume an average Anchovy is worth 23 Euro Cents
#AnchWeightedPrice[]<-0.23
#!DECIDE REVENUE COVERAGE LEVEL
# Revenue_CL<-1  
# 
# #Number of years the price is averaged over
# Num_Y_Price<-5
# #Number of years the harvest is averaged over
# Num_Y_Harvest<-5

#CREATE MATRIX OBJECT TO STORE VALUES
AnchPayment<-matrix(NA,ncol=sims[3],nrow=30)
TrigLevel<-matrix(NA,ncol=sims[3],nrow=30)

#!EXTRACT ITERATIONS FOR 30 YEARS, FROM year =71 TO 100
for( i in 1:sims[3])
{
  for(y in 1:30)
  {
    j<-y+17
    
    AveRev<-mean(AnchYield[(j-Num_Y_Harvest):(j-1),i]*AnchWeightedPrice[(j-Num_Y_Price):(j-1),i])  
    Trigger<-Revenue_CL* AveRev
    TrigLevel[y,i]<-Trigger
    if (AnchYield[j,i]*AnchWeightedPrice[j,i]<Trigger)
    {
      AnchPayment[y,i]<-(Trigger-AnchYield[j,i]*AnchWeightedPrice[j,i])
    }else
    {                                                           
      AnchPayment[y,i]<-0
    }
  }#end year loop
}#end iteration loop

#################################################
#!Calculate the premiums, insurance fund
Premium<-0
AnchInsFund<-matrix(0,ncol=1000,nrow=30)
PremiumPaidIn<-matrix(0,ncol=1000,nrow=30)
DividendPaidOut<-matrix(0,ncol=1000,nrow=30)
#Maximum that the lowest the insurance fund should be after 10 years in operation
# FundBottom<-0.01
# #Annual operating costs in millions of EUROs
# AnualOpCosts<-0
# #Annual investment interest
# InvRate<-0.05
# #Borrowing rate if fund falls below zero
# BorRate<-0.08

#Determine the 75th percentile of payments by insurance
MaxPayout<-sort(as.vector(AnchPayment))[30*sims[3]*75/100]   #75% de 30000
AnchovyPayment<-AnchPayment
AnchovyPayment[AnchovyPayment[]>MaxPayout]<-MaxPayout

if(MaxPayout>0)
{
  while(min(AnchInsFund[11:30,])<FundBottom)
  {
    Premium<-Premium+1
    AnchInsFund<-matrix(0,ncol=sims[3],nrow=30)
    for( i in 1:sims[3])
    {
      for(y in 1:29)
      {   
        if(AnchInsFund[y,i]<0)
        {
          AnchInsFund[y+1,i]<-AnchInsFund[y,i]*(1+BorRate)+Premium-AnchovyPayment[y,i]-AnualOpCosts
          PremiumPaidIn[y+1,i]<-Premium
        }else
        {
          if(AnchInsFund[y,i]<2*MaxPayout)
          {
            AnchInsFund[y+1,i]<-AnchInsFund[y,i]*(1+InvRate)+Premium-AnchovyPayment[y,i]-AnualOpCosts
            PremiumPaidIn[y+1,i]<-Premium
          }else
          {
            AnchInsFund[y+1,i]<-AnchInsFund[y,i]-AnchovyPayment[y,i]-AnualOpCosts
            PremiumPaidIn[y+1,i]<-0
            DividendPaidOut[y+1,i]<-AnchInsFund[y,i]*InvRate
          }
        }         
      }#end year loop       
    }#end iteration loop      
  }#end while loop
}#end if maxpayout is non-zero

#Reinsurance premium
ReInsPayment<-AnchPayment-AnchovyPayment
#Calculate the expected payouts for re-insurance, and add 25% to cover costs and profits   
ReInsPremium<-mean(apply(ReInsPayment,2,mean))*1.25

#####################################################################################
#!FUND DEVELOPMENT GRATH
#windows(7,5)
#par(mfrow=c(1,1))
# M<-min(AnchInsFund[11:30,]) 
# for( i in 1:1000)
# {
#   for(y in 1:30)
#   {     
#     if (AnchInsFund[y,i]==M) 
#     {
#       Y<-y
#       Iter<-i
#     }
#   } 
# }

# plot(AnchInsFund[,Iter],type="l", lwd=2, main="Insurance Fund", xlab="Years of operation", ylim=c(min(AnchInsFund),2*(MaxPayout+Premium)), ylab="Millions of EUROs", col = "red")
# lines(AnchInsFund[,1000], col = "purple", lwd=2)
# lines(AnchInsFund[,50], col="green", lwd=2)
# lines(AnchInsFund[,150], col="blue", lwd=2)
# abline(h=2*MaxPayout, col = "black", lty = 2) 
# legend("bottomright", "Capped fund limit", lty = 2, col="black", bty ="n")
# legend("topright", paste(sep="", "", Revenue_CL*100,"% CL Revenue policy"),bty ="n")
##################################################################################### 
PremiumEHCR<-Premium
AvePremiumEHCR<-mean(PremiumPaidIn)
ReInsPremiumEHCR<-ReInsPremium
#Average yield to compare to premiums
MeanYieldEHCR<-mean(as.vector(AnchYield[18:48,])*0.23)
SdYieldEHCR<-sd(as.vector(AnchYield[18:48,])*0.23)

##################################################################################### 
############ COMPARE HCR AND EHCR         ########################################### 
##################################################################################### 

HCR<-rbind(PremiumHCR,
           AvePremiumHCR,
           ReInsPremiumHCR,
           MeanYieldHCR,
           SdYieldHCR,
           SdYieldHCR*100/MeanYieldHCR,
           Prob_CC_Reached_Y_HCR*100,
           Prob_Crash_Y_HCR*100,
           MeanYieldHCR-AvePremiumHCR-ReInsPremiumHCR)



EHCR<-rbind(PremiumEHCR,
            AvePremiumEHCR,            
            ReInsPremiumEHCR,
            MeanYieldEHCR,
            SdYieldEHCR,
            SdYieldEHCR*100/MeanYieldEHCR,
            Prob_CC_Reached_Y_EHCR*100,
            Prob_Crash_Y_EHCR*100,
            MeanYieldEHCR-AvePremiumEHCR-ReInsPremiumEHCR)

Results<-cbind(HCR,EHCR)
rownames(Results)<-c("Payment mil euros","Ave Premium mil euros","ReIns Premium mil euros", "Aver Annual Yield in mil of euros","SD Yield","CV %", "Prob CC reached per Year %","Prob of annual stock crash %","difference")
colnames(Results)<-c("HCR", "EHCR")
a<-signif(Results/10,2)
b<-plot(18:48,CatchAnnualHCR[18:48,50]/10^6,type='l', ylab="Yearly simulated catch (millions)", xlab="Year",xaxt='n',xlim=c(18,48), ylim=c(80,1300),col='green', main="Random simulated catches under two regimes")
lines(18:48,CatchAnnualEHCR[18:48,50]/10^6,type='l',lty=2, col='green')
axis(1,at=seq(from=18,to=48,by=10),lab=seq(from=0,to=30,by=10))
for (i in c(1,51,151)){
  lines(18:47,CatchAnnualHCR[18:47,i]/10^6,col=(i+3))
  lines(18:47,CatchAnnualEHCR[18:47,i]/10^6, col=(i+3),lty=2)
}
legend(20,1200, # places a legend at the appropriate place 
       c("EHCR","HCR (Constant F)"), # puts text in the legend
       
       lty=c(2,1), # gives the legend appropriate symbols (lines)
       
       lwd=c(1,1),col=c("black","black")) # gives the legend lines the correct color and width
list(a=a,b=b)


# data.frame(
#   Name = c( "F"),#),
#   #"Decimal"),
#   #"Range",
#   #"Custom Format",
#   #"Animation"),
#   Value = as.character(c(FishMortTarget*CatchHalfAnnualHCR[7,1]*Premium)),#, 
#   # input$decimal
#   #paste(input$range, collapse=' '),
#   #input$format,
#   #input$animation
#   #)), 
#   stringsAsFactors=FALSE)

 
 
 
  
})
  

  
  
output$plot3 <- renderPlot({
  strategy()$b
}) 
  
output$values2 <- renderTable({
  strategy()$a
})  
  
})










#})
  
  
 