

library(shiny)
library(plyr)
library(ggplot2)
load("CondicionInicial2.RData")
Simulacion<-100#200;

Anyos<-40;

perd<-c(0,1)
PERD<-factor(perd)

#############################################################################################

#############################################################################################

#############################   PARAMETROS DEL MODELO BIOLOGICO     #########################

#############################################################################################

#############################################################################################





fec<-500; #eggs g-1

sexr<-0.5;

L<-18.95*(1-exp(-0.9*c(1:24)/12));#De Bellido 2000

Weight<-0.0038*L^3.1939;                                                                                 #############MODIFICAR SEGUN TESIS DE MARGARITA##################

#ro<-0.7;

#lambda<-.2;

ro<-0.8;

#lambda<-.05;
lambda<-0.2

Age<-2;

Year<-2;

Mes<-10;

Natural<-0.1;

FishingReference<-.2;

ScalingNumber<-5e8;

FactorRegulador<-1.5;

#Descargas<-matrix(c( 729.1999814, 637.7041958, 567.9486547, 324.4248588, 196.2893917, 110.2328006, 85.005432,   86.73152914, 86.9152896,  113.2173257, 240.3737251, 502.6743187, 1025.101012, 776.1171792, 730.4873785, 388.4697023, 190, 84.92539376, 37.72987643, 33.74368569, 55.5901853,  121.7030472, 309.1634101, 716.2193659),nrow=2,ncol=12, byrow=T)
# Primera fila son las medias y la segunda las desviaciones tipicas de descargas alcala segun datos confederacion de 1942 al 1999

Levantes<-matrix(c(6.10294117647059,7.26470588235294,9.83823529411765,5.48529411764706,6.39705882352941,6.58823529411765,6.48529411764706,4.17647058823529,6.11764705882353,6.89705882352941,6.20588235294118,7.54411764705882,3.91217932344921,4.20887324239971,4.32736674447365,3.04583551272688,3.02987574973356,3.90206393350704,2.99931058254864,3.33396899576254,4.23445470133370,4.25923520820623,3.44854508964202,3.91834157971126),nrow=2,ncol=12, byrow=T)

# Primera fila son las medias y la segunda las desviaciones tipicas de descargas alcala segun datos del modelo FO de 2009

##Condiciones iniciales
TAC_prop<-c(0.0205500460075657,0.0165627236478888,0.0205500460075657,0.00674777630099172,0.0203455679378387,0.00398732235967692,0.0233104999488805,0.00879255699826194,0.0258664758204683,0.0161537675084347,0.0259687148553318,0.0224925876699724,0.0212657192516103,0.0240261731929251,0.0127798793579389,0.00408956139454044,0.0166649626827523,0.00429403946426746,0.0200388508332481,0.0202433289029752,0.0234127389837440,0.00388508332481341,0.0185052653102955,0.0209590021470197,0.0130865964625294,0.00746344954503630,0.00909927410285247,0.00644105919640119,0.0121664451487578,0.00419180042940395,0.0177895920662509,0.00408956139454044,0.0141089868111645,0.0140067477763010,0.0110418157652592,0.00971270831203354,0.00398732235967692,0.0105306205909416,0.00807688375421736,0.00756568857989981,0.00746344954503630,0.00572538595235661,0.00909927410285247,0.00817912278908087,0.0188119824148860,0.0211634802167468,0.00572538595235661,0.00398732235967692,0.0252530416112872,0.00357836622022288,0.0125754012882118,0.0140067477763010,0.00654329823126470,0.00971270831203354,0.0122686841836213,0.0196298946937941,0.0111440548001227,0.0118597280441673,0.00858807892853492,0.0103261425212146,0.0126776403230754,0.0181985482057049,0.0119619670790308,0.00756568857989981,0.00705449340558225,0.00654329823126470,0.00490747367344852,0.00449851753399448,0.00490747367344852,0.00357836622022288,0.00368060525508639,0.0194254166240671,0.00531642981290257,0.00991718638176056,0.00449851753399448,0.00644105919640119,0.00582762498722012,0.00470299560372150,0.0134955526019834,0.0118597280441673,0.00695225437071874,0.00531642981290257,0.0147224210203456,0.0137000306717105,0.00460075656885799,0.00357836622022288,0.00357836622022288)
##############load CondicionInicial

##################for i=1:Simulacion N(:,:,:,i)=CondicionInicial;end

#eggs<-array(0, dim=c(Anyos,12,Simulacion));

Suma<-0;
Fishing<-c()
Fishing[1]<-FishingReference;
W<-c()
D<-c()
W[1]<-0;

D[1]<-1;



#############################################################################################

#############################################################################################

#############################   PARAMETROS DEL MODELO ECONOMICO     #########################

#############################################################################################

#############################################################################################



#Parámetros diversos

ComisionLonja<-0.03;

ComisionCofradia<-0.02;

T4<-0.0206;#Comisión que se paga por el uso del puerto basada en un procentaje de las ventas

IVA<-0.1;

#Parametros de la flota

Esloras<-c(22,22.06,  21.15,  13.86,  16.5,   13.86,  19.95,17.25,22.2, 19,  22.14,24, 25, 24.3, 17.3, 12.98,21.58,11.4, 17.65,23.07,20.4, 12.2, 16.75,21.15,18.27,16.29,15.2, 15.44,17,13.56,19,13.3, 17,16.4, 16.35,15.6, 13.7, 17.15,15.9, 18.01,16.35,15.14,15.7, 16.3, 22.91,18,11.87,13.09,19.5, 11.8, 16.11,15.94,11.25,14.8, 18.46,18,18,17.83,16.65,16.35,15.2, 18.06,17.5, 17.8, 16.05,11.25,13.13,15.05,13.75,12.32,12.2, 16.95,13.2, 17.54,13.12,13.56,15.25,11.3, 17.9, 16.5, 15.94,14.41,17.72,13.32,10.8, 12.38,13.55);

GT<-c(56.85,64.09,55,19.2, 22.68,18.15,48.54,28.35,47,36.21,61.09,66.38,85,66.45,30.2, 7.64, 77.5, 5.91, 63.38,45.81,56.43,12.03,25.12,58.9, 26.68,18.19,13.86,11.57,22.44,9.22, 38.49,13.59,27,20.26,21.94,20.78,18.71,26.4, 21.94,14.62,18.05,16.71,23.21,16.41,51.66,28.59,9.58, 11.73,39.06,8.13, 20.3, 17.75,8.61, 21.64,24.23,29.55,18.33,14.86,26.8, 21.94,21.77,29.67,19.95,8.5,17.54,8.61, 14.11,15.42,16.04,12.11,9.36, 52,14.88,19.86,11.5, 5.5, 12.93,6.21, 26.53,24.2, 16.09,8.88, 24.44,8.43, 10.57,10.04,10.4);

CV<-c(250.18,375.28,316.41,69.9, 117.73,66.23,316.41,111.85,331.13,181.02,143.63,316.41,309.05,232.52,117.73,64.05,117.73,80.94,244.83,183.22,243.08,50.04,117.73,268.58,117.73,119.57,66.96,161.15,66.23,64.75,161.88,129.51,128.77,125.09,117,74.32,66,200,117,89.04,116.26,108.9,103.02,39,242.83,175.86,24.28,69.9, 312.73,58.87,88.3, 94.19,20.6, 154.53,88.3, 128.77,129.51,80.94,88.3, 106.7,88.3, 114.79,221,145,210,20.6, 139.8,98,160,285,73.58,294.33,88.3, 147.17,106.7,66.23,94.19,69.9, 121.41,72.11,94.28,49.3, 169.24,69.9, 86.83,128,140);

TAC_legal<-c(120600,97200,120600,39600,119400,23400,136800,51600,151800,94800,152400,132000,124800,141000,75000,24000,97800,25200,117600,118800,137400,22800,108600,123000,76800,43800,53400,37800,71400,24600,104400,24000,82800,82200,64800,57000,23400,61800,47400,44400,43800,33600,53400,48000,110400,124200,33600,23400,148200,21000,73800,82200,38400,57000,72000,115200,65400,69600,50400,60600,74400,106800,70200,44400,41400,38400,28800,26400,28800,21000,21600,114000,31200,58200,26400,37800,34200,27600,79200,69600,40800,31200,86400,80400,27000,21000,21000);

Suma_TAC_legal<-sum(TAC_legal);

####Segmentacion de la flota

Pequenyo<-which(Esloras<=18)    #Esloras[Esloras<=18];

Mediano<-which(Esloras>18 & Esloras<=24);

Grande<-which(Esloras>24);

#####Costes fijos mantenimiento barco y artes

RevisionAnual<-2500+(2500/15)*(Esloras-10);#Coste anualizado

RevisionSeguridadAnual<-500; #Se realiza cad dos años a los equipos de seguridad, su coste anualizado son 500

MantenimientoArtesAnual<-8000+(4000/15)*(Esloras-10);#Coste anualizado

#####Gastos Laborales Fijos
Tripulantes<-round(6+(8/15)*(Esloras-10));

SS_Media<-300;#Coste mensual medio de la seguridad social por miembro tripulacion
SS_Anual<-c()
SS_Anual[Pequenyo]<-SS_Media*Tripulantes[Pequenyo]*6;#Coste anual de la SSpara todos los tripulantes

SS_Anual[Mediano]<-SS_Media*Tripulantes[Mediano]*8;

SS_Anual[Grande]<-SS_Media*Tripulantes[Grande]*10;

#Otros

AguaPuertoAnual<-15*36;

Ferreteria<-10+(20/15)*(Esloras-10);#Gastos diarios de ferreteria

#Valores de iniciacion

#Perdidas<-array(0,dim=c(length(Esloras), Anyos, Simulacion));



#############################################################################################

#############################################################################################

#############################,   ciclos biologico-economicos       #########################

#############################################################################################

#############################################################################################

k<-0
Catches<-array(NA, dim=c(Anyos,Simulacion))
TAC<-array(NA, dim=c(Anyos,Simulacion,length(Esloras)))
Balance<-TAC
Balance_Total<-TAC
Armador<-TAC
Armador_noins<-TAC
Trabajadores<-Balance
#Perdidas_barco<-array(NA, dim=c(length(Esloras),2,Simulacion))
PerdidasTrabajo<-array(0, dim=c(Anyos,Simulacion,length(Esloras)))#Será 1 si hay pérdidas o 0 en caso contrario
Perdidastotales<-PerdidasTrabajo
Perdidas_armador<-PerdidasTrabajo
Perdidas<-PerdidasTrabajo
Catchesbio<-Catches
Catchesnum<-Catches

#Catches<-array(6, dim=c(1,Simulacion));

TAC_Adaptativa<-array(NA,dim=c(Anyos,Simulacion))
#W<-c()
#D<-c()
#for Factor=0.1:0.1:3

#·························for (Factor in seq(0.1,3,0.1)){
Factor<-1#0.1
k<-k+1;

Suma_TAC_legal<-sum(TAC_legal);

Suma_TAC_legal<-Suma_TAC_legal*Factor;




Catches[1,1:Simulacion]<-6#array(6, dim=c(1,Simulacion));

# Descargas_mes_a_mes<-mapply(rnorm,Anyos*Simulacion,Descargas[1,], Descargas[2,])
# Descargas_mes_a_mes[Descargas_mes_a_mes<0]<-0
Descargas_mes_a_mes<-sample(c(0,1), Anyos*Simulacion, replace=T,prob=c(0.03,0.97)) 
Descargas_mes_a_mes[(1:length(Descargas_mes_a_mes))%%Anyos==1]<-1

Levantes_mes_a_mes<-mapply(rnorm,Anyos*Simulacion,Levantes[1,], Levantes[2,])
Levantes_mes_a_mes[Levantes_mes_a_mes<0]<-0

W<-rowSums(Levantes_mes_a_mes[,4:9])#Levantes anuales meses de puesta

# Descargas_mes_a_mes_puesta<-Descargas_mes_a_mes[,4:9]
# Descargas_mes_a_mes_puesta[Descargas_mes_a_mes_puesta<10|Descargas_mes_a_mes_puesta>1000]<-1
# Descargas_mes_a_mes_puesta[Descargas_mes_a_mes_puesta>10 & Descargas_mes_a_mes_puesta<1000]<-0
#D<-rowSums(Descargas_mes_a_mes_puesta)#
D<-Descargas_mes_a_mes#esto es descargas_año_a_año pero no cambio el nombre por comodidad
W_año_anterior<-c(0,W)
D_año_anterior<-c(1,D)



FishingReference<-0.2;
Fishing<-array(NA,dim=c(Anyos,Simulacion))

#N[,,,s]<-Condicioninicial

#equivalente de Polina, problema por qué el primer índice edad varía de 1 a 24 Anchovy[1,2:7,,]<-CC

#N(1:24,1:3,1:12,1:s)=ScalingNumber;

eggs<-array(0, dim=c(Anyos,12,Simulacion))
#eggs[3:17,1:12,s]<-0
SpawningBiomass<-eggs


Esfuerzo<-array(NA,dim=c(Anyos*Simulacion,length(Esloras)))
Esfuerzo[,Pequenyo]<-rep(pmin(rep(180,Simulacion*Anyos),rnorm(Simulacion*Anyos,80,15)),times=length(Pequenyo))

Esfuerzo[,Mediano]<-rep(pmin(rep(180,Simulacion*Anyos),rnorm(Simulacion*Anyos,115,20)),times=length(Mediano))  
#min(180,normrnd(80,15));#Máximo de 180 dias segun la orden que regula al sector
Esfuerzo[,Grande]<-rep(pmin(rep(180,Simulacion*Anyos),rnorm(Simulacion*Anyos,130,10)),times=length(Grande))
#Esfuerzo[2,,]<-min(rep(180,Simulacion*Anyos),rnorm(Simulacion*Anyos,115,20));#med

# Esfuerzo[3,,]<-min(rep(180,Simulacion*Anyos),rnorm(Simulacion*Anyos,130,10));#grande
# dimnames(Esfuerzo)<-list(c("Pequeño", "Mediano","Grande"),1:Anyos,1:Simulacion)
Precio_mat<-matrix(rnorm(Anyos*Simulacion,2,0.4),nrow=Anyos)#€/Kg
#Precio_mat<-matrix(rnorm(Anyos*Simulacion,1.7,0.3),nrow=Anyos)#€/Kg
#Precio_mat<-matrix(rep(rnorm(Anyos*Simulacion,1.7,0.3),length(Esloras),each=Anyos),nrow=Anyos)#€/Kg
Precio_in<-array(Precio_mat,dim=c(Anyos,Simulacion,length(Esloras)))
####Combustible
Precio<-aperm(Precio_in,c(1,3,2))
GasoilPrecio<-rnorm(Anyos*Simulacion,0.5,0.1)#€/L


HorasFaena<-10;

CV_Kw<-0.745699871; #Numero de Kw que tiene un CV

Gasoil_Hora<-(3.976+0.236*CV/CV_Kw)*1.2;#Consumo horario de gasoil en litros SEGUN BASTERRDIE ET AL 2010

#Gasoil_Hora<-Gasoil_Hora*1.2;#Ineficiencia de moteores viejos
ConsumoLubricantesAnual<-(100/5)*Esfuerzo


ConsumoGasoilAnual<-t(Gasoil_Hora*t(Esfuerzo))*HorasFaena*GasoilPrecio;


#Hielo

HieloDiario<-75+(75/15)*(Esloras-10);

HieloAnual<-HieloDiario*t(Esfuerzo);#la columna representa el valor para una combinación año simulación

#Comida
ComidaDiaria<-c()
ComidaDiaria[Pequenyo]<-0;

ComidaDiaria[Mediano]<-0;

ComidaDiaria[Grande]<-50;

ComidaAnual<-ComidaDiaria*t(Esfuerzo)#la columna representa el valor para una combinación año simulación

#Porexspan

PorexDiario<-150;

PorexAnual<-PorexDiario*Esfuerzo;

#Otros
Premium<-0
AnchInsFund<-matrix(0,ncol=Simulacion,nrow=Anyos-10)
PremiumPaidIn<-matrix(0,ncol=Simulacion,nrow=Anyos-10)
DividendPaidOut<-matrix(0,ncol=Simulacion,nrow=Anyos-10)
AnchPayment<-matrix(0,ncol=Simulacion,nrow=Anyos-10)
#Maximum that the lowest the insurance fund should be after 10 years in operation
FundBottom<-0.01
AnualOpCosts<-0
#Annual investment interest
InvRate<-0.05
#Borrowing rate if fund falls below zero
BorRate<-0.08



FerreteriaAnual<-Ferreteria*t(Esfuerzo)#la columna representa el valor para una combinación año simulación
prob2perd<-c()
N<-array(0,dim=c(24,Anyos,12,Simulacion))

for (s in 1:Simulacion){
  N[,1,,s]<-CondicionInicial2$CondicionInicial[,30,]
  N[,2,,s]<-CondicionInicial2$CondicionInicial[,30,]
}
#N[,1:30,,s]<-CondInR}


####TAC_adaptativa
# TAC_Adaptativa_W<-Suma_TAC_legal*(-0.0314*W_año_anterior+2.35);##El primero no vale ojo cada 30 años
# 
# mat_D_TAC_dividida<-rbind((1-D_año_anterior/6)*TAC_Adaptativa_W,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
# 
# 
# TAC_Adaptativa_def<-matrix(Suma_TAC_legal,nrow=Anyos, ncol=Simulacion)
# TAC_Adaptativa_W_D<-apply(mat_D_TAC_dividida, 2, max)
# mat_D_TAC_mult<-rbind(TAC_Adaptativa_W_D,Suma_TAC_legal*FactorRegulador)
# TAC_Adaptativa_W_D_mult<-apply(mat_D_TAC_mult, 2, min)
# TAC_Adaptativa_def<-matrix(TAC_Adaptativa_W_D_mult[2:30001],nrow=30)
# ######################################3



shinyServer(function(input, output) { 
  # Reactive expression to compose a data frame containing all of
  # the values
  #Rev_CL<-reactive(input$Rev_CL)
  #tac<-reactive(input$tac)
  #FactorRegulador<-reactive(input$FactorRegulador)

#Despertar para TAC adaptativa
##############################################################
  #output$plot1 <- renderPlot({
   # tac<-tac()
    #FactorRegulador<-FactorRegulador()
    #Suma_TAC_legal<-tac
    #par(mfrow=c(1,1), mar=c(5, 6, 4, 2) + 0.1 )
    #D_año_anterior_0<-0
    #TAC_Adaptativa_W_0<-Suma_TAC_legal*(-0.0314*sort(W_año_anterior)+2.35)
    #mat_D_TAC_dividida_0<-rbind(TAC_Adaptativa_W_0,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
    #TAC_Adaptativa_W_D_0<-apply(mat_D_TAC_dividida_0, 2, max)
   # mat_D_TAC_mult_0<-rbind(TAC_Adaptativa_W_D_0,Suma_TAC_legal*FactorRegulador)
   # TAC_Adaptativa_W_D_0_mult<-apply(mat_D_TAC_mult_0, 2, min)
   # plot(sort(W_año_anterior),TAC_Adaptativa_W_D_0_mult,main="EHCR",type='l',xlab="Number of days strong wind blows", yaxt='n',ylab="")
   # axis(2,at=c(Suma_TAC_legal/FactorRegulador
          #      ,5e6,6e6,7e6,8e6,Suma_TAC_legal*FactorRegulador
   # ),lab=c(expression(paste("TAC/",Phi)),5e6,6e6,7e6,8e6,expression(paste("TAC*",Phi))),las=2)
    

########################################################

    #####Sin ponerle el tope
    #plot(sort(W_año_anterior),TAC_Adaptativa_W_D_0,main="EHCR",type='l',xlab="Number of days strong wind blows", yaxt='n',ylab="")
    #axis(2,at=c(4e6,6e6,8e6,1e7,1.2e7,1.4e7),lab=c(expression(paste("TAC legal/",Phi)),6e6,8e6,1e7,1.2e7,1.4e7),las=2)
    
#     D_año_anterior_0<-1
#     mat_D_TAC_dividida_0<-rbind((1-D_año_anterior_0/6)*TAC_Adaptativa_W_0,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
#     TAC_Adaptativa_W_D_0<-apply(mat_D_TAC_dividida_0, 2, max)
#     mat_D_TAC_mult_0<-rbind(TAC_Adaptativa_W_D_0,Suma_TAC_legal*FactorRegulador)
#     TAC_Adaptativa_W_D_0_mult<-apply(mat_D_TAC_mult_0, 2, min)
#     lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0_mult, col='blue')
#     ###sin el tope
#     ###lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0, col='blue')
#     
#     D_año_anterior_0<-2
#     mat_D_TAC_dividida_0<-rbind((1-D_año_anterior_0/6)*TAC_Adaptativa_W_0,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
#     TAC_Adaptativa_W_D_0<-apply(mat_D_TAC_dividida_0, 2, max) 
#     mat_D_TAC_mult_0<-rbind(TAC_Adaptativa_W_D_0,Suma_TAC_legal*FactorRegulador)
#     TAC_Adaptativa_W_D_0_mult<-apply(mat_D_TAC_mult_0, 2, min)
#     lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0_mult, col='red')
#     #sin el tope
#     #lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0, col='red')
#     
#     D_año_anterior_0<-3
#     mat_D_TAC_dividida_0<-rbind((1-D_año_anterior_0/6)*TAC_Adaptativa_W_0,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
#     TAC_Adaptativa_W_D_0<-apply(mat_D_TAC_dividida_0, 2, max) 
#     mat_D_TAC_mult_0<-rbind(TAC_Adaptativa_W_D_0,Suma_TAC_legal*FactorRegulador)
#     TAC_Adaptativa_W_D_0_mult<-apply(mat_D_TAC_mult_0, 2, min)
#     lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0_mult, col='green')
#     #sin el tope
#     #lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0, col='green')
#     
#     D_año_anterior_0<-4
#     mat_D_TAC_dividida_0<-rbind((1-D_año_anterior_0/6)*TAC_Adaptativa_W_0,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
#     TAC_Adaptativa_W_D_0<-apply(mat_D_TAC_dividida_0, 2, max) 
#     mat_D_TAC_mult_0<-rbind(TAC_Adaptativa_W_D_0,Suma_TAC_legal*FactorRegulador)
#     TAC_Adaptativa_W_D_0_mult<-apply(mat_D_TAC_mult_0, 2, min)
#     lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0_mult, col='pink')
#     #sin el tope
#     #lines(sort(W_año_anterior),TAC_Adaptativa_W_D_0, col='pink')
#     legend("topright",  title=("Number of months \nwith bad discharges level"),
#            c("0","1","2","3","4"), lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
#            
#            lwd=c(2.5,2.5,2.5,2.5,2.5),col=c('black','blue','red','green','pink'), cex=0.8,bty='n',  inset=0.05)
  #})

strategy<-reactive({
  tac<-input$tac
  #FactorRegulador<-FactorRegulador()
  
  #Revenue_CL<-input$Rev_CL/100
  
  
  Suma_TAC_legal<-tac*1000000
  
  #TAC_Adaptativa_W<-Suma_TAC_legal*(-0.0314*W_año_anterior+2.35);##El primero no vale ojo cada 30 años
  
 #### mat_D_TAC_dividida<-rbind((1-D_año_anterior/6)*TAC_Adaptativa_W,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
  #mat_D_TAC_dividida<-rbind(TAC_Adaptativa_W,rep(Suma_TAC_legal/FactorRegulador,Simulacion*Anyos+1))
  #TAC_Adaptativa_W_D<-apply(mat_D_TAC_dividida, 2, max)
  #mat_D_TAC_mult<-rbind(TAC_Adaptativa_W_D,Suma_TAC_legal*FactorRegulador)
  #TAC_Adaptativa_W_D_mult<-apply(mat_D_TAC_mult, 2, min)
  #TAC_Adaptativa_def<-matrix(TAC_Adaptativa_W_D_mult[2:(Anyos*Simulacion+1)],nrow=Anyos)
  
  
  
 TAC_Adaptativa_def<-matrix(tac*1000000, nrow=Anyos, ncol=Simulacion)
  for (s in 1:Simulacion){
    
    
    for (year in 2:Anyos){
      
      Suma_1<-0
      FishingReference<-0.5
     
      
      while (Suma_1*1e-3>TAC_Adaptativa_def[year, s] | Suma_1==0)
      {
        
        #############################     ciclos biologico       #########################
        
        Fishing[year, s]=FishingReference
        #LLenar la población del año de las edades 7:24 luego calcular la spawning biomass, los huevos
        N[7:24,year,1,s]<-N[6:23,year-1,12,s]*exp(-Natural)
       
        for (month in 2:12){

          if (month>6){
          #Spawning que uso, desde Marzo hasta Julio (2 hasta 7)
            N[6,year,month,s]<-eggs[year, month-5,s]*exp(-lambda*Levantes_mes_a_mes[(year+(s-1)*Anyos),(month-2)])*exp(-lambda*Levantes_mes_a_mes[(year+(s-1)*Anyos),(month-1)])*exp(-lambda*Levantes_mes_a_mes[(year+(s-1)*Anyos),month])*D[(s-1)*Anyos+year]
         #D_año_anterior     
              #(ro^3)*pnorm(log(Descargas_mes_a_mes[(year+(s-1)*Anyos),(month-2)])-log(100))*pnorm(log(Descargas_mes_a_mes[(year+(s-1)*Anyos),(month-1)])-log(100))*pnorm(log(Descargas_mes_a_mes[(year+(s-1)*Anyos),month])-log(100)) 
            
            if (N[6,year,month,s]>ScalingNumber) {
              N[6,year,month,s]<-ScalingNumber}# end if
            if (N[6,year,month,s]<1e6) {
              N[6,year,month,s]<-1e6}
            
            
          }#end if
        N[7:24,year,month,s]<-N[6:23,year,month-1,s]*exp(-Natural-Fishing[year,s])
        N[7:24,year,12,s]<-N[6:23,year,11,s]*exp(-Natural)
        SpawningBiomass[year, month, s]=sum(Weight[11:24]*N[11:24,year,month,s]);
        spawn<-sample(1:4, 1, replace=T,prob=c(0.37,0.37,0.22,0.04)) 
        eggs[year, month, s]<-fec*sexr*spawn*SpawningBiomass[year, month, s];
        eggs[year, 2, s]<-0;
          
       }#end for month
        
        
        Catchesbio[year,s]<-sum(Weight[6:24]%*%(N[6:24, year, 2:11, s]*(1-exp(-Natural-Fishing[year,s]))*Fishing[year,s]/(Fishing[year,s]+Natural)))
        
        Catchesnum[year,s]<-sum(N[6:24, year, 2:11, s]*(1-exp(-Natural-Fishing[year,s]))*Fishing[year,s]/(Fishing[year,s]+Natural))
        
        Catches[year,s]<-Catchesbio[year,s]*1e-9;#%En miles de toneladas
        Suma<-Catchesbio[year,s]
        Suma_1<-Suma
        
        Suma<-0
        
        FishingReference<-FishingReference*.9
        
      }#end while
      
      
      
      
      ############################################################################################
      
      
      
      ###############################     ciclos económico       ###########################
      
      
      
      #asume que un boqueron pesa 10g, la TAC está en Kg
      
      #Desactivandolo se conserva la TAC asignada por el estado
      
      TAC[year,s,]<-1e6*Catches[year,s]*TAC_prop
      
    }#end for year
  }#end simulation

Ingresos_num<-Catches*1e6*Precio_mat+5*1e6 #catches en kilogramos y precio euro por kilo para toda la flota
Ingresos_num_transf<-Ingresos_num*1e-5

###########################3Insurance
# AveRev<-median(Ingresos_num[11:Anyos,]*1e-5) #dividiendo euros por 10-5
# Trigger<-Revenue_CL* AveRev
# 
# #Insurance para 20 años
# for( i in 1:Simulacion)
# {
#   for(y in 11:Anyos)
#   {
#     #j<-y+17
#     
#     #AveRev<-mean(AnchYield[(j-Num_Y_Harvest):(j-1),i]*AnchWeightedPrice[(j-Num_Y_Price):(j-1),i])  
#     #TrigLevel[y,i]<-Trigger
#     if (Ingresos_num_transf[y,i]<Trigger) 
#     #(AnchYield[j,i]*AnchWeightedPrice[j,i]<Trigger)
#     {
#       #AnchPayment[y,i]<-(Trigger-AnchYield[j,i]*AnchWeightedPrice[j,i])
#       AnchPayment[(y-10),i]<-(Trigger-Ingresos_num_transf[y,i])
#     }else
#     {                                                           
#       AnchPayment[(y-10),i]<-0
#     }
#   }#end year loop
# }#end iteration loop
# 
# MaxPayout<-sort(as.vector(AnchPayment))[(Anyos-10)*Simulacion*75/100]   #75% de (Anyos-10)*Simulacion
# AnchovyPayment<-AnchPayment
# AnchovyPayment[AnchovyPayment[]>MaxPayout]<-MaxPayout
# 
# if(MaxPayout>0)
# {
#   while(min(AnchInsFund[(Anyos-19):(Anyos-10),])<FundBottom) #(min(AnchInsFund[11:30,])<FundBottom)
#   {
#     Premium<-Premium+1
#     AnchInsFund<-matrix(0,ncol=Simulacion,nrow=Anyos-10)
#     for( i in 1:Simulacion)
#     {
#       for(y in 1:(Anyos-11))
#       {   
#         if(AnchInsFund[y,i]<0)
#         {
#           AnchInsFund[y+1,i]<-AnchInsFund[y,i]*(1+BorRate)+Premium-AnchovyPayment[y,i]-AnualOpCosts
#           PremiumPaidIn[y+1,i]<-Premium
#         }else
#         {
#           if(AnchInsFund[y,i]<2*MaxPayout)
#           {
#             AnchInsFund[y+1,i]<-AnchInsFund[y,i]*(1+InvRate)+Premium-AnchovyPayment[y,i]-AnualOpCosts
#             PremiumPaidIn[y+1,i]<-Premium
#           }else
#           {
#             AnchInsFund[y+1,i]<-AnchInsFund[y,i]-AnchovyPayment[y,i]-AnualOpCosts
#             PremiumPaidIn[y+1,i]<-0
#             DividendPaidOut[y+1,i]<-AnchInsFund[y,i]*InvRate
#           }
#         }         
#       }#end year loop       
#     }#end iteration loop      
#   }#end while loop
# }#end if maxpayout is non-zero
# 
# #Reinsurance premium
# ReInsPayment<-AnchPayment-AnchovyPayment
# #Calculate the expected payouts for re-insurance, and add 25% to cover costs and profits   
# ReInsPremium<-mean(apply(ReInsPayment,2,mean))*1.25
# 
# PremiumHCR<-Premium
# AvePremiumHCR<-mean(PremiumPaidIn)
# ReInsPremiumHCR<-ReInsPremium
# 
# #para acelerar cálculos
# #TAC_re<-aperm(TAC,c(1,3,2))  #Cambio de dimensiones de la TAC
#       # Ingresos<-TAC_re*Precio
#     #Ingresos[,s,]<-TAC[,s,]*Precio[,s];
#    # Ingresos
# 
# PremiumPaid<-rbind(matrix(0,nrow=10,ncol=Simulacion),PremiumPaidIn*1e5)
# Prem<-array(PremiumPaid,dim=c(Anyos,Simulacion,length(Esloras)))
# Premi<-aperm(Prem,c(3,1,2))
# 
# PREMIUM<-Premi*array(matrix(TAC_prop,nrow=length(Esloras),ncol=Anyos),dim=c(length(Esloras),Anyos,Simulacion))
# 
# 
# 
# DividendPaid<-rbind(matrix(0,nrow=10,ncol=Simulacion),DividendPaidOut*1e5)
# Divi<-array(DividendPaid,dim=c(Anyos,Simulacion,length(Esloras)))
# Divid<-aperm(Divi,c(3,1,2))
# 
# DIVID<-Divid*array(matrix(TAC_prop,nrow=length(Esloras),ncol=Anyos),dim=c(length(Esloras),Anyos,Simulacion))
# 
# InsurancePaid<-rbind(matrix(0,nrow=10,ncol=Simulacion),AnchovyPayment*1e5)
# Insu<-array(InsurancePaid,dim=c(Anyos,Simulacion,length(Esloras)))
# Insur<-aperm(Insu,c(3,1,2))
# 
# INSUR<-Insur*array(matrix(TAC_prop,nrow=length(Esloras),ncol=Anyos),dim=c(length(Esloras),Anyos,Simulacion))

###############################################
####Combustible




#%%%%%%%%%%ingresos
for (s in 1:Simulacion){
  Ingresos<-TAC[,s,]*Precio_mat[,s]+matrix(5*1e6*TAC_prop,nrow=Anyos,ncol=length(Esloras),byrow=T) #distribuidos por barco

    #%%%%%%%%%%%%--GASTOS--%%%%%%%%%%%%%
    



    Gastos_1<-Ingresos*(ComisionLonja+ComisionCofradia+T4+IVA);#Impuestos
    
    Gastos_2<-ConsumoGasoilAnual+ConsumoLubricantesAnual+t(HieloAnual)+t(ComidaAnual)+PorexAnual+AguaPuertoAnual+t(FerreteriaAnual);#Gastos variables como funcion del esfuerzo
    
    Gastos_3<-matrix(rep(SS_Anual+RevisionAnual+RevisionSeguridadAnual+MantenimientoArtesAnual,Anyos*Simulacion),nrow=length(Esloras))#+FerreteriaAnual#Gastos fijos anuales
    
    #%%%%%%%%%%%%--BALANCE--%%%%%%%%%%%%%
    
    Balance[,s,]<-Ingresos-Gastos_1-Gastos_2[((s-1)*Anyos+1):(s*Anyos),]
    #Balance_Total[,s,]<-Ingresos-Gastos_1-Gastos_2[((s-1)*Anyos+1):(s*Anyos),]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])-t(PREMIUM[,,s])+t(DIVID[,,s])
    #Balance_medios[,s,]<-Ingresos-Gastos_1-Gastos_2[((s-1)*Anyos+1):(s*Anyos),]/2
   # Perdidas<-arrayInd(which(Balance[,s,]<0),dim(Balance[, s, ]))
    #NoPerdidas<-arrayInd(which(Balance[,s,]>=0),dim(Balance[, s, ]))
    #Armador[,s,]<-(Balance[,s,]<0)*0.7*Balance[,s,]+(Balance[,s,]>=0)*0.35*Balance[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])-t(PREMIUM[,,s])+t(DIVID[,,s])+t(INSUR[,,s])
####################################################333
  #Armador noins se convierte en Armador
#Armador_noins[,s,]<-(Balance[,s,]<0)*0.7*Balance[,s,]+(Balance[,s,]>=0)*0.35*Balance[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])
Armador[,s,]<-(Balance[,s,]<0)*0.7*Balance[,s,]+(Balance[,s,]>=0)*0.35*Balance[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])


  # Armador[,s,]<-Armador[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])
    #colnames(Perdidastotales)<-c("Año","Barco")
  # Perdidas[,s,]<-1*(Balance[,s,]<0)
  Perdidas[,s,]<-1*(Balance[,s,]<0)
   #Perdidastotales[,s,]<-1*(Balance_Total[,s,]<0)
  Perdidas_armador[,s,]<-1*(Armador[,s,]<0)
    #Perdidasdf<-data.frame(Perdidastotales)
    #Perdidasdf_1130<-subset(Perdidasdf,Año>10)
    #Perdidas_barco<-count(Perdidasdf_1130,"Barco")
    #PerdidasTrabajo<-count(Perdidas_barco,"freq")
    
   # Armador[Perdidas[,1],s,Perdidas[,2]]<-0.7*(Balance[Perdidas[,1],s,Perdidas[,2]])#-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)]))
    #Perdidastotales<-arrayInd(which(Balance_Total[,s,]<0),dim(Balance_Total[, s, ]))
     #Año,sim,barco
    #Responder a la pregunta, en cuántas simulaciones el barco tiene tres años seguidos de pérdidas
    
    #Armador[NoPerdidas[,1],s,NoPerdidas[,2]]<-0.35*(Balance[NoPerdidas[,1],s,NoPerdidas[,2]])  #.7*Balance/2                                  
    #PerdidasTrabajo[NoPerdidas[,1],s,NoPerdidas[,2]]<-0
    
    ################################################3
    #Despertar para agregar Gastos_3
    #Armador[,s,]<-Armador[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])
   
   
   
    #for barco=1:length(Esloras)
    
    # if (Ingresos(barco)-Gastos_1(barco)-Gastos_2(barco)<0)
    #   
    #   Armador(barco,year, s)<-0.7*((Ingresos(barco)-Gastos_1(barco)-Gastos_2(barco)))-Gastos_3(barco);#Las pérdidas se las come entera con patatas
    # 
    # Perdidas(barco,year, s)<-1;
    # 
    # else
    #   
    #   Armador(barco,year, s)<-0.7*((Ingresos(barco)-Gastos_1(barco)-Gastos_2(barco))/2)-Gastos_3(barco);
    # 
    # end
    # 
    # end
    
    #Trabajadores(:,year, s)<-(Ingresos-Gastos_1-Gastos_2)/2;
    Trabajadores[,s,]<-Balance[,s,]/2
    
  }#end Simulacion
  PerdidasTrabajo<-Perdidas_armador#Perdidas#Perdidastotales despertar para agregar gastos 3
  PerdidasTrabajodf<-adply(PerdidasTrabajo[11:Anyos,1:Simulacion,],1:3)#100 para acelerar el cáclculo, Convierte PerdidasTrabajo en una tabla año,simulacion,barco,valor
  PerdidasTrabajodfnoNA<-na.omit(PerdidasTrabajodf)
  perdidasbyship<-split(PerdidasTrabajodfnoNA,PerdidasTrabajodfnoNA$X3)#Separa por barcos: Tablas con #año,simulacion,barco,valor
  
  for (i in 1:length(Esloras)){
    if(sum(unique(sort(perdidasbyship[[i]]$V1))-c(0,1))==0){#Pregunta si hay tanto pérdidas y ganancias en la columna
    perdidasbyship_reales<-split(perdidasbyship[[i]],perdidasbyship[[i]]$V1)#separa por ganancias o pérdidas
    #separa sólo pérdidas=1 por barco
    
    prob2perdu<-which(diff(as.numeric(perdidasbyship_reales[[2]]$X1))==1 & diff(as.numeric(perdidasbyship_reales[[2]]$X2))==0)
    #Busca años consecutivos de pérdidas en una simulación
    prob2perd[i]<-length(unique(perdidasbyship_reales[[2]]$X2[prob2perdu])) #Elimina casos en los que se producen 2 años seguidos de pérdida varias veces en la misma simulación, sólo cuenta 1
  }#prob2perd es el número de simulaciones en las cuales se experimentan dos años seguidos de pérdidas al menos una vez
  else
  {
   
    prob2perd[i]<-perdidasbyship[[i]]$V1[1]*Simulacion #Como todo son pérdidas o ganancias multiplica 1 o 0 por el número de Simulaciones
  }
  }

 prob2perdporc<-round(prob2perd*100/Simulacion)# En porcentaje
 Provisional<-na.omit(Armador[11:Anyos,,])
###########3 Despertar
#Provisional_noins<-na.omit(Armador_noins[11:Anyos,,])
###############33


  #Provisional=reshape(Armador(:,11:30,1:Simulacion),87,[])';
  
  GananciaPromedio<-apply(Provisional,3,mean)*1e-3 #media por barco en miles de euros
  #GananciaPromedio=mean(Provisional)'*1e-3;
######################Despertar
#GananciaPromedio_noins<-apply(Provisional_noins,3,mean)*1e-3 #media por barco en miles de euros

#GananciaPromedio=mean(Provisional)'*1e-3;
  GananciaPromedioTotal<-apply(Armador[11:Anyos,,]*1e-3,c(1,2),sum)
  #GananciaSD=std(Provisional)'*1e-3;
  GananciaSD<-apply(Provisional,3,sd)*1e-3 #sd por barco
SpawningBiomassMediaPicoPuesta<-hhhhhhhhhhh
  collapseprob<-length(unique((which(SpawningBiomass[11:Anyos,6,]*1e-9<1)-1)%/%(Anyos-10)))/Simulacion #Ojo si hay un colapso en una simulación, no tiene sentido contar más
  Tripulantesenriesgo<-sum(Tripulantes[which(prob2perdporc>50)])
 Tripulantesenriesgoporc<-sum(Tripulantes[which(prob2perdporc>50)])#*100/sum(Tripulantes)
##################Despertar
#Difins_noins<-apply((Armador[11:Anyos,,]-Armador_noins[11:Anyos,,]),3,sum)*1e-3

############Despertar
  #Results<-cbind(Esloras,Tripulantes,signif(GananciaPromedio,2),signif(GananciaSD,2),prob2perdporc,Difins_noins)

Results<-cbind(Esloras,Tripulantes,signif(GananciaPromedio,2),signif(GananciaSD,2),prob2perdporc)
################Despertar
#Results2<-cbind(collapseprob, Tripulantesenriesgoporc, mean(GananciaPromedioTotal), sd(GananciaPromedioTotal), PremiumHCR*1e5)
Results2<-cbind(collapseprob, Tripulantesenriesgoporc, mean(GananciaPromedioTotal), sd(GananciaPromedioTotal))

#Despertar
# colnames(Results)<-c("Longitud barco (m)", "Número de tripulantes",  "Ganancia media", "Ganancia SD anual","Prob de 2 años seguidos de pérdidas (%)","Diferencia en euros entre las ganancias con seguro y sin seguro (en miles de euros)")
# colnames(Results2)<-c("Probabilidad de colapso (entre 0 y 1)","Trabajadores en riesgo (%), tripulantes de barcos con prob. de 2 años seguidos de pérdidas>50%","Ganancia media anual de toda la flota", "Ganancia sd", "Prima media anual")

colnames(Results)<-c("Longitud barco (m)", "Número de tripulantes",  "Ganancia media (Miles de euros)", "Ganancia SD anual","Prob de 2 años seguidos de pérdidas (%)")
colnames(Results2)<-c("Probabilidad de colapso (entre 0 y 1)","Trabajadores en riesgo (Total=823 tripulantes), número de tripulantes de barcos con prob. >50% de tener 2 años seguidos de pérdidas","Ganancia media anual de toda la flota (Miles de euros)", "Ganancia sd")
 a<-Results
b<-Results2
s<-53
#Despertar para insurance
#c<-plot(Armador[11:Anyos,s,85])
#points(Armador_noins[11:Anyos,s,85],pch=3)

c<-qplot(Esloras,GananciaPromedio,ylab="Ganancia media (miles de €)")+geom_point(size=2)
#list(b=b,c=c)
list(a=a,b=b,c=c)
})
  
#   Results<-cbind(HCR,EHCR)
#   rownames(Results)<-c("Payment mil euros","Ave Premium mil euros","ReIns Premium mil euros", "Aver Annual Yield in mil of euros","SD Yield","CV %", "Prob CC reached per Year %","Prob of annual stock crash %","difference")
#   colnames(Results)<-c("HCR", "EHCR")
#   a<-signif(Results/10,2)
#   b<-plot(18:48,CatchAnnualHCR[18:48,50]/10^6,type='l', ylab="Yearly simulated catch (millions)", xlab="Year",xaxt='n',xlim=c(18,48), ylim=c(80,1300),col='green', main="Random simulated catches under two regimes")
#   lines(18:48,CatchAnnualEHCR[18:48,50]/10^6,type='l',lty=2, col='green')
#   axis(1,at=seq(from=18,to=48,by=10),lab=seq(from=0,to=30,by=10))
#   for (i in c(1,51,151)){
#     lines(18:47,CatchAnnualHCR[18:47,i]/10^6,col=(i+3))
#     lines(18:47,CatchAnnualEHCR[18:47,i]/10^6, col=(i+3),lty=2)
#   }
#   legend(20,1200, # places a legend at the appropriate place 
#          c("EHCR","HCR (Constant F)"), # puts text in the legend
#          
#          lty=c(2,1), # gives the legend appropriate symbols (lines)
#          
#          lwd=c(1,1),col=c("black","black")) # gives the legend lines the correct color and width
#   list(a=a,b=b)
#   
#   
#   # data.frame(
#   #   Name = c( "F"),#),
#   #   #"Decimal"),
#   #   #"Range",
#   #   #"Custom Format",
#   #   #"Animation"),
#   #   Value = as.character(c(FishMortTarget*CatchHalfAnnualHCR[7,1]*Premium)),#, 
#   #   # input$decimal
#   #   #paste(input$range, collapse=' '),
#   #   #input$format,
#   #   #input$animation
#   #   #)), 
#   #   stringsAsFactors=FALSE)
#   
#   
#   
#   
#   
# })
# 

output$plot3 <- renderPlot({
  strategy()$c
}) 

output$values <- renderTable({
  strategy()$a
}) 

output$values2 <- renderTable({
  strategy()$b
  }) 
  
  
  
  
  
  
  
  
 








})
  
  
 #Plots varias
# s<-50
# par(mfrow=c(3,1))
# plot(colMeans((-t(PREMIUM[,,s])+t(DIVID[,,s])+t(INSUR[,,s]))[11:30,]))
# plot(colMeans(((Balance[,s,]<0)*0.7*Balance[,s,]+(Balance[,s,]>=0)*0.35*Balance[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)]))[11:30,]),ylab="")
# plot(colMeans(((Balance[,s,]<0)*0.7*Balance[,s,]+(Balance[,s,]>=0)*0.35*Balance[,s,]-t(Gastos_3[,((s-1)*Anyos+1):(s*Anyos)])-t(PREMIUM[,,s])+t(DIVID[,,s])+t(INSUR[,,s]) )[11:30,]),ylab="")
# s<-51
# s<-100
# s<-52
# s<-53
# plot(rowMeans(INSUR[,,1]))
# plot(rowMeans(INSUR[,,50]))
# plot(rowMeans(INSUR[,,100]))
# 
# 
# plot(rowMeans(PREMIUM[,,1]))
# plot(rowMeans(PREMIUM[,,50]))
# plot(rowMeans(PREMIUM[,,100]))
# 
# plot(rowMeans(DIVID[,,1]))
# plot(rowMeans(DIVID[,,50]))
# plot(rowMeans(DIVID[,,100]))
# 
# plot(Armador[11:30,s,1])
#  points(Armador_noins[11:30,s,1],pch=3)
