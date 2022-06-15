# This is an R script to plot output from log files of GeoHiSSE/MuSSE/MuHiSSE analyses
# presented in McCullough et al. 2022 systematic biology. 

# written by Jenna McCullough and Rosana Zenil-Ferguson 
# last edited on 15 June 2022 by Jenna McCullough 




library(devtools)
install_github("revbayes/RevGadgets")
library(RevGadgets)
library(ggplot2)

#multiplot.R needs to be in your working directory for this to work. 
source("multiplot.R")


#Remember, colors need to be in alphabetical order of your parameters!!!!!

###########################################
###########################################
# Model 1 ANALYSIS: GEOHISSE ISLANDS
###########################################
###########################################
islands_1.sse<-read.table("01_islands/geohisse-islands-1mil-25burnin-resamp75.log", header=TRUE)
head(islands_1.sse)
str(islands_1.sse)

#anagenetic dispersal for model 1 
anagenetic.disp_1<-data.frame(dens=c(islands_1.sse$anagenetic_dispersal_21, islands_1.sse$anagenetic_dispersal_31, islands_1.sse$anagenetic_dispersal_54, islands_1.sse$anagenetic_dispersal_64),Type=rep(c("Continent to widespread A","Island to widespread A","Continent to widespread B","Island to widespread B"), each=length(islands_1.sse$anagenetic_dispersal_21)))
anagenetic.colors=c("#336600","#330033","#66CC66","#996699") 

#Continents A = #336600 
#Islands A = #66CC66 
#Continents B = #330033
#Islands B = #996699 

p1_1<-ggplot(anagenetic.disp_1, aes(x=dens, fill=Type))+labs(title="Anagenetic dispersal for Islands v. continents",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =anagenetic.colors )+xlim(0,1)+ylim(0,75)
p1_1

#Extinction for model 1 
extinction_1<-data.frame(dens=c(islands_1.sse$extinction_rates.2.,islands_1.sse$extinction_rates.3., islands_1.sse$extinction_rates.5., islands_1.sse$extinction_rates.6.),Type=rep(c("Continents A", "Islands A", "Continents B", "Islands B"), each=length(islands_1.sse$extinction_rates.5.)))
extinction.colors=c("#336600","#330033","#66CC66","#996699") 
p2_1<-ggplot(extinction_1, aes(x=dens, fill=Type))+labs(title="Extinction for islands v. contients",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = extinction.colors )+xlim(0,1.3)
p2_1

#Diversification for model 1 

# islands A : speciation_rates.2. + speciation_rates.5. + speciation_rates.6. + speciation_rates.7. + speciation_rates.8.  - extinction_rates.3.
# islands B : speciation_rates.10. + speciation_rates.13. + speciation_rates.14. + speciation_rates.15. + speciation_rates.16. - extinction_rates.6.
# continents A : speciation_rates.1. + speciation_rates.3. + speciation_rates.4. + speciation_rates.7. + speciation_rates.8. - extinction_rates.2.
# continents B : speciation_rates.9. + speciation_rates.11. + speciation_rates.12. + speciation_rates.15. + speciation_rates.16. - extinction_rates.5.
diversification_1<-data.frame(dens=c(islands_1.sse$speciation_rates.2. + islands_1.sse$speciation_rates.5. + islands_1.sse$speciation_rates.6. + islands_1.sse$speciation_rates.7. + islands_1.sse$speciation_rates.8.  - islands_1.sse$extinction_rates.3.,
                                              islands_1.sse$speciation_rates.1. + islands_1.sse$speciation_rates.3. + islands_1.sse$speciation_rates.4. + islands_1.sse$speciation_rates.7. + islands_1.sse$speciation_rates.8. - islands_1.sse$extinction_rates.2.,
                                              islands_1.sse$speciation_rates.9. + islands_1.sse$speciation_rates.11. + islands_1.sse$speciation_rates.12. + islands_1.sse$speciation_rates.15. + islands_1.sse$speciation_rates.16. - islands_1.sse$extinction_rates.5.,
                                              islands_1.sse$speciation_rates.10. + islands_1.sse$speciation_rates.13. + islands_1.sse$speciation_rates.14. + islands_1.sse$speciation_rates.15. + islands_1.sse$speciation_rates.16.  - islands_1.sse$extinction_rates.6.),
                                       Type=rep(c("Islands A", "Continents A", "Continents B", "Islands B" ), each=length(islands_1.sse$speciation_rates.1.)))

diversification.colors=c("#336600","#330033","#66CC66","#996699")
p3_1<-ggplot(diversification_1, aes(x=dens, fill=Type))+labs(title="Diversification of Islands v. contients",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,1)
p3_1

# uncomment when it's time to make PDFs 
#pdf("final-output/1_ana_ext_div-v3.pdf", width = 5, height = 10)
#multiplot(p1_1, p2_1, p3_1)
#dev.off()
 
 
#hidden rates of model 1
hidden_rates_1<-data.frame(dens=c(islands_1.sse$hidden_rate_1, islands_1.sse$hidden_rate_2),Type=rep(c("Hidden rate 1","Hidden rate 2"), each=length(islands_1.sse$anagenetic_dispersal_21)))
hidden.colors=c("black", "gray") 
p4_1<-ggplot(hidden_rates_1, aes(x=dens, fill=Type))+labs(title="hidden rates of model 1",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = hidden.colors )+xlim(0,2)
p4_1

#pdf("final-output/1_hidden-rates.pdf", width = 5, height = 3.3)
#dev.off()

###########################################
###########################################
# model 2 ANALYSIS: GEOHISSE ISLANDS EXTENDED MODEL 
###########################################
###########################################


islands_2_extended.sse<-read.table("02_islands-extended/geohisse-islands-680K-25burnin-resamp45.log", header=TRUE)
head(islands_2_extended.sse)# just to check what I read
str(islands_2_extended.sse)

#anagenetic dispersal for model 2 
anagenetic.disp_2<-data.frame(dens=c(islands_2_extended.sse$anagenetic_dispersal_21, islands_2_extended.sse$anagenetic_dispersal_31, islands_2_extended.sse$anagenetic_dispersal_54, islands_2_extended.sse$anagenetic_dispersal_64),Type=rep(c("Continent to widespread A","Island to widespread A","Continent to widespread B","Island to widespread B"), each=length(islands_2_extended.sse$anagenetic_dispersal_21)))
anagenetic.colors=c("#336600","#330033","#66CC66","#996699") 

p1_2<-ggplot(anagenetic.disp_2, aes(x=dens, fill=Type))+labs(title="Anagenetic dispersal for Islands v. continents extended",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =anagenetic.colors )+xlim(0,1.3)+ylim(0,75)
p1_2

#Extinction for model 2 
extinction_2<-data.frame(dens=c(islands_2_extended.sse$extinction_rates.2.,islands_2_extended.sse$extinction_rates.3., islands_2_extended.sse$extinction_rates.5., islands_2_extended.sse$extinction_rates.6.),Type=rep(c("Continents A", "Islands A", "Continents B", "Islands B"), each=length(islands_2_extended.sse$extinction_rates.5.)))
extinction.colors=c("#336600","#330033","#66CC66","#996699") 
p2_2<-ggplot(extinction_2, aes(x=dens, fill=Type))+labs(title="Extinction for islands v. continents Extended",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = extinction.colors )+xlim(0,1.3)
p2_2


# Diversification for  model 2 
# islands A : speciation_2 + speciation_5 + speciation_7 - extinction_rates.3.
# islands B : speciation_10 +speciation_13 + speciation_15 - extinction_rates.6.
# continents A : speciation_1 + speciation_3 + speciation_7 - extinction_rates.2.
# continents B : speciation_9 + speciation_11 + speciation_15 - extinction_rates.5.

diversification_2 <- data.frame(dens=c(islands_2_extended.sse$speciation_2 + islands_2_extended.sse$speciation_5 + islands_2_extended.sse$speciation_7 - islands_2_extended.sse$extinction_rates.3.,
                                       islands_2_extended.sse$speciation_10 + islands_2_extended.sse$speciation_13 + islands_2_extended.sse$speciation_15 - islands_2_extended.sse$extinction_rates.6.,
                                       islands_2_extended.sse$speciation_1 + islands_2_extended.sse$speciation_3 + islands_2_extended.sse$speciation_7 - islands_2_extended.sse$extinction_rates.2.,
                                       islands_2_extended.sse$speciation_9 + islands_2_extended.sse$speciation_11 + islands_2_extended.sse$speciation_15 - islands_2_extended.sse$extinction_rates.5.),
                                Type=rep(c("Islands A", "Islands B", "Continents A", "Continents B"), each=length(islands_2_extended.sse$extinction_rates.3.)))

diversification.colors=c("#336600","#330033","#66CC66","#996699")
p3_2<-ggplot(diversification_2, aes(x=dens, fill=Type))+labs(title="diversification of Islands v. continents extended model",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,5)
p3_2

# comparing types of speciation in islands v. continents  for model 2 in extended speciations 
# Continents A					    	# islands A
# widespread sympatry = 1 		# widespread sympatry = 2
# subset sympatry = 3				  # subset sympatry = 5
# allopatric = 7				    	# allopatric = 7 

# Continents B					  	# islands B
# widespread sympatry = 9 	# widespread sympatry = 10
# subset sympatry = 11			# subset sympatry = 13
# allopatric = 15					  # allopatric = 15 

speciation_2 <- data.frame(dens=c(islands_2_extended.sse$speciation_1, islands_2_extended.sse$speciation_3, islands_2_extended.sse$speciation_7, 
                                  islands_2_extended.sse$speciation_2 , islands_2_extended.sse$speciation_5, islands_2_extended.sse$speciation_7, 
                                  islands_2_extended.sse$speciation_9, islands_2_extended.sse$speciation_11, islands_2_extended.sse$speciation_15,
                                  islands_2_extended.sse$speciation_10, islands_2_extended.sse$speciation_13, islands_2_extended.sse$speciation_15), 
                           Type=rep(c("Continents A WS", "Continents A SS", "Continents A Allo", "Islands A WS", "Islands A SS", "Islands A Allo", "Continents B WS", "Continents B SS", "Continents B Allo",  "Islands B WS", "Islands B SS", "Islands B Allo"), 
                                    each=length(islands_2_extended.sse$speciation_15)))    

p4_2<-ggplot(speciation_2, aes(x=dens, fill=Type))+labs(title="Types of speciation in Islands V. continents Extended",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlim(-0.1,2.25)
p4_2

# make a violin plot 
dp1 <- ggplot(speciation_2, aes(x=Type, y=dens,fill=Type)) + geom_violin(trim=FALSE)+
  labs(title="Types of speciation in Islands V. continents Ext",x="State", y = "Posterior Density")
dp1<-dp1 + theme_classic() +ylim(0,2)
dp1

#pdf("final-output/2_speciation-types.pdf", width = 4, height = 3)
#dev.off()

#pdf("final-output/2_ana_ext_div-v2.pdf", width = 5, height = 10)
#multiplot(p1_2, p2_2, p3_2)
#dev.off()
# 

# hidden rates of model 2 
hidden_rates_2a<-data.frame(dens=c(islands_2_extended.sse$hidden_rate_1, islands_2_extended.sse$hidden_rate_2),Type=rep(c("Hidden rate 1","Hidden rate 2"), each=length(islands_2_extended.sse$anagenetic_dispersal_21)))

p4_2_hidden<-ggplot(hidden_rates_2a, aes(x=dens, fill=Type))+labs(title="hidden rates of model 2",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = hidden.colors )+scale_x_continuous(limits=c(0,2.5))
p4_2_hidden


#pdf("final-output/2_hidden-rates.pdf", width = 5, height = 3.3)
#dev.off()



###########################################
###########################################
# THIRD ANALYSIS: GEOHISSE WALLACE'S LINE 
###########################################
###########################################

WL_3.sse<-read.table("03_wallaces-line/geohisse-islands-780K-25burnin-resamp55.log", header=TRUE)
head(WL_3.sse)# just to check what I read
str(WL_3.sse)
#anagenetic dispersal for model 3 
anagenetic.disp_3<-data.frame(dens=c(WL_3.sse$anagenetic_dispersal_21, WL_3.sse$anagenetic_dispersal_31, WL_3.sse$anagenetic_dispersal_54, WL_3.sse$anagenetic_dispersal_64),Type=rep(c("East Line to widespread A","West of line to widespread A","East of Line to widespread B","West of Line to widespread B"), each=length(WL_3.sse$anagenetic_dispersal_21)))
anagenetic.colors=c("#336600","#330033","#66CC66","#996699") 

p1_3<-ggplot(anagenetic.disp_3, aes(x=dens, fill=Type))+labs(title="Anagenetic dispersal for East V. West Wallace's Line",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =anagenetic.colors )+ylim(0,50)+xlim(0,0.6)
p1_3
#This looks weird because A is really tiny and B is not. Feel free to play around with y axis  


#Extinction for model 3 
extinction_3<-data.frame(dens=c(WL_3.sse$extinction_rates.2.,WL_3.sse$extinction_rates.3., WL_3.sse$extinction_rates.5., WL_3.sse$extinction_rates.6.),Type=rep(c("EAST A", "WEST A", "EAST B", "WEST B"), each=length(WL_3.sse$extinction_rates.5.)))
extinction.colors=c("#336600","#330033","#66CC66","#996699") 


p2_3<-ggplot(extinction_3, aes(x=dens, fill=Type))+labs(title="Extinction for EAST v. WEST",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlim(0,1.3) +scale_fill_manual( values = extinction.colors ) 

p2_3

#Diversification for model 3 
# East A : speciation_rates.1. + speciation_rates.3. + speciation_rates.4. + speciation_rates.7. + speciation_rates.8. - extinction_rates.2.
# East B : speciation_rates.9. + speciation_rates.11. + speciation_rates.12. + speciation_rates.15. + speciation_rates.16. - extinction_rates.5.
# West A : speciation_rates.2. + speciation_rates.5. + speciation_rates.6. + speciation_rates.7. + speciation_rates.8.  - extinction_rates.3.
# West B : speciation_rates.10. + speciation_rates.13. + speciation_rates.14. + speciation_rates.15. + speciation_rates.16. - extinction_rates.6.

diversification_3<-data.frame(dens=c(WL_3.sse$speciation_rates.2. + WL_3.sse$speciation_rates.5. + WL_3.sse$speciation_rates.6. + WL_3.sse$speciation_rates.7. + WL_3.sse$speciation_rates.8.  - WL_3.sse$extinction_rates.3.,
                                     WL_3.sse$speciation_rates.1. + WL_3.sse$speciation_rates.3. + WL_3.sse$speciation_rates.4. + WL_3.sse$speciation_rates.7. + WL_3.sse$speciation_rates.8. - WL_3.sse$extinction_rates.2.,
                                     WL_3.sse$speciation_rates.9. + WL_3.sse$speciation_rates.11. + WL_3.sse$speciation_rates.12. + WL_3.sse$speciation_rates.15. + WL_3.sse$speciation_rates.16. - WL_3.sse$extinction_rates.5.,
                                     WL_3.sse$speciation_rates.10. + WL_3.sse$speciation_rates.13. + WL_3.sse$speciation_rates.14. + WL_3.sse$speciation_rates.15. + WL_3.sse$speciation_rates.16.  - WL_3.sse$extinction_rates.6.),
                              Type=rep(c("West A", "East A", "East B", "West B" ), each=length(WL_3.sse$speciation_rates.1.)))

diversification.colors=c("#336600","#330033","#66CC66","#996699")

p3_3<-ggplot(diversification_3, aes(x=dens, fill=Type))+labs(title="diversification of East V. west",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,1.5)

p3_3

#pdf("final-output/3_ana_ext_div.pdf", width =5, height = 10)
#multiplot(p1_3, p2_3, p3_3)
#dev.off()
#

# plotting hidden rates for model 3 
hidden_rates_3<-data.frame(dens=c(WL_3.sse$hidden_rate_1, WL_3.sse$hidden_rate_2),Type=rep(c("Hidden rate 1","Hidden rate 2"), each=length(WL_3.sse$anagenetic_dispersal_21)))
p4_3<-ggplot(hidden_rates_3, aes(x=dens, fill=Type))+labs(title="hidden rates of model 3",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = hidden.colors )+scale_x_continuous(limits=c(0,2))
p4_3

#pdf("final-output/3_hidden-rates.pdf", width = 5, height = 3.3)
#dev.off()



###########################################
###########################################
# FOURTH ANALYSIS: GEOHISSE WALLACE'S LINE EXTENDED MODEL 
###########################################
###########################################


WL_4_extended.sse<-read.table("04_wallaces-line-extended/geohisse-islands-714K-25burnin-resamp50.log", header=TRUE)
head(WL_4_extended.sse)# just to check what I read
str(WL_4_extended.sse)

#anagenetic dispersal for model 4 
anagenetic.disp_4<-data.frame(dens=c(WL_4_extended.sse$anagenetic_dispersal_21, WL_4_extended.sse$anagenetic_dispersal_31, WL_4_extended.sse$anagenetic_dispersal_54, WL_4_extended.sse$anagenetic_dispersal_64),Type=rep(c("East to widespread A","West to widespread A","East to widespread B","West to widespread B"), each=length(WL_4_extended.sse$anagenetic_dispersal_21)))
anagenetic.colors=c("#336600","#330033","#66CC66","#996699") 

p1_4<-ggplot(anagenetic.disp_4, aes(x=dens, fill=Type))+labs(title="Anagenetic dispersal for East v. West extended",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =anagenetic.colors )+xlim(0,0.5)+ylim(0,100)
p1_4
# A is much narrower than B 

#Extinction for model 4
extinction_4<-data.frame(dens=c(WL_4_extended.sse$extinction_rates.2.,WL_4_extended.sse$extinction_rates.3., WL_4_extended.sse$extinction_rates.5., WL_4_extended.sse$extinction_rates.6.),Type=rep(c("east A", "West A", "east B", "West B"), each=length(WL_4_extended.sse$extinction_rates.5.)))
extinction.colors=c("#336600","#330033","#66CC66","#996699") 
p2_4<-ggplot(extinction_4, aes(x=dens, fill=Type))+labs(title="Extinction for East v. west Extended model",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = extinction.colors )+xlim(0,1.3)
p2_4


# Diversification for  model 2 
# West A : speciation_2 + speciation_5 + speciation_7 - extinction_rates.3.
# West B : speciation_10 +speciation_13 + speciation_15 - extinction_rates.6.
# east A : speciation_1 + speciation_3 + speciation_7 - extinction_rates.2.
# east B : speciation_9 + speciation_11 + speciation_15 - extinction_rates.5.

diversification_4 <- data.frame(dens=c(WL_4_extended.sse$speciation_2 + WL_4_extended.sse$speciation_5 + WL_4_extended.sse$speciation_7 - WL_4_extended.sse$extinction_rates.3.,
                                       WL_4_extended.sse$speciation_10 + WL_4_extended.sse$speciation_13 + WL_4_extended.sse$speciation_15 - WL_4_extended.sse$extinction_rates.6.,
                                       WL_4_extended.sse$speciation_1 + WL_4_extended.sse$speciation_3 + WL_4_extended.sse$speciation_7 - WL_4_extended.sse$extinction_rates.2.,
                                       WL_4_extended.sse$speciation_9 + WL_4_extended.sse$speciation_11 + WL_4_extended.sse$speciation_15 - WL_4_extended.sse$extinction_rates.5.),
                                Type=rep(c("West A", "West B", "East A", "East B"), each=length(WL_4_extended.sse$extinction_rates.3.)))

diversification.colors=c("#336600","#330033","#66CC66","#996699")
p3_4<-ggplot(diversification_4, aes(x=dens, fill=Type))+labs(title="Diversification of West v. east extended model",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,5)
p3_4

# comparing types of speciation in West v. east  for model 2 in extended speciations 
# east A					        	# West A
# widespread sympatry = 1 	# widespread sympatry = 2
# subset sympatry = 3				# subset sympatry = 5
# allopatric = 7				    # allopatric = 7 

# east B					        	# West B
# widespread sympatry = 9 	# widespread sympatry = 10
# subset sympatry = 11			# subset sympatry = 13
# allopatric = 15			    	# allopatric = 15 

speciation_4 <- data.frame(dens=c(WL_4_extended.sse$speciation_1, WL_4_extended.sse$speciation_3, WL_4_extended.sse$speciation_7, 
                                  WL_4_extended.sse$speciation_2 , WL_4_extended.sse$speciation_5, WL_4_extended.sse$speciation_7, 
                                  WL_4_extended.sse$speciation_9, WL_4_extended.sse$speciation_11, WL_4_extended.sse$speciation_15,
                                  WL_4_extended.sse$speciation_10, WL_4_extended.sse$speciation_13, WL_4_extended.sse$speciation_15), 
                           Type=rep(c("East A WS", "East A SS", "East A Allo", "West A WS", "West A SS", "West A Allo", "East B WS", "East B SS", "East B Allo",  "West B WS", "West B SS", "West B Allo"), 
                                    each=length(WL_4_extended.sse$speciation_15)))    
p4_4<-ggplot(speciation_4, aes(x=dens, fill=Type))+labs(title="Types of speciation in West V. east Extended",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlim(0,2)
p4_4



# make a violin plot 
dp2 <- ggplot(speciation_4, aes(x=Type, y=dens,fill=Type)) + geom_violin(trim=FALSE)+
  labs(title="Types of speciation in East v. West Ext",x="State", y = "Posterior Density")
dp2<-dp2 + theme_classic() +ylim(0,2)
dp2

#pdf("final-output/4_speciation-types.pdf", width = 4, height = 3)
#dp2
#dev.off()


#
#pdf("final-output/4_ana_ext_div.pdf", width = 5, height = 10)
#multiplot(p1_4, p2_4, p3_4)
#dev.off()


hidden_rates_4<-data.frame(dens=c(WL_4_extended.sse$hidden_rate_1, WL_4_extended.sse$hidden_rate_2),Type=rep(c("Hidden rate 1","Hidden rate 2"), each=length(WL_4_extended.sse$anagenetic_dispersal_21)))
p4_4_hidden<-ggplot(hidden_rates_4, aes(x=dens, fill=Type))+labs(title="hidden rates of model 4",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = hidden.colors )+scale_x_continuous(limits=c(0,2.5))
p4_4_hidden

#pdf("final-output/4_hidden-rates.pdf", width = 5, height = 3.3)
p4_4_hidden
#dev.off()

###########################################
###########################################
# FIFTH ANALYSIS: MUSSE OF COMBINATORIAL EFFECT OF E/W LINE AND ISLANDS VERSUS CONTINENTS 
###########################################
###########################################

#MUSSE combined analyses 
musse_comb.sse<-read.table("05_MuSSE-wallace-islands/MuSSE-540K-25burnin-resamp50.log", header=TRUE)
head(musse_comb.sse)# just to check what I read
str(musse_comb.sse)

# 0 = Islands East of Line
# 1 = Continent East of Line
# 2 = Island West of Line 
# 3 = Continent West of Line 

# Anagenetic dispersal for model 5: MuSSE 
anagenetic.disp_5<-data.frame(dens=c(musse_comb.sse$rate_01, musse_comb.sse$rate_02, musse_comb.sse$rate_03, musse_comb.sse$rate_10, musse_comb.sse$rate_12, musse_comb.sse$rate_13, musse_comb.sse$rate_20, musse_comb.sse$rate_21, musse_comb.sse$rate_23, musse_comb.sse$rate_30, musse_comb.sse$rate_31, musse_comb.sse$rate_32), Type=rep(c("0 to 1", "0 to 2", "0 to 3", "1 to 0", "1 to 2", "1 to 3", "2 to 0", "2 to 1", "2 to 3", "3 to 0", "3 to 1", "3 to 2"), each=length(musse_comb.sse$rate_32)))

anagenetic.colors=c("#336600","#330033","#66CC66","#996699", 'red', "orange", "blue","black") 
p1_5<-ggplot(anagenetic.disp_5, aes(x=dens, fill=Type))+labs(title="Anagenetic dispersal for MuSSE",x="Rate", y="Posterior Density")+geom_density(alpha=0.7) +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(0,500)
p1_5

# make a violin plot of this
p1_5_violin<- ggplot(anagenetic.disp_5, aes(x=Type, y=dens,fill=Type)) + geom_violin(trim=FALSE)+
  labs(title=" Anagenetic dispersal MuSSE",x="State", y = "Posterior Density") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

p1_5_violin


#Extinction for model 5: MuSSE 
extinction_5 <-data.frame(dens=c(musse_comb.sse$extinction.1., musse_comb.sse$extinction.2., musse_comb.sse$extinction.3., musse_comb.sse$extinction.4.), Type=rep(c("East Islands", "East Continents", "West Islands", "West Continents"), each=length(musse_comb.sse$extinction.4.)))
  
extinction.colors=c("#336600","#330033","#66CC66","#996699") 
p2_5<-ggplot(extinction_5, aes(x=dens, fill=Type))+labs(title="Extinction for MuSSE",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    scale_fill_manual( values = extinction.colors ) +
    xlim(0,1.3)
p2_5

#diversification for model 5: MuSSE
# 0 = Islands East of Line = speciation.1. - extinction.1.
# 1 = Continent East of Line = speciation.2. - extinction.2.
# 2 = Island West of Line = speciation.3. - extinction.3.
# 3 = Continent West of Line = speciation.4. - extinction.4.

diversification_5 <- data.frame(dens=c(musse_comb.sse$speciation.1. - musse_comb.sse$extinction.1., 
                                       musse_comb.sse$speciation.2. - musse_comb.sse$extinction.2., 
                                       musse_comb.sse$speciation.3. - musse_comb.sse$extinction.3., 
                                       musse_comb.sse$speciation.4. - musse_comb.sse$extinction.4.), 
                                Type=rep(c("East Islands", "East Continents", "West Islands", "West Continents"), each=length(musse_comb.sse$speciation.4.)))

diversification.colors=c("#336600","#330033","#66CC66","#996699")
p3_5<-ggplot(diversification_5, aes(x=dens, fill=Type))+labs(title="diversification MuSSE",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,0.5)
  
p3_5

#pdf("final-output/5_p1_5_violin.pdf", width = 10, height = 10)
#multiplot(p1_5_violin)
#dev.off()
#

###########################################
###########################################
# SIXTH ANALYSIS: MUHISSE OF COMBINATORIAL EFFECT OF E/W LINE AND ISLANDS VERSUS CONTINENTS 
###########################################
###########################################

# FOR THE RECORD: the alpha and betas in the trace file names are not to be interpretted.
#  it's not what you think! don't be fooled. 

#MUHiSSE combined analyses 
muhisse_comb.sse<-read.table("06_MuHiSSE-wallace-islands/MuHiSSE-600K-25burnin-resamp50.log", header=TRUE)
head(muhisse_comb.sse)# just to check what I read
str(muhisse_comb.sse)

# Anagenetic dispersal for model 6: MuHiSSE 
anagenetic.colors=c("#336600","#330033","#66CC66","#996699", 'red', "orange", "blue","black") 
anagenetic.disp_6 <- data.frame(dens=c(muhisse_comb.sse$rate_01, muhisse_comb.sse$rate_02, muhisse_comb.sse$rate_03, muhisse_comb.sse$rate_10, muhisse_comb.sse$rate_12, muhisse_comb.sse$rate_13, muhisse_comb.sse$rate_20, muhisse_comb.sse$rate_21, muhisse_comb.sse$rate_23, muhisse_comb.sse$rate_30, muhisse_comb.sse$rate_31, muhisse_comb.sse$rate_32), Type=rep(c("0 to 1", "0 to 2", "0 to 3", "1 to 0", "1 to 2", "1 to 3", "2 to 0", "2 to 1", "2 to 3", "3 to 0", "3 to 1", "3 to 2"), each=length(muhisse_comb.sse$rate_32)))
  
p1_6<-ggplot(anagenetic.disp_6, aes(x=dens, fill=Type))+labs(title="Anagenetic dispersal for MuHiSSE",x="Rate", y="Posterior Density")+geom_density(alpha=0.7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(0,500)
p1_6

# make a violin plot of this
p1_6_violin<- ggplot(anagenetic.disp_6, aes(x=Type, y=dens,fill=Type)) + geom_violin(trim=FALSE)+
  labs(title=" Anagenetic dispersal MuHiSSE",x="State", y = "Posterior Density") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

p1_6_violin


#Extinction for model 6: MuHiSSE 
extinction.colors=c("#336600","#330033","#66CC66","#996699", 'red', "orange", "blue","black") 
extinction_6 <-data.frame(dens=c(muhisse_comb.sse$extinction.1., muhisse_comb.sse$extinction.2., muhisse_comb.sse$extinction.3., muhisse_comb.sse$extinction.4., muhisse_comb.sse$extinction.5., muhisse_comb.sse$extinction.6., muhisse_comb.sse$extinction.7., muhisse_comb.sse$extinction.8.), Type=rep(c("East Islands A", "East Continents A", "West Islands A", "West Continents A", "East Islands B", "East Continents B", "West Islands B", "West Continents B"), each=length(muhisse_comb.sse$extinction.4.)))

# 8 extinctions parameters. 
#1 = 0A
#2 = 1A
#3 = 2A
#4 = 3A
#5 = 0B
#6 = 1B
#7 = 2B
#8 = 3B 

p2_6<-ggplot(extinction_6, aes(x=dens, fill=Type))+labs(title="Extinction for MuHiSSE",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_fill_manual( values = extinction.colors ) +
  xlim(-0.05,0.3) + ylim(0,60)
p2_6


#diversification for model 6: MuHiSSE NO ALPHAS
# 0A = Islands East of Line = speciation.1. - extinction.1.
# 1A = Continent East of Line = speciation.2. - extinction.2.
# 2A = Island West of Line = speciation.3. - extinction.3.
# 3A = Continent West of Line = speciation.4. - extinction.4.
# 0B = Islands East of Line = speciation.5. - extinction.5.
# 1B = Continent East of Line = speciation.6. - extinction.6.
# 2B = Island West of Line = speciation.7. - extinction.7.
# 3B = Continent West of Line = speciation.8. - extinction.8.

diversification_6 <- data.frame(dens=c(muhisse_comb.sse$speciation.1. - muhisse_comb.sse$extinction.1., 
                                       muhisse_comb.sse$speciation.2. - muhisse_comb.sse$extinction.2., 
                                       muhisse_comb.sse$speciation.3. - muhisse_comb.sse$extinction.3., 
                                       muhisse_comb.sse$speciation.4. - muhisse_comb.sse$extinction.4., 
                                       muhisse_comb.sse$speciation.5. - muhisse_comb.sse$extinction.5.,
                                       muhisse_comb.sse$speciation.6. - muhisse_comb.sse$extinction.6.,
                                       muhisse_comb.sse$speciation.7. - muhisse_comb.sse$extinction.7.,
                                       muhisse_comb.sse$speciation.8. - muhisse_comb.sse$extinction.8.), 
                                Type=rep(c("East Islands A", "East Continents A", "West Islands A", "West Continents A",
                                           "East Islands B", "East Continents B", "West Islands B", "West Continents B"),
                                         each=length(muhisse_comb.sse$speciation.4.)))

p3_6 <-ggplot(diversification_6, aes(x=dens, fill=Type))+labs(title="diversification MuHiSSE",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,0.75)

p3_6


# look at differences within East versus West 
diversification_6_east <- data.frame(dens=c(muhisse_comb.sse$speciation.1. - muhisse_comb.sse$extinction.1., 
                                       muhisse_comb.sse$speciation.2. - muhisse_comb.sse$extinction.2., 
                                       muhisse_comb.sse$speciation.5. - muhisse_comb.sse$extinction.5.,
                                       muhisse_comb.sse$speciation.6. - muhisse_comb.sse$extinction.6.), 
                                Type=rep(c("East Islands A", "East Continents A", 
                                           "East Islands B", "East Continents B"),
                                         each=length(muhisse_comb.sse$speciation.4.)))


diversification.colors=c("#336600","#330033","#66CC66","#996699", 'red', "orange", "blue","black")

p3_6_east<-ggplot(diversification_6_east, aes(x=dens, fill=Type))+labs(title="diversification MuHiSSE of East only",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,0.75)

p3_6_east

diversification_6_west <- data.frame(dens=c(muhisse_comb.sse$speciation.3. - muhisse_comb.sse$extinction.3., 
                                            muhisse_comb.sse$speciation.4. - muhisse_comb.sse$extinction.4., 
                                            muhisse_comb.sse$speciation.7. - muhisse_comb.sse$extinction.7.,
                                            muhisse_comb.sse$speciation.8. - muhisse_comb.sse$extinction.8.), 
                                     Type=rep(c("West Islands A", "West Continents A", 
                                                "West Islands B", "West Continents B"),
                                              each=length(muhisse_comb.sse$speciation.4.)))


diversification.colors=c("#336600","#330033","#66CC66","#996699", 'red', "orange", "blue","black")

p3_6_west<-ggplot(diversification_6_west, aes(x=dens, fill=Type))+labs(title="diversification MuHiSSE of west only",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = diversification.colors )+xlim(0,0.75)

p3_6_west

# the trend we see in MuSSE, that there is more diversification within eastern islands, is a real trend for east of WL. 
# Compare the East continents / east islands A and B. 
p3_6_east
# this shows that B and A of eastern islands show the same pattern, which means that this is a real trend

#Compare the West continents / west islands A and B
p3_6_west
# more overlap between the West continentsA and West islandsA. 
# Because the B states are more overlap is higher means that this is not a real pattern 

#what this means is that islands in the east opperate differently than islands in the west, indicating
# that islands as a whole in the world shouldn't be lumped together 
# moving forward, we should separate out islands east of WL, continents east of WL, and "the west". 

# conclusion: 
# In the west, the diversification is similar to the contients and islands: there's no significant differences. 
# In the east, there is significant differences: different diversification rates and dispersal rates. 

#pdf("final-output/6_ana_ext_div_Ediv_Wdiv.pdf", width = 5, height = 15)
#multiplot(p1_6_violin, p2_6, p3_6, p3_6_west, p3_6_east)
#dev.off()
#

# hidden rates of model 6 
hidden_rates_6<-data.frame(dens=c(muhisse_comb.sse$hidden_rate1, muhisse_comb.sse$hidden_rate2),Type=rep(c("Hidden rate 1","Hidden rate 2"), each=length(muhisse_comb.sse$hidden_rate1)))
p4_6<-ggplot(hidden_rates_6, aes(x=dens, fill=Type))+labs(title="hidden rates of model 6",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = hidden.colors )+scale_x_continuous(limits=c(0,2.5))
p4_6


