
##########################################
#                                        #
#  Relative rates through time plotter   #
#                                        #
##########################################


library(pbapply)
library(BTRTools)
library(ggplot2)
library(phytools)

#tree - BE CARFEUL 
treeA <- treeODONTS
#treeA<-read.nexus("D:/BayesTraitsV2/hackett250.nex")
#Post prob data
#rjpp.output<-rjpp("D:/BayesTraitsV2/rostrumpcsscaledhack.txt.PP.txt","D:/BayesTraitsV2/rostrumpcsscaledhack.txt.PP.trees",treeA)
#rjpp.output<-rjpp("D:/BayesTraitsV2/braincasepcsscaledkappa.txt.PP.txt","D:/BayesTraitsV2/braincasepcsscaledkappa.txt.PP.trees",treeARCHS)
#rjpp.output<-rjpp("D:/BayesTraitsV2/rostrumpcsscaledelt.txt.PP.txt","D:/BayesTraitsV2/rostrumpcsscaleddelt.txt.PP.trees",treeA)

rttplotter<-function(rjpp.output,treeA){

pprates<-rjpp.output$data$meanRate[-1]

scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]

H1<-nodeHeights(treeA)
timedepth<-round(max(H1))
rate.at.time<-vector()
range.at.time<-vector()
twentyfifth.at.time<-vector()
seventyfifth.at.time<-vector()

for (i in 1:timedepth){
  #slice at time of timebin:
  spot<-i
  edges<-which(H1[,2]>spot&H1[,1]<spot)
  rate.at.time[i]<-mean(scaled.edges[edges])
  twentyfifth.at.time[i]<-mean(scaled.25[edges])
  seventyfifth.at.time[i]<-mean(scaled.75[edges])
  
}

df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]), top25=twentyfifth.at.time[-timedepth], top75=seventyfifth.at.time[-timedepth])

z <- ggplot(df2, aes(x = mya, y = rate)) + geom_line(color = "dodgerblue",size=2) + geom_ribbon(aes(ymin=top25, ymax=top75, x=mya), fill = "blue", alpha = 0.3)
z
}

rttplotter<-function(rjpp.output,treeA){
  
  pprates<-rjpp.output$data$meanRate[-1]
  
  scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
  scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
  scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
  
  H1<-nodeHeights(treeA)
  timedepth<-round(max(H1))
  rate.at.time<-vector()
  range.at.time<-vector()
  twentyfifth.at.time<-vector()
  seventyfifth.at.time<-vector()
  
  for (i in 1:timedepth){
    #slice at time of timebin:
    spot<-i
    edges<-which(H1[,2]>spot&H1[,1]<spot)
    rate.at.time[i]<-mean(scaled.edges[edges])
    twentyfifth.at.time[i]<-mean(scaled.25[edges])
    seventyfifth.at.time[i]<-mean(scaled.75[edges])
    
  }
  
  
  
  df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]), top25=twentyfifth.at.time[-timedepth], top75=seventyfifth.at.time[-timedepth])
  
  z <- ggplot(df2, aes(x = mya, y = rate)) + geom_line(color = "dodgerblue",size=2) + geom_ribbon(aes(ymin=top25, ymax=top75, x=mya), fill = "blue", alpha = 0.3)
  z
}

rttplotter<-function(rjpp.output,treeA){
  
  pprates<-rjpp.output$data$meanRate[-1]
  
  scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
  scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
  scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
  
  H1<-nodeHeights(treeA)
  timedepth<-round(max(H1))
  rate.at.time<-vector()
  range.at.time<-vector()
  twentyfifth.at.time<-vector()
  seventyfifth.at.time<-vector()
  
  for (i in 1:timedepth){
    #slice at time of timebin:
    spot<-i
    edges<-which(H1[,2]>spot&H1[,1]<spot)
    rate.at.time[i]<-mean(scaled.edges[edges])
    twentyfifth.at.time[i]<-mean(scaled.25[edges])
    seventyfifth.at.time[i]<-mean(scaled.75[edges])
    
  }
  
  
  
  df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]), top25=twentyfifth.at.time[-timedepth], top75=seventyfifth.at.time[-timedepth])
  
  z <- ggplot(df2, aes(x = mya, y = rate)) + geom_line(color = "dodgerblue",size=2) + geom_ribbon(aes(ymin=top25, ymax=top75, x=mya), fill = "blue", alpha = 0.3)
  z
}

rttplotter2<-function(rjpp.output,treeA,module.name){
  
  #pprates<-rjpp.output$data$meanRate[-1]/mean(rjpp.output$data$meanRate[-1])
  pprates<-rjpp.output$data$meanRate[-1]
  
  #scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
  #scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
  #scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
  
  H1<-nodeHeights(treeA)
  timedepth<-round(max(H1))
  rate.at.time<-vector()

  
  for (i in 1:timedepth){
    #slice at time of timebin:
    spot<-i
    edges<-which(H1[,2]>spot&H1[,1]<spot)
    rate.at.time[i]<-mean(pprates[edges])
    
  }
  rate.at.time<-rate.at.time[1:timedepth-1]/mean(rate.at.time[1:timedepth-1])
  
  
  
  df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]))
  colnames(df2)[2]<-module.name
  return(df2)
}



########################################
########################################
#here is the part where you actually plot
########################################
########################################
#rostrumres, vaultres, etc are the output you got from rjpp in the other 
#plotting script



basisphen_BTraits<-BTRTools::rjpp(rjlog = "basisphen_var.txt.VarRates.txt",
                                 rjtrees = "basisphen_var.txt.Output.trees", 
                                 tree = treeA)


basiocc_BTraits<-BTRTools::rjpp(rjlog = "basiocc_var.txt.VarRates.txt",
                                  rjtrees = "basiocc_var.txt.Output.trees", 
                                  tree = treeA)


frontal_BTraits<-BTRTools::rjpp(rjlog = "frontal_var.txt.VarRates.txt",
                                rjtrees = "frontal_var.txt.Output.trees", 
                                tree = treeA)


jug_BTraits<-BTRTools::rjpp(rjlog = "jug_var.txt.VarRates.txt",
                                rjtrees = "jug_var.txt.Output.trees", 
                                tree = treeA)


mandp_BTraits<-BTRTools::rjpp(rjlog = "mandp_var.txt.VarRates.txt",
                                rjtrees = "mandp_var.txt.Output.trees", 
                                tree = treeA)


max_BTraits<-BTRTools::rjpp(rjlog = "max_var.txt.VarRates.txt",
                                rjtrees = "max_var.txt.Output.trees", 
                                tree = treeA)


nas_BTraits<-BTRTools::rjpp(rjlog = "nas_var.txt.VarRates.txt",
                                rjtrees = "nas_var.txt.Output.trees", 
                                tree = treeA)


occipcon_BTraits<-BTRTools::rjpp(rjlog = "occipcon_var.txt.VarRates.txt",
                                rjtrees = "occipcon_var.txt.Output.trees", 
                                tree = treeA)


pal_BTraits<-BTRTools::rjpp(rjlog = "pal_var.txt.VarRates.txt",
                                 rjtrees = "pal_var.txt.Output.trees", 
                                 tree = treeA)


parietal_BTraits<-BTRTools::rjpp(rjlog = "parietal_var.txt.VarRates.txt",
                                  rjtrees = "parietal_var.txt.Output.trees", 
                                  tree = treeA)



premax_BTraits<-BTRTools::rjpp(rjlog = "premax_var.txt.VarRates.txt",
                               rjtrees = "premax_var.txt.Output.trees", 
                               tree = treeA)



pteryg_BTraits<-BTRTools::rjpp(rjlog = "pteryg_var.txt.VarRates.txt",
                                 rjtrees = "pteryg_var.txt.Output.trees", 
                                 tree = treeA)


squar_BTraits<-BTRTools::rjpp(rjlog = "squar_var.txt.VarRates.txt",
                                 rjtrees = "squar_var.txt.Output.trees", 
                                 tree = treeA)


supocc_BTraits<-BTRTools::rjpp(rjlog = "supocc_var.txt.VarRates.txt",
                               rjtrees = "supocc_var.txt.Output.trees", 
                               tree = treeA)



zygo_BTraits<-BTRTools::rjpp(rjlog = "zygo_var.txt.VarRates.txt",
                                 rjtrees = "zygo_var.txt.Output.trees", 
                                 tree = treeA)


zygo_squa_BTraits<-BTRTools::rjpp(rjlog = "zygo_squa_var.txt.VarRates.txt",
                                 rjtrees = "zygo_squa_var.txt.Output.trees", 
                                 tree = treeA)



whole_BTraits<-BTRTools::rjpp(rjlog = "whole_var.txt.VarRates.txt",
                              rjtrees = "whole_var.txt.Output.trees", 
                              tree = treeA)


bs<-rttplotter2(rjpp.output = basisphen_BTraits, treeA = treeA, module.name = "basisphenoid")
bo<-rttplotter2(rjpp.output = basiocc_BTraits, treeA = treeA, module.name = "basioccipital")
fr<-rttplotter2(rjpp.output = frontal_BTraits, treeA = treeA, module.name = "frontal")
jg<-rttplotter2(rjpp.output = jug_BTraits, treeA = treeA, module.name = "jug")
mp<-rttplotter2(rjpp.output = mandp_BTraits, treeA = treeA, module.name = "mandibualr process")
mx<-rttplotter2(rjpp.output = max_BTraits, treeA = treeA, module.name = "maxilla")
na<-rttplotter2(rjpp.output = nas_BTraits, treeA = treeA, module.name = "nasal")
oc<-rttplotter2(rjpp.output = occipcon_BTraits, treeA = treeA, module.name = "occipital")
pa<-rttplotter2(rjpp.output = pal_BTraits, treeA = treeA, module.name = "palate")
pl<-rttplotter2(rjpp.output = parietal_BTraits, treeA = treeA, module.name = "parietal")
pr<-rttplotter2(rjpp.output = premax_BTraits, treeA = treeA, module.name = "premaxilla")
pt<-rttplotter2(rjpp.output = pteryg_BTraits, treeA = treeA, module.name = "pterygoid")
sq<-rttplotter2(rjpp.output = squar_BTraits, treeA = treeA, module.name = "squamosal")
so<-rttplotter2(rjpp.output = supocc_BTraits, treeA = treeA, module.name = "supraoccipital")
zg<-rttplotter2(rjpp.output = zygo_BTraits, treeA = treeA, module.name = "zygomatic")
zs<-rttplotter2(rjpp.output = zygo_squa_BTraits, treeA = treeA, module.name = "squamosal (with zygomatic process)")
#wo<-rttplotter2(rjpp.output = whole_BTraits, treeA = treeA, module.name = "whole")

#you can get the age of any node using the branching times function 
branching.times(treeARCHS)
#remember the counting rules for nodes (above) So the node numbered #tips + 1 is the root node

node_ages=as.data.frame(branching.times(treeARCHS))

node_ages[row.names(node_ages) == findMRCA(treeARCHS, c(treeARCHS$tip.label[1],treeARCHS$tip.label[5])),]

dud<-join_all(list(mx,pr,fr,jg), by="mya")

library(plyr)
library(dplyr)
library(reshape2)
df3<-join_all(list(bs, bo, fr, jg, mp, mx, na, oc, pa, pl, pr, pt, so, sq, zg, zs), by="mya")
ratedata<-melt(df3, id.vars = "mya")

ggplot(ratedata, aes(x = mya, y = value, colour=variable)) + geom_line(size=2) +
  scale_color_manual(values=c("lightblue","darkgreen","pink","coral", "brown","lightgreen",
                              "red","turquoise","yellow","black","darkblue","purple",
                              "orange", "grey", "grey", "darkred")) +
  theme_classic()+
  #geom_hline(yintercept = 1)+
  #geom_vline(xintercept = -50)+
  scale_x_continuous(breaks = seq(-50 , 0, 2)) +
  scale_y_continuous(limit = c(0.2, 6))



#All rates for the skulls 
ggplot() +
  geom_line(data = whole_ARCHS, aes(x = mya, y = rates)) + geom_line(size=2) + # must include argument label "data"
  geom_line(data = whole_MYSTS, aes(x = mya, y = rates)) + geom_line(size=2) +
  geom_line(data = whole_ODONTS, aes(x = mya, y = rates)) + geom_line(size=2) +
  scale_color_manual(values=c("palegreen","#D55E00","#6A3D9A")) +
  scale_x_continuous(breaks = seq(-50 , 0, 5)) +
  scale_y_continuous(limit = c(0.2, 3)) +
  theme_classic()




spheres3d(final_mirrored_odonts[nas,,17], radius = 2, color = "red")
spheres3d(final_mirrored_odonts[premax,,17], radius =  2, color = "darkblue")
spheres3d(final_mirrored_odonts[maxilla,,17], radius =2, color = "lightgreen")
spheres3d(final_mirrored_odonts[frontal,,17], radius =  2, color = "pink")
spheres3d(final_mirrored_odonts[pteryg,,17], radius =  2, color = "purple")
spheres3d(final_mirrored_odonts[pal,,17], radius =  2, color = "yellow")
spheres3d(final_mirrored_odonts[supocc,,17], radius =  2, colour = "orange")
spheres3d(final_mirrored_odonts[basiocc,,17], radius =  2, color = "lightblue")
spheres3d(final_mirrored_odonts[basisphen,,17], radius =  2, color = "darkgreen")
spheres3d(final_mirrored_odonts[mandp,,17], radius =  2, color = "brown")
spheres3d(final_mirrored_odonts[jug,17], radius =  2, color = "coral")
spheres3d(final_mirrored_odonts[occipcon,,17], radius =  2, color = "turquoise")
spheres3d(final_mirrored_odonts[parietal,,17], radius =  2, color = "black")
spheres3d(final_mirrored_odonts[zygo_squa,,17], radius = 2, color = "darkred")
#spheres3d(shapedata[zygo,,42], radius =  0.0002, color = "magenta")



