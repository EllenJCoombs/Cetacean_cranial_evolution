

################################################
#                                              #
#  Plotting outputs from BTs onto phylogeny    #
#  Code: Ryan Felice                           #
#                                              #
################################################

library(here)
library(phytools)
library(deeptime)
library(tibble)
library(ggtree)
library(plyr)
library(dplyr)
library(ggplot2)
library(BTRTools)

#Run this function first 

#uses treeio to combine tree topology and BayesTraits posterior into single S4 object for plotting.
add_rjpp_to_tree <- function(rjpp_out){
  rjpp_data <- as_tibble(rjpp_out$data)
  timetree <- rjpp_out$meantree
  timetree$edge.length <- rjpp_data$orgBL[-1]
  timetree$root.time <- max(nodeHeights(timetree))
  rjpp_data_nodes <- rjpp_data %>% rename(., node=descNode) %>% mutate(., iters = rjpp_out$niter) %>% mutate(., ppRate = round(nOrgnNRate/iters,2))
  timetree <- treeio::as.treedata(timetree)
  treedata <- treeio::full_join(timetree, y = rjpp_data_nodes, by = "node")
  return(treedata)
}

#You can do cool things like this 
tree_2_BTraits<-BTRTools::rjpp(rjlog = "whole_var.txt.VarRates.txt",
                               rjtrees = "whole_var.txt.Output.trees", 
                               tree = treeWHOLE) #this is your time scaled tree that was used to input into bayestraits



save(tree_2_BTraits, file = 'tree_2_BTraits.R')


tree_2_w_data <- add_rjpp_to_tree(tree_2_BTraits)
p<-ggtree(tree_2_w_data, aes(color = log(meanRate)), size=1)

#Create a colour palette 
colspal<-colorRampPalette(c("blue","cyan","yellow","red"))

#check the ranges of the mean rates 
ggplot(tree_2_w_data@extraInfo, aes(x=log(meanRate)))+geom_histogram()

#plot
tree_2_w_data <- add_rjpp_to_tree(tree_2_BTraits)
threshold <- .4 # the minimum posterior probability you want to plot a symbol for 
p<-ggtree(tree_2_w_data, aes(color = log(meanRate)), size=2)+ #this is where you can add  layout="fan" if you want a fan 
  scale_colour_stepsn(breaks = seq(from=-1,to=6,by=.2), colours = colspal(100))+
  #theme(legend.position="top")+
  theme(legend.position=c(.32,.83),legend.direction = "horizontal",legend.box.background = element_rect(colour = "black",size =1))+
  scale_size(range = c(1,2))+ 
  labs(title="Cetacean  skull rates",
       color="log(Rate)")+
  geom_nodepoint(aes(subset=ppRate>threshold, size = ppRate),color='black',fill="grey", shape=24)+
  geom_tiplab(label= sub("_", " ",tree_2_w_data@phylo$tip.label), size=2, color = "black")+
  coord_cartesian(xlim = c(-50, 9), #you have to fiddle with these values to get your tip labels to show. the first value should be just before your root time, second value pads out space for tip labels
                  ylim = c(-2, 200), #first value makes room for geo timescale, second value is vertical space and should be a few more than your number of tips
                  expand = FALSE) +
  scale_x_continuous(breaks=-epochs$max_age[c(1:6)], labels=epochs$max_age[c(1:6)]) + 
  theme(panel.grid.major.x = element_line(colour="grey", size=0.5), legend.key.height =unit(.5,"cm"))#should also be modified based on your time scale limits
p <- revts(p) 
ptree2 <- gggeo_scale(p, dat = "epochs", neg = FALSE, center_end_labels = TRUE, height = unit(1, "line"),  size=3)

ptree2
