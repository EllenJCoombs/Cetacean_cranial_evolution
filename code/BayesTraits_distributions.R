
##############################################
#                                            #
#   Rates distributions from Bayes Traits    #
#        Code from Ryan Felice               #
#                                            #
##############################################




rate_summary <- function(rjpp_out, time_tree, lookup.table, group_column, taxa, min_clade_size=2){
  #select the species from  the lookup table that are part of the current dataset
  lookup.table <- rename(lookup.table, GROUP = group_column)
  lookup.table <- rename(lookup.table, SPECIES = taxa)
  current.species <- filter(lookup.table, SPECIES %in% rjpp_out$meantree$tip.label)
  clades <- unique(lookup.table$GROUP)
  #select the sub tree for each group
  time_trees <- lapply(1:length(clades), function(x) keep.tip(time_tree, (filter(current.species, GROUP == clades[x]) %>% pull(SPECIES))))
  names(time_trees)<-clades
  taxa_per_clade <- unlist(lapply(1:length(time_trees), function(x) Ntip(time_trees[[x]])))
  time_trees <- time_trees[which(taxa_per_clade>=min_clade_size)]
  #get the time represented by each sub tree
  time_per_tree <- unlist(lapply(1:length(time_trees), function(x) max(nodeHeights(time_trees[[x]]))))
  #the first column is the rate on the root and our time tree has no root edge so this is removed with the "[-1,]"
  rate_table <- rjpp_out$scalars$rates[-1,]
  #make copy the mean tree x times where x is the number of trees in the posterior distribution
  treelist<-rep(list(rjpp_out$meantree),dim(rate_table)[2])
  #now convert the edge lengths of each of those trees to be the rate scalar for that edge
  #this allows us to keep rates associated with the appropriate edges as we go to the next step
  for (k in 1:length(treelist)){
    treelist[[k]]$edge.length<-rate_table[,k]
  }
  #now subset those trees based on group identity
  clades <- names(time_trees)
  results_table <- as_tibble(t(setNames(as.numeric(rep("", length(clades))), clades))[0, ])
  results_table_scaled <- results_table 
  for (i in 1:length(treelist)){
    scaled_trees <- lapply(1:length(clades), function(x) keep.tip(treelist[[i]], time_trees[[x]]$tip.label))
    mean_rate_list <- unlist(lapply(1:length(scaled_trees), function(x) mean(scaled_trees[[x]]$edge.length)))
    mean_rate_list_scaled <- mean_rate_list/time_per_tree
    names(mean_rate_list) <- names(mean_rate_list_scaled) <- colnames(results_table)
    results_table <- bind_rows(results_table, mean_rate_list)
    results_table_scaled <- bind_rows(results_table_scaled, mean_rate_list_scaled)
  }
  #comnbine into two tables, one for unscaled one for scaled 
  results <- results_table %>% pivot_longer(cols = clades, 
                                            names_to = "Group",
                                            values_to = "MeanRate")
  results_scaled <- results_table_scaled %>% pivot_longer(cols = clades, 
                                                          names_to = "Group",
                                                          values_to = "MeanRate")
  myresults<-list(results, results_scaled)
names(myresults)<-c("results","results_scaled")
return(myresults)
}

                                    
      my_rates<- rate_summary(rjpp_out = tree_2_BTraits,
                        lookup.table = species_data,
                        group_column = "family",
                        taxa = "label",
                        time_tree = time_tree,
                        min_clade_size = 0)
#and plot:

results2<-my_rates$results
phyloseq <- c("Aetiocetidae", "Aglaocetidae", "Agorophiidae", "Albireonidae", "Allodelphinidae", "Ambulocetidae", "Balaenidae", "Balaenopteridae", "Basilosauridae",
              "Cetotheriidae",  "Delphinida",  "Delphinidae", "Eschrichtiidae", "Eurhinodelphinidae", "Iniidae", "Kentriodontidae", "Kekenodontidae", "Kogiidae", "Lipotidae",
              "Lophocetinae", "Mammalodontidae",  "Monodontidae", "Mystacodontidae", "Odobenocetopsidae",  "Pakicetidae", "Patriocetidae", "Pelocetidae", "Phocoenidae", "Physeteridae", "Platanistidae",
              "Pontoporiidae", "Prosqualodontidae", "Protocetidae",  "Remingtonocetidae", "Simocetidae",  "Squalodelphinidae", "Squalodontidae", "Squaloziphiidae", "Tranatocetidae",
              "Waipatiidae",  "Xenorophidae", "Ziphiidae")
phyloseq <- phyloseq[which((phyloseq %in% unique(results2$Group)))]
results3 <- results2
results3$Group <- factor(results3$Group, levels = phyloseq)
p2 <- ggplot(data=results3, aes(x=log(MeanRate), group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.9)+
  scale_fill_viridis_d(breaks=phyloseq)
p2
species_data2 <- species_data %>% filter(family %in% unique(results2$Group))
species_data2 <- species_data2 %>% 
  filter(!duplicated(paste0(pmax(suborder, family), pmin(suborder, family))))
species_data2 <- species_data2 %>% select(c(suborder, family))
x<-list("Aetiocetidae"=  "mysticete" ,
        "Aglaocetidae"=  "mysticete",
        "Agorophiidae"=  "odontocete",
        "Albireonidae"  = "odontocete"  ,
        "Allodelphinidae"= "odontocete"  ,
        "Ambulocetidae" = "archaeocete" ,
        "Balaenidae"= "mysticete",
        "Balaenopteridae"= "mysticete" ,
        "Basilosauridae" = "archaeocete",
        "Cetotheriidae"= "mysticete",
        "Delphinida"=  "odontocete" ,
        "Delphinidae"=  "odontocete"  ,
        "Eschrichtiidae"     =  "mysticete",
        "Eurhinodelphinidae"=  "odontocete",
        "Iniidae"=  "odontocete",
        "Kentriodontidae"=  "odontocete",
        "Kekenodontidae"     =  "archaeocete",
        "Kogiidae" = "odontocete",
        "Lipotidae" = "odontocete",
        "Lophocetinae" = "odontocete",
        "Mammalodontidae" = "mysticete",
        "Monodontidae" = "odontocete",
        "Mystacodontidae" = "mysticete",
        "Odobenocetopsidae" = "odontocete",
        "Pakicetidae" = "archaeocete", 
        "Patriocetidae" = "odontocete",
        "Pelocetidae" = "mysticete",
        "Phocoenidae" = "odontocete",
        "Physeteridae" = "odontocete",
        "Platanistidae" = "odontocete",
        "Pontoporiidae" = "odontocete",
        "Prosqualodontidae" = "odontocete", 
        "Protocetidae" = "archaeocete",
        "Remingtonocetidae" = "archaeocete",
        "Simocetidae" = "odontocete", 
        "Squalodelphinidae" = "odontocete",
        "Squalodontidae" = "odontocete", 
        "Squaloziphiidae" = "odontocete",
        "Tranatocetidae" = "mysticete",
        "Waipatiidae" = "odontocete", 
        "Xenorophidae" = "odontocete", 
        "Ziphiidae" = "odontocete")
        
results3<-results3 %>% mutate(suborder = recode(results3$Group, !!!x))
results4 <- split(results3,f = results3$suborder)
library(gridExtra)
library(RColorBrewer)
library(patchwork)
p1 <- ggplot(data=results4$archaeocete, aes(x=log(MeanRate), group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.9)+
  scale_fill_viridis_d( breaks=phyloseq)+
  facet_grid(suborder~.)+
  theme_bw()+
  guides(fill=guide_legend(ncol=2))+
  xlim(-3,7)+
  ylim(0,3)+
  theme(strip.text.y = element_text(size = 7))
p2 <- p1 %+% results4$mysticete
p3 <- p2 %+% results4$odontocete
p1 + p2 + p3 + plot_layout(ncol = 1)

ggplotly(p3)                              
                                    
  
