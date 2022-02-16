

library(geomorph)
library(ape)
library(rgl)

###########################################
#                                         #
#     Read in your tree and species data  #
#     specific to mysticetes, archs,      #
#     odonts                              #
#                                         #
###########################################


#species data 
species_data <- read.csv('Whole skull phylo names.csv', fileEncoding="UTF-8-BOM")
mysticetes_data <- read.csv('mysticetes phylo.csv', fileEncoding="UTF-8-BOM")
archaeocetes_data <- read.csv('archaeocetes phylo.csv', fileEncoding="UTF-8-BOM")
odontocetes_data <- read.csv('odontocetes phylo.csv', fileEncoding="UTF-8-BOM")

#make sure the names are the phylo names
Full_names_phylo=species_data$phylo.name
Full_names_phylo=mysticetes_data$phylo.name
Full_names_phylo=odontocetes_data$phylo.name
Full_names_phylo=archaeocetes_data$phylo.name

dimnames(final_procrusted_DATA)[[3]]<-Full_names_phylo 
final_procrusted_DATA <- final_procrusted_DATA[,,treeWHOLE$tip.label]
View(cbind(dimnames(final_procrusted_DATA)[[3]], treeWHOLE$tip.label))

#########################################
#                                       #
#    final mirrored data should be      #
#            #PROCRUSTED                #
#                                       #
#########################################

#Procrustes the data 
Y.gpa=gpagen(final_dataset) #Remove non-shape aspects 
data=Y.gpa$coords #Subset out the coords 
size=Y.gpa$Csize


final_procrusted_dataset <- data 
#check the procrusted data has the same names as the tree 
dimnames(final_procrusted_ARCHS)[[3]]<-Full_names_phylo 
final_procrusted_ARCHS <- final_procrusted_ARCHS[,,treeARCHS$tip.label]
View(dimnames(final_procrusted_ARCHS)[[3]])

View(cbind(dimnames(final_procrusted_ARCHS)[[3]], treeARCHS$tip.label))

############################
#                          #
#         Regions          #
#                          #
############################

#regionsOdonts <- read.csv('regions_odonts_archs.csv')
regionsALLMYSTS <- read.csv('RegionsALLMYSTS.csv', fileEncoding="UTF-8-BOM") #the landmarks have been rearranged to be mysticete-correct 

regionsALLMYSTS_zygo_squa <- read.csv('RegionsALLMYSTS_zygo_squa.csv', fileEncoding="UTF-8-BOM")

regionsALLMYSTS$bone 

#makes sure the specimen name is the same 
View(cbind(dimnames(final_procrusted_DATA)[[3]], treeWHOLE$tip.label))

#double check the data - are these lms correct? i.e., no RHS lms
#spheres3d(slidedlmsODONTS[c(1:66),,4], radius =  2)
#double check curve lengths are correct 
sum(regionsLHS$lm == "c52")

nas <- which(regionsALLMYSTS$bone=="nasal")
#plot them to check 
spheres3d(final_procrusted_DATA[nas,,146], radius = 0.0002, col = 'red')
#text3d(arranged[nas,,3], text=nas)
#text3d(slidedlmsMYSTS_plot[nas,,34], text=nas, col = 'blue')


#pull out the modules 
premax <- which(regionsALLMYSTS$bone=="premax")
#plot them to check 
spheres3d(final_procrusted_ODONTS[premax,,11], radius =  0.0002, col = 'red')
#text3d(arranged[premax,,2], text=premax)
#text3d(newpts[premax,,95], text=premax, col = 'blue')

#pull out the modules 
maxilla <- which(regionsALLMYSTS$bone=="maxilla")
#plot them to check 
spheres3d(final_procrusted_ODONTS[maxilla,,99], radius =  0.0002)
#text3d(final_mirrored_mysts[maxilla,,1], text= regionsALLMYSTS$lm)


#pull out the modules 
frontal <- which(regionsALLMYSTS$bone=="frontal")
#plot them to check 
spheres3d(final_procrusted_ODONTS[frontal,, 13], radius =  0.0002)
#text3d(shapedata[frontal,,42], text=frontal)


#pull out the modules 
pteryg <- which(regionsALLMYSTS$bone=="pterygoid")
#plot them to check 
spheres3d(final_procrusted_ODONTS[pteryg,,3], radius = 0.0002, col = 'green')
#text3d(shapedata[pteryg,,42], text=pteryg)


#pull out the modules 
pal <- which(regionsALLMYSTS$bone=="palate")
#plot them to check 
spheres3d(final_procrusted_ODONTS[pal,,2], radius =  0.0002)
#text3d(shapedata[pal,,42], text=pal)

#pull out the modules 
supocc <- which(regionsALLMYSTS$bone=="supraoccipital")
#plot them to check 
spheres3d(final_procrusted_ODONTS[supocc,,1], radius =  0.0002)
#text3d(shapedata[supocc,,42], text=occipital)

#pull out the modules 
basiocc <- which(regionsALLMYSTS$bone=="basioccipital")
#plot them to check 
spheres3d(final_procrusted_ODONTS[basiocc,,9], radius =  0.0002)
#text3d(shapedata[basiocc,,42], text=basiocc)

#pull out the modules 
basisphen <- which(regionsALLMYSTS$bone=="basisphenoid")
#plot them to check 
spheres3d(final_procrusted_ODONTS[basisphen,,1], radius =  0.0002)
#text3d(shapedata[basisphen,,42], text=basisphen)


#pull out the modules 
mandp <- which(regionsALLMYSTS$bone=="mandibular process")
#plot them to check 
spheres3d(final_procrusted_ODONTS[mandp,,1], radius =  0.0002)
#text3d(shapedata[mandp,,42], text=mandp)


#pull out the modules 
occipcon <- which(regionsALLMYSTS$bone=="occipital")
#plot them to check 
spheres3d(final_procrusted_ODONTS[occipcon,,1], radius =  0.0002)
#text3d(shapedata[occipcon,,42], text=occipcon)


#pull out the modules 
jug <- which(regionsALLMYSTS$bone=="jugal")
#plot them to check 
spheres3d(final_procrusted_ODONTS[jug,,1], radius =  0.0002)
#text3d(shapedata[jug,,2], text=jug)


#pull out the modules 
parietal <- which(regionsALLMYSTS$bone=="parietal")
#plot them to check 
spheres3d(final_procrusted_ODONTS[parietal,,1], radius =  0.0002)
#text3d(shapedata[parietal,,2], text=parietal)


#pull out the modules 
squa <- which(regionsALLMYSTS$bone=="squamosal")
#plot them to check 
spheres3d(final_procrusted_ODONTS[squa,,1], radius =  0.0002)
#text3d(final_procrusted_ARCHS[squa,,1], text=squa)


#pull out the modules 
zygo <- which(regionsALLMYSTS$bone=="zygomatic")
#plot them to check 
spheres3d(final_procrusted_ODONTS[zygo,,1], radius =  0.0002)
#text3d(shapedata[zygo,,2], text=squa)

zygo_squa <- which(regionsALLMYSTS_zygo_squa$bone=="zygo-squa")
#plot them to check 
spheres3d(final_procrusted_ODONTS[zygo_squa,,3], radius =  0.0002)
#text3d(shapedata[zygo,,2], text=squa)




#check - Procrusted
atarfa=ply2mesh(file="E:/Ply ASCII/ply ASCII/All/ply/Delphinus delphis AMNH 75332.ply")
shade3d(atarfa,col='white')
spheres3d(final_procrusted_ODONTS[nas,,83], radius = 0.0005, color = "red")
spheres3d(final_procrusted_ODONTS[premax,,83], radius =  0.0005, color = "darkblue")
spheres3d(final_procrusted_ODONTS[maxilla,,83], radius = 0.0005, color = "lightgreen")
spheres3d(final_procrusted_ODONTS[frontal,,83], radius =  0.0005, color = "pink")
spheres3d(final_procrusted_ODONTS[pteryg,,83], radius =  0.0005, color = "purple")
spheres3d(final_procrusted_ODONTS[pal,,83], radius =  0.0005, color = "yellow")
spheres3d(final_procrusted_ODONTS[supocc,,83], radius =  0.0005, color = "orange")
spheres3d(final_procrusted_ODONTS[basiocc,,83], radius =  0.0005, color = "lightblue")
spheres3d(final_procrusted_ODONTS[basisphen,,83], radius =  0.0005, color = "darkgreen")
spheres3d(final_procrusted_ODONTS[mandp,,83], radius =  0.0005, color = "lightblue")
spheres3d(final_procrusted_ODONTS[jug,,83], radius =  0.0005, color = "coral")
spheres3d(final_procrusted_ODONTS[occipcon,,83], radius =  0.0005, color = "turquoise")
spheres3d(final_procrusted_ODONTS[parietal,,83], radius =  0.0005, color = "black")
spheres3d(final_procrusted_ODONTS[squa,,83], radius =  0.0005, color = "darkred")
spheres3d(final_procrusted_ODONTS[zygo_squa,,83], radius =  0.0005, color = "green")


#check - NOT procrusted 

atarfa=ply2mesh(file="D:/Ply ASCII/ply ASCII/All/ply/Delphinus delphis AMNH 75332.ply")
shade3d(atarfa,col='white')
spheres3d(final_mirrored_odonts[nas,,24], radius = 2, color = "red")
spheres3d(final_mirrored_odonts[premax,,24], radius =  2, color = "darkblue")
spheres3d(final_mirrored_odonts[maxilla,,24], radius = 2, color = "lightgreen")
spheres3d(final_mirrored_odonts[frontal,,24], radius =  2, color = "pink")
spheres3d(final_mirrored_odonts[pteryg,,24], radius =  2, color = "purple")
spheres3d(final_mirrored_odonts[pal,,24], radius =  2, color = "yellow")
spheres3d(final_mirrored_odonts[supocc,,24], radius =  2, colour = "orange")
spheres3d(final_mirrored_odonts[basiocc,,24], radius =  2, color = "lightblue")
spheres3d(final_mirrored_odonts[basisphen,,24], radius =  2, color = "darkgreen")
spheres3d(final_mirrored_odonts[mandp,,24], radius =  2, color = "tan")
spheres3d(final_mirrored_odonts[jug,,24], radius =  2, color = "grey")
spheres3d(final_mirrored_odonts[occipcon,,24], radius =  2, color = "turquoise")
spheres3d(final_mirrored_odonts[parietal,,24], radius =  2, color = "black")
spheres3d(final_mirrored_odonts[squa,,24], radius = 2, color = "darkred")



# Dolphin for chapter 

atarfa=ply2mesh(file="E:/Ply ASCII/ply ASCII/All/ply/Delphinus delphis AMNH 75332.ply")
shade3d(atarfa,col='white')
spheres3d(final_dataset[nas,,48], radius = 2, color = "red")
spheres3d(final_dataset[premax,,48], radius =  2, color = "darkblue")
spheres3d(final_dataset[maxilla,,48], radius =2, color = "lightgreen")
spheres3d(final_dataset[frontal,,48], radius =  2, color = "pink")
spheres3d(final_dataset[pteryg,,48], radius =  2, color = "purple")
spheres3d(final_dataset[pal,,48], radius =  2, color = "yellow")
spheres3d(final_dataset[supocc,,48], radius =  2, color = "orange")
spheres3d(final_dataset[basiocc,,48], radius =  2, color = "lightblue")
spheres3d(final_dataset[basisphen,,48], radius =  2, color = "darkgreen")
spheres3d(final_dataset[mandp,,48], radius =  2, color = "tan")
spheres3d(final_dataset[jug,,48], radius =  2, color = "grey")
spheres3d(final_dataset[occipcon,,48], radius =  2, color = "turquoise")
spheres3d(final_dataset[parietal,,48], radius =  2, color = "black")
spheres3d(final_dataset[zygo_squa,,48], radius = 2, color = "darkred")

#spheres3d(shapedata[zygo,,42], radius =  0.0002, color = "magenta")


#Whale for chapter 

atarfa=ply2mesh(file="E:/Ply ASCII/ply ASCII/Mysts/ply/Balaenoptera musculus NHM 1892.3.1.1.ply")
shade3d(atarfa,col='white')
spheres3d(final_mirrored_mysts[nas,,11], radius = 12, color = "red")
spheres3d(final_mirrored_mysts[premax,,11], radius =  12, color = "darkblue")
spheres3d(final_mirrored_mysts[maxilla,,11], radius = 12, color = "lightgreen")
spheres3d(final_mirrored_mysts[frontal,,11], radius =  12, color = "pink")
spheres3d(final_mirrored_mysts[pteryg,,11], radius =  12, color = "purple")
spheres3d(final_mirrored_mysts[pal,,11], radius =  12, color = "yellow")
spheres3d(final_mirrored_mysts[supocc,,11], radius =  12, color = "orange")
spheres3d(final_mirrored_mysts[basiocc,,11], radius =  12, color = "lightblue")
spheres3d(final_mirrored_mysts[basisphen,,11], radius =  12, color = "darkgreen")
spheres3d(final_mirrored_mysts[mandp,,11], radius =  12, color = "burlywood4")
spheres3d(final_mirrored_mysts[jug,,11], radius =  12, color = "grey")
spheres3d(final_mirrored_mysts[occipcon,,11], radius =  12, color = "turquoise")
spheres3d(final_mirrored_mysts[parietal,,11], radius =  12, color = "black")
spheres3d(final_mirrored_mysts[zygo_squa,,11], radius = 12, color = "darkred")

#spheres3d(shapedata[zygo,,42], radius =  0.0002, color = "magenta")


#Archaeocetes 
atarfa=ply2mesh(file="E:/Ply ASCII/ply ASCII/Archs/ply/Zygorhiza kochi USNM 11962.ply")
shade3d(atarfa,col='white')
spheres3d(final_mirrored_archs[nas,,1], radius = 2, color = "red")
spheres3d(final_mirrored_archs[premax,,1], radius =  2, color = "darkblue")
spheres3d(final_mirrored_archs[maxilla,,1], radius = 2, color = "lightgreen")
spheres3d(final_mirrored_archs[frontal,,1], radius =  2, color = "pink")
spheres3d(final_mirrored_archs[pteryg,,1], radius =  2, color = "purple")
spheres3d(final_mirrored_archs[pal,,1], radius =  2, color = "yellow")
spheres3d(final_mirrored_archs[supocc,,1], radius =  2, color = "orange")
spheres3d(final_mirrored_archs[basiocc,,1], radius =  2, color = "lightblue")
spheres3d(final_mirrored_archs[basisphen,,1], radius =  2, color = "darkgreen")
spheres3d(final_mirrored_archs[mandp,,1], radius =  2, color = "burlywood4")
spheres3d(final_mirrored_archs[jug,,1], radius =  2, color = "grey")
spheres3d(final_mirrored_archs[occipcon,,1], radius =  2, color = "turquoise")
spheres3d(final_mirrored_archs[parietal,,1], radius =  2, color = "black")
spheres3d(final_mirrored_archs[zygo_squa,,1], radius = 2, color = "darkred")




#Half skull
atarfa=ply2mesh(file="D:/Ply ASCII/ply ASCII/All/ply/Delphinus delphis AMNH 75332.ply")
shade3d(atarfa,col=bone1)
spheres3d(final_mirrored_odonts[c(1:4, 124:153),,24], radius = 2, color = "red")
spheres3d(final_mirrored_odonts[c(5:8, 64:66, 154:233, 1074:1113),,24], radius =  2, color = "darkblue")
spheres3d(final_mirrored_odonts[c(9:15, 58, 61:63, 234:368, 999:1073),,24], radius = 2, color = "lightgreen")
spheres3d(final_mirrored_odonts[c(19:24, 369:458),,24], radius =  2, color = "pink")
spheres3d(final_mirrored_odonts[c(59:60, 969:998),,24], radius =  2, color = "purple")
spheres3d(final_mirrored_odonts[c(55:57, 894:968),,24], radius =  2, color = "yellow")
spheres3d(final_mirrored_odonts[c(39:45, 639:738),,24], radius =  2, colour = "orange")
spheres3d(final_mirrored_odonts[c(48:49, 774:833),,24], radius =  2, color = "lightblue")
spheres3d(final_mirrored_odonts[c(51:54, 834:893),,24], radius =  2, color = "darkgreen")
spheres3d(final_mirrored_odonts[c(34:35, 609:638),,24], radius =  2, color = "brown")
spheres3d(final_mirrored_odonts[c(16,17,18),,24], radius =  2, color = "coral")
spheres3d(final_mirrored_odonts[c(41, 42, 47, 47, 739:773),,24], radius =  2, color = "turquoise")
spheres3d(final_mirrored_odonts[c(25:29, 459:523),,24], radius =  2, color = "black")
spheres3d(final_mirrored_odonts[c(30:33, 36,37, 524:608),,24], radius = 2, color = "darkred")
#spheres3d(shapedata[zygo,,42], radius =  0.0002, color = "magenta")




atarfa=ply2mesh(file="D:/Ply ASCII/ply ASCII/All/ply/Balaenoptera brydei USNM 572922.ply")
shade3d(atarfa,col='white')
spheres3d(final_mirrored_mysts[nas,,8], radius = 4, color = "red")
spheres3d(final_mirrored_mysts[premax,,8], radius =  4, color = "darkblue")
spheres3d(final_mirrored_mysts[maxilla,,8], radius = 4, color = "lightgreen")
spheres3d(final_mirrored_mysts[frontal,,8], radius =  4, color = "pink")
spheres3d(final_mirrored_mysts[pteryg,,8], radius =  4, color = "purple")
spheres3d(final_mirrored_mysts[pal,,8], radius =  4, color = "yellow")
spheres3d(final_mirrored_mysts[supocc,,8], radius =  4, colour = "orange")
spheres3d(final_mirrored_mysts[basiocc,,8], radius =  4, color = "lightblue")
spheres3d(final_mirrored_mysts[basisphen,,8], radius =  4, color = "darkgreen")
spheres3d(final_mirrored_mysts[mandp,,8], radius =  4, color = "brown")
spheres3d(final_mirrored_mysts[jug,,8], radius =  4, color = "coral")
spheres3d(final_mirrored_mysts[occipcon,,8], radius =  4, color = "turquoise")
spheres3d(final_mirrored_mysts[parietal,,8], radius =  4, color = "black")
spheres3d(final_mirrored_mysts[squa,,8], radius = 4, color = "darkred")
#spheres3d(shapedata[zygo,,42], radius =  0.0002, color = "magenta")




###########################################
#                                         #
#    MAKE SURE THE DATA AND TREE MATCH    #
#                                         #
###########################################

#pulling out the data for each region from all specimens 

nas_data=final_procrusted_DATA[nas,,]
premax_data=final_procrusted_DATA[premax,,]
max_data=final_procrusted_DATA[maxilla,,]
frontal_data=final_procrusted_DATA[frontal,,]
pteryg_data=final_procrusted_DATA[pteryg,,]
pal_data=final_procrusted_DATA[pal,,]
supocc_data=final_procrusted_DATA[supocc,,]
basiocc_data=final_procrusted_DATA[basiocc,,]
basisphen_data=final_procrusted_DATA[basisphen,,]
mandp_data=final_procrusted_DATA[mandp,,]
jug_data=final_procrusted_DATA[jug,,]
occipcon_data=final_procrusted_DATA[occipcon,,]
parietal_data=final_procrusted_DATA[parietal,,]
squa_data=final_procrusted_DATA[squa,,]
#zygo_data=final_procrusted_DATA[zygo,,]
zygo_squa_data=final_procrusted_DATA[zygo_squa,,]

nas_data=final_procrusted_ODONTS[nas,,]
premax_data=final_procrusted_ODONTS[premax,,]
max_data=final_procrusted_MYSTS[maxilla,,]
frontal_data=final_procrusted_ODONTS[frontal,,]
pteryg_data=final_procrusted_ODONTS[pteryg,,]
pal_data=final_procrusted_ODONTS[pal,,]
supocc_data=final_procrusted_MYSTS[supocc,,]
basiocc_data=final_procrusted_ODONTS[basiocc,,]
basisphen_data=final_procrusted_ODONTS[basisphen,,]
mandp_data=final_procrusted_ODONTS[mandp,,]
jug_data=final_procrusted_ODONTS[jug,,]
occipcon_data=final_procrusted_ODONTS[occipcon,,]
parietal_data=final_procrusted_ODONTS[parietal,,]
squa_data=final_procrusted_ODONTS[squa,,]
#zygo_data=final_procrusted_ODONTS[zygo,,]
zygo_squa_data=final_procrusted_ODONTS[zygo_squa,,]


#makes sure names match shapedata 
#dimnames(nas_data)[[3]]<-Full_names_phylo 
#dimnames(premax_data)[[3]]<-Full_names_phylo 
#dimnames(max_data)[[3]]<-Full_names_phylo 
#dimnames(frontal_data)[[3]]<-Full_names_phylo 
#dimnames(pteryg_data)[[3]]<-Full_names_phylo 
#dimnames(supocc_data)[[3]]<-Full_names_phylo 
#dimnames(basiocc_data)[[3]]<-Full_names_phylo 
#dimnames(basisphen_data)[[3]]<-Full_names_phylo 
#dimnames(mandp_data)[[3]]<-Full_names_phylo 
#dimnames(occipcon_data)[[3]]<-Full_names_phylo 
#dimnames(parietal_data)[[3]]<-Full_names_phylo 
#dimnames(squa_data)[[3]]<-Full_names_phylo 
#dimnames(zygo_data)[[3]]<-Full_names_phylo 
#dimnames(pal_data)[[3]]<-Full_names_phylo 
#dimnames(jug_data)[[3]]<-Full_names_phylo 


#pPCA bone by bone 

#################################################################
#                                                               #
#   These are for BayesTraits - run BayesTraits.R afterwards    #
#                                                               #
#################################################################

#Whole skull 


phyPCA <- geomorph::gm.prcomp(final_procrusted_ODONTS, phy = treeODONTS, GLS = TRUE)

phyPCA <- geomorph::gm.prcomp(final_procrusted_MYSTS, phy = treeMYSTS, GLS = TRUE)

plot(phyPCA, axis1 = 1, axis2 = 2) #labels = T) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
whole_var=phyPCA$x[,1:30]# was pc.scores - 95%  
write.table(whole_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/whole skull/whole_var.txt",
            quote = FALSE, col.names = FALSE)


#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#### nasal bone PCA - PHYLO PCA

phyPCA <- geomorph::gm.prcomp(nas_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #labels = T) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
nas_var=phyPCA$x[,1:5]# was pc.scores - 95%  
write.table(nas_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/nasal/nas_var.txt",
            quote = FALSE, col.names = FALSE)


#PCA=plotTangentSpace(nas_data, label= mysticetes_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#nas_var=PCA$pc.scores[,1:3] ## PC3 96%
#write.table(nas_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/nasal/nas_var.txt",
            #quote = FALSE, col.names = FALSE)
#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)


phyPCA <- geomorph::gm.prcomp(premax_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
premax_var=phyPCA$x[,1:13]# was pc.scores - 95%  
write.table(premax_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/premaxilla/premax_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### premaxilla bone PCA
#PCA=plotTangentSpace(premax_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#premax_var=PCA$pc.scores[,1:7]
#write.table(premax_var*1000,
#file = "F:/PTS FINAL LHS/BayesTraits/premax/premax_var.txt",
#quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(max_data, phy = treeWHOLE, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
max_var=phyPCA$x[,1:19]# was pc.scores - 95%  
write.table(max_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/maxilla/max_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### maxilla bone PCA
#PCA=plotTangentSpace(max_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#max_var=PCA$pc.scores[,1:16] 
#write.table(max_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/maxilla/max_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(frontal_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
frontal_var=phyPCA$x[,1:11]# was pc.scores - 95%  
write.table(frontal_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/frontal/frontal_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### frontal bone PCA
#PCA=plotTangentSpace(frontal_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#frontal_var=PCA$pc.scores[,1:8] 
#write.table(frontal_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/frontal/frontal_var.txt",
            #quote = FALSE, col.names = FALSE)



phyPCA <- geomorph::gm.prcomp(pteryg_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
pteryg_var=phyPCA$x[,1:7]# was pc.scores - 95%  
write.table(pteryg_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/pterygoid/pteryg_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### pterygoid bone PCA
#PCA=plotTangentSpace(pteryg_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#pteryg_var=PCA$pc.scores[,1:7] 
#write.table(pteryg_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/pterygoid/pteryg_var.txt",
            #quote = FALSE, col.names = FALSE)

phyPCA <- geomorph::gm.prcomp(pal_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
pal_var=phyPCA$x[,1:8]# was pc.scores - 95%  
write.table(pal_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/palate/pal_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### palate bone PCA
#PCA=plotTangentSpace(pal_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## PCA 96%
#pal_var=PCA$pc.scores[,1:8] 
#write.table(pal_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/palate/pal_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(supocc_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
supocc_var=phyPCA$x[,1:13]# was pc.scores - 95%  
write.table(supocc_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/supraoccipital/supocc_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### supraoccipital bone PCA
#PCA=plotTangentSpace(supocc_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#supocc_var=PCA$pc.scores[,1:11] 
#write.table(supocc_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/supraoccipital/supocc_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(basiocc_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
basiocc_var=phyPCA$x[,1:7]# was pc.scores - 95%  
write.table(basiocc_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/basioccipital/basiocc_var.txt",
            quote = FALSE, col.names = FALSE)


#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### basioccipital bone PCA
#PCA=plotTangentSpace(basiocc_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#basiocc_var=PCA$pc.scores[,1:8] 
#write.table(basiocc_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/basioccipital/basiocc_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(basisphen_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
basisphen_var=phyPCA$x[,1:4]# was pc.scores - 95%  
write.table(basisphen_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/basisphenoid/basisphen_var.txt",
            quote = FALSE, col.names = FALSE)


#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### basisphenoid bone PCA
#PCA=plotTangentSpace(basisphen_data, label= species_data$species, axis1=1, axis2=2)
#PCA$pc.summary ## 
#basisphen_var=PCA$pc.scores[,1:5] 
#write.table(basisphen_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/basisphenoid/basisphen_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(mandp_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
mandp_var=phyPCA$x[,1:6]# was pc.scores - 95%  
write.table(mandp_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/mandibular process/mandp_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### mandibular bone PCA
#PCA=plotTangentSpace(mandp_data, label= species_data$species, axis1=1, axis2=2)
#PCA$pc.summary ## 
#mandp_var=PCA$pc.scores[,1:5] 
#write.table(mandp_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/mandibular process/mandp_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(jug_data, phy = treeWHOLE, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
jug_var=phyPCA$x[,1:5]# was pc.scores - 95%  
write.table(jug_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/jugal/jug_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### jug bone PCA
#PCA=plotTangentSpace(jug_data, label= species_data$species, axis1=1, axis2=2)
#PCA$pc.summary ## 
#jug_var=PCA$pc.scores[,1:4] 
#write.table(jug_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/jugal/jug_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(occipcon_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
occipcon_var=phyPCA$x[,1:6]# was pc.scores - 95%  
write.table(occipcon_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/occipital condyle/occipcon_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)


#pPCA bone by bone 
#### occipital bone PCA
#PCA=plotTangentSpace(occipcon_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## PC6 96%
#occipcon_var=PCA$pc.scores[,1:6] 
#write.table(occipcon_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/occipital condyle/occipcon_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(parietal_data, phy = treeODONTS, GLS = TRUE) 
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
parietal_var=phyPCA$x[,1:10]# was pc.scores - 95%  
write.table(parietal_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/parietal/parietal_var.txt",
            quote = FALSE, col.names = FALSE)


#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### occipital bone PCA
#PCA=plotTangentSpace(parietal_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ##
#parietal_var=PCA$pc.scores[,1:8] 
#write.table(parietal_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/parietal/parietal_var.txt",
            #quote = FALSE, col.names = FALSE)


phyPCA <- geomorph::gm.prcomp(squa_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
squar_var=phyPCA$x[,1:11]# was pc.scores - 95%  
write.table(squar_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/squamosal/squar_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)

#pPCA bone by bone 
#### occipital bone PCA
#PCA=plotTangentSpace(squa_data, label= species_data$suborder, axis1=1, axis2=2)
#PCA$pc.summary ## 
#squa_var=PCA$pc.scores[,1:9] 
#write.table(squa_var*1000,
            #file = "F:/PTS FINAL LHS/BayesTraits/squamosal/squa_var.txt",
            #quote = FALSE, col.names = FALSE)



phyPCA <- geomorph::gm.prcomp(zygo_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
zygo_var=phyPCA$x[,1:8]# was pc.scores - 95%  
write.table(zygo_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/zygomatic/zygo_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)


#Carla's example 
#Comparing regions - this doesn't have ecological data yet 
#C_19=compare.multi.evol.rates(A=Y.gpa.rhs_172, phy=tree_172, gp=M19)
#PS_disp=morphol.disparity(Y.gpa.rhs_173[PS,,]~1)/length(PS)
#Sq_disp=morphol.disparity(Y.gpa.rhs_173[Sq,,]~1)/length(Sq)


phyPCA <- geomorph::gm.prcomp(zygo_squa_data, phy = treeODONTS, GLS = TRUE)
plot(phyPCA, axis1 = 1, axis2 = 2) #to plot 
summary(phyPCA) # take a look at the PCs (now called comp)
zygo_squa_var=phyPCA$x[,1:14]# was pc.scores - 95%  
write.table(zygo_squa_var*1000,
            file = "D:/Whole skull analyses July 2020/BayesTraits/odontocetes/zygo-squa/zygo_squa_var.txt",
            quote = FALSE, col.names = FALSE)

#to look at specimen position 
text(phyPCA$x[,1],phyPCA$x[,2], pos = 4)



library(geomorph) #compare.multi.evo.rates
library(ape)
library(geiger) 
#Increase your R memory 
#memory.size() reports the current or maximum memory allocation of the malloc function used in this version of R.
#memory.limit() reports or increases the limit in force on the total allocation.
# eg. memory.limit(size = 4000000) 


#Check the trees match
View(cbind(dimnames(final_procrusted_ODONTS)[[3]], treeODONTS$tip.label))

#Need shapedata + SVP tree 
bones14 <- regionsALLMYSTS$bone
bones13 <- regionsALLMYSTS_zygo_squa$bone

bones14_MYSTS=compare.multi.evol.rates(A=final_procrusted_MYSTS, phy=treeMYSTS, gp=bones14)
bones13_MYSTS=compare.multi.evol.rates(A=final_procrusted_MYSTS, phy=treeMYSTS, gp=bones13)


#Bone by bone (no ecology data)
nas_disp=morphol.disparity(final_procrusted_DATA[nas,,]~1)/length(nas)
premax_disp=morphol.disparity(final_procrusted_DATA[premax,,]~1)/length(premax)
max_disp=morphol.disparity(final_procrusted_ARCHS[maxilla,,]~1)/length(max)
frontal_disp=morphol.disparity(final_procrusted_DATA[frontal,,]~1)/length(frontal)
pteryg_disp=morphol.disparity(final_procrusted_DATA[pteryg,,]~1)/length(pteryg)
supocc_disp=morphol.disparity(final_procrusted_DATA[supocc,,]~1)/length(supocc)
basiocc_disp=morphol.disparity(final_procrusted_DATA[basiocc,,]~1)/length(basiocc)
basisphen_disp=morphol.disparity(final_procrusted_DATA[basisphen,,]~1)/length(basisphen)
mandp_disp=morphol.disparity(final_procrusted_DATA[mandp,,]~1)/length(mandp)
occipcon_disp=morphol.disparity(final_procrusted_DATA[occipcon,,]~1)/length(occipcon)
parietal_disp=morphol.disparity(final_procrusted_DATA[parietal,,]~1)/length(parietal)
pal_disp=morphol.disparity(final_procrusted_DATA[pal,,]~1)/length(pal)
jug_disp=morphol.disparity(final_procrusted_DATA[jug,,]~1)/length(jug)
zygo_sqau_disp=morphol.disparity(final_procrusted_DATA[zygo_squa,,]~1)/length(zygo_squa)

save(bones14_MYSTS, file="bones14_MYSTS.R")

bones14_ARCHS #to get rates 
bones13_ARCHS
nas_dip # to get disparity for each bone 

disp_rates_ODONTS=read.csv("Rate_disparity_bone_ODONTS.csv", fileEncoding="UTF-8-BOM")
disp<-disp_rates_ODONTS[,2]
rates<-disp_rates_ODONTS[,3]
labels<-disp_rates_ODONTS[,1]

#corr rate disp complex - simple plot 
plot(disp~rates, xlab="Rate", ylab="Disparity", main="rate vs disp", pch=19)
text( rates,disp, labels, offset = 4)
regr=lm(disp~rates)
abline(regr)
summary(regr)#R2= 0.71 p=0.0001
cor.test(rates,disp, method = c( "spearman"))#0.80,p=0.0009


#nicer plot 

library(ggplot2)
library(ggrepel)
library(ggrepel)

b <- ggplot(data = disp_rates_ODONTS, aes(x = rate, y = disparity, colour = bone))
b <- b + geom_point(size=2) + labs(x="Evolutionary rate",y="Disparity") 
b = b + stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ x,colour="blue")
b = b + geom_text_repel(aes(label = labels))#with numbers of specimens 
b



EMR<-compare.multi.evol.rates(A=Y.gpa$coords,gp=land.gp, 
                              Subset=TRUE, phy= plethspecies$phy,iter=999)
summary(EMR)


plot(bones14_ODONTS)
