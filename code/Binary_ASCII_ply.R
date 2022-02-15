
##################################
#                                #
# TO VISUALISE LMs ON THE MESH   #
#                                #
##################################


#run this script before reading in your .pts files 

#this makes nice 3D outputs of the files for publications as well (such as the M.Mirus) - SEE BOTTOM
#script for converting binary to ASCII .ply files
#rm(list=ls())

install.packages("Rvcg")
install.packages("rgl")
library(Rvcg)
library(rgl)

#set working directory with ascii or binary ply files
#put the .ply files you want to change (for reading into R) in the .ply binary folder (not the .ply folder) 
setwd("D:/Ply ASCII/ply binary")

#set output folder
#The extra forward slash at the end tells R that that is the output folder
outputfolder<-("D:/Ply ASCII/ply binary/")

meshlist<-dir(pattern='.ply',recursive=F)

for (i in 1:length(meshlist)){
  x<-vcgImport(file=meshlist[i]) #import meshes
  
  vcgPlyWrite(x,filename=paste(outputfolder,meshlist[i],sep=""),binary=FALSE) #export
}


#EXTRAS

#Remove ply from the filename 
#filelist <- dir(pattern='.ply', recursive=F)
#gsub("ply", "", filelist)

#outputfolder<-("R:/Ellen/WHOLE skull analyses/ply/")

#To visualise ASCII files using newpts from 2. Ryan's code 
#Set the directory to the one with the .ply in it 
checkLM(newpts,path="./ply/",suffix=".ply",pt.size=1,render="s",alpha=1)


