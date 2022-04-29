

############################################################################
##                    RESAMPLING                                          ##
############################################################################
	cursub.closer<-function(cur,req)
		{
			mat<-as.matrix(dist(cur))
				DPO<-NULL
				for (j in 1:nrow(cur)-1)
					{
						a<-mat[j+1,j]
						DPO<-c(DPO,a)
					}
			DCO<-NULL
				for (j in 1:length(DPO))
					{
						DCO[j]<-sum(DPO[1:j])
					}
			DN<-sum(DPO)/(req+1)
			DCN<-DN*(1:req)
				clo<-NULL
				for (k in 1:length(DCN))
					{
						clo[k]  <- which.min(abs(DCO- DCN[k]))
					}
			clo<-clo+1
			cur[clo,]
		}
## extension Ã  une liste de courbes et de nombre de points requis
	subsampl.closer<-function(matlm,curlist,required,fix)
		{
		output<-matlm[fix,]
			for (i in 1:length(curlist))
				{
				cur<-matlm[curlist[[i]],]
				req<-required[i]
				out<-cursub.closer(cur,req)
				output<-rbind(output,out[1:length(out),])
				}
			output	
		}
## think about doing a check for duplicates, taking an anatomical point as a curve point... in short, it's a little dangerous if you don't have nbptsinitial>>nptsrequired

############################################################################
##                    RESAMPLING AVEC INTEPOLATION                        ##
############################################################################

cursub.interpo<-function(cur,req)
{
mat<-as.matrix(dist(cur))
DPO<-NULL									#DPO= distance pt to pt original
	for (j in 1:nrow(cur)-1)
		{
			a<-mat[j+1,j]
			DPO<-c(DPO,a)
		}
DCO<-NULL									#DCO= Chordal distance to the original initial point
	for (j in 1:length(DPO))
		{
			DCO[j]<-sum(DPO[1:j])
		}

DN<-sum(DPO)/(req+1)								#Total distance/nb of points = distance between 2 points which will be created (+1 because for n points required, there are n+1 intervals since the initial and terminal points which are anatomical are not counted.
DCN<-DN*(1:(req))  								#DCN= distance between the initial point and the new points
proxima <- matrix(nrow=req,ncol=2)						#matrix containing nearest points
for (k in 1:length(DCN))
	{
		first <- which.min(abs(DCO- DCN[k]))				#nearest point
		proxima[k,1] <- first
		second <- which.min(abs(DCO[-first]- DCN[k]))
		ifelse(first==second,second<-(second+1),second<-second)		#since we turn the pt closest to the vector if first=second, second is actually the point first+1
		proxima[k,2] <- second#second point les plus proche
	}
proxima<-proxima+1 								#because dist of the 1st pt to itself=0 suddenly it does not appear in the measurements of dist length =5 for 6pt)
proxima2 <- matrix(nrow=req,ncol=2)
for (i in 1:req)
	{
		if(proxima[i,1]>proxima[i,2])
		{
		proxima2[i,1]<-proxima[i,2];proxima2[i,2]<-proxima[i,1]}
		else if (proxima[i,1]<proxima[i,2]){
		proxima2[i,1]<-proxima[i,1];proxima2[i,2]<-proxima[i,2]}
	}									#reset points in ascending index order (if nearest point is one rowindex higher than second-nearest point)
VEC<-matrix( nrow=req, ncol = 3)						#VEC=vector from proximal point[n,1] to proximal point[n,2]
for(l in 1:req)
	{
		VEC[l,]<-as.matrix(cur[proxima2[l,2],]-cur[proxima2[l,1],])
	}
COMP<-NULL									#COMP=distance between the point to create and the proximal point[n,i]
for(n in 1:req)
	{
		COMP[n]<-DCN[n]-DCO[proxima2[n,1]-1]				#because DCO makes p-1 point because cordal distance from the first point to itself=0
	}
NOR<-NULL  									#Norm to apply to the vector
for(m in 1:req)
	{
		NOR[m]<-COMP[m]/(DPO[proxima2[m,1]])				####corrected on 20-07-15
	}
VECF<-VEC*NOR
PTS<-cur[proxima2[,1],]+VECF
PTS<-rbind(cur[1,],PTS,cur[dim(cur)[1],])
PTS
}

## extension Ã  une liste de courbes et de nombre de points requis
	subsampl.inter<-function(matlm,curlist,required,fix)
		{
		if (is.list(curlist)==F)
		print("curlist must be a list giving the curves(rowindex)")
		else 
		if (is.vector(required)==F)
		print("required must be a vector giving the number of points required per curve")
		else 
		if (length(curlist)!=length(required))
		print("curlist and required must be of same length")
		else 
		
		output<-matlm[fix,]
			for (i in 1:length(curlist))
				{
				cur<-matlm[curlist[[i]],]
				req<-required[i]
				out<-cursub.interpo(cur,req)
				rownames(out)<-paste("curve",i,"-",(1:dim(out)[1]-1),sep="")
				output<-rbind(output,out[2:(dim(out)[1]-1),])
				}
			output	
	}
	
	
	subsampl.inter2<-function(matlm,curlist,required,fix)
	{
	  if (is.list(curlist)==F)
	    print("curlist must be a list giving the curves(rowindex)")
	  else 
	    if (is.vector(required)==F)
	      print("required must be a vector giving the number of points required per curve")
	  else 
	    if (length(curlist)!=length(required))
	      print("curlist and required must be of same length")
	  else 
	    
	    output<-matlm[fix,]
	  for (i in 1:length(curlist))
	  {
	    
	    cur<-matlm[curlist[[i]],]
	    req<-required[i]
	    if (any(matlm[curlist[[i]],]==9999)) {
	      out <- matrix(9999, nrow = req+2,  ncol = 3)
	    }
	    else {
	      out<-cursub.interpo(cur,req)
	    }
	    rownames(out)<-paste("curve",i,"-",(1:dim(out)[1]-1),sep="")
	    output<-rbind(output,out[2:(dim(out)[1]-1),])
	  }
	  output	
	}
