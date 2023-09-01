
##########################################################
#                                                        #
#  Process and visualise the results from Bayes Traits   #
#                                                        #
##########################################################

library(devtools)
install_github("hferg/BTprocessR")
library(BTprocessR)
library(dplyr)

#Install BTprocessR package.... R version needs to be >3.6 for this part
##library(devtools)
#install_github("hferg/BTprocessR")
#Comparing model fit using marginal likelihoods and Bayes Factor

#Make a list of all the Stone files
stones <- c(file=paste("BM_whole_var.txt.Stones.txt"),
            file=paste("BM_var_whole_var.txt.Stones.txt"),
            file=paste("delta_whole_var.txt.Stones.txt"),
            file=paste("delta_var_whole_var.txt.Stones.txt"),
            file=paste("kappa_whole_var.txt.Stones.txt"),
            file=paste("kappa_var_whole_var.txt.Stones.txt"),
            file=paste("lambda_whole_var.txt.Stones.txt"),
            file=paste("lambda_var_whole_var.txt.Stones.txt"),
            file=paste("OU_whole_var.txt.Stones.txt"),
            file=paste("OU_var_whole_var.txt.Stones.txt"))
#Calculate marginal likelihoods
marginal_likelihoods <- getStones(stones, labels = c("BM", "BM_var", "Delta", "Delta_var", "Kappa", "Kappa_var", "Lambda", "Lambda_var", "OU", "OU_var"))

#Check marginal likelihoods
marginal_likelihoods
#Look at them stones.... I mean plot those Bayes Factors (Bayes factors > 6 should be supported)
plot(marginal_likelihoods)


post <- loadPosterior("Lambda_whole_var.txt.Log.txt")
plot(post)
mean(post$Lambda)


#Check marginal likelihoods
marginal_likelihoods
#Look at them stones.... I mean plot those Bayes Factors (Bayes factors > 6 should be supported)
plot(marginal_likelihoods)


#What estimation of the model do you get? How strong is it? 
# E.g. 'Lambda was estimated as 0.69 for the whole skull, suggesting a moderately high level of phylogenetic signal'
post <- loadPosterior("Lambda_whole_var.txt.Log.txt")
plot(post)
mean(post$Lambda)
