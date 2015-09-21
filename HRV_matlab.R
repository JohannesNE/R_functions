require(R.matlab)
require(dplyr)
require(rhdf5)


#Removes dimensons by converting to a vector
asVec <- function(X){
	t <- lapply(X, as.vector)
	names(t) <- dimnames(X)[[1]]
	t
}

#Extract Specific analyses Slightly better For-loop way
getData <- function(path, keepMatrix = F){
	toVec <- function(X){		#Funtion to switch between drop and asVec. Drop keeps matrix structure
		if(keepMatrix) drop(X)
		else asVec(X)
	}
	
	data <- readMat(path)
	ana<-data$Res #Reads analysis from data list
	ana<-toVec(ana)
	
	for(i in 1:length(ana)){ #loops through the datastructure converting one branch at a time
		if(length(dim(ana[[i]]))>1){
			ana[[i]]<-toVec(ana[[i]])	
			
			for(j in 1:length(ana[[i]])){
				if(length(dim(ana[[c(i,j)]]))>1){
					ana[[c(i,j)]]<-toVec(ana[[c(i,j)]])
					
					for(k in 1:length(ana[[c(i,j)]])){
						if(length(dim(ana[[c(i,j,k)]]))>1){
							ana[[c(i,j,k)]]<-toVec(ana[[c(i,j,k)]])
						}
						
					}
				}
				
			}
		}
		 		
	}
	ana #returns ana
}


