#Classify 3 types of people by G-value(5 questions' average G-value)#
library(igraph)
print('Please confirm the prefered threshold is 0.3')
threshold = 0.3

#calculate the first question's  G-value#
way <- file.choose() 
Rdata <- read.csv(way)

name <- names(Rdata)[-1] # save the name
M1<- as.matrix(Rdata[,-1])
diag(M1) <- 0
result <- cbind(name,'role','type') #final result will save in this file#

#Directed Effect# 
#Let variable Zji measure the strength of a connection between actors j and supervisor i:
#0 for no connection;
#1 for an asymmetric connection from j to i
#2 for an asymmetric tie from i to j
#and 3 for reciprocal connections.
Deffect1 <- array(0,length(name))
Deffect1[M1[,1]==1] <- 2
Deffect1 <- Deffect1+M1[1,]
G1 <- cbind(name[-1],Deffect1[-1])

#Undirected Effect#
#The summed term measures connections from j with strong connections to colleagues k
#who have strong connections to supervisor i. #
mid1 <- array(0,length(name)) 
for(i in 2:ncol(M1)){
  mid1[i] <- sum(M1[i,]*M1[,1])
}
UDeffect1 <- (3*(mid1[-1] - min(mid1[-1])))/(max(mid1[-1]-min(mid1[-1])))
if(max(mid1[-1]) == 0){
  UDeffect1 <- 0 
}
G1 <- cbind(G1,UDeffect1)
GValue1 <- as.numeric(G1[,2])+as.numeric(G1[,3])


#calculate the second question's  G-value#
way <- file.choose() 
Rdata <- read.csv(way)
name <- names(Rdata)[-1] 
M2<- as.matrix(Rdata[,-1])
diag(M2) <- 0
Deffect2 <- array(0,length(name))
Deffect2[M2[,1]==1] <- 2
Deffect2 <- Deffect2+M2[1,]
G2 <- cbind(name[-1],Deffect2[-1])

mid2 <- array(0,length(name))
for(i in 2:ncol(M2)){
  mid2[i] <- sum(M2[i,]*M2[,1])
}
UDeffect2 <- (3*(mid2[-1] - min(mid2[-1])))/(max(mid2[-1]-min(mid2[-1])))
if(max(mid2[-1]) == 0){
  UDeffect2 <- 0 
}
G2 <- cbind(G2,UDeffect2)
GValue2 <- as.numeric(G2[,2])+as.numeric(G2[,3])

#calculate the third question's  G-value#
way <- file.choose() 
Rdata <- read.csv(way)

name <- names(Rdata)[-1]
M3<- as.matrix(Rdata[,-1])
diag(M3) <- 0
Deffect3 <- array(0,length(name))
Deffect3[M3[,1]==1] <- 2
Deffect3 <- Deffect3+M3[1,]
G3 <- cbind(name[-1],Deffect3[-1])

mid3 <- array(0,length(name))
for(i in 2:ncol(M3)){
  mid3[i] <- sum(M3[i,]*M3[,1])
}
UDeffect3 <- (3*(mid2[-1] - min(mid3[-1])))/(max(mid3[-1]-min(mid3[-1])))
if(max(mid3[-1]) == 0){
  UDeffect3 <- 0 
}
G3 <- cbind(G3,UDeffect3)
GValue3 <- as.numeric(G3[,2])+as.numeric(G3[,3])


#calculate the fourth question's  G-value#
way <- file.choose() 
Rdata <- read.csv(way)

name <- names(Rdata)[-1]
M4<- as.matrix(Rdata[,-1])
diag(M4) <- 0
Deffect4 <- array(0,length(name))
Deffect4[M4[,1]==1] <- 2
Deffect4 <- Deffect3+M3[1,]
G4 <- cbind(name[-1],Deffect4[-1])

mid4 <- array(0,length(name)) 
for(i in 2:ncol(M4)){
  mid4[i] <- sum(M4[i,]*M4[,1])
}
UDeffect4 <- (3*(mid4[-1] - min(mid4[-1])))/(max(mid4[-1]-min(mid4[-1])))
if(max(mid4[-1]) == 0){
  UDeffect4 <- 0 
}
G4 <- cbind(G4,UDeffect4)
GValue4 <- as.numeric(G4[,2])+as.numeric(G4[,3])

#calculate the fifth question's  G-value#
way <- file.choose() 
Rdata <- read.csv(way)

name <- names(Rdata)[-1] 
M5<- as.matrix(Rdata[,-1])
diag(M5) <- 0
Deffect5 <- array(0,length(name))
Deffect5[M5[,1]==1] <- 2
Deffect5 <- Deffect5+M5[1,]
G5 <- cbind(name[-1],Deffect5[-1])

mid5 <- array(0,length(name)) 
for(i in 2:ncol(M5)){
  mid5[i] <- sum(M5[i,]*M5[,1])
}
UDeffect5 <- (3*(mid5[-1] - min(mid5[-1])))/(max(mid5[-1]-min(mid5[-1])))
if(max(mid5[-1]) == 0){
  UDeffect5 <- 0 
}
G5 <- cbind(G5,UDeffect5)
GValue5 <- as.numeric(G5[,2])+as.numeric(G5[,3])

#G-value everage
GValue <- (GValue2+GValue1+GValue3+GValue4+GValue5)/5
names(GValue) <- name[-1] #G-value#

#sort G-value#
Gsort <- sort(GValue,decreasing = TRUE)

#output the barplot#
filename <- strsplit(way,'\\\\')
plotname <- paste('Barplot_',filename[[1]][length(filename[[1]])],sep='')
plotname <- strsplit(plotname,'\\.')
plotname <- paste(plotname[[1]][1],'.jpeg',sep = '')
filename[[1]][length(filename[[1]])] <- plotname
plotway <- paste(filename[[1]],collapse = '\\')

barplot(Gsort)

#find cliff#
mid <- array(0,length(Gsort))
mid[1:(length(Gsort)-1)]<-Gsort[-1]
Gcliff <- (Gsort-mid)[1:(length(Gsort)-1)]
Fcliff <- cbind(Gcliff,Gsort[1:(length(Gsort)-1)],Gsort[-1])
colnames(Fcliff)[2:3] <- c('Upper','Lower') 


#decide class#
if(sum(Fcliff[,'Gcliff'] >= threshold) >=2){ #when there are more than two cliffs#
  Dis4 <- abs((Fcliff[,'Upper']-4)+(Fcliff[,'Lower']-4))
  Dis2 <- abs((Fcliff[,'Upper']-2)+(Fcliff[,'Lower']-2))
  Flag4 <- names(which.min(Dis4[Fcliff[,'Gcliff'] >= threshold]))
  Flag2 <- names(which.min(Dis2[Fcliff[,'Gcliff'] >= threshold]))
  core_members <-names(Gsort)[1:which(names(Gsort) == Flag4)]
  peripheral <- names(Gsort)[(which(names(Gsort) == Flag4)+1):which(names(Gsort) == Flag2)]
  if(Fcliff[,'Upper'][Fcliff[,'Gcliff'] > threshold][which.min(Dis4[Fcliff[,'Gcliff'] >= threshold])] <= 2){
    peripheral <-names(Gsort)[1:which(names(Gsort) == Flag4)]
    core_members <- NA
  }
  if(Fcliff[,'Upper'][Fcliff[,'Gcliff'] > threshold][which.min(Dis2[Fcliff[,'Gcliff'] >= threshold])] >= 4){
    core_members <- names(Gsort)[1:which(names(Gsort) == Flag2)]
    peripheral <- NA
  }
}

#if there is only one cliff we will compare it with 3# 
#over 3 is core, below 3 is peripheral#
if(sum(Fcliff[,'Gcliff'] >= threshold) ==1){ 
  if(Fcliff[,'Upper'][Fcliff[,'Gcliff'] >= threshold] >= 3){
    Flag4 <- rownames(Fcliff)[Fcliff[,'Gcliff'] >= threshold]
    core_members <-names(Gsort)[1:which(names(Gsort) == Flag4)]
    peripheral <- NA
  }else{
    Flag2 <- rownames(Fcliff)[Fcliff[,'Gcliff'] >= threshold]
    peripheral <- names(Gsort)[1:which(names(Gsort) == Flag2)]
    core_members <- NA
  }
}
#if there is no cliff we define there is no core and no peripheral#
if(sum(Fcliff[,'Gcliff'] >= threshold) ==0){
  core_members <- NA
  peripheral <- NA
  print("-------CAN NOT FIND CLIFF PLEASE USE OTHER ALGORITHM --------")
}

supervisor <- name[1]
result[,2][is.element(name,supervisor)] <- 'supervisor'
result[,3][is.element(name,supervisor)] <- 0
result[,2][is.element(name,core_members)] <- 'core member'
result[,3][is.element(name,core_members)] <- 1
result[,2][is.element(name,peripheral)] <- 'peripheral'
result[,3][is.element(name,peripheral)] <- 2



#In the final step, all of these four categories are taken as in-group members, and anyone else who does not fit in the above categories were coded as outsiders.
record <- c(supervisor,core_members,peripheral)
outsider <- name[!is.element(name,record)]

#save result 6 means outsider
result[,2][is.element(name,outsider)] <- 'outsider'
result[,3][is.element(name,outsider)] <- 6

#output you will see the where the result is saved
print(c("supervisor is", supervisor))
print("supervisor's core member are:")
print(core_members)
print("supervisors's peripheral circle members are: ")
print(peripheral)
print('the outsider are')
print(outsider)

#output to file
filename <- strsplit(way,'\\\\')
filename[[1]][length(filename[[1]])] <- paste('result_',filename[[1]][length(filename[[1]])],sep='')
way <- paste(filename[[1]],collapse = '\\')
write.csv(result,way)
rm(list = ls())
