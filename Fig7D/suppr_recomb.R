### AIM: Calculation of suppressed recombination by investigating the genotype difference between males and females
### Author: David Willemsen
## loading the genotype and phenotype data from cross I

crossI=read.table("C:/Users/DWillemsen/Desktop/Genome_Transcriptome/goped7.csv",sep=",",header = T,check.names = F) ## loads the data set
colnames(crossI)[1]<-"Fam" ## replace the first colname, because it was missing
CrossI_F2=subset(crossI,crossI[,3]!=0)  ## filtering the grandparents and unknown ancestry individuals out of data
crossI_F2=subset(CrossI_F2,CrossI_F2[,3]!="G_M03") ## filtering the F1 generation out of data

### loading the linkage map
LM=read.table("C:/Users/DWillemsen/Desktop/LinkageMap_Dario/go32014_all_pos.csv",sep=",") ##loading the linkage map for cross I

Female_F2=subset(crossI_F2,crossI_F2[,5]==2) # only Females of the F2 Generation
Male_F2=subset(crossI_F2,crossI_F2[,5]==1)  #  only Males of the F2 Generation

LG1=subset(LM,LM[,2]==1)
LG2=subset(LM,LM[,2]==2)
LG3=subset(LM,LM[,2]==3)
LG4=subset(LM,LM[,2]==4)
LG5=subset(LM,LM[,2]==5)
LG6=subset(LM,LM[,2]==6)
LG7=subset(LM,LM[,2]==7)
LG8=subset(LM,LM[,2]==8)
LG9=subset(LM,LM[,2]==9)
LG10=subset(LM,LM[,2]==10)
LG11=subset(LM,LM[,2]==11)
LG12=subset(LM,LM[,2]==12)
LG13=subset(LM,LM[,2]==13)
LG14=subset(LM,LM[,2]==14)
LG15=subset(LM,LM[,2]==15)
LG16=subset(LM,LM[,2]==16)
LG17=subset(LM,LM[,2]==17)
LG18=subset(LM,LM[,2]==18)
LG19=subset(LM,LM[,2]==19)

### finding the position of each marker in the dataset

i=1
pos_1=c() 
while(i<=(nrow(LG1))){
  x=which(colnames(CrossI_F2)==LG1[i,1])   # which markername of cross I F2 generation is identical with the markername of the linkage group
  print(paste(x,i))  ## printing the process and success
  if(length(x)!=0){ 
    pos_1[i]<-x     ## position "x" of marker LG1[i,1]
  }
  else{pos_1[i]<-0   ## if marker is not found, it is notated as 0 [important for later analysis]
  }
  i=i+1
  print(i)  ##printing the process
}

i=1
pos_2=c()
while(i<=(nrow(LG2))){
  x=which(colnames(CrossI_F2)==LG2[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_2[i]<-x
  }
  else{pos_2[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_3=c()
while(i<=(nrow(LG3))){
  x=which(colnames(CrossI_F2)==LG3[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_3[i]<-x
  }
  else{pos_3[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_4=c()
while(i<=(nrow(LG4))){
  x=which(colnames(CrossI_F2)==LG4[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_4[i]<-x
  }
  if(length(x)==0){
    pos_4[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_5=c()
while(i<=(nrow(LG5))){
  x=which(colnames(CrossI_F2)==LG5[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_5[i]<-x
  }
  else{pos_5[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_6=c()
while(i<=(nrow(LG6))){
  x=which(colnames(CrossI_F2)==LG6[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_6[i]<-x
  }
  else{pos_6[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_7=c()
while(i<=(nrow(LG7))){
  x=which(colnames(CrossI_F2)==LG7[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_7[i]<-x
  }
  else{pos_7[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_8=c()
while(i<=(nrow(LG8))){
  x=which(colnames(CrossI_F2)==LG8[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_8[i]<-x
  }
  else{pos_8[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_9=c()
while(i<=(nrow(LG9))){
  x=which(colnames(CrossI_F2)==LG9[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_9[i]<-x
  }
  else{pos_9[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_10=c()
while(i<=(nrow(LG10))){
  x=which(colnames(CrossI_F2)==LG10[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_10[i]<-x
  }
  else{pos_10[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_11=c()
while(i<=(nrow(LG11))){
  x=which(colnames(CrossI_F2)==LG11[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_11[i]<-x
  }
  else{pos_11[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_12=c()
while(i<=(nrow(LG12))){
  x=which(colnames(CrossI_F2)==LG12[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_12[i]<-x
  }
  else{pos_12[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_13=c()
while(i<=(nrow(LG13))){
  x=which(colnames(CrossI_F2)==LG13[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_13[i]<-x
  }
  else{pos_13[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_14=c()
while(i<=(nrow(LG14))){
  x=which(colnames(CrossI_F2)==LG14[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_14[i]<-x
  }
  else{pos_14[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_15=c()
while(i<=(nrow(LG15))){
  x=which(colnames(CrossI_F2)==LG15[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_15[i]<-x
  }
  else{pos_15[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_16=c()
while(i<=(nrow(LG16))){
  x=which(colnames(CrossI_F2)==LG16[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_16[i]<-x
  }
  else{pos_16[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_17=c()
while(i<=(nrow(LG17))){
  x=which(colnames(CrossI_F2)==LG17[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_17[i]<-x
  }
  else{pos_17[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_18=c()
while(i<=(nrow(LG18))){
  x=which(colnames(CrossI_F2)==LG18[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_18[i]<-x
  }
  else{pos_18[i]<-0
  }
  i=i+1
  print(i)
}

i=1
pos_19=c()
while(i<=(nrow(LG19))){
  x=which(colnames(CrossI_F2)==LG19[i,1])
  print(paste(x,i))
  if(length(x)!=0){
    pos_19[i]<-x
  }
  else{pos_19[i]<-0
  }
  i=i+1
  print(i)
}

### Finding the AA,AB,BB counts for males and females

## Males

group=c()
f=1
i=1
m=1
Male_1_aa=c() ##counts for genotype AA in linkage group 1 
Male_1_ab=c() ##counts for genotype AB in linkage group 1 
Male_1_bb=c() ##counts for genotype BB in linkage group 1 
Male_1_NA=c() ##counts for NA in linkage group 1 
Marker_LG1=c()
cm_LG1=c()
while(i<=length(pos_1)){
  if(pos_1[i]!=0){
    Male_1_aa[m]<-length(which(Male_F2[,pos_1[i]]=="aa")) ## number of AAs at given position
    Male_1_ab[m]<-length(which(Male_F2[,pos_1[i]]=="ab")) ## number if ABs at given position
    Male_1_bb[m]<-length(which(Male_F2[,pos_1[i]]=="bb")) ## number if BBs at given position
    Male_1_NA[m]<-length(which(Male_F2[,pos_1[i]]=="-"))  ## number if - at given position
    Marker_LG1[m]<-as.character(LG1[i,1]) ## Markername
    cm_LG1[m]<-as.character(LG1[i,3])  ## centiMorgan at given position
    m=m+1
    group[f]<-"LG1"
    f=f+1
  }
  i=i+1
}


i=1
m=1
Male_2_aa=c()
Male_2_ab=c()
Male_2_bb=c()
Male_2_NA=c()
Marker_LG2=c()
cm_LG2=c()
while(i<=length(pos_2)){
  if(pos_2[i]!=0){
    Male_2_aa[m]<-length(which(Male_F2[,pos_2[i]]=="aa"))
    Male_2_ab[m]<-length(which(Male_F2[,pos_2[i]]=="ab"))
    Male_2_bb[m]<-length(which(Male_F2[,pos_2[i]]=="bb"))
    Male_2_NA[m]<-length(which(Male_F2[,pos_2[i]]=="-"))
    Marker_LG2[m]<-as.character(LG2[i,1])
    cm_LG2[m]<-as.character(LG2[i,3])
    m=m+1
    group[f]<-"LG2"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_3_aa=c()
Male_3_ab=c()
Male_3_bb=c()
Male_3_NA=c()
Marker_LG3=c()
cm_LG3=c()
while(i<=length(pos_3)){
  if(pos_3[i]!=0){
    Male_3_aa[m]<-length(which(Male_F2[,pos_3[i]]=="aa"))
    Male_3_ab[m]<-length(which(Male_F2[,pos_3[i]]=="ab"))
    Male_3_bb[m]<-length(which(Male_F2[,pos_3[i]]=="bb"))
    Male_3_NA[m]<-length(which(Male_F2[,pos_3[i]]=="-"))
    Marker_LG3[m]<-as.character(LG3[i,1])
    cm_LG3[m]<-as.character(LG3[i,3])
    m=m+1
    group[f]<-"LG3"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_4_aa=c()
Male_4_ab=c()
Male_4_bb=c()
Male_4_NA=c()
Marker_LG4=c()
cm_LG4=c()
while(i<=length(pos_4)){
  if(pos_4[i]!=0){
    Male_4_aa[m]<-length(which(Male_F2[,pos_4[i]]=="aa"))
    Male_4_ab[m]<-length(which(Male_F2[,pos_4[i]]=="ab"))
    Male_4_bb[m]<-length(which(Male_F2[,pos_4[i]]=="bb"))
    Male_4_NA[m]<-length(which(Male_F2[,pos_4[i]]=="-"))
    Marker_LG4[m]<-as.character(LG4[i,1])
    cm_LG4[m]<-as.character(LG4[i,3])
    m=m+1
    group[f]<-"LG4"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_5_aa=c()
Male_5_ab=c()
Male_5_bb=c()
Male_5_NA=c()
Marker_LG5=c()
cm_LG5=c()
while(i<=length(pos_5)){
  if(pos_5[i]!=0){
    Male_5_aa[m]<-length(which(Male_F2[,pos_5[i]]=="aa"))
    Male_5_ab[m]<-length(which(Male_F2[,pos_5[i]]=="ab"))
    Male_5_bb[m]<-length(which(Male_F2[,pos_5[i]]=="bb"))
    Male_5_NA[m]<-length(which(Male_F2[,pos_5[i]]=="-"))
    Marker_LG5[m]<-as.character(LG5[i,1])
    cm_LG5[m]<-as.character(LG5[i,3])
    m=m+1
    group[f]<-"LG5"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_6_aa=c()
Male_6_ab=c()
Male_6_bb=c()
Male_6_NA=c()
Marker_LG6=c()
cm_LG6=c()
while(i<=length(pos_6)){
  if(pos_6[i]!=0){
    Male_6_aa[m]<-length(which(Male_F2[,pos_6[i]]=="aa"))
    Male_6_ab[m]<-length(which(Male_F2[,pos_6[i]]=="ab"))
    Male_6_bb[m]<-length(which(Male_F2[,pos_6[i]]=="bb"))
    Male_6_NA[m]<-length(which(Male_F2[,pos_6[i]]=="-"))
    Marker_LG6[m]<-as.character(LG6[i,1])
    cm_LG6[m]<-as.character(LG6[i,3])
    m=m+1
    group[f]<-"LG6"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_7_aa=c()
Male_7_ab=c()
Male_7_bb=c()
Male_7_NA=c()
Marker_LG7=c()
cm_LG7=c()
while(i<=length(pos_7)){
  if(pos_7[i]!=0){
    Male_7_aa[m]<-length(which(Male_F2[,pos_7[i]]=="aa"))
    Male_7_ab[m]<-length(which(Male_F2[,pos_7[i]]=="ab"))
    Male_7_bb[m]<-length(which(Male_F2[,pos_7[i]]=="bb"))
    Male_7_NA[m]<-length(which(Male_F2[,pos_7[i]]=="-"))
    Marker_LG7[m]<-as.character(LG7[i,1])
    cm_LG7[m]<-as.character(LG7[i,3])
    m=m+1
    group[f]<-"LG7"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_8_aa=c()
Male_8_ab=c()
Male_8_bb=c()
Male_8_NA=c()
Marker_LG8=c()
cm_LG8=c()
while(i<=length(pos_8)){
  if(pos_8[i]!=0){
    Male_8_aa[m]<-length(which(Male_F2[,pos_8[i]]=="aa"))
    Male_8_ab[m]<-length(which(Male_F2[,pos_8[i]]=="ab"))
    Male_8_bb[m]<-length(which(Male_F2[,pos_8[i]]=="bb"))
    Male_8_NA[m]<-length(which(Male_F2[,pos_8[i]]=="-"))
    Marker_LG8[m]<-as.character(LG8[i,1])
    cm_LG8[m]<-as.character(LG8[i,3])
    m=m+1
    group[f]<-"LG8"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_9_aa=c()
Male_9_ab=c()
Male_9_bb=c()
Male_9_NA=c()
Marker_LG9=c()
cm_LG9=c()
while(i<=length(pos_9)){
  if(pos_9[i]!=0){
    Male_9_aa[m]<-length(which(Male_F2[,pos_9[i]]=="aa"))
    Male_9_ab[m]<-length(which(Male_F2[,pos_9[i]]=="ab"))
    Male_9_bb[m]<-length(which(Male_F2[,pos_9[i]]=="bb"))
    Male_9_NA[m]<-length(which(Male_F2[,pos_9[i]]=="-"))
    Marker_LG9[m]<-as.character(LG9[i,1])
    cm_LG9[m]<-as.character(LG9[i,3])
    m=m+1
    group[f]<-"LG9"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_10_aa=c()
Male_10_ab=c()
Male_10_bb=c()
Male_10_NA=c()
Marker_LG10=c()
cm_LG10=c()
while(i<=length(pos_10)){
  if(pos_10[i]!=0){
    Male_10_aa[m]<-length(which(Male_F2[,pos_10[i]]=="aa"))
    Male_10_ab[m]<-length(which(Male_F2[,pos_10[i]]=="ab"))
    Male_10_bb[m]<-length(which(Male_F2[,pos_10[i]]=="bb"))
    Male_10_NA[m]<-length(which(Male_F2[,pos_10[i]]=="-"))
    Marker_LG10[m]<-as.character(LG10[i,1])
    cm_LG10[m]<-as.character(LG10[i,3])
    m=m+1
    group[f]<-"LG10"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_11_aa=c()
Male_11_ab=c()
Male_11_bb=c()
Male_11_NA=c()
Marker_LG11=c()
cm_LG11=c()
while(i<=length(pos_11)){
  if(pos_11[i]!=0){
    Male_11_aa[m]<-length(which(Male_F2[,pos_11[i]]=="aa"))
    Male_11_ab[m]<-length(which(Male_F2[,pos_11[i]]=="ab"))
    Male_11_bb[m]<-length(which(Male_F2[,pos_11[i]]=="bb"))
    Male_11_NA[m]<-length(which(Male_F2[,pos_11[i]]=="-"))
    Marker_LG11[m]<-as.character(LG11[i,1])
    cm_LG11[m]<-as.character(LG11[i,3])
    m=m+1
    group[f]<-"LG11"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_12_aa=c()
Male_12_ab=c()
Male_12_bb=c()
Male_12_NA=c()
Marker_LG12=c()
cm_LG12=c()
while(i<=length(pos_12)){
  if(pos_12[i]!=0){
    Male_12_aa[m]<-length(which(Male_F2[,pos_12[i]]=="aa"))
    Male_12_ab[m]<-length(which(Male_F2[,pos_12[i]]=="ab"))
    Male_12_bb[m]<-length(which(Male_F2[,pos_12[i]]=="bb"))
    Male_12_NA[m]<-length(which(Male_F2[,pos_12[i]]=="-"))
    Marker_LG12[m]<-as.character(LG12[i,1])
    cm_LG12[m]<-as.character(LG12[i,3])
    m=m+1
    group[f]<-"LG12"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_13_aa=c()
Male_13_ab=c()
Male_13_bb=c()
Male_13_NA=c()
Marker_LG13=c()
cm_LG13=c()
while(i<=length(pos_13)){
  if(pos_13[i]!=0){
    Male_13_aa[m]<-length(which(Male_F2[,pos_13[i]]=="aa"))
    Male_13_ab[m]<-length(which(Male_F2[,pos_13[i]]=="ab"))
    Male_13_bb[m]<-length(which(Male_F2[,pos_13[i]]=="bb"))
    Male_13_NA[m]<-length(which(Male_F2[,pos_13[i]]=="-"))
    Marker_LG13[m]<-as.character(LG13[i,1])
    cm_LG13[m]<-as.character(LG13[i,3])
    m=m+1
    group[f]<-"LG13"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_14_aa=c()
Male_14_ab=c()
Male_14_bb=c()
Male_14_NA=c()
Marker_LG14=c()
cm_LG14=c()
while(i<=length(pos_14)){
  if(pos_14[i]!=0){
    Male_14_aa[m]<-length(which(Male_F2[,pos_14[i]]=="aa"))
    Male_14_ab[m]<-length(which(Male_F2[,pos_14[i]]=="ab"))
    Male_14_bb[m]<-length(which(Male_F2[,pos_14[i]]=="bb"))
    Male_14_NA[m]<-length(which(Male_F2[,pos_14[i]]=="-"))
    Marker_LG14[m]<-as.character(LG14[i,1])
    cm_LG14[m]<-as.character(LG14[i,3])
    m=m+1
    group[f]<-"LG14"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_15_aa=c()
Male_15_ab=c()
Male_15_bb=c()
Male_15_NA=c()
Marker_LG15=c()
cm_LG15=c()
while(i<=length(pos_15)){
  if(pos_15[i]!=0){
    Male_15_aa[m]<-length(which(Male_F2[,pos_15[i]]=="aa"))
    Male_15_ab[m]<-length(which(Male_F2[,pos_15[i]]=="ab"))
    Male_15_bb[m]<-length(which(Male_F2[,pos_15[i]]=="bb"))
    Male_15_NA[m]<-length(which(Male_F2[,pos_15[i]]=="-"))
    Marker_LG15[m]<-as.character(LG15[i,1])
    cm_LG15[m]<-as.character(LG15[i,3])
    m=m+1
    group[f]<-"LG15"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_16_aa=c()
Male_16_ab=c()
Male_16_bb=c()
Male_16_NA=c()
Marker_LG16=c()
cm_LG16=c()
while(i<=length(pos_16)){
  if(pos_16[i]!=0){
    Male_16_aa[m]<-length(which(Male_F2[,pos_16[i]]=="aa"))
    Male_16_ab[m]<-length(which(Male_F2[,pos_16[i]]=="ab"))
    Male_16_bb[m]<-length(which(Male_F2[,pos_16[i]]=="bb"))
    Male_16_NA[m]<-length(which(Male_F2[,pos_16[i]]=="-"))
    Marker_LG16[m]<-as.character(LG16[i,1])
    cm_LG16[m]<-as.character(LG16[i,3])
    m=m+1
    group[f]<-"LG16"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_17_aa=c()
Male_17_ab=c()
Male_17_bb=c()
Male_17_NA=c()
Marker_LG17=c()
cm_LG17=c()
while(i<=length(pos_17)){
  if(pos_17[i]!=0){
    Male_17_aa[m]<-length(which(Male_F2[,pos_17[i]]=="aa"))
    Male_17_ab[m]<-length(which(Male_F2[,pos_17[i]]=="ab"))
    Male_17_bb[m]<-length(which(Male_F2[,pos_17[i]]=="bb"))
    Male_17_NA[m]<-length(which(Male_F2[,pos_17[i]]=="-"))
    Marker_LG17[m]<-as.character(LG17[i,1])
    cm_LG17[m]<-as.character(LG17[i,3])
    m=m+1
    group[f]<-"LG17"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_18_aa=c()
Male_18_ab=c()
Male_18_bb=c()
Male_18_NA=c()
Marker_LG18=c()
cm_LG18=c()
while(i<=length(pos_18)){
  if(pos_18[i]!=0){
    Male_18_aa[m]<-length(which(Male_F2[,pos_18[i]]=="aa"))
    Male_18_ab[m]<-length(which(Male_F2[,pos_18[i]]=="ab"))
    Male_18_bb[m]<-length(which(Male_F2[,pos_18[i]]=="bb"))
    Male_18_NA[m]<-length(which(Male_F2[,pos_18[i]]=="-"))
    Marker_LG18[m]<-as.character(LG18[i,1])
    cm_LG18[m]<-as.character(LG18[i,3])
    m=m+1
    group[f]<-"LG18"
    f=f+1
  }
  i=i+1
}

i=1
m=1
Male_19_aa=c()
Male_19_ab=c()
Male_19_bb=c()
Male_19_NA=c()
Marker_LG19=c()
cm_LG19=c()
while(i<=length(pos_19)){
  if(pos_19[i]!=0){
    Male_19_aa[m]<-length(which(Male_F2[,pos_19[i]]=="aa"))
    Male_19_ab[m]<-length(which(Male_F2[,pos_19[i]]=="ab"))
    Male_19_bb[m]<-length(which(Male_F2[,pos_19[i]]=="bb"))
    Male_19_NA[m]<-length(which(Male_F2[,pos_19[i]]=="-"))
    Marker_LG19[m]<-as.character(LG19[i,1])
    cm_LG19[m]<-as.character(LG19[i,3])
    m=m+1
    group[f]<-"LG19"
    f=f+1
  }
  i=i+1
}

###Females

i=1
m=1
Female_1_aa=c()
Female_1_ab=c()
Female_1_bb=c()
Female_1_NA=c()
while(i<=length(pos_1)){
  if(pos_1[i]!=0){
    Female_1_aa[m]<-length(which(Female_F2[,pos_1[i]]=="aa"))
    Female_1_ab[m]<-length(which(Female_F2[,pos_1[i]]=="ab"))
    Female_1_bb[m]<-length(which(Female_F2[,pos_1[i]]=="bb"))
    Female_1_NA[m]<-length(which(Female_F2[,pos_1[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_2_aa=c()
Female_2_ab=c()
Female_2_bb=c()
Female_2_NA=c()
while(i<=length(pos_2)){
  if(pos_2[i]!=0){
    Female_2_aa[m]<-length(which(Female_F2[,pos_2[i]]=="aa"))
    Female_2_ab[m]<-length(which(Female_F2[,pos_2[i]]=="ab"))
    Female_2_bb[m]<-length(which(Female_F2[,pos_2[i]]=="bb"))
    Female_2_NA[m]<-length(which(Female_F2[,pos_2[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_3_aa=c()
Female_3_ab=c()
Female_3_bb=c()
Female_3_NA=c()
while(i<=length(pos_3)){
  if(pos_3[i]!=0){
    Female_3_aa[m]<-length(which(Female_F2[,pos_3[i]]=="aa"))
    Female_3_ab[m]<-length(which(Female_F2[,pos_3[i]]=="ab"))
    Female_3_bb[m]<-length(which(Female_F2[,pos_3[i]]=="bb"))
    Female_3_NA[m]<-length(which(Female_F2[,pos_3[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_4_aa=c()
Female_4_ab=c()
Female_4_bb=c()
Female_4_NA=c()
while(i<=length(pos_4)){
  if(pos_4[i]!=0){
    Female_4_aa[m]<-length(which(Female_F2[,pos_4[i]]=="aa"))
    Female_4_ab[m]<-length(which(Female_F2[,pos_4[i]]=="ab"))
    Female_4_bb[m]<-length(which(Female_F2[,pos_4[i]]=="bb"))
    Female_4_NA[m]<-length(which(Female_F2[,pos_4[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_5_aa=c()
Female_5_ab=c()
Female_5_bb=c()
Female_5_NA=c()
while(i<=length(pos_5)){
  if(pos_5[i]!=0){
    Female_5_aa[m]<-length(which(Female_F2[,pos_5[i]]=="aa"))
    Female_5_ab[m]<-length(which(Female_F2[,pos_5[i]]=="ab"))
    Female_5_bb[m]<-length(which(Female_F2[,pos_5[i]]=="bb"))
    Female_5_NA[m]<-length(which(Female_F2[,pos_5[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_6_aa=c()
Female_6_ab=c()
Female_6_bb=c()
Female_6_NA=c()
while(i<=length(pos_6)){
  if(pos_6[i]!=0){
    Female_6_aa[m]<-length(which(Female_F2[,pos_6[i]]=="aa"))
    Female_6_ab[m]<-length(which(Female_F2[,pos_6[i]]=="ab"))
    Female_6_bb[m]<-length(which(Female_F2[,pos_6[i]]=="bb"))
    Female_6_NA[m]<-length(which(Female_F2[,pos_6[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_7_aa=c()
Female_7_ab=c()
Female_7_bb=c()
Female_7_NA=c()
while(i<=length(pos_7)){
  if(pos_7[i]!=0){
    Female_7_aa[m]<-length(which(Female_F2[,pos_7[i]]=="aa"))
    Female_7_ab[m]<-length(which(Female_F2[,pos_7[i]]=="ab"))
    Female_7_bb[m]<-length(which(Female_F2[,pos_7[i]]=="bb"))
    Female_7_NA[m]<-length(which(Female_F2[,pos_7[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_8_aa=c()
Female_8_ab=c()
Female_8_bb=c()
Female_8_NA=c()
while(i<=length(pos_8)){
  if(pos_8[i]!=0){
    Female_8_aa[m]<-length(which(Female_F2[,pos_8[i]]=="aa"))
    Female_8_ab[m]<-length(which(Female_F2[,pos_8[i]]=="ab"))
    Female_8_bb[m]<-length(which(Female_F2[,pos_8[i]]=="bb"))
    Female_8_NA[m]<-length(which(Female_F2[,pos_8[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_9_aa=c()
Female_9_ab=c()
Female_9_bb=c()
Female_9_NA=c()
while(i<=length(pos_9)){
  if(pos_9[i]!=0){
    Female_9_aa[m]<-length(which(Female_F2[,pos_9[i]]=="aa"))
    Female_9_ab[m]<-length(which(Female_F2[,pos_9[i]]=="ab"))
    Female_9_bb[m]<-length(which(Female_F2[,pos_9[i]]=="bb"))
    Female_9_NA[m]<-length(which(Female_F2[,pos_9[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_10_aa=c()
Female_10_ab=c()
Female_10_bb=c()
Female_10_NA=c()
while(i<=length(pos_10)){
  if(pos_10[i]!=0){
    Female_10_aa[m]<-length(which(Female_F2[,pos_10[i]]=="aa"))
    Female_10_ab[m]<-length(which(Female_F2[,pos_10[i]]=="ab"))
    Female_10_bb[m]<-length(which(Female_F2[,pos_10[i]]=="bb"))
    Female_10_NA[m]<-length(which(Female_F2[,pos_10[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_11_aa=c()
Female_11_ab=c()
Female_11_bb=c()
Female_11_NA=c()
while(i<=length(pos_11)){
  if(pos_11[i]!=0){
    Female_11_aa[m]<-length(which(Female_F2[,pos_11[i]]=="aa"))
    Female_11_ab[m]<-length(which(Female_F2[,pos_11[i]]=="ab"))
    Female_11_bb[m]<-length(which(Female_F2[,pos_11[i]]=="bb"))
    Female_11_NA[m]<-length(which(Female_F2[,pos_11[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_12_aa=c()
Female_12_ab=c()
Female_12_bb=c()
Female_12_NA=c()
while(i<=length(pos_12)){
  if(pos_12[i]!=0){
    Female_12_aa[m]<-length(which(Female_F2[,pos_12[i]]=="aa"))
    Female_12_ab[m]<-length(which(Female_F2[,pos_12[i]]=="ab"))
    Female_12_bb[m]<-length(which(Female_F2[,pos_12[i]]=="bb"))
    Female_12_NA[m]<-length(which(Female_F2[,pos_12[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_13_aa=c()
Female_13_ab=c()
Female_13_bb=c()
Female_13_NA=c()
while(i<=length(pos_13)){
  if(pos_13[i]!=0){
    Female_13_aa[m]<-length(which(Female_F2[,pos_13[i]]=="aa"))
    Female_13_ab[m]<-length(which(Female_F2[,pos_13[i]]=="ab"))
    Female_13_bb[m]<-length(which(Female_F2[,pos_13[i]]=="bb"))
    Female_13_NA[m]<-length(which(Female_F2[,pos_13[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_14_aa=c()
Female_14_ab=c()
Female_14_bb=c()
Female_14_NA=c()
while(i<=length(pos_14)){
  if(pos_14[i]!=0){
    Female_14_aa[m]<-length(which(Female_F2[,pos_14[i]]=="aa"))
    Female_14_ab[m]<-length(which(Female_F2[,pos_14[i]]=="ab"))
    Female_14_bb[m]<-length(which(Female_F2[,pos_14[i]]=="bb"))
    Female_14_NA[m]<-length(which(Female_F2[,pos_14[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_15_aa=c()
Female_15_ab=c()
Female_15_bb=c()
Female_15_NA=c()
while(i<=length(pos_15)){
  if(pos_15[i]!=0){
    Female_15_aa[m]<-length(which(Female_F2[,pos_15[i]]=="aa"))
    Female_15_ab[m]<-length(which(Female_F2[,pos_15[i]]=="ab"))
    Female_15_bb[m]<-length(which(Female_F2[,pos_15[i]]=="bb"))
    Female_15_NA[m]<-length(which(Female_F2[,pos_15[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_16_aa=c()
Female_16_ab=c()
Female_16_bb=c()
Female_16_NA=c()
while(i<=length(pos_16)){
  if(pos_16[i]!=0){
    Female_16_aa[m]<-length(which(Female_F2[,pos_16[i]]=="aa"))
    Female_16_ab[m]<-length(which(Female_F2[,pos_16[i]]=="ab"))
    Female_16_bb[m]<-length(which(Female_F2[,pos_16[i]]=="bb"))
    Female_16_NA[m]<-length(which(Female_F2[,pos_16[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_17_aa=c()
Female_17_ab=c()
Female_17_bb=c()
Female_17_NA=c()
while(i<=length(pos_17)){
  if(pos_17[i]!=0){
    Female_17_aa[m]<-length(which(Female_F2[,pos_17[i]]=="aa"))
    Female_17_ab[m]<-length(which(Female_F2[,pos_17[i]]=="ab"))
    Female_17_bb[m]<-length(which(Female_F2[,pos_17[i]]=="bb"))
    Female_17_NA[m]<-length(which(Female_F2[,pos_17[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_18_aa=c()
Female_18_ab=c()
Female_18_bb=c()
Female_18_NA=c()
while(i<=length(pos_18)){
  if(pos_18[i]!=0){
    Female_18_aa[m]<-length(which(Female_F2[,pos_18[i]]=="aa"))
    Female_18_ab[m]<-length(which(Female_F2[,pos_18[i]]=="ab"))
    Female_18_bb[m]<-length(which(Female_F2[,pos_18[i]]=="bb"))
    Female_18_NA[m]<-length(which(Female_F2[,pos_18[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}

i=1
m=1
Female_19_aa=c()
Female_19_ab=c()
Female_19_bb=c()
Female_19_NA=c()
while(i<=length(pos_19)){
  if(pos_19[i]!=0){
    Female_19_aa[m]<-length(which(Female_F2[,pos_19[i]]=="aa"))
    Female_19_ab[m]<-length(which(Female_F2[,pos_19[i]]=="ab"))
    Female_19_bb[m]<-length(which(Female_F2[,pos_19[i]]=="bb"))
    Female_19_NA[m]<-length(which(Female_F2[,pos_19[i]]=="-"))
    
    m=m+1
  }
  i=i+1
}


## Calculating genotype difference

### Calculating the genotype difference for every position and linkage group
aa_1=((Male_1_aa/(Male_1_aa+Male_1_ab+Male_1_bb))-(Female_1_aa/(Female_1_aa+Female_1_ab+Female_1_bb))) ## Difference of genotype frequency of Male AA and genotype frequency of Female AA (Linkage Group 1)
ab_1=((Male_1_ab/(Male_1_aa+Male_1_ab+Male_1_bb))-(Female_1_ab/(Female_1_aa+Female_1_ab+Female_1_bb))) ## Difference of genotype frequency of Male AB and genotype frequency of Female AB (Linkage Group 1)
bb_1=((Male_1_bb/(Male_1_aa+Male_1_ab+Male_1_bb))-(Female_1_bb/(Female_1_aa+Female_1_ab+Female_1_bb))) ## Difference of genotype frequency of Male BB and genotype frequency of Female BB (Linkage Group 1)

aa_2=((Male_2_aa/(Male_2_aa+Male_2_ab+Male_2_bb))-(Female_2_aa/(Female_2_aa+Female_2_ab+Female_2_bb)))
ab_2=((Male_2_ab/(Male_2_aa+Male_2_ab+Male_2_bb))-(Female_2_ab/(Female_2_aa+Female_2_ab+Female_2_bb)))
bb_2=((Male_2_bb/(Male_2_aa+Male_2_ab+Male_2_bb))-(Female_2_bb/(Female_2_aa+Female_2_ab+Female_2_bb)))

aa_3=((Male_3_aa/(Male_3_aa+Male_3_ab+Male_3_bb))-(Female_3_aa/(Female_3_aa+Female_3_ab+Female_3_bb)))
ab_3=((Male_3_ab/(Male_3_aa+Male_3_ab+Male_3_bb))-(Female_3_ab/(Female_3_aa+Female_3_ab+Female_3_bb)))
bb_3=((Male_3_bb/(Male_3_aa+Male_3_ab+Male_3_bb))-(Female_3_bb/(Female_3_aa+Female_3_ab+Female_3_bb)))

aa_4=((Male_4_aa/(Male_4_aa+Male_4_ab+Male_4_bb))-(Female_4_aa/(Female_4_aa+Female_4_ab+Female_4_bb)))
ab_4=((Male_4_ab/(Male_4_aa+Male_4_ab+Male_4_bb))-(Female_4_ab/(Female_4_aa+Female_4_ab+Female_4_bb)))
bb_4=((Male_4_bb/(Male_4_aa+Male_4_ab+Male_4_bb))-(Female_4_bb/(Female_4_aa+Female_4_ab+Female_4_bb)))

aa_5=((Male_5_aa/(Male_5_aa+Male_5_ab+Male_5_bb))-(Female_5_aa/(Female_5_aa+Female_5_ab+Female_5_bb)))
ab_5=((Male_5_ab/(Male_5_aa+Male_5_ab+Male_5_bb))-(Female_5_ab/(Female_5_aa+Female_5_ab+Female_5_bb)))
bb_5=((Male_5_bb/(Male_5_aa+Male_5_ab+Male_5_bb))-(Female_5_bb/(Female_5_aa+Female_5_ab+Female_5_bb)))

aa_6=((Male_6_aa/(Male_6_aa+Male_6_ab+Male_6_bb))-(Female_6_aa/(Female_6_aa+Female_6_ab+Female_6_bb)))
ab_6=((Male_6_ab/(Male_6_aa+Male_6_ab+Male_6_bb))-(Female_6_ab/(Female_6_aa+Female_6_ab+Female_6_bb)))
bb_6=((Male_6_bb/(Male_6_aa+Male_6_ab+Male_6_bb))-(Female_6_bb/(Female_6_aa+Female_6_ab+Female_6_bb)))

aa_7=((Male_7_aa/(Male_7_aa+Male_7_ab+Male_7_bb))-(Female_7_aa/(Female_7_aa+Female_7_ab+Female_7_bb)))
ab_7=((Male_7_ab/(Male_7_aa+Male_7_ab+Male_7_bb))-(Female_7_ab/(Female_7_aa+Female_7_ab+Female_7_bb)))
bb_7=((Male_7_bb/(Male_7_aa+Male_7_ab+Male_7_bb))-(Female_7_bb/(Female_7_aa+Female_7_ab+Female_7_bb)))

aa_8=((Male_8_aa/(Male_8_aa+Male_8_ab+Male_8_bb))-(Female_8_aa/(Female_8_aa+Female_8_ab+Female_8_bb)))
ab_8=((Male_8_ab/(Male_8_aa+Male_8_ab+Male_8_bb))-(Female_8_ab/(Female_8_aa+Female_8_ab+Female_8_bb)))
bb_8=((Male_8_bb/(Male_8_aa+Male_8_ab+Male_8_bb))-(Female_8_bb/(Female_8_aa+Female_8_ab+Female_8_bb)))

aa_9=((Male_9_aa/(Male_9_aa+Male_9_ab+Male_9_bb))-(Female_9_aa/(Female_9_aa+Female_9_ab+Female_9_bb)))
ab_9=((Male_9_ab/(Male_9_aa+Male_9_ab+Male_9_bb))-(Female_9_ab/(Female_9_aa+Female_9_ab+Female_9_bb)))
bb_9=((Male_9_bb/(Male_9_aa+Male_9_ab+Male_9_bb))-(Female_9_bb/(Female_9_aa+Female_9_ab+Female_9_bb)))

aa_10=((Male_10_aa/(Male_10_aa+Male_10_ab+Male_10_bb))-(Female_10_aa/(Female_10_aa+Female_10_ab+Female_10_bb)))
ab_10=((Male_10_ab/(Male_10_aa+Male_10_ab+Male_10_bb))-(Female_10_ab/(Female_10_aa+Female_10_ab+Female_10_bb)))
bb_10=((Male_10_bb/(Male_10_aa+Male_10_ab+Male_10_bb))-(Female_10_bb/(Female_10_aa+Female_10_ab+Female_10_bb)))

aa_11=((Male_11_aa/(Male_11_aa+Male_11_ab+Male_11_bb))-(Female_11_aa/(Female_11_aa+Female_11_ab+Female_11_bb)))
ab_11=((Male_11_ab/(Male_11_aa+Male_11_ab+Male_11_bb))-(Female_11_ab/(Female_11_aa+Female_11_ab+Female_11_bb)))
bb_11=((Male_11_bb/(Male_11_aa+Male_11_ab+Male_11_bb))-(Female_11_bb/(Female_11_aa+Female_11_ab+Female_11_bb)))

aa_12=((Male_12_aa/(Male_12_aa+Male_12_ab+Male_12_bb))-(Female_12_aa/(Female_12_aa+Female_12_ab+Female_12_bb)))
ab_12=((Male_12_ab/(Male_12_aa+Male_12_ab+Male_12_bb))-(Female_12_ab/(Female_12_aa+Female_12_ab+Female_12_bb)))
bb_12=((Male_12_bb/(Male_12_aa+Male_12_ab+Male_12_bb))-(Female_12_bb/(Female_12_aa+Female_12_ab+Female_12_bb)))

aa_13=((Male_13_aa/(Male_13_aa+Male_13_ab+Male_13_bb))-(Female_13_aa/(Female_13_aa+Female_13_ab+Female_13_bb)))
ab_13=((Male_13_ab/(Male_13_aa+Male_13_ab+Male_13_bb))-(Female_13_ab/(Female_13_aa+Female_13_ab+Female_13_bb)))
bb_13=((Male_13_bb/(Male_13_aa+Male_13_ab+Male_13_bb))-(Female_13_bb/(Female_13_aa+Female_13_ab+Female_13_bb)))

aa_14=((Male_14_aa/(Male_14_aa+Male_14_ab+Male_14_bb))-(Female_14_aa/(Female_14_aa+Female_14_ab+Female_14_bb)))
ab_14=((Male_14_ab/(Male_14_aa+Male_14_ab+Male_14_bb))-(Female_14_ab/(Female_14_aa+Female_14_ab+Female_14_bb)))
bb_14=((Male_14_bb/(Male_14_aa+Male_14_ab+Male_14_bb))-(Female_14_bb/(Female_14_aa+Female_14_ab+Female_14_bb)))

aa_15=((Male_15_aa/(Male_15_aa+Male_15_ab+Male_15_bb))-(Female_15_aa/(Female_15_aa+Female_15_ab+Female_15_bb)))
ab_15=((Male_15_ab/(Male_15_aa+Male_15_ab+Male_15_bb))-(Female_15_ab/(Female_15_aa+Female_15_ab+Female_15_bb)))
bb_15=((Male_15_bb/(Male_15_aa+Male_15_ab+Male_15_bb))-(Female_15_bb/(Female_15_aa+Female_15_ab+Female_15_bb)))

aa_16=((Male_16_aa/(Male_16_aa+Male_16_ab+Male_16_bb))-(Female_16_aa/(Female_16_aa+Female_16_ab+Female_16_bb)))
ab_16=((Male_16_ab/(Male_16_aa+Male_16_ab+Male_16_bb))-(Female_16_ab/(Female_16_aa+Female_16_ab+Female_16_bb)))
bb_16=((Male_16_bb/(Male_16_aa+Male_16_ab+Male_16_bb))-(Female_16_bb/(Female_16_aa+Female_16_ab+Female_16_bb)))

aa_17=((Male_17_aa/(Male_17_aa+Male_17_ab+Male_17_bb))-(Female_17_aa/(Female_17_aa+Female_17_ab+Female_17_bb)))
ab_17=((Male_17_ab/(Male_17_aa+Male_17_ab+Male_17_bb))-(Female_17_ab/(Female_17_aa+Female_17_ab+Female_17_bb)))
bb_17=((Male_17_bb/(Male_17_aa+Male_17_ab+Male_17_bb))-(Female_17_bb/(Female_17_aa+Female_17_ab+Female_17_bb)))

aa_18=((Male_18_aa/(Male_18_aa+Male_18_ab+Male_18_bb))-(Female_18_aa/(Female_18_aa+Female_18_ab+Female_18_bb)))
ab_18=((Male_18_ab/(Male_18_aa+Male_18_ab+Male_18_bb))-(Female_18_ab/(Female_18_aa+Female_18_ab+Female_18_bb)))
bb_18=((Male_18_bb/(Male_18_aa+Male_18_ab+Male_18_bb))-(Female_18_bb/(Female_18_aa+Female_18_ab+Female_18_bb)))

aa_19=((Male_19_aa/(Male_19_aa+Male_19_ab+Male_19_bb))-(Female_19_aa/(Female_19_aa+Female_19_ab+Female_19_bb)))
ab_19=((Male_19_ab/(Male_19_aa+Male_19_ab+Male_19_bb))-(Female_19_ab/(Female_19_aa+Female_19_ab+Female_19_bb)))
bb_19=((Male_19_bb/(Male_19_aa+Male_19_ab+Male_19_bb))-(Female_19_bb/(Female_19_aa+Female_19_ab+Female_19_bb)))


################# writing the summary text file
Marker=c(Marker_LG1,Marker_LG2,Marker_LG3,Marker_LG4,Marker_LG5,Marker_LG6,Marker_LG7,Marker_LG8,Marker_LG9,Marker_LG10,Marker_LG11,Marker_LG12,Marker_LG13,Marker_LG14,Marker_LG15,Marker_LG16,Marker_LG17,Marker_LG18,Marker_LG19)
cm=c(cm_LG1,cm_LG2,cm_LG3,cm_LG4,cm_LG5,cm_LG6,cm_LG7,cm_LG8,cm_LG9,cm_LG10,cm_LG11,cm_LG12,cm_LG13,cm_LG14,cm_LG15,cm_LG16,cm_LG17,cm_LG18,cm_LG19)
AA=c(aa_1,aa_2,aa_3,aa_4,aa_5,aa_6,aa_7,aa_8,aa_9,aa_10,aa_11,aa_12,aa_13,aa_14,aa_15,aa_16,aa_17,aa_18,aa_19)
AB=c(ab_1,ab_2,ab_3,ab_4,ab_5,ab_6,ab_7,ab_8,ab_9,ab_10,ab_11,ab_12,ab_13,ab_14,ab_15,ab_16,ab_17,ab_18,ab_19)
BB=c(bb_1,bb_2,bb_3,bb_4,bb_5,bb_6,bb_7,bb_8,bb_9,bb_10,bb_11,bb_12,bb_13,bb_14,bb_15,bb_16,bb_17,bb_18,bb_19)

DATA=cbind(group,Marker,cm,AA,AB,BB) ## creating a matrix
colnames(DATA)<-c("LG","Marker","cM","AA(m-f)","AB(m-f)","BB(m-f)") ## colnames if the matrix
write.table(DATA,"C:/YOURDESKTOP/DATA/Genome_wide_Recombination_Cross1.txt",sep=",",quote=F,col.names=T,row.names=F) ## saving file

