  library(ggplot2)
  library(RColorBrewer)
  data<-read.csv("F:\\Clusters.csv")
  
  cluster1 <- subset(data, data$Assignments=='Cluster1')
  cluster2 <- subset(data, data$Assignments=='Cluster2')
  cluster3 <- subset(data, data$Assignments=='Cluster3')
  
 
  cluster1_LOS <- mean(cluster1$LOS2)
  cluster1_TTS <- mean(cluster1$TTS)
  home1<- nrow(subset(cluster1, cluster1$Discharge_Destination==0))/nrow(cluster1)
  avgAge1 <- mean(cluster1$Age2)
  male1 <-nrow(subset(cluster1, cluster1$Sex==1))/nrow(cluster1)
  female1 <- nrow(subset(cluster1, cluster1$Sex==2))/nrow(cluster1)
  frag1<- nrow(subset(cluster1, cluster1$Fragility==1))/nrow(cluster1)
  frac1 <- nrow(subset(cluster1, cluster1$Fracture_Type==4))/nrow(cluster1)
  
  
  
  cluster2_LOS <- mean(cluster2$LOS2)
  cluster2_TTS <- mean(cluster2$TTS)
  home2<- nrow(subset(cluster2, cluster2$Discharge_Destination==0))/nrow(cluster2)
  avgAge2 <- mean(cluster2$Age2)
  male2 <-nrow(subset(cluster2, cluster2$Sex==1))/nrow(cluster2)
  female2 <- nrow(subset(cluster2, cluster2$Sex==2))/nrow(cluster2)
  frag2<- nrow(subset(cluster2, cluster2$Fragility==1))/nrow(cluster2)
  frac2 <- nrow(subset(cluster2, cluster2$Fracture_Type==4))/nrow(cluster2)
  
  cluster3_LOS <- mean(cluster3$LOS2)
  cluster3_TTS <- mean(cluster3$TTS)
  home3<- nrow(subset(cluster3, cluster3$Discharge_Destination==0))/nrow(cluster3)
 
  avgAge3 <- mean(cluster3$Age2)
  male3 <-nrow(subset(cluster3, cluster3$Sex==1))/nrow(cluster3)
  female3 <- nrow(subset(cluster3, cluster3$Sex==2))/nrow(cluster3)
  frag3<- nrow(subset(cluster3, cluster3$Fragility==1))/nrow(cluster3)
  frac3 <- nrow(subset(cluster3, cluster3$Fracture_Type==4))/nrow(cluster3)
  

  
  boxplot( LOS2 ~ Assignments, data, xlab="Cluster", ylab="LOS" , col=I("#3366FF"))
  boxplot( TTS ~ Assignments, data, xlab="Cluster", ylab="TTS" )
  boxplot( Age2 ~ Assignments, data, xlab="Cluster", ylab="Age" )
  
  #p <- ggplot(data, aes(factor(Assignments), LOS2))
  #p + geom_boxplot(fill = "grey80",outlier.colour = "#3366FF", outlier.size = 3, colour = I("#3366FF"))

  #LOS
  cols <- brewer.pal(3, "Set3")# No. of colors, and name of the palette
  ggplot(data, aes(factor(Assignments), y = LOS2))  + 
    stat_boxplot(geom ='errorbar', stat_params = list(width = 0.5)) + 
    geom_boxplot(fill = cols,outlier.colour = "black", outlier.size = 3) +
    xlab("Discovered Clusters of Patients") +
    ylab("Length of Stay (Days)")
    
  #TTS
  cols <- brewer.pal(3, "Set3")
  ggplot(data, aes(factor(Assignments), y = TTS))  + 
    stat_boxplot(geom ='errorbar', stat_params = list(width = 0.5)) + 
    geom_boxplot(fill = cols,outlier.colour = "black", outlier.size = 3) +
    xlab("Discovered Clusters of Patients") +
    ylab("Time to Surgery(Days)") 
    #scale_y_discrete(limits=c(2:2:10))
  
  #Age
  cols <- brewer.pal(3, "Set3")
  ggplot(data, aes(factor(Assignments), y = Age2))  + 
    stat_boxplot(geom ='errorbar', stat_params = list(width = 0.5)) + 
    geom_boxplot(fill = cols,outlier.colour = "black", outlier.size = 3) +
    xlab("Discovered Clusters of Patients") +
    ylab("Patient Age") 
  