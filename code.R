library("qgraph")
library("psych")
library("dplyr")
library("bootnet")
library("tidyverse")
library("ggplot2")
library("bootnet")  
library("NetworkComparisonTest")

###data
mydata  <- read.csv("C:/Users/yangt/Desktop/paper/网络分析-心理安全感/BMC/返修/PSS.csv")
items <- mydata[,9:44]
dimensions  <- mydata[,45:51]


####Total sample network of psychological safety
##Fig.1a
myplot_dimensions<-plot(net_dimensions <- estimateNetwork(dimensions, default = "EBICglasso", tuning = 0.5,
                                 corMethod="cor",corArgs=list(method="spearman",                                
                                  use="pairwise.complete.obs")),
     layout = "spring",
     label.cex = 1, 
     negDashed = T, 
     legend=T,
     legend.cex = 1,)
summary(net_dimensions )
aen_glasso_dimensions <- qgraph::EBICglasso(cor(dimensions), n=9282, gamma = 0.1)  
aen_glasso_dimensions

##Fig.1b
myplot_items<-plot(net_items<- estimateNetwork(items, default = "EBICglasso", tuning = 0.5,
     corMethod="cor",corArgs=list(method="spearman",                                 
     use="pairwise.complete.obs")),
     layout = "spring",
     groups = list("GS" = 1:10, 
                   "CE" = 11:15,
                   "NPF" = 16:22,
                   "CA" = 23:25,
                   "TU" = 26:30,
                   "RE" = 31:33,
                   "EX" = 34:36),
     label.cex = 1, 
     negDashed = T, 
     legend=T,
     legend.cex = 0.5,)
summary(net_items )
aen_glasso_items <- qgraph::EBICglasso(cor(items), n=9282, gamma = 0.1)  
aen_glasso_items


###Estimation of centrality indicators
##Fig.2a
centrality_dimensions <- centralityTable(net_dimensions)
centrality_dimensions
centralityPlot(net_dimensions, include=c("Strength",
                            'Closeness',
                            "Betweenness"),
               scale = "z-scores")

## Fig.2b
centrality_items <- centralityTable(net_items)
centrality_items
centralityPlot(net_items,
               include=c("Strength","Betweenness","Closeness"),
               scale = "z-scores")

##Fig. S1a)
EdgeWgt_dimensions <- bootnet(net_dimensions,
                              nBoots=1000,
                              nCores = 14,
                              statistics = c("Strength"))
plot(EdgeWgt_dimensions,"Strength")


#Fig. S1b
EdgeWgt_items <- bootnet(net_items,
                              nBoots=1000,
                              nCores = 14,
                              statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_items,"Strength")


###Bootstrapped Difference Test (Edge weight accuracy)
##Fig. S2a
EdgeWgt_dimensions <- bootnet(net_dimensions,
                   nBoots=1000,
                   nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_dimensions,labels=T,order="sample")

##Fig. S2b
EdgeWgt_items<- bootnet(net_items,
                              nBoots=1000,
                              nCores = 14)

dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_items,labels=F,order="sample")


###Stability estimation of centrality indicators
##Fig. S3a
Centrality_Stability_dimensions <- bootnet(net_dimensions, 
                                nBoots = 1000, 
                                type = "case",
                                nCores = 14, 
                                statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(Centrality_Stability_dimensions,statistics = "Strength")



##Fig. S3b
Centrality_Stability_items <- bootnet(net_items, 
                                           nBoots = 1000, 
                                           type = "case",
                                           nCores = 14, 
                                           statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(Centrality_Stability_items,statistics = "Strength")


#Correlation Stability Coefficient
corStability(Centrality_Stability_dimensions)
corStability(Centrality_Stability_items)



######             ######
#Gender network 

######             #####
###Fig.3a
mydata_male <- mydata[mydata$gender == '1', ]
male_dimensions  <- mydata_male[,45:51]
myplot_male_dimensions<-plot(net_male_dimensions <- 
                               estimateNetwork(male_dimensions, 
                                default = "EBICglasso", tuning = 0.5,
                                corMethod="cor",corArgs=list(method="spearman",                                 
                                 use="pairwise.complete.obs")),
                        layout = "spring",
                        label.cex = 1, 
                        negDashed = T, 
                        legend=T,
                        legend.cex = 1,)
summary(net_male_dimensions )
aen_glasso_male_dimensions <- qgraph::EBICglasso(cor(male_dimensions), n=4664, gamma = 0.1)  
view(aen_glasso_male_dimensions)

###Fig.3b
mydata_female <- mydata[mydata$gender == '2', ]
female_dimensions  <- mydata_female[,45:51]
myplot_female_dimensions<-plot(net_female_dimensions <- 
                                 estimateNetwork(female_dimensions, 
                                  default = "EBICglasso", tuning = 0.5,
                                  corMethod="cor",corArgs=list(method="spearman",                                 
                                  use="pairwise.complete.obs")),
                        layout = "spring",
                        label.cex = 1, 
                        negDashed = T, 
                        legend=T,
                        legend.cex = 1,)
summary(net_female_dimensions )
aen_glasso_female_dimensions <- qgraph::EBICglasso(cor(female_dimensions), n=4618, gamma = 0.1)  
view(aen_glasso_female_dimensions)

##Fig.3c
male_items  <- mydata_male[,9:44]
myplot_male_items<-plot(net_male_items<- estimateNetwork(male_items, 
                                         default = "EBICglasso", tuning = 0.5,
                                         corMethod="cor",corArgs=list(method="spearman",                                 
                                         use="pairwise.complete.obs")),
                   layout = "spring",
                   groups = list("GS" = 1:10, 
                                 "CE" = 11:15,
                                 "NPF" = 16:22,
                                 "CA" = 23:25,
                                 "TU" = 26:30,
                                 "RE" = 31:33,
                                 "EX" = 34:36),
                   label.cex = 1, 
                   negDashed = T, 
                   legend=T,
                   legend.cex = 0.5,)
summary(net_male_items )
aen_glasso_male_items <- qgraph::EBICglasso(cor(male_items), n=4664, gamma = 0.1)  
view(aen_glasso_male_items)


##Fig.3d
female_items  <- mydata_female[,9:44]
myplot_female_items<-plot(net_female_items<- estimateNetwork(female_items, 
                                              default = "EBICglasso", tuning = 0.5,
                                              corMethod="cor",corArgs=list(method="spearman",                                 
                                              use="pairwise.complete.obs")),
                   layout = "spring",
                   groups = list("GS" = 1:10, 
                                 "CE" = 11:15,
                                 "NPF" = 16:22,
                                 "CA" = 23:25,
                                 "TU" = 26:30,
                                 "RE" = 31:33,
                                 "EX" = 34:36),
                   label.cex = 1, 
                   negDashed = T, 
                   legend=T,
                   legend.cex = 0.5,)
summary(net_female_items )
aen_glasso_female_items <- qgraph::EBICglasso(cor(female_items), n=4618, gamma = 0.1)  
view(aen_glasso_female_items)

###Estimation of centrality indicators in gender networks
##Fig.4a
centrality_male_dimensions <- centralityTable(net_male_dimensions)
centrality_female_dimensions <- centralityTable(net_female_dimensions)
dev.new(width = 15, height = 15) 
centralityPlot(include=c("Strength"),
               list(male=net_male_dimensions, female=net_female_dimensions))
centrality_male_dimensions
centrality_female_dimensions


#Fig.4b
centrality_male_items <- centralityTable(net_male_items)
centrality_female_items <- centralityTable(net_female_items)
dev.new(width = 15, height = 15) 
centralityPlot(include=c("Strength"),
               list(male=net_male_items, female=net_female_items))
centrality_male_items
centrality_female_items

#Comparison of Differences in Dimension Centrality Indicators
set.seed(123)
NCT_centrality_gender_dimensions <- NCT(net_male_dimensions, net_female_dimensions,
                                        it = 1000,
                                        weighted = TRUE,
                                        test.edges = TRUE, edges = "ALL",  
                                        test.centrality = TRUE, 
                                        centrality = c("strength"),
                                        nodes = "all")
summary(NCT_centrality_gender_dimensions)

sum(NCT_centrality_gender_dimensions$einv.pval$`p-value`<0.05)
(edgesSign_gender_dimensions <-  NCT_centrality_gender_dimensions$einv.pvals[which(NCT_centrality_gender_dimensions$einv.pvals[,3]<=.0500),])


#Comparison of Differences in Central Indicators of  Items
set.seed(123)
NCT_centrality_gender_items <- NCT(net_male_items,net_female_items,
                                       it = 1000,
                                        weighted=TRUE,
                                        test.edges=TRUE,edges="ALL",  
                                        test.centrality=TRUE, 
                                        centrality=c("strength"),
                                        nodes="all")
summary(NCT_centrality_gender_items)

sum(NCT_centrality_gender_items$einv.pval$`p-value`<0.05)
(edgesSign_gender_items <-  NCT_centrality_gender_items$einv.pvals[which(NCT_centrality_gender_items$einv.pvals[,3]<=.0500),])


#
EdgeWgt_male_dimensions <- bootnet(net_male_dimensions,
                              nBoots=1000,
                              nCores = 14,
                              statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_male_dimensions,"Strength")

#
EdgeWgt_female_dimensions <- bootnet(net_female_dimensions,
                              nBoots=1000,
                              nCores = 14,
                              statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_female_dimensions,"Strength")

#
EdgeWgt_male_items <- bootnet(net_male_items,
                         nBoots=1000,
                         nCores = 14,
                         statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_male_items,"Strength")

#
EdgeWgt_female_items <- bootnet(net_female_items,
                         nBoots=1000,
                         nCores = 14,
                         statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_female_items,"Strength")


###
##
EdgeWgt_male_dimensions <- bootnet(net_male_dimensions,
                              nBoots=1000,
                              nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_male_dimensions,labels=T,order="sample")

##
EdgeWgt_female_dimensions <- bootnet(net_female_dimensions,
                              nBoots=1000,
                              nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_female_dimensions,labels=T,order="sample")


##
EdgeWgt_male_items<- bootnet(net_male_items,
                        nBoots=1000,
                        nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_male_items,labels=F,order="sample")

##
EdgeWgt_female_items<- bootnet(net_female_items,
                        nBoots=1000,
                        nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_female_items,labels=F,order="sample")

###
##
Centrality_Stability_male_dimensions <- bootnet(net_male_dimensions, 
                                           nBoots = 1000, 
                                           type = "case",
                                           nCores = 14, 
                                           statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_male_dimensions,statistics = "Strength")

##
Centrality_Stability_female_dimensions <- bootnet(net_female_dimensions, 
                                           nBoots = 1000, 
                                           type = "case",
                                           nCores = 14, 
                                           statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_female_dimensions,statistics = "Strength")


##
Centrality_Stability_male_items <- bootnet(net_male_items, 
                                      nBoots = 1000, 
                                      type = "case",
                                      nCores = 14, 
                                      statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_male_items,statistics = "Strength")

##
Centrality_Stability_female_items <- bootnet(net_female_items, 
                                      nBoots = 1000, 
                                      type = "case",
                                      nCores = 14, 
                                      statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_female_items,statistics = "Strength")



#Correlation Stability Coefficient
corStability(Centrality_Stability_male_dimensions)
corStability(Centrality_Stability_female_dimensions)

corStability(Centrality_Stability_male_items)
corStability(Centrality_Stability_female_items)




######             ######
      #Age network 
######             #####
###Fig.5a
mydata_youth <- mydata[mydata$age == '1', ]
youth_dimensions  <- mydata_youth[,45:51]

myplot_youth_dimensions<-plot(net_youth_dimensions <- 
                               estimateNetwork(youth_dimensions, 
                                               default = "EBICglasso", tuning = 0.5,
                                               corMethod="cor",corArgs=list(method="spearman",                                 
                                                                            use="pairwise.complete.obs")),
                             layout = "spring",
                             label.cex = 1, 
                             negDashed = T, 
                             legend=T,
                             legend.cex = 1,)
summary(net_youth_dimensions )
aen_glasso_youth_dimensions <- qgraph::EBICglasso(cor(youth_dimensions), n=175, gamma = 0.1)  
view(aen_glasso_youth_dimensions)

###Fig.5b
mydata_midlife <- mydata[mydata$age == '2', ]
midlife_dimensions  <- mydata_midlife[,45:51]
myplot_midlife_dimensions<-plot(net_midlife_dimensions <- 
                                 estimateNetwork(midlife_dimensions, 
                                                 default = "EBICglasso", tuning = 0.5,
                                                 corMethod="cor",corArgs=list(method="spearman",                                 
                                                                              use="pairwise.complete.obs")),
                               layout = "spring",
                               label.cex = 1, 
                               negDashed = T, 
                               legend=T,
                               legend.cex = 1,)
summary(net_midlife_dimensions )
aen_glasso_midlife_dimensions <- qgraph::EBICglasso(cor(midlife_dimensions), n=175, gamma = 0.1)  
view(aen_glasso_midlife_dimensions)

##Fig.5c
youth_items  <- mydata_youth[,9:44]
myplot_youth_items<-plot(net_youth_items<- estimateNetwork(youth_items, 
                                                         default = "EBICglasso", tuning = 0.5,
                                                         corMethod="cor",corArgs=list(method="spearman",                  
                                                                                      use="pairwise.complete.obs")),
                        layout = "spring",
                        groups = list("GS" = 1:10, 
                                      "CE" = 11:15,
                                      "NPF" = 16:22,
                                      "CA" = 23:25,
                                      "TU" = 26:30,
                                      "RE" = 31:33,
                                      "EX" = 34:36),
                        label.cex = 1, 
                        negDashed = T, 
                        legend=T,
                        legend.cex = 0.5,)
summary(net_youth_items )
aen_glasso_youth_items <- qgraph::EBICglasso(cor(youth_items), n=175, gamma = 0.1)  
view(aen_glasso_youth_items)

##Fig.5d
midlife_items  <- mydata_midlife[,9:44]
myplot_midlife_items<-plot(net_midlife_items<- estimateNetwork(midlife_items, 
                                                             default = "EBICglasso", tuning = 0.5,
                                                             corMethod="cor",corArgs=list(method="spearman",                                 
                                                                                          use="pairwise.complete.obs")),
                          layout = "spring",
                          groups = list("GS" = 1:10, 
                                        "CE" = 11:15,
                                        "NPF" = 16:22,
                                        "CA" = 23:25,
                                        "TU" = 26:30,
                                        "RE" = 31:33,
                                        "EX" = 34:36),
                          label.cex = 1, 
                          negDashed = T, 
                          legend=T,
                          legend.cex = 0.5,)
summary(net_midlife_items )
aen_glasso_midlife_items <- qgraph::EBICglasso(cor(midlife_items), n=175, gamma = 0.1)  
view(aen_glasso_midlife_items)


###Estimation of centrality indicators in age networks
##Fig.6a
centrality_youth_dimensions <- centralityTable(net_youth_dimensions)
centrality_midlife_dimensions <- centralityTable(net_midlife_dimensions)

dev.new(width = 15, height = 15) 
centralityPlot(include=("Strength"),
               list(youth=net_youth_dimensions, midlife=net_midlife_dimensions))
centrality_youth_dimensions
centrality_midlife_dimensions

#Fig.6b
centrality_youth_items <- centralityTable(net_youth_items)
centrality_midlife_items <- centralityTable(net_midlife_items)

dev.new(width = 15, height = 15) 
centralityPlot(include=c("Strength"),
               list(myouth=net_youth_items, midlife=net_midlife_items))

centrality_youth_items
centrality_midlife_items

#Comparison of Differences in Age Dimension Centrality Indicators
set.seed(123)
NCT_centrality_age_dimensions <- NCT(net_youth_dimensions,net_midlife_dimensions,
                                        it = 1000,
                                        weighted=TRUE,
                                        test.edges=TRUE,edges="ALL",
                                        test.centrality=TRUE, 
                                        centrality=c("strength"),
                                        nodes="all")
summary(NCT_centrality_age_dimensions)

sum(NCT_centrality_age_dimensions$einv.pval$`p-value`<0.05)
(edgesSign_age_dimensions <-  NCT_centrality_age_dimensions$einv.pvals[which(NCT_centrality_age_dimensions$einv.pvals[,3]<=.0500),])


#items
set.seed(123)
NCT_centrality_age_items <- NCT(net_youth_items,net_midlife_items,
                                    it = 1000,
                                   weighted=TRUE,
                                   test.edges=TRUE,edges="ALL",  
                                   test.centrality=TRUE, 
                                   centrality=c("strength"),
                                   nodes="all")
summary(NCT_centrality_age_items)

sum(NCT_centrality_age_items$einv.pval$`p-value`<0.05)
(edgesSign_age_items <-  NCT_centrality_age_items$einv.pvals[which(NCT_centrality_age_items$einv.pvals[,3]<=.0500),])


##
#
EdgeWgt_youth_dimensions <- bootnet(net_youth_dimensions,
                                   nBoots=1000,
                                   nCores = 14,
                                   statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_youth_dimensions,"Strength")

#
EdgeWgt_midlife_dimensions <- bootnet(net_midlife_dimensions,
                                     nBoots=1000,
                                     nCores = 14,
                                     statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_midlife_dimensions,"Strength")

#
EdgeWgt_youth_items <- bootnet(net_youth_items,
                              nBoots=1000,
                              nCores = 14,
                              statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_youth_items,"Strength")

#
EdgeWgt_midlife_items <- bootnet(net_midlife_items,
                                nBoots=1000,
                                nCores = 14,
                                statistics = c("Strength"))
dev.new(width = 15, height = 15) 
plot(EdgeWgt_midlife_items,"Strength")


###
##
EdgeWgt_youth_dimensions <- bootnet(net_youth_dimensions,
                                   nBoots=1000,
                                   nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_youth_dimensions,labels=T,order="sample")

##
EdgeWgt_midlife_dimensions <- bootnet(net_midlife_dimensions,
                                     nBoots=1000,
                                     nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_midlife_dimensions,labels=T,order="sample")


##
EdgeWgt_youth_items<- bootnet(net_youth_items,
                             nBoots=1000,
                             nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_youth_items,labels=F,order="sample")

##
EdgeWgt_midlife_items<- bootnet(net_midlife_items,
                               nBoots=1000,
                               nCores = 14)
dev.new(width = 0.5, height = 15) 
plot(EdgeWgt_midlife_items,labels=F,order="sample")


###
##
Centrality_Stability_youth_dimensions <- bootnet(net_youth_dimensions, 
                                                nBoots = 1000, 
                                                type = "case",
                                                nCores = 14, 
                                                statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_youth_dimensions,statistics = "Strength")

##
Centrality_Stability_midlife_dimensions <- bootnet(net_midlife_dimensions, 
                                                  nBoots = 1000, 
                                                  type = "case",
                                                  nCores = 14, 
                                                  statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_midlife_dimensions,statistics = "Strength")


##
Centrality_Stability_youth_items <- bootnet(net_youth_items, 
                                           nBoots = 1000, 
                                           type = "case",
                                           nCores = 14, 
                                           statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_youth_items,statistics = "Strength")

##
Centrality_Stability_midlife_items <- bootnet(net_midlife_items, 
                                             nBoots = 1000, 
                                             type = "case",
                                             nCores = 14, 
                                             statistics = c("Strength"))
dev.new(width = 0.5, height = 15) 
plot(Centrality_Stability_midlife_items,statistics = "Strength")



#Correlation Stability Coefficient
corStability(Centrality_Stability_youth_dimensions)
corStability(Centrality_Stability_midlife_dimensions)
corStability(Centrality_Stability_youth_items)
corStability(Centrality_Stability_midlife_items)

