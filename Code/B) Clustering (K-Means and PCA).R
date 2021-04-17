
######
# Please note: All codes work with OKCUPID_final.csv which is a preprocessed version of the original OKCupid database
######

###########################################################################
## (A) PCA Data Exploration
###########################################################################

library(dplyr)
library(ggplot2)
library(GGally)
library(ggfortify)
okc = read.csv("OKCUPID_final.csv", fileEncoding="UTF-8-BOM")
attach(okc)

## (1) Initial data exploration (histograms) ## 

names(okc)

#-A look at the data, just replace the x with a variable
ggplot(data = okc) +
  geom_histogram(mapping = aes(x = speaks), binwidth = 0.5)

ggplot(data = okc) + geom_bar(mapping = aes(x = has_cat))
mean(height)


## (2) PCA Plots ## 

# -All features
okc_all=okc[,1:79]
okc_all=okc_all[-c(2:3,5,10:11,16:17,19)]

pca=prcomp(okc_all, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'status',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_colour_manual(values = c("darkblue", "hotpink", "hotpink", "lightblue"))

# -Personal traits
okc_vars=okc[,c(1,4,8,36:42, 21:29, 79)]

pca=prcomp(okc_vars, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'status',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_colour_manual(values = c("darkblue", "hotpink", "hotpink", "lightblue"))

# -Ethnicities
ethnicity = c(1, 21:29, 18, 20)
okc_ethnicity=okc[,ethnicity]

pca=prcomp(okc_ethnicity, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'status',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_colour_manual(values = c("darkblue", "hotpink", "hotpink", "lightblue"))

# -Personal preferences
diets = c(43:48,4,6:8,12,79)
okc_diets=okc[,diets]

pca=prcomp(okc_diets, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'diet',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_colour_manual(values = c("lightblue", "pink", "hotpink", "lightpink","hotpink", "maroon"))

# -Profession
okc_prof=okc[,c(9,13:15,49:57)]
okc_prof$age = okc$age

pca=prcomp(okc_prof, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'education',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_color_gradient(low = "lightblue", high = "lightpink")

# -Kids and Pets
okc_k=okc[,c(30:35,79)]
okc_k$age = okc$age

pca=prcomp(okc_k, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'age',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_color_gradient(low = "lightblue", high = "lightpink")

# - Religion
okc_r=okc[,c(1,6,7,12,18,58:66, 30:31)]

pca=prcomp(okc_r, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE,colour = "relig_imprt",
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_color_gradient(low = "lightblue", high = "lightpink")

# -Astrological sign
okc_s=okc[,c(1,18,20,67:79)]

pca=prcomp(okc_s, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'sign_imprt',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_color_gradient(low = "lightblue", high = "lightpink")


###########################################################################
## (B) K Means with Selected Features
###########################################################################

library(dplyr)
library(ggplot2)
library(GGally)
library(ggfortify)
okc = read.csv("C:/Users/HP-073/Desktop/Master of Management in Analytics (McGill)/Fall 2020/MGSC661 - Multivariate Stats for Machine Learning/F) Final Project/OKCUPID_final.csv", fileEncoding="UTF-8-BOM")
attach(okc)

## (1) Data preprocessing ##

# - These features are combined as we saw they are very similar
# Mix diet.vegetarian and diet.vegan
okc_pp=mutate(okc, diet.vegan.vegetarian = if_else(diet.vegan == 1|diet.vegetarian == 1, 1, 0))
# Mix job.businessandmanagement and job.medicine...health
okc_pp=mutate(okc_pp, job.business.and.health = if_else(job.business.and.management == 1|job.medicine...health == 1, 1, 0))
# Mix religion.christian and religion.catholic
okc_pp=mutate(okc_pp, religion.christianity.catholicism = if_else(religion.christianity == 1|religion.catholicism == 1, 1, 0))

# - Create filtered database based on selected features 
library(dplyr)
okc_pp <- dplyr::select(okc_pp,
                           orientation.straight,orientation.gay,
                           diet.anything,diet.vegan.vegetarian, drugs,
                           white, black, asian,
                           age, education, dropped.out,
                           job.education,job.non.technical, job.business.and.health,
                           likes_cat,likes_dog,havekids,wantkids,
                           religion.christianity.catholicism,religion.atheism,religion.judaism)


## (2) Graph PCA of final list of features ##

pca=prcomp(okc_pp, scale=TRUE)
autoplot(pca, data = okc, loadings = TRUE, loadings.label = TRUE, colour = 'status',
         loadings.label.colour='blue', loadings.colour = 'magenta')+
  scale_colour_manual(values = c("darkblue", "hotpink", "hotpink", "lightblue"))


## (3) K MEANS ##

# -Standardize dataset for K Means
test.data<-scale(okc_pp)
test.data <- as.data.frame(test.data)

# -Elbow method (optimal k=7)

set.seed(123)
#install.packages('factoextra')
library(factoextra)
elbow=fviz_nbclust(test.data, kmeans, method = "wss", linecolor = 'hot pink')
elbow+geom_vline(xintercept = 7, linetype = 2, colour='darkblue')+ theme_gray(base_size = 14)

# -Apply K Means to okc_pp (non standardized dataset)
km.k=kmeans(test.data, 7) #7 clusters
test.data$cluster=as.factor(km.k$cluster)
okc_pp$cluster=test.data$cluster

# -Analyzing centroids on non standardized data 
library(dplyr)
centroids=okc_pp %>% group_by(cluster) %>% dplyr::summarize(mean(orientation.straight),mean(orientation.gay),
                                                  mean(diet.anything),mean(diet.vegan.vegetarian), mean(drugs),
                                                  mean(white),mean(black),mean(asian),
                                                  mean(age), mean(education), mean(dropped.out),
                                                  mean(job.education),mean(job.non.technical),mean(job.business.and.health),
                                                  mean(likes_cat),mean(likes_dog),mean(havekids),mean(wantkids),
                                                  mean(religion.christianity.catholicism),mean(religion.atheism),mean(religion.judaism))


###########################################################################
## (C) K Means with PCA
###########################################################################

library(dplyr)
library(ggplot2)
library(GGally)
library(ggfortify)
okc_pp = read.csv("C:/Users/HP-073/Desktop/Master of Management in Analytics (McGill)/Fall 2020/MGSC661 - Multivariate Stats for Machine Learning/F) Final Project/OKCUPID_final.csv", fileEncoding="UTF-8-BOM")
attach(okc_pp)

## (1) Data preprocessing ##

#-Drop variables that are non-numeric
okc_pp=okc_pp[,-c(2,3,5,10,11,16,17,19)]


## (2) PCA ##

#-Apply PCA
pca=prcomp(okc_pp, scale=TRUE)

#-PVE to find number of PCs
pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))

#-Transform dataset to PCA form
test.data <- predict(pca, newdata =okc_pp)
test.data <- as.data.frame(test.data)
test.data<-scale(test.data)
test.data <- as.data.frame(test.data)
test.data=test.data[,c(1:45)]


## (3) K Means ##

library(ggplot2)
plot=ggplot(test.data,aes(y=PC2, x=PC1))
km.k=kmeans(test.data, 7) #7 clusters
test.data$cluster=as.factor(km.k$cluster)
attach(test.data)
plot+geom_point(aes(colour=cluster))