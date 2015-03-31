---
title: "Null Communities"
author: "Ben Weinstein"
date: "Monday, March 30, 2015"
output: html_document
---

Say you have a phylogeny like the one in the picante package.

```{r}
#Load packages
library(picante)
library(ape)
library(ggplot2)
library(dplyr)
library(reshape)
```

The picante package has some fun fake data to play with

```{r, echo=FALSE}
#load data
data(phylocom)

tree<-phylocom$phylo

#plot tree
plot(tree)
```

Lets make five observed assemblages
```{r}
#Richness is drawn from a poisson (lambda=8)
r<-rpois(5,8)

#sample observed species from the phylogeny without replacement
samp<-function(x){ sample(size=x,tree$tip.label,replace=F)}
pres<-lapply(r,samp)
```

Tidy up the dataframe to format the way required by picante. Columns as species, rows as sites.
```{r}
m<-melt(pres)
siteXspp<-acast(m,L1~value,fill=0,fun.aggregate=length)
```

So now we have a site by species matrix. This is normally what you would get from the field data

#Calculate Phylogenetic Betadiversity

```{r}
phylo_beta<-as.matrix(phylosor(siteXspp,tree))

