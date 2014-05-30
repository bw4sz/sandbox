Visualizing interaction networks: An example using faculty abstracts
========================================================

I've been playing around with how best to visualize single level networks with pairwise interactions. 

Here i took 10 abstract for each member of the Stony Brook Faculty and performed the following analysis

1. Decomposed each abstract into a word cloud
2. Created a count of each word for each faculty member
3. Compared word usage among faculty members
4. Created a distance matrix among each pair of faculty
5. Created an interaction matrix


```r

opts_chunk$set(warnings = FALSE, messages = FALSE, dpi = 100)
# Script to take in vector of names to create word clouds and network of
# abstract word interactions ie to show how the department is connected

# load libraries

library("XML")
library("stringr")
library("RCurl")
```

```
## Loading required package: bitops
```

```r
library("wordcloud")
```

```
## Loading required package: Rcpp
## Loading required package: RColorBrewer
```

```r
library("tm")
require(reshape2)
```

```
## Loading required package: reshape2
```

```r
require(sna)
```

```
## Loading required package: sna
## sna: Tools for Social Network Analysis
## Version 2.3-2 created on 2014-01-13.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##  For citation information, type citation("sna").
##  Type help(package="sna") to get started.
```

```r
require(bipartite)
```

```
## Loading required package: bipartite
## Loading required package: vegan
## Loading required package: permute
## Loading required package: lattice
## This is vegan 2.0-10
##  This is bipartite 2.04
##  For latest changes see versionlog in  ?"bipartite-package".
##  For citation see: citation("bipartite").
##  Have a nice time plotting and analysing two-mode networks.
```

```r
require(igraph)
```

```
## Loading required package: igraph
## 
## Attaching package: 'igraph'
## 
## The following objects are masked from 'package:sna':
## 
##     %c%, betweenness, bonpow, closeness, degree, dyad.census,
##     evcent, hierarchy, is.connected, neighborhood, triad.census
```

```r

# source functions
getAbstracts <- function(author, university, dFrom, dTill, nRecs) {
    # For more details about Pubmed queries see:
    # http://www.ncbi.nlm.nih.gov/books/NBK25500/
    
    # Text search - basic URL
    eSearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="
    # Data record download - basic URL
    eDDownload <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id="
    
    # In case of multiple words (e.g., first and the last name), add '+' sign in
    # between them
    aL <- str_replace_all(author, " ", "+")
    # Add the search keyword - author
    aQ <- paste(aL, "[author]", sep = "")
    
    # add institution?
    
    if (exists("university")) {
        aU <- str_replace_all(university, " ", "+")
        # Add the search keyword - affiliation
        aUU <- paste(aL, "[ad]", sep = "")
    }
    
    # Format the publication date and add the search keyword - pdat If only one
    # year is provided, use that year, otherwise use year_1:year_2
    dQ <- ""
    
    if ((str_length(dFrom) > 0) & (str_length(dTill) > 0)) {
        d1 <- paste(dFrom, dTill, sep = ":")
        dQ <- paste(d1, "[pdat]", sep = "")
    }
    
    if ((str_length(dFrom) > 0) & (str_length(dTill) == 0)) 
        dQ <- paste(dFrom, "[pdat]", sep = "")
    
    if ((str_length(dTill) > 0) & (str_length(dFrom) == 0)) 
        dQ <- paste(dTill, "[pdat]", sep = "")
    
    # Add two seqrch queries together
    hlpQ1 <- aQ
    
    if (str_length(dQ) > 0) 
        hlpQ1 <- paste(aQ, dQ, sep = "+")
    
    # Add the max number of retrieved articles at the end of the query
    rmQ <- paste("&retmax=", nRecs, sep = "")
    hlpQ2 <- paste(hlpQ1, rmQ, sep = "")
    
    if (exists("university")) {
        hlpQ3 <- paste(hlpQ2, aUU, sep = "+")
    }
    
    # Finalize the query and serch Pubmed
    searchUrl <- paste(eSearch, hlpQ2, sep = "")
    # Wait - to ensure that all requests will be processed
    Sys.sleep(3)
    hlpURL <- getURL(searchUrl)
    # The result is in form of XML document - you can paste the searchUrl in the
    # browser to see/download it
    doc <- xmlTreeParse(hlpURL, asText = TRUE)
    IdlistHlp = xmlValue(doc[["doc"]][["eSearchResult"]][["IdList"]])
    
    # I am sure there is more elegant way (i.e., a function) to proccess this,
    # but I was lazy to search for it
    if (length(IdlistHlp) > 0) {
        Idlist <- c()
        
        # Each ID is 8 digits long
        for (k in 1:(str_length(IdlistHlp)/8)) Idlist <- c(Idlist, str_sub(IdlistHlp, 
            start = 8 * (k - 1) + 1, end = k * 8))
        
        # Once we retrieved articles' IDs for the author/dates, we can process them
        # and get abstracts
        Sys.sleep(2)
        hlp1 <- paste(eDDownload, paste(Idlist, collapse = ",", sep = ""), sep = "")
        hlp2 <- paste(hlp1, "&rettype=abstract", sep = "")
        testDoc <- xmlTreeParse(hlp2, useInternalNodes = TRUE)
        topFetch <- xmlRoot(testDoc)
        abst <- xpathSApply(topFetch, "//Abstract", xmlValue)
    }
    
    # In case that nothing was found
    if (length(IdlistHlp) == 0) 
        abst = c("Zero", "Articles", "Found")
    
    abst
}

plotWC <- function(abstracts, nc, cs) {
    # Once we have abstracts, we can create a document corpus
    abstTxt <- Corpus(VectorSource(abstracts))
    
    text2.corpus = tm_map(abstTxt, removePunctuation)
    text2.corpus = tm_map(text2.corpus, tolower)
    text2.corpus = tm_map(text2.corpus, removeWords, stopwords("english"))
    
    # Transform it into a matrix and sort based on the total word occurence
    tdm <- TermDocumentMatrix(text2.corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    
    # Select the color scheme
    pal2 <- brewer.pal(nc, cs)
    
    # And plot the cloud
    wordcloud(d$word, d$freq, scale = c(8, 0.2), min.freq = 2, max.words = 50, 
        random.order = FALSE, rot.per = 0.15, color = pal2, vfont = c("sans serif", 
            "plain"))
}
```


Individual faculty: Catherine Graham
==================================


```r

# Get inputs, download abstracts, and create a corresponding wordcloud

# Run test
abs <- getAbstracts(author = "Catherine H. Graham", "Stony Brook", 2010, 2014, 
    10)

# plot the abstracts, the 2nd and third argument are the color brewer
# ?brewer.pal, number of colors and palette
plotWC(abs, 8, "Accent")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


All faculty
=============


```r

# Step two make a network of the participants

profs <- c("Heather J. Lynch", "Catherine H. Graham", "Lev R. Ginzburg", "H. Resit Akcakaya", 
    "Dianna K. Padilla", "John R. True", "Walt F Eanes", "Mike Bell", "Jeffrey Levinton", 
    "Brenna Henn", "Liliana M. Davalos", "Joshua S. Rest", "Jessica Gurevitch", 
    "Stephen B. Baines", "Douglas Futuyma")
abs_all <- lapply(profs, function(x) {
    abs <- getAbstracts(x, "Stony Brook", 1990, 2014, 15)
})

# plot all as one word cloud
plotWC(abs_all, 8, "Accent")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Decompose words into frequency counts
==========


```r

# seperate into individual matrices

me <- lapply(abs_all, function(x) {
    abstTxt <- Corpus(VectorSource(x))
    
    text2.corpus = tm_map(abstTxt, removePunctuation)
    text2.corpus = tm_map(text2.corpus, tolower)
    text2.corpus = tm_map(text2.corpus, removeWords, stopwords("english"))
    
    # Transform it into a matrix and sort based on the total word occurence
    tdm <- TermDocumentMatrix(text2.corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
})

names(me) <- profs
mem <- melt(me)
```

```
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
## Using word as id variables
```

```r

mem$word <- as.character(mem$word)

# what are the strongest interacting words
word_matrix <- acast(mem, L1 ~ word, fill = 0)
```


Create distance matrix
========


```r
# as distance matrix
dist_matrix <- dist(word_matrix)

# To do only keep shared words?

g <- graph.adjacency(as.matrix(dist_matrix), diag = FALSE, mode = "lower", weighted = TRUE)

# names of the vertices you just imported:
V(g)$name
```

```
##  [1] "Brenna Henn"         "Catherine H. Graham" "Dianna K. Padilla"  
##  [4] "Douglas Futuyma"     "H. Resit Akcakaya"   "Heather J. Lynch"   
##  [7] "Jeffrey Levinton"    "Jessica Gurevitch"   "John R. True"       
## [10] "Joshua S. Rest"      "Lev R. Ginzburg"     "Liliana M. Davalos" 
## [13] "Mike Bell"           "Stephen B. Baines"   "Walt F Eanes"
```

```r
E(g)$size
```

```
## NULL
```

```r

plot.igraph(g, layout = layout.fruchterman.reingold, edge.color = "black", edge.width = E(g)$weight/50)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r

# color by weight
cols <- gray(E(g)$weight/max(E(g)$weight))

plot.igraph(g, layout = layout.fruchterman.reingold, edge.color = cols, edge.width = E(g)$weight/50)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

```r

# that was grey, try color

colramp <- colorRampPalette(c("blue", "red"))(length(E(g)$weight))

# original order
orig <- E(g)$weight/max(E(g)$weight)

orig.order <- data.frame(orig, 1:length(orig))

weight.order <- orig.order[order(E(g)$weight/max(E(g)$weight)), ]

# merge with col
colramp.w <- data.frame(weight.order, colramp)

# get original order
colsRB <- colramp.w[order(colramp.w$X1.length.orig.), ]

plot.igraph(g, layout = layout.fruchterman.reingold, edge.color = as.character(colsRB$colramp), 
    edge.width = (E(g)$weight/50))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-53.png) 

```r

mem$word <- as.character(mem$word)


# make another prettier graph

## Test
mem[order(mem$value, decreasing = TRUE), ][1:20, ]
```

```
##               word variable value                  L1
## 5202   populations     freq    64         Brenna Henn
## 693        species     freq    57 Catherine H. Graham
## 1794       species     freq    55   H. Resit Akcakaya
## 5203       african     freq    38         Brenna Henn
## 1795        change     freq    32   H. Resit Akcakaya
## 694   phylogenetic     freq    31 Catherine H. Graham
## 1796       climate     freq    31   H. Resit Akcakaya
## 695           data     freq    30 Catherine H. Graham
## 5204       genetic     freq    27         Brenna Henn
## 5205          data     freq    26         Brenna Henn
## 5206         human     freq    24         Brenna Henn
## 5207    population     freq    23         Brenna Henn
## 696       patterns     freq    22 Catherine H. Graham
## 1797        models     freq    22   H. Resit Akcakaya
## 6177          data     freq    22  Liliana M. Davalos
## 6178     evolution     freq    22  Liliana M. Davalos
## 697      diversity     freq    21 Catherine H. Graham
## 3128   populations     freq    21        John R. True
## 5208        africa     freq    21         Brenna Henn
## 6179 morphological     freq    21  Liliana M. Davalos
```


Trying different layouts
===


```r
plot.igraph(g, layout = layout.lgl, edge.color = as.character(colsRB$colramp), 
    edge.width = (E(g)$weight/50))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

```r

layout <- layout.reingold.tilford(g, circular = T)

plot.igraph(g, layout = layout, edge.color = as.character(colsRB$colramp), edge.width = (E(g)$weight/50))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 

```r


plot.igraph(g, layout = layout.auto, edge.color = as.character(colsRB$colramp), 
    edge.width = (E(g)$weight/50))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-63.png) 
