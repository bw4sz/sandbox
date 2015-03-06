library(XML)
library(stringr)
library(rvest)
library(dplyr)

url<-"http://git.psylab.info/r-library/src/25635d3e6d3d/bibs/en-books.bib?at=master"
p<- html(url)

p %>% html_node("url")
tables <- readHTMLTable(url,header = TRUE,trim=T)

tables[[1]]
names(tables)<-"t"
str(tables)
string<-as.character(tables[[1]][1,"V2"])

#get everythhing after url
r<-str_extract_all(string,"(http:.*?})")[[1]]

#oops looks like we have a trailing brace
r<-sapply(r,function(x){
gsub(x,pattern="}",replacement="")
})

#only get the pdfs
r<-r[str_detect(r,".pdf")]

#download and name
for(x in 1:length(r)){
  book<-r[x]
#get filename
filnam<-str_match(book,"(\\w+).pdf")[,1]

#download each
download.file(book,paste("C:\\Users\\Ben\\Dropbox\\Rbooks",filnam,sep="\\"),mode="wb")
}