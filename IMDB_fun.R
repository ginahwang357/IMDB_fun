#IMDB DATAMINING FUN
#ALL THE LIBRARIES
install.packages("devtools")
find_rtools()
library("devtools")
install_github("hadley/rvest")
install_github("smbache/magrittr")
library(magrittr)
library(rvest)
library(RCurl)
library(XML)


#graphical packages
install.packages('ggplot2', dependencies = TRUE)
library("ggplot2")
library("plyr")

#ALL THE HTMLS
imdburl = "http://www.imdb.com/chart/top"
sitepage = htmlParse(imdburl)
linksineed = getNodeSet(sitepage, "//*/table[@class='chart']/tbody/tr/td[@class='titleColumn']/a/@href")

allthelinks = paste("http://www.imdb.com", linksineed, sep="")

#Simplertestcases
sampleof10 = allthelinks[1:10]
sampleof100 = allthelinks[1:100]

#get cast list
getcast = function(thelink) {
  casey = html(thelink) %>% html_nodes("#titleCast .itemprop span") %>% html_text()
  return(casey)
}

#if not list of 15 add "None" to list
addontovec = function(vec) {
  if (length(vec) != 15) {
    currentlength = length(vec)
    while (currentlength != 15) {
      vec[currentlength + 1] = NA
      currentlength = length(vec)
    }
  }
  return(vec)
}

#GIVES CAST FOR EVERY MOVIE IN MATRIX
rvestforallm = function(arr) {
  count = length(arr)
  D = matrix( c(rep(0, count * 15)), nrow = count, ncol=15)
  cast = c(rep, 15)
  for (i in 1:count) {
    page = html(arr[i])
    cast  = page %>% html_nodes("#titleCast .itemprop span") %>% html_text()
    if (length(cast) != 15) {
      cast = addontovec(cast)
    }
    D[i, ] = cast
  }
  return(D)
}

#GIVES CAST FOR EVERY MOVIE IN VECTOR
rvestforallv = function(arr) {
  count = length(arr)
  D = c(rep(0, count * 15))
  cast = c(rep, 15)
  end = 0
  repeated = 1
  for (i in 1:count) {
    print(repeated)
    page = html(arr[i])
    cast  = page %>% html_nodes("#titleCast .itemprop span") %>% html_text()
    if (length(cast) != 15) {
      cast = addontovec(cast)
    }
    D[(end + 1):(15*repeated)] = cast
    end = 15*repeated
    repeated = repeated + 1
  }
  return(D)
}

#colour
rhg_cols = c(rep(c("#771C19", "#AA3929"), 12), "#771C19")

#sampleof100
dataframe100 = data.frame(cast=rvestforallv(sampleof100))
table100 = table(dataframe100$cast)
backto100 = as.data.frame(table100)
s100 = backto100[order(-backto100$Freq), ]
#top10peop
top10peop = head(s100, 10)
top10peop$ordered = reorder(top10peop$Var1, top10peop$Freq)

top10peop=top10peop[order(nrow(top10peop):1),] 
ggplot(top10peop, aes(y= Freq)) + geom_bar(aes(x=ordered), data=top10peop, stat="identity", fill=rhg_cols ) + 
  ggtitle('Top 25 Actors/Actresses') + labs(x="Name", y="Frequency") +
  theme(
    axis.text.x = element_text(vjust=0.6, hjust = 1), 
    plot.title = element_text(size=20, face="bold", vjust=2),
    axis.title.x = element_text(color="#771C19", vjust=-0.35),
    axis.title.y = element_text(color="#771C19" , vjust=0.35)) + coord_flip()

#top25peop
top25peop = head(s100, 25)
top25peop$ordered = reorder(top25peop$Var1, top25peop$Freq)

top25peop=top25peop[order(nrow(top25peop):1),] 
ggplot(top25peop, aes(y = Freq)) + geom_bar(aes(x=ordered), data=top25peop, stat="identity", fill=rhg_cols ) + 
  ggtitle('Top 25 Actors/Actresses') + labs(x="Name", y="Frequency") +
  theme(
    axis.text.x = element_text(vjust=0.6, hjust = 1), 
    plot.title = element_text(size=20, face="bold", vjust=2),
    axis.title.x = element_text(color="#771C19", vjust=-0.35),
    axis.title.y = element_text(color="#771C19" , vjust=0.35)) + coord_flip()

#LETSDOIT
#sampleof250
fdata = data.frame(cast=rvestforallv(allthelinks))
ftable = table(fdata$cast)
f100 = as.data.frame(ftable)
ff100 = f100[order(-f100$Freq), ]
#top10peop
f10p = head(ff100, 10)
f10p$ordered = reorder(f10p$Var1, f10p$Freq)

f10p=f10p[order(nrow(f10p):1),] 
ggplot(f10p, aes(y= Freq)) + geom_bar(aes(x=ordered), data=f10p, stat="identity", fill=rhg_cols ) + 
  ggtitle('Top 25 Actors/Actresses') + labs(x="Name", y="Frequency") +
  theme(
    axis.text.x = element_text(vjust=0.6, hjust = 1), 
    plot.title = element_text(size=20, face="bold", vjust=2),
    axis.title.x = element_text(color="#771C19", vjust=-0.35),
    axis.title.y = element_text(color="#771C19" , vjust=0.35)) +  coord_flip() 

#top25peop
f25p = head(ff100, 25)
f25p$ordered = reorder(f25p$Var1, f25p$Freq)

f25p=f25p[order(nrow(f25p):1),] 


ggplot(f25p, aes(y = Freq)) + geom_bar(aes(x=ordered), data=f25p, stat="identity", fill=rhg_cols ) + 
  ggtitle('Top 25 Actors/Actresses') + labs(x="Name", y="Frequency") +
  theme(
    axis.text.x = element_text(vjust=0.6, hjust = 1), 
    plot.title = element_text(size=20, face="bold", vjust=2),
    axis.title.x = element_text(color="#771C19", vjust=-0.35),
    axis.title.y = element_text(color="#771C19" , vjust=0.35)) +  coord_flip() 
#specifics
topperson = head(ff100, 1)
numberofones = sum(f100$Freq == 1)