library(RISmed)
library(ggplot2)

#Pubmed search terms
query <- "(cortex OR subcortical OR brain) AND (cognitive OR cognition OR perception OR (decision AND making) OR attention OR ((recall OR recognition OR declarative) AND memory) OR emotion)"

#Scrape publication data using NCBI E-Utilities API
pubmed_search <- EUtilsSummary(query, type="esearch",db = "pubmed",mindate=1980, maxdate=2014, retmax=150000) #
QueryCount(pubmed_search)
pubmed_records <- EUtilsGet(pubmed_search)
years <- Year(pubmed_records)
pubmed_pubs_count <- as.data.frame(table(years))

#Get count of pubs per year
total <- NULL
for (i in 1980:2014){
peryear <- EUtilsSummary("", type="esearch", db="pubmed", mindate=i, maxdate=i)
total[i] <- QueryCount(peryear)
}
year <- 1980:2014
total_pubs_count<- as.data.frame(cbind(year,total[year]))
names(total_pubs_count) <- c("year","Total_publications")
names(pubmed_pubs_count) <-  c("year","PubMed_publications")
pubs_year <-  merge(pubmed_pubs_count,total_pubs_count,by="year")
#Normalize publication counts by total pubs that year
pubs_year$PubMed_publications_normalized <-  pubs_year$PubMed_publications *100000 / pubs_year$Total_publications

#write as tab delimited
write.table(pubs_year,"PubMed_publications_per_year.txt",quote=F,sep="\t",row.names=F)
 
#Get top journals to calculate number of publications per journal
journal <- MedlineTA(pubmed_records)
pubmed_journal_count <- as.data.frame(table(journal))
pubmed_journal_count_top25 <- pubmed_journal_count[order(-pubmed_journal_count[,2]),][1:25,]
 
journal_names <- paste(pubmed_journal_count_top25$journal,"[jo]",sep="")
 
total_journal <- NULL
for (i in journal_names){
perjournal <- EUtilsSummary(i, type='esearch', db='pubmed',mindate=1980, maxdate=2014)
total_journal[i] <- QueryCount(perjournal)
}
 
journal_pubmed_total <- cbind(pubmed_journal_count_top25,total_journal)
names(journal_pubmed_total) <- c("journal","PubMed_publications","Total_publications")
journal_pubmed_total$PubMed_publications_normalized <- journal_pubmed_total$PubMed_publications / journal_pubmed_total$Total_publications

#write as tab delimited
write.table(journal_pubmed_total,"PubMed_publications_per_journal.txt",quote=F,sep="\t",row.names=F)


pubs_per_year <- read.table("PubMed_publications_per_year.txt",header = T,sep="\t")
#pubs_per_journal <- read.table("PubMed_publications_per_journal.txt",header = T,sep="\t")

#Plot
ggplot(pubs_per_year,aes(year, PubMed_publications_normalized)) + geom_line (colour="blue",size=2) +
xlab("Year") +
ylab("CogNeuro/100000 articles")+
ggtitle("CogNeuro PubMed articles")
 
# ggplot(pubs_per_journal,aes(journal, PubMed_publications,fill=journal)) + geom_bar(stat="identity")+
# coord_flip()+
# theme(legend.position="none")
 
# ggplot(pubs_per_journal ,aes(journal, PubMed_publications_normalized,fill=journal)) + geom_bar(stat="identity")+
# coord_flip()+
# theme(legend.position="none")


 
