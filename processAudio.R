#' ----
#' title: " A script for manipulation of the Fitzwilliam Museum audio crowdsourcing project"
#' author: "Daniel Pett"
#' date: "05/10/2020"
#' output: csv_document
#' ----

# Set working directory (for example as below)
setwd("~/Documents/research/micropasts/analysis/deathOnTheNile/") #MacOSX

# Create CSV directory if does not exist
if (!file.exists('csv')){
  dir.create('csv')
}

# Create archives directory if does not exist
if (!file.exists('archives')){
  dir.create('archives')
}

# Create JSON folder 
if (!file.exists('json')){
  dir.create('json')
}

# Add necessary libraries
library(jsonlite)
library(stringr)

# Load user data
# http://crowdsourced.micropasts.org/admin/users/export?format=csv (when logged in as admin)
# This saves as all_users.csv and put this in the csv folder

users <- read.csv('csv/all_users.csv', header=TRUE)
users <- users[,c("id","fullname","name")]

# Set the project name
project <- 'deathOnTheNile'

# Set the base url of the application
baseUrl <- 'http://crowdsourced.micropasts.org/project/'

# Set the task runs api path
tasks <- '/tasks/export?type=task&format=json'

# Form the export url
url <- paste(baseUrl,project, tasks, sep='')
archives <- paste('archives/',project,'Tasks.zip', sep='')

# Import tasks from json, this method has changed due to coding changes by SciFabric to their code
download.file(url,archives)
unzip(archives)
taskPath <- paste('json/', project, '.json', sep='')
rename <- paste(project, '_task.json', sep='')
file.rename(rename, taskPath)

# Read json files
which(lapply(readLines(taskPath), function(x) tryCatch({jsonlite::fromJSON(x); 1}, error=function(e) 0)) == 0)
trT <- fromJSON(paste(readLines(taskPath), collapse=""))
trT <- cbind(trT$id,trT$info)
trTfull <- trT

trT <- trT[,c(1,2,3)]
names(trT) <- c("taskID","track","automatedTranscription")
tmp <- trT
head(tmp$track)
tmp <- apply(tmp, 2, function(x) gsub("https://fitz-audio-guide-micropasts.s3.eu-west-2.amazonaws.com/", "  ", x))
tmp <- apply(tmp, 2, function(x) gsub("Chunk", "Chunk_", x))
tmp <- data.frame(tmp)
tmp$sort <- str_sub(tmp$track, start=-6)
tmp$sort <- gsub(".mp3",'',tmp$sort)
tmp$sort <- gsub("_",'',tmp$sort)
tmp$sortTwo <- gsub("-","_",tmp$track)
tmp$sortTwo <- sapply(str_split(tmp$sortTwo, "_",  n = 2), `[`, 2)
tmp$sortTwo <- gsub(".mp3",'',tmp$sortTwo)
tmp$sortTwo <- gsub('[[:digit:]]+', '', tmp$sortTwo)
tmp$sortTwo <- gsub("_Chunk_",'',tmp$sortTwo)
head(tmp)
tmpClean <- tmp[order(tmp$sortTwo,as.numeric(tmp$sort)),] 

# Write CSV file output
csvname <- paste('csv/', 'cleanedTasks', '.csv', sep='')
write.csv(tmpClean, file=csvname,row.names=FALSE, na="")

# Import task runs from json
taskruns <- '/tasks/export?type=task_run&format=json'
urlRuns <- paste(baseUrl,project, taskruns, sep='')
archiveRuns <-paste('archives/', project, 'TasksRun.zip', sep='')
download.file(urlRuns,archiveRuns)
unzip(archiveRuns)
taskruns <- paste('json/', project, '_task_run.json', sep='')
renameRuns <-paste(project, '_task_run.json', sep='')   
file.rename(renameRuns, taskruns)

# Read the JSON
json <- fromJSON(taskruns)
transcriptionEntry <- json$info
df <- subset(json, select = c(3,4))

transcribed <- cbind(transcriptionEntry, df)
names(transcribed) <- c("transcription",  'valid', 'comments', 'userID', 'taskID')
head(transcribed)

# Add user credit
tsks <- unique(as.character(transcribed$taskID))
head(tsks)
credits <- data.frame(taskID=character(length(tsks)),inputBy=character(length(tsks)), stringsAsFactors = FALSE) #blank df to fill
for (a in 1:length(tsks)){
  atask <- transcribed[transcribed$taskID == tsks[a],]
  contribs <- sort(unique(as.numeric(as.character(atask$userID))))
  contribsNm <- users[users$id %in% contribs,]
  credits$taskID[a] <- tsks[a]
  credits$inputBy[a] <- paste(as.character(contribsNm$fullname), collapse="; ")
}

# Merge task summaries with image URL and user credit data.
credurl <- merge(credits, trT, by="taskID")
payload <- merge(transcribed,credurl, by="taskID")
head(payload)

tmpPayload <- payload
head(tmpPayload$track)
tmpPayload <- apply(tmpPayload, 2, function(x) gsub("https://fitz-audio-guide-micropasts.s3.eu-west-2.amazonaws.com/", "  ", x))
tmpPayload <- apply(tmpPayload, 2, function(x) gsub("Chunk", "Chunk_", x))
tmpPayload <- data.frame(tmpPayload)
tmpPayload$sort <- str_sub(tmpPayload$track, start=-6)
tmpPayload$sort <- gsub(".mp3",'',tmpPayload$sort)
tmpPayload$sort <- gsub("_",'',tmpPayload$sort)
tmpPayload$sortTwo <- gsub("-","_",tmpPayload$track)
tmpPayload$sortTwo <- sapply(str_split(tmpPayload$sortTwo, "_",  n = 2), `[`, 2)
tmpPayload$sortTwo <- gsub(".mp3",'',tmpPayload$sortTwo)
tmpPayload$sortTwo <- gsub('[[:digit:]]+', '', tmpPayload$sortTwo)
tmpPayload$sortTwo <- gsub("_Chunk_",'',tmpPayload$sortTwo)
head(tmpPayload)
tmpCleanPayload <- tmpPayload[order(tmpPayload$sortTwo,as.numeric(tmpPayload$sort)),] 

# Write CSV file output
csvname <- paste('csv/', 'cleanedTasksFull', '.csv', sep='')
write.csv(tmpCleanPayload, file=csvname,row.names=FALSE, na="")


# Add three skipped lines between each unique index cards (i.e. between task sets).
tmpCleanPayloadOne <- tmpCleanPayload[which(is.na(tmpCleanPayload$taskID)), ] #blank df to fill
newrow <- rep(NA,ncol(tmpCleanPayload))
for (a in 1:length(tsks)){
  atask <- tmpCleanPayload[tmpCleanPayload$taskID == tsks[a],]
  tmpCleanPayloadOne <- rbind(tmpCleanPayloadOne,atask,newrow,newrow)
}
head(tmpCleanPayloadOne)


# Write CSV file output
csvname <- paste('csv/', project, '.csv', sep='')
write.csv(tmpCleanPayloadOne, file=csvname,row.names=FALSE, na="")


d <- fromJSON(paste(readLines(taskruns), collapse=""))
d <- as.data.frame(d)
user_id <- d$user_id
as.data.frame(user_id) -> user_id

#Match contributors'IDs with their names
data2 <- read.csv("csv/all_users.csv", sep=",")
data2[c("id", "fullname")] -> newdata2
names(newdata2) <- c("user_id", "fullname")
namescon <- merge(user_id, newdata2, by="user_id")
as.vector(namescon$fullname) -> names

#Extract and print unique names
unique(names) -> names
export <- paste(as.character(names), collapse=", ")
file <- paste('csv/', project, '.txt', sep='')

write.table(export,file=file, sep = "\t", row.names = FALSE)

extra <- read.csv('csv/fitzAudioGuide_task_run 2.csv')
combine <- merge(trT, extra, by='taskID')
write.csv(combine, file='csv/combined.csv',row.names=FALSE, na="")
nasty <- read.csv('csv/combined.csv')
l <- lapply(nasty$Info, fromJSON)
