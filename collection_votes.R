####
### Vote Collection
### PhD Wikipedia
### Nicole Schwitter
### 2020-11-20
####

####
### 0. Libraries ----
####
#library(RSelenium)
library(httr)
library(rvest) 
library(rlist)
library(stringr)
library(rapportools)

options(stringsAsFactors = FALSE)

####
### 1. Get pages bis 2020 ----
####

voteoverview <- read_html("https://de.wikipedia.org/wiki/Wikipedia:Adminkandidaturen/Archiv")

links <- html_nodes(voteoverview, "td li a")
links <- html_attr(links, "href")
links <- lapply(links, function(x) paste0("https://de.wikipedia.org", x))

#remove manual ones
scrapablelinks <- list.remove(links, c(1,2))

####
### 2. Iterate through pages
####


votingdf <- data.frame(source="",
                 type="administrator", 
                 candidate="", 
                 enddate="",
                 result="",
                 provotes="",
                 antivotes="",
                 neutralvotes="",
                 problemvotes="",
                 stringsAsFactors=FALSE) 



for (i in 1:length(scrapablelinks)) {
  message(i, " Reading link ", scrapablelinks[[i]])
  voteoverviewyear <- read_html(scrapablelinks[[i]])
  candidatelinks <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(2) , #mw-content-text li a:nth-child(3)")
  
  if (i==1) {
    candidatelinks <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(2)")
  }

  if (i==14) {
    candidatelinks <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(3) , #mw-content-text li :nth-child(2), li a:nth-child(4)")
  }

  
  candidatelinks <- as.list(html_attr(candidatelinks, "href"))
  
  candidatelinksl <- lapply(candidatelinks, function(x) !grepl('Meinungsbild', x, fixed=T))
  candidatelinks <- candidatelinks[which(unlist(candidatelinksl))]
  candidatelinksl <- lapply(candidatelinks, function(x) !grepl('type=revision', x, fixed=T))
  candidatelinks <- candidatelinks[which(unlist(candidatelinksl))] 
  if (i>10){  
  candidatelinksl <- lapply(candidatelinks, function(x) !grepl('Spezial:', x, fixed=T))
  candidatelinks <- candidatelinks[which(unlist(candidatelinksl))]   
  candidatelinksl <- lapply(candidatelinks, function(x) !grepl('oldid', x, fixed=T))
  candidatelinks <- candidatelinks[which(unlist(candidatelinksl))] 
  }
  
  candidatelinks <- lapply(candidatelinks, function(x){
    if (!startsWith(x, 'http'))
    paste0("https://de.wikipedia.org", x) 
    else x
  })
  
  candidates <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(1)")
  candidates <- html_attr(candidates, "href")
  candidates <- lapply(candidates, function(x){
    x <- gsub("Benutzerin:", "Benutzer:", x)
    x <- gsub("User:", "Benutzer:", x)
    x <- str_extract(x, "Benutzer:.*")
    x <- gsub("&action.*", "", x)
  })
  
  candidates <- list.clean(candidates, fun = is.na)
  results <- html_text(html_nodes(voteoverviewyear, "#mw-content-text li"))
  
  if (length(candidates)!=length(candidatelinks)){
    message("there is some problem")
    next
  }
  if (length(candidates)!=length(results)){
    message("there is some problem")
    next
  }
  
      
  for (j in 1:length(candidatelinks)) {
    message(j, " Reading candidate link ", candidatelinks[[j]])
    
    if (grepl("oldid", candidatelinks[[j]], fixed=T)){
      next
    }
    if (grepl("redlink", candidatelinks[[j]], fixed=T)){
      next
    }
    
    votingpage <- read_html(candidatelinks[[j]])
    
    source <- candidatelinks[[j]]
    candidate <- candidates[[j]]
    result <- results[[j]]

    enddate <- html_text(html_nodes(votingpage, ".mw-parser-output div b")) 
    if (length(enddate)>1){
      enddatel <- lapply(enddate, function(x) grepl('201[0-9]', x))
      enddate <- enddate[which(unlist(enddatel))]
    }
    enddate <- gsub(" um.*", "", enddate)
    
    voters <- html_nodes(votingpage, "ol a")
    voters <- html_attr(voters, "href")
    
    voters <- lapply(voters, function(x){
      x <- gsub("Benutzerin:", "Benutzer:", x)
      x <- gsub("User:", "Benutzer:", x)
      x <- str_extract(x, "Benutzer:.*")
      x <- gsub("&action.*", "", x)
      x <- gsub("/.*", "", x)
    })
    
    voters <- unique(voters)
    voters <- list.clean(voters, fun = is.na)
    voters <- voters[voters != candidate]
    
    
    #split up in pro, anti and neutral
    votingpagetext <- content(GET(candidatelinks[[j]]), "text")

    texts <- as.list(strsplit(votingpagetext, "mw-headline")[[1]])
    
    
    propartl <- lapply(texts, function(x) grepl('id=\"Pro', substr(x,0, 25), fixed=T))
    propart <- texts[which(unlist(propartl))]
    antipartl <- lapply(texts, function(x) grepl('id=\"[KC]ontra', substr(x,0, 25)))
    antipart <- texts[which(unlist(antipartl))]
    neutralpartl <- lapply(texts, function(x) grepl('id=\"Enthaltung', substr(x,0, 25), fixed=T))
    neutralpart <- texts[which(unlist(neutralpartl))] 
    
    if(candidate=="Benutzer:Hansele"){
      propart <- texts[[3]]
      antipart <- texts[[6]]
      neutralpart <- texts[[7]]
    }
    
    provotesl <- lapply(voters, function(x){
      x <- gsub("Benutzer:", "", x)
      grepl(x, propart, fixed = TRUE)
    })
    provotes <- unlist(voters[which(unlist(provotesl))])
    provotes <- paste0(provotes,  collapse=", ")
    
    antivotesl <- lapply(voters, function(x){
      x <- gsub("Benutzer:", "", x)
      grepl(x, antipart, fixed = TRUE)
    })
    antivotes <- unlist(voters[which(unlist(antivotesl))])
    antivotes <- paste0(antivotes,  collapse=", ")
    
        
    neutralvotesl <- lapply(voters, function(x){
      x <- gsub("Benutzer:", "", x)
      grepl(x, neutralpart, fixed = TRUE)
    })
    neutralvotes <- voters[which(unlist(neutralvotesl))]
    neutralvotes <- paste0(neutralvotes,  collapse=", ")

    provotes01 <- (ifelse(provotesl==T, 1, 0)) #bad programming style, I know I know
    antivotes01 <- (ifelse(antivotesl==T, 1, 0))
    neutralvotes01 <- (ifelse(neutralvotesl==T, 1, 0))
    problemvotes01 <- provotes01 + antivotes01 + neutralvotes01
    problemvotesl <- as.list(problemvotes01>1)
    
    problemvotes <- voters[which(unlist(problemvotesl))]
    problemvotes <- paste0(problemvotes,  collapse=", ")
    
    if(is.empty(result)){result=""}
    if(is.empty(enddate)){enddate=""}
    if(is.empty(provotes)){provotes=""}
    if(is.empty(antivotes)){antivotes=""}
    if(is.empty(neutralvotes)){neutralvotes=""}
    if(is.empty(problemvotes)){problemvotes=""}
    
    
    
    votingline <- data.frame(source=source,
                           type="administrator", 
                           result=result, 
                           candidate=candidate, 
                           enddate=enddate,
                           provotes=provotes,
                           antivotes=antivotes,
                           neutralvotes=neutralvotes,
                           problemvotes=problemvotes,
                           stringsAsFactors=FALSE) 
    
    votingdf <- rbind(votingdf, votingline)
    
  }
}


##########
###Older ones
##########
scrapablelinks2 <- links[1:3]

votingdf2_2 <- data.frame(source="",
                       type="administrator", 
                       candidate="", 
                       enddate="",
                       result="",
                       provotes="",
                       antivotes="",
                       neutralvotes="",
                       problemvotes="",
                       allvoters="",
                       stringsAsFactors=FALSE) 

i=1
j=1

for (i in 1:length(scrapablelinks2)) {
  message(i, " Reading link ", scrapablelinks2[[i]])
  voteoverviewyear <- read_html(scrapablelinks2[[i]])
  if (i==1){
    candidatelinks <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(2)")
  } else {
    candidatelinks <- html_nodes(voteoverviewyear, ".autonumber")
  }
  
  if (i==2) {
    candidatelinks <- list.remove(candidatelinks, 7) #two links for one user
  }
  
  if (i==3) {
    candidatelinks <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(2)")
  }
  
  candidatelinks <- as.list(html_attr(candidatelinks, "href"))
  
  candidatelinks <- lapply(candidatelinks, function(x){
    if (!startsWith(x, 'http'))
      paste0("https://de.wikipedia.org", x) 
    else x
  })
  
  candidates <- html_nodes(voteoverviewyear, "#mw-content-text li a:nth-child(1)")
  candidates <- html_attr(candidates, "href")
  candidates <- lapply(candidates, function(x){
    x <- gsub("Benutzerin:", "Benutzer:", x)
    x <- gsub("User:", "Benutzer:", x)
    x <- str_extract(x, "Benutzer:.*")
    x <- gsub("&action.*", "", x)
  })
  
  candidates <- list.clean(candidates, fun = is.na)
  results <- html_text(html_nodes(voteoverviewyear, "#mw-content-text li"))
  
  if (length(candidates)!=length(candidatelinks)){
    message("there is some problem")
    next
  }
  if (length(candidates)!=length(results)){
    message("there is some problem")
    next
  }
  
  for (j in 1:length(candidatelinks)) {
    message(j, " Reading candidate link ", candidatelinks[[j]])

    if (candidatelinks[[j]]=="https://de.wikipedia.org/w/index.php?title=Wikipedia:Administratorkandidaturen&diff=4394778&oldid=4393796"){
      candidatelinks[[j]] <- "https://de.wikipedia.org/w/index.php?title=Wikipedia:Adminkandidaturen/Alt01&oldid=4393796" 
    }    
        
    if (candidatelinks[[j]]=="https://de.wikipedia.org/w/index.php?title=Wikipedia%3AAdminkandidaturen&diff=20009438&oldid=20007990#MacPac"){
      candidatelinks[[j]] <- "https://de.wikipedia.org/w/index.php?title=Wikipedia:Adminkandidaturen/Alt02&oldid=20007990" 
    }
    
    if (candidatelinks[[j]]=="https://de.wikipedia.org/w/index.php?title=Wikipedia:Adminkandidaturen&diff=13710841&oldid=13710811"){
      candidatelinks[[j]] <- "https://de.wikipedia.org/w/index.php?title=Wikipedia:Adminkandidaturen/Alt02&oldid=13710811" 
    }
    
    
    if (!grepl("oldid", candidatelinks[[j]], fixed=T)){
      next
    }
    
    votingpage <- read_html(candidatelinks[[j]])
    
    source <- candidatelinks[[j]]
    candidate <- candidates[[j]]
    candidate <- gsub("%C3%B6", "ö",candidate)
    candidate <- gsub("%C3%A4", "ä",candidate)
    candidate <- gsub("%C3%BC", "ü",candidate)
    candidate <- gsub("%C3%9F", "ß",candidate)
    candidate <- gsub("%27", "'",candidate)
    
    result <- results[[j]]
    
    users <- html_text(html_nodes(votingpage, ".mw-headline a"))
    users <- sapply(users, trimws)
    users <- sapply(users, function(x) gsub(" ", "_", x))
    #deal with exceptions
    if (i==2){
      users <- sapply(users, function(x) gsub("Benutzer:", "", x, fixed=T))
      users <- sapply(users, function(x) gsub("Eike_Sauer", "Eike_sauer", x, fixed=T))
      users <- sapply(users, function(x) gsub("gunny", "Gunfighter-6", x, fixed=T))
      users <- sapply(users, function(x) gsub("Leon_Weber", "LeonWeber", x, fixed=T))
      
    }
    if (i==3){
      users <- sapply(users, function(x) gsub("Fristu_/_WikiWichtel", "Fristu", x))
      users <- sapply(users, function(x) gsub("Martin_Vogel", "Martin-vogel", x))
      users <- sapply(users, function(x) gsub("Felix_Stember_(aka_Gunny)", "Felix_Stember", x, fixed=T))
      users <- sapply(users, function(x) gsub("Roosterfan_(torte)", "Roosterfan", x, fixed=T))
      users <- sapply(users, function(x) gsub("Tolanor", "Tolanor_von_Preto", x, fixed=T))
      users <- sapply(users, function(x) gsub("Holger_Thölking", "Herr_Th.", x, fixed=T))
      users <- sapply(users, function(x) gsub("Christian_Bier", "ChristianBier", x, fixed=T))
    }
    
    users <- sapply(users, function(x) gsub("Wiederwahl_", "", x, fixed=T))
    
    users <- sapply(users, function(x) paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep=""))
    #users <- sapply(users, function(x) gsub("Dbenzhuser", "dbenzhuser", x, fixed=T))
    
        
    candidatetemp <- gsub(" ", "_", candidate)
    candidatetemp <- paste(toupper(substr(candidate, 1, 1)), substr(candidate, 2, nchar(candidate)), sep="")
    if (candidate=="Benutzer:Paul_Conradi") {candidatetemp <- "Benutzer:PaCo"}
    if (candidate=="Benutzer:Holger_Thölking") {candidatetemp <- "Benutzer:Herr_Th."}
    if (candidate=="Benutzer:Gunny") {candidatetemp <- "Benutzer:Gunfighter-6"}
    
    #if (candidate=="Benutzer:Elian") {candidatetemp <- "Benutzer:elian"}
    
    if(i==2 & j==139){
      enddate = "22. Dezember 2005"
    } else {
      enddate <- html_text(html_nodes(votingpage, ".mw-parser-output div b")) 
      enddatel <- (lapply(enddate, function(x) grepl('[0-9]\\.', x)))
      enddate <- (enddate[which(unlist(enddatel))])
      enddate <- (enddate[which((users==substr(candidatetemp, 10, str_length(candidatetemp))))])
    }

#    if (i==1) { 
#      enddate <- enddate[[j+1]]
#    }
#    else {
#      enddate <- enddate[2]
#    }
    enddate <- gsub(" um.*", "", enddate)
    
    if (i==1){
      enddate <- paste0(enddate, " 2004")
    }
    if (i==3 & !grepl("200", enddate)){
      enddate <- paste0(enddate, " 2006")
    }

    voters <- html_nodes(votingpage, "ol a")
    voters <- html_attr(voters, "href")
    
    voters <- lapply(voters, function(x){
      x <- gsub("Benutzer_Diskussion:", "Benutzer:", x)
      x <- gsub("User_talk:", "Benutzer:", x)
      x <- gsub("Benutzerin:", "Benutzer:", x)
      x <- gsub("User:", "Benutzer:", x)
      x <- str_extract(x, "Benutzer:.*")
      x <- gsub("&action.*", "", x)
      x <- gsub("/.*", "", x)
    })       
    voters <- unique(voters)
    voters <- list.clean(voters, fun = is.na)
    voters <- voters[voters != candidate] 
    
    
    #split up in pro, anti and neutral
    votingpagetext <- content(GET(candidatelinks[[j]]), "text")
    
    texts <- as.list(strsplit(votingpagetext, "mw-headline")[[1]])

    #get textparts which are about candidate
    candidatel <- lapply(texts, function(x) grepl((substr(candidate, 10, str_length(candidatetemp))), substr(x,0, 35), fixed=T))
    
    #get textparts which are pro/anti/enthaltung, combine with those about candidate
    propartl <- lapply(texts, function(x) grepl('id=\"Pro', substr(x,0, 25), fixed=T))
    firstpropart <- min(which(propartl == TRUE & candidatel == TRUE))
    propartl <- replace(propartl, 1:length(propartl), FALSE)
    propartl <- replace(propartl, firstpropart, TRUE)
    propart <- texts[which(unlist(propartl))]
    
    propart <- gsub("Benutzer_Diskussion:", "Benutzer:", propart)
    propart <- gsub("User_talk:", "Benutzer:", propart)
    propart <- gsub("Benutzerin:", "Benutzer:", propart)
    propart <- gsub("User:", "Benutzer:", propart)    
    
    antipartl <- lapply(texts, function(x) grepl('id=\"[KC]ontra', substr(x,0, 25)))
    firstantipart <- min(which(antipartl == TRUE & candidatel == TRUE))
    antipartl <- replace(antipartl, 1:length(antipartl), FALSE)
    antipartl <- replace(antipartl, firstantipart, TRUE)  
    antipart <- texts[which(unlist(antipartl))]
    
    antipart <- gsub("Benutzer_Diskussion:", "Benutzer:", antipart)
    antipart <- gsub("User_talk:", "Benutzer:", antipart)
    antipart <- gsub("Benutzerin:", "Benutzer:", antipart)
    antipart <- gsub("User:", "Benutzer:", antipart)  
    
    neutralpartl <- lapply(texts, function(x) grepl('id=\"Enthaltung', substr(x,0, 25), fixed=T))
    firstneutralpart <- min(which(neutralpartl == TRUE & candidatel == TRUE))
    neutralpartl <- replace(neutralpartl, 1:length(neutralpartl), FALSE)
    neutralpartl <- replace(neutralpartl, firstneutralpart, TRUE)      
    neutralpart <- texts[which(unlist(neutralpartl))] 
    
    neutralpart <- gsub("Benutzer_Diskussion:", "Benutzer:", neutralpart)
    neutralpart <- gsub("User_talk:", "Benutzer:", neutralpart)
    neutralpart <- gsub("Benutzerin:", "Benutzer:", neutralpart)
    neutralpart <- gsub("User:", "Benutzer:", neutralpart)  
    
    provotesl <- lapply(voters, function(x){
      x <- gsub("Benutzer:", "", x)
      grepl(x, propart, fixed = TRUE)
    })
    #using _2 as this has an end
    provotesl2 <- lapply(voters, function(x){
      #x <- gsub("Benutzer:", "", x)
      x2 <- paste0(x, "&")      
      x <- paste0(x, "\"")
      grepl(x, propart, fixed = TRUE)|grepl(x2, propart, fixed = TRUE)
    })
    provotes <- unlist(voters[which(unlist(provotesl))])
    provotes <- paste0(provotes,  collapse=", ")
    
    provotes2 <- unlist(voters[which(unlist(provotesl2))])
    provotes2 <- paste0(provotes2,  collapse=", ")
    
    antivotesl <- lapply(voters, function(x){
      x <- gsub("Benutzer:", "", x)
      grepl(x, antipart, fixed = TRUE)
    })
    antivotesl2 <- lapply(voters, function(x){
      #x <- gsub("Benutzer:", "", x)
      x2 <- paste0(x, "&")      
      x <- paste0(x, "\"")
      grepl(x, antipart, fixed = TRUE)|grepl(x2, antipart, fixed = TRUE)
    })
    
    antivotes <- unlist(voters[which(unlist(antivotesl))])
    antivotes <- paste0(antivotes,  collapse=", ")
    
    antivotes2 <- unlist(voters[which(unlist(antivotesl2))])
    antivotes2 <- paste0(antivotes2,  collapse=", ")
    
    neutralvotesl <- lapply(voters, function(x){
      x <- gsub("Benutzer:", "", x)
      grepl(x, neutralpart, fixed = TRUE)
    })
    neutralvotesl2 <- lapply(voters, function(x){
      #x <- gsub("Benutzer:", "", x)
      x2 <- paste0(x, "&")      
      x <- paste0(x, "\"")
      grepl(x, neutralpart, fixed = TRUE)|grepl(x2, neutralpart, fixed = TRUE)
    })
    
    neutralvotes <- voters[which(unlist(neutralvotesl))]
    neutralvotes <- paste0(neutralvotes,  collapse=", ")
    
    neutralvotes2 <- voters[which(unlist(neutralvotesl2))]
    neutralvotes2 <- paste0(neutralvotes2,  collapse=", ")  
    
    provotes01 <- (ifelse(provotesl==T, 1, 0)) #bad programming style :(
    antivotes01 <- (ifelse(antivotesl==T, 1, 0))
    neutralvotes01 <- (ifelse(neutralvotesl==T, 1, 0))
    problemvotes01 <- provotes01 + antivotes01 + neutralvotes01
    problemvotesl <- as.list(problemvotes01>1)
    
    problemvotes <- voters[which(unlist(problemvotesl))]
    problemvotes <- paste0(problemvotes,  collapse=", ")
    
    
    provotes012 <- (ifelse(provotesl2==T, 1, 0)) #bad programming style :(
    antivotes012 <- (ifelse(antivotesl2==T, 1, 0))
    neutralvotes012 <- (ifelse(neutralvotesl2==T, 1, 0))
    problemvotes012 <- provotes012 + antivotes012 + neutralvotes012
    problemvotesl2 <- as.list(problemvotes012>1)
    
    problemvotes2 <- voters[which(unlist(problemvotesl2))]
    problemvotes2 <- paste0(problemvotes2,  collapse=", ")
    
    
    #votingpagetext <- content(GET(candidatelinks[[j]]), "text")
    #votingpagetext <- as.list(strsplit(votingpagetext, "<h3>")[[1]])
    
    if(is.empty(result)){result=""}
    if(is.empty(enddate)){enddate=""}
    if(is.empty(provotes)){provotes=""}
    if(is.empty(antivotes)){antivotes=""}
    if(is.empty(neutralvotes)){neutralvotes=""}
    if(is.empty(problemvotes)){problemvotes=""}
    
    if(is.empty(provotes2)){provotes2=""}
    if(is.empty(antivotes2)){antivotes2=""}
    if(is.empty(neutralvotes2)){neutralvotes2=""}
    if(is.empty(problemvotes2)){problemvotes2=""}
    
    allvoters <- unlist(paste0(voters, collapse=", "))
    
    votingline <- data.frame(source=source,
                             type="administrator", 
                             result=result, 
                             candidate=candidate, 
                             enddate=enddate,
                             provotes=provotes2,
                             antivotes=antivotes2,
                             neutralvotes=neutralvotes2,
                             problemvotes=problemvotes2,
                             allvoters=allvoters,
                             stringsAsFactors=FALSE) 
    
    votingdf2_2 <- rbind(votingdf2_2, votingline)
    
  }
}  


##########
###Oldest ones
##########

oldestlinks <- html_nodes(voteoverview, "ul:nth-child(7) li a")
oldestlinks <- html_attr(oldestlinks, "href")
oldestlinks <- lapply(oldestlinks, function(x) paste0("https://de.wikipedia.org", x))

scrapablelinks3 <- list.remove(oldestlinks, 1) #it's just one to automate


votingdf3 <- data.frame(source="",
                       type="administrator", 
                       candidate="", 
                       enddate="",
                       result="",
                       provotes="",
                       antivotes="",
                       neutralvotes="",
                       problemvotes="",
                       stringsAsFactors=FALSE) 


voteoverviewyear <- read_html(scrapablelinks3[[1]])
  
candidates <- html_nodes(voteoverviewyear, ".mw-headline a")
candidates <- html_attr(candidates, "title")

source <- "https://de.wikipedia.org/wiki/Wikipedia:Adminkandidaturen/Archiv/2004-1"

voters <- html_nodes(voteoverviewyear, "#mw-content-text li a")
voters <- html_attr(voters, "href")
voters <- list.remove(voters, range = 1:316)

voters <- lapply(voters, function(x){
  x <- gsub("Benutzerin:", "Benutzer:", x)
  x <- gsub("User:", "Benutzer:", x)
  x <- str_extract(x, "Benutzer:.*")
  x <- gsub("&action.*", "", x)
  x <- gsub("/.*", "", x)
})       
voters <- unique(voters)
voters <- list.clean(voters, fun = is.na)


#split up in texts about candidate 
#and possibly pro anti
votingpagetext <- content(GET(scrapablelinks3[[1]]), "text")

texts <- as.list(strsplit(votingpagetext, "mw-headline")[[1]])

j=1

enddates <- html_text(html_nodes(voteoverviewyear, ".mw-parser-output div b")) 
enddatesl <- (lapply(enddates, function(x) grepl('[0-9]\\.', x)))
enddates <- as.list(enddates[which(unlist(enddatesl))])

enddates <- lapply(enddates, function (x) gsub(" um.*", "", x))
enddates <- lapply(enddates, function (x) paste0(x, "2004"))

for (j in 1:length(candidates)) {
  #get textparts which are about candidate
  candidatel <- lapply(texts, function(x) grepl((substr(candidates[[j]], 10, str_length(candidates[[j]]))), substr(x,0, 35), fixed=T))
  
  #get textparts which are pro/anti/enthaltung, combine with those about candidate
  propartl <- lapply(texts, function(x) grepl('id=\"Pro', substr(x,0, 25), fixed=T))
  firstpropart <- min(which(propartl == TRUE & candidatel == TRUE))
  propartl <- replace(propartl, 1:length(propartl), FALSE)
  propartl <- replace(propartl, firstpropart, TRUE)
  propart <- texts[which(unlist(propartl))]
  
  antipartl <- lapply(texts, function(x) grepl('id=\"[KC]ontra', substr(x,0, 25)))
  firstantipart <- min(which(antipartl == TRUE & candidatel == TRUE))
  antipartl <- replace(antipartl, 1:length(antipartl), FALSE)
  antipartl <- replace(antipartl, firstantipart, TRUE)  
  antipart <- texts[which(unlist(antipartl))]
  
  neutralpartl <- lapply(texts, function(x) grepl('id=\"Enthaltung', substr(x,0, 25), fixed=T))
  firstneutralpart <- min(which(neutralpartl == TRUE & candidatel == TRUE))
  neutralpartl <- replace(neutralpartl, 1:length(neutralpartl), FALSE)
  neutralpartl <- replace(neutralpartl, firstneutralpart, TRUE)      
  neutralpart <- texts[which(unlist(neutralpartl))] 
  
  provotesl <- lapply(voters, function(x){
    x <- gsub("Benutzer:", "", x)
    grepl(x, propart, fixed = TRUE)
  })
  provotes <- unlist(voters[which(unlist(provotesl))])
  provotes <- paste0(provotes,  collapse=", ")
  
  antivotesl <- lapply(voters, function(x){
    x <- gsub("Benutzer:", "", x)
    grepl(x, antipart, fixed = TRUE)
  })
  antivotes <- unlist(voters[which(unlist(antivotesl))])
  antivotes <- paste0(antivotes,  collapse=", ")
  
  
  neutralvotesl <- lapply(voters, function(x){
    x <- gsub("Benutzer:", "", x)
    grepl(x, neutralpart, fixed = TRUE)
  })
  neutralvotes <- voters[which(unlist(neutralvotesl))]
  neutralvotes <- paste0(neutralvotes,  collapse=", ")
  
  provotes01 <- (ifelse(provotesl==T, 1, 0)) #bad programming style, I know I know
  antivotes01 <- (ifelse(antivotesl==T, 1, 0))
  neutralvotes01 <- (ifelse(neutralvotesl==T, 1, 0))
  problemvotes01 <- provotes01 + antivotes01 + neutralvotes01
  problemvotesl <- as.list(problemvotes01>1)
  
  problemvotes <- voters[which(unlist(problemvotesl))]
  problemvotes <- paste0(problemvotes,  collapse=", ")
  
  if (provotes==""&antivotes=="")
  {
    candidatevotepart <- texts[which(unlist(candidatel))] 
    problemvotesl <- lapply(voters, function(x){
    x <- gsub("Benutzer:", "", x)
      grepl(x, candidatevotepart, fixed = TRUE)
    })
    
    problemvotes <- unlist(voters[which(unlist(problemvotesl))])
    problemvotes <- problemvotes[problemvotes != candidates[[j]]]
    problemvotes <- paste0(problemvotes,  collapse=", ")
    
  }
  
  
  if (j>=42 & j <101){
    enddate <- enddates[[j-41]]
  } else {
    enddate <- " 2004"
  }

  
  if(is.empty(result)){result=""}
  if(is.empty(enddate)){enddate=""}
  if(is.empty(provotes)){provotes=""}
  if(is.empty(antivotes)){antivotes=""}
  if(is.empty(neutralvotes)){neutralvotes=""}
  if(is.empty(problemvotes)){problemvotes=""}
  

  votingline <- data.frame(source=source,
                           type="administrator", 
                           result="", 
                           candidate=candidates[[j]], 
                           enddate=enddate,
                           provotes=provotes,
                           antivotes=antivotes,
                           neutralvotes=neutralvotes,
                           problemvotes=problemvotes,
                           stringsAsFactors=FALSE) 
  
  votingdf3 <- rbind(votingdf3, votingline)
  
  
}


save(votingdf, votingdf2_2, votingdf3, file = "allvotingdfs_2.RData")
#load("allvotingdfs.RData")

votingdf$allvoters <- ""
votingdf3$allvoters <- ""

finalvotingdf <- rbind(votingdf, votingdf2_2, votingdf3)
saveRDS(finalvotingdf, "finalvotingdf_2")

write.csv(votingdf2_2, "votes20042006_2.csv")
write.csv(finalvotingdf,'autommatedvotes20042020.csv')
