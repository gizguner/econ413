library(stringi)
library(data.table)
library(pdftools)
library(tesseract)
library(rvest)
library(readtext)
library(stringr)



AlreadyDownloaded=FALSE  ####### When TRUE it won't download the files again
AlreadyRead=FALSE       ####### When TRUE it won't load the PDF's to the dataframe
################################################################################

Sys.setlocale("LC_ALL", locale="Turkish")




# Creating a temporary working directory to store all files - including OCR generated png's
################################################################################
maindir=getwd()
tempfolder="TEMPORARY"

if(!dir.exists(tempfolder)){
  dir.create(tempfolder)
  dir.create(paste0(tempfolder,"\\downloads"))
}
setwd(tempfolder)
################################################################################





if(!AlreadyDownloaded){
  
  tesseract_download("tur")
  
  
  # Function to get Links
  getLink <- function(i){
    a=html_attr(nodes[i], "style")
    if(is.na(a))
    {
      url=html_attr(nodes[i], "href")
      out=paste0("https://www.sbb.gov.tr", url)
      return(out)
    }
    return(NULL)
  }
  
  # Encode Turkish URL's to prevent 404 errors
  ##############################################################################
  URL_encode_turkish <- function(url)
  {
    a=c("Ç","ç","Ğ","ğ","İ","ı","Ö","ö","Ş","ş","Ü","ü")
    b=c("%c3%87", "%c3%a7" ,"%c4%9e" ,"%c4%9f", "%c4%b0", "%c4%b1", "%c3%96", "%c3%b6", "%c5%9e", "%c5%9f", "%c3%9c", "%c3%bc")
    for(i in 1:12) {
      url=gsub(a[i], b[i], url)
    }
    return (url)
  }
  ##############################################################################
  
  
  
  doc <- html_node(read_html("https://www.sbb.gov.tr/yillik-programlar/"), "#post-34")
  nodes<-doc %>%   html_nodes('a')
  
  
  linkList <- lapply(c(1:length(nodes)), getLink)
  rm("nodes","doc")
  
  
  # Looping through the list and downloading each item
  for(i in 1:length(linkList)) {
    URL=linkList[i]
    if(URL=="NULL") next
    loc=stri_locate_last_fixed(URL,"/")
    year <- substring(URL, loc[1]+1, loc[1]+4)
    URL=toString(URL)
    ext=tools::file_ext(sub("\\?.+", "", URL))
    
    destfile=paste0("downloads\\",year,".",ext)
    cat("Downloading",year,"\n")
    download.file(URL_encode_turkish(URL), destfile, mode="wb")
  }
  rm("ext","i","ListLength","year","URL","loc","linkList","desfile","AlreadyDownloaded","getLink","destfile","URL_encode_turkish")
  
}

#Deleting the temporary generated png's (from OCR) incase they didn't automatically
################################################################################
DeleteTmpPng <- function(){
  pngList<-list.files(pattern = ".png")  
  for(j in 1:length(pngList))
  {
    unlink(pngList[j])
  }
}
################################################################################



if(!AlreadyRead)
{
  pdfList<-list.files(pattern = ".pdf",full.names = TRUE, recursive = TRUE)  
  docList<-list.files(pattern = ".doc|.docx",full.names = TRUE, recursive = TRUE) 
  
  ## Creating the main empty data table
  FullList <- data.table(year=integer(),contents=character())
  
  
  
  clean_characters <- function(input)
  {
    output<-gsub('[[:punct:] ]+',' ',input)
    output<-gsub('õ|İ|ı','i',output)
    output<-gsub('İ','i',output) #### sometimes İ fails to replace 
    output<-gsub('ḡ','g',output)
    output<-gsub('ç','c',output)
    output<-gsub('ğ','g',output)
    output<-gsub('ö','o',output)
    output<-gsub('ş','s',output)
    output<-gsub('ü','u',output)
    return(output)
  }
  
  totalPdf=length(pdfList)
  ## Reading the Pds's
  for(i in 1:totalPdf) {
    cat("Reading PDF", i , "of" ,totalPdf,"\n")
    PDF <- pdf_text(pdfList[i])
    PDF <-  strsplit(PDF, split = "\r\n| ") 
    contents <- unname(unlist(PDF))
    contents<-paste(tolower(contents), collapse = " ")
    
    if(contents=="")
    {
      PDF <- pdf_ocr_text(
        pdfList[i],
        pages = NULL, ## NULL will make it download every page
        opw = "",
        upw = "",
        language = "tur",
        dpi = 600
      )
      PDF <-  strsplit(tolower(PDF), split = "\r\n| ")
      contents <- unname(unlist(PDF))
      contents<-paste(tolower(contents), collapse = " ")
      contents=gsub("\r|\n", " ", contents)
      # Delete OCR generated png files 
      DeleteTmpPng()
    }
    contents=clean_characters(contents)
    
    
    rm("PDF")
    year=tools::file_path_sans_ext(basename(pdfList[i]))
    
    ddd <- data.table(year, contents)
    FullList<-rbind(FullList, ddd,fill=TRUE)  
    rm("ddd")
  }
  
  ##Reading the docs
  totalDoc=length(docList)
  for(i in 1:totalDoc) {
    cat("Reading DOC", i , "of" ,totalDoc,"\n")
    
    contents<-readtext(docList[i],encoding = "UTF8")
    contents=contents[2]
    contents=tolower(contents)
    contents=gsub("\r|\n", " ", contents)
    contents=clean_characters(contents)
    
    year=tools::file_path_sans_ext(basename(docList[i]))
    
    
    ddd <- data.table(year, contents)
    FullList<-rbind(FullList, ddd,fill=TRUE)  
    
  }
  rm("ddd","i","pdfList","totalDoc","docList","contents")
} ## Reading files finished

##Counting words
Word_Count <- function(word){
  dat <- data.table(year=integer(),count=integer())
  
  word<-as.character(word)
  for(i in 1:nrow(FullList))
  {
    selyear <- FullList[i,year]
    out=str_count(FullList[i], word)[2]
    dat<-rbind(dat, data.table(year =selyear, count = out),fill=TRUE)  
  }
  colnames(dat)[2] = word
  return (dat[order(year)])
}


##Putting the counted words in a data table
WordCountTable<- function(listofwords)
{
  aa = Word_Count(listofwords[1])
  for (i in 2:length(listofwords))
  {
    bb = Word_Count(listofwords[i])
    aa <- merge(x = aa, y = bb, by = "year", all.x = TRUE)
  }
  return (aa)
}


setwd(maindir)
