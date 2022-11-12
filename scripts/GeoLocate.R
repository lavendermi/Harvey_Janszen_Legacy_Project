# from http://www.geo-locate.org/files/glc.r.txt

#simple script to run a csv through GEOLocate webservices
#returns two files. One with all the results and another with only the first result of each run
#for this to work csv will need at least the following column headers in any order but case sensitive: Country,Locality,StateProvince,County
#Question & comments to Nelson Rios(nelson.rios@yale.edu)
#last modified: 21 Sept 2018
#Thanks to Javier Diaz for adding command line options. 
#How to run it:
#Rscript --quiet --vanilla glc.R  -w [...] -i [...] -o [...] -p [...] 
#
#Help:
#Rscript --quiet --vanilla glc.R -h

#Packages required: RJSONIO, RCurl, optparse and getopt (optparse takes a dependency on this).

library(RJSONIO)
library(RCurl)
library(optparse)

main <- function(options){
  
  setwd(options$workdir)
  
  # Parameters assingment block.
  INPUTFILENAME <- options$input
  OUTPUTFILENAME <- options$output
  OUTPUTFILENAMEFIRSTRESULT <- options$result
  OPTIONS <- options$options
  
  glcIn= read.csv(INPUTFILENAME)
  numGLCRuns = 0
  recordCounter = 0
  for (k in 1:nrow(glcIn)){
    print(k)
    Sys.sleep(3) #be nice and pause a few seconds between requests 
    Country=glcIn[k,]$Country
    Locality=glcIn[k,]$Locality
    StateProvince=glcIn[k,]$StateProvince
    County=glcIn[k,]$County
    
    params <- ""
    
    if (!is.na(Country)) {
      params <- paste(params, paste("country", Country, sep = '='), sep = '&')
    }
    
    if (!is.na(Locality)) {
      params <- paste(params, paste("locality", Locality, sep = '='), sep = '&')
    }
    
    if (!is.na(StateProvince)) {
      params <- paste(params, paste("state", StateProvince, sep = '='), sep = '&')
    }
    
    if (!is.na(County)) {
      params <- paste(params, paste("county", County, sep = '='), sep = '&')
    }
    
    params <- substr(params, 2, nchar(params))
    
    q=paste("http://geo-locate.org/webservices/geolocatesvcv2/glcwrap.aspx?",params,OPTIONS, sep='')
    q=gsub(' ','%20',q)
    
    tryCatch({
      JSONresponse = basicTextGatherer()
      curlPerform(url = q, writefunction = JSONresponse$update)
      glcRecNum = k
      glc = fromJSON(JSONresponse$value())
      numresults = glc$numResults
      if (numresults > 0){ 
        for (i in 1:numresults) {
          glcRank  = i
          glcLongitude = glc$resultSet$features[[i]]$geometry$coordinates[1]
          glcLatitude = glc$resultSet$features[[i]]$geometry$coordinates[2]
          glcPrecision = glc$resultSet$features[[i]]$properties$precision
          glcScore = glc$resultSet$features[[i]]$properties$score
          glcParsepattern = glc$resultSet$features[[i]]$properties$parsePattern
          glcUncert = glc$resultSet$features[[i]]$properties$uncertaintyRadiusMeters
          glcPoly = glc$resultSet$features[[i]]$properties$uncertaintyPolygon
          #if a polygon is present reformat coordinates to geolocate format-a comma delimited array
          if ("coordinates"%in%names(glcPoly)){
            sPoly = ''
            for (v in 1:length(glcPoly$coordinates[[1]][])){
              vLon=format(glcPoly$coordinates[[1]][[v]][1])
              vLat=format(glcPoly$coordinates[[1]][[v]][2])
              sPoly  = paste(sPoly,vLat, vLon, sep=',')
            }
            # Strip the leading commas
            sPoly=sub("^,+", "", sPoly)
            glcPoly=sPoly
          }
          df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
          recordCounter = recordCounter + 1
          if (recordCounter==1)
            write.table(x=df, file=OUTPUTFILENAME, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',', qmethod="double") else
              write.table(x=df, file=OUTPUTFILENAME, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',', qmethod="double")
        }
      } else {
        glcRank  = 1
        glcLongitude = NA
        glcLatitude = NA
        glcPrecision = NA
        glcScore = NA
        glcParsepattern = NA
        glcUncert = NA
        glcPoly = NA
        df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
        recordCounter = recordCounter + 1
        if (recordCounter==1)
          write.table(x=df, file=OUTPUTFILENAME, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',') else
            write.table(x=df, file=OUTPUTFILENAME, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',') 
      }
    },error = function(err) 
    {
      glcRank  = 0
      glcLongitude = NA
      glcLatitude = NA
      glcPrecision = "ERROR GETTING JSON"
      glcScore = 0
      glcParsepattern = NA
      glcUncert = NA
      glcPoly = NA
      df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
      recordCounter = recordCounter + 1
      if (recordCounter==1)
        write.table(x=df, file=OUTPUTFILENAME, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',') else
          write.table(x=df, file=OUTPUTFILENAME, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',')  
      
    })
  }
  
  glcFiltered=read.csv(OUTPUTFILENAME)
  glcFiltered=glcFiltered[glcFiltered$glcRank==1,]
  write.table(x=glcFiltered, file=OUTPUTFILENAMEFIRSTRESULT, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',')
  
  #freeing resources
  rm(list=ls())
  
}

option_list = list(
  make_option(c("-w", "--workdir"), 
              type = "character",
              default = "E:/FishNet2GeoRef",
              help = "Working directory.",
              metavar = "character"),
  make_option(c("-i", "--input"), 
              type = "character",
              default = "glc_in.csv",
              help = "Input filename.",
              metavar = "character"),
  make_option(c("-o", "--output"), 
              type = "character",
              default = "glc_out.csv",
              help = "Output filename.",
              metavar = "character"),
  make_option(c("-r", "--result"), 
              type = "character",
              default = "glc_out_first_result.csv",
              help = "First result output filename.",
              metavar = "character"),
  make_option(c("-p", "--options"), 
              type = "character",
              default = "&douncert=true&dopoly=false&displacepoly=false",
              help = "Options string.",
              metavar = "character")
  
)


opt_parser <- OptionParser(option_list = option_list);
opt <- parse_args(opt_parser);

main(opt)

