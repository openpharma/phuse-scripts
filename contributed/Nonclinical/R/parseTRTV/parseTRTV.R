###################################################################################
# Script name   : TRTVparse.R
# Date Created  : 12-18-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : William Houser
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
#ww/16-Oct-2021     Applied below update
# 
# 1. Added support for parsing components and component concentration, 
#    e.g.35% w/w of 200 mM Tris(hydroxymethyl) aminomethane buffer,
# 2. Remove the manual seq variable, use the nrow(XVTable) to set the sequence of each row.
#
#ww/12-Sep-2021     Applied below update
# 
# 1. Added support for parsing components with two sets of number and units, 
#    e.g.35% w/w of 200 mM Tris(hydroxymethyl) aminomethane buffer,
# 2. Create parseTRTV function so the script can be used by RShinny App.

#ww/16-Aug-2021     Applied below updates
# 
# 1. Pre-processed the vehicle string, replace the unicode � with +/-
# 2. Pre-processed the vehicle string, replace "to" in the attribute string with a DASH, e.g. "PH 3 to 5" to "PH 3 - 5"
# 3. After parsing, Removed the component(s) after the attribute, attribute will be the last.
# 4. Corrected the index values used in the vector when adding rows to XVtable for the attributes (an(r|(R)n)?) 
# 5. When checking whether a string is a number: if the string starts with a number, get the number and ignore the non-numeric part. 
#    E.g. "pH 6.0, Sterile Filtered", the ", Sterile Filtered" part will be ignored and number 6.0 will be returned.
# 6. cleanTokens_R function added logic to check if f component exists before R. Only ignore R if f components exists. 
#    if no f component exists before R, no action. This is to support the parse of vehicle as below:
#    "0.2% HPMC, 0.01% Acetic Acid, pH 3.6"
# 7. Sink the parse result to a file called trtv-parse.txt.
# -----------------   ------------------------------------------------------------
# wph/24-Dec-2020     Initial posting to GitHub
#
# -------------------------------------------------------------------------------
# Purpose       : To process values supplied in CDISC SENDIG parameter TRTV in the ts.xpt 
#                 dataset and create a treatment vehicle component table like the XV 
#                 example similar to the AC domain in SENDIG-AR that is in the PHUSE 
#                 whitepaper "Recommendations for Exchanging Vehicle details"
#
# Description   : This is accomplished with the following functions:
#                  vehicleTokenize - to categorize portions of the TRTV.
#                  cleanTokens_i   - to categorize some portions to be ignored becasue
#                                    a synonym term is supplied in TRTV
#                  cleanTokens_ur  - To look for additional numbers in portions of
#                                    TRTV that were initially classified as Unknown
#                                    because we found a unit of measure or a +/- 
#                                    indicator without a number where expected.
#                  makeXV         -  to create the XV table by analyzing the 
#                                    tokenized TRTV with extensive use of regular
#                                    expressions.
#                  parseTRTV      -  to parse one line of the TRTV. This is to be used by RShinny app.
#                 
# Input         : All input files are expected to be in the working directory
#                 trtv       - a text file with the text from TRTV in submissions with one value per line.
#                 tokens.csv - a table of meaningful string segments that are expected in 
#                              TRTV with their category and standardized values. This program
#                              assumes that this file is already correctly sorted.
#
# Output        : The output is written to a set of xvnn.csv files.
#
# Parameters    : none. 
#
# Usage notes   :
#
# The tokens.csv file is used to identify meaningful substrings in TRTV and categorize them. This script assumes that tokens.csv
# is sorted such that any string that matches the "Source" column in a row will not match any row higher in the table.
# At this point, this has been accomplished by sorting the "Len" column from highest to lowest.  Generally this column
# is the length of the "Source" column; however, when "Source" includes regular expressions with escape caracters or 
# repeated elements the "Len" column is assigned a value that doesn't always match the number of characters in "Source"
# If a match is preceeded by a alphabetic charactor or an underscore, the tokenize function assumes the match is a continuation
# of another word and considers this to not really be a match.
#
# The "Category" column is 1 letter long. The makeXV function expects each to be exactly 1 letter.  The possible values
# and their meanings are listed here:
#   c = component
#   a = attribute (like pH)
#   r = range (like +/-)
#   R = range (like "-" in "3-5")
#   u = unit
#   n = number [+-]{0,1}[0-9]*\.{0,1}[0-9]+
#   f = the following item is diluent
#   U = unknown
#   i = ignore, for example if a component is supplied with a synonym, the synonym is categorized as "i" so that it is ignored.
###################################################################################
# FUTURE ENHANCEMENTS TO CONSIDER:
# If we see "AQUEOUS" we might want to make 2 rows: 1 to say "with diluent =" and one to say "WATER". Place these rows at the bottom 2 rows of the table.
#
# If we have a number and a unit and are missing a component, but have a "U" in the right place for a component, we should supply "U" as the component. In this example SIMETHICONE is "U": 0.5% METHYL CELLULOSE 400 CP (MC)/0.1% SODIUM LAURYL SULFATE (SLS)/0.01% SIMETHICONE IN DEIONIZED WATER
# We need to define our level of confidence in our transformation.  Are we confident in the transformation only if there are no "U" rows? If so, we might want see how many of the "U" rows can be classified as "i".
###################################################################################

library(tidyverse);

vehicleTokenize <-function(string,tokens,firstRow=1)
{
  # "string" is a TRTV text string,  this function returns a table of vehicle component tokens.
  # This is accomplished by recursively calling this function. Looking in the "tokens" table
  # starting with row specified in the function parameter "firstRow".
  # The use if "firstRow" enables recursive calls avoid checking rows in the "tokens" table that were
  # already checked in a previous call.  The use if firstRow should increase performance.
  tokens1=NA
  tokens2=NA
  tokens3=NA
  pos <- 0
  posStop <- 0
  matchFound <- FALSE
  # look at each row in "tokens" starting with "firstRow" for a match in "string"
  row <- firstRow
  while((row<=nrow(tokens))&&(FALSE==matchFound))
  {
    pos <- regexpr(tokens$Source[row],string,ignore.case=TRUE)
    posStop <- attr(pos,"match.length") +pos-1
    if (pos>0)
    {
      matchFound <- TRUE
    }
    #print(paste("in:",string,"\ncompare:",tokens$Source[row],"\npos:",pos,"\nposStop:",posStop,"\n"))
    if (1<pos)
    {
      if (grepl("[a-zA-Z_]",substr(string,pos-1,pos-1)))
      { #if the match is the continuation of a previous word, this was not a match.
        #This check was introduced without any real evidence to indicate this was necessary.
        matchFound <- FALSE  
      }
    }
    if (nchar(string)>posStop)
    {
      if (grepl("[a-zA-Z_]",substr(string,posStop+1,posStop+1)))
      { #if the match includes the first part the following word, this was not a match
        #This check was introduced without any real evidence to indicate this was necessary.
        matchFound <- FALSE 
      }
    }
    if (!matchFound)
    {
      row <- row+1
    }
  }
  # if no matches in the tokens table were found, see if the string starts with a number
  if (!matchFound)
  {
    #ww When check whether a string is a number: if the string starts with a number, get the number and ignore the non-numeric part. 
    #E.g. "pH 6.0, Sterile Filtered", the ", Sterile Filtered" part will be ignored and number 6.0 will be returned.
    num_regx = "^\\s*[+-]{0,1}[0-9]*\\.{0,1}[0-9]+\\s*"
    numn1 <- regexpr(num_regx,string,FALSE)
    if (numn1 > 0 ){
      numn2 <-  attr(numn1,"match.length") +numn1 -1
      numstr <- substr(string, numn1, numn2)
      matchFound <-  TRUE;
      tokens2 <- data.frame(Source = string, 
                            Category = "n",
                            CTCode = NA,
                            Value = numstr,
                            stringsAsFactors=FALSE)
      return(tokens2)
    }
  }
  if (matchFound)
  {
    #print(tokens[row, ])
    # we have found a match for a portion of the string.
    # Tokenize the portion before the match into tokens1 
    #    the matching portion in tokens2
    #    the portion after the match in tokens3
    if (1==pos)
    {
      tokens1 <-NULL
    }
    else
    {
      #print(paste0("getting ready to parse into tokens1:", substr(string,1,pos-1)))
      tokens1 <- vehicleTokenize(substr(string,1,pos-1),tokens, row+1)
    }
    if (nchar(string)>posStop)
    {
      #print(paste0("getting ready to parse into tokens3:", substr(string,posStop+1,nchar(string))))
      tokens3 <- vehicleTokenize(substr(string,posStop+1,nchar(string)),tokens, row+1)
    }
    else
    {
      tokens3 <-NULL
    }
    #print(paste0("Category =",tokens1$Category[nrow(tokens1)]))
    #print(paste0("rows = ",nrow(tokens1)))
    tokens2 <- data.frame(Source = substr(string,pos,posStop), 
                          Category = tokens$Category[row],
                          CTCode = tokens$CTCode[row],
                          Value = tokens$Value[row],
                          stringsAsFactors=FALSE)
    
    return(rbind(tokens1,tokens2,tokens3))
  }
  # we get here only if we found no match.
  if (grepl("^\\s$",string))
  {
    tokens2 <- NULL
  }
  else
  {
    tokens2 <- data.frame(Source = string, 
                        Category = "U",
                        CTCode = NA,
                        Value = NA,
                        stringsAsFactors=FALSE)
  }
}

cleanTokens_ur <-function(TRTVtokens)
{
  # If we see "+/-" , a unit of measure, or (if after "f" a "-") and the previous row is "U",look for a number at the end of the "U" row.
  # Also if we see "+/-" or (if after "f" a "-") and the next row is "U", look for a number at the start of the "U" row.
  f_found <- FALSE
  i <- 2 #start with second row
  while (i <= nrow(TRTVtokens))
  {
    if ("f" == TRTVtokens$Category[i])
    {
      f_found <-TRUE
    }
    if ((("u" == TRTVtokens$Category[i])||("r" == TRTVtokens$Category[i])||(f_found&&("R" == TRTVtokens$Category[i])))&&("U" == TRTVtokens$Category[i-1]))
    {
      #look for a number at the end of Source in i-1 to go with the unit or +/- or -
      numStringPos <- regexpr("[+-]{0,1}[0-9]*\\.{0,1}[0-9]+\\s*$",TRTVtokens$Source[i-1])
      numStringPosStop <- attr(numStringPos,"match.length") +numStringPos-1
      num = substr(TRTVtokens$Source[i-1],numStringPos,numStringPosStop)

      #if a number was found at the end of the previous row.
      if (numStringPos>=2)
      {
        # make a new row for the "U" row for the next before the number
        tokens_U <- data.frame(Source=substr(TRTVtokens$Source[i-1],1,numStringPos-1),
                               Category = "U",
                               CTCode = NA,
                               Value = NA,
                               stringsAsFactors=FALSE)
        # make a new row for the new "n" row
        tokens_n <- data.frame(Source = num,
                               Category = "n",
                               CTCode = NA,
                               Value = num,
                               stringsAsFactors=FALSE)
        #make the new tokens table
        if (i >= 3)
        {
          TRTVtokens <- rbind(slice(TRTVtokens,1:(i-2)),tokens_U,tokens_n,slice(TRTVtokens,i:nrow(TRTVtokens)))
        }
        else
        {
          TRTVtokens <- rbind(tokens_U,tokens_n,slice(TRTVtokens,i:nrow(TRTVtokens)))
        }
      }
      else
      {
        # a number was not found at the end of the previous row to go with a "u" an "r" or an "R".
      }
    }
    if (i+1 <= nrow(TRTVtokens))
    {
      if ((("r" == TRTVtokens$Category[i])||(f_found&&("R" == TRTVtokens$Category[i])))&&("U" == TRTVtokens$Category[i+1]))
      {
        #look for a number at the start of Source in i+1 to go with the +/- or -
        numStringPos <-regexpr("^\\s*[+-]{0,1}[0-9]*\\.{0,1}[0-9]+",TRTVtokens$Source[i+1])
        numStringPosStop <- attr(numStringPos,"match.length")+numStringPos-1
        num = substr(TRTVtokens$Source[i+1],numStringPos, numStringPosStop)
        #print(paste("Source=",TRTVtokens$Source[i+1]))
        #print(paste("numStringPos=",numStringPos))
        #print(paste("numStringPosStop=",numStringPosStop))
        #print(paste("num =",num))
        #
        # if a number was found at the end of the following row... 
        if (numStringPosStop<nchar(TRTVtokens$Source[i+1]))
        {
          # make a row for the text before the number as a "U" row
          tokens_U <- data.frame(Source=substr(TRTVtokens$Source[i+1],numStringPosStop+1,nchar(TRTVtokens$Source[i+1])),
                                 Category = "U",
                                 CTCode = NA,
                                 Value = NA,
                                 stringsAsFactors = FALSE)
          # make a new row for the new "n" row
          tokens_n <- data.frame(Source=num,
                                 Category = "n",
                                 CTCode = NA,
                                 Value = num,
                                 stringsAsFactors = FALSE)
          #print(tokens_n)

          #make the new tokens table
          if (i+2 <= nrow(TRTVtokens))
          {
            TRTVtokens <- rbind(slice(TRTVtokens,1:i),tokens_n,tokens_U,slice(TRTVtokens,i+2:nrow(TRTVtokens)))
          }
          else
          {
            TRTVtokens <- rbind(slice(TRTVtokens,1:i),tokens_n,tokens_U)
          }
          
        }
        else
        {
          # A number was not found at the end of the previous row to go with the r.
        }
        #print(tokens_U)
        
      }
    }
    i <- i+1
  }
  return(TRTVtokens)
}


# if the same component listed in two consecutive rows (ignoring unknown rows) the source probably is providing a synonym. Ignore the duplicate.
cleanTokens_syn <-function(TRTVtokens)
{
  i <- 2
  while (i <= nrow(TRTVtokens))
  {
    if( ("c" == TRTVtokens$Category[i] ))
    {
      #we have found a component. See if the next row is also a component row, ignoring rows of category "U"
      j <- i +1
      while ((j <= nrow(TRTVtokens))&&("U" == TRTVtokens$Category[j]))
      {
        j <- j+1
      }
      if ((j <= nrow(TRTVtokens))&&("c" == TRTVtokens$Category[j])&&(TRTVtokens$Value[i] == TRTVtokens$Value[j]))
      {
        #we found that row j is also a a component row and has the same Value as row i.  Change row j to be ignored.
        TRTVtokens$Category[j] <- "i"
      }
    }
    i <- i+1
  }
  return(TRTVtokens)
}  

cleanTokens_R <-function(TRTVtokens)
{ # ignore R  before f.  Call this after cleanTokens_rn(). If you decide to change this to process R before f, also change the behaviour in the cleanTokens_ru() function.
  f_found <-FALSE
  #ww check if f exists, replace will occur only if f exists
  f_exist <-  FALSE;
  i <- 1
  while ( !f_exist && i <= nrow(TRTVtokens))
  {
    if( ("f" == TRTVtokens$Category[i] ) ){
      f_exist <- TRUE;
     }
      
    i <- i + 1
  }
  
  i <- 1
  while (f_exist && i <= nrow(TRTVtokens))
  {
    if( ("f" == TRTVtokens$Category[i] ))
    {
      f_found <- TRUE
    }
    if ("R" == TRTVtokens$Category[i])
    {
      if (f_found)
      {
        # no change
      }
      else
      {
        #we found an R before an "f"
        #   ignor the R now and ignore the number before and
        TRTVtokens$Category[i] <- "i"
        #if the n is right after the R and before an "f", ignore it also
        if ("n" == TRTVtokens$Category[i+1] )
        {
          TRTVtokens$Category[i+1] <- "i"
        }
      }
    }
    i <- i+1
  }
  return(TRTVtokens)
} 
# help function to support the creation of XV table for component, component can be in two formats:
# ((nu){1,2}c), which can be nunuc, nunuc
# or 
# (c(nu){0,2})), which can be cnunu, cnu, c.
makeXVComp <- function(TRTVtokens, xref, component_i, m, m_i, XVtable){
 
  mlen = attr(m[[1]],"match.length")
  
  if (m[[1]][m_i+2] > 0)
  {       
    # we have nunuc (match length =5) or nuc (match length = 3) depending on length of the match
    ind = m[[1]][m_i+2];
    mlength = mlen[m_i+2];
    if (mlength == 5 || mlength == 3){
      
      value = TRTVtokens$Value[xref[0+ind]]    #Since we have nunuc, 0 tokens from here is n
      valueU = TRTVtokens$Value[xref[1+ind]]   #Since we have nunuc, 1 token  from here is u
      valueC = if (mlength == 5) TRTVtokens$Value[xref[2+ind]] else NA   #if match lenght is 5, nunuc, 2 tokens from here is n
      valueCU = if (mlength == 5) TRTVtokens$Value[xref[3+ind]] else NA   #if match lenght is 5, nunuc, 3 token  from here is u
      if (!is.na(valueU) && !grepl("%", valueU)){
        valueC <- value
        valueCU <- valueU
        value <- NA
        valueU <- NA
      } 
      ##Treatment Vehicle Component row
      XVrow <- data.frame(STUDYID = NA, 
                DOMAIN = "XV",
                XVSEQ = nrow(XVtable) + 1,
                XVGRPID = 1,
                XVSGRPID = component_i,
                XVPARMCD = "TRTVC",
                XVPARM = "Treatment Vehicle Component",
                XVSPARM =  TRTVtokens$Value[xref[mlength - 1 +ind]],  #nunuc, 4 tokens from here is c; nuc, 2 tokens from her is c.
                XVVAL = value,
                XVVALU = valueU,
                XVVALNF = if (is.na(value)) "QS" else NA,
                stringsAsFactors=FALSE)
      XVtable <- rbind(XVtable,XVrow)
      ##Treatment Vehicle Component Concentration row
      if (!is.na(valueC)){
        XVrow <- data.frame(STUDYID = NA, 
               DOMAIN = "XV",
               XVSEQ = nrow(XVtable) + 1,
               XVGRPID = 1,
               XVSGRPID = component_i,
               XVPARMCD = "TRTVCC",
               XVPARM = "Treatment Vehicle Component Concentration",
               XVSPARM =  TRTVtokens$Value[xref[mlength - 1 +ind]],  #nunuc, 4 tokens from here is c; nuc, 2 tokens from her is c.
               XVVAL = valueC,
               XVVALU = valueCU,
               XVVALNF = NA,
               stringsAsFactors=FALSE)
        XVtable <- rbind(XVtable,XVrow)
      }
    }
  }
  else if ((m[[1]][m_i+4]>0))
  {
    # we have cnunu (match length = 5) or cnu (match length = 3) or c (match length = 1) depending on length of the match
    ind = m[[1]][m_i+4];
    mlength = mlen[m_i+4];
    if (mlength == 5 || mlength == 3 || mlength ==1){
      value = if (mlength >=3 ) TRTVtokens$Value[xref[1+ind]] else NA    #Since we have cnunu or cnu, 1 tokens from here is n
      valueU = if (mlength >= 3) TRTVtokens$Value[xref[2+ind]] else NA   #Since we have cnunu or cnu, 2 tokens from here is u
      valueC = if (mlength == 5) TRTVtokens$Value[xref[3+ind]] else NA   #Since we have cnunu, 3 tokens from here is u  
      valueCU = if (mlength == 5)TRTVtokens$Value[xref[4+ind]] else NA  #Since we have cnunu, 4 tokens from here is u
      if (!is.na(valueU) && !grepl("%", valueU)){
        valueC <- value
        valueCU <- valueU
        value <- NA
        valueU <- NA
      } 
      XVrow <- data.frame(STUDYID = NA, 
                DOMAIN = "XV",
                XVSEQ = nrow(XVtable) + 1,
                XVGRPID = 1,
                XVSGRPID = component_i,
                XVPARMCD = "TRTVC",
                XVPARM = "Treatment Vehicle Component",
                XVSPARM = TRTVtokens$Value[xref[0+ind]],  #Since we have cnunu, 0 tokens from here is c
                XVVAL = value,
                XVVALU = valueU,
                XVVALNF = if (is.na(value)) "QS" else NA,
                stringsAsFactors=FALSE)
      XVtable <- rbind(XVtable,XVrow)
      ##Treatment Vehicle Component Concentration row
      if (!is.na(valueC)){
        XVrow <- data.frame(STUDYID = NA, 
          DOMAIN = "XV",
          XVSEQ = nrow(XVtable) + 1,
          XVGRPID = 1,
          XVSGRPID = component_i,
          XVPARMCD = "TRTVCC",
          XVPARM = "Treatment Vehicle Component Concentration",
          XVSPARM = TRTVtokens$Value[xref[0+ind]],  #Since we have cnunu, 0 tokens from here is c
          XVVAL = valueC,
          XVVALU = valueCU,
          XVVALNF = NA,
          stringsAsFactors=FALSE)
        XVtable <- rbind(XVtable,XVrow)
      }
    } 
  }
  return(XVtable)
}
makeXV <- function(TRTVtokens, category_noU)
{
  # For best results, please apply the cleaning functions to TRTVtokens before calling this function.
  #
  # This function receives a table of the tokenized TRTV text string and examines the categories assigned to 
  # the tokens with extensive use of regular expressions to create a XV table like the XV 
  # example in the PHUSE whitepaper "Recommendations for Exchanging Vehicle details" that is similar to the 
  # AC domain in SENDIG-AR.

  # Let's start by creating regular expressions of the main parts of a vehicle text string starting with vehicle components
  # each component is either
  #     an amount and unit of measure (nu) followed by 
  #     a component name (c)
  #component_regx <- "(nuc)"
  component_regx <- "((nu){1,2}c)"
  # or
  #     a component name (c) followed by 
  #     an optional amount and unit of measure (nu)?
  #
  #component_regx <- paste("(",component_regx,"|","(c(nu)?))",sep="")
  component_regx <- paste("(",component_regx,"|","(c(nu){0,2}))",sep="")
  # Each dilluent has 1 component preceded by an "f" category token.
  #
  diluent_regx <- paste("f",component_regx,sep="")

  # Each attribute has
  #   an attribute name
  #   an attribute value
  #   optionally an attribute tolerance range
  attribute_regx <-paste("(an(rn|(R)n)?)",sep="")
  # We will later put these regular expressions together to describe the treatment vehicle text string.
  
  #Put the categories together into a single string and delete the tokens that are Unknown and those that should be ignored.
  #category_noU <- str_replace_all(paste(p$Category,collapse=""),"U|i","") 
  
  #ww remove the component after the attribute, attribute is the last group.
  a1 <- regexpr(attribute_regx,category_noU,ignore.case=TRUE)
  if (a1 > 0 ){
    a1Stop <- attr(a1,"match.length") +a1 -1
    category_noU <- substr(category_noU,1,a1Stop)
    #print(paste0("category_noU  = ",category_noU))
  }
  #The first row of our output data frame is the full text string.  Since this function doesn't receive the original text
  #string, it is recreated using the TRTVtokens$Source.
  XVtable <- data.frame(STUDYID = NA, 
                        DOMAIN = "XV",
                        XVSEQ = 1,
                        XVGRPID = 1,
                        XVSGRPID = NA,
                        XVPARMCD = "TRTV",
                        XVPARM = "Treatment Vehicle",
                        XVSPARM = NA,
                        XVVAL = paste0(TRTVtokens$Source,collapse = " "),
                        XVVALU = NA,
                        XVVALNF = NA,
                        stringsAsFactors=FALSE)
  
  # Before continuing, let's make sure we have a string we can understand
  # I expect one or more components
  # optionally one diluent
  # optionally one or more attributes
  if (TRUE == grepl(paste0("^",component_regx,"+(",diluent_regx,")?(",attribute_regx,")*$",sep=""),category_noU))
  {
    # The regular expression we just used includes repetitions of component_regx and attribute_regx. Let's determine the number of repetitions of each.
    # We will use this to create a regular expression with the correct number of repetitions, because we can then get an index telling us the start of each group.
    #
    # To determine the number of repetitions of component_regx, let's extract from category_noU the portion before the the diluent_regx or attribute_regx
    if (grepl(paste("(",diluent_regx,")|(",attribute_regx,")",sep=""),category_noU))
    {
      m <- regexec(paste("(",diluent_regx,")|(",attribute_regx,")",sep=""),category_noU) # m is assigned the character position of each match in category_nuU and 0 for parts that have no match.
      end_pos <- min(m[[1]][which(m[[1]]>0)])-1 # Find the first position of a match and subtract 1.
    } else
    {
      # we have no match, use the whole string
      end_pos <- nchar(category_noU)
    }
    x <- substr(category_noU,1,end_pos)
    component_reps <- str_count(x,component_regx)
    #
    # To determine the number of repetitions of attribute_regx, let's extract from category_noU the portion we didn't include last time.
    # This will include the final character of the previous search in the new search. This does no harm and avoids needing to check to see if the previous search's end is the end of the string.
    x <- substr(category_noU,end_pos,nchar(category_noU))
    attribute_reps <- str_count(x,attribute_regx)
    #print(paste0("components = ",component_reps," attributes = ",attribute_reps))
    
    # Now that we have the number of repetitions lets make a regular expression with the right number of repetitions of the components
    trtv_regx <- "^";
    i <- 1
    while (i <= component_reps)
    {
      trtv_regx <- paste0(trtv_regx,component_regx,sep="")
      i <- i+1
    }
    trtv_regx <- paste0(trtv_regx,"(",diluent_regx,")?",sep="")
    i <- 1
    while (i <= attribute_reps)
    {
      trtv_regx <- paste0(trtv_regx,"(",attribute_regx,")")
      i<- i+1
    }
    trtv_regx <- paste0(trtv_regx,"$")

    # testing the results
    #print(category_noU)
    #print(paste0("trtv_regx = ",trtv_regx))
    #print(paste0(grepl(trtv_regx,category_noU)))
    
    # store the charactor positions of each "(" in m[[1]][1]
    m <- regexec(trtv_regx,category_noU)
     
    # create xref to convert from character positions in m to rows in TRTVtokens.  We only need this because we deleted the U and i categories.
    xref <- c()
    i <-1
    j <-1
    while(j<=nrow(TRTVtokens))
    {
      if (grepl("U|i",TRTVtokens$Category[j]))
      {
        #skip this row
      }
      else
      {
        xref[i] <- j
        i <- i+1
      }
      j <- j+1 
    }

    # Create the XVtable start by adding rows for the components  "(((nu){1,2}c)|(c(nu){0,2}))"
    # item 2 will have nunuc or nuc depends on the matching length
    # item 3 will have nunuc or nuc depends on the matching length
    # item 4 will have nu
    # item 5 will have cnunu or c depends on the matching length.
    # item 6 will have nu
    # if any part is missing, the value in m will be zero.
    #
    component_i <- 1  #start the loop with the first component
    m_i <- 1 #start processing from the beginning element of m[[1]][]
    while (component_i <= component_reps)
    {
      XVtable = makeXVComp(TRTVtokens,xref, component_i, m, m_i, XVtable)
      m_i <-m_i +5 #There are 5 items in m for each component.
      component_i <- component_i +1
    }
    
    #Add rows to XVtable for diluent f((nuc)|(c(nu)?))
    if (m[[1]][m_i+1] >0)
    {
      #We have a diluent.  The code for a component above is copied here. I should have created a function.
      m_i <- m_i +1
      XVtable = makeXVComp(TRTVtokens,xref, component_i, m, m_i, XVtable)
      m_i <-m_i +5
    } 
    ## If no diluent component, still need to move the index for the next group.
    else{
      m_i <-m_i + 6
    }
    
    #Add rows to XVtable for the attributes (an(r|(R)n)?)
    attribute_i <- 1
    while (attribute_i <= attribute_reps)
    {
      ##ww Corrected indexes from if (m[[1]][m_i+3]>0)
      if (m[[1]][m_i+4]>0)
      {
        #we have an "anRn"
        #create row for "an"
        lowest <- as.numeric(TRTVtokens$Value[xref[1+m[[1]][m_i+1]]])
        highest <- as.numeric(TRTVtokens$Value[xref[3+m[[1]][m_i+1]]])
        XVrow <- data.frame(STUDYID = NA, 
                            DOMAIN = "XV",
                            XVSEQ = nrow(XVtable) + 1,
                            XVGRPID = 1,
                            XVSGRPID = component_i,
                            XVPARMCD = "TRTVP",
                            XVPARM = "Treatment Vehicle Property",
                            XVSPARM = TRTVtokens$Value[xref[0+m[[1]][m_i+1]]],
                            XVVAL = (highest+lowest)/2,
                            XVVALU = NA,
                            XVVALNF = NA,
                            stringsAsFactors=FALSE)
        XVtable <- rbind(XVtable,XVrow)
        #create row for "Rn"
        XVrow <- data.frame(STUDYID = NA, 
                            DOMAIN = "XV",
                            XVSEQ = nrow(XVtable) + 1,
                            XVGRPID = 1,
                            XVSGRPID = component_i,
                            XVPARMCD = "TRTVC",
                            XVPARM = "Treatment Vehicle Component",
                            XVSPARM = paste0(TRTVtokens$Value[xref[0+m[[1]][m_i+1]]]," Tolerance Range"),
                            XVVAL = (highest-lowest)/2,
                            XVVALU = NA,
                            XVVALNF = NA,
                            stringsAsFactors=FALSE)
        XVtable <- rbind(XVtable,XVrow)
      }
      else 
      {
        #ww Corrected indexes from if (m[[1]][m_i+2]>0)
        if (m[[1]][m_i+3]>0)
        {
          #we have an "anrn"
          #create row for "an"
          XVrow <- data.frame(STUDYID = NA, 
                              DOMAIN = "XV",
                              XVSEQ = nrow(XVtable) + 1,
                              XVGRPID = 1,
                              XVSGRPID = component_i,
                              XVPARMCD = "TRTVP",
                              XVPARM = "Treatment Vehicle Property",
                              XVSPARM = TRTVtokens$Value[xref[0+m[[1]][m_i+1]]],
                              XVVAL = TRTVtokens$Value[xref[1+m[[1]][m_i+1]]],
                              XVVALU = NA,
                              XVVALNF = NA,
                              stringsAsFactors=FALSE)
          XVtable <- rbind(XVtable,XVrow)
          #create row for "rn"
          XVrow <- data.frame(STUDYID = NA, 
                              DOMAIN = "XV",
                              XVSEQ = nrow(XVtable) + 1,
                              XVGRPID = 1,
                              XVSGRPID = component_i,
                              XVPARMCD = "TRTVC",
                              XVPARM = "Treatment Vehicle Component",
                              XVSPARM = paste0(TRTVtokens$Value[xref[0+m[[1]][m_i+1]]]," Tolerance Range"),
                              XVVAL = TRTVtokens$Value[xref[3+m[[1]][m_i+1]]],
                              XVVALU = NA,
                              XVVALNF = NA,
                              stringsAsFactors=FALSE)
          XVtable <- rbind(XVtable,XVrow)
        }
        else 
        {
          #ww Corrected indexes from if ((m[[1]][m_i+1]>0))
          if ((m[[1]][m_i+2]>0))
          {
            #we have "an" without "rn" or "Rn"
            #create row for "an"
            XVrow <- data.frame(STUDYID = NA, 
                                DOMAIN = "XV",
                                XVSEQ = nrow(XVtable) + 1,
                                XVGRPID = 1,
                                XVSGRPID = component_i,
                                XVPARMCD = "TRTVP",
                                XVPARM = "Treatment Vehicle Property",
                                XVSPARM = TRTVtokens$Value[xref[0+m[[1]][m_i+1]]],
                                XVVAL = TRTVtokens$Value[xref[1+m[[1]][m_i+1]]],
                                XVVALU = NA,
                                XVVALNF = NA,
                                stringsAsFactors=FALSE)
            XVtable <- rbind(XVtable,XVrow)
          }
        }
      }
      m_i <-m_i +3
      attribute_i <- attribute_i+1
    }
  } else
  {
    return(NA)
  }
  return(XVtable)
}
# trtv is the source vehicle string
# takens contains all the token records.
ParseTRTV <- function(trtv, tokens){
  trtvOrg = trtv
  Encoding(trtvOrg)<-"UTF-8"
  #print(paste0("Vehicle:",as.character(trtvOrg)))
  ##ww replace the � with +/-
  #trtvNew = str_replace_all(trtvOrg,"�","+/-")
  trtvNew = str_replace_all(trtvOrg,"\u00B1","+/-")
  ##ww Replace "to" in the attribute string with a DASH
  ph_regx <- "[p|P][h|H](\\s)*[0-9]+(\\.[0-9]+)?(\\s)*[t|T][o|O]"
  
  phn1 <- regexpr(ph_regx,trtvNew,FALSE)
  if (phn1 > 0 ){
    phn2 = attr(phn1,"match.length") +phn1 -1
    ## Original attribute string
    phs1 = substr(trtvNew, phn1, phn2)
    ## Replace "to" with a DASH
    phs2 = paste0(substr(trtvNew, phn1, phn2-2), "-")
    ## The new vehicle string
    trtvNew <- str_replace_all(trtvNew,ph_regx,phs2)
  }
  print(paste0("Vehicle to be parsed:",as.character(trtvNew)))
  p<-vehicleTokenize(as.character((trtvNew)), tokens)
  
  p<-cleanTokens_syn(cleanTokens_R(cleanTokens_ur(p)))
  print(p$Category)
  category_noU <- str_replace_all(paste(p$Category,collapse=""),"U|i","")
  print(paste0("category_noU  = ",category_noU))
  #print(grepl("^((nuc)|(c(nu)?))+(f((nuc)|(c(nu)?)))?(an(rn)?)*$",category_noU))
  t<-makeXV(p,category_noU)
  if (is.null(ncol(t)))
  {
    print(paste0("Vehicle Parse failed:",as.character(trtv)))
  }
  return(t)
}
#####################################################################################################33
#set the working directory
(pathBase <- getwd())
if (!is.null(pathBase)) setwd(pathBase)

logFile <- paste0(pathBase,"/output/trtv-parse.log") 
OutCsvFile <- NA
# read the table of Treatment Vehicles that need to be parsed.  Each line is a copy of the TRTV parameer from ts.xpt in a SEND package
trtv <- read.table(paste0(pathBase,"/trtv_test"),sep="|");
#trtv <- read.table(paste0(pathBase,"/BioCelerate_TRTV.csv"),sep="|");
#read the table of component names that are already sorted from longest text string to shortest text string.
tokensTable <- read.table(paste0(pathBase,"/tokens.csv"),sep=",", header = TRUE,stringsAsFactors=FALSE);
sink(file = logFile)

tRow <- 1
nSuccess <- 0
while (tRow <= nrow(trtv))
{
  ##ww Pre-process the vehicle string
  ## 1. Replace the unicode � with +/-
  ## 2. Replace to in the attribute string with ad DASH, e.g. "PH 3 to 5" to "PH 3 - 5"
 
  trtvOrg = trtv[tRow, ]
  t = ParseTRTV(trtvOrg, tokensTable)
  if (!is.null(ncol(t)))
  {
    OutCsvFile <- paste0(pathBase,"/output/xv",tRow, ".csv")
    write.csv(t,file=OutCsvFile,na="",row.names=FALSE)
    nSuccess <- nSuccess +1
  }
  tRow <-tRow+1
}
print(paste0("We eavluated ",nrow(trtv)," treatment vehicle descriptions and created XV tables for ",nSuccess," of them = ",100*nSuccess/nrow(trtv),"%"))
# revert sink back to console
sink(type = "message")
sink()