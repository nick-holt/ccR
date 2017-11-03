#----------------
# ccR
#----------------
#
# Created by: Nick Holt, PhD, Institute for Advanced Analytics, Bellarmine University
# Created on: 10-13-17
#
# Last edited: 11-3-17
#       # Established ability to retreive full campaign list using 'next' API argument
#       # Built and tested functions to extract campaigns and all contacts for a given campaign
#
#
# Current Status: In development 
#
# Set of R functions to interface with the Constant Contact API
# initially inspired by: https://technistas.com/2013/04/03/working-with-json-rest-apis-from-r/


# OAuthAccessToken Info
username <- "<insert username>"
api_key <- "<insert API key>"
app_name <- "ccR"
access_token <- "<insert access token>"

        # get access token via: https://constantcontact.mashery.com/io-docs

# dependencies
library(RCurl)
library(rjson)
library(jsonlite)
library(stringr)
library(lubridate)
library(purrr)

#----------------------------------------------
# Transform Campaign JSON into data frame
#----------------------------------------------
getCampaignData <- function(campaignJSON) {
        JSONList <- jsonlite::fromJSON(campaignJSON)
        results <- JSONList$results
        return(results)
}

#---------------------------------------------
# Extract campaign data for all campaigns
#---------------------------------------------
extractAllCampaigns <- function(){
        start_url <- paste0("https://api.constantcontact.com/v2/emailmarketing/campaigns?status=ALL&access_token=", access_token, "&api_key=", api_key)
        campaignJSON <- getURL(start_url)
        campaign.dataframe <- getCampaignData(campaignJSON)
        
        # Get list of pages
        nextlist <- paginator(start_url)
        
        # loop to build full contacts list from list of pages
        for(i in seq_along(nextlist)){
                nexti <- nextlist[i]
                campaignJSON = getURL(url = paste0("https://api.constantcontact.com", nexti, "&access_token=", access_token, "&api_key=", api_key))
                campaign.dataframe <- rbind(campaign.dataframe, getCampaignData(campaignJSON))
        }
        return(campaign.dataframe)
}

#-------------------------------------------------------------
# Function to extract information from a page of contact info
#-------------------------------------------------------------
getCampaignContacts <- function(contactsJSON) {
        JSONList <- jsonlite::fromJSON(contactsJSON)
        results <- JSONList$results
        return(results)
}

#---------------------------------------------
# Function to extract pagination urls to list
#---------------------------------------------
paginator <- function(start_url){
        page_list <- NULL
        page <- getURL(start_url)
        contents <- rjson::fromJSON(page)
        start_page_meta <- contents$meta
        next_link <- NULL
        next_link <- start_page_meta[1]$pagination$next_link
        while(is.null(next_link) == FALSE){
                page_list <- c(page_list, next_link)
                next_page <- getURL(url = paste0("https://api.constantcontact.com", next_link, "&access_token=", access_token, "&api_key=", api_key))
                contents <- fromJSON(next_page)
                next_page_meta <- contents$meta
                next_link <- next_page_meta[1]$pagination$next_link
                next_link
        }
        return(page_list)
}

#-------------------------------------------------------------------------------
# Function to loop over pagination within a campaign and extract all contacts
#-------------------------------------------------------------------------------
extractCampaignContacts <- function(campaign_id){
        
        # set contacts.dataframe to NULL
        contacts.dataframe <- NULL
        
        # start url
        start_url <- paste0("https://api.constantcontact.com/v2/emailmarketing/campaigns/", campaign_id, "/tracking/sends?limit=500&access_token=", access_token, "&api_key=", api_key)
        
        # Get first page of contacts
        contactsJSON <- getURL(start_url)
        contacts.dataframe <- getCampaignContacts(contactsJSON)

        # Get list of pages
        nextlist <- paginator(start_url)
        
        # loop to build full contacts list from list of pages
        for(i in seq_along(nextlist)){
                nexti <- nextlist[i]
                contactsJSON = getURL(url = paste0("https://api.constantcontact.com", nexti, "&access_token=", access_token, "&api_key=", api_key))
                contacts.dataframe <- rbind(contacts.dataframe, getCampaignContacts(contactsJSON))
        }
        return(contacts.dataframe)
}
