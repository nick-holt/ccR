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

# converter function for JSON to df from API

getCampaignData <- function(campaignJSON) {
        idlist <- NULL
        namelist <- NULL
        urllist <- NULL
        statuslist <- NULL
        datelist <- NULL
        JSONList <- jsonlite::fromJSON(campaignJSON)
        results <- JSONList$results
        #for (i in 1:length(results)) {
        #        namelist <- c(namelist, results[i][[1]]$name )
        #        urllist <- c(urllist, paste("https://api.constantcontact.com/v2/emailmarketing/campaigns/", results[i][[1]]$id, sep="", collapse=NULL))
        #        statuslist <- c(statuslist, results[i][[1]]$status)
        #        datelist <- c(datelist, results[i][[1]]$modified_date)
        #        idlist <- c(idlist, results[i][[1]]$id)
        #}
        #campaignDF = data.frame(id = idlist, name = namelist, url = urllist, status = statuslist, last_modified_date=datelist,stringsAsFactors=FALSE) %>%
        #        mutate(last_modified_date = ymd(str_extract(last_modified_date, "\\d+-\\d+-\\d+")))
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

contactsJSON = getURL("https://api.constantcontact.com/v2/emailmarketing/campaigns/1120949536316/tracking/sends?next=bGltaXQ9NTAwJm5leHQ9MTExMzY3NzA1NDU1MSVhZTY2MDk2NS1mMDUzLTQwZmEtOTg2OS04NjI1Y2VjZjdlN2IlQUFnQUFBRk5JOGhRd0FBQUVGTlh4Q0IrU1JIamlhN1VybEthaDRZQUFCQlVQOW93ZmtrUjQ0cEExSzVTbW9lR0FBQUFBQT09&access_token=543671a7-e5cb-4c37-8029-13101d5bfbf1&api_key=qy2gc5nd3pw5g6rj6tmc5zwt")

# function to get contacts for a given campagin id
getCampaignContacts <- function(contactsJSON) {
        
        # initialize lists
        #idlist <- NULL
        #emaillist <- NULL
        #activitylist <- NULL
        #campaignlist <- NULL
        #datelist <- NULL
        
        # convert JSON to list
        JSONList <- jsonlite::fromJSON(contactsJSON)
        results <- JSONList$results
        
        # loop over elements and extract information into lists
        #for (i in 1:length(results)) {
        #        idlist <- c(idlist, results[i][[1]]$contact_id)
        #        emaillist <- c(emaillist, results[i][[1]]$email_address)
        #        activitylist <- c(activitylist, results[i][[1]]$activity_type )
        #        campaignlist <- c(campaignlist, results[i][[1]]$campaign_id)
        #        datelist <- c(datelist, results[i][[1]]$send_date)
        #}
        
        # combine list results in to data frame
        #contactsDF <- data.frame(contact_id = idlist, email = emaillist, activity = activitylist, campaign_id = campaignlist, send_date=datelist, stringsAsFactors=FALSE) %>%
        #        mutate(send_date = ymd(str_extract(send_date, "\\d+-\\d+-\\d+")))
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
campaign_id <- target_ids[14]

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





