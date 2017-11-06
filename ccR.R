#####################################################################################################################################
#----------------                                                                                                                   #
# ccR                                                                                                                               #
#----------------                                                                                                                   #
#                                                                                                                                   #
# Set of R functions to interface with the Constant Contact API                                                                     #
#                                                                                                                                   #
# inspired by: https://technistas.com/2013/04/03/working-with-json-rest-apis-from-r/                                                #
#                                                                                                                                   #
#                                                                                                                                   #
# Created by: Nick Holt, PhD, Institute for Advanced Analytics, Bellarmine University                                               #
# Created on: 10-13-17                                                                                                              #
#                                                                                                                                   #
#                                                                                                                                   #
# Current Status: In development                                                                                                    #
#                                                                                                                                   #
# Last edited: 11-6-17                                                                                                              #
#                                                                                                                                   #
# Log:                                                                                                                              #
#       # 10-13-17: Established ability to retreive full campaign list using 'next' API argument                                    #
#       # 11-03-17: Built and tested functions to extract campaigns and all contacts for a given campaign                           #
#       # 11-06-17: Built and tested functions to extract specific actions for a campaign and all actions for a campaign            #
#                                                                                                                                   #
#####################################################################################################################################

# get access token via: https://constantcontact.mashery.com/io-docs

# dependencies
library(RCurl)
library(rjson)
library(jsonlite)
library(stringr)
library(lubridate)
library(purrr)
library(dplyr)

#------------------------------------------------------
# Function to transform Campaign JSON into data frame
#------------------------------------------------------
getCampaignData <- function(campaignJSON) {
        JSONList <- jsonlite::fromJSON(campaignJSON)
        results <- JSONList$results
        return(results)
}

#-----------------------------------------------------
# Function to extract campaign data for all campaigns
#-----------------------------------------------------
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

#----------------------------------------------------------------------------------------------------
# General function to extract information about a variety of different actions for a given campaign
#----------------------------------------------------------------------------------------------------

# extractAction function can take the following arguments:
        # "clicks" -- returns information about all clicks for a given campaign_id (a click implies that the email was also opened)
        # "opens" -- returns information about all opens for a given campaign_id (clicks are not double counted in opens)
        # "forwards" -- returns information about all forwards for a given campaign_id
        # "unsubscribes" -- returns information about all unsubscribes for a given campaign_id
        # "bounces" -- returns information about all bounces for a given campaign_id

extractAction <- function(action_type, campaign_id){
        
        # build getActions function to extract JSON to df
        getActions <- function(actionsJSON){
                JSONList <- jsonlite::fromJSON(actionsJSON)
                results <- JSONList$results
                return(results)
        }
        
        # set clicks.dataframe to NULL
        actions.dataframe <- NULL
        
        # start url
        start_url <- paste0("https://api.constantcontact.com/v2/emailmarketing/campaigns/", campaign_id, "/tracking/", action_type, "?limit=500&access_token=", access_token, "&api_key=", api_key)
        
        # Get first page of contacts
        actionsJSON <- getURL(start_url)
        actions.dataframe <- getActions(actionsJSON)
        
        # Get list of pages
        nextlist <- paginator(start_url)
        
        # loop to build full contacts list from list of pages
        for(i in seq_along(nextlist)){
                nexti <- nextlist[i]
                actionsJSON = getURL(url = paste0("https://api.constantcontact.com", nexti, "&access_token=", access_token, "&api_key=", api_key))
                actions.dataframe <- rbind(actions.dataframe, getActions(actionsJSON))
        }
        return(actions.dataframe)
}

#-------------------------------------------------------
# Function to extract all actions for a given campaign
#-------------------------------------------------------

# possible actions: 
        # Bounce
        # Open
        # Click
        # Forward
        # Unsubscribe

extractCampaignActions <- function(campaign_id){
        
        # extract actions into separate dfs
        clicks.dataframe <- extractAction("clicks", campaign_id)
        opens.dataframe <- extractAction("opens", campaign_id)
        forwards.dataframe <- extractAction("forwards", campaign_id)
        unsubscribes.dataframe <- extractAction("unsubscribes", campaign_id)
        bounces.dataframe <- extractAction("bounces", campaign_id)
        
        # build dummy dataframe with all possible columns
        dummy.actions <- data.frame(matrix(ncol = 15)) %>%
                filter(is.na(X1) == F)
        colnames(dummy.actions) <- c("activity_type", "campaign_id", "contact_id", "email_address", "link_id", "click_date", "open_date", "unsubscribe_date", "unsubscribe_source", "unsubscribe_reason", "forward_date", "bounce_code", "bounce_description", "bounce_message", "bounce_date")
        
        # combine results into one consistent data frame
        actions <- bind_rows(dummy.actions, clicks.dataframe, opens.dataframe, forwards.dataframe, unsubscribes.dataframe, bounces.dataframe)
        return(actions)
}

