library(redcapAPI)

# Your API key here
apikey = "XXX"
url = "https://redcap.iths.org/api/"
rcon = redcapConnection(        
  url = "https://redcap.iths.org/api/",        
  token = apikey    
)    

# get meta-data from the survey (e.g., questions, response options) 
metadata = exportMetaData(rcon=rcon)    

# get survey data
# labels = FALSE makes sure not to get labels
weekly_checkin = exportRecords(rcon=rcon, factors=F, labels = FALSE)    
weekly_checkin = weekly_checkin[grep("weekly_checkin", weekly_checkin$redcap_event_name),] 
