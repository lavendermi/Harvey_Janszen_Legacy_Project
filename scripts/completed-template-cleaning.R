###############################################################
####### ocurrence data entry template cleaning and checking ###

#1) identify rows with missing info for vName, vSciName, sciName, and entries in data entry remarks

# check that UTM has right number of digits 

# check that all rows have crucial info
  # archive ID - check that all contain HJ and number
  # pageNum - check numeric, check below resonable threshold and > 0
  # numpage check numeric, check below resonable threshold and > 0
  # conf - either h m l 
  # vTaxonRank - from acceptable set of strings
  # occStatus - either present or absent
  # date - within certain date range and numeric
  # locality
  # coutnry - check if spelled properly
  # stateProvince - check if spelled properly
  # county - check if spelled properly
  # recordedby 
  #velev - only numeric (meters implied)
  # v elev reference as character sting
  # vlat as decimal or 3 segments - and within certain range that would match where he collected with some buffer though
  # vUTM - the right number of elements and with 10U and starting with 3 or 4 and 4 5 or 6 --> look at map to check this 
  # coord uncertainty - only numeric (meters implied)