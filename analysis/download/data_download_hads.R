# Script for downloading data from the HADS network
# HADS van be downloaded programmatically using URL builder below

# This script is currently (2021-07-22) set up to download a selection of:
# COOP and DCP stations in Nevada and California near Lake Tahoe
# For calendar years 2020 and 2021

# The addition of more states and stations will require identification
# on the HADS portal:
# https://mesonet.agron.iastate.edu/request/dcp/fe.phtml

# Keith Jennings
# kjennings@lynker.com
# 2021-07-22

# User-specified options
# Networks
hads_url02_net = c('NV_DCP', 
                   'NV_COOP',
                   'CA_DCP', 
                   'CA_COOP')
# Stations
hads_url03_sta = c('&stations=CVDN2&stations=KNXN2&',
                   '&stations=GALN2&',
                   '&stations=BDMC1&stations=CNLC1&stations=DNRC1&stations=DUCC1&stations=BTUC1&stations=LSPC1&stations=SKBC1&stations=LLSC1&stations=MELC1&stations=CKGC1&stations=ONCC1&stations=OWNC1&stations=RNYC1&stations=HVSC1&',
                   '&stations=FOIC1&stations=GKSC1&stations=HLLC1&stations=HYSC1&stations=MTSC1&stations=SEYC1&stations=TRTC1&')

# Year, month, day, hour, and minute of start and stop
# You can only access 1 year of data at a time, there is no year2
# Your url can specify future dates (they won't be downloaded obviously, 
# but they also won't throw an error)
hads_url05_yr1 = c(2020, 2021)
hads_url07_mo1 = 1
hads_url09_dy1 = 1
hads_url11_hr1 = 0
hads_url13_mi1 = 0
hads_url15_mo2 = 12
hads_url17_dy2 = 31
hads_url19_hr2 = 23
hads_url21_mi2 = 59

# Build the URL string
hads_url01_str = "https://mesonet.agron.iastate.edu/cgi-bin/request/hads.py?network="
hads_url04_str = "year="
hads_url06_str = "&month1="
hads_url08_str = "&day1="
hads_url10_str = "&hour1="
hads_url12_str = "&minute1="
hads_url14_str = "&month2="
hads_url16_str = "&day2="
hads_url18_str = "&hour2="
hads_url20_str = "&minute2="

# Reset the timeout option so download can finish (default is 60 seconds)
# getOption('timeout')
# [1] 60
options(timeout = 1000) # this will reset to default when session terminated

# Loop through the options and download HADS data
for(i in seq_along(hads_url02_net)){
  # Sub loop by years
  for(j in seq_along(hads_url05_yr1)){
    dest = paste0('data/met/hads_',
                  hads_url02_net[i],
                  '_',
                  hads_url05_yr1[j],
                  '.txt')
    source = paste0(hads_url01_str, hads_url02_net[i], hads_url03_sta[i],
                    hads_url04_str, hads_url05_yr1[j], hads_url06_str,
                    hads_url07_mo1, hads_url08_str,    hads_url09_dy1,
                    hads_url10_str, hads_url11_hr1,    hads_url12_str,
                    hads_url13_mi1, hads_url14_str,    hads_url15_mo2,
                    hads_url16_str, hads_url17_dy2,    hads_url18_str,
                    hads_url19_hr2, hads_url20_str,    hads_url21_mi2)
    download.file(url = source, destfile = dest)
  }
}
