import os
import pandas as pd
import numpy as np
import wrds
from datetime import date

today = date.today()

start_yr = 1925
end_yr = today.strftime("%Y")

db = wrds.Connection() # pw: 

# %% ---------------------------------------------------------------------------------
# CRSP from WRDS API
# ---------

###############
####### daily
###############

## individual assets

parm = {'permno': tuple([14593]),
        'startdate': '1/1/' + str(start_yr),
        'enddate': '12/31/' + str(end_yr)}

parm = {'startdate': '1/1/' + str(start_yr),
        'enddate': '12/31/' + str(end_yr)}



# %% ---------------------------------------------------------------------------------
# Valuation Ratios from WRDS API
# ---------

parm = {'startdate': str(start_yr)+'0101' ,
        'enddate': str(end_yr)+'1231'}   #yyyymmdd


valuation_ratios = db.raw_sql("""
                          SELECT a.*
                          FROM wrdsapps.firm_ratio as a
                          WHERE a.public_date between %(startdate)s and %(enddate)s
                      """, params=parm)


path = 'D:\\databases\\wrds\\valuation_ratios\\valuation_ratios' + today.strftime("%d%m%Y") + ".csv"

# save the data to a local file
valuation_ratios.to_csv(path, index=False)
        




