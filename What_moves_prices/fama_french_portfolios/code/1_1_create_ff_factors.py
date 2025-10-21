##########################################
# Fama French 3 Factors + 10 Portfolios  #
# Fabio Girardi                          #
# Date: June 2023                        #
# Updated: June 2023                     #
##########################################

import pandas as pd
import numpy as np
import datetime as dt
import wrds
#import psycopg2 
import matplotlib.pyplot as plt
from dateutil.relativedelta import *
from pandas.tseries.offsets import *
from scipy import stats
import quandl 
from fredapi import Fred


# Set your Quandl API key
quandl.ApiConfig.api_key = 'e9x5YJCqg37PxC-ye_oz'  # Replace with your actual API key
# Set your FRED API key
fred = Fred(api_key='05b7028e75efcfb893c3f3344e587442')  # Replace with your actual FRED API key

#### Period

start_date = '01/01/1959'
end_date = '2022/12/31'


#### What do you need?

is_market_factor = True
factors3 = True
portfolios10 = True

###################
# Connect to WRDS #
###################

conn=wrds.Connection(username='girardifab') # 


###################
# Market Factor   #
###################

if is_market_factor == True:
    
    # Query the WRDS API to get market return data for the CRSP US Total Market Index
    market_returns = conn.raw_sql(f"""
        SELECT date, vwretd as market_return_with, vwretx as market_return_without
        FROM crsp.msi
        WHERE date >= '{start_date}' AND date <= '{end_date}'
    """)

    market_returns['date'] = pd.to_datetime(market_returns['date'],format = "%Y-%m-%d")
    market_returns['date'] = market_returns['date'] + MonthEnd(0)
    market_returns['market_return_with'] = pd.to_numeric(market_returns['market_return_with'])
    market_returns['market_return_without'] = pd.to_numeric(market_returns['market_return_without'])

    # Download the data
    risk_free_rate_data = fred.get_series('TB3MS', start=start_date, end=end_date)
    risk_free_rate_data = risk_free_rate_data.reset_index()
    risk_free_rate_data.rename(columns={'index': 'date', 0: 'risk_free_rate'}, inplace=True)
    risk_free_rate_data['risk_free_rate'] = risk_free_rate_data['risk_free_rate'] / 1200
    risk_free_rate_data['date'] = pd.to_datetime(risk_free_rate_data['date'])
    risk_free_rate_data['date'] = risk_free_rate_data['date'] + MonthEnd(0)
    risk_free_rate_data = risk_free_rate_data.reset_index(drop = True)

    market_factor = pd.merge(market_returns,risk_free_rate_data, how = "inner", on='date')
    market_factor['MKT_with'] = market_factor["market_return_with"] - market_factor["risk_free_rate"]
    market_factor['MKT_without'] = market_factor["market_return_without"] - market_factor["risk_free_rate"]
    
    del market_returns, risk_free_rate_data

market_factor.mean(axis=0)[1:6]*12



###################
# Compustat Block #
###################
comp = conn.raw_sql("""
                    select gvkey, datadate, at, pstkl, txditc,
                    pstkrv, seq, pstk
                    from comp.funda
                    where indfmt='INDL' 
                    and datafmt='STD'
                    and popsrc='D'
                    and consol='C'
                    and datadate >= '01/01/1959'
                    """, date_cols=['datadate'])

comp['year']=comp['datadate'].dt.year

# create preferrerd stock

comp['ps'] = np.where(comp['pstkrv'].isnull(), comp['pstkl'], comp['pstkrv'])
comp['ps'] = np.where(comp['ps'].isnull(),comp['pstk'], comp['ps'])
comp['ps'] = np.where(comp['ps'].isnull(),0,comp['ps'])
comp['txditc'] = comp['txditc'].fillna(0)

# create book equity

comp['be']=comp['seq']+comp['txditc']-comp['ps'] # total equity + Deferred Taxes and Investment Tax Credit - Preferred Stock
comp['be']=np.where(comp['be']>0, comp['be'], np.nan)

# number of years in Compustat

comp=comp.sort_values(by=['gvkey','datadate'])
comp['count']=comp.groupby(['gvkey']).cumcount()

comp=comp[['gvkey','datadate','year','be','count']]

#comp.to_csv("D:\\databases\\wrds\\crsp\\cross_section\\comp.csv", index=False)
#comp = pd.read_csv("D:\\databases\\wrds\\crsp\\cross_section\\comp.csv", engine = "python")

comp['datadate'] = pd.to_datetime(comp['datadate'])
comp['jdate'] = comp['datadate'] + MonthEnd(0)


###################
# CRSP Block      #
###################
# sql similar to crspmerge macro
crsp_m = conn.raw_sql("""
                      select a.permno, a.permco, a.date, b.shrcd, b.exchcd,
                      a.ret, a.retx, a.shrout, a.prc
                      from crsp.msf as a
                      left join crsp.msenames as b
                      on a.permno=b.permno
                      and b.namedt<=a.date
                      and a.date<=b.nameendt
                      where a.date between '01/01/1959' and '12/31/2023'
                      and b.exchcd between 1 and 3
                      """, date_cols=['date']) 


# change variable format to int
crsp_m[['permco','permno','shrcd','exchcd']]=crsp_m[['permco','permno','shrcd','exchcd']].astype(int)

#crsp_m.to_csv("D:\\databases\\wrds\\crsp\\cross_section\\crsp_m.csv", index=False)
#crsp_m = pd.read_csv("D:\\databases\\wrds\\crsp\\cross_section\\crsp_m.csv", engine = "python")

# Line up date to be end of month
crsp_m['date'] = pd.to_datetime(crsp_m['date'])
crsp_m['jdate'] = crsp_m['date'] + MonthEnd(0)


# add delisting return
dlret = conn.raw_sql("""
                     select permno, dlret, dlstdt 
                     from crsp.msedelist
                     """, date_cols=['dlstdt'])


dlret.permno=dlret.permno.astype(int)
#dlret['dlstdt']=pd.to_datetime(dlret['dlstdt'])
dlret['jdate']=dlret['dlstdt']+MonthEnd(0)

crsp = pd.merge(crsp_m, dlret, how='left',on=['permno','jdate'])

crsp['dlret']=crsp['dlret'].fillna(0)
crsp['ret']=crsp['ret'].fillna(0)

# retadj factors in the delisting returns
crsp['retadj']=(1+crsp['ret'])*(1+crsp['dlret'])-1
crsp['retxadj']=(1+crsp['retx'])*(1+crsp['dlret'])-1

# calculate market equity
crsp['prc'] = crsp['prc'].abs()
crsp['me'] = crsp['prc'].abs()*crsp['shrout'] 


crsp=crsp.drop(['dlret','dlstdt','shrout'], axis=1)
crsp=crsp.sort_values(by=['jdate','permco','me'])


### Up to hear, I create  adjusted returns and market equity for the current date

### End section


### Aggregate Market Cap ###

# sum of me across different permno belonging to same permco a given date
crsp_summe = crsp.groupby(['jdate','permco'])['me'].sum().reset_index()

# largest mktcap within a permco/date
crsp_maxme = crsp.groupby(['jdate','permco'])['me'].max().reset_index()

# join by jdate/maxme to find the permno
crsp1=pd.merge(crsp, crsp_maxme, how='inner', on=['jdate','permco','me'])

# drop me column and replace with the sum me
crsp1=crsp1.drop(['me'], axis=1)

# join with sum of me to get the correct market cap info
crsp2=pd.merge(crsp1, crsp_summe, how='inner', on=['jdate','permco'])

# sort by permno and date and also drop duplicates
crsp2=crsp2.sort_values(by=['permno','jdate']).drop_duplicates()
crsp2['year']=crsp2['jdate'].dt.year
crsp2['month']=crsp2['jdate'].dt.month

### Added

crsp2.loc[crsp2.permno==42040]
crsp2['ret_delta'] = crsp2['ret'] - crsp2['retx']  
crsp2['prc_lag'] = crsp2.groupby(['permno'])['prc'].shift(1)
crsp2['div'] = crsp2['ret_delta']*crsp2['prc_lag'] 
crsp2['div_smooth'] = crsp2.groupby('permno')['div'].transform(lambda x: x.rolling(window = 12).sum())

###


# keep December market cap
decme=crsp2[crsp2['month']==12]
decme=decme[['permno','date','jdate','me','year']].rename(columns={'me':'dec_me'})

### July to June dates
crsp2['ffdate']=crsp2['jdate']+MonthEnd(-6)
crsp2['ffyear']=crsp2['ffdate'].dt.year
crsp2['ffmonth']=crsp2['ffdate'].dt.month
crsp2['1+retx']=1+crsp2['retx']
crsp2=crsp2.sort_values(by=['permno','date'])

# cumret by stock
crsp2['cumretx']=crsp2.groupby(['permno','ffyear'])['1+retx'].cumprod()

# lag cumret
crsp2['lcumretx']=crsp2.groupby(['permno'])['cumretx'].shift(1)

# lag market cap
crsp2['lme']=crsp2.groupby(['permno'])['me'].shift(1)

# if first permno then use me/(1+retx) to replace the missing value
crsp2['count']=crsp2.groupby(['permno']).cumcount()
crsp2['lme']=np.where(crsp2['count']==0, crsp2['me']/crsp2['1+retx'], crsp2['lme'])

# baseline me
mebase=crsp2[crsp2['ffmonth']==1][['permno','ffyear', 'lme']].rename(columns={'lme':'mebase'})

# merge result back together
crsp3=pd.merge(crsp2, mebase, how='left', on=['permno','ffyear'])
crsp3['wt']=np.where(crsp3['ffmonth']==1, crsp3['lme'], crsp3['mebase']*crsp3['lcumretx'])

decme['year']=decme['year']+1
decme=decme[['permno','year','dec_me']]


# Info as of June
crsp3_jun = crsp3[crsp3['month']==6]

crsp_jun = pd.merge(crsp3_jun, decme, how='inner', on=['permno','year'])
crsp_jun=crsp_jun[['permno','date', 'jdate', 'shrcd','exchcd','retadj','retxadj','me','wt','cumretx','mebase','lme','dec_me']]
crsp_jun=crsp_jun.sort_values(by=['permno','jdate']).drop_duplicates()


#######################
# CCM Block           #
#######################
ccm=conn.raw_sql("""
                  select gvkey, lpermno as permno, linktype, linkprim, 
                  linkdt, linkenddt
                  from crsp.ccmxpf_linktable
                  where substr(linktype,1,1)='L'
                  and (linkprim ='C' or linkprim='P')
                  """, date_cols=['linkdt', 'linkenddt'])

ccm[['gvkey','permno']]=ccm[['gvkey','permno']].astype(int)

# if linkenddt is missing then set to today date
ccm['linkenddt']=ccm['linkenddt'].fillna(pd.to_datetime('today'))

comp[['gvkey']] = comp[['gvkey']].astype("int")

ccm1=pd.merge(comp[['gvkey','datadate','be', 'count']],ccm,how='left',on=['gvkey']) #!
ccm1['yearend']=ccm1['datadate']+YearEnd(0)
ccm1['jdate']=ccm1['yearend']+MonthEnd(6)


# set link date bounds
ccm2=ccm1[(ccm1['jdate']>=ccm1['linkdt'])&(ccm1['jdate']<=ccm1['linkenddt'])]
ccm2=ccm2[['gvkey','permno','datadate','yearend', 'jdate','be', 'count']]

# link comp and crsp
ccm_jun=pd.merge(crsp_jun, ccm2, how='inner', on=['permno', 'jdate'])
ccm_jun['beme']=ccm_jun['be']*1000/ccm_jun['dec_me']

# select NYSE stocks for bucket breakdown
# exchcd = 1 and positive beme and positive me and shrcd in (10,11) and at least 2 years in comp
nyse=ccm_jun[(ccm_jun['exchcd']==1) & (ccm_jun['beme']>0) & (ccm_jun['me']>0) & \
             (ccm_jun['count']>=1) & ((ccm_jun['shrcd']==10) | (ccm_jun['shrcd']==11))]






if(factors3 == True):

    # size breakdown
    nyse_sz=nyse.groupby(['jdate'])['me'].median().to_frame().reset_index().rename(columns={'me':'sizemedn'})

    # beme breakdown
    nyse_bm=nyse.groupby(['jdate'])['beme'].describe(percentiles=[0.3, 0.7]).reset_index()
    nyse_bm=nyse_bm[['jdate','30%','70%']].rename(columns={'30%':'bm30', '70%':'bm70'})

    nyse_breaks = pd.merge(nyse_sz, nyse_bm, how='inner', on=['jdate'])

    # join back size and beme breakdown
    ccm1_jun = pd.merge(ccm_jun, nyse_breaks, how='left', on=['jdate'])


    # function to assign sz and bm bucket
    def sz_bucket(row):
        if row['me']==np.nan:
            value=''
        elif row['me']<=row['sizemedn']:
            value='S'
        else:
            value='B'
        return value

    def bm_bucket(row):
        if 0<=row['beme']<=row['bm30']:
            value = 'L'
        elif row['beme']<=row['bm70']:
            value='M'
        elif row['beme']>row['bm70']:
            value='H'
        else:
            value=''
        return value


    # assign size portfolio
    ccm1_jun['szport']=np.where((ccm1_jun['beme']>0)&(ccm1_jun['me']>0)&(ccm1_jun['count']>=1), ccm1_jun.apply(sz_bucket, axis=1), '')

    # assign book-to-market portfolio
    ccm1_jun['bmport']=np.where((ccm1_jun['beme']>0)&(ccm1_jun['me']>0)&(ccm1_jun['count']>=1), ccm1_jun.apply(bm_bucket, axis=1), '')

    # create positivebmeme and nonmissport variable
    ccm1_jun['posbm']=np.where((ccm1_jun['beme']>0)&(ccm1_jun['me']>0)&(ccm1_jun['count']>=1), 1, 0)
    ccm1_jun['nonmissport']=np.where((ccm1_jun['bmport']!=''), 1, 0)


    # store portfolio assignment as of June
    june=ccm1_jun[['permno','date', 'jdate', 'bmport','szport','posbm','nonmissport']]
    june['ffyear']=june['jdate'].dt.year

    # merge back with monthly records
    crsp3 = crsp3[['date','permno','shrcd','exchcd','retadj','retxadj','me','wt','cumretx','ffyear','jdate']]
    ccm3=pd.merge(crsp3, 
            june[['permno','ffyear','szport','bmport','posbm','nonmissport']], how='left', on=['permno','ffyear'])

    # keeping only records that meet the criteria
    ccm4=ccm3[(ccm3['wt']>0)& (ccm3['posbm']==1) & (ccm3['nonmissport']==1) & 
            ((ccm3['shrcd']==10) | (ccm3['shrcd']==11))]


    ############################
    # Form Fama French Factors #
    ############################

    # function to calculate value weighted return
    def wavg(group, avg_name, weight_name):
        d = group[avg_name]
        w = group[weight_name]
        try:
            return (d * w).sum() / w.sum()
        except ZeroDivisionError:
            return np.nan
        
        
    # value-weigthed return
    vwret=ccm4.groupby(['jdate','szport','bmport']).apply(wavg, 'retadj','wt').to_frame().reset_index().rename(columns={0: 'vwret'})
    vwret['sbport']=vwret['szport']+vwret['bmport']

    # firm count
    vwret_n=ccm4.groupby(['jdate','szport','bmport'])['retadj'].count().reset_index().rename(columns={'retadj':'n_firms'})
    vwret_n['sbport']=vwret_n['szport']+vwret_n['bmport']

    # tranpose
    ff_factors=vwret.pivot(index='jdate', columns='sbport', values='vwret').reset_index()
    ff_nfirms=vwret_n.pivot(index='jdate', columns='sbport', values='n_firms').reset_index()


    # create SMB and HML factors
    ff_factors['H']=(ff_factors['BH']+ff_factors['SH'])/2
    ff_factors['L']=(ff_factors['BL']+ff_factors['SL'])/2
    ff_factors['HML'] = ff_factors['H']-ff_factors['L']

    ff_factors['B']=(ff_factors['BL']+ff_factors['BM']+ff_factors['BH'])/3
    ff_factors['S']=(ff_factors['SL']+ff_factors['SM']+ff_factors['SH'])/3
    ff_factors['SMB'] = ff_factors['S']-ff_factors['B']
    ff_factors=ff_factors.rename(columns={'jdate':'date'})
    ff_factors = ff_factors.reset_index(drop = True)
    
    if is_market_factor == True:
        
        ff_factors=pd.merge(ff_factors,market_factor, how = "inner", on = ['date'])
    
    
    # n firm count
    ff_nfirms['H']=ff_nfirms['SH']+ff_nfirms['BH']
    ff_nfirms['L']=ff_nfirms['SL']+ff_nfirms['BL']
    ff_nfirms['HML']=ff_nfirms['H']+ff_nfirms['L']

    ff_nfirms['B']=ff_nfirms['BL']+ff_nfirms['BM']+ff_nfirms['BH']
    ff_nfirms['S']=ff_nfirms['SL']+ff_nfirms['SM']+ff_nfirms['SH']
    ff_nfirms['SMB']=ff_nfirms['B']+ff_nfirms['S']
    ff_nfirms['TOTAL']=ff_nfirms['SMB']
    ff_nfirms=ff_nfirms.rename(columns={'jdate':'date'})


    ###################
    # Compare With FF #
    ###################

    _ff = conn.get_table(library='ff', table='factors_monthly')
    _ff=_ff[['date','smb','hml']]
    _ff['date']=_ff['date']+MonthEnd(0)
    _ff['date']= pd.to_datetime(_ff['date'], format="%Y-%m-%d")

    _ffcomp = pd.merge(_ff, ff_factors[['date','SMB','HML']], how='inner', on=['date'])
    _ffcomp70=_ffcomp[_ffcomp['date']>='01/01/1970']
    
    smb_valid = _ffcomp70[['smb', 'SMB']].dropna()
    hml_valid = _ffcomp70[['hml', 'HML']].dropna()
    # Convert to float to avoid float/Decimal issues
    smb_valid = smb_valid.astype(float)
    hml_valid = hml_valid.astype(float)
    print(stats.pearsonr(smb_valid['smb'], smb_valid['SMB']))
    print(stats.pearsonr(hml_valid['hml'], hml_valid['HML']))


    _ffcomp.head(2)
    _ffcomp.tail(2)

    _ffcomp.date = pd.to_datetime(_ffcomp.date)
    _ffcomp.set_index('date', inplace=True)
    _ffcomp.head(2)

    plt.figure(figsize=(16,12))
    plt.suptitle('Comparison of Results', fontsize=20)

    ax1 = plt.subplot(211)
    ax1.set_title('SMB', fontsize=15)
    ax1.set_xlim([dt.datetime(1962,6,1), dt.datetime(2017,12,31)])
    ax1.plot(_ffcomp['smb'], 'r--', _ffcomp['SMB'], 'b-')
    ax1.legend(('smb_from_ff','SMB_mine'), loc='upper right', shadow=True)

    ax2 = plt.subplot(212)
    ax2.set_title('HML', fontsize=15)
    ax2.plot(_ffcomp['hml'], 'r--', _ffcomp['HML'], 'b-')
    ax2.set_xlim([dt.datetime(1962,6,1), dt.datetime(2017,12,31)])
    ax2.legend(('hml_from_ff','HML_mine'), loc='upper right', shadow=True)

    plt.subplots_adjust(top=0.92, hspace=0.2)

    plt.show()

