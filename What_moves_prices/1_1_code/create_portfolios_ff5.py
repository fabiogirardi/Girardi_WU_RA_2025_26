##########################################
# Create cross-sectional Portfolios      #
# Fabio Girardi                          #
# Date: June 2025                        #
# Updated: July 2025                     #
##########################################

import pandas as pd
import numpy as np
from pandas.tseries.offsets import *
import wrds
import random
import matplotlib.pyplot as plt



#DIR = "C:\\Users\\R59\\Documents\\GitHub\\project_dividends\\"
DIR = "C:\\Users\\fgirardi\\Downloads\\"

# select relevant variables
COUNTRY = "usa"
IDVARS = ['id', 'permno', 'size_grp', 'crsp_shrcd', 'crsp_exchcd', 'ff49','prc', 'eom']
CHARS = ['ret_12_1', 'be_me', 'market_equity', 'shares', 'div1m_me', 'div3m_me','div12m_me','ope_be','rd_at','capx_at']
SORTFREQ = 'Y'
N_PORT = 10 # choose 10,5,3

# sorting variable
SORTVAR = ['market_equity', 'be_me', 'ret_12_1','ope_be','rd_at','capx_at']
EVALVAR = ['ret_lead1m', 'retx_lead1m','eps_adj_lead1m', 'dps_adj_lead1m'] #

# date
start_date = 19670101
end_date = 20220101

out_dict = {}

# read characteristics data
#chars = pd.read_csv("D:\\databases\\Characteristics\\" + COUNTRY +".csv", engine='pyarrow', usecols=IDVARS+CHARS)
#column_names = pd.read_csv("D:\\databases\\Characteristics\\usa.csv", nrows=0).columns.tolist()
chars = pd.read_csv(DIR + "usa.csv", engine='pyarrow', usecols=IDVARS+CHARS)

chars.isna().sum()/ chars.shape[0] 

chars = chars[IDVARS+CHARS].rename(columns={'eom':'month_id'})
chars = chars[(chars['month_id']>=start_date) & (chars['month_id']<=end_date) & (chars['permno'].notna())]
chars = chars.drop(columns=['prc'], errors='ignore')
chars['month_id'] = chars['month_id'].astype('str').str[:-2].astype('int')
chars = chars.drop(columns=['prc'], errors='ignore')
# load CRSP

conn = wrds.Connection(wrds_username='girardifab')
crsp_m = conn.raw_sql(f"""
                      select a.permno, a.permco, a.date, b.shrcd, b.exchcd,
                      a.ret, a.retx, a.shrout, a.prc
                      from crsp.msf as a
                      left join crsp.msenames as b
                      on a.permno=b.permno
                      and b.namedt<=a.date
                      and a.date<=b.nameendt
                      where a.date between '{start_date}' and '{end_date}'
                      and b.exchcd between 1 and 3
                      --- and a.prc > 1
                      and b.shrcd between 10 and 11
                      """, date_cols=['date'])


#crsp_m = pd.read_csv("D:\\databases\\wrds\\crsp\\cross_section\\crsp_m.csv", engine='pyarrow')

# change variable format to int
crsp_m['date'] = pd.to_datetime(crsp_m['date'])    
crsp_m['date'] = pd.to_datetime(crsp_m['date']) + pd.offsets.MonthEnd(0)
crsp_m[['permco','permno','shrcd','exchcd']]=crsp_m[['permco','permno','shrcd','exchcd']].astype(int)
crsp_m.insert(2, 'month_id', crsp_m['date'].dt.year*100 + crsp_m['date'].dt.month)
crsp_m.insert(3, 'quarter_id', crsp_m['date'].dt.year*100 + crsp_m['date'].dt.quarter)

# merge characteristics to crsp ret, retx series
df = pd.merge(crsp_m,chars, on=['permno', 'month_id'])
del chars

df['ret_q'] = df.groupby(['permno','quarter_id'])["ret"].transform(lambda x: x + 1)
df['ret_q'] = df.groupby(['permno','quarter_id'])["ret_q"].transform('prod').transform(lambda x: x - 1)

df['retx_q'] = df.groupby(['permno','quarter_id'])["retx"].transform(lambda x: x + 1)
df['retx_q'] = df.groupby(['permno','quarter_id'])["retx_q"].transform('prod').transform(lambda x: x - 1)





#crsp_q = crsp_m.copy()

#crsp_q = crsp_q[crsp_q['date'].dt.is_quarter_end]

#crsp_q = crsp_q.sort_values(['permno','quarter_id'])
#crsp_q['ret_q_lead1q'] = crsp_q.groupby(['permno'])['ret_q'].shift(-1)
#crsp_q['retx_q_lead1q'] = crsp_q.groupby(['permno'])['retx_q'].shift(-1)

######################################
### Import EPS and DPS data
######################################


cols_to_eps_dps = [
    'eps_basic_split_adj',
    'dps_split_adj',
    'eps_basic',
    'dps',
    'eps_diluted'
]

eps_dps_data = pd.read_csv(
    r'C:\Users\R59\Documents\GitHub\Girardi_WU_RA_2025_26\What_moves_prices\1_2_data\eps_dps_data.csv',
    engine='pyarrow',
    usecols=['permno', 'date'] + cols_to_eps_dps
)

eps_dps_data['date'] = pd.to_datetime(eps_dps_data['date'])
eps_dps_data['date'] = eps_dps_data['date'] + pd.offsets.QuarterEnd(0)
eps_dps_data.insert(2, 'quarter_id', eps_dps_data['date'].dt.year*100 + eps_dps_data['date'].dt.quarter)
eps_dps_data.drop(columns=['date'], inplace=True)


# Rename EPS and DPS columns
eps_dps_data = eps_dps_data.rename(columns={
    'eps_basic_split_adj': 'eps_adj',
    'dps_split_adj': 'dps_adj', 
    'eps_basic': 'eps',
    'dps': 'dps',
    'eps_diluted': 'eps_diluted'
})

# Update the column list to reflect new names
cols_to_eps_dps = ['eps_adj', 'dps_adj', 'eps', 'dps', 'eps_diluted']

### Merge EPS and DPS data with CRSP
 
df = pd.merge(df, eps_dps_data, on=['permno', 'quarter_id'], how='left').reset_index(drop=True)
df = df.sort_values(['permno', 'quarter_id']).drop_duplicates(subset=['permno', 'quarter_id'], keep='last')
# Keep only end of quarter values
df = df[df['date'].dt.is_quarter_end]


# chars need to be lagged w.r.t. returns
df['ret_lead1m'] = df.sort_values(['permno','month_id']).groupby('permno')['ret_q'].shift(-1)
df['retx_lead1m'] = df.sort_values(['permno','month_id']).groupby('permno')['retx_q'].shift(-1)
df['eps_adj_lead1m'] = df.sort_values(['permno','month_id']).groupby('permno')['eps_adj'].shift(-1)
df['dps_adj_lead1m'] = df.sort_values(['permno','month_id']).groupby('permno')['dps_adj'].shift(-1)

#######################################
#######################################
### Check for missing values
#######################################

# Calculate percentage of missing values in EVALVAR columns (should be 0 after dropping)
missing_pct = df[EVALVAR].isna().mean() 
print("Percentage of missing values in EVALVAR columns:")
print(missing_pct)

# Drop rows with any missing values in EVALVAR columns
df = df.dropna(subset=EVALVAR, how='any')

# Calculate percentage of missing values in EVALVAR columns (should be 0 after dropping)
missing_pct = df[EVALVAR].isna().mean() 
print("Percentage of missing values in EVALVAR columns:")
print(missing_pct)


# Winsorize evalvar at 1st and 99th percentiles within each month_id


def winsorize_series(x, evalvar):
    lower = x[evalvar].quantile(0.001)
    upper = x[evalvar].quantile(0.999)
    return x[evalvar].clip(lower, upper)

for evalvar in EVALVAR:
    df[evalvar + '_win'] = df.groupby('month_id')[evalvar].transform(lambda x: winsorize_series(df.loc[x.index], evalvar))
        

# Round all numerical columns to 4 decimal places
numeric_cols = df.select_dtypes(include=[np.number]).columns
df[numeric_cols] = df[numeric_cols].round(4)
                
sortvar = 'market_equity'  # Default sorting variable

old_sort = True # Set to True for old sorting method, False for new sorting method

for sortvar in SORTVAR:
    out = []
    for evalvar in EVALVAR:
        # construct NYSE breakpoints (separate dataframe, i.e., stocks with crsp_exchcd = 1)
        
        #* if monthly sorting, replace quarter_id by month_id below
        
        nyse = (df.sort_values(['permno', 'month_id'])
                .drop_duplicates(subset=['permno', 'quarter_id'], keep='last')
                )      #* before there was shrcd == 1, but this is not needed anymore, as we do not only use NYSE stocks
          
        if N_PORT==10:
            
            
            if old_sort:

                breakp = nyse.groupby(['quarter_id'])[sortvar].describe(percentiles=[i/10 for i in range(1,10)]).reset_index()
                breakp_merge = breakp[['quarter_id']+[str(i*10)+'%' for i in range(1,10)]]
            
                # merge back the breakpoints to original dataframe
                df_break = pd.merge(df, breakp_merge, how='left', on=['quarter_id'])
            
                # create column that includes the labels
                m1 = (df_break[sortvar] < df_break['10%']) #& (df_break[evalvar].notna())
                m2 = ((df_break['10%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['20%']))  #& (df_break[evalvar].notna())
                m3 = ((df_break['20%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['30%']))  #& (df_break[evalvar].notna())
                m4 = ((df_break['30%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['40%']))  #& (df_break[evalvar].notna())
                m5 = ((df_break['40%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['50%']))  #& (df_break[evalvar].notna())
                m6 = ((df_break['50%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['60%']))  #& (df_break[evalvar].notna())
                m7 = ((df_break['60%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['70%']))  #& (df_break[evalvar].notna())
                m8 = ((df_break['70%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['80%']))  #& (df_break[evalvar].notna())
                m9 = ((df_break['80%'] <= df_break[sortvar])  & (df_break[sortvar] < df_break['90%']))  #& (df_break[evalvar].notna())
                m10 = (df_break[sortvar] >= df_break['90%'])

                vals = [i for i in range(1,11)]
                default = np.nan

                df_break['portf_decile'] = np.select([m1, m2, m3, m4, m5, m6,m7,m8,m9,m10], vals, default=default)
            
            else:
                #* if new sorting, use the following code
                df_break = df.copy()
                df_break['portf_decile'] = (df.groupby('month_id')[sortvar].transform(lambda x: pd.qcut(x.rank(method='first'), N_PORT, labels=False) + 1))

                
            
            df_break['portf_mcap'] = df_break.groupby(['month_id','portf_decile'])['market_equity'].transform('sum')
            df_break['weight'] = df_break['market_equity'] / df_break['portf_mcap']
            df_break.groupby(['month_id', 'portf_decile'], as_index=False)['permno'].count()    


        outTemp = df_break.groupby(['month_id', 'portf_decile'], as_index=False, group_keys=False).apply(
            lambda x: np.sum(x['weight'] * x[evalvar + '_win']))
        outTemp['count'] = df_break.groupby(['month_id', 'portf_decile'], as_index=False)['permno'].count()['permno']
        outTemp.columns = ['month_id', 'portf_decile', 'vw_average', 'count']
        outTemp['grp_count'] = outTemp.groupby(['month_id'])['count'].transform('min')
        outTemp = outTemp.pivot(index=['month_id'], columns=['portf_decile'], values=['vw_average']).reset_index()
        outTemp.columns = ['month_id', 'D_1', 'D_2', 'D_3', 'D_4', 'D_5', 'D_6', 'D_7', 'D_8', 'D_9', 'D_10']
        out.append(outTemp)


    out_wide = pd.merge(out[0], out[1], on=['month_id'])
    out_wide['month'] = out_wide['month_id'].astype('str').str[-2:].astype('int')

    if ['ret_lead1m', 'retx_lead1m'] == EVALVAR[:2]:  # Check if first two entries in EVALVAR match
        for i in range(1,11):
            out_wide['V'+str(i)] = out_wide['D_'+str(i)+'_y'] +1     # at time t is P_t+1 / P_t
            out_wide['V'+str(i)] = out_wide['V'+str(i)].cumprod().shift(1) # at time t is V_t = P_t / P_0
            out_wide.loc[0,'V'+str(i)] = 1 # V_0 = 1
            out_wide['y'+str(i)] = out_wide['D_'+str(i)+'_x'] - out_wide['D_'+str(i)+'_y'] # at time t is y_t+1 = D_t+1 / P_t
            out_wide['D_'+str(i)] = (out_wide['y'+str(i)] * out_wide['V'+str(i)]).shift(1)
            out_wide['DP_'+str(i)] = (out_wide['y'+str(i)]*(out_wide['D_'+str(i)+'_y'] +1)**-1).shift(1)   
            out_wide['D_'+str(i)+'_x'] = out_wide['D_'+str(i)+'_x'].shift(1) # at time t is P_t+D_t / P_t-1
            out_wide['D_'+str(i)+'_y'] = out_wide['D_'+str(i)+'_y'].shift(1) # at time t is P_t / P_t-1           

        out_wide.insert(1, 'quarter_id', 
                        pd.to_datetime(out_wide['month_id'], format="%Y%m").dt.year * 100 
                        + pd.to_datetime(out_wide['month_id'], format="%Y%m").dt.quarter)

        out_quarter = (out_wide[['quarter_id']
                                + ['D_'+str(i)+'_x' for i in range(1,11)]
                                + ['D_'+str(i) for i in range(1,11)]
                                + ['y'+str(i) for i in range(1,11)]
                                + ['DP_'+str(i) for i in range(1,11)]]
                                .groupby('quarter_id')
                                .sum()
                                .rolling(4)
                                .sum()
                        )
                        
        #out_quarter.columns = out_quarter.columns.str.replace('y', 'DP_')
        for i in range(1,11):
            out_quarter.rename(columns={'D_'+str(i)+'_x': 'ret_'+str(i)}, inplace=True)
            out_quarter.rename(columns={'D_'+str(i)+'_y': 'retx_'+str(i)}, inplace=True)
        
        for i in range(1,11):
            out_quarter['D_gr'+str(i)] = np.log(out_quarter['D_'+str(i)]) - np.log(out_quarter['D_'+str(i)].shift(1))
        for i in range(1,11):
            out_quarter['D_gr_yearly'+str(i)] = np.log(out_quarter['D_'+str(i)]) - np.log(out_quarter['D_'+str(i)].shift(4))
                    
        out_quarter[np.isinf(out_quarter)] = np.nan   

    if ['eps_adj_lead1m', 'dps_adj_lead1m'] == EVALVAR[2:4]:

        out_wide_eps = pd.merge(out[2], out[3], on=['month_id'])
        out_wide_eps['D_'+str(i)+'_x'] = out_wide['D_'+str(i)+'_x'].shift(1) # at time t is P_t+D_t / P_t-1
        out_wide_eps['D_'+str(i)+'_y'] = out_wide['D_'+str(i)+'_y'].shift(1) # at time t is P_t / P_t-1     
        
        out_wide_eps.insert(1, 'quarter_id',
                        pd.to_datetime(out_wide_eps['month_id'], format="%Y%m").dt.year * 100
                        + pd.to_datetime(out_wide_eps['month_id'], format="%Y%m").dt.quarter)

        for i in range(1,11):
            out_wide_eps.rename(columns={'D_'+str(i)+'_x': 'Eps_'+str(i)}, inplace=True)
        for i in range(1,11):
            out_wide_eps.rename(columns={'D_'+str(i)+'_y': 'Dps_'+str(i)}, inplace=True)

        out_quarter_eps = (out_wide_eps[['quarter_id']
                                + ['Eps_'+str(i) for i in range(1,11)]
                                + ['Dps_'+str(i) for i in range(1,11)]]
                                .groupby('quarter_id')
                                .sum()
                                .rolling(4)
                                .sum()
                        )
        for i in range(1,11):
            out_quarter_eps['Eps_gr'+str(i)] = out_quarter_eps['Eps_'+str(i)] / out_quarter_eps['Eps_'+str(i)].shift(1) - 1
        for i in range(1,11):
            out_quarter_eps['Dps_gr'+str(i)] = out_quarter_eps['Dps_'+str(i)] / out_quarter_eps['Dps_'+str(i)].shift(1) - 1

        for i in range(1,11):
            out_quarter_eps['Eps_gr_yr'+str(i)] = out_quarter_eps['Dps_'+str(i)] / out_quarter_eps['Dps_'+str(i)].shift(4) - 1
        for i in range(1,11):
            out_quarter_eps['Dps_gr_yr'+str(i)] = out_quarter_eps['Dps_'+str(i)] / out_quarter_eps['Dps_'+str(i)].shift(4) - 1
            
        out_quarter_eps[np.isinf(out_quarter_eps)] = np.nan

    out_quarter = pd.merge(out_quarter, out_quarter_eps, on='quarter_id', how='outer')
    out_dict.update({sortvar:out_quarter})


out_dict['market_equity']['D_1'].corr(out_dict['market_equity']['Dps_1'])

out_dict['market_equity']['D_10'].corr(out_dict['market_equity']['Dps_10'])

out_dict['market_equity']['Dps_10'].max()


for i in range(1, 11):
    corr = out_dict['market_equity'][f'Dps_gr{i}'].corr(out_dict['market_equity'][f'D_gr{i}'])
    print(f"Correlation between Dps_gr{i} and D_gr{i}: {corr:.4f}")
    corr = out_dict['market_equity'][f'Dps_gr_yr{i}'].corr(out_dict['market_equity'][f'D_gr_yearly{i}'])
    print(f"Correlation between Dps_gr_yr{i} and D_gr_yearly{i}: {corr:.4f}")





### DESCRIPTIVES FOR DIVIDEND GROWTH (only D_gr1 ... D_gr10, not yearly)
out_dividends_gr = pd.DataFrame(
    index=[f'D_gr{i}' for i in range(1, 11)],
    columns=['size', 'size_std', 'bm', 'bm_std', 'mom', 'mom_std', 'prof', 'prof_std', 'inv', 'inv_std']
)

# Only select columns that match exactly 'D_gr1' to 'D_gr10'
dgr_cols = [f'D_gr{i}' for i in range(1, 11)]

out_dividends_gr['size'] = out_dict['market_equity'].iloc[:-1][dgr_cols].mean().values
out_dividends_gr['size_std'] = out_dict['market_equity'].iloc[:-1][dgr_cols].std().values

out_dividends_gr['bm'] = out_dict['be_me'].iloc[:-1][dgr_cols].mean().values
out_dividends_gr['bm_std'] = out_dict['be_me'].iloc[:-1][dgr_cols].std().values

out_dividends_gr['mom'] = out_dict['ret_12_1'].iloc[:-1][dgr_cols].mean().values
out_dividends_gr['mom_std'] = out_dict['ret_12_1'].iloc[:-1][dgr_cols].std().values

out_dividends_gr['prof'] = out_dict['ope_be'].iloc[:-1][dgr_cols].mean().values
out_dividends_gr['prof_std'] = out_dict['ope_be'].iloc[:-1][dgr_cols].std().values

out_dividends_gr['inv'] = out_dict['capx_at'].iloc[:-1][dgr_cols].mean().values
out_dividends_gr['inv_std'] = out_dict['capx_at'].iloc[:-1][dgr_cols].std().values

out_dividends_gr


out_earnings_gr = pd.DataFrame(
    index=[f'E_gr{i}' for i in range(1, 11)],
    columns=['size', 'size_std', 'bm', 'bm_std', 'mom', 'mom_std', 'prof', 'prof_std', 'inv', 'inv_std']
)

egr_cols = [f'Eps_gr{i}' for i in range(1, 11)]

out_earnings_gr['size'] = out_dict['market_equity'].iloc[:-1][egr_cols].mean().values
out_earnings_gr['size_std'] = out_dict['market_equity'].iloc[:-1][egr_cols].std().values

out_earnings_gr['bm'] = out_dict['be_me'].iloc[:-1][egr_cols].mean().values
out_earnings_gr['bm_std'] = out_dict['be_me'].iloc[:-1][egr_cols].std().values

out_earnings_gr['mom'] = out_dict['ret_12_1'].iloc[:-1][egr_cols].mean().values
out_earnings_gr['mom_std'] = out_dict['ret_12_1'].iloc[:-1][egr_cols].std().values

out_earnings_gr['prof'] = out_dict['ope_be'].iloc[:-1][egr_cols].mean().values
out_earnings_gr['prof_std'] = out_dict['ope_be'].iloc[:-1][egr_cols].std().values

out_earnings_gr['inv'] = out_dict['capx_at'].iloc[:-1][egr_cols].mean().values
out_earnings_gr['inv_std'] = out_dict['capx_at'].iloc[:-1][egr_cols].std().values

out_earnings_gr



### DESCRIPTIVES FOR RETURNS
out_ret = pd.DataFrame(
    index=[f'ret{i}' for i in range(1, 11)],
    columns=['size', 'size_std', 'bm', 'bm_std', 'mom', 'mom_std', 'prof', 'prof_std', 'inv', 'inv_std']
)

out_ret['size'] = out_dict['market_equity'].iloc[:-1].filter(regex='ret').mean().values
out_ret['size_std'] = out_dict['market_equity'].iloc[:-1].filter(regex='ret').std().values

out_ret['bm'] = out_dict['be_me'].iloc[:-1].filter(regex='ret').mean().values
out_ret['bm_std'] = out_dict['be_me'].iloc[:-1].filter(regex='ret').std().values

out_ret['mom'] = out_dict['ret_12_1'].iloc[:-1].filter(regex='ret').mean().values
out_ret['mom_std'] = out_dict['ret_12_1'].iloc[:-1].filter(regex='ret').std().values
#out_ret.to_csv(DIR + "1_2_output\\descriptives_ret.csv")

out_ret['prof'] = out_dict['ope_be'].iloc[:-1].filter(regex='ret').mean().values
out_ret['prof_std'] = out_dict['ope_be'].iloc[:-1].filter(regex='ret').std().values

out_ret['inv'] = out_dict['capx_at'].iloc[:-1].filter(regex='ret').mean().values
out_ret['inv_std'] = out_dict['capx_at'].iloc[:-1].filter(regex='ret').std().values


out_ret









# Plot D_gr1 and D_gr10 for mom, size, bm
import matplotlib.pyplot as plt

# Define the variables and their labels
sortvars = ['market_equity', 'be_me','ret_12_1'] #,'ope_be','capx_at']
labels = [ 'Size', 'Book-to-Market','Momentum'] #,'Operating Profitability','Capital Expenditures']

plt.figure(figsize=(15, 8))

for idx, (sortvar, label) in enumerate(zip(sortvars, labels), 1):
    plt.subplot(1, 3, idx)
    # Dropna to ensure aligned index
    d_gr1 = out_dict[sortvar]['D_gr1'].dropna()
    d_gr10 = out_dict[sortvar]['D_gr10'].dropna()
    common_idx = d_gr1.index.intersection(d_gr10.index)
    plt.plot(common_idx, d_gr1.loc[common_idx], label='D_gr1')
    plt.plot(common_idx, d_gr10.loc[common_idx], label='D_gr10')
    # Calculate and display correlation
    corr = d_gr1.loc[common_idx].corr(d_gr10.loc[common_idx])
    plt.title(f'D_gr1 vs D_gr10 ({label})\nCorr={corr:.2f}')
    plt.xlabel('quarter_id')
    plt.ylabel('Dividend Growth')
    plt.legend()

plt.tight_layout()
plt.show()





plt.figure(figsize=(15, 8))

for idx, (sortvar, label) in enumerate(zip(sortvars, labels), 1):
    plt.subplot(1, 3, idx)
    # Dropna to ensure aligned index
    d_gr1 = out_dict[sortvar]['D_gr_yearly1'].dropna()
    d_gr10 = out_dict[sortvar]['D_gr_yearly10'].dropna()
    common_idx = d_gr1.index.intersection(d_gr10.index)
    plt.plot(common_idx, d_gr1.loc[common_idx], label='D_gr1')
    plt.plot(common_idx, d_gr10.loc[common_idx], label='D_gr10')
    # Calculate and display correlation
    corr = d_gr1.loc[common_idx].corr(d_gr10.loc[common_idx])
    plt.title(f'D_gr1 vs D_gr10 ({label})\nCorr={corr:.2f}')
    plt.xlabel('quarter_id')
    plt.ylabel('Dividend Growth')
    plt.legend()

plt.tight_layout()
plt.show()




plt.figure(figsize=(15, 8))

for idx, (sortvar, label) in enumerate(zip(sortvars, labels), 1):
    plt.subplot(1, 3, idx)
    # Dropna to ensure aligned index
    d_gr1 = out_dict[sortvar]['ret_1'].dropna()
    d_gr10 = out_dict[sortvar]['ret_10'].dropna()
    common_idx = d_gr1.index.intersection(d_gr10.index)
    plt.plot(common_idx, d_gr1.loc[common_idx], label='D_gr1')
    plt.plot(common_idx, d_gr10.loc[common_idx], label='D_gr10')
    # Calculate and display correlation
    corr = d_gr1.loc[common_idx].corr(d_gr10.loc[common_idx])
    plt.title(f'ret_1 vs ret_10 ({label})\nCorr={corr:.2f}')
    plt.xlabel('quarter_id')
    plt.ylabel('Dividend Growth')
    plt.legend()

plt.tight_layout()
plt.show()








def winsorize_series(x, evalvar):
    lower = x[evalvar].quantile(0.05)
    upper = x[evalvar].quantile(0.95)
    return x[evalvar].clip(lower, upper)

for evalvar in EVALVAR:
    df[evalvar + '_win'] = df.groupby('month_id')[evalvar].transform(lambda x: winsorize_series(df.loc[x.index], evalvar))

df['dps_adj_win'].max()  # Check if the adjusted dividends are correct
(df['retx_q'] + df['dps_adj'] / df['prc'] - df['ret_q']).mean()  # Check if the returns are equal to retx + dps - ret

# Search for Apple (AAPL) in the dataframe
apple_data = df[df['permno'] == 14593]  # Apple's PERMNO is 14593
apple_data = apple_data[['quarter_id', 'ret', 'retx', 'dps', 'eps',  'dps_adj', 'eps_adj']].drop_duplicates(subset=[ 'quarter_id'], keep='last')

