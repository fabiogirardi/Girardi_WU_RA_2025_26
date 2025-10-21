######################################################################################################
# LOAD PACKAGES & CLEAR WORKSPACE
######################################################################################################



library('chattr')
library('tidyverse')
library('dplyr')
library('zoo')
library('glmnet')
library('sparsegl')
library('reshape2')
library('RColorBrewer')
library('matrixStats')
library('psych')
library('oem')
library('vars')
library('mvtnorm')
library('xtable')
library("dint") 
library('ggpubr')
library('BigVAR')
library('rlist')
library('lubridate')
library("roll")
library('gdata')
library('ggcorrplot')
library('sparsepca')
library('readxl')
library("matrixcalc")
library("DescTools") # 


rm(list=ls())
gc(reset = T)
options(max.print = 2000)

#####################
### Recession_Periods_Necessary to plot
#####################

recessions.df = read.table(textConnection("Peak, Trough
                                          1857-06-01, 1858-12-01
                                          1860-10-01, 1861-06-01
                                          1865-04-01, 1867-12-01
                                          1869-06-01, 1870-12-01
                                          1873-10-01, 1879-03-01
                                          1882-03-01, 1885-05-01
                                          1887-03-01, 1888-04-01
                                          1890-07-01, 1891-05-01
                                          1893-01-01, 1894-06-01
                                          1895-12-01, 1897-06-01
                                          1899-06-01, 1900-12-01
                                          1902-09-01, 1904-08-01
                                          1907-05-01, 1908-06-01
                                          1910-01-01, 1912-01-01
                                          1913-01-01, 1914-12-01
                                          1918-08-01, 1919-03-01
                                          1920-01-01, 1921-07-01
                                          1923-05-01, 1924-07-01
                                          1926-10-01, 1927-11-01
                                          1929-08-01, 1933-03-01
                                          1937-05-01, 1938-06-01
                                          1945-02-01, 1945-10-01
                                          1948-11-01, 1949-10-01
                                          1953-07-01, 1954-05-01
                                          1957-08-01, 1958-04-01
                                          1960-04-01, 1961-02-01
                                          1969-12-01, 1970-11-01
                                          1973-11-01, 1975-03-01
                                          1980-01-01, 1980-07-01
                                          1981-07-01, 1982-11-01
                                          1990-07-01, 1991-03-01
                                          2001-03-01, 2001-11-01
                                          2007-12-01, 2009-09-01"), 
                           sep=',', colClasses=c('Date', 'Date'), header=TRUE)   ### 2020-03-01, 2020-07-01 



recessions = c("1953-07-01","1954-08-01","1953-09-01","1953-10-01","1953-11-01","1953-12-01","1954-01-01","1954-02-01","1954-03-01","1954-04-01", "1954-05-01",
               
               "1957-08-01","1957-09-01","1957-10-01","1957-11-01","1957-12-01","1958-01-01","1958-02-01","1958-03-01", "1958-04-01",
               
               "1960-04-01","1960-05-01","1960-06-01","1960-07-01","1960-08-01","1960-09-01","1960-10-01","1960-11-01","1960-12-01","1960-01-01", "1961-02-01",
               
               "1969-12-01", "1970-01-01","1970-02-01","1970-03-01","1970-04-01","1970-05-01","1970-06-01","1970-07-01","1970-08-01","1970-09-01","1970-10-01","1970-11-01",
               
               "1973-11-01","1973-12-01","1974-01-01","1974-02-01","1974-03-01","1974-04-01","1974-05-01","1974-06-01","1974-07-01","1974-08-01","1974-09-01","1974-10-01","1974-11-01","1974-12-01","1975-01-01","1975-02-01","1975-03-01",
               
               "1980-01-01","1980-02-01","1980-03-01","1980-04-01","1980-05-01","1980-06-01", "1980-07-01",
               
               "1981-07-01","1981-08-01","1981-09-01","1981-10-01","1981-11-01","1981-12-01","1982-01-01","1982-02-01","1982-03-01","1982-04-01","1982-05-01","1982-06-01","1982-07-01","1982-08-01","1982-09-01","1982-10-01","1982-11-01",
               
               "1990-07-01","1990-08-01","1990-09-01","1990-10-01","1990-11-01","1990-12-01","1991-01-01","1991-02-01", "1991-03-01",
               
               "2001-03-01","2001-04-01","2001-05-01","2001-06-01","2001-07-01","2001-08-01","2001-09-01","2001-10-01", "2001-11-01",
               
               "2007-12-01","2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01", "2009-06-01"
               )


######################################################################################################
# PARAMETER INPUT
######################################################################################################

# Who is running the script?

who = "R59"

##### Choose basic parameters

startdate = 198012
enddate = 202312
freq_raw_data = "m"
freq_var_data = "y" # frequency of the data
market = "shiller"
forecast_horizon = "all"
factors = FALSE
rolling.pca=TRUE
pca = TRUE
n_pc = 2
splitratio = 1/2 # share of the training set
oos = T
annualized = FALSE
path_functions = paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\")
lags = 1
fit_intercept = FALSE

### Shrinkage

pen_search = "LOO" # LOO or Rolling
sep_lambdas = TRUE
alpha = 1 # 0 ridge, 1 lasso
sparse_pca = T
alpha_pca = 2.e-4  # now it is set exogenously
beta_pca = 0
t1 = 1/2
t2 = 1
optimal_lambda_2_is = 2e-04
### Model

model = "all_models" # either "all_models" or "full"
run_ols = FALSE

# set WD

old_version = FALSE

if(old_version == TRUE){
  
  setwd(paste0("D:\\GitHub\\Dynamics_Returns_and_Fundamentals\\data\\"))  # old
  
} else { 
  
  setwd(paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\data\\output_2024"))
  
}

getwd()



#################################
######### Set of valuation ratios
#################################


CHARS= c("capital_ratio", "equity_invcap", "debt_invcap", "totdebt_invcap",
         
        "at_turn", "inv_turn", "pay_turn", "rect_turn", "sale_equity", "sale_invcap", "sale_nwc",
        
        "invt_act", "rect_act", "fcf_ocf", "ocf_lct", "cash_debt", "cash_lt", "cfm", "short_debt", "profit_lct", "curr_debt",
        "debt_ebitda", "dltt_be", "int_debt", "int_totdebt", "lt_debt", "lt_ppent",
        
         "cash_conversion", "cash_ratio", "curr_ratio", "quick_ratio",
        
         "efftax","gprof", "aftret_eq", "aftret_equity", "aftret_invcapx", "gpm", "npm", "opmad", "opmbd", "pretret_earnat",
         "pretret_noa", "ptpm", "roa", "roce", "roe",
        
         "de_ratio", 	"debt_assets", "debt_at", "debt_capital", "intcov", "intcov_ratio",
        
         "dpr", "peg_trailing", 'bm', "evm", "pcf", "pe_exi", "pe_inc", "pe_op_basic", "pe_op_dil", "ps", "ptb",
        
         'ntis', 'tbl', 'tms', 'svar', 'dfy', 'siioutofsample',
        
         "accrual", "rd_sale", "adv_sale", "staff_sale")
           

CATEGORIES <- list(capitalization = c("capital_ratio", "equity_invcap", "debt_invcap", "totdebt_invcap"),
                       efficiency = c("at_turn", "inv_turn", "pay_turn", "rect_turn", "sale_equity", "sale_invcap", "sale_nwc"),
              financial_soundness = c("invt_act", "rect_act", "fcf_ocf", "ocf_lct", "cash_debt", "cash_lt", "cfm", 
                                      "short_debt", "profit_lct", "curr_debt", "debt_ebitda", "dltt_be", "int_debt",
                                      "int_totdebt", "lt_debt", "lt_ppent"),
                        liquidity = c("cash_conversion", "cash_ratio", "curr_ratio", "quick_ratio"),
                    profitability = c("efftax","gprof", "aftret_eq", "aftret_equity", "aftret_invcapx", "gpm", "npm",
                                      "opmad", "opmbd", "pretret_earnat", "pretret_noa", "ptpm", "roa", "roce", "roe"),
                         solvency = c("de_ratio", 	"debt_assets", "debt_at", "debt_capital", "intcov", "intcov_ratio"),
                        valuation = c("dpr", "peg_trailing", "bm", "evm", "pcf", "pe_exi", "pe_inc",
                                      "pe_op_basic", "pe_op_dil", "ps", "ptb"), 
                   equity_premium = c('ntis', 'tbl', 'tms', 'svar', 'dfy','siioutofsample'), #'dp', 'ep', 
                            other = c("accrual", "rd_sale", "adv_sale", "staff_sale"),
                             full = CHARS)

######################################################################################################
# DATA PREPARATION
######################################################################################################

# Import data row data

df <- read_csv("earnings_dividends_price_ratios_monthly_public.csv") %>% as.data.frame()

df <- df %>% rename_at(.vars = vars(ends_with("_weighted")), .funs = funs(sub("_weighted$", "", .)))

# Prepare data

source(file=paste0(path_functions,"data_preparation.R"), chdir = T)  ## create p/d, p/e, ed, and ret and they lead values

df.all.freq <- data_preparation(df, market= market, annualize = T, freq.raw.data = freq_raw_data) %>% filter(month_id >= startdate, month_id <= enddate ) %>% as.data.frame()

df.all.freq$quarter_id =  ymd(paste0(df.all.freq$month_id, "01")) + months(1) - days(1)


#df.all.freq$cash_conversion <- Winsorize(df.all.freq$cash_conversion,val = quantile(df.all.freq$cash_conversion, probs = c(0.01, 0.99)))

#df.all.freq[,c('month_id','cash_conversion')]

# Create dataframe only containing the frequency chosen

source(file =paste0(path_functions,"choose_data_frequency.R"), chdir = T)

out.data.freq <- choose_data_frequency(df.all.freq, freq=freq_var_data)

df.freq <-  out.data.freq$dataframe 

df.freq <- df.freq[!apply(df.freq[, CHARS], 1, function(row) any(is.na(row))), ]

DEPVAR <- out.data.freq$dep_var

rownames(df.freq) = NULL



#### Plot PD and PE

recessions.trim = subset(recessions.df, Peak >= min(df.all.freq$quarter_id[61:588]))

fund_plt_1 = ggplot(data=df.all.freq) + theme_bw() +
              geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
              geom_line( mapping= aes(y= p_d, x= quarter_id, color = "price-to-dividend ratio"), linetype="solid",size=1.15, alpha=0.6) +
              geom_line( mapping= aes(y= p_e, x= quarter_id, color = "price-to-earning ratio"), linetype="solid",size=1.15, alpha=0.6) +
              scale_color_manual(values = c('price-to-dividend ratio' = 'blue','price-to-earning ratio' = 'darkred')) +
              labs(color = '') + 
              xlab("Time") + ylab("price-to-dividend & price-to-earnings on S&P 500") +
              theme(legend.position = c(0.15,0.20), legend.text = element_text(size=13))

ggarrange(fund_plt_1, nrow = 1, ncol = 1, common.legend = T)

#### Plot DPS and ES


fund_plt_1 = ggplot(data=df.all.freq) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.5)  +
  geom_line( mapping= aes(y= D, x= quarter_id, color = "Shiller Data"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= dividends_index, x= quarter_id, color = "Firm-level Aggregation"), linetype="solid",size=1.25, alpha=0.6) +
  scale_color_manual(values = c('Shiller Data' = 'blue','Firm-level Aggregation' = 'darkred')) +
  labs(color = '') + 
  xlab("Time") + ylab("DPS S&P 500") +
  theme(legend.position = c(0.15,0.20), legend.text = element_text(size=13))


fund_plt_2 = ggplot(data=df.all.freq) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.5)  +
  geom_line( mapping= aes(y= E, x= quarter_id, color = "Shiller Data"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= earnings_index, x= quarter_id, color = "Firm-level Aggregation"), linetype="solid",size=1.25, alpha=0.6) +
  scale_color_manual(values = c('Shiller Data' = 'blue','Firm-level Aggregation' = 'darkred')) +
  labs(color = '') + 
  xlab("Time") + ylab("EPS S&P 500") +
  theme(legend.position = c(0.15,0.20), legend.text = element_text(size=13))

ggarrange(fund_plt_1,fund_plt_2, nrow = 2, ncol = 1, common.legend = T)

####


df.all.freq$d_gr_q_bu = c(rep(NA,3),diff(log(df.all.freq[,'dividends_index']),3))
df.all.freq$d_gr_1y_bu = c(rep(NA,12),diff(log(df.all.freq[,'dividends_index']),12))
df.all.freq$e_gr_q_bu = c(rep(NA,3),diff(log(df.all.freq[,'earnings_index']),3))
df.all.freq$e_gr_1y_bu = c(rep(NA,12),diff(log(df.all.freq[,'earnings_index']),12))

cor(df.all.freq$dividends_index,df.all.freq$D, use = "complete.obs")
cor(df.all.freq$earnings_index,df.all.freq$E, use = "complete.obs")
cor(df.all.freq$d_gr_q_bu,df.all.freq$d_gr_q, use = "complete.obs")
cor(df.all.freq$e_gr_q_bu,df.all.freq$e_gr_q, use = "complete.obs")
cor(df.all.freq$d_gr_1y_bu,df.all.freq$d_gr_1y, use = "complete.obs")
cor(df.all.freq$e_gr_1y_bu,df.all.freq$e_gr_1y, use = "complete.obs")



######################################################################################################
# Basic correlations PD and PE with returns and dividends
######################################################################################################


## Create rolling-correlations

df.all.freq[,'cor_5y_pd_d_gr_1y_lead'] = roll_cor( df.all.freq[,'d_gr_1y_lead'], df.all.freq[,'p_d'], width = 60)
df.all.freq[,'cor_5y_pd_ret_1y_lead']  = roll_cor( df.all.freq[,'ret_1y_lead'] , df.all.freq[,'p_d'], width = 60)
df.all.freq[,'cor_5y_pe_e_gr_1y_lead'] = roll_cor( df.all.freq[,'e_gr_1y_lead'], df.all.freq[,'p_e'], width = 60)
df.all.freq[,'cor_5y_pe_ret_1y_lead']  = roll_cor( df.all.freq[,'ret_1y_lead'] , df.all.freq[,'p_e'], width = 60)

df.all.freq[,'cor_5y_pd_d_gr_3y_lead'] = roll_cor( df.all.freq[,'d_gr_3y_lead'], df.all.freq[,'p_d'], width = 60)
df.all.freq[,'cor_5y_pd_ret_3y_lead']  = roll_cor( df.all.freq[,'ret_3y_lead'] , df.all.freq[,'p_d'], width = 60)
df.all.freq[,'cor_5y_pe_e_gr_3y_lead'] = roll_cor( df.all.freq[,'e_gr_3y_lead'], df.all.freq[,'p_e'], width = 60)
df.all.freq[,'cor_5y_pe_ret_3y_lead']  = roll_cor( df.all.freq[,'ret_3y_lead'] , df.all.freq[,'p_e'], width = 60)

df.all.freq[,'cor_5y_pd_d_gr_5y_lead'] = roll_cor( df.all.freq[,'d_gr_5y_lead'], df.all.freq[,'p_d'], width = 60)
df.all.freq[,'cor_5y_pd_ret_5y_lead']  = roll_cor( df.all.freq[,'ret_5y_lead'] , df.all.freq[,'p_d'], width = 60)
df.all.freq[,'cor_5y_pe_e_gr_5y_lead'] = roll_cor( df.all.freq[,'e_gr_5y_lead'], df.all.freq[,'p_e'], width = 60)
df.all.freq[,'cor_5y_pe_ret_5y_lead']  = roll_cor( df.all.freq[,'ret_5y_lead'] , df.all.freq[,'p_e'], width = 60)


### Create Unconditional correlations

cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'d_gr_1y_lead'], df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_d'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'ret_1y_lead'] , df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_d'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'d_gr_1y_lead'], df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_e'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'ret_1y_lead'] , df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_e'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'d_gr_5y_lead'], df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_d'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'ret_5y_lead'] , df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_d'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'e_gr_5y_lead'], df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_e'], use = "complete.obs")
cor( df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'ret_5y_lead'] , df.all.freq[df.all.freq[,'quarter_id'] > "1979-01-01",'p_e'], use = "complete.obs")



##############
#### Figure 1 - Plot Time varying correlations
####

recessions.trim = subset(recessions.df, Peak >= min(df.all.freq$quarter_id[60:504]))

fund_plt_1 = ggplot(data=df.all.freq[60:504,]) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
  geom_line( mapping= aes(y= cor_5y_pd_d_gr_5y_lead, x= quarter_id, color = "next 5y earnings growth"), linetype="solid",size=1.15, alpha=0.6) +
  geom_line( mapping= aes(y= cor_5y_pd_d_gr_5y_lead, x= quarter_id, color = "next 5y dividend growth"), linetype="solid",size=1.15, alpha=0.6) +
  geom_line( mapping= aes(y= cor_5y_pd_ret_5y_lead, x= quarter_id, color = "next 5y return"), linetype="solid",size=1.15, alpha=0.6) +
  scale_color_manual(values = c('next 5y dividend growth' = 'blue','next 5y earnings growth' = 'chartreuse4','next 5y return' = 'darkred')) +
  labs(color = '') + 
  xlab("Time") + ylab("Cor( pd , . )") + ylim(c(-1,1)) +  geom_hline(yintercept=-0.006, col= "blue", size = 1.2, linetype="twodash") +  geom_hline(yintercept=-0.697, col= "darkred", size = 1.2, linetype="twodash") +  theme(legend.position = c(0.15,0.20), legend.text = element_text(size=13))


fund_plt_2 = ggplot(data=df.all.freq[60:504,]) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
  geom_line( mapping= aes(y= cor_5y_pe_e_gr_5y_lead, x= quarter_id, color = "next 5y earnings growth"), linetype="solid",size=1.15, alpha=0.6) +
  geom_line( mapping= aes(y= cor_5y_pe_ret_5y_lead, x= quarter_id, color = "next 5y return"), linetype="solid",size=1.15, alpha=0.6) +
  scale_color_manual(values = c('next 5y earnings growth' = 'chartreuse4', 'next 5y return' = 'darkred')) +
  labs(color = '') + 
  xlab("Time") + ylab("Cor( pe , . )") + ylim(c(-1,1))  +  geom_hline(yintercept=0.503, col= "chartreuse4", size = 1.2, linetype="twodash") +  geom_hline(yintercept=-0.320, col= "darkred", size = 1.2, linetype="twodash") +
  theme(legend.position = c(0.15,0.20), legend.text = element_text(size=13))

ggarrange(fund_plt_1, fund_plt_2, nrow = 1, ncol = 2, common.legend = T)



ggarrange(fund_plt_1, fund_plt_2, nrow = 1, ncol = 2, common.legend = T)


#################################################
#### Figure 5 - Cumulative variance Canonical PCs
#################################################

pc_explained_var = as.data.frame(matrix(data = NA, nrow = length(CHARS), ncol = 2*length(CATEGORIES)))

colnames(pc_explained_var) = c(paste0(names(CATEGORIES),"_y"),paste0(names(CATEGORIES),"_m")) 


temp_df_pc_y = (df.freq[,CHARS])

temp_df_pc_m = (df.all.freq[,CHARS])

pc_df_y = as.data.frame(matrix(NA, nrow = nrow(temp_df_pc_y), ncol = 0))


for (i in names(CATEGORIES)) {
  
  temp_pc_y = prcomp( na.omit(temp_df_pc_y[CATEGORIES[[i]]]), center = T, scale. = T)
  temp_pc_m = prcomp( na.omit(temp_df_pc_m[CATEGORIES[[i]]]), center = T, scale. = T)
  
  if (freq_var_data == "y")  {
    
    df.freq[,paste0(c("pc1_","pc2_"),i)] =  scale(temp_df_pc_y[CATEGORIES[[i]]], center = T, scale = F) %*% prcomp( na.omit(temp_df_pc_y[CATEGORIES[[i]]]), center = T, scale. = T)$rotation[,1:n_pc]
  
    }
  
    df.all.freq[,paste0(c("pc1_","pc2_"),i)] = scale(temp_df_pc_m[CATEGORIES[[i]]], center = T, scale = F) %*% prcomp( na.omit(temp_df_pc_m[CATEGORIES[[i]]]), center = T, scale. = T)$rotation[,1:n_pc]
  
  n_eigv_y = length(temp_pc_y$sdev)
  
  n_eigv_m = length(temp_pc_m$sdev)
  
  temp_ = temp_pc_y$x[,1:n_pc]
  
  colnames(temp_) = paste0(i," pc",c(1:n_pc)) 
  
  pc_df_y = cbindX(pc_df_y, temp_)
  
  temp_pc_y = temp_pc_y$sdev^2/sum(temp_pc_y$sdev^2)
  temp_pc_m = temp_pc_m$sdev^2/sum(temp_pc_m$sdev^2)
  
    pc_explained_var[1:n_eigv_y,paste0(i,"_y")] = cumsum(temp_pc_y)
    pc_explained_var[1:n_eigv_m,paste0(i,"_m")] = cumsum(temp_pc_m)
  
}

rm(temp_pc_y, temp_pc_m, n_eigv_y, n_eigv_m)


source(file =paste0(path_functions,"plot_pca_v2.R"), chdir = T)

plot_pca = plot_pc(pc_explained_var); plot_pca$plot; plot_pca$plot_base; 


#################################################
#### Figure 5 - Cumulative variance Sparse PCs
#################################################

df.freq[,c("spc1_full","spc2_full")] <-  scale(df.freq[CATEGORIES[["full"]]], center = T, scale = F) %*%  spca(na.omit(df.freq[CATEGORIES[["full"]]]), center=TRUE, scale = TRUE, alpha = alpha_pca, beta = beta_pca, max_iter = 1000, verbose = F)$loadings[,1:n_pc]


df.all.freq[,c("spc1_full","spc2_full")] <-  scale(df.all.freq[CATEGORIES[["full"]]], center = T, scale = F) %*%  spca(na.omit(df.all.freq[CATEGORIES[["full"]]]), center=TRUE, scale = TRUE, alpha = alpha_pca, beta = beta_pca, max_iter = 1000, verbose = F)$loadings[,1:n_pc]

colMeans(df.freq[,c("spc1_full","spc2_full")]) ; colMeans(df.freq[,c("pc1_full","pc2_full")] )
colVars(as.matrix(df.freq[,c("spc1_full","spc2_full")])) ; colVars(as.matrix(df.freq[,c("pc1_full","pc2_full")]))

cor(df.freq[,c("spc1_full","spc2_full")]) 

### Plot time series of Pcs and sparse PCs

recessions.trim = subset(recessions.df, Peak >= min(df.freq$quarter_id))

spc_full_plot =  ggplot(data=df.freq) + theme_bw() +
                geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
                geom_line( mapping= aes(y= spc1_full, x= quarter_id, color = "SPc1 (full set)"), linetype="solid",size=1.15, alpha=0.6) +
                geom_line( mapping= aes(y= spc2_full, x= quarter_id, color = "SPc2 (full set)"), linetype="solid",size=1.15, alpha=0.6) +
                labs(color = '') + 
                scale_color_manual(values = c(
                  'SPc1 (full set)' = 'blue',
                  'SPc2 (full set)' = 'darkred')) +
                  xlab("time") + ylab("SPc (full set)") +  ylim(c(-28,12)) +
                  theme(legend.position = c(0.10,0.20)) 

pc_full_plot =  ggplot(data=df.freq) + theme_bw() +
                geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
                geom_line( mapping= aes(y= pc1_full, x= quarter_id, color = "Pc1 (full set)"), linetype="solid",size=1.15, alpha=0.6) +
                geom_line( mapping= aes(y= pc2_full, x= quarter_id, color = "Pc2 (full set)"), linetype="solid",size=1.15, alpha=0.6) +
                labs(color = '') + 
                scale_color_manual(values = c(
                  'Pc1 (full set)' = 'blue',
                  'Pc2 (full set)' = 'darkred')) +
                xlab("time") + ylab("PCs (full set)") + ylim(c(-30,15)) +
                theme(legend.position = c(0.10,0.20)) 



pc_prof_plot =  ggplot(data=df.freq) + theme_bw() +
                geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
                geom_line( mapping= aes(y= pc1_profitability, x= quarter_id, color = "Pc1 (profitability)"), linetype="solid",size=1.15, alpha=0.6) +
                geom_line( mapping= aes(y= pc2_profitability, x= quarter_id, color = "Pc2 (profitability)"), linetype="solid",size=1.15, alpha=0.6) +
                labs(color = '') + 
                scale_color_manual(values = c(
                  'Pc1 (profitability)' = 'blue',
                  'Pc2 (profitability)' = 'darkred')) +
                xlab("time") + ylab("PCs (profitability)") + # ylim(c(0.7,1)) +
                theme(legend.position = c(0.10,0.8)) 

ggarrange(pc_full_plot,spc_full_plot, pc_prof_plot, nrow = 3, ncol = 1, common.legend = F)
ggarrange(spc_full_plot, pc_prof_plot, nrow = 2, ncol = 1, common.legend = F)




######################################################################################################
# RUN THE (UNIVARIATE) STATIC MODEL
######################################################################################################

# Univariate OLS: in sample and out of sample

source(file =paste0(path_functions,"ols_univariate_v6.R"), chdir = T)

if(run_ols == TRUE){
  
  ols_univ = ols_univariate(data = df.all.freq, splitratio = splitratio, CHARS = CHARS, data.freq = "q", oos_ols = F)
  #r2.ols_oos = ols_univ$r2.oos
  #r2.ols_oos_grounded = r2.ols_oos  %>%  mutate(across(where(is.numeric), ~if_else(. < 0, 0, .))) %>%  drop_na() %>% as.data.frame()
  ols_univ$plot_is
  #ols_univ$plot_oos

}




######################################################################################################
#  In sample
######################################################################################################

###
### OLS

df.all.freq.pcs  = na.omit(df.all.freq[,c('quarter_id',"month_id",'month',"d_gr_1y_lead","e_gr_1y_lead",'ret_1y_lead',"p_d_1y_lead","p_e_1y_lead", DEPVAR,"spc1_full","spc2_full","pc1_full","pc2_full",'pc1_capitalization','pc2_capitalization',"pc1_efficiency", "pc2_efficiency","pc1_financial_soundness","pc2_financial_soundness","pc1_liquidity","pc2_liquidity","pc1_profitability","pc2_profitability","pc1_solvency","pc2_solvency","pc1_valuation","pc2_valuation","pc1_equity_premium","pc2_equity_premium","pc1_other","pc2_other", "e_d","e_gr_1y")])

df.freq.pcs  = na.omit(df.freq[,c('quarter_id',"month_id",'month',"d_gr_1y_lead","e_gr_1y_lead",'ret_1y_lead',"p_d_1y_lead","p_e_1y_lead",DEPVAR,"spc1_full","spc2_full","pc1_full","pc2_full",'pc1_capitalization','pc2_capitalization',"pc1_efficiency", "pc2_efficiency","pc1_financial_soundness","pc2_financial_soundness","pc1_liquidity","pc2_liquidity","pc1_profitability","pc2_profitability","pc1_solvency","pc2_solvency","pc1_valuation","pc2_valuation","pc1_equity_premium","pc2_equity_premium","pc1_other","pc2_other", "e_d","e_gr_1y")])


df.all.freq = df.all.freq[complete.cases(df.all.freq[, CHARS]), ]

#library(GGally)

#ggpairs(df.m.freq[,c("spc1_full","spc2_full",'pc1_capitalization','pc2_capitalization',"pc1_efficiency", "pc2_efficiency","pc1_financial_soundness","pc2_financial_soundness","pc1_liquidity","pc2_liquidity","pc1_profitability","pc2_profitability","pc1_solvency","pc2_solvency","pc1_valuation","pc2_valuation","pc1_equity_premium","pc2_equity_premium","pc1_other","pc2_other")], title="correlogram with ggpairs()") 

#alpha.pc = seq(0,0.001, length =50)
#r2_ols_lambda = matrix(NA, nrow = 50, ncol = 2)

#for (i in 1:50) {
  
#  df.all.freq.pcs[,c("spc1_full","spc2_full")] <-  scale(df.all.freq[CATEGORIES[["full"]]], center = T, scale = F) %*%  spca(na.omit(df.all.freq[CATEGORIES[["full"]]]), center=TRUE, scale = TRUE, alpha = alpha.pc[i], beta = beta_pca, max_iter = 1000, verbose = F)$loadings[,1:n_pc]
  
#  ols_div = lm(d_gr_1y_lead  ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs)
#  ols_ret = lm(ret_1y_lead  ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs)
  
#  r2_ols_lambda[i,]  = c ( 1 -  var(ols_div$residuals) / var(df.all.freq.pcs$d_gr_1y_lead), 1 - var(ols_ret$residuals) / var(df.all.freq.pcs$ret_1y_lead) )
  
#}



ols_ret = lm(ret_1y_lead  ~ d_gr_1y + p_d + p_e + ret_1y + pc1_full + pc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(ols_ret) #Adj R2 0.488
ols_ret = lm(ret_1y_lead  ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(ols_ret) #Adj R2 0.488

## Baseline model Ols

ols_div = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(ols_div)$r.squared #Adj R2 0.759 
ols_ear = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(ols_ear)$r.squared #Adj R2 0.487
ols_ret = lm(ret_1y_lead  ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(ols_ret)$r.squared #Adj R2 0.488


df.all.freq.pcs$d_gr_1y_lead_hat = fitted(ols_div)
df.all.freq.pcs$e_gr_1y_lead_hat = fitted(ols_ear)
df.all.freq.pcs$ret_1y_lead_hat  = fitted(ols_ret)

##

depvar_div_ols      = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y, data = df.all.freq.pcs);  summary(depvar_div_ols)$r.squared #Adj R2 0.549
full_div_ols        = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full, data = df.all.freq.pcs) ; summary(full_div_ols)$r.squared #Adj R2 0.555 
capit_div_ols       = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_capitalization + pc2_capitalization, data = df.all.freq.pcs) ; summary(capit_div_ols)$r.squared #Adj R2 0.596
effic_div_ols       = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_efficiency + pc2_efficiency, data = df.all.freq.pcs) ; summary(effic_div_ols)$r.squared #Adj R2 0.690
fin_sound_div_ols   = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_financial_soundness + pc2_financial_soundness, data = df.all.freq.pcs) ; summary(fin_sound_div_ols)$r.squared #Adj R2 0.667 
prof_div_ols        = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(prof_div_ols)$r.squared #Adj R2 0.759
liquid_div_ols      = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_liquidity + pc2_liquidity, data = df.all.freq.pcs) ; summary(liquid_div_ols)$r.squared #Adj R2 0.639
solven_div_ols      = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_solvency + pc2_solvency, data = df.all.freq.pcs) ; summary(solven_div_ols)$r.squared #Adj R2 0.558 
valuation_div_ols   = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_valuation + pc2_valuation, data = df.all.freq.pcs) ; summary(valuation_div_ols)$r.squared #Adj R2 0.695  
equity_prem_div_ols = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_equity_premium + pc2_equity_premium, data = df.all.freq.pcs) ; summary(equity_prem_div_ols)$r.squared #Adj R2 0.642 
other_div_ols       = lm(d_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_other + pc2_other, data = df.all.freq.pcs) ; summary(other_div_ols)$r.squared #Adj R2 0.571 

models_div_ols  = list(capit_div_ols,effic_div_ols,fin_sound_div_ols,prof_div_ols,liquid_div_ols,solven_div_ols,valuation_div_ols,equity_prem_div_ols,other_div_ols)$r.squared




depvar_ret_ols      = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y, data = df.all.freq.pcs); summary(depvar_ret_ols)$r.squared #Adj R2 0.105 
full_ret_ols        = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full, data = df.all.freq.pcs) ; summary(full_ret_ols)$r.squared #Adj R2 0.125
capit_ret_ols       = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_capitalization + pc2_capitalization, data = df.all.freq.pcs) ; summary(capit_ret_ols)$r.squared #Adj R2 0.239
effic_ret_ols       = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_efficiency + pc2_efficiency, data = df.all.freq.pcs) ; summary(effic_ret_ols)$r.squared #Adj R2 0.283
fin_sound_ret_ols   = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_financial_soundness + pc2_financial_soundness, data = df.all.freq.pcs) ; summary(fin_sound_ret_ols)$r.squared #Adj R2 0.2757 
prof_ret_ols        = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(prof_ret_ols)$r.squared #Adj R2 0.285 
liquid_ret_ols      = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_liquidity + pc2_liquidity, data = df.all.freq.pcs) ; summary(liquid_ret_ols)$r.squared #Adj R2 0.168
solven_ret_ols      = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_solvency + pc2_solvency, data = df.all.freq.pcs) ; summary(solven_ret_ols)$r.squared #Adj R2 0.205
valuation_ret_ols   = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_valuation + pc2_valuation, data = df.all.freq.pcs) ; summary(valuation_ret_ols)$r.squared #Adj R2 0.202   
equity_prem_ret_ols = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_equity_premium + pc2_equity_premium, data = df.all.freq.pcs) ; summary(equity_prem_ret_ols)$r.squared #Adj R2 0.207
other_ret_ols       = lm(ret_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_other + pc2_other, data = df.all.freq.pcs) ; summary(other_ret_ols)$r.squared #Adj R2 0.325 

models_ret_ols  = list(capit_ret_ols,effic_ret_ols,fin_sound_ret_ols,prof_ret_ols,liquid_ret_ols,solven_ret_ols,valuation_ret_ols,equity_prem_ret_ols,other_ret_ols)



depvar_ear_ols      = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y, data = df.all.freq.pcs); summary(depvar_ear_ols)$r.squared #Adj R2 0.3091
full_ear_ols        = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full, data = df.all.freq.pcs) ; summary(full_ear_ols)$r.squared #Adj R2 0.3298
capit_ear_ols       = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_capitalization + pc2_capitalization, data = df.all.freq.pcs) ; summary(capit_ear_ols)$r.squared #Adj R2 0.4172
effic_ear_ols       = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_efficiency + pc2_efficiency, data = df.all.freq.pcs) ; summary(effic_ear_ols)$r.squared #Adj R2 0.3849
fin_sound_ear_ols   = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_financial_soundness + pc2_financial_soundness, data = df.all.freq.pcs) ; summary(fin_sound_ear_ols)$r.squared #Adj R2 0.3922
prof_ear_ols        = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.all.freq.pcs) ; summary(prof_ear_ols)$r.squared #Adj R2 0.4254
liquid_ear_ols      = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_liquidity + pc2_liquidity, data = df.all.freq.pcs) ; summary(liquid_ear_ols)$r.squared #Adj R2 0.5372 
solven_ear_ols      = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_solvency + pc2_solvency, data = df.all.freq.pcs) ; summary(solven_ear_ols)$r.squared #Adj R2 0.3327 
valuation_ear_ols   = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_valuation + pc2_valuation, data = df.all.freq.pcs) ; summary(valuation_ear_ols)$r.squared #Adj R2 0.3421  
equity_prem_ear_ols = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_equity_premium + pc2_equity_premium, data = df.all.freq.pcs) ; summary(equity_prem_ear_ols)$r.squared #Adj R2 0.4604 
other_ear_ols       = lm(e_gr_1y_lead ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_other + pc2_other, data = df.all.freq.pcs) ; summary(other_ear_ols)$r.squared #Adj R2 0.3395 

models_ear_ols  = list(capit_ear_ols,effic_ear_ols,fin_sound_ear_ols,prof_ear_ols,liquid_ear_ols,solven_ear_ols,valuation_ear_ols,equity_prem_ear_ols,other_ear_ols)








fund_plt_1 = ggplot(data = df.all.freq.pcs) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_1y_lead, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= d_gr_1y_lead_hat, x= quarter_id, color = "fitted values"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'data' = 'blue',
    'fitted values' = 'darkred')) +
  xlab("time") + ylab("dividend growth") + ylim(c(-0.3,0.2)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))

fund_plt_2 = ggplot(data = df.all.freq.pcs) + theme_bw() +
  geom_line( mapping= aes(y= e_gr_1y_lead, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= e_gr_1y_lead_hat, x= quarter_id, color = "fitted values"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'data' = 'blue',
    'fitted values' = 'darkred')) +
  xlab("time") + ylab("earnings growth") + ylim(c(-2.2,2.2)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))

fund_plt_3 = ggplot(data = df.all.freq.pcs) + theme_bw() +
  geom_line( mapping= aes(y= ret_1y_lead, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= ret_1y_lead_hat, x= quarter_id, color = "fitted values"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'data' = 'blue',
    'fitted values' = 'darkred')) +
  xlab("time") + ylab("return") + ylim(c(-0.55,0.55)) + 
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))


ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, nrow = 3, ncol = 1, common.legend = T)



# Convert predictors (X) to matrix
X <- as.matrix(df.all.freq.pcs[, c(DEPVAR,"spc1_full", "spc2_full", "pc1_profitability", "pc2_profitability")])

# Convert dependent variables (Y) to matrix
Y <- as.matrix(df.all.freq.pcs[, c("d_gr_1y_lead", "p_d_1y_lead", "p_e_1y_lead", "ret_1y_lead")])

# Run Lasso regression
lasso_model <- glmnet(X, Y[,1], alpha = 1); summary(lasso_model)


#################################
## Compare with De la O Table III
#################################

cor(df.all.freq[df.all.freq[,'quarter_id']>= "2003-01-01" & df.all.freq[,'quarter_id']<= "2015-12-31",'d_gr_1y_lead'],df.all.freq[df.all.freq[,'quarter_id']>= "2003-01-01" & df.all.freq[,'quarter_id']<= "2015-12-31",'p_d'], use = "complete.obs") # De la O: 0.70
cor(df.all.freq[df.all.freq[,'quarter_id']>= "2003-01-01" & df.all.freq[,'quarter_id']<= "2015-12-31",'e_gr_1y_lead'],df.all.freq[df.all.freq[,'quarter_id']>= "2003-01-01" & df.all.freq[,'quarter_id']<= "2015-12-31",'p_e'], use = "complete.obs") # De la O: 0.61
cor(df.all.freq[df.all.freq[,'quarter_id']>= "1976-01-01" & df.all.freq[,'quarter_id']<= "2002-12-31",'e_gr_1y_lead'],df.all.freq[df.all.freq[,'quarter_id']>= "1976-01-01" & df.all.freq[,'quarter_id']<= "2002-12-31",'p_e'], use = "complete.obs") # De la O: 0.14
cor(df.all.freq[df.all.freq[,'quarter_id']>= "1976-01-01" & df.all.freq[,'quarter_id']<= "2015-12-31",'e_gr_1y_lead'],df.all.freq[df.all.freq[,'quarter_id']>= "1976-01-01" & df.all.freq[,'quarter_id']<= "2015-12-31",'p_e'], use = "complete.obs") # De la O: 0.27



cor(df.freq[df.freq[,'quarter_id']>= "2003-01-01" & df.freq[,'quarter_id']<= "2015-12-31",'d_gr_1y_lead'],df.freq[df.freq[,'quarter_id']>= "2003-01-01" & df.freq[,'quarter_id']<= "2015-12-31",'p_d'], use = "complete.obs") # De la O: 0.70
cor(df.freq[df.freq[,'quarter_id']>= "2003-01-01" & df.freq[,'quarter_id']<= "2015-12-31",'e_gr_1y_lead'],df.freq[df.freq[,'quarter_id']>= "2003-01-01" & df.freq[,'quarter_id']<= "2015-12-31",'p_e'], use = "complete.obs") # De la O: 0.61
cor(df.freq[df.freq[,'quarter_id']>= "1976-01-01" & df.freq[,'quarter_id']<= "2002-12-31",'e_gr_1y_lead'],df.freq[df.freq[,'quarter_id']>= "1976-01-01" & df.freq[,'quarter_id']<= "2002-12-31",'p_e'], use = "complete.obs") # De la O: 0.14
cor(df.freq[df.freq[,'quarter_id']>= "1976-01-01" & df.freq[,'quarter_id']<= "2015-12-31",'e_gr_1y_lead'],df.freq[df.freq[,'quarter_id']>= "1976-01-01" & df.freq[,'quarter_id']<= "2015-12-31",'p_e'], use = "complete.obs") # De la O: 0.27



######################################################################################################
# MULTIVARIATE DYNAMIC MODEL
######################################################################################################

#alpha_pca = 1.75e-4 

var_variable = c(DEPVAR,'quarter_id',"spc1_full","spc2_full","pc1_capitalization","pc2_capitalization","pc1_efficiency","pc2_efficiency","pc1_financial_soundness","pc2_financial_soundness","pc1_profitability","pc2_profitability","pc1_liquidity","pc2_liquidity","pc1_solvency","pc2_solvency","pc1_valuation","pc2_valuation","pc1_equity_premium","pc2_equity_premium","pc1_other","pc2_other")

df.freq.pcs = df.freq.pcs[complete.cases(df.freq.pcs[, var_variable]), ]

capit_var       = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_capitalization","pc2_capitalization")], p=lags, type= "const") ; summary(capit_var) #Adj R2 0.6244, 0.878, 0.680, 0.286
effic_var       = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_efficiency","pc2_efficiency")], p=lags, type= "const") ; summary(effic_var) #Adj R2 0.625,0.865,0.671,0.179
fin_sound_var   = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_financial_soundness","pc2_financial_soundness")], p=lags, type= "const") ; summary(fin_sound_var) #Adj R2 0.689,0.880,0.622,0.300
prof_var        = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_profitability","pc2_profitability")], p=lags, type= "const") ; summary(prof_var) #Adj R2 0.757,0.902,0.623,0.389
liquid_var      = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_liquidity","pc2_liquidity")], p=lags, type= "const") ; summary(liquid_var) #Adj R2 0.662,0.873,0.755,0.199 
solven_var      = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_solvency","pc2_solvency")], p=lags, type= "const") ; summary(solven_var) #Adj R2 0.638,0.860,0.600,0.095
valuation_var   = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_valuation","pc2_valuation")], p=lags, type= "const") ; summary(valuation_var) #Adj R2 0.698,0.865,0.599,0.141
equity_prem_var = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_equity_premium","pc2_equity_premium")], p=lags, type= "const") ; summary(equity_prem_var) #Adj R2 0.606,0.901,0.621,0.342
other_var       = VAR(y=df.freq.pcs[, c(DEPVAR,"spc1_full","spc2_full","pc1_other","pc2_other")], p=lags, type= "const") ; summary(other_var) #Adj R2 0.654,0.907,0.590,0.413
depvar_var      = VAR(y=df.freq.pcs[, c(DEPVAR)], p=lags, type= "const"); summary(depvar_var) #Adj R2 0.589,0.857,0.621,0.033



#####################################################
#### In-Sample R^2 as a function of Lambda_2
#####################################################

source(file=paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\estimate_model_in_sample_v5.R"), chdir = T)
source(file=paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\estimate_model_in_sample_v6_always_full.R"), chdir = T)

grid_sparse_pc = seq(0.00000,0.0003, length.out = 51)
grid_sparse_pc_len = length(grid_sparse_pc)

r2.models_matrix_unc = as.data.frame(matrix(NA,grid_sparse_pc_len,5))
r2.models_matrix_lasso = as.data.frame(matrix(NA,grid_sparse_pc_len,5))
r2.models_matrix_ridge = as.data.frame(matrix(NA,grid_sparse_pc_len,5))
r2.models_matrix_elnet = as.data.frame(matrix(NA,grid_sparse_pc_len,5))

explained_variance = as.data.frame(matrix(NA,grid_sparse_pc_len, n_pc+2))
perc_zero_loadings = as.data.frame(matrix(NA,grid_sparse_pc_len, n_pc+1))

r2.models_matrix_unc[,1] = r2.models_matrix_lasso[,1] = r2.models_matrix_ridge[,1] = r2.models_matrix_elnet[,1] = explained_variance[,1] = perc_zero_loadings[,1] = c(round(grid_sparse_pc,digits = 6))
colnames(r2.models_matrix_unc)[1:5] = colnames(r2.models_matrix_lasso)[1:5] = colnames(r2.models_matrix_ridge)[1:5] = colnames(r2.models_matrix_elnet)[1:5] =  c("alpha",DEPVAR)

colnames(explained_variance)[1:(n_pc+2)] = c("alpha", paste0("percentage_pc",1:n_pc), "percentage_cumulative_pc")
colnames(perc_zero_loadings)[1:(n_pc+1)] = c("alpha", paste0("perc_zero_pc",1:n_pc))

Sigma_df = t(scale(na.omit(df.freq[,CHARS]), center = T, scale = T)) %*% scale(na.omit(df.freq[,CHARS]), center = T, scale = T) / (nrow(na.omit(df.freq[,CHARS]))-1)

SPc1_loadings_matrix = as.data.frame(matrix(NA,length(CHARS), grid_sparse_pc_len))
SPc2_loadings_matrix = as.data.frame(matrix(NA,length(CHARS), grid_sparse_pc_len))


for (i in 1:grid_sparse_pc_len) {
  
  var_insample = estimate_model_in_sample(data = df.freq, data.pc= df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, 
                                          time.col = "quarter_id", data.freq = freq_var_data, lags = lags, model = "profitability", pca = T, n_pc = n_pc,
                                          where.funcs = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\", 
                                          optional.args = list(sep.lambdas=sep_lambdas, fit.intercept = fit_intercept, pen.search = pen_search), 
                                          sparse.pca = sparse_pca, alpha.pca = grid_sparse_pc[i], beta.pca = beta_pca, t1 = t1, t2 = t2, extra = NA)
                            
  r2.models_unc   <- var_insample$r2.is_uncon; #print(r2.models_unc)
  r2.models_lasso <- var_insample$r2.is_lasso; print(r2.models_lasso)
  r2.models_ridge <- var_insample$r2.is_ridge; #print(r2.models_ridge)
  r2.models_elnet <- var_insample$r2.is_elnet; #print(r2.models_elnet)
  
  r2.models_unc_adj   <- var_insample$r2.is_uncon_adj; #print(r2.models_unc_adj)
  r2.models_lasso_adj <- var_insample$r2.is_lasso_adj; #print(r2.models_lasso_adj)
  r2.models_ridge_adj <- var_insample$r2.is_ridge_adj; #print(r2.models_ridge_adj)
  r2.models_elnet_adj <- var_insample$r2.is_elnet_adj; #print(r2.models_elnet_adj)  
  
  r2.models_matrix_unc[i,2:5] = r2.models_unc[6,2:5]
  r2.models_matrix_lasso[i,2:5] = r2.models_lasso[6,2:5]
  r2.models_matrix_ridge[i,2:5] = r2.models_ridge[6,2:5]
  r2.models_matrix_elnet[i,2:5] = r2.models_elnet[6,2:5]
  
  rowSums(r2.models_matrix_lasso[,-1])
  
  var_insample$loadings_spca_full
  var_insample$loadings_pca_full
  
  var_spc_1 = t(var_insample$loadings_spca_full[,1]) %*% Sigma_df %*% var_insample$loadings_spca_full[,1]
  var_pc_1 = t(var_insample$loadings_pca_full[,1]) %*% Sigma_df %*% var_insample$loadings_pca_full[,1]
  
  var_spc_2 = t(var_insample$loadings_spca_full[,2]) %*% Sigma_df %*% var_insample$loadings_spca_full[,2]
  var_pc_2 = t(var_insample$loadings_pca_full[,2]) %*% Sigma_df %*% var_insample$loadings_pca_full[,2]
  
  explained_variance[i,2:4] = c(var_spc_1/var_pc_1, var_spc_2/var_pc_2, (var_spc_1 + var_spc_2) /(var_pc_1 + var_pc_2))
  perc_zero_loadings[i,2:3] = colSums( var_insample$loadings_spca_full[,1:2] == 0)/ nrow(var_insample$loadings_spca_full)
  
  SPc1_loadings_matrix[,i] = var_insample$loadings_spca_full[,1]
  SPc2_loadings_matrix[,i] = var_insample$loadings_spca_full[,2]
 
}


SPc1_loadings_matrix = data.frame(variables = CHARS, SPc1_loadings_matrix)
SPc2_loadings_matrix = data.frame(variables = CHARS, SPc2_loadings_matrix)


df.freq.chars = df.freq[,CHARS]


SPC1_matrix = as.data.frame(matrix(NA, grid_sparse_pc_len, length(CATEGORIES))); colnames(SPC1_matrix) = c("lambda", names(CATEGORIES)[-10]); SPC1_matrix[,'lambda'] = grid_sparse_pc
SPC2_matrix = as.data.frame(matrix(NA, grid_sparse_pc_len, length(CATEGORIES))); colnames(SPC2_matrix) = c("lambda", names(CATEGORIES)[-10]); SPC2_matrix[,'lambda'] = grid_sparse_pc


for (j in 1:9) {
  
  for (i in 1:grid_sparse_pc_len) {
    
  Sigma_cat =  as.matrix(t(na.omit(df.freq.chars[,CHARS %in% CATEGORIES[[j]]]))) %*% as.matrix(na.omit(df.freq.chars[,CHARS %in% CATEGORIES[[j]]])) / (nrow(na.omit(df.freq.chars))-1)
  
  SPC1_matrix[i,1+j] = SPc1_loadings_matrix[SPc1_loadings_matrix[,1] %in% CATEGORIES[[j]],1+i] %*% Sigma_cat %*% SPc1_loadings_matrix[SPc1_loadings_matrix[,1] %in% CATEGORIES[[j]],1+i] / SPc1_loadings_matrix[SPc1_loadings_matrix[,1] %in% CATEGORIES[[j]],2] %*% Sigma_cat %*% SPc1_loadings_matrix[SPc1_loadings_matrix[,1] %in% CATEGORIES[[j]],2] 
  
  SPC2_matrix[i,1+j] = SPc2_loadings_matrix[SPc2_loadings_matrix[,1] %in% CATEGORIES[[j]],1+i] %*% Sigma_cat %*% SPc2_loadings_matrix[SPc2_loadings_matrix[,1] %in% CATEGORIES[[j]],1+i] / SPc2_loadings_matrix[SPc2_loadings_matrix[,1] %in% CATEGORIES[[j]],2] %*% Sigma_cat %*% SPc2_loadings_matrix[SPc2_loadings_matrix[,1] %in% CATEGORIES[[j]],2] 
  
  }

}



  ##############
  #### Figure 4 - Sparse Vs Unconstrained PCs
  ############## 
  

  fund_plt_7 =  ggplot(data = explained_variance) + theme_bw() +
                geom_line( mapping= aes(y= percentage_pc1, x= alpha, color = "ratio variance PC1"), linetype="solid",size=1.25, alpha=0.6 ) +
                geom_line( mapping= aes(y= percentage_pc2, x= alpha, color = "ratio variance PC2"), linetype="solid",size=1.25, alpha=0.6 ) +
                geom_line( mapping= aes(y= percentage_cumulative_pc, x= alpha, color = "ratio variance PC1 + PC2"), linetype="solid",size=1.25, alpha=0.6 ) +
                geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) + 
                labs(color = '')+ 
                scale_color_manual(values = c(
                  'ratio variance PC1' = 'blue',
                  'ratio variance PC2' = 'darkred',
                  "ratio variance PC1 + PC2" = 'darkgreen')) +
                xlab(expression(lambda[2])) + ylab("ratio explained variance SPC / PC") + ylim(c(0.63,1.01)) +
                theme(legend.position = c(0.8,0.70)) + ggtitle("ratio explained variance SPC / PC") 
    
  fund_plt_8 = ggplot(data = perc_zero_loadings) + theme_bw() +
                geom_line( mapping= aes(y= perc_zero_pc1, x= alpha, color = "% zero SPC1"), linetype="solid",size=1.25, alpha=0.6 ) +
                geom_line( mapping= aes(y= perc_zero_pc2, x= alpha, color = "% zero SPC2"), linetype="solid",size=1.25, alpha=0.6 ) +
                geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) + 
                labs(color = '')+ 
                scale_color_manual(values = c(
                  '% zero SPC1' = 'blue',
                  '% zero SPC2' = 'darkred')) +
                xlab(expression(lambda[2])) + ylab("% of zeros in the loadings vector") +
                theme(legend.position = c(0.8,0.20)) + ggtitle("liquidity") + ggtitle("% zero in eigenvector") 
  
  ggarrange(fund_plt_7, fund_plt_8, nrow = 1, ncol = 2, common.legend = F)
  

  ##############
  #### Figure 4 - Sparse Vs Unconstrained PCs
  ############## 
  
  
  
  fund_plt_9 = ggplot(data = SPC1_matrix[1:16,]) + theme_bw() +
    geom_point(mapping = aes(y = capitalization, x = lambda, color = "Capitalization"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = capitalization, x = lambda, color = "Capitalization"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = efficiency, x = lambda, color = "Efficiency"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = efficiency, x = lambda, color = "Efficiency"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = financial_soundness, x = lambda, color = "Financial Soundness"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = financial_soundness, x = lambda, color = "Financial Soundness"), size = 1.25, alpha = 0.8) +
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    labs(color = '') +
    scale_color_manual(values = c(
      'Capitalization' = 'blue',
      'Efficiency' = 'darkred',
      "Financial Soundness" = 'darkgreen')) +
    xlab(expression(lambda[2])) +
    ylab("Ratio Explained Variance SPC1 / PC1") +
    ylim(c(0, 3.2)) +
    theme(legend.position = c(0.8, 0.70)) +
    ggtitle("Ratio Explained Variance SPC1 / PC1")
  
  fund_plt_10 =  ggplot(data = SPC2_matrix[1:16,]) + theme_bw() +
    geom_point(mapping = aes(y = capitalization, x = lambda, color = "Capitalization"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = capitalization, x = lambda, color = "Capitalization"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = efficiency, x = lambda, color = "Efficiency"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = efficiency, x = lambda, color = "Efficiency"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = financial_soundness, x = lambda, color = "Financial Soundness"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = financial_soundness, x = lambda, color = "Financial Soundness"), size = 1.25, alpha = 0.8) +
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    labs(color = '') +
    scale_color_manual(values = c(
      'Capitalization' = 'blue',
      'Efficiency' = 'darkred',
      "Financial Soundness" = 'darkgreen')) +
    xlab(expression(lambda[2])) + ylab("ratio explained variance SPC2 / PC2") + ylim(c(0,3.2)) +
    theme(legend.position = c(0.8,0.70)) + ggtitle("ratio explained variance SPC2 / PC2") 
  
  ggarrange(fund_plt_9, fund_plt_10, nrow = 1, ncol = 2, common.legend = T)
  
  
  
  
  
  fund_plt_11 <- ggplot(data = SPC1_matrix[1:16,]) + theme_bw() +
    geom_point(mapping = aes(y = liquidity , x = lambda, color = "Liquidity"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = liquidity , x = lambda, color = "Liquidity"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = profitability     , x = lambda, color = "Profitability"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = profitability     , x = lambda, color = "Profitability"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = solvency, x = lambda, color = "Solvency"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = solvency, x = lambda, color = "Solvency"), size = 1.25, alpha = 0.8) +
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    labs(color = '') +
    scale_color_manual(values = c(
      'Liquidity' = 'blue',
      'Profitability' = 'darkred',
      "Solvency" = 'darkgreen')) +
    xlab(expression(lambda[2])) +
    ylab("Ratio Explained Variance SPC1 / PC1") +
    ylim(c(0, 3.1)) +
    theme(legend.position = c(0.8, 0.70)) +
    ggtitle("Ratio Explained Variance SPC1 / PC1")
  
  fund_plt_12 =  ggplot(data = SPC2_matrix[1:16,]) + theme_bw() +
    geom_point(mapping = aes(y = liquidity, x = lambda, color = "Liquidity"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = liquidity, x = lambda, color = "Liquidity"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = profitability, x = lambda, color = "Profitability"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = profitability, x = lambda, color = "Profitability"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = solvency, x = lambda, color = "Solvency"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = solvency, x = lambda, color = "Solvency"), size = 1.25, alpha = 0.8) +
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    labs(color = '') +
    scale_color_manual(values = c(
      'Liquidity' = 'blue',
      'Profitability' = 'darkred',
      "Solvency" = 'darkgreen')) +
    xlab(expression(lambda[2])) + ylab("ratio explained variance SPC2 / PC2") + ylim(c(0,3.1)) +
    theme(legend.position = c(0.8,0.70)) + ggtitle("ratio explained variance SPC2 / PC2") 
  
  ggarrange(fund_plt_11, fund_plt_12, nrow = 1, ncol = 2, common.legend = T)
  
  
  
  
  
  fund_plt_13 <- ggplot(data = SPC1_matrix[1:16,]) + theme_bw() +
    geom_point(mapping = aes(y = valuation , x = lambda, color = "Valuation"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = valuation , x = lambda, color = "Valuation"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = equity_premium    , x = lambda, color = "Equity Premium"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = equity_premium    , x = lambda, color = "Equity Premium"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = other, x = lambda, color = "Other"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = other, x = lambda, color = "Other"), size = 1.25, alpha = 0.8) +
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    labs(color = '') +
    scale_color_manual(values = c(
      'Valuation' = 'blue',
      'Equity Premium' = 'darkred',
      "Other" = 'darkgreen')) +
    xlab(expression(lambda[2])) +
    ylab("Ratio Explained Variance SPC1 / PC1") +
    ylim(c(0, 3.1)) +
    theme(legend.position = c(0.8, 0.70)) +
    ggtitle("Ratio Explained Variance SPC1 / PC1")
  
  fund_plt_14 =  ggplot(data = SPC2_matrix[1:16,]) + theme_bw() +
    geom_point(mapping = aes(y = valuation , x = lambda, color = "Valuation"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = valuation , x = lambda, color = "Valuation"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = equity_premium, x = lambda, color = "Equity Premium"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = equity_premium, x = lambda, color = "Equity Premium"), size = 1.25, alpha = 0.8) +
    geom_point(mapping = aes(y = other, x = lambda, color = "Other"), size = 2, alpha = 0.8) +
    geom_line(mapping = aes(y = other, x = lambda, color = "Other"), size = 1.25, alpha = 0.8) +
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    labs(color = '') +
    scale_color_manual(values = c(
      'Valuation' = 'blue',
      'Equity Premium' = 'darkred',
      "Other" = 'darkgreen')) +
    xlab(expression(lambda[2])) + ylab("ratio explained variance SPC2 / PC2") + ylim(c(0,3.1)) +
    theme(legend.position = c(0.8,0.70)) + ggtitle("ratio explained variance SPC2 / PC2") 
  
  ggarrange(fund_plt_13, fund_plt_14, nrow = 1, ncol = 2, common.legend = T)
  

  


  
  ###########################################################
  #### Cross-validation In Sample
  ###########################################################
  
  source(file=paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\cross_validation_spca.R"), chdir = T)
  
  sparsity_params = seq(0.0000,0.0005,length=51)
  
  cv_lamda = cross_validation_spca(data = df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, sparsity.params = sparsity_params, beta.pca = 0, n_pc = n_pc)
  
  optimal_lambda_2_is = cv_lamda$optimal_lambda
  
  
  plot_1 <- ggplot(data=cv_lamda$cv_errors) + 
    theme_bw() +
    geom_line(mapping=aes(y=Mean_Squared_Error, x=Sparsity_Parameter), linetype="solid", size=1.15, alpha=0.6) +
    geom_point(mapping=aes(y=Mean_Squared_Error, x=Sparsity_Parameter), size=3, alpha=0.8) +  # Add points
    labs(color = '') + 
    xlab(bquote("Sparsity Parameter " ~ lambda[2])) +  # Combine text and Greek letter with subscript
    ylab("Mean Squared Error") +
    theme(legend.position = c(0.25, 0.80), legend.text = element_text(size=13))
  
  
  
  ggarrange(plot_1, nrow = 1, ncol = 1, common.legend = F)
  
  
  
  
  
  ###################################################
  #### Figure 6 - In-sample $R_{is}^2$ and Sparse PCs  
  ################################################### 
  
  
  d_gr_1y_temp = cbind(r2.models_matrix_unc[,1:2],r2.models_matrix_lasso[,2],r2.models_matrix_ridge[,2],r2.models_matrix_elnet[,2]); colnames(d_gr_1y_temp) = c("alpha","d_gr_1y_unc","d_gr_1y_lasso","d_gr_1y_ridge","d_gr_1y_elnet")
  ret_1y_temp = cbind(r2.models_matrix_unc[,c(1,5)],r2.models_matrix_lasso[,c(5)],r2.models_matrix_ridge[,c(5)],r2.models_matrix_elnet[,c(5)]); colnames(ret_1y_temp) = c("alpha","ret_1y_unc","ret_1y_lasso","ret_1y_ridge","ret_1y_elnet")
  
  fund_plt_1 = ggplot(data = d_gr_1y_temp[1:36,]) + theme_bw() +
    geom_line( mapping= aes(y= d_gr_1y_unc, x= alpha, color = "unconstrained"), linetype="solid",size=1.25, alpha=0.6 ) +
    geom_line( mapping= aes(y= d_gr_1y_lasso, x= alpha, color = "lasso"), linetype="solid",size=1.25, alpha=0.6 ) +
    geom_line( mapping= aes(y= d_gr_1y_elnet, x= alpha, color = "elastic net"), linetype="solid",size=1.25, alpha=0.6 ) +
    geom_line( mapping= aes(y= d_gr_1y_ridge, x= alpha, color = "ridge"), linetype="solid",size=1.25, alpha=0.6 ) +
    labs(color = '')+ 
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    scale_color_manual(values = c(
      'unconstrained' = 'blue',
      'lasso' = 'darkred',
      'elastic net' = 'darkgoldenrod1',
      "ridge" = 'darkgreen')) +
    xlab(expression(lambda[2])) + ylab("in-sample R2 dividend growth") + ylim(c(0.60,0.95)) +
    theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))
  
  fund_plt_2 = ggplot(data = ret_1y_temp[1:36,]) + theme_bw() +
    geom_line( mapping= aes(y= ret_1y_unc, x= alpha, color = "unconstrained"), linetype="solid",size=1.25, alpha=0.6 ) +
    geom_line( mapping= aes(y= ret_1y_lasso, x= alpha, color = "lasso"), linetype="solid",size=1.25, alpha=0.6 ) +
    geom_line( mapping= aes(y= ret_1y_elnet, x= alpha, color = "elastic net"), linetype="solid",size=1.25, alpha=0.6 ) +
    geom_line( mapping= aes(y= ret_1y_ridge, x= alpha, color = "ridge"), linetype="solid",size=1.25, alpha=0.6 ) +
    labs(color = '')+ 
    geom_vline(xintercept = 0.0002037, linetype = "dashed", color = "black", size = 1) +
    scale_color_manual(values = c(
      'unconstrained' = 'blue',
      'lasso' = 'darkred',
      'elastic net' = 'darkgoldenrod1',
      "ridge" = 'darkgreen')) +
    xlab(expression(lambda[2])) + ylab("in-sample R2 return") + ylim(c(0.10,0.55)) + 
    theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))
  
  ggarrange(fund_plt_1, fund_plt_2, nrow = 1, ncol = 2, common.legend = T)
  
  
  ###########################################################
  #### Figure 6 - In-sample $R_{is}^2$ with specific lambda_2
  ###########################################################
  
  source(file=paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\estimate_model_in_sample_v6_always_full.R"), chdir = T)
  

    var_insample = estimate_model_in_sample(data = df.freq, data.pc= df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, 
                                            time.col = "quarter_id", data.freq = freq_var_data, lags = lags, model = "all_models", pca = T, n_pc = n_pc,
                                            where.funcs = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\", 
                                            optional.args = list(sep.lambdas=sep_lambdas, fit.intercept = fit_intercept, pen.search = pen_search), 
                                            sparse.pca = sparse_pca, alpha.pca = optimal_lambda_2_is, beta.pca = beta_pca, t1 = t1, t2 = t2, extra = NA)
    
    r2.models_unc   <- var_insample$r2.is_uncon; print(r2.models_unc)
    r2.models_lasso <- var_insample$r2.is_lasso; print(r2.models_lasso)
    r2.models_ridge <- var_insample$r2.is_ridge; print(r2.models_ridge)
    r2.models_elnet <- var_insample$r2.is_elnet; print(r2.models_elnet)
    
    
    r2.models_unc_adj   <- var_insample$r2.is_uncon_adj; #print(r2.models_unc_adj)
    r2.models_lasso_adj <- var_insample$r2.is_lasso_adj; #print(r2.models_lasso_adj)
    r2.models_ridge_adj <- var_insample$r2.is_ridge_adj; #print(r2.models_ridge_adj)
    r2.models_elnet_adj <- var_insample$r2.is_elnet_adj; #print(r2.models_elnet_adj)  
    
    
  # ##### Table 4 - Coefficients Estimates
  
    coef_is_unc_list_is = var_insample$coef_unc_list
    coef_is_lasso_list_is = var_insample$coef_lasso_list
    coef_is_ridge_list_is = var_insample$coef_ridge_list
    coef_is_elnet_list_is = var_insample$coef_elnet_list
    
  # Fitted values
  
    vars_unc_list_is   = var_insample$vars_unc_list
    vars_lasso_list_is = var_insample$vars_lasso_list
    vars_ridge_list_is = var_insample$vars_ridge_list
    vars_elnet_list_is = var_insample$vars_elnet_list
  
  
    cor(vars_unc_list_is$profitability[vars_unc_list_is$profitability[,'quarter_id']>= "2003-12-31" & vars_unc_list_is$profitability[,'quarter_id']<= "2015-12-31",'d_gr_1y'], 
        vars_unc_list_is$profitability[vars_unc_list_is$profitability[,'quarter_id']>= "2003-12-31" & vars_unc_list_is$profitability[,'quarter_id']<= "2015-12-31",'d_gr_1y_pred'])
    
    cor(vars_unc_list_is$profitability[vars_unc_list_is$profitability[,'quarter_id']>= "2003-12-31" & vars_unc_list_is$profitability[,'quarter_id']<= "2015-12-31",'ret_1y'], 
        vars_unc_list_is$profitability[vars_unc_list_is$profitability[,'quarter_id']>= "2003-12-31" & vars_unc_list_is$profitability[,'quarter_id']<= "2015-12-31",'ret_1y_pred'])
    
    
    cor(vars_unc_list_is$profitability[vars_unc_list_is$profitability[,'quarter_id']>= "2003-12-31" & vars_unc_list_is$profitability[,'quarter_id']<= "2015-12-31",'e_gr_1y'], 
        vars_unc_list_is$profitability[vars_unc_list_is$profitability[,'quarter_id']>= "2003-12-31" & vars_unc_list_is$profitability[,'quarter_id']<= "2015-12-31",'e_gr_1y_pred'])
    
    cor(vars_unc_list_is$profitability[-41,'p_d'],vars_unc_list_is$profitability[-1,'d_gr_1y_pred'])
    cor(vars_unc_list_is$profitability[-41,'p_d'],vars_unc_list_is$profitability[-1,'d_gr_1y'])
    cor(vars_unc_list_is$profitability[-41,'p_d'],vars_unc_list_is$profitability[-1,'ret_1y_pred'])
    cor(vars_unc_list_is$profitability[-41,'p_d'],vars_unc_list_is$profitability[-1,'ret_1y'])
    
    
    cor(vars_unc_list_is$profitability[-42,'d_gr_1y_pred'],vars_unc_list_is$profitability[-42,'ret_1y_pred'])
  
    
    
  
  ## merge vars by models
  
    var_full   = merge(vars_unc_list_is$profitability,  vars_lasso_list_is$profitability, by = c("quarter_id", "month", DEPVAR, "SPC1_full","SPC2_full","profitability_pca_1","profitability_pca_2","e_gr_1y"), suffixes = c("_unc","_lasso"))
    
    var_full_2 = merge(vars_ridge_list_is$profitability,  vars_elnet_list_is$profitability, by = c("quarter_id", "month", DEPVAR, "SPC1_full","SPC2_full","profitability_pca_1","profitability_pca_2","e_gr_1y"), suffixes = c("_elnet","_ridge"))
    
    var_all_is = merge(var_full,var_full_2, by = c("quarter_id", "month", DEPVAR, "SPC1_full","SPC2_full","profitability_pca_1","profitability_pca_2","e_gr_1y"))
    
    rm(var_full, var_full_2)
    
    source(file=paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\plot_is_sp_fabio.R"), chdir = T)
    
    plot_is(var_all_is, freq_var_data)
    
    var_all_is[,c('d_gr_1y','d_gr_1y_pred_lasso','d_gr_1y_pred_ridge')]  
    var_all_is[,c('e_gr_1y','e_gr_1y_pred_lasso','e_gr_1y_pred_ridge')]  
    
  
  
    plot_1 = ggplot(data = var_all_is) + theme_bw() + labs(color = '')+ 
              geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
              xlab("Time") + ylab("dividend growth") +
              geom_line( mapping= aes(y= d_gr_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
              geom_line( mapping= aes(y= d_gr_1y_pred_elnet, x= quarter_id, color = "fitted values (elastic-net)"), linetype="twodash",size=1.25, alpha=0.6) +
              scale_color_manual(values = c(
                'data' = 'blue',
                'fitted values (elastic-net)' = 'red')) +
              theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    
   plot_2 =  ggplot(data = var_all_is) + theme_bw() + labs(color = '')+ 
              geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
              xlab("Time") + ylab("earnings growth") +
              geom_line( mapping= aes(y= e_gr_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
              geom_line( mapping= aes(y= e_gr_1y_pred_elnet, x= quarter_id, color = "fitted values (elastic-net)"), linetype="twodash",size=1.25, alpha=0.6) +
              scale_color_manual(values = c(
                'data' = 'blue',
                'fitted values (elastic-net)' = 'red')) +
              theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
   plot_3 =  ggplot(data = var_all_is) + theme_bw() + labs(color = '')+ 
             geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
             xlab("Time") + ylab("return") +
             geom_line( mapping= aes(y= ret_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
             geom_line( mapping= aes(y= ret_1y_pred_elnet, x= quarter_id, color = "fitted values (elastic-net)"), linetype="twodash",size=1.25, alpha=0.6) +
             scale_color_manual(values = c(
               'data' = 'blue',
               'fitted values (elastic-net)' = 'red')) +
             theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
   
   ggarrange(plot_1, plot_2, plot_3, nrow = 3, ncol = 1, common.legend = T)
 
 
  ######################################################################################################
  #  Out-of-sample
  ######################################################################################################
  
  ######################################################################################################
  # Dynamic model Out of Sample
  ######################################################################################################
  
  #source(file=paste0(path_functions,"estimate_model_rolling_v2.R"), chdir = T)
  #source(file=paste0(path_functions,"estimate_model_rolling_v4_temp.R"), chdir = T)
  #source(file=paste0(path_functions,"estimate_model_rolling_v5.R"), chdir = T)
 
  source(file="C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\estimate_model_rolling_v6.R", chdir = T)
  
  
  grid_sparse_pc = seq(0.00000,0.0004, length.out = 41)
  grid_sparse_pc_len = length(grid_sparse_pc)
  mod_r2 = 12
  
  
  r2.models_matrix_div_unc = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pd_unc  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pe_unc  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_ret_unc = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  
  r2.models_matrix_div_lasso = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pd_lasso  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pe_lasso  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_ret_lasso = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  
  r2.models_matrix_div_ridge = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pd_ridge  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pe_ridge  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_ret_ridge = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  
  r2.models_matrix_div_elnet = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pd_elnet  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_pe_elnet  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  r2.models_matrix_ret_elnet = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
  
  
  r2.models_matrix_div_unc[,1]   = r2.models_matrix_pd_unc[,1]   = r2.models_matrix_pe_unc[,1]   = r2.models_matrix_ret_unc[,1]   = 
  r2.models_matrix_div_lasso[,1] = r2.models_matrix_pd_lasso[,1] = r2.models_matrix_pe_lasso[,1] = r2.models_matrix_ret_lasso[,1] = 
  r2.models_matrix_div_ridge[,1] = r2.models_matrix_pd_ridge[,1] = r2.models_matrix_pe_ridge[,1] = r2.models_matrix_ret_ridge[,1] = 
  r2.models_matrix_div_elnet[,1] = r2.models_matrix_pd_elnet[,1] = r2.models_matrix_pe_elnet[,1] = r2.models_matrix_ret_elnet[,1] = round(grid_sparse_pc,digits = 6)
  
  
colnames(r2.models_matrix_div_unc)[1:mod_r2] = colnames(r2.models_matrix_pd_unc)[1:mod_r2] = colnames(r2.models_matrix_pe_unc)[1:mod_r2] = colnames(r2.models_matrix_ret_unc)[1:mod_r2] =
colnames(r2.models_matrix_div_lasso)[1:mod_r2] = colnames(r2.models_matrix_pd_lasso)[1:mod_r2] = colnames(r2.models_matrix_pe_lasso)[1:mod_r2] = colnames(r2.models_matrix_ret_lasso)[1:mod_r2] =   
colnames(r2.models_matrix_div_ridge)[1:mod_r2] = colnames(r2.models_matrix_pd_ridge)[1:mod_r2] = colnames(r2.models_matrix_pe_ridge)[1:mod_r2] = colnames(r2.models_matrix_ret_ridge)[1:mod_r2] =    
colnames(r2.models_matrix_div_elnet)[1:mod_r2] = colnames(r2.models_matrix_pd_elnet)[1:mod_r2] = colnames(r2.models_matrix_pe_elnet)[1:mod_r2] = colnames(r2.models_matrix_ret_elnet)[1:mod_r2] =
    c("alpha","DEPVAR","capitalization","efficiency","financial_soundness","liquidity","profitability", "solvency", "valuation", "equity_premium", "other","full")
  

n_pc = 2
i=18
t1 = 0.5
pen_search = "Rolling"; pen_search = "LOO"
DEPVAR = c("d_gr_1y","p_d","e_d","ret_1y"); DEPVAR = c("d_gr_1y","p_d","p_e","ret_1y")
#splitratio = 0.65
#for (j in 1:grid_sparse_pc_len) {


for (i in seq(1,grid_sparse_pc_len, by = 1)) {
  
  
  model.estimates_lasso = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR,add.vars = CHARS,pca.cat.dict = CATEGORIES, 
                                         model = "profitability", type = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data, 
                                         split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\",
                                         optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search), 
                                         sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2)
  
  r2.models <- model.estimates_lasso$r2; print(r2.models)
  
  r2.models_matrix_div_lasso[i,2:mod_r2] = r2.models[,2]
  r2.models_matrix_pd_lasso[i,2:mod_r2]  = r2.models[,3]
  r2.models_matrix_pe_lasso[i,2:mod_r2]  = r2.models[,4]
  r2.models_matrix_ret_lasso[i,2:mod_r2] = r2.models[,5]
  
  
  # var_hat_lasso = model.estimates$real.pred.matrix
  
  model.estimates_ridge = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR,add.vars = CHARS,pca.cat.dict = CATEGORIES, 
                                         model = "profitability", type = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data, 
                                         split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\", 
                                         optional.args = list(sep.lambdas=sep_lambdas,fit.intercept=fit_intercept, alpha= 0.0000, pen.search = pen_search),
                                         sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2)
  
  r2.models <- model.estimates_ridge$r2; print(r2.models)
  
  r2.models_matrix_div_ridge[i,2:mod_r2] = r2.models[,2]
  r2.models_matrix_pd_ridge[i,2:mod_r2]  = r2.models[,3]
  r2.models_matrix_pe_ridge[i,2:mod_r2]  = r2.models[,4]
  r2.models_matrix_ret_ridge[i,2:mod_r2] = r2.models[,5]
  
  # var_hat_ridge = model.estimates$real.pred.matrix
  
  
  model.estimates_elnet = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, 
                                         model = "profitability", type = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data, 
                                         split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\",
                                         optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 0.5, pen.search = pen_search), 
                                         sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2)
  
  r2.models <- model.estimates_elnet$r2; print(r2.models)
  
  r2.models_matrix_div_elnet[i,2:mod_r2] = r2.models[,2]
  r2.models_matrix_pd_elnet[i,2:mod_r2]  = r2.models[,3]
  r2.models_matrix_pe_elnet[i,2:mod_r2]  = r2.models[,4]
  r2.models_matrix_ret_elnet[i,2:mod_r2] = r2.models[,5]
  

}


for (i in seq(1,grid_sparse_pc_len, by = 1)) {
  
    model.estimates_unc = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES,
                                         model = "profitability", type = "unconstrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data,
                                         split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\", 
                                         optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search), 
                                         sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2)
    
    r2.models <- model.estimates_unc$r2; print(r2.models)
    
    r2.models_matrix_div_unc[i,2:mod_r2] = r2.models[,2]
    r2.models_matrix_pd_unc[i,2:mod_r2]  = r2.models[,3]
    r2.models_matrix_pe_unc[i,2:mod_r2]  = r2.models[,4]
    r2.models_matrix_ret_unc[i,2:mod_r2] = r2.models[,5]
    
    # var_hat_unc = model.estimates$real.pred.matrix
    
}

    


d_gr_1y_temp = cbind(r2.models_matrix_div_unc[,c(1,7)],r2.models_matrix_div_lasso[,7],r2.models_matrix_div_ridge[,7],r2.models_matrix_div_elnet[,7]); colnames(d_gr_1y_temp) = c("alpha","d_gr_1y_unc","d_gr_1y_lasso","d_gr_1y_ridge","d_gr_1y_elnet")
ret_1y_temp = cbind(r2.models_matrix_ret_unc[,c(1,7)],r2.models_matrix_ret_lasso[,7],r2.models_matrix_ret_ridge[,7],r2.models_matrix_ret_elnet[,7]); colnames(ret_1y_temp) = c("alpha","ret_1y_unc","ret_1y_lasso","ret_1y_ridge","ret_1y_elnet")

fund_plt_1 = ggplot(data = d_gr_1y_temp[1:36,]) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_1y_unc, x= alpha, color = "unconstrained"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= d_gr_1y_lasso, x= alpha, color = "lasso"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= d_gr_1y_elnet, x= alpha, color = "elastic net"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= d_gr_1y_ridge, x= alpha, color = "ridge"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_vline(xintercept = 0.0001504, linetype = "dashed", color = "black", size = 1) + 
  labs(color = '')+ 
  scale_color_manual(values = c(
    'unconstrained' = 'blue',
    'lasso' = 'darkred',
    'elastic net' = 'darkgoldenrod1',
    "ridge" = 'darkgreen')) +
  xlab(expression(lambda[2])) + ylab("Out-of-sample R2 dividend growth") + ylim(c(0.2,0.5)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))

fund_plt_2 = ggplot(data = ret_1y_temp[1:36,]) + theme_bw() +
  geom_line( mapping= aes(y= ret_1y_unc, x= alpha, color = "unconstrained"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= ret_1y_lasso, x= alpha, color = "lasso"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= ret_1y_elnet, x= alpha, color = "elastic net"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= ret_1y_ridge, x= alpha, color = "ridge"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_vline(xintercept = 0.0001504, linetype = "dashed", color = "black", size = 1) + 
  labs(color = '')+ 
  scale_color_manual(values = c(
    'unconstrained' = 'blue',
    'lasso' = 'darkred',
    'elastic net' = 'darkgoldenrod1',
    "ridge" = 'darkgreen')) +
  xlab(expression(lambda[2])) + ylab("Out-of-sample R2 return") + ylim(c(-0.50,0.2)) + 
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))

ggarrange(fund_plt_1, fund_plt_2, nrow = 1, ncol = 2, common.legend = T)    


###########################################################
#### Cross-validation Out of Sample
###########################################################

source(file=paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\cross_validation_spca.R"), chdir = T)

sparsity_params = seq(0.000125,0.00025,length=51)

cv_lamda = cross_validation_spca(data = df.freq[1:21,], dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, sparsity.params = sparsity_params, beta.pca = 0, n_pc = n_pc)

optimal_lambda_2_oos = cv_lamda$optimal_lambda

cv_lamda$optimal_lambda


plot_1 <- ggplot(data=cv_lamda$cv_errors) + 
          theme_bw() +
          geom_line(mapping=aes(y=Mean_Squared_Error, x=Sparsity_Parameter), linetype="solid", size=1.15, alpha=0.6) +
          geom_point(mapping=aes(y=Mean_Squared_Error, x=Sparsity_Parameter), size=3, alpha=0.8) +  # Add points
          labs(color = '') + 
          xlab(bquote("Sparsity Parameter " ~ lambda[2])) +  # Combine text and Greek letter with subscript
          ylab("Mean Squared Error") +
          theme(legend.position = c(0.25, 0.80), legend.text = element_text(size=13))

ggarrange(plot_1,  nrow = 1, ncol = 1, common.legend = F)


############################################################
#### Out-of-sample $R_{oos}^2$ with cross-validated lambda_2
############################################################
  

source(file="C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\estimate_model_rolling_v6.R", chdir = T)

model.estimates_lasso = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR,add.vars = CHARS,pca.cat.dict = CATEGORIES, 
                                       model = "all_models", type = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data, 
                                       split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\",
                                       optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search), 
                                       sparse.pca = T, alpha.pca = optimal_lambda_2_oos, beta.pca = 0, t1 = t1, t2 = t2)

r2.models <- model.estimates_lasso$r2; print(r2.models)

r2.models_matrix_div_lasso[i,2:mod_r2] = r2.models[,2]
r2.models_matrix_pd_lasso[i,2:mod_r2]  = r2.models[,3]
r2.models_matrix_pe_lasso[i,2:mod_r2]  = r2.models[,4]
r2.models_matrix_ret_lasso[i,2:mod_r2] = r2.models[,5]


# var_hat_lasso = model.estimates$real.pred.matrix

model.estimates_ridge = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR,add.vars = CHARS,pca.cat.dict = CATEGORIES, 
                                       model = "all_models", type = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data, 
                                       split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\", 
                                       optional.args = list(sep.lambdas=sep_lambdas,fit.intercept=fit_intercept, alpha= 0.0000, pen.search = pen_search),
                                       sparse.pca = T, alpha.pca = optimal_lambda_2_oos, beta.pca = 0, t1 = t1, t2 = t2)

r2.models <- model.estimates_ridge$r2; print(r2.models)

r2.models_matrix_div_ridge[i,2:mod_r2] = r2.models[,2]
r2.models_matrix_pd_ridge[i,2:mod_r2]  = r2.models[,3]
r2.models_matrix_pe_ridge[i,2:mod_r2]  = r2.models[,4]
r2.models_matrix_ret_ridge[i,2:mod_r2] = r2.models[,5]

# var_hat_ridge = model.estimates$real.pred.matrix


model.estimates_elnet = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, 
                                       model = "all_models", type = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data, 
                                       split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\",
                                       optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 0.5, pen.search = pen_search), 
                                       sparse.pca = T, alpha.pca = optimal_lambda_2_oos, beta.pca = 0, t1 = t1, t2 = t2)

r2.models <- model.estimates_elnet$r2; print(r2.models)

r2.models_matrix_div_elnet[i,2:mod_r2] = r2.models[,2]
r2.models_matrix_pd_elnet[i,2:mod_r2]  = r2.models[,3]
r2.models_matrix_pe_elnet[i,2:mod_r2]  = r2.models[,4]
r2.models_matrix_ret_elnet[i,2:mod_r2] = r2.models[,5]



model.estimates_unc = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES,
                                     model = "all_models", type = "unconstrained", time.col = "quarter_id", n_pc = n_pc, data.freq = freq_var_data,
                                     split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = "C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\functions_estimation\\", 
                                     optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search), 
                                     sparse.pca = T, alpha.pca = optimal_lambda_2_oos, beta.pca = 0, t1 = t1, t2 = t2)

r2.models <- model.estimates_unc$r2; print(r2.models)

r2.models_matrix_div_unc[i,2:mod_r2] = r2.models[,2]
r2.models_matrix_pd_unc[i,2:mod_r2]  = r2.models[,3]
r2.models_matrix_pe_unc[i,2:mod_r2]  = r2.models[,4]
r2.models_matrix_ret_unc[i,2:mod_r2] = r2.models[,5]



model.estimates_lasso$real.pred.matrix$profitability[,'e_gr_1y'] = model.estimates_unc$real.pred.matrix$profitability[,'e_gr_1y'] = model.estimates_ridge$real.pred.matrix$profitability[,'e_gr_1y'] = model.estimates_elnet$real.pred.matrix$profitability[,'e_gr_1y'] = df.freq[df.freq[,'month_id']>200012,'e_gr_1y'] 


model.estimates_lasso$real.pred.matrix$profitability[,'e_gr_1y_pred'] = model.estimates_lasso$real.pred.matrix$profitability[,'d_gr_1y_pred'] + model.estimates_lasso$real.pred.matrix$profitability[,'p_d_pred'] - model.estimates_lasso$real.pred.matrix$profitability[,'p_e_pred'] + df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_e']  - df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_d']

model.estimates_ridge$real.pred.matrix$profitability[,'e_gr_1y_pred'] = model.estimates_ridge$real.pred.matrix$profitability[,'d_gr_1y_pred'] + model.estimates_ridge$real.pred.matrix$profitability[,'p_d_pred'] - model.estimates_ridge$real.pred.matrix$profitability[,'p_e_pred'] + df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_e']  - df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_d']

model.estimates_unc$real.pred.matrix$profitability[,'e_gr_1y_pred']   = model.estimates_unc$real.pred.matrix$profitability[,'d_gr_1y_pred'] + model.estimates_unc$real.pred.matrix$profitability[,'p_d_pred'] - model.estimates_unc$real.pred.matrix$profitability[,'p_e_pred'] + df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_e']  - df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_d']

model.estimates_elnet$real.pred.matrix$profitability[,'e_gr_1y_pred'] = model.estimates_elnet$real.pred.matrix$profitability[,'d_gr_1y_pred'] + model.estimates_elnet$real.pred.matrix$profitability[,'p_d_pred'] - model.estimates_elnet$real.pred.matrix$profitability[,'p_e_pred'] + df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_e']  - df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_d']



######################################################


vars_unc_list_oos   = model.estimates_unc$real.pred.matrix
vars_lasso_list_oos = model.estimates_lasso$real.pred.matrix
vars_ridge_list_oos = model.estimates_ridge$real.pred.matrix
vars_elnet_list_oos = model.estimates_elnet$real.pred.matrix
  
  
  
cor(vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2001-12-31",'d_gr_1y'], 
    vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2001-12-31",'d_gr_1y_pred'])

cor(vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2001-12-31",'e_gr_1y'], 
    vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2001-12-31",'e_gr_1y_pred'])

cor(vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2001-12-31",'ret_1y'], 
    vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2001-12-31",'ret_1y_pred'])
  
cor(vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2003-12-31" & vars_lasso_list_oos$profitability[,'quarter_id']<= "2015-12-31",'ret_1y'], 
    vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "2003-12-31" & vars_lasso_list_oos$profitability[,'quarter_id']<= "2015-12-31",'ret_1y_pred'])
  


cor(vars_elnet_list_oos$profitability[vars_elnet_list_oos$profitability[,'quarter_id']>= "2001-12-31",'d_gr_1y'], 
    vars_elnet_list_oos$profitability[vars_elnet_list_oos$profitability[,'quarter_id']>= "2001-12-31",'d_gr_1y_pred'])

cor(vars_elnet_list_oos$profitability[vars_elnet_list_oos$profitability[,'quarter_id']>= "2001-12-31",'e_gr_1y'], 
    vars_elnet_list_oos$profitability[vars_elnet_list_oos$profitability[,'quarter_id']>= "2001-12-31",'e_gr_1y_pred'])

cor(vars_elnet_list_oos$profitability[vars_elnet_list_oos$profitability[,'quarter_id']>= "2001-12-31",'ret_1y'], 
    vars_elnet_list_oos$profitability[vars_elnet_list_oos$profitability[,'quarter_id']>= "2001-12-31",'ret_1y_pred'])



  
mean(vars_lasso_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "1999-12-31",'d_gr_1y'])
mean(vars_unc_list_oos$profitability[vars_lasso_list_oos$profitability[,'quarter_id']>= "1999-12-31",'d_gr_1y'])
  
  ## merge vars by models
  
  var_full_oos   = merge(
                          x = vars_unc_list_oos$profitability,
                          y = vars_lasso_list_oos$profitability,
                          by = c("quarter_id", DEPVAR, "e_gr_1y", "SPC1_full", "SPC2_full", "PC1_profitability", "PC2_profitability"),
                          suffixes = c("_unc", "_lasso")
                        )
  
  var_full_oos_2 = merge(vars_elnet_list_oos$profitability,vars_ridge_list_oos$profitability, by = c("quarter_id", DEPVAR, "e_gr_1y", "SPC1_full","SPC2_full","PC1_profitability","PC2_profitability"), suffixes = c("_elnet","_ridge"))
  
  var_full_oos = merge(var_full_oos,var_full_oos_2, by = c("quarter_id", DEPVAR, "e_gr_1y", "SPC1_full","SPC2_full","PC1_profitability","PC2_profitability"))
  
  rm(var_full_oos_2)
  
  #var_full_oos[,'e_gr_1y'] = df.freq[df.freq[,'month_id']>200012,'e_gr_1y'] 
  #var_full_oos[,'e_gr_1y_pred_lasso'] = var_full_oos[,'d_gr_1y_pred_lasso'] + var_full_oos[,'p_d_pred_lasso'] - var_full_oos[,'p_e_pred_lasso'] + df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_e']  - df.freq[df.freq[,'month_id']>199912 & df.freq[,'month_id']<202112,'p_d']
  
  
  recessions.trim = subset(recessions.df, Peak >= min(var_full_oos$quarter_id))
  
  plot_1 = ggplot(data = var_full_oos) + theme_bw() + labs(color = '')+ 
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    xlab("Time") + ylab("dividend growth") +
    geom_line( mapping= aes(y= d_gr_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
    geom_line( mapping= aes(y= d_gr_1y_pred_elnet, x= quarter_id, color = "fitted values (elastic-net)"), linetype="twodash",size=1.25, alpha=0.6) +
    scale_color_manual(values = c(
      'data' = 'blue',
      'fitted values (elastic-net)' = 'red')) +
    theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
  
  
  plot_2 =  ggplot(data = var_full_oos) + theme_bw() + labs(color = '')+ 
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    xlab("Time") + ylab("earnings growth") +
    geom_line( mapping= aes(y= e_gr_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
    geom_line( mapping= aes(y= e_gr_1y_pred_elnet, x= quarter_id, color = "fitted values (elastic-net)"), linetype="twodash",size=1.25, alpha=0.6) +
    scale_color_manual(values = c(
      'data' = 'blue',
      'fitted values (elastic-net)' = 'red')) +
    theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
  
  plot_3 =  ggplot(data = var_full_oos) + theme_bw() + labs(color = '')+ 
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    xlab("Time") + ylab("return") +
    geom_line( mapping= aes(y= ret_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
    geom_line( mapping= aes(y= ret_1y_pred_elnet, x= quarter_id, color = "fitted values (elastic-net)"), linetype="twodash",size=1.25, alpha=0.6) +
    scale_color_manual(values = c(
      'data' = 'blue',
      'fitted values (elastic-net)' = 'red')) +
    theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
  
  ggarrange(plot_1, plot_2, plot_3, nrow = 3, ncol = 1, common.legend = T)
  
  
  
  
  
  
  #########################################
  ### De La O & Meyers JoF 2021 Comparison
  #########################################
  
  de_la_o <- read_xlsx("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\data\\de_lao_mayers_jof.xlsx") %>%  mutate_if(is.character, as.numeric) %>% as.data.frame()
  
  df.q.freq = df.all.freq[df.all.freq[,'month'] %in% c(3,6,9,12),]
  

  df.q.freq  = na.omit(df.q.freq[,c('quarter_id',"month_id",'month',"d_gr_1y_lead","e_gr_1y_lead",'ret_1y_lead',DEPVAR,"spc1_full","spc2_full","pc1_full","pc2_full",'pc1_capitalization','pc2_capitalization',"pc1_efficiency", "pc2_efficiency","pc1_financial_soundness","pc2_financial_soundness","pc1_liquidity","pc2_liquidity","pc1_profitability","pc2_profitability","pc1_solvency","pc2_solvency","pc1_valuation","pc2_valuation","pc1_equity_premium","pc2_equity_premium","pc1_other","pc2_other", "e_d","e_gr_1y")])
  
  df.q.freq[,'Year'] = year(as.Date(paste0(df.q.freq[,'month_id'], "01"), format = "%Y%m%d"))
  
  
  df.q.freq$quarter_id =  ymd(paste0(df.q.freq$month_id, "01")) + months(1) - days(1)

    
  de_la_o = merge(de_la_o, df.q.freq, by = c("Year", "month"), all = T )
  
  
  
  colnames(de_la_o)[4:7] = c("Expected_one_year_log_earnings_growth", "Realized_next_year_log_earnings_growth", "Expected_one_year_log_dividend_growth", "Realized_next_year_log_dividend_growth")
  
  cor(de_la_o[,c('Realized_next_year_log_earnings_growth', 'e_gr_1y_lead' )], use = "pairwise.complete.obs"); cor(de_la_o[,c('Realized_next_year_log_dividend_growth' , 'd_gr_1y_lead')], use = "pairwise.complete.obs")
  
  
  
  
  exp_delao_ear_full =   lm(Expected_one_year_log_earnings_growth ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(exp_delao_ear_full)
  exp_delao_ear_DEPVAR = lm(Expected_one_year_log_earnings_growth ~ d_gr_1y + p_d + p_e + ret_1y, data = de_la_o) ; summary(exp_delao_ear_DEPVAR)
  exp_delao_ear_pd_pe =  lm(Expected_one_year_log_earnings_growth ~ p_d + p_e, data = de_la_o); summary(exp_delao_ear_pd_pe)
  exp_delao_ear_ed =     lm(Expected_one_year_log_earnings_growth ~ e_d, data = de_la_o); summary(exp_delao_ear_ed)
  
  exp_delao_div_full =   lm(Expected_one_year_log_dividend_growth ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(exp_delao_div_full)
  exp_delao_div_DEPVAR = lm(Expected_one_year_log_dividend_growth ~ d_gr_1y + p_d + p_e + ret_1y, data = de_la_o); summary(exp_delao_div_DEPVAR)
  exp_delao_div_pd_pe =  lm(Expected_one_year_log_dividend_growth ~ p_d + p_e, data = de_la_o); summary(exp_delao_div_pd_pe)
  exp_delao_div_ed =     lm(Expected_one_year_log_dividend_growth ~ e_d, data = de_la_o); summary(exp_delao_div_ed)
  
  
  
  
  act_delao_ear_full =   lm(Realized_next_year_log_earnings_growth ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(act_delao_ear_full)
  act_delao_ear_DEPVAR = lm(Realized_next_year_log_earnings_growth ~ d_gr_1y + p_d + p_e + ret_1y, data = de_la_o); summary(act_delao_ear_DEPVAR)
  act_delao_ear_pd_pe =  lm(Realized_next_year_log_earnings_growth ~ p_d + p_e, data = de_la_o); summary(act_delao_ear_pd_pe)
  act_delao_ear_ed =     lm(Realized_next_year_log_earnings_growth ~ e_d, data = de_la_o); summary(act_delao_ear_ed)
  
  act_delao_div_full =   lm(Realized_next_year_log_dividend_growth ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(act_delao_div_full)
  act_delao_div_DEPVAR = lm(Realized_next_year_log_dividend_growth ~ d_gr_1y + p_d + p_e + ret_1y, data = de_la_o); summary(act_delao_div_DEPVAR)
  act_delao_div_pd_pe =  lm(Realized_next_year_log_dividend_growth ~ p_d + p_e, data = de_la_o); summary(act_delao_div_pd_pe)
  act_elao_div_ed =      lm(Realized_next_year_log_dividend_growth ~ e_d, data = de_la_o); summary(act_elao_div_ed)
  
  summary(exp_delao_ear_full); summary(exp_delao_ear_pd_pe); summary(act_delao_ear_full);  summary(act_delao_ear_pd_pe) 
  summary(exp_delao_div_full); summary(exp_delao_div_pd_pe); summary(act_delao_div_full);  summary(act_delao_div_pd_pe)
  
  
  models_ols = list(exp_delao_ear_full,exp_delao_ear_DEPVAR,exp_delao_ear_pd_pe,exp_delao_ear_ed,act_delao_ear_full,act_delao_ear_DEPVAR,act_delao_ear_pd_pe,act_delao_ear_ed)
  
  
  de_la_o <- de_la_o[!apply(de_la_o[, c("Realized_next_year_log_earnings_growth","Expected_one_year_log_earnings_growth")], 1, function(row) any(is.na(row))), ]
  
  de_la_o$Expected_one_year_log_earnings_growth_fitted = c(rep(NA,16), predict(exp_delao_ear_full)); 
  de_la_o$Realized_next_year_log_earnings_growth_fitted = c(rep(NA,16), predict(act_delao_ear_full)); 
  de_la_o$Expected_one_year_log_dividend_growth_fitted = c(rep(NA,108), predict(exp_delao_div_full)); 
  de_la_o$Realized_next_year_log_dividend_growth_fitted = c(rep(NA,108), predict(act_delao_div_full)); 
  
  
  
  
  recessions.trim = subset(recessions.df, Peak >= min(de_la_o$quarter_id))

  ggplot(data=de_la_o) + theme_bw() +
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    geom_line( mapping= aes(y= Realized_next_year_log_earnings_growth, x= quarter_id, color = "actual De La O & Myers"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= e_gr_1y_lead, x= quarter_id, color = "Shiller"), linetype="solid",size=1.15, alpha=0.6) +
    labs(color = '') + 
    scale_color_manual(values = c(
      'actual De La O & Myers' = 'blue',
      'Shiller' = 'darkred')) +
    xlab("time") + ylab("earning growth") +
    theme(legend.position = c(0.25,0.80), legend.text = element_text(size=13)) 
  
  
  
  recessions.trim = subset(recessions.df, Peak >= min(de_la_o$quarter_id[c(109:192)]))
  
  
   ggplot(data=de_la_o[109:159,]) + theme_bw() +
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    geom_line( mapping= aes(y= Realized_next_year_log_dividend_growth, x= quarter_id, color = "actual De La O & Myers"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= d_gr_1y_lead, x= quarter_id, color = "Shiller"), linetype="solid",size=1.15, alpha=0.6) +
    labs(color = '') + 
    scale_color_manual(values = c(
      'Shiller' = 'darkred',
      'actual De La O & Myers' = 'darkgreen')) +
    xlab("time") + ylab("dividend growth") +
    theme(legend.position = c(0.80,0.25), legend.text = element_text(size=13)) 
   
   
   
   
   recessions.trim = subset(recessions.df, Peak >= min(de_la_o$quarter_id[17:159]))
   
   
  plot_delao_ear = ggplot(data=de_la_o) + theme_bw() +
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    geom_line( mapping= aes(y= Expected_one_year_log_earnings_growth, x= quarter_id, color = "IBES analysts expectations"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= Realized_next_year_log_earnings_growth_fitted, x= quarter_id, color = "model-implied expectations"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= Realized_next_year_log_earnings_growth, x= quarter_id, color = "actual"), linetype="solid",size=1.15, alpha=0.6) +
    labs(color = '') + 
    scale_color_manual(values = c(
      "IBES analysts expectations" = 'darkgreen',
      'model-implied expectations' = 'darkred',
      'actual' = 'blue')) +
    xlab("time") + ylab("earning growth") +
    theme(legend.position = c(0.25,0.80), legend.text = element_text(size=13)) 
  
  
  
  
  
  recessions.trim = subset(recessions.df, Peak >= min(de_la_o$quarter_id[c(109:159)]))
  
 
  
  plot_delao_div = ggplot(data=de_la_o[109:160,]) + theme_bw() +
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    geom_line( mapping= aes(y= Expected_one_year_log_dividend_growth, x= quarter_id, color = "IBES analysts expectations"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= Realized_next_year_log_dividend_growth_fitted, x= quarter_id, color = "model-implied expectations"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= Realized_next_year_log_dividend_growth, x= quarter_id, color = "actual"), linetype="solid",size=1.15, alpha=0.6) +
    labs(color = '') + 
    scale_color_manual(values = c(
      "IBES analysts expectations" = 'darkgreen',
      'model-implied expectations' = 'darkred',
      'actual' = 'blue')) +
    xlab("time") + ylab("dividend growth") +
    theme(legend.position = c(0.25,0.80), legend.text = element_text(size=13)) 
  
  ggarrange(plot_delao_ear, plot_delao_div, nrow = 2,ncol = 1, common.legend = T)
  
  
  
  
  
  

  
  library(stargazer)

  exp_delao_ear_full#,delao_ear_DEPVAR,delao_ear_pd_pe,delao_ear_ed, #delao_div_full,delao_div_DEPVAR,delao_div_pd_pe,delao_div_ed,
  
  stargazer(models_ols,#delao_ear_DEPVAR,delao_ear_pd_pe,delao_ear_ed, #delao_div_full,delao_div_DEPVAR,delao_div_pd_pe,delao_div_ed,
            title = "Regression Results",  # Table title
            out = "regression_table.tex"   # Output the table to a .tex file
  )
  
  
  
  
  
  
  act_real_delao_div =   lm(Realized_next_year_log_dividend_growth ~ Expected_one_year_log_dividend_growth, data = de_la_o); summary(act_real_delao_div)
  act_real_delao_ear =   lm(Realized_next_year_log_earnings_growth ~ Expected_one_year_log_earnings_growth, data = de_la_o); summary(act_real_delao_ear)
  
  
  de_la_o$fe_div_stat = (de_la_o$Realized_next_year_log_dividend_growth - de_la_o$Realized_next_year_log_dividend_growth_fitted)
  de_la_o$fe_div_for = (de_la_o$Realized_next_year_log_dividend_growth - de_la_o$Expected_one_year_log_dividend_growth)
  de_la_o$fe_ear_stat = (de_la_o$Realized_next_year_log_earnings_growth - de_la_o$Realized_next_year_log_earnings_growth_fitted)
  de_la_o$fe_ear_for = (de_la_o$Realized_next_year_log_earnings_growth - de_la_o$Expected_one_year_log_earnings_growth)
  
  
  recessions.trim = subset(recessions.df, Peak >= min(de_la_o$quarter_id[17:159]))
  
  plot_delao_fe_ear = ggplot(data=de_la_o) + theme_bw() +
                      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
                      geom_line( mapping= aes(y= fe_ear_for, x= quarter_id, color = "Forecast error IBES analysts"), linetype="solid",size=1.15, alpha=0.6) +
                      geom_line( mapping= aes(y= fe_ear_stat, x= quarter_id, color = "Forecast error model-implied"), linetype="solid",size=1.15, alpha=0.6) +
                      labs(color = '') + 
                      scale_color_manual(values = c(
                        "Forecast error IBES analysts" = 'darkgreen',
                        'Forecast error model-implied' = 'darkred')) +
                      xlab("time") + ylab("Forecast error earnings growth") +
                      theme(legend.position = c(0.25,0.80), legend.text = element_text(size=13)) 
  
  
  recessions.trim = subset(recessions.df, Peak >= min(de_la_o$quarter_id[c(109:159)]))
  
  
  plot_delao_fe_div = ggplot(data=de_la_o[109:160,]) + theme_bw() +
                      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
                      geom_line( mapping= aes(y= fe_div_for, x= quarter_id, color = "Forecast error IBES analysts"), linetype="solid",size=1.15, alpha=0.6) +
                      geom_line( mapping= aes(y= fe_div_stat, x= quarter_id, color = "Forecast error model-implied"), linetype="solid",size=1.15, alpha=0.6) +
                      labs(color = '') + 
                      scale_color_manual(values = c(
                        "Forecast error IBES analysts" = 'darkgreen',
                        'Forecast error model-implied' = 'darkred')) +
                      xlab("time") + ylab("Forecast error dividend growth") +
                      theme(legend.position = c(0.25,0.80), legend.text = element_text(size=13)) 
  
  
  ggarrange(plot_delao_fe_ear, plot_delao_fe_div, nrow = 2,ncol = 1, common.legend = T)
  
  
  
  fe_ear_for_reg_full    = lm(fe_ear_for ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(fe_ear_for_reg_full)
  fe_ear_for_reg_base    = lm(fe_ear_for ~ d_gr_1y + p_d + p_e + ret_1y, data = de_la_o); summary(fe_ear_for_reg_base)
  fe_ear_for_reg_only_pc = lm(fe_ear_for ~ spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(fe_ear_for_reg_only_pc)
  
  
  fe_div_for_reg_full    = lm(fe_div_for ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(fe_div_for_reg_full)
  fe_div_for_reg_base    = lm(fe_div_for ~ d_gr_1y + p_d + p_e + ret_1y, data = de_la_o); summary(fe_div_for_reg_base)
  fe_div_for_reg_only_pc = lm(fe_div_for ~ spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = de_la_o); summary(fe_div_for_reg_only_pc)
  
  
  cor(de_la_o[,'spc1_full'], de_la_o[,'p_e'], use = "pairwise.complete.obs")

  
  
  
  
  
  #########################################
  ### Martin Svix
  #########################################
  
  
  svix <- read_csv("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\data\\output_2024\\svix_spx.csv") %>%  mutate_if(is.character, as.numeric) %>% as.data.frame()
  
  svix = svix[,c("date","12-mo")]
  
  colnames(svix)[colnames(svix) == "12-mo"] <- "m_12"
  
  svix$date = as.Date(svix$date, format = "%Y-%m-%d")

  svix_quarter <- svix %>%
                      mutate(quarter = quarter(date, with_year = TRUE)) %>%  # Create year-quarter identifier
                      group_by(quarter) %>%
                      slice_tail(n = 1) %>%  # Keep last row of each quarter
                      ungroup() %>% as.data.frame()
  
  svix_quarter$month_id <- as.numeric(format(svix_quarter$date, "%Y%m"))
  
  df.q.freq = merge(df.q.freq, svix_quarter, by = "month_id", all = T)
  
  svix_full =   lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.q.freq); summary(svix_full)
  svix_DEPVAR = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y, data = df.q.freq) ; summary(svix_DEPVAR)
  svix_pd_pe =  lm(m_12 ~ p_d + p_e, data = df.q.freq); summary(svix_pd_pe)
  svix_ed =     lm(m_12 ~ e_d, data = df.q.freq); summary(svix_ed)
  svix_full_spc =   lm(m_12 ~ spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.q.freq); summary(svix_full_spc)
  
  depvar_svix      = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y, data = df.q.freq);  summary(depvar_svix)$r.squared #R2 0.411
  full_svix        = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full, data = df.q.freq) ; summary(full_svix)$r.squared #R2 0.433 
  capit_svix       = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_capitalization + pc2_capitalization, data = df.q.freq) ; summary(capit_svix)$r.squared #R2 0.509
  effic_svix       = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_efficiency + pc2_efficiency, data = df.q.freq) ; summary(effic_svix)$r.squared #R2 0.442
  fin_sound_svix   = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_financial_soundness + pc2_financial_soundness, data = df.q.freq) ; summary(fin_sound_svix)$r.squared #R2 0.590 
  prof_svix        = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_profitability + pc2_profitability, data = df.q.freq) ; summary(prof_svix)$r.squared #R2 0.549
  liquid_svix      = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_liquidity + pc2_liquidity, data = df.q.freq) ; summary(liquid_svix)$r.squared #R2 0.500
  solven_svix      = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_solvency + pc2_solvency, data = df.q.freq) ; summary(solven_svix)$r.squared #R2 0.573 
  valuation_svix   = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_valuation + pc2_valuation, data = df.q.freq) ; summary(valuation_svix)$r.squared #R2 0.556  
  equity_prem_svix = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_equity_premium + pc2_equity_premium, data = df.q.freq) ; summary(equity_prem_svix)$r.squared #R2 0.544 
  other_svix       = lm(m_12 ~ d_gr_1y + p_d + p_e + ret_1y + spc1_full + spc2_full + pc1_other + pc2_other, data = df.q.freq) ; summary(other_svix)$r.squared #R2 0.568 
  
  
  df.q.freq = df.q.freq[62:165,]
  df.q.freq$prof_svix = predict(prof_svix)
  
  
  
  recessions.trim = subset(recessions.df, Peak >= min(df.q.freq$quarter_id))
  
  martin_predicted = ggplot(data=df.q.freq) + theme_bw() +
    geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
    geom_line( mapping= aes(y= m_12, x= quarter_id, color = "Data"), linetype="solid",size=1.15, alpha=0.6) +
    geom_line( mapping= aes(y= prof_svix, x= quarter_id, color = "Fitted"), linetype="solid",size=1.15, alpha=0.6) +
    labs(color = '') + 
    scale_color_manual(values = c(
      "Data" = 'darkgreen',
      'Fitted' = 'darkred')) +
    xlab("time") + ylab("1-year Svix (Martin '17)") +
    theme(legend.position = c(0.25,0.80), legend.text = element_text(size=13)) 
  
 
  ggarrange( martin_predicted, nrow = 1,ncol = 1, common.legend = T)
  
  
  ######################################################################################################
  # Campbell and Shiller decomposition in sample
  ######################################################################################################
  
  
  source(file =paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\decomposition_table_fabio.R"), chdir = T)
  
  decomposition_list_real = list()
  decomposition_list_pred = list()
  
  average.dp <- mean(df.freq[,"D"]/df.freq[,"P"])
  
  kappa <- 1/(1+average.dp)
  
  in_sample = TRUE
  
  if (in_sample == TRUE){
    
    estimations <- names(vars_unc_list_is)
    var_model_list = c("vars_unc_list_is","vars_lasso_list_is","vars_ridge_list_is","vars_elnet_list_is")
    
  }else{
    
    estimations <- names(vars_unc_list_oos)
    var_model_list = c("vars_unc_list_oos","vars_lasso_list_oos","vars_ridge_list_oos","vars_elnet_list_oos")
    
    }
  
  get("vars_unc_list_is")
  
  var_model = "vars_unc_list_is"
  var_model = "vars_lasso_list_is"
  var_model = "vars_ridge_list_is"
  name = "profitability"
  
  for (var_model in var_model_list) {
    
    for (name in estimations[-c(1,11)]){
      
      tempData = get(var_model)
      tempData = as.data.frame(tempData[[name]])
      
      if("month" %in% names(tempData)) {
        tempData <- subset(tempData, select = -month)
      }
      
      cols_to_real <- grep("_pred$", colnames(tempData), value=TRUE, invert = TRUE)[c(2:5,10)]
      
      colnames(tempData)[c(2:5,18)] = paste0(cols_to_real,"_real")
      cols_to_lead <- grep("_pred$", colnames(tempData), value=TRUE)[c(1:4,9)]
      cols_to_lead2 = grep("_real$", colnames(tempData), value=TRUE)
      cols_to_lead = c(cols_to_lead2,cols_to_lead)
      
      tempData = tempData[,c("quarter_id",cols_to_lead)]
      
      for(col in cols_to_lead){
        
        tempData[paste0(col, "_1y_lead")] <- lead(tempData[, col], n = 1) 
        
      }
      
      tempDecomp <- decomposition_table(data = tempData, horizon = "1y", kappa=kappa, sub_1 = "2001-01-01")

      decomposition_list_real[[name]] = tempDecomp$tab.out.real
      decomposition_list_pred[[name]] = tempDecomp$tab.out.pred
      
    }
    
    assign(paste0("decomposition_real_",var_model),decomposition_list_real)
    assign(paste0("decomposition_pred_",var_model),decomposition_list_pred)
  }
  
  
  decomposition_real_vars_unc_list_is$profitability
  decomposition_pred_vars_elnet_list_is$profitability
  decomposition_pred_vars_lasso_list_is$profitability
  
  
  cor(df.freq[,"p_d_1y_lead"],df.freq[,'p_d'])
  cor(df.freq[22:42,"p_e_1y_lead"],df.freq[22:42,'p_e']) 
  
  
  ######################################################################################################
  # Campbell and Shiller decomposition out of sample
  ######################################################################################################
  
  
  source(file =paste0("C:\\Users\\R59\\Documents\\GitHub\\Dynamics_Returns_and_Fundamentals\\code\\version_2024\\decomposition_table_fabio.R"), chdir = T)
  
  decomposition_list_real = list()
  decomposition_list_pred = list()
  
  average.dp <- mean(df.freq[,"D"]/df.freq[,"P"])
  
  kappa <- 1/(1+average.dp)
  
  in_sample = FALSE
  
  if (in_sample == TRUE){
    
    estimations <- names(vars_unc_list_is)
    var_model_list = c("vars_unc_list_is","vars_lasso_list_is","vars_ridge_list_is","vars_elnet_list_is")
    
  }else{
    
    estimations <- names(vars_unc_list_oos)
    var_model_list = c("vars_unc_list_oos","vars_lasso_list_oos","vars_ridge_list_oos","vars_elnet_list_oos")
    if ("no_pc" %in% estimations) {
      estimations <- estimations[estimations != "no_pc"]
    }
  }
  
  
  name = "profitability"
  
  for (var_model in var_model_list) {
    
    #for (name in estimations){
      
      tempData = get(var_model)
      tempData = as.data.frame(tempData[[name]])
      
      if("month" %in% names(tempData)) {
        tempData <- subset(tempData, select = -month)
      }
      
      cols_to_real <- grep("_pred$", colnames(tempData), value=TRUE, invert = TRUE)[c(2:5,11)]
      
      colnames(tempData)[c(2:5,19)] = paste0(cols_to_real,"_real")
      cols_to_lead <- grep("_pred$", colnames(tempData), value=TRUE)[c(1:4,9)]
      cols_to_lead2 = grep("_real$", colnames(tempData), value=TRUE)
      cols_to_lead = c(cols_to_lead2,cols_to_lead)
      
      tempData = tempData[,c("quarter_id",cols_to_lead)]
      
      for(col in cols_to_lead){
        
        tempData[paste0(col, "_1y_lead")] <- lead(tempData[, col], n = 1) 
        
      }
      
      tempDecomp <- decomposition_table(data = tempData, horizon = "1y", kappa=kappa, sub_1 = "2001-01-01")
      
      decomposition_list_real[[name]] = tempDecomp$tab.out.real
      decomposition_list_pred[[name]] = tempDecomp$tab.out.pred
      
    #}
    
    assign(paste0("decomposition_real_",var_model),decomposition_list_real)
    assign(paste0("decomposition_pred_",var_model),decomposition_list_pred)
  }
  
  decomposition_real_vars_lasso_list_oos$profitability
  decomposition_pred_vars_lasso_list_oos$profitability
  decomposition_pred_vars_elnet_list_oos$profitability
  
  
    
  
  
  cor(vars_lasso_list_oos$profitability[,'d_gr_1y'],vars_lasso_list_oos$profitability[,'d_gr_1y_pred'])
  cor(vars_lasso_list_oos$profitability[,'e_gr_1y'],vars_lasso_list_oos$profitability[,'e_gr_1y_pred'])
  cor(vars_lasso_list_oos$profitability[-c(8,9),'e_gr_1y'],vars_lasso_list_oos$profitability[-c(8,9),'e_gr_1y_pred'])
  cor(vars_elnet_list_oos$profitability[-c(8,9),'e_gr_1y'],vars_elnet_list_oos$profitability[-c(8,9),'e_gr_1y_pred'])
  
  cor(vars_lasso_list_oos$profitability[,'p_d'],vars_lasso_list_oos$profitability[,'p_d_pred'])
  cor(vars_lasso_list_oos$profitability[,'ret_1y'],vars_lasso_list_oos$profitability[,'ret_1y_pred'])
  
################################################
################################################
################################################
  
  
var_insample$pred_lasso_t1_list$profitability$e_gr_1y_pred_t1 = vars_lasso_list_is$profitability$e_gr_1y_pred
var_insample$pred_ridge_t1_list$profitability$e_gr_1y_pred_t1 = vars_ridge_list_is$profitability$e_gr_1y_pred
var_insample$pred_elnet_t1_list$profitability$e_gr_1y_pred_t1 = vars_elnet_list_is$profitability$e_gr_1y_pred

var_insample$pred_lasso_t2_list$profitability$e_gr_1y_pred_t2 = var_insample$pred_lasso_t2_list$profitability$d_gr_1y_pred_t2 + var_insample$pred_lasso_t2_list$profitability$p_d_pred_t2 - var_insample$pred_lasso_t2_list$profitability$p_e_pred_t2 + var_insample$pred_lasso_t1_list$profitability$p_e_pred_t1 - var_insample$pred_lasso_t1_list$profitability$p_d_pred_t1

var_insample$pred_ridge_t2_list$profitability$e_gr_1y_pred_t2 = var_insample$pred_ridge_t2_list$profitability$d_gr_1y_pred_t2 + var_insample$pred_ridge_t2_list$profitability$p_d_pred_t2 - var_insample$pred_ridge_t2_list$profitability$p_e_pred_t2 + var_insample$pred_ridge_t1_list$profitability$p_e_pred_t1 - var_insample$pred_ridge_t1_list$profitability$p_d_pred_t1


var_insample$pred_elnet_t2_list$profitability$e_gr_1y_pred_t2 = var_insample$pred_elnet_t2_list$profitability$d_gr_1y_pred_t2 + var_insample$pred_elnet_t2_list$profitability$p_d_pred_t2 - var_insample$pred_elnet_t2_list$profitability$p_e_pred_t2 + var_insample$pred_elnet_t1_list$profitability$p_e_pred_t1 - var_insample$pred_elnet_t1_list$profitability$p_d_pred_t1





var_insample$vars_lasso_list$profitability



  
pred_2y_lasso = cbind(var_insample$pred_lasso_t1_list$profitability[,c('quarter_id')], as.data.frame(cbind(var_insample$pred_lasso_t1_list$profitability[,c('d_gr_1y_pred_t1','ret_1y_pred_t1','e_gr_1y_pred_t1')] + var_insample$pred_lasso_t2_list$profitability[,c('d_gr_1y_pred_t2','ret_1y_pred_t2','e_gr_1y_pred_t2')] ,var_insample$pred_lasso_t2_list$profitability[,c('p_d_pred_t2','p_e_pred_t2')])))
colnames(pred_2y_lasso)[1:4] = c("quarter_id", "d_gr_1y_pred_t1_t2","ret_1y_pred_t1_t2",'e_gr_1y_pred_t1_t2')

pred_2y_ridge = cbind(var_insample$pred_ridge_t1_list$profitability[,c('quarter_id')], as.data.frame(cbind(var_insample$pred_ridge_t1_list$profitability[,c('d_gr_1y_pred_t1','ret_1y_pred_t1')] + var_insample$pred_ridge_t2_list$profitability[,c('d_gr_1y_pred_t2','ret_1y_pred_t2')], var_insample$pred_ridge_t2_list$profitability[,c('p_d_pred_t2','p_e_pred_t2')])))
colnames(pred_2y_ridge)[1:3] = c("quarter_id", "d_gr_1y_pred_t1_t2","ret_1y_pred_t1_t2")

pred_2y_elnet = cbind(var_insample$pred_elnet_t1_list$profitability[,c('quarter_id')], as.data.frame(cbind(var_insample$pred_elnet_t1_list$profitability[,c('d_gr_1y_pred_t1','ret_1y_pred_t1','e_gr_1y_pred_t1')] + var_insample$pred_elnet_t2_list$profitability[,c('d_gr_1y_pred_t2','ret_1y_pred_t2','e_gr_1y_pred_t2')], var_insample$pred_elnet_t2_list$profitability[,c('p_d_pred_t2','p_e_pred_t2')])))
colnames(pred_2y_ridge)[1:4] = c("quarter_id", "d_gr_1y_pred_t1_t2","ret_1y_pred_t1_t2",'e_gr_1y_pred_t1_t2')


pred_3y_lasso = cbind(var_insample$pred_lasso_t1_list$profitability[,c('quarter_id')], as.data.frame(cbind(var_insample$pred_lasso_t1_list$profitability[,c('d_gr_1y_pred_t1','ret_1y_pred_t1')] + var_insample$pred_lasso_t2_list$profitability[,c('d_gr_1y_pred_t2','ret_1y_pred_t2')] + var_insample$pred_lasso_t3_list$profitability[,c('d_gr_1y_pred_t3','ret_1y_pred_t3')], var_insample$pred_lasso_t3_list$profitability[,c('p_d_pred_t3','p_e_pred_t3')])))
colnames(pred_3y_lasso)[1:3] = c("quarter_id", "d_gr_1y_pred_t1_t3","ret_1y_pred_t1_t3")

pred_3y_ridge = cbind(var_insample$pred_ridge_t1_list$profitability[,c('quarter_id')], as.data.frame(cbind(var_insample$pred_ridge_t1_list$profitability[,c('d_gr_1y_pred_t1','ret_1y_pred_t1')] + var_insample$pred_ridge_t2_list$profitability[,c('d_gr_1y_pred_t2','ret_1y_pred_t2')] + var_insample$pred_ridge_t3_list$profitability[,c('d_gr_1y_pred_t3','ret_1y_pred_t3')], var_insample$pred_ridge_t3_list$profitability[,c('p_d_pred_t3','p_e_pred_t3')])))
colnames(pred_3y_ridge)[1:3] = c("quarter_id", "d_gr_1y_pred_t1_t3","ret_1y_pred_t1_t3")


pred_3y_elnet = cbind(var_insample$pred_elnet_t1_list$profitability[,c('quarter_id')], as.data.frame(cbind(var_insample$pred_elnet_t1_list$profitability[,c('d_gr_1y_pred_t1','ret_1y_pred_t1')] + var_insample$pred_elnet_t2_list$profitability[,c('d_gr_1y_pred_t2','ret_1y_pred_t2')] + var_insample$pred_elnet_t3_list$profitability[,c('d_gr_1y_pred_t3','ret_1y_pred_t3')], var_insample$pred_elnet_t3_list$profitability[,c('p_d_pred_t3','p_e_pred_t3')])))
colnames(pred_3y_elnet)[1:3] = c("quarter_id", "d_gr_1y_pred_t1_t3","ret_1y_pred_t1_t3")






cor(df.freq[1:41,c('d_gr_2y_lead')],pred_2y_lasso[1:41,c('d_gr_1y_pred_t1_t2')])
cor(df.freq[1:40,c('d_gr_3y_lead')],pred_3y_lasso[1:40,c('d_gr_1y_pred_t1_t3')])
  
cor(df.freq[1:41,c('ret_2y_lead')],pred_2y_lasso[1:41,c('ret_1y_pred_t1_t2')])
cor(df.freq[1:40,c('ret_3y_lead')],pred_3y_lasso[1:40,c('ret_1y_pred_t1_t3')])
  



var_insample$vars_lasso_list$profitability

var_insample$pred_lasso_t1_list$profitability



average.dp <- mean(df.freq[,"D"]/df.freq[,"P"])

kappa <- 1/(1+average.dp)

delta_pd = c(NA,diff(df.freq[,'p_d']))

delta_Epd_lasso = c(NA, diff(var_insample$pred_lasso_t1_list$profitability[,'p_d_pred_t1']))

delta_Er_lasso = c(NA, diff(var_insample$pred_lasso_t1_list$profitability[,'ret_1y_pred_t1']))

delta_Edgr_lasso = c(NA, diff(var_insample$pred_lasso_t1_list$profitability[,'d_gr_1y_pred_t1']))




delta_Epd_ridge = c(NA, diff(var_insample$pred_ridge_t1_list$profitability[,'p_d_pred_t1']))

delta_Er_ridge = c(NA, diff(var_insample$pred_ridge_t1_list$profitability[,'ret_1y_pred_t1']))
  
delta_Edgr_ridge = c(NA, diff(var_insample$pred_ridge_t1_list$profitability[,'d_gr_1y_pred_t1']))



delta_Epd_elnet = c(NA, diff(var_insample$pred_elnet_t1_list$profitability[,'p_d_pred_t1']))

delta_Er_elnet = c(NA, diff(var_insample$pred_elnet_t1_list$profitability[,'ret_1y_pred_t1']))

delta_Edgr_elnet = c(NA, diff(var_insample$pred_elnet_t1_list$profitability[,'d_gr_1y_pred_t1']))


pd_dyn = cbind(as.Date(df.freq[-42,'quarter_id']),as.data.frame(cbind(delta_pd[-42],delta_Edgr_elnet,delta_Er_elnet,delta_Epd_elnet,delta_Edgr_lasso,delta_Er_lasso,delta_Epd_lasso,delta_Edgr_ridge,delta_Er_ridge,delta_Epd_ridge)))
colnames(pd_dyn)[1:2] = c("quarter_id","delta_pd")





delta_pe = c(NA,diff(df.freq[,'p_e']))

delta_Epe_lasso = c(NA, diff(var_insample$pred_lasso_t1_list$profitability[,'p_e_pred_t1']))

delta_Er_lasso = c(NA, diff(var_insample$pred_lasso_t1_list$profitability[,'ret_1y_pred_t1']))

delta_Eegr_lasso = c(NA, diff(var_insample$pred_lasso_t1_list$profitability[,'e_gr_1y_pred_t1']))




delta_Epe_ridge = c(NA, diff(var_insample$pred_ridge_t1_list$profitability[,'p_e_pred_t1']))

delta_Er_ridge = c(NA, diff(var_insample$pred_ridge_t1_list$profitability[,'ret_1y_pred_t1']))

delta_Eegr_ridge = c(NA, diff(var_insample$pred_ridge_t1_list$profitability[,'e_gr_1y_pred_t1']))



delta_Epe_elnet = c(NA, diff(var_insample$pred_elnet_t1_list$profitability[,'p_e_pred_t1']))

delta_Er_elnet = c(NA, diff(var_insample$pred_elnet_t1_list$profitability[,'ret_1y_pred_t1']))

delta_Eegr_elnet = c(NA, diff(var_insample$pred_elnet_t1_list$profitability[,'e_gr_1y_pred_t1']))




pe_dyn = cbind(as.Date(df.freq[-42,'quarter_id']),as.data.frame(cbind(delta_pe[-42],delta_Eegr_elnet,delta_Er_elnet,delta_Epe_elnet,delta_Eegr_lasso,delta_Er_lasso,delta_Epe_lasso,delta_Eegr_ridge,delta_Er_ridge,delta_Epe_ridge)))
colnames(pe_dyn)[1:2] = c("quarter_id","delta_pe")






recessions.trim = subset(recessions.df, Peak >= min(pd_dyn$quarter_id))

fund_plt_1 = ggplot(data = pd_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line( mapping= aes(y= delta_pd, x= quarter_id, color = "Delta pd"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= delta_Edgr_elnet, x= quarter_id, color = "DR"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'Delta pd' = 'darkred',
    'DR' = 'blue'), labels = c(expression(Delta * "pd","DR"))) +
  xlab("time") + ylab("variation log price-to-dividend ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))


fund_plt_2 = ggplot(data = pd_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(mapping= aes(y= delta_pd, x= quarter_id, color = "Delta pd"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= -delta_Er_elnet, x= quarter_id, color = "-RR"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '') + 
  scale_color_manual(values = c(
    'Delta pd' = 'darkred',
    "-RR" = 'blue'), labels = c("-RR",expression(Delta * "pd"))) +
  xlab("time") + ylab("variation log price-to-dividend ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))


fund_plt_3 = ggplot(data = pd_dyn) + theme_bw() +
              geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
              geom_line(mapping= aes(y= delta_pd, x= quarter_id, color = "Delta pd"), linetype="solid",size=1.25, alpha=0.6 ) +
              geom_line( mapping= aes(y= delta_Edgr_elnet-delta_Er_elnet, x= quarter_id, color = "DR-RR"), linetype="solid",size=1.25, alpha=0.6 ) +
              labs(color = '')+ 
              scale_color_manual(values = c(
                'Delta pd' = 'darkred',
                'DR-RR' = 'blue'), labels = c(expression(Delta * "pd"), "DR-RR")) +
  xlab("time") + ylab("variation log price-to-dividend ratio") + 
              theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))


fund_plt_4 = ggplot(data = pd_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(mapping= aes(y= delta_pd, x= quarter_id, color = "Delta pd"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= delta_Edgr_elnet-delta_Er_elnet+kappa*delta_Epd_elnet, x= quarter_id, color = "DR-RR+PDR"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'Delta pd' = 'darkred',
    'DR-RR+PDR' = 'blue'), labels = c(expression(Delta * "pd"), "DR-RR+PDR")) +
  xlab("time") + ylab("variation log price-to-dividend ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))

ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, fund_plt_4, nrow = 2, ncol = 2, common.legend = F)












recessions.trim = subset(recessions.df, Peak >= min(pe_dyn$quarter_id))

fund_plt_1 = ggplot(data = pe_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line( mapping= aes(y= delta_pe, x= quarter_id, color = "Delta pe"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= delta_Eegr_elnet, x= quarter_id, color = "ER"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'Delta pe' = 'darkred',
    'ER' = 'blue'), labels = c(expression(Delta * "pe","ER"))) +
  xlab("time") + ylab("variation log price-to-earnings ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))


fund_plt_2 = ggplot(data = pe_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(mapping= aes(y= delta_pe, x= quarter_id, color = "Delta pe"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= -delta_Er_elnet, x= quarter_id, color = "-RR"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '') + 
  scale_color_manual(values = c(
    'Delta pe' = 'darkred',
    "-RR" = 'blue'), labels = c("-RR",expression(Delta * "pe"))) +
  xlab("time") + ylab("variation log price-to-earnings ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))


fund_plt_3 = ggplot(data = pe_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(mapping= aes(y= delta_pe, x= quarter_id, color = "Delta pe"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= delta_Eegr_elnet-delta_Er_elnet, x= quarter_id, color = "ER-RR"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'Delta pe' = 'darkred',
    'ER-RR' = 'blue'), labels = c(expression(Delta * "pe"), "ER-RR")) +
  xlab("time") + ylab("variation log price-to-earnings ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))


fund_plt_4 = ggplot(data = pe_dyn) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(mapping= aes(y= delta_pe, x= quarter_id, color = "Delta pe"), linetype="solid",size=1.25, alpha=0.6 ) +
  geom_line( mapping= aes(y= delta_Eegr_elnet-delta_Er_elnet+kappa*delta_Epe_elnet, x= quarter_id, color = "ER-RR+PER"), linetype="solid",size=1.25, alpha=0.6 ) +
  labs(color = '')+ 
  scale_color_manual(values = c(
    'Delta pe' = 'darkred',
    'ER-RR+PER' = 'blue'), labels = c(expression(Delta * "pe"), "ER-RR+PER")) +
  xlab("time") + ylab("variation log price-to-earnings ratio") + 
  theme(legend.position = c(0.25,0.25), legend.text = element_text(size=13))

ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, fund_plt_4, nrow = 2, ncol = 2, common.legend = F)




##########################
# Bayesian model averaging In sample
##########################

### Variance of individual predictions 

ll_matrix = as.data.frame(matrix(NA, nrow = nrow(var_all_is), ncol =  length(CATEGORIES)))

colnames(ll_matrix) = c("quarter_id", names(CATEGORIES)[c(-10)])

ll_matrix[,'quarter_id'] = vars_unc_list_is$profitability[,'quarter_id']

Sigma = cov(vars_unc_list_is$profitability[, DEPVAR])

i=j=1

var(df.freq[,'d_gr_1y'], na.rm = T); var(df.freq[,'p_d'], na.rm = T); var(df.freq[,'p_e'], na.rm = T); var(df.freq[,'ret_1y'], na.rm = T)

i=28

for (j in 1:(ncol(ll_matrix)-1)) {
  
  for (i in 1:NROW(ll_matrix)) {
  
        ll_matrix[i,1+j] = as.numeric(dmvnorm( x = vars_elnet_list_is[[j]][i, DEPVAR] , mean = as.numeric(vars_elnet_list_is[[j]][i,paste0(DEPVAR,"_pred")]), sigma = Sigma, log = T))
      
  }
  
}

w_0 = rep(1/(ncol(ll_matrix)-1),ncol(ll_matrix)-1) ## initial distribution

posterior_function  = function(w,ll_matrix){
                                              
                                              w_1 = as.numeric(as.numeric(w*exp(ll_matrix)) / as.numeric(exp(ll_matrix)) %*% as.numeric(w))
                                              
                                              return(w_1 = w_1)
                                              
                                           }


w_t <- as.data.frame(matrix(NA, nrow = nrow(ll_matrix)+1, ncol = NCOL(ll_matrix)))

w_t[-1,1] = as.character(vars_unc_list_is$profitability[,'quarter_id'])

colnames(w_t) = c("quarter_id",names(CATEGORIES)[-10])

w_t[1,-1] = w_0

for (j in 1:nrow(ll_matrix)) {
  
  w_t[1+j,-1] =  posterior_function(as.numeric(w_t[j,-1]),as.numeric(ll_matrix[j,-1]))
  
}

w_t[1,1] = as.character(as.Date(w_t[2,1]) - months(12))

apply(w_t[,-1], 1, max); apply(w_t[,-1], 1, sum)



var_hat_mix_is = as.data.frame( matrix(NA,nrow = nrow(ll_matrix), ncol = ncol(vars_unc_list_is$no_pc)))

colnames(var_hat_mix_is) = colnames(vars_unc_list_is$no_pc)

var_hat_mix_is[,c('quarter_id','month', DEPVAR,"e_gr_1y")] = vars_elnet_list_is$no_pc[,c('quarter_id','month', DEPVAR,"e_gr_1y")]


for (i in 1:nrow(ll_matrix)) {
  
  for (j in 1:(ncol(ll_matrix)-1)) {

    if (j==1) {
      
      var_hat_mix_is[i,paste0(c(DEPVAR,"e_gr_1y"),"_pred")] = as.numeric(vars_elnet_list_is[[j]][i,paste0(c(DEPVAR,"e_gr_1y"),"_pred")])*w_t[i,j+1]
      
    }else{
      
      var_hat_mix_is[i,paste0(c(DEPVAR,"e_gr_1y"),"_pred")] = var_hat_mix_is[i,paste0(c(DEPVAR,"e_gr_1y"),"_pred")] + as.numeric(vars_elnet_list_is[[j]][i,paste0(c(DEPVAR,"e_gr_1y"),"_pred")])*w_t[i,j+1]
      
    }
    
  }
  
}

r2.univeriate_bmix <- data.frame(matrix(ncol=(length(DEPVAR))+1, nrow=1)) 
colnames(r2.univeriate_bmix) <- c(DEPVAR,"e_gr_1y")

r2.univeriate_bmix[,DEPVAR[1]]  = 1- sum((var_hat_mix_is[,DEPVAR[1]] - var_hat_mix_is[,paste0(DEPVAR[1],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_is[,DEPVAR[1]] - mean(var_hat_mix_is[,DEPVAR[1]]) )**2)
r2.univeriate_bmix[,DEPVAR[2]]  = 1- sum((var_hat_mix_is[,DEPVAR[2]] - var_hat_mix_is[,paste0(DEPVAR[2],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_is[,DEPVAR[2]] - mean(var_hat_mix_is[,DEPVAR[2]]) )**2)
r2.univeriate_bmix[,DEPVAR[3]]  = 1- sum((var_hat_mix_is[,DEPVAR[3]] - var_hat_mix_is[,paste0(DEPVAR[3],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_is[,DEPVAR[3]] - mean(var_hat_mix_is[,DEPVAR[3]]) )**2)
r2.univeriate_bmix[,DEPVAR[4]]  = 1- sum((var_hat_mix_is[,DEPVAR[4]] - var_hat_mix_is[,paste0(DEPVAR[4],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_is[,DEPVAR[4]] - mean(var_hat_mix_is[,DEPVAR[4]]) )**2)
r2.univeriate_bmix[,"e_gr_1y"]  = 1- sum((var_hat_mix_is[,"e_gr_1y"] - var_hat_mix_is[,"e_gr_1y_pred"])**2, na.rm = T) /  sum((var_hat_mix_is[,"e_gr_1y"] - mean(var_hat_mix_is[,"e_gr_1y"]) )**2)

print(r2.univeriate_bmix)
r2.models_elnet

cor(var_hat_mix_is[,'d_gr_1y'],var_hat_mix_is[,'d_gr_1y_pred'], use = "complete.obs")
cor(var_hat_mix_is[,'e_gr_1y'],var_hat_mix_is[,'e_gr_1y_pred'], use = "complete.obs")


w_t[,1] = as.Date(w_t[,1])

recessions.trim = subset(recessions.df, Peak >= min(w_t$quarter_id))

fund_plt_1 = ggplot(data=w_t) + theme_bw()+
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line( mapping= aes(y= capitalization   , x= quarter_id, color = "Capitalization"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= efficiency  , x= quarter_id, color = "Efficiency"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= financial_soundness, x= quarter_id, color = "Financial Soundness"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= liquidity, x= quarter_id, color = "Liquidity"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= profitability, x= quarter_id, color = "Profitability"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= solvency, x= quarter_id, color = "Solvency"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= valuation, x= quarter_id, color = "Valuation"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= equity_premium, x= quarter_id, color = "Equity Premium"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= other, x= quarter_id, color = "Other"), linetype="solid",size=1) +
  labs(color = '')+ 
  xlab("time") + ylab("posterior probability")+
  theme(legend.position = c(0.20,0.80))
ggarrange(fund_plt_1,  nrow = 1, ncol = 1, common.legend = T)

w_t[,'quarter_id'] = as.Date(w_t[,'quarter_id']) 

# Create individual plots
p1 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=capitalization, x=quarter_id), color="darkblue", size=1)  + ylim(c(0,0.4)) + 
  labs(title="Capitalization", y="Probability", x="Time") + theme(legend.position = "none")

p2 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=efficiency, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.4)) + 
  labs(title="Efficiency", y="Probability", x="Time") + theme(legend.position = "none")

p3 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=financial_soundness, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.85)) + 
  labs(title="Financial Soundness", y="Probability", x="Time") + theme(legend.position = "none")

p4 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=liquidity, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.4)) + 
  labs(title="Liquidity", y="Probability", x="Time") + theme(legend.position = "none")

p5 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=profitability, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.85)) + 
  labs(title="Profitability", y="Probability", x="Time") + theme(legend.position = "none")

p6 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=solvency, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.85)) + 
  labs(title="Solvency", y="Probability", x="Time") + theme(legend.position = "none")

p7 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=valuation, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.85)) + 
  labs(title="Valuation", y="Probability", x="Time") + theme(legend.position = "none")

p8 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=equity_premium, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.85)) + 
  labs(title="Equity Premium", y="Probability", x="Time") + theme(legend.position = "none")

p9 <- ggplot(data=w_t) + theme_bw() +
  geom_rect(data=recessions.trim, aes(NULL, NULL, xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line(aes(y=other, x=quarter_id), color="darkblue", size=1) + ylim(c(0,0.8)) + 
  labs(title="Other", y="Probability", x="Time") + theme(legend.position = "none")

# Arrange all plots vertically
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 2, nrow = 5)





var_models                           = as.data.frame(cbind(w_t[,1],rowVars(as.matrix(w_t[,-1]))))
colnames(var_models)                 = c("quarter_id","posterior_variance")
var_models[,c("quarter_id")]         = as.Date(var_models[,c("quarter_id")])
var_models[,c("posterior_variance")] = as.numeric(var_models[,c("posterior_variance")])  


ggplot(data=var_models) + theme_bw()+
  geom_line( mapping= aes(y= posterior_variance, x= quarter_id, color = "yearly frequency"), linetype="solid",size=1.25) +
  scale_color_manual(values = c(
    'yearly frequency' = 'blue')) +
  labs(color = '')+ 
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  xlab("time") + ylab("variance posterior distribution")+
  theme(legend.position = c(0.80,0.20))






#################################
#################################

##########################
# Bayesian model averaging  OOS
##########################

### Variance of individual predictions 

ll_matrix = as.data.frame(matrix(NA, nrow = nrow(var_full_oos), ncol =  length(CATEGORIES)))

colnames(ll_matrix) = c("quarter_id", names(CATEGORIES)[c(-10)])

ll_matrix[,'quarter_id'] = vars_unc_list_oos$profitability[,'quarter_id']

Sigma = cov(df.freq[1:21, DEPVAR])

i=j=1

var(df.freq[,'d_gr_1y'], na.rm = T); var(df.freq[,'p_d'], na.rm = T); var(df.freq[,'p_e'], na.rm = T); var(df.freq[,'ret_1y'], na.rm = T)

i=28

for (j in 1:(ncol(ll_matrix)-1)) {
  
  for (i in 1:NROW(ll_matrix)) {
    
    ll_matrix[i,1+j] = as.numeric(dmvnorm( x = vars_elnet_list_oos[[j]][i, DEPVAR] , mean = as.numeric(vars_elnet_list_oos[[j]][i,paste0(DEPVAR,"_pred")]), sigma = Sigma, log = T))
    
  }
  
}

w_0 = rep(1/(ncol(ll_matrix)-1),ncol(ll_matrix)-1) ## initial distribution

posterior_function  = function(w,ll_matrix){
  
  w_1 = as.numeric(as.numeric(w*exp(ll_matrix)) / as.numeric(exp(ll_matrix)) %*% as.numeric(w))
  
  return(w_1 = w_1)
  
}


w_t <- as.data.frame(matrix(NA, nrow = nrow(ll_matrix)+1, ncol = NCOL(ll_matrix)))

w_t[-1,1] = as.character(vars_elnet_list_oos$profitability[,'quarter_id'])

colnames(w_t) = c("quarter_id",names(CATEGORIES)[-10])

w_t[1,-1] = w_0

for (j in 1:nrow(ll_matrix)) {
  
  w_t[1+j,-1] =  posterior_function(as.numeric(w_t[j,-1]),as.numeric(ll_matrix[j,-1]))
  
}

w_t[1,1] = as.character(as.Date(w_t[2,1]) - months(12))

apply(w_t[,-1], 1, max); apply(w_t[,-1], 1, sum)



var_hat_mix_oos = as.data.frame( matrix(NA,nrow = nrow(ll_matrix), ncol = ncol(vars_elnet_list_oos$no_pc)+2))

colnames(var_hat_mix_oos) = colnames(var_hat_mix_is)

var_hat_mix_oos[,c('quarter_id', DEPVAR,"e_gr_1y")] = vars_elnet_list_oos$profitability[,c('quarter_id', DEPVAR,"e_gr_1y")]
var_hat_mix_is

for (i in 1:nrow(ll_matrix)) {
  
  for (j in 1:(ncol(ll_matrix)-1)) {
    
    if (j==1) {
      
      var_hat_mix_oos[i,paste0(c(DEPVAR),"_pred")] = as.numeric(vars_elnet_list_oos[[j]][i,paste0(c(DEPVAR),"_pred")])*w_t[i,j+1]
      
    }else{
      
      var_hat_mix_oos[i,paste0(c(DEPVAR),"_pred")] = var_hat_mix_oos[i,paste0(c(DEPVAR),"_pred")] + as.numeric(vars_elnet_list_oos[[j]][i,paste0(c(DEPVAR),"_pred")])*w_t[i,j+1]
      
    }
    
  }
  
}

r2.univeriate_bmix_oos <- data.frame(matrix(ncol=(length(DEPVAR))+1, nrow=1)) 
colnames(r2.univeriate_bmix_oos) <- c(DEPVAR,"e_gr_1y")

r2.univeriate_bmix_oos[,DEPVAR[1]]  = 1- sum((var_hat_mix_oos[,DEPVAR[1]] - var_hat_mix_oos[,paste0(DEPVAR[1],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_oos[,DEPVAR[1]] - mean(var_hat_mix_oos[,DEPVAR[1]]) )**2)
r2.univeriate_bmix_oos[,DEPVAR[2]]  = 1- sum((var_hat_mix_oos[,DEPVAR[2]] - var_hat_mix_oos[,paste0(DEPVAR[2],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_oos[,DEPVAR[2]] - mean(var_hat_mix_oos[,DEPVAR[2]]) )**2)
r2.univeriate_bmix_oos[,DEPVAR[3]]  = 1- sum((var_hat_mix_oos[,DEPVAR[3]] - var_hat_mix_oos[,paste0(DEPVAR[3],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_oos[,DEPVAR[3]] - mean(var_hat_mix_oos[,DEPVAR[3]]) )**2)
r2.univeriate_bmix_oos[,DEPVAR[4]]  = 1- sum((var_hat_mix_oos[,DEPVAR[4]] - var_hat_mix_oos[,paste0(DEPVAR[4],'_pred')])**2, na.rm = T) /  sum((var_hat_mix_oos[,DEPVAR[4]] - mean(var_hat_mix_oos[,DEPVAR[4]]) )**2)
#r2.univeriate_bmix_oos[,"e_gr_1y"]  = 1- sum((var_hat_mix_is[,"e_gr_1y"] - var_hat_mix_is[,"e_gr_1y_pred"])**2, na.rm = T) /  sum((var_hat_mix_is[,"e_gr_1y"] - mean(var_hat_mix_is[,"e_gr_1y"]) )**2)

print(r2.univeriate_bmix_oos)
r2.models_elnet

cor(var_hat_mix_oos[,'d_gr_1y'],var_hat_mix_oos[,'d_gr_1y_pred'], use = "complete.obs")
cor(var_hat_mix_oos[,'ret_1y'],var_hat_mix_oos[,'ret_1y_pred'], use = "complete.obs")


w_t[,1] = as.Date(w_t[,1])
recessions.trim = subset(recessions.df, Peak >= min(w_t$quarter_id))

fund_plt_1 = ggplot(data=w_t) + theme_bw()+
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line( mapping= aes(y= capitalization   , x= quarter_id, color = "Capitalization"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= efficiency  , x= quarter_id, color = "Efficiency"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= financial_soundness, x= quarter_id, color = "Financial Soundness"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= liquidity, x= quarter_id, color = "Liquidity"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= profitability, x= quarter_id, color = "Profitability"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= solvency, x= quarter_id, color = "Solvency"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= valuation, x= quarter_id, color = "Valuation"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= equity_premium, x= quarter_id, color = "Equity Premium"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= other, x= quarter_id, color = "Other"), linetype="solid",size=1) +
  labs(color = '')+ 
  xlab("time") + ylab("posterior probability")+
  theme(legend.position = c(0.20,0.80))
ggarrange(fund_plt_1,  nrow = 1, ncol = 1, common.legend = T)




var_models = as.data.frame(cbind(w_t[,1],rowVars(as.matrix(w_t[,-1]))))
colnames(var_models) = c("quarter_id","posterior_variance")
var_models[,c("quarter_id")] = as.Date(var_models[,c("quarter_id")])
var_models[,c("posterior_variance")] = as.numeric(var_models[,c("posterior_variance")])  


ggplot(data=var_models) + theme_bw()+
  geom_line( mapping= aes(y= posterior_variance, x= quarter_id, color = "yearly frequency"), linetype="solid",size=1.25) +
  scale_color_manual(values = c(
    'yearly frequency' = 'blue')) +
  labs(color = '')+ 
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  xlab("time") + ylab("variance posterior distribution")+
  theme(legend.position = c(0.80,0.20))




######################################################################################################
#  Out-of-sample
######################################################################################################

######################################################################################################
# Dynamic model Out of Sample
######################################################################################################

source(file=paste0(path_functions,"estimate_model_rolling_v2.R"), chdir = T)
source(file=paste0(path_functions,"estimate_model_rolling_v4_temp.R"), chdir = T)
source(file=paste0(path_functions,"estimate_model_rolling_v5.R"), chdir = T)


grid_sparse_pc = seq(0.00000,0.0005, length.out = 51)
grid_sparse_pc_len = length(grid_sparse_pc)


mod_r2 = 11
r2.models_matrix_div_unc = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pd_unc  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pe_unc  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_ret_unc = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))

r2.models_matrix_div_lasso = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pd_lasso  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pe_lasso  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_ret_lasso = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))

r2.models_matrix_div_ridge = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pd_ridge  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pe_ridge  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_ret_ridge = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))

r2.models_matrix_div_elnet = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pd_elnet  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_pe_elnet  = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))
r2.models_matrix_ret_elnet = as.data.frame(matrix(NA,grid_sparse_pc_len,mod_r2))


r2.models_matrix_div_unc[,1]   = r2.models_matrix_pd_unc[,1]   = r2.models_matrix_pe_unc[,1]   = r2.models_matrix_ret_unc[,1]   = 
r2.models_matrix_div_lasso[,1] = r2.models_matrix_pd_lasso[,1] = r2.models_matrix_pe_lasso[,1] = r2.models_matrix_ret_lasso[,1] = 
r2.models_matrix_div_ridge[,1] = r2.models_matrix_pd_ridge[,1] = r2.models_matrix_pe_ridge[,1] = r2.models_matrix_ret_ridge[,1] = 
r2.models_matrix_div_elnet[,1] = r2.models_matrix_pd_elnet[,1] = r2.models_matrix_pe_elnet[,1] = r2.models_matrix_ret_elnet[,1] = round(grid_sparse_pc,digits = 6)


colnames(r2.models_matrix_div_unc)[1:mod_r2] = colnames(r2.models_matrix_pd_unc)[1:mod_r2] = colnames(r2.models_matrix_pe_unc)[1:mod_r2] = colnames(r2.models_matrix_ret_unc)[1:mod_r2] = 
colnames(r2.models_matrix_div_lasso)[1:mod_r2] = colnames(r2.models_matrix_pd_lasso)[1:mod_r2] = colnames(r2.models_matrix_pe_lasso)[1:mod_r2] = colnames(r2.models_matrix_ret_lasso)[1:mod_r2] =   
colnames(r2.models_matrix_div_ridge)[1:mod_r2] = colnames(r2.models_matrix_pd_ridge)[1:mod_r2] = colnames(r2.models_matrix_pe_ridge)[1:mod_r2] = colnames(r2.models_matrix_ret_ridge)[1:mod_r2] =    
colnames(r2.models_matrix_div_elnet)[1:mod_r2] = colnames(r2.models_matrix_pd_elnet)[1:mod_r2] = colnames(r2.models_matrix_pe_elnet)[1:mod_r2] = colnames(r2.models_matrix_ret_elnet)[1:mod_r2] =
c("alpha","DEPVAR","capitalization","efficiency","financial_soundness","liquidity","profitability", "solvency", "valuation", "equity_premium", "other")


n_pc = 2
i=18
t1 = 0.5
pen_search = "Rolling"; pen_search = "LOO"
DEPVAR = c("d_gr_1y","p_d","e_d","ret_1y"); DEPVAR = c("d_gr_1y","p_d","p_e","ret_1y")



for (i in 1:grid_sparse_pc_len) {
    
  
model.estimates.unc = estimate_model(data = df.freq, data.pc = df.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES,
                                     model = "unconstrained", time.col = "quarter_id", n_pc = n_pc, data.freq = data_freq, model.pc = "all_models", 
                                     split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = path_functions, 
                                     optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search), 
                                     sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2, extra = "profitability")
    
r2.models <- model.estimates.unc$r2; print(r2.models)

r2.models_matrix_div_unc[i,2:13] = r2.models[,2]
r2.models_matrix_pd_unc[i,2:13]  = r2.models[,3]
r2.models_matrix_pe_unc[i,2:13]  = r2.models[,4]
r2.models_matrix_ret_unc[i,2:13] = r2.models[,5]



model.estimates.ridge = estimate_model(data = df.freq, data.pc = df.all.freq, dep.vars = DEPVAR,add.vars = CHARS,pca.cat.dict = CATEGORIES, 
                                       model = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = data_freq, model.pc = "full", 
                                       split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = path_functions, 
                                       optional.args = list(sep.lambdas=sep_lambdas,fit.intercept=fit_intercept, alpha= 0.0000, pen.search = pen_search),
                                       sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2, extra = "profitability")

r2.models <- model.estimates.ridge$r2; print(r2.models)

r2.models_matrix_div_ridge[i,2:13] = r2.models[,2]
r2.models_matrix_pd_ridge[i,2:13]  = r2.models[,3]
r2.models_matrix_pe_ridge[i,2:13]  = r2.models[,4]
r2.models_matrix_ret_ridge[i,2:13] = r2.models[,5]



model.estimates_lasso = estimate_model(data = df.freq,data.pc = df.all.freq, dep.vars = DEPVAR,add.vars = CHARS,pca.cat.dict = CATEGORIES, 
                                       model = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = data_freq, model.pc = "full", 
                                       split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = path_functions,
                                       optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 1, pen.search = pen_search), 
                                       sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2, extra = "profitability")

r2.models <- model.estimates_lasso$r2; print(r2.models)

r2.models_matrix_div_lasso[i,2:13] = r2.models[,2]
r2.models_matrix_pd_lasso[i,2:13]  = r2.models[,3]
r2.models_matrix_pe_lasso[i,2:13]  = r2.models[,4]
r2.models_matrix_ret_lasso[i,2:13] = r2.models[,5]



model.estimates_elnet = estimate_model(data = df.freq,data.pc = df.all.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, 
                                       model = "constrained", time.col = "quarter_id", n_pc = n_pc, data.freq = data_freq, model.pc = "full", 
                                       split.ratio = splitratio, lags = 1, pca = TRUE, rolling.pca = rolling.pca, where.funcs  = path_functions,
                                       optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha= 0.5, pen.search = pen_search), 
                                       sparse.pca = T, alpha.pca = grid_sparse_pc[i], beta.pca = 0, t1 = t1, t2 = t2, extra = "profitability")

r2.models <- model.estimates_elnet$r2; print(r2.models)

r2.models_matrix_div_elnet[i,2:13] = r2.models[,2]
r2.models_matrix_pd_elnet[i,2:13]  = r2.models[,3]
r2.models_matrix_pe_elnet[i,2:13]  = r2.models[,4]
r2.models_matrix_ret_elnet[i,2:13] = r2.models[,5]

}




unc_oos = as.data.frame(cbind(r2.models_matrix_div_unc[,c('alpha','full')], r2.models_matrix_pd_unc[,c('full')], r2.models_matrix_pe_unc[,c('full')], r2.models_matrix_ret_unc[,c('full')]))
lasso_oos = as.data.frame(cbind(r2.models_matrix_div_lasso[,c('alpha','full')], r2.models_matrix_pd_lasso[,c('full')], r2.models_matrix_pe_lasso[,c('full')],r2.models_matrix_ret_lasso[,c('full')]))
ridge_oos = as.data.frame(cbind(r2.models_matrix_div_ridge[,c('alpha','full')], r2.models_matrix_pd_ridge[,c('full')], r2.models_matrix_pe_ridge[,c('full')],r2.models_matrix_ret_ridge[,c('full')]))
elnet_oos = as.data.frame(cbind(r2.models_matrix_div_elnet[,c('alpha','full')], r2.models_matrix_pd_elnet[,c('full')], r2.models_matrix_pe_elnet[,c('full')],r2.models_matrix_ret_elnet[,c('full')]))

colnames(unc_oos)   = c('alpha','d_gr_unc','p_d_unc','p_e_unc','ret_unc')
colnames(lasso_oos) = c('alpha','d_gr_las','p_d_las','p_e_las','ret_las')
colnames(ridge_oos) = c('alpha','d_gr_rid','p_d_rid','p_e_rid','ret_rid')
colnames(elnet_oos) = c('alpha','d_gr_eln','p_d_eln','p_e_eln','ret_eln')

R2_oos = merge(unc_oos,lasso_oos, by = c("alpha")); R2_oos = merge(R2_oos,ridge_oos, by = c("alpha")); R2_oos = merge(R2_oos,elnet_oos, by = c("alpha"))



fund_plt_1 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_unc, x= alpha, color = "log dividend growth"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_d_unc, x= alpha, color = "log price-to-dividend"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_e_unc, x= alpha, color = "log price-to-earnings"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= ret_unc, x= alpha, color = "log return"), linetype="solid",size=1., alpha=0.6) +
  scale_color_manual(values = c(
    'log dividend growth' = "green",
    'log price-to-dividend' = 'blue',
    'log price-to-earnings' = 'darkred',
    'log return' = "violet"
    )) +
  labs(color = '')+ 
  xlab("lambda_2") + ylab("Out-of-sample R2 unconstrained") +
  theme(legend.position = c(0.15,0.80)) + ylim(-1.3,1)

fund_plt_2 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_las, x= alpha, color = "log dividend growth"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_d_las, x= alpha, color = "log price-to-dividend"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_e_las, x= alpha, color = "log price-to-earnings"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= ret_las, x= alpha, color = "log return"), linetype="solid",size=1., alpha=0.6) +
  scale_color_manual(values = c(
    'log dividend growth' = "green",
    'log price-to-dividend' = 'blue',
    'log price-to-earnings' = 'darkred',
    'log return' = "violet"
  )) +
  labs(color = '')+ 
  xlab("lambda_2") + ylab("Out-of-sample R2 lasso") +
  theme(legend.position = c(0.15,0.80)) + ylim(-1.2,1)

fund_plt_3 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_rid, x= alpha, color = "log dividend growth"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_d_rid, x= alpha, color = "log price-to-dividend"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_e_rid, x= alpha, color = "log price-to-earnings"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= ret_rid, x= alpha, color = "log return"), linetype="solid",size=1., alpha=0.6) +
  scale_color_manual(values = c(
    'log dividend growth' = "green",
    'log price-to-dividend' = 'blue',
    'log price-to-earnings' = 'darkred',
    'log return' = "violet"
  )) +
  labs(color = '')+ 
  xlab("lambda_2") + ylab("Out-of-sample R2 ridge") +
  theme(legend.position = c(0.15,0.80)) + ylim(-1.2,1)

fund_plt_4 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_eln, x= alpha, color = "log dividend growth"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_d_eln, x= alpha, color = "log price-to-dividend"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= p_e_eln, x= alpha, color = "log price-to-earnings"), linetype="solid",size=1., alpha=0.6) +
  geom_line( mapping= aes(y= ret_eln, x= alpha, color = "log return"), linetype="solid",size=1., alpha=0.6) +
  scale_color_manual(values = c(
    'log dividend growth' = "green",
    'log price-to-dividend' = 'blue',
    'log price-to-earnings' = 'darkred',
    'log return' = "violet"
  )) +
  labs(color = '')+ 
  xlab("lambda_2") + ylab("Out-of-sample R2 elastic net") +
  theme(legend.position = c(0.15,0.80)) + ylim(-1.2,1)

ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, fund_plt_4, nrow = 2, ncol = 2, common.legend = T)





















unc_oos_fullextra = as.data.frame(cbind(r2.models_matrix_div_unc[,c('alpha','full&extra')], r2.models_matrix_pd_unc[,c('full&extra')], r2.models_matrix_pe_unc[,c('full&extra')], r2.models_matrix_ret_unc[,c('full&extra')]))
lasso_oos_fullextra = as.data.frame(cbind(r2.models_matrix_div_lasso[,c('alpha','full&extra')], r2.models_matrix_pd_lasso[,c('full&extra')], r2.models_matrix_pe_lasso[,c('full&extra')],r2.models_matrix_ret_lasso[,c('full&extra')]))
ridge_oos_fullextra = as.data.frame(cbind(r2.models_matrix_div_ridge[,c('alpha','full&extra')], r2.models_matrix_pd_ridge[,c('full&extra')], r2.models_matrix_pe_ridge[,c('full&extra')],r2.models_matrix_ret_ridge[,c('full&extra')]))
elnet_oos_fullextra = as.data.frame(cbind(r2.models_matrix_div_elnet[,c('alpha','full&extra')], r2.models_matrix_pd_elnet[,c('full&extra')], r2.models_matrix_pe_elnet[,c('full&extra')],r2.models_matrix_ret_elnet[,c('full&extra')]))


colnames(unc_oos_fullextra)   = c('alpha','d_gr_unc','p_d_unc','p_e_unc','ret_unc')
colnames(lasso_oos_fullextra) = c('alpha','d_gr_las','p_d_las','p_e_las','ret_las')
colnames(ridge_oos_fullextra) = c('alpha','d_gr_rid','p_d_rid','p_e_rid','ret_rid')
colnames(elnet_oos_fullextra) = c('alpha','d_gr_eln','p_d_eln','p_e_eln','ret_eln')

R2_oos = merge(unc_oos_fullextra,lasso_oos_fullextra, by = c("alpha")); R2_oos = merge(R2_oos,ridge_oos_fullextra, by = c("alpha")); R2_oos = merge(R2_oos,elnet_oos_fullextra, by = c("alpha"))




fund_plt_1 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= d_gr_unc, x= alpha, color = "Unconstrained"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= d_gr_las, x= alpha, color = "Lasso"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= d_gr_rid, x= alpha, color = "Ridge"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= d_gr_eln, x= alpha, color = "Elastic-net"), linetype="solid",size=1.25, alpha=0.6) +
  scale_color_manual(values = c(
    'Unconstrained' = 'blue',
    'Lasso' = 'darkred',
    'Elastic-net' = 'darkgoldenrod1',
    "Ridge" = 'darkgreen')) + labs(color = '')+ 
  xlab(expression(lambda[2])) + ylab("oos R2 dividend growth") + ylim(c(0.2,0.5)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))

fund_plt_2 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= p_d_unc, x= alpha, color = "Unconstrained"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= p_d_las, x= alpha, color = "Lasso"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= p_d_rid, x= alpha, color = "Ridge"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= p_d_eln, x= alpha, color = "Elastic-net"), linetype="solid",size=1.25, alpha=0.6) +
  scale_color_manual(values = c(
    'Unconstrained' = 'blue',
    'Lasso' = 'darkred',
    'Elastic-net' = 'darkgoldenrod1',
    "Ridge" = 'darkgreen')) + labs(color = '')+ 
  xlab(expression(lambda[2])) + ylab("oos R2 log price-to-dividend") + ylim(c(0.65,0.85)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))


fund_plt_3 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= p_e_unc, x= alpha, color = "Unconstrained"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= p_e_las, x= alpha, color = "Lasso"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= p_e_rid, x= alpha, color = "Ridge"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= p_e_eln, x= alpha, color = "Elastic-net"), linetype="solid",size=1.25, alpha=0.6) +
  scale_color_manual(values = c(
    'Unconstrained' = 'blue',
    'Lasso' = 'darkred',
    'Elastic-net' = 'darkgoldenrod1',
    "Ridge" = 'darkgreen')) + labs(color = '')+ 
  xlab(expression(lambda[2])) + ylab("oos R2 log price-to-earning") + ylim(c(-0.4,0.4)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))


fund_plt_4 = ggplot(data = R2_oos) + theme_bw() +
  geom_line( mapping= aes(y= ret_unc, x= alpha, color ="Unconstrained"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= ret_las, x= alpha, color = "Lasso"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= ret_rid, x= alpha, color = "Ridge"), linetype="solid",size=1.25, alpha=0.6) +
  geom_line( mapping= aes(y= ret_eln, x= alpha, color = "Elastic-net"), linetype="solid",size=1.25, alpha=0.6) +
  scale_color_manual(values = c(
    'Unconstrained' = 'blue',
    'Lasso' = 'darkred',
    'Elastic-net' = 'darkgoldenrod1',
    "Ridge" = 'darkgreen')) + labs(color = '')+ 
  xlab(expression(lambda[2])) + ylab("oos R2 return") + ylim(c(-1.,0.5)) +
  theme(legend.position = c(0.85,0.85), legend.text = element_text(size=13))


ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, fund_plt_4, nrow = 2, ncol = 2, common.legend = T)



vars_unc_oos =  model.estimates.unc$real.pred.matrix
vars_lasso_oos  = model.estimates_lasso$real.pred.matrix
vars_ridge_oos  = model.estimates.ridge$real.pred.matrix
vars_elnet_oos  = model.estimates_elnet$real.pred.matrix



var_full_oos = merge(vars_unc_oos$full_profitability,vars_lasso_oos$full_profitability, by = c("quarter_id", paste0(c(DEPVAR, "PC1_full","PC2_full","PC1_profitability","PC2_profitability"),"_real")), suffixes = c("_unc","_lasso"))

var_full_oos_2 = merge(vars_elnet_oos$full_profitability,vars_ridge_oos$full_profitability, by = c("quarter_id", paste0(c(DEPVAR, "PC1_full","PC2_full","PC1_profitability","PC2_profitability"),"_real")), suffixes = c("_elnet","_ridge"))

var_full_oos = merge(var_full_oos,var_full_oos_2, by = c("quarter_id", paste0(c(DEPVAR, "PC1_full","PC2_full","PC1_profitability","PC2_profitability"),"_real")))

rm(var_full_oos_2)

names(var_full_oos) <- gsub("_real", "", names(var_full_oos))


source(file=paste0(path_functions,"plot_is_sp_fabio.R"), chdir = T)

plot_is(var_full_oos, data_freq)




### Variance of individual predictions 

ll_matrix = as.data.frame(matrix(NA, nrow = nrow(var_hat_ridge$depvar), ncol =  length(CATEGORIES)+1))

colnames(ll_matrix) = c("quarter_id","depvar", names(CATEGORIES)[-10])

ll_matrix[,'quarter_id'] = var_hat_unc$depvar[,'quarter_id']

Sigma = cov(df.freq[, paste0(DEPVAR)], use ="pairwise.complete.obs")

i=j=1

for (i in 1:NROW(ll_matrix)) {
  for (j in 1:(ncol(ll_matrix)-1)) {
    
    sigma_data_temp = df.freq[df.freq[,'quarter_id'] >= as.Date("1978-12-31") & df.freq[,'quarter_id'] < as.Date("1998-12-31") + months(12*i)  , c("quarter_id",DEPVAR)]
    Sigma = cov(sigma_data_temp[, paste0(DEPVAR)], use ="pairwise.complete.obs")
    ll_matrix[i,1+j] = as.numeric(dmvnorm( x = var_hat_unc[[j]][i, paste0(DEPVAR,"_real")] , mean = as.numeric(var_hat_ridge[[j]][i,paste0(DEPVAR,"_pred")]), sigma = Sigma, log = T))
    
  }
}


w_0 = rep(1/(ncol(ll_matrix)-1),ncol(ll_matrix)-1) ## initial distribution

posterior_function  = function(w,ll_matrix)
  
{
  w_1 = as.numeric(as.numeric(w*exp(ll_matrix)) / as.numeric(exp(ll_matrix)) %*% as.numeric(w))
  
  return(w_1 = w_1)
  
}


w_t <- as.data.frame(matrix(NA, nrow = nrow(ll_matrix)+1, ncol = NCOL(ll_matrix)))

w_t[-1,1] = as.character(var_hat_unc$depvar[,'quarter_id'])

colnames(w_t) = c("quarter_id","depvar", names(CATEGORIES)[-10])

w_t[1,-1] = w_0

for (j in 1:nrow(ll_matrix)) {
  
  w_t[1+j,-1] =  posterior_function(as.numeric(w_t[j,-1]),as.numeric(ll_matrix[j,-1]))
  
}

w_t[1,1] = as.character(as.Date(w_t[2,1]) - months(12))

apply(w_t[,-1], 1, max)







w_t[,1] = as.Date(w_t[,1])
recessions.trim = subset(recessions.df, Peak >= min(w_t$quarter_id))

fund_plt_1 = ggplot(data=w_t) + theme_bw()+
  geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4) +
  geom_line( mapping= aes(y= depvar , x= quarter_id, color = "Without Pcs"), linetype="solid",size=1.) +
  geom_line( mapping= aes(y= capitalization   , x= quarter_id, color = "Capitalization"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= efficiency  , x= quarter_id, color = "Efficiency"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= financial_soundness, x= quarter_id, color = "Financial Soundness"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= liquidity, x= quarter_id, color = "Liquidity"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= profitability, x= quarter_id, color = "Profitability"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= solvency, x= quarter_id, color = "Solvency"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= valuation, x= quarter_id, color = "Valuation"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= equity_premium, x= quarter_id, color = "Equity Premium"), linetype="solid",size=1) +
  geom_line( mapping= aes(y= other, x= quarter_id, color = "Other"), linetype="solid",size=1) +
  labs(color = '')+ 
  xlab("time") + ylab("posterior probability")+
  theme(legend.position = c(0.20,0.80))
ggarrange(fund_plt_1,  nrow = 1, ncol = 1, common.legend = T)











var_hat_mix = as.data.frame( matrix(NA,nrow = nrow(ll_matrix), ncol = ncol(var_hat_ridge$depvar)))

colnames(var_hat_mix) = colnames(var_hat_ridge$depvar)

var_hat_mix[,c('quarter_id', paste0(DEPVAR,"_real"))] = var_hat_ridge$depvar[,c('quarter_id', paste0(DEPVAR,"_real"))]


for (i in 1:nrow(ll_matrix)) {
  for (j in 1:(ncol(ll_matrix)-1)) {
    
    if (j==1) {
      
      var_hat_mix[i,paste0(DEPVAR,"_pred")] = as.numeric(var_hat_ridge[[j]][i,paste0(DEPVAR,"_pred")])*w_t[i,j+1]
      
    }else{
      
      var_hat_mix[i,paste0(DEPVAR,"_pred")] = var_hat_mix[i,paste0(DEPVAR,"_pred")] + as.numeric(var_hat_ridge[[j]][i,paste0(DEPVAR,"_pred")])*w_t[i,j+1]
      
    }
    
  }
}

r2.univeriate_bmix <- data.frame(matrix(ncol=(length(DEPVAR)), nrow=1)) 
colnames(r2.univeriate_bmix) <- c(DEPVAR)

r2.univeriate_bmix[,DEPVAR[1]]  = 1- sum((var_hat_mix[,paste0(DEPVAR[1],"_real")] - var_hat_mix[,paste0(DEPVAR[1],'_pred')])**2, na.rm = T) /  sum((var_hat_mix[,paste0(DEPVAR[1],"_real")] - mean(var_hat_mix[,paste0(DEPVAR[1],"_real")]) )**2)
r2.univeriate_bmix[,DEPVAR[2]]  = 1- sum((var_hat_mix[,paste0(DEPVAR[2],"_real")] - var_hat_mix[,paste0(DEPVAR[2],'_pred')])**2, na.rm = T) /  sum((var_hat_mix[,paste0(DEPVAR[2],"_real")] - mean(var_hat_mix[,paste0(DEPVAR[2],"_real")]) )**2)
r2.univeriate_bmix[,DEPVAR[3]]  = 1- sum((var_hat_mix[,paste0(DEPVAR[3],"_real")] - var_hat_mix[,paste0(DEPVAR[3],'_pred')])**2, na.rm = T) /  sum((var_hat_mix[,paste0(DEPVAR[3],"_real")] - mean(var_hat_mix[,paste0(DEPVAR[3],"_real")]) )**2)
r2.univeriate_bmix[,DEPVAR[4]]  = 1- sum((var_hat_mix[,paste0(DEPVAR[4],"_real")] - var_hat_mix[,paste0(DEPVAR[4],'_pred')])**2, na.rm = T) /  sum((var_hat_mix[,paste0(DEPVAR[4],"_real")] - mean(var_hat_mix[,paste0(DEPVAR[4],"_real")]) )**2)


print(r2.univeriate_bmix)



######################################################################################################
# decompositions in-sample & univariate results
######################################################################################################

# Table 3
source(file =paste0(path_functions,"table_3.R"), chdir = T)
table_3 = table_3(df.all.freq, overlapping = FALSE)

# Univariate OLS: in sample and out of sample

source(file =paste0(path_functions,"ols_univariate_v2.R"), chdir = T)
ols_univ = ols_univariate(df.all.freq, splitratio = splitratio, CHARS, data.freq = "q")

ols_univ$plot_is
ols_univ$plot_oos

r2.ols_oos = ols_univ$r2.oos
r2.ols_oos_grounded = r2.ols_oos  %>% 
  mutate(across(where(is.numeric), ~if_else(. < 0, 0, .))) %>% 
  drop_na() %>% 
  as.data.frame()


##############
#### Figure 12
##############


alpha_pc = seq(0,0.001,length.out = 20)
  temp_df_pc_y = na.omit(df.all.freq[df.all.freq[,'month'] == 12,CHARS])
temp_df_pc_q = na.omit(df.all.freq[,CHARS])

loadings_spc1 = as.data.frame(matrix(data = NA, nrow = length(alpha_pc), ncol = (1+length(CATEGORIES[["full"]]))))
loadings_spc2 = as.data.frame(matrix(data = NA, nrow = length(alpha_pc), ncol = (1+length(CATEGORIES[["full"]]))))
loadings_spc3 = as.data.frame(matrix(data = NA, nrow = length(alpha_pc), ncol = (1+length(CATEGORIES[["full"]]))))
loadings_spc4 = as.data.frame(matrix(data = NA, nrow = length(alpha_pc), ncol = (1+length(CATEGORIES[["full"]]))))

colnames(loadings_spc1) = colnames(loadings_spc2) = colnames(loadings_spc3) = colnames(loadings_spc4) = c("alpha", CATEGORIES[["full"]]) 

for (i in 1:length(alpha_pc)) {
  
  loadings_spc1[i,1] = loadings_spc2[i,1] = loadings_spc3[i,1] = loadings_spc4[i,1] = alpha_pc[i]
  loadings_spc = spca(temp_df_pc_y[CATEGORIES[["full"]]], center = T, scale = T, verbose = F, alpha = alpha_pc[i])
  loadings_spc1[i,-1] = as.numeric(loadings_spc$loadings[,1])
  loadings_spc2[i,-1] = as.numeric(loadings_spc$loadings[,2])
  loadings_spc3[i,-1] = as.numeric(loadings_spc$loadings[,3])
  loadings_spc4[i,-1] = as.numeric(loadings_spc$loadings[,4])
  
}


apply(loadings_spc1 == 0, 1, count)
apply(loadings_spc1[CATEGORIES[[1]]] == 0, 1, count)/ length(CATEGORIES[[1]])
apply(loadings_spc1[CATEGORIES[[2]]] == 0, 1, count)/ length(CATEGORIES[[2]])
apply(loadings_spc1[CATEGORIES[[3]]] == 0, 1, count)/ length(CATEGORIES[[3]])
apply(loadings_spc1[CATEGORIES[[4]]] == 0, 1, count)/ length(CATEGORIES[[4]])
apply(loadings_spc1[CATEGORIES[[5]]] == 0, 1, count)/ length(CATEGORIES[[5]])

spc1_plot <- expand.grid(lambda_2=loadings_spc1[,1], predictor=colnames(loadings_spc1[,-1]))
spc1_plot$loading_Pc1 <- as.numeric(as.matrix(loadings_spc1[,-1]))

spc1_plot = ggplot(spc1_plot, aes(lambda_2, predictor, fill= loading_Pc1)) +   scale_y_discrete(limits = rev(levels(CHARS)))+
  geom_tile()+ theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", space = "rgb", guide = "colourbar")

spc2_plot <- expand.grid(lambda_2=loadings_spc2[,1], predictor=colnames(loadings_spc2[,-1]))
spc2_plot$loading_Pc2 <- as.numeric(as.matrix(loadings_spc2[,-1]))

spc2_plot = ggplot(spc2_plot, aes(lambda_2, predictor, fill= loading_Pc2)) +   scale_y_discrete(limits = rev(levels(CHARS)))+
  geom_tile()+ theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", space = "rgb", guide = "colourbar")

spc3_plot <- expand.grid(lambda_2=loadings_spc3[,1], predictor=colnames(loadings_spc3[,-1]))
spc3_plot$loading_Pc3 <- as.numeric(as.matrix(loadings_spc3[,-1]))

spc3_plot = ggplot(spc3_plot, aes(lambda_2, predictor, fill= loading_Pc3)) +   scale_y_discrete(limits = rev(levels(CHARS)))+
  geom_tile()+ theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", space = "rgb", guide = "colourbar")

spc4_plot <- expand.grid(lambda_2=loadings_spc4[,1], predictor=colnames(loadings_spc4[,-1]))
spc4_plot$loading_Pc4 <- as.numeric(as.matrix(loadings_spc4[,-1]))

spc4_plot = ggplot(spc4_plot, aes(lambda_2, predictor, fill= loading_Pc4)) +   scale_y_discrete(limits = rev(levels(CHARS)))+
  geom_tile()+ theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", space = "rgb", guide = "colourbar")

ggarrange(spc1_plot, spc2_plot, spc3_plot, spc4_plot, nrow = 2, ncol = 2, common.legend = F)







######################################################################################################
# to test the estimate_model function:
######################################################################################################
data <- df.freq
data.pc <- df.all.freq
dep.vars <- DEPVAR
add.vars <- CHARS
pca.cat.dict <- CATEGORIES
model <- "constrained"
model.pc = "full&extra"
time.col <- "quarter_id"
data.freq <- data_freq
split.ratio <- splitratio
lags <- lags
optional.args = list(sep.lambdas=TRUE, fit.intercept=fit_intercept, alpha=1)
where.funcs = path_functions
sparse.pca = T
alpha.pca = 4.5e-5  #1e-3
beta.pca = 0



model.estimates <- estimate_model(data = df.freq, data.pc = df.all.freq, dep.vars = DEPVAR, add.vars = CHARS, pca.cat.dict = CATEGORIES, model = "lasso_var", time.col = "quarter_id", data.freq = data_freq, split.ratio = splitratio, lags = lags, pca = pca, rolling.pca, where.funcs = path_functions, optional.args = list(sep.lambdas=sep_lambdas, fit.intercept=fit_intercept, alpha=alpha), robust.pca = T, alpha.pca = alpha_pca)

