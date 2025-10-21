library("lubridate") 

# data = df.all.freq; splitratio = splitratio; CHARS = CHARS; data.freq = "m"

ols_univariate = function(data, CHARS, data.freq = "m", splitratio, oos_ols = F) {

  CHARS = c(CHARS,"p_d", "p_e","e_d", "d_gr_1y", "e_gr_1y","ret_1y")
  
  DEPVAR <- c('d_gr_1y_lead','p_d_1y_lead','e_d_1y_lead','p_e_1y_lead','ret_1y_lead','e_gr_1y_lead','d_gr_3y_lead','p_d_3y_lead','e_d_3y_lead','p_e_3y_lead','ret_3y_lead','e_gr_3y_lead','d_gr_5y_lead','p_d_5y_lead','e_d_5y_lead','p_e_5y_lead','ret_5y_lead','e_gr_5y_lead')
  
  X = data[,c('month_id',CHARS)]
  
  Y = data[,c('month_id',DEPVAR)]


  if (data.freq == "m") {
    
    Y[,c('month_id')] = (as.Date(data[,c('month_id')]) + months(3)) 
    
  } 
  
  if (data.freq == "y") {
    
    Y[,c('quarter_id')] = (as.Date(data[,c('quarter_id')]) + months(12)) 
    
  }
  


  columns_output = c("predictor","intercept_1y","beta_1y","t_stat_intercept_1y","t_stat_beta_1y","r2_1y","prediction_var_1y","intercept_3y","beta_3y","t_stat_intercept_3y","t_stat_beta_3y","r2_3y","prediction_var_3y","intercept_5y","beta_5y","t_stat_intercept_5y","t_stat_beta_5y","r2_5y","prediction_var_5y")
    


  
  outputs_d_gr = as.data.frame(matrix(NA,ncol = length(columns_output), nrow = (NCOL(X)-1)))
  outputs_e_gr = as.data.frame(matrix(NA,ncol = length(columns_output), nrow = (NCOL(X)-1)))
  outputs_p_d  = as.data.frame(matrix(NA,ncol = length(columns_output), nrow = (NCOL(X)-1)))
  outputs_e_d  = as.data.frame(matrix(NA,ncol = length(columns_output), nrow = (NCOL(X)-1)))
  outputs_ret  = as.data.frame(matrix(NA,ncol = length(columns_output), nrow = (NCOL(X)-1)))
  outputs_p_e  = as.data.frame(matrix(NA,ncol = length(columns_output), nrow = (NCOL(X)-1)))
  
  colnames(outputs_d_gr) = colnames(outputs_e_gr) = colnames(outputs_p_d) = colnames(outputs_e_d) = colnames(outputs_ret) =  colnames(outputs_p_e) = columns_output
  
 i=1
  for (i in 1:NCOL(subset( X, select = -month_id ))) {
    
    
    model = lm(as.matrix(Y[,-1]) ~ X[,i+1])
    
    outputs_d_gr[i,1] = outputs_e_gr[i,1] = outputs_p_d[i,1] = outputs_p_e[i,1] = outputs_e_d[i,1] = outputs_ret[i,1] = colnames(X)[i+1]
    
    
    
    outputs_d_gr[i,c(2:3,8:9,14:15)] = as.numeric(model$coefficients[,DEPVAR[c(1,7,13)]])
    
    outputs_d_gr[i,c(4:5,10:11,16:17)] = c(coef(summary(model))[[1]][, "t value"],coef(summary(model))[[7]][, "t value"],coef(summary(model))[[13]][, "t value"])
    
    outputs_d_gr[i,c(6,12,18)] = c(summary(model)[[1]]$r.squared, summary(model)[[7]]$r.squared, summary(model)[[13]]$r.squared)
    
    outputs_d_gr[i,c(7,13,19)] = c(var(predict(model)[,DEPVAR[1]]), var(predict(model)[,DEPVAR[7]]), var(predict(model)[,DEPVAR[13]]))
    
    
    
    outputs_p_d[i,c(2:3,8:9,14:15)] = as.numeric(model$coefficients[,DEPVAR[c(2,8,14)]])
    
    outputs_p_d[i,c(4:5,10:11,16:17)] = c(coef(summary(model))[[2]][, "t value"],coef(summary(model))[[8]][, "t value"],coef(summary(model))[[14]][, "t value"])
    
    outputs_p_d[i,c(6,12,18)] = c(summary(model)[[2]]$r.squared, summary(model)[[8]]$r.squared, summary(model)[[14]]$r.squared)
    
    outputs_p_d[i,c(7,13,19)] = c(var(predict(model)[,DEPVAR[2]]), var(predict(model)[,DEPVAR[8]]), var(predict(model)[,DEPVAR[14]]))
    
    
    
    outputs_e_d[i,c(2:3,8:9,14:15)] = as.numeric(model$coefficients[,DEPVAR[c(3,9,15)]])
    
    outputs_e_d[i,c(4:5,10:11,16:17)] = c(coef(summary(model))[[3]][, "t value"],coef(summary(model))[[9]][, "t value"],coef(summary(model))[[15]][, "t value"])
    
    outputs_e_d[i,c(6,12,18)] = c(summary(model)[[3]]$r.squared, summary(model)[[9]]$r.squared, summary(model)[[15]]$r.squared)
    
    outputs_e_d[i,c(7,13,19)] = c(var(predict(model)[,DEPVAR[3]]), var(predict(model)[,DEPVAR[9]]), var(predict(model)[,DEPVAR[15]]))
    
    
    
    outputs_p_e[i,c(2:3,8:9,14:15)] = as.numeric(model$coefficients[,DEPVAR[c(4,10,16)]])
    
    outputs_p_e[i,c(4:5,10:11,16:17)] = c(coef(summary(model))[[4]][, "t value"],coef(summary(model))[[10]][, "t value"],coef(summary(model))[[16]][, "t value"])
    
    outputs_p_e[i,c(6,12,18)] = c(summary(model)[[4]]$r.squared, summary(model)[[10]]$r.squared, summary(model)[[16]]$r.squared)
    
    outputs_p_e[i,c(7,13,19)] = c(var(predict(model)[,DEPVAR[4]]), var(predict(model)[,DEPVAR[10]]), var(predict(model)[,DEPVAR[16]]))
    
    
    
    outputs_ret[i,c(2:3,8:9,14:15)] = as.numeric(model$coefficients[,DEPVAR[c(5,11,17)]])
    
    outputs_ret[i,c(4:5,10:11,16:17)] = c(coef(summary(model))[[5]][, "t value"],coef(summary(model))[[11]][, "t value"],coef(summary(model))[[17]][, "t value"])
    
    outputs_ret[i,c(6,12,18)] = c(summary(model)[[5]]$r.squared, summary(model)[[11]]$r.squared, summary(model)[[17]]$r.squared)
    
    outputs_ret[i,c(7,13,19)] = c(var(predict(model)[,DEPVAR[5]]), var(predict(model)[,DEPVAR[11]]), var(predict(model)[,DEPVAR[17]]))
    
    
    
    outputs_e_gr[i,c(2:3,8:9,14:15)] = as.numeric(model$coefficients[,DEPVAR[c(6,12,18)]])
    
    outputs_e_gr[i,c(4:5,10:11,16:17)] = c(coef(summary(model))[[6]][, "t value"],coef(summary(model))[[12]][, "t value"],coef(summary(model))[[18]][, "t value"])
    
    outputs_e_gr[i,c(6,12,18)] = c(summary(model)[[6]]$r.squared, summary(model)[[12]]$r.squared, summary(model)[[18]]$r.squared)
    
    outputs_e_gr[i,c(7,13,19)] = c(var(predict(model)[,DEPVAR[6]]), var(predict(model)[,DEPVAR[12]]), var(predict(model)[,DEPVAR[18]]))
      
  }

  
    
  d_gr_r2_plot = ggplot(outputs_d_gr, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) + 
    geom_line(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_1y, group = 1), fill="red", alpha=0.3) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) + 
    geom_line(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_3y,  group = 1), fill="darkgreen", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "3 years", group = 1)) + 
    geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_5y,  group = 1), fill="blue", alpha=0.2) + 
    scale_x_discrete(guide = guide_axis(angle = 90), labels = c(
      "Capitalization Ratio", "Common Equity/Invested Capital", "Long-term Debt/Invested Capital","Total Debt/Invested Capital",
      "Asset Turnover", "Inventory Turnover", "Payables Turnover", "Receivables Turnover", "Sales/Stockholders Equity", "Sales/Invested Capital", "Sales/Working Capital",
      "Inventory/Current Assets","Receivables/Current Assets","Free Cash Flow/Operating Cash Flow","Operating CF/Current Liabilities","Cash Flow/Total Debt","Cash Balance/Total Liabilities","Cash Flow Margin","Short-Term Debt/Total Debt","Profit Before Depreciation/Current Liabilities","Current Liabilities/Total Liabilities","Total Debt/EBITDA","Long-term Debt/Book Equity","Interest/Average Long-term Debt","Interest/Average Total Debt", "Long-term Debt/Total Liabilities","Total Liabilities/Total Tangible Assets",
      "Cash Conversion Cycle (Days)","Cash Ratio","Current Ratio", "Quick Ratio (Acid Test)", 
      "Effective Tax Rate","Gross Profit/Total Assets","After-tax Return on Average Common Equity","After-tax Return on Total Stockholders’ Equity","After-tax Return on Invested Capital", "Gross Profit Margin","Net Profit Margin","Operating Profit Margin After Depreciation","Operating Profit Margin Before Depreciation","Pre-tax Return on Total Earning Assets","Pre-tax return on Net Operating Assets","Pre-tax Profit Margin","Return on Assets","Return on Capital Employed","Return on Equity",
      "Total Debt/Equity","Total Liabilities/Total Assets","Total Debt/Capital","After-tax Interest Coverage","Interest Coverage Ratio",
      "Dividend Payout Ratio","Trailing P/E to Growth (PEG) ratio","Book/Market","Shillers Cyclically Adjusted P/E Ratio","Enterprise Value Multiple","Price/Cash flow","P/E (Diluted, Excl. EI)","P/E (Diluted, Incl. EI)","Price/Operating Earnings (Basic, Excl. EI)","Price/Operating Earnings (Diluted, Excl. EI)","Price/Sales","Price/Book", 
      "ntis", "tbl", "tms", "svar","dfy", "siiOutOfSample",
      "Accruals/Average Assets","Research and Development/Sales","Avertising Expenses/Sales","Labor Expenses/Sales",
      "price-to-dividend","price-to-earning","earning-to-dividend",'lagged dividend growth (1-year)','lagged earnings growth (1-year)','lagged price growth (1-year)','lagged dividend growth (3-years)','lagged earnings growth (3-years)','lagged price growth (3-years)'
      ) ) +
    labs(title = sprintf("in-sample (log) dividend growth"))+
    xlab("") + ylab("in-sample R2") +
    scale_color_manual(values = c("1 year" = "red", "3 years" = "darkgreen", '5 years' = 'blue'))+ ylim(c(0,0.50)) +
    labs(color = '') +  theme(legend.position = c(0.05,0.65), legend.text = element_text(size=13))
  
  d_gr_r2_plot = d_gr_r2_plot +
    # Add vertical lines
    geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") + # After "totdebt_invcap"
    geom_vline(xintercept = 11.5, linetype = "dashed", color = "black") + # After "sale_nwc"
    geom_vline(xintercept = 17.5, linetype = "dashed", color = "black") + # After "lt_ppent"
    geom_vline(xintercept = 23.5, linetype = "dashed", color = "black") + # After "quick_ratio"
    geom_vline(xintercept = 47.5, linetype = "dashed", color = "black") + # After "roe"
    geom_vline(xintercept = 54.5, linetype = "dashed", color = "black") + # After "intcov_ratio"
    geom_vline(xintercept = 62.5, linetype = "dashed", color = "black") + # After "ptb"
    geom_vline(xintercept = 69.5, linetype = "dashed", color = "black") + # After "siioutofsample"
    geom_vline(xintercept = 75.5, linetype = "dashed", color = "black") + # After "staff_sale"
    # Add labels for the sections
    annotate("text", x = 4.5, y = 0.55, label = "Capitalization", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 11.5, y = 0.55, label = "Sales & Working Capital", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 17.5, y = 0.55, label = "Long-term Debt", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 23.5, y = 0.55, label = "Quick Ratio", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 47.5, y = 0.55, label = "Return on Equity", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 54.5, y = 0.55, label = "Interest Coverage", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 62.5, y = 0.55, label = "Price to Book", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 69.5, y = 0.55, label = "Out of Sample", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 75.5, y = 0.55, label = "Staff to Sales", size = 4, angle = 90, vjust = -0.5) +
    theme(plot.margin = margin(20, 10, 20, 10)) # Adjust margins
  
  e_gr_r2_plot = ggplot(outputs_e_gr, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_1y, group = 1), fill="red", alpha=0.3) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_3y,  group = 1), fill="darkgreen", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_5y,  group = 1), fill="blue", alpha=0.3) + 
    scale_x_discrete(guide = guide_axis(angle = 90), labels = c(
      "Capitalization Ratio", "Common Equity/Invested Capital", "Long-term Debt/Invested Capital","Total Debt/Invested Capital",
      "Asset Turnover", "Inventory Turnover", "Payables Turnover", "Receivables Turnover", "Sales/Stockholders Equity", "Sales/Invested Capital", "Sales/Working Capital",
      "Inventory/Current Assets","Receivables/Current Assets","Free Cash Flow/Operating Cash Flow","Operating CF/Current Liabilities","Cash Flow/Total Debt","Cash Balance/Total Liabilities","Cash Flow Margin","Short-Term Debt/Total Debt","Profit Before Depreciation/Current Liabilities","Current Liabilities/Total Liabilities","Total Debt/EBITDA","Long-term Debt/Book Equity","Interest/Average Long-term Debt","Interest/Average Total Debt", "Long-term Debt/Total Liabilities","Total Liabilities/Total Tangible Assets",
      "Cash Conversion Cycle (Days)","Cash Ratio","Current Ratio", "Quick Ratio (Acid Test)", 
      "Effective Tax Rate","Gross Profit/Total Assets","After-tax Return on Average Common Equity","After-tax Return on Total Stockholders’ Equity","After-tax Return on Invested Capital", "Gross Profit Margin","Net Profit Margin","Operating Profit Margin After Depreciation","Operating Profit Margin Before Depreciation","Pre-tax Return on Total Earning Assets","Pre-tax return on Net Operating Assets","Pre-tax Profit Margin","Return on Assets","Return on Capital Employed","Return on Equity",
      "Total Debt/Equity","Total Liabilities/Total Assets","Total Debt/Capital","After-tax Interest Coverage","Interest Coverage Ratio",
      "Dividend Payout Ratio","Trailing P/E to Growth (PEG) ratio","Book/Market","Shillers Cyclically Adjusted P/E Ratio","Enterprise Value Multiple","Price/Cash flow","P/E (Diluted, Excl. EI)","P/E (Diluted, Incl. EI)","Price/Operating Earnings (Basic, Excl. EI)","Price/Operating Earnings (Diluted, Excl. EI)","Price/Sales","Price/Book", 
      "ntis", "tbl", "tms", "svar","dfy", "siiOutOfSample",
      "Accruals/Average Assets","Research and Development/Sales","Avertising Expenses/Sales","Labor Expenses/Sales",
      "price-to-dividend","price-to-earning","earning-to-dividend",'lagged dividend growth (1-year)','lagged earnings growth (1-year)','lagged price growth (1-year)','lagged dividend growth (3-years)','lagged earnings growth (3-years)','lagged price growth (3-years)'
    ) ) +
    xlab("predictor") + ylab("in-sample R2") +
    scale_color_manual(values = c("1 year" = "red", '5 years' = 'blue'))+
    labs(color = '') +  theme(legend.position = c(0.05,0.65))
  
  
  e_gr_r2_plot = e_gr_r2_plot +
    # Add vertical lines
    geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") + # After "totdebt_invcap"
    geom_vline(xintercept = 11.5, linetype = "dashed", color = "black") + # After "sale_nwc"
    geom_vline(xintercept = 17.5, linetype = "dashed", color = "black") + # After "lt_ppent"
    geom_vline(xintercept = 23.5, linetype = "dashed", color = "black") + # After "quick_ratio"
    geom_vline(xintercept = 47.5, linetype = "dashed", color = "black") + # After "roe"
    geom_vline(xintercept = 54.5, linetype = "dashed", color = "black") + # After "intcov_ratio"
    geom_vline(xintercept = 62.5, linetype = "dashed", color = "black") + # After "ptb"
    geom_vline(xintercept = 69.5, linetype = "dashed", color = "black") + # After "siioutofsample"
    geom_vline(xintercept = 75.5, linetype = "dashed", color = "black") + # After "staff_sale"
    # Add labels for the sections
    annotate("text", x = 4.5, y = 0.55, label = "Capitalization", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 11.5, y = 0.55, label = "Sales & Working Capital", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 17.5, y = 0.55, label = "Long-term Debt", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 23.5, y = 0.55, label = "Quick Ratio", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 47.5, y = 0.55, label = "Return on Equity", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 54.5, y = 0.55, label = "Interest Coverage", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 62.5, y = 0.55, label = "Price to Book", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 69.5, y = 0.55, label = "Out of Sample", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 75.5, y = 0.55, label = "Staff to Sales", size = 4, angle = 90, vjust = -0.5) +
    theme(plot.margin = margin(20, 10, 20, 10)) # Adjust margins
  
  
  
  ret_r2_plot = ggplot(outputs_ret, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_1y, group = 1), fill="red", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_3y,  group = 1), fill="darkgreen", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_5y,  group = 1), fill="blue", alpha=0.2) + 
    scale_x_discrete(guide = guide_axis(angle = 90), labels = c(
      "Capitalization Ratio", "Common Equity/Invested Capital", "Long-term Debt/Invested Capital","Total Debt/Invested Capital",
      "Asset Turnover", "Inventory Turnover", "Payables Turnover", "Receivables Turnover", "Sales/Stockholders Equity", "Sales/Invested Capital", "Sales/Working Capital",
      "Inventory/Current Assets","Receivables/Current Assets","Free Cash Flow/Operating Cash Flow","Operating CF/Current Liabilities","Cash Flow/Total Debt","Cash Balance/Total Liabilities","Cash Flow Margin","Short-Term Debt/Total Debt","Profit Before Depreciation/Current Liabilities","Current Liabilities/Total Liabilities","Total Debt/EBITDA","Long-term Debt/Book Equity","Interest/Average Long-term Debt","Interest/Average Total Debt", "Long-term Debt/Total Liabilities","Total Liabilities/Total Tangible Assets",
      "Cash Conversion Cycle (Days)","Cash Ratio","Current Ratio", "Quick Ratio (Acid Test)", 
      "Effective Tax Rate","Gross Profit/Total Assets","After-tax Return on Average Common Equity","After-tax Return on Total Stockholders’ Equity","After-tax Return on Invested Capital", "Gross Profit Margin","Net Profit Margin","Operating Profit Margin After Depreciation","Operating Profit Margin Before Depreciation","Pre-tax Return on Total Earning Assets","Pre-tax return on Net Operating Assets","Pre-tax Profit Margin","Return on Assets","Return on Capital Employed","Return on Equity",
      "Total Debt/Equity","Total Liabilities/Total Assets","Total Debt/Capital","After-tax Interest Coverage","Interest Coverage Ratio",
      "Dividend Payout Ratio","Trailing P/E to Growth (PEG) ratio","Book/Market","Shillers Cyclically Adjusted P/E Ratio","Enterprise Value Multiple","Price/Cash flow","P/E (Diluted, Excl. EI)","P/E (Diluted, Incl. EI)","Price/Operating Earnings (Basic, Excl. EI)","Price/Operating Earnings (Diluted, Excl. EI)","Price/Sales","Price/Book", 
      "ntis", "tbl", "tms", "svar","dfy", "siiOutOfSample",
      "Accruals/Average Assets","Research and Development/Sales","Avertising Expenses/Sales","Labor Expenses/Sales",
      "price-to-dividend","price-to-earning","earning-to-dividend",'lagged dividend growth (1-year)','lagged earnings growth (1-year)','lagged price growth (1-year)','lagged dividend growth (3-years)','lagged earnings growth (3-years)','lagged price growth (3-years)'
    ) ) +
    labs(title = sprintf("in-sample (log) return"))+
    xlab("") + ylab("in-sample R2") +
    scale_color_manual(values = c("1 year" = "red", "3 years" = "darkgreen", '5 years' = 'blue'))+ ylim(c(0,0.50)) +
    labs(color = '') +  theme(legend.position = c(0.05,0.65), legend.text = element_text(size=13))
  
  ret_r2_plot = ret_r2_plot +
    # Add vertical lines
    geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") + # After "totdebt_invcap"
    geom_vline(xintercept = 11.5, linetype = "dashed", color = "black") + # After "sale_nwc"
    geom_vline(xintercept = 17.5, linetype = "dashed", color = "black") + # After "lt_ppent"
    geom_vline(xintercept = 23.5, linetype = "dashed", color = "black") + # After "quick_ratio"
    geom_vline(xintercept = 47.5, linetype = "dashed", color = "black") + # After "roe"
    geom_vline(xintercept = 54.5, linetype = "dashed", color = "black") + # After "intcov_ratio"
    geom_vline(xintercept = 62.5, linetype = "dashed", color = "black") + # After "ptb"
    geom_vline(xintercept = 69.5, linetype = "dashed", color = "black") + # After "siioutofsample"
    geom_vline(xintercept = 75.5, linetype = "dashed", color = "black") + # After "staff_sale"
    # Add labels for the sections
    annotate("text", x = 4.5, y = 0.55, label = "Capitalization", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 11.5, y = 0.55, label = "Sales & Working Capital", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 17.5, y = 0.55, label = "Long-term Debt", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 23.5, y = 0.55, label = "Quick Ratio", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 47.5, y = 0.55, label = "Return on Equity", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 54.5, y = 0.55, label = "Interest Coverage", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 62.5, y = 0.55, label = "Price to Book", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 69.5, y = 0.55, label = "Out of Sample", size = 4, angle = 90, vjust = -0.5) +
    annotate("text", x = 75.5, y = 0.55, label = "Staff to Sales", size = 4, angle = 90, vjust = -0.5) +
    theme(plot.margin = margin(20, 10, 20, 10)) # Adjust margins
  
  
  p_e_r2_plot = ggplot(outputs_p_e, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_1y, group = 1), fill="red", alpha=0.3) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_3y,  group = 1), fill="darkgreen", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_5y,  group = 1), fill="blue", alpha=0.2) + 
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    labs(title = sprintf("in-sample log price-to-earnings ratio"))+
    xlab("predictor") + ylab("in-sample R2") +
    scale_color_manual(values = c("1 year" = "red", "3 years" = "darkgreen", '5 years' = 'blue'))+
    labs(color = '') +  theme(legend.position = c(0.05,0.65))
  
  p_d_r2_plot = ggplot(outputs_p_d, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_1y, group = 1), fill="red", alpha=0.3) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_3y,  group = 1), fill="darkgreen", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_5y,  group = 1), fill="blue", alpha=0.2) + 
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    labs(title = sprintf("in-sample log price-to-dividend ratio"))+
    xlab("predictor") + ylab("in-sample R2") +
    scale_color_manual(values = c("1 year" = "red", "3 years" = "darkgreen", '5 years' = 'blue'))+
    labs(color = '') +  theme(legend.position = c(0.05,0.65))
  
  e_d_r2_plot = ggplot(outputs_e_d, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_1y, color = "1 year", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_1y, group = 1), fill="red", alpha=0.3) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_3y, color = "3 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_3y,  group = 1), fill="darkgreen", alpha=0.5) + 
    geom_point(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) + geom_line(aes(x = factor(predictor, levels = CHARS), r2_5y, color = "5 years", group = 1)) +  
    geom_area(aes(x = factor(predictor, levels = CHARS), r2_5y,  group = 1), fill="blue", alpha=0.2)+ 
    scale_x_discrete(guide = guide_axis(angle = 90), labels = c(
      "Capitalization Ratio", "Common Equity/Invested Capital", "Long-term Debt/Invested Capital","Total Debt/Invested Capital",
      "Asset Turnover", "Inventory Turnover", "Payables Turnover", "Receivables Turnover", "Sales/Stockholders Equity", "Sales/Invested Capital", "Sales/Working Capital",
      "Inventory/Current Assets","Receivables/Current Assets","Free Cash Flow/Operating Cash Flow","Operating CF/Current Liabilities","Cash Flow/Total Debt","Cash Balance/Total Liabilities","Cash Flow Margin","Short-Term Debt/Total Debt","Profit Before Depreciation/Current Liabilities","Current Liabilities/Total Liabilities","Total Debt/EBITDA","Long-term Debt/Book Equity","Interest/Average Long-term Debt","Interest/Average Total Debt", "Long-term Debt/Total Liabilities","Total Liabilities/Total Tangible Assets",
      "Cash Conversion Cycle (Days)","Cash Ratio","Current Ratio", "Quick Ratio (Acid Test)", 
      "Effective Tax Rate","Gross Profit/Total Assets","After-tax Return on Average Common Equity","After-tax Return on Total Stockholders’ Equity","After-tax Return on Invested Capital", "Gross Profit Margin","Net Profit Margin","Operating Profit Margin After Depreciation","Operating Profit Margin Before Depreciation","Pre-tax Return on Total Earning Assets","Pre-tax return on Net Operating Assets","Pre-tax Profit Margin","Return on Assets","Return on Capital Employed","Return on Equity",
      "Total Debt/Equity","Total Liabilities/Total Assets","Total Debt/Capital","After-tax Interest Coverage","Interest Coverage Ratio",
      "Dividend Payout Ratio","Trailing P/E to Growth (PEG) ratio","Book/Market","Shillers Cyclically Adjusted P/E Ratio","Enterprise Value Multiple","Price/Cash flow","P/E (Diluted, Excl. EI)","P/E (Diluted, Incl. EI)","Price/Operating Earnings (Basic, Excl. EI)","Price/Operating Earnings (Diluted, Excl. EI)","Price/Sales","Price/Book", 
      "ntis", "tbl", "tms", "svar","dfy", "siiOutOfSample",
      "Accruals/Average Assets","Research and Development/Sales","Avertising Expenses/Sales","Labor Expenses/Sales",
      "price-to-dividend","price-to-earning","earning-to-dividend",'lagged dividend growth (1-year)','lagged earnings growth (1-year)','lagged price growth (1-year)','lagged dividend growth (3-years)','lagged earnings growth (3-years)','lagged price growth (3-years)'
    ) ) +
    labs(title = sprintf("in-sample log earnings-to-dividends ratio"))+
    xlab("predictor") + ylab("in-sample R2") +
    scale_color_manual(values = c("1 year" = "red","3 years" = "darkgreen",  '5 years' = 'blue'))+
    labs(color = '') +  theme(legend.position = c(0.05,0.65))
  
  
  
   plot_is = ggarrange(d_gr_r2_plot,  ret_r2_plot,nrow = 2, ncol = 1, common.legend = T) #e_gr_r2_plot,
  


  #####################################
  ### Out-of-sample OLS
  #####################################
  
    if(oos_ols == T){
    train_date = sort(data[,'quarter_id'])[nrow(data)*splitratio] # choose initial split of the data
    n_test <- nrow (data %>% filter(quarter_id > train_date) %>%  dplyr::select(all_of(DEPVAR))  ) # nrow (data %>% filter(quarter_id > train_date) %>%  dplyr::select(all_of(DEPVAR))  %>% drop_na())
    
    ### Create matrix for OOS R2 for individual regressions
    
    r2.final = data.frame(matrix(nrow= length(CHARS), ncol=length(DEPVAR)+1)) 
    r2.final[,1] = CHARS
    colnames(r2.final) <- c("predictor",DEPVAR)
    
    ###
  
    i=1; j=1; i = n_test
    
    for (j in 1:length(CHARS)) {
      
      DEPVAR2 = DEPVAR
      
      ols_hat <- as.data.frame(matrix(NA,nrow = n_test, ncol = length(DEPVAR2)+1))
      
      colnames(ols_hat) = c('quarter_id',DEPVAR2)
      
      for (i in 1:n_test){
        
        if (data.freq == "q") {
          
          train.test.split = ceiling_date( train_date %m+% months(3*(i-1)), "month") - days(1)
          test.test.split = ceiling_date( train_date %m+% months(3*i), "month") - days(1)
          
        } 
        
        if (data.freq == "y") {
          
          train.test.split = ceiling_date( train_date %m+%  months(12*(i-1)), "month") - days(1)
          
        }
        
        Y_train <- data  %>% filter(quarter_id < train.test.split) %>%  
          mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>% 
          dplyr::select(c('quarter_id',all_of(DEPVAR2))) %>% as.data.frame() # %>% drop_na() 
        
        X_train <- data  %>% filter(quarter_id < train.test.split) %>%  
          mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>% 
          dplyr::select(c('quarter_id',all_of(CHARS))) %>% as.data.frame() # %>% drop_na() 
        
        X_test  <- data  %>% filter(quarter_id == train.test.split) %>%  
          mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>% 
          dplyr::select(c('quarter_id',all_of(CHARS))) %>% as.data.frame() # %>% drop_na() 
        
        model = lm( as.matrix(Y_train[,DEPVAR2])  ~ X_train[,CHARS[j]])
        
        pred <-  c(1, X_test[,CHARS[j]]) %*% model$coefficients
        
        ols_hat[i, 1] <- as.character(train.test.split) 
          
        ols_hat[i,2:(length(DEPVAR2)+1)] <- pred
        
      }
      
      ols_hat[,'quarter_id'] = as.Date(ols_hat[,'quarter_id'])
      
      colnames(ols_hat) = c('quarter_id', paste0(DEPVAR2,'_hat'))
      
      Y_test =  data %>% filter(quarter_id > train_date) %>%  
                mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>% 
                dplyr::select(c('quarter_id',all_of(DEPVAR2))) 
      
      #Y_test[,'quarter_id'] = ceiling_date( Y_test[,'quarter_id']  %m+% months(3), "month") - days(1)
      
      ols_hat = merge(Y_test, ols_hat, by = "quarter_id", all = T )
      
      #assign(paste0("ols_hat_",CHARS[j]),ols_hat)
      
      for (i in 1:length(DEPVAR2)) {
        
        #r2.final[j, DEPVAR[i]] <- 1- sum((Y_test[,DEPVAR2[i]]- ols_hat[,paste0(DEPVAR2,'_hat')[i]])**2, na.rm = T) / sum((Y_test[,DEPVAR2[i]] - mean(data[,DEPVAR2[i]], na.rm = T))**2, na.rm = T)
        r2.final[j, DEPVAR[i]] <- 1- sum((ols_hat[,DEPVAR2[i]]- ols_hat[,paste0(DEPVAR2,'_hat')[i]])**2, na.rm = T) / sum((ols_hat[,DEPVAR2[i]] - (cumsum(data[,DEPVAR2[i]]) / seq_along(data[,DEPVAR2[i]]))[(NROW(data)-n_test+1):NROW(data)] )**2, na.rm = T)
        
      }
    }
  
    
    
    r2.final_grounded =  r2.final  %>% mutate(across(where(is.numeric), ~if_else(. < 0, 0, .))) %>% drop_na() %>% as.data.frame()
    
    d_gr_r2_oos_plot =  ggplot(r2.final_grounded, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
      geom_point(aes(x = factor(predictor, levels = CHARS), d_gr_1y_lead, color = "1 year", group = 1)) + 
      geom_line(aes(x = factor(predictor, levels = CHARS), d_gr_1y_lead, color = "1 year", group = 1)) +  
      geom_area(aes(x = factor(predictor, levels = CHARS), d_gr_1y_lead, group = 1), fill="red", alpha=0.3) + 
      geom_point(aes(x = factor(predictor, levels = CHARS), d_gr_3y_lead, color = "3 years", group = 1)) + 
      geom_line(aes(x = factor(predictor, levels = CHARS), d_gr_3y_lead, color = "3 years", group = 1)) +  
      geom_area(aes(x = factor(predictor, levels = CHARS), d_gr_3y_lead,  group = 1), fill="darkgreen", alpha=0.5) + 
      geom_point(aes(x = factor(predictor, levels = CHARS), d_gr_5y_lead, color = "5 years", group = 1)) + 
      geom_line(aes(x = factor(predictor, levels = CHARS), d_gr_5y_lead, color = "5 years", group = 1)) +  
      geom_area(aes(x = factor(predictor, levels = CHARS), d_gr_5y_lead,  group = 1), fill="blue", alpha=0.2) + 
      scale_x_discrete(guide = guide_axis(angle = 90), labels = c(
        "Capitalization Ratio", "Common Equity/Invested Capital", "Long-term Debt/Invested Capital","Total Debt/Invested Capital",
        "Asset Turnover", "Inventory Turnover", "Payables Turnover", "Receivables Turnover", "Sales/Stockholders Equity", "Sales/Invested Capital", "Sales/Working Capital",
        "Inventory/Current Assets","Receivables/Current Assets","Free Cash Flow/Operating Cash Flow","Operating CF/Current Liabilities","Cash Flow/Total Debt","Cash Balance/Total Liabilities","Cash Flow Margin","Short-Term Debt/Total Debt","Profit Before Depreciation/Current Liabilities","Current Liabilities/Total Liabilities","Total Debt/EBITDA","Long-term Debt/Book Equity","Interest/Average Long-term Debt","Interest/Average Total Debt", "Long-term Debt/Total Liabilities","Total Liabilities/Total Tangible Assets",
        "Cash Conversion Cycle (Days)","Cash Ratio","Current Ratio", "Quick Ratio (Acid Test)", 
        "Accruals/Average Assets","Research and Development/Sales","Avertising Expenses/Sales","Labor Expenses/Sales",
        "Effective Tax Rate","Gross Profit/Total Assets","After-tax Return on Average Common Equity","After-tax Return on Total Stockholders’ Equity","After-tax Return on Invested Capital", "Gross Profit Margin","Net Profit Margin","Operating Profit Margin After Depreciation","Operating Profit Margin Before Depreciation","Pre-tax Return on Total Earning Assets","Pre-tax return on Net Operating Assets","Pre-tax Profit Margin","Return on Assets","Return on Capital Employed","Return on Equity",
        "Total Debt/Equity","Total Liabilities/Total Assets","Total Debt/Capital","After-tax Interest Coverage","Interest Coverage Ratio","Dividend Payout Ratio","" ) )  +
      labs(title = sprintf("out-of-sample dividend growth rates"))+
      xlab("predictor") + ylab("out-of-sample R2") +
      scale_color_manual(values = c("1 year" = "red", '3 years' = 'darkgreen'  , '5 years' = 'blue'))+ ylim(c(0,0.60)) +
      labs(color = '') +  theme(legend.position = c(0.05,0.65), legend.text = element_text(size=13))
    
    e_gr_r2_oos_plot = ggplot(r2.final_grounded, aes(x = factor(predictor, levels = CHARS), e_gr_1y_lead, colour="1-year")) + theme_bw() +
      geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), e_gr_5y_lead, colour="5-year")) +
      #geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), r2_q, colour="1-quarter"))+
      geom_point() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(title = sprintf("log earning growth"))+
      xlab("predictor") + ylab("out-of-sample R2") +
      scale_color_manual(values = c("1-year" = "black", '5-year' = 'red'))+
      labs(color = '') +  theme(legend.position = c(0.20,0.85))
    
    p_d_r2_oos_plot = ggplot(r2.final_grounded, aes(x = factor(predictor, levels = CHARS), p_d_1y_lead, colour="1-year")) + theme_bw() +
      geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), p_d_5y_lead, colour="5-year")) +
      #geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), r2_q, colour="1-quarter"))+
      geom_point() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(title = sprintf("log price-to-dividend ratio")) +
      xlab("predictor") + ylab("out-of-sample R2") +
      scale_color_manual(values = c("1-year" = "black", '5-year' = 'red'))+
      labs(color = '') +  theme(legend.position = c(0.95,0.65))
    
    p_e_r2_oos_plot = ggplot(r2.final_grounded, aes(x = factor(predictor, levels = CHARS), p_e_1y_lead, colour="1-year")) + theme_bw() +
      geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), p_e_5y_lead, colour="5-year")) +
      #geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), r2_q, colour="1-quarter"))+
      geom_point() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(title = sprintf("log price-to-dividend ratio")) +
      xlab("predictor") + ylab("out-of-sample R2") +
      scale_color_manual(values = c("1-year" = "black", '5-year' = 'red'))+
      labs(color = '') +  theme(legend.position = c(0.95,0.65))
    
    e_d_r2_oos_plot = ggplot(r2.final_grounded, aes(x = factor(predictor, levels = CHARS), e_d_1y_lead, colour="1-year")) + theme_bw() +
      geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), e_d_5y_lead, colour="5-year")) +
      #geom_point() +geom_point(aes(x = factor(predictor, levels = CHARS), r2_q, colour="1-quarter"))+
      geom_point() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(title = sprintf("log earning-to-dividend ratio")) +
      xlab("predictor") + ylab("out-of-sample R2") +
      scale_color_manual(values = c("1-year" = "black", '5-year' = 'red'))+
      labs(color = '') +  theme(legend.position = c(0.05,0.65))
    
    ret_r2_oos_plot = ggplot(r2.final_grounded, aes(x = factor(predictor, levels = CHARS))) + theme_bw() +
      geom_point(aes(x = factor(predictor, levels = CHARS), ret_1y_lead, color = "1 year", group = 1)) + 
      geom_line(aes(x = factor(predictor, levels = CHARS), ret_1y_lead, color = "1 year", group = 1)) +  
      geom_area(aes(x = factor(predictor, levels = CHARS), ret_1y_lead, group = 1), fill="red", alpha=0.3) +
      geom_point(aes(x = factor(predictor, levels = CHARS), ret_3y_lead, color = "3 years", group = 1)) + 
      geom_line(aes(x = factor(predictor, levels = CHARS), ret_3y_lead, color = "3 years", group = 1)) +  
      geom_area(aes(x = factor(predictor, levels = CHARS), ret_3y_lead,  group = 1), fill="darkgreen", alpha=0.5) + 
      geom_point(aes(x = factor(predictor, levels = CHARS), ret_5y_lead, color = "5 years", group = 1)) + 
      geom_line(aes(x = factor(predictor, levels = CHARS), ret_5y_lead, color = "5 years", group = 1)) +  
      geom_area(aes(x = factor(predictor, levels = CHARS), ret_5y_lead,  group = 1), fill="blue", alpha=0.2) + 
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(title = sprintf("out-of-sample log returns"))+
      xlab("predictor") + ylab("out-of-sample R2") +
      scale_color_manual(values = c("1 year" = "red", '3 years' = 'darkgreen', '5 years' = 'blue'))+ ylim(c(0,0.62)) +
      labs(color = '') +  theme(legend.position = c(0.05,0.65), legend.text = element_text(size=13))
    
    plot_oos = ggarrange(d_gr_r2_oos_plot, ret_r2_oos_plot, nrow =2, ncol = 1, common.legend = T)
    
    plot_full = ggarrange(d_gr_r2_plot, ret_r2_plot, d_gr_r2_oos_plot, ret_r2_oos_plot, nrow =4, ncol = 1, common.legend = T)
    
    return(list(outputs_d_gr = outputs_d_gr, outputs_p_d = outputs_p_d, outputs_e_d = outputs_e_d, outputs_ret = outputs_ret, outputs_p_e = outputs_p_e, outputs_e_gr = outputs_e_gr, plot_is = plot_is, r2.oos = r2.final, plot_oos = plot_oos, plot_full = plot_full))
    } 
  
  if(oos_ols == F){
    
    return(list(outputs_d_gr = outputs_d_gr, outputs_p_d = outputs_p_d, outputs_e_d = outputs_e_d, outputs_ret = outputs_ret, outputs_p_e = outputs_p_e, outputs_e_gr = outputs_e_gr, plot_is = plot_is))
    
    }
}



























