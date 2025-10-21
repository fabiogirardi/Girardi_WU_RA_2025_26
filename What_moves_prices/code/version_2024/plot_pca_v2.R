library("ggpubr")
library('ggplot2')

plot_pc = function(df){
  
      fund_plt_1 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= capitalization_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("capitalization") + ylim(0.25,1)
      
      fund_plt_2 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= efficiency_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("efficiency") + ylim(0.25,1)
      
      
      fund_plt_3 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= financial_soundness_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("financial soundness") + ylim(0.25,1)
      
      
      fund_plt_4 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= liquidity_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("liquidity") + ylim(0.25,1)
      
      
      fund_plt_5 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= profitability_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("profitability") + ylim(0.25,1)
      
      
      fund_plt_6 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= solvency_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("solvency") + ylim(0.25,1)
      
      
      fund_plt_7 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= valuation_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("valuation") + ylim(0.25,1)
      
      
      fund_plt_8 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= equity_premium_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("equity premium") + ylim(0.25,1)
      
      
      fund_plt_9 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= other_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("other") + ylim(0.25,1)
      
      
      fund_plt_10 = ggplot(data = df[1:4,]) + theme_bw() +
        geom_line( mapping= aes(y= full_y, x= 1:4, color = "annual frequency"), linetype="solid",linewidth=1.5, alpha=0.6 ) +
        scale_color_manual(values = c('annual frequency' = 'darkblue')) +
        labs(color = '')+ 
        xlab("number of principal components") + ylab("cumulative explained variance") +
        theme(legend.position = "none") + ggtitle("full set") + ylim(0.25,1)
      
      plot = ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, fund_plt_4, fund_plt_5, fund_plt_6, fund_plt_7, fund_plt_8, fund_plt_9, fund_plt_10, nrow = 5, ncol = 2, common.legend = F)
      
      plot_base = ggarrange(fund_plt_5, fund_plt_10, nrow = 1, ncol = 2, common.legend = F)
      
      return(list(plot = plot, plot_base = plot_base))
}