library('ggpubr')
library('ggplot2')


plot_is = function(df, freq){
  
  if(freq == "q"){
  
    recessions.trim = subset(recessions.df, Peak >= min(df$quarter_id))
  
    fund_plt_1 = ggplot(data = df) + theme_bw() + labs(color = '')+ 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("dividend growth rate") +
      geom_line( mapping= aes(y= d_gr_q, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= d_gr_q_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= d_gr_q_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      #geom_line( mapping= aes(y= d_gr_1y_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1., alpha=0.6) +
      geom_line( mapping= aes(y= d_gr_q_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'black',
        'lasso' = 'red',
        'ridge' = 'darkgreen',
        #'elastic net' = 'darkgreen',
        'unconstrained' = "black")) +
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    fund_plt_2 = ggplot(data = df) + theme_bw() + labs(color = '')+ 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("log price-to-dividend ratio") +
      geom_line( mapping= aes(y= p_d, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_d_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_d_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      #geom_line( mapping= aes(y= p_d_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1., alpha=0.6) +
      geom_line( mapping= aes(y= p_d_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'black',
        'lasso' = 'red',
        'ridge' = 'darkgreen',
        #'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) +
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    fund_plt_3 = ggplot(data = df) + theme_bw() +  labs(color = '') + 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("log price-to-earnings ratio") +
      theme(legend.position = c(0.15,0.80)) +
      geom_line( mapping= aes(y= p_e, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_e_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_e_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      #geom_line( mapping= aes(y= p_e_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_e_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'black',
        'lasso' = 'red',
        'ridge' = 'darkgreen',
        # 'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) + 
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    fund_plt_4 = ggplot(data = df) + theme_bw() + labs(color = '')+ 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("return rate") +
      geom_line( mapping= aes(y= ret_q, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= ret_q_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= ret_q_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      # geom_line( mapping= aes(y= ret_1y_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= ret_q_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'black',
        'lasso' = 'red',
        'ridge' = 'darkgreen',
        # 'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) +
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
  }else{
    
    recessions.trim = subset(recessions.df, Peak >= min(df$quarter_id))
    
    fund_plt_1 = ggplot(data = df) + theme_bw() + labs(color = '')+ 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("dividend growth") +
      geom_line( mapping= aes(y= d_gr_1y_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= d_gr_1y_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      #geom_line( mapping= aes(y= d_gr_1y_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1., alpha=0.6) +
      geom_line( mapping= aes(y= d_gr_1y_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= d_gr_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'darkgoldenrod1',
        'lasso' = 'darkred',
        'ridge' = 'darkgreen',
        #'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) +
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    fund_plt_2 = ggplot(data = df) + theme_bw() + labs(color = '')+ 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("log price-to-dividend ratio") +
      geom_line( mapping= aes(y= p_d_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_d_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      #geom_line( mapping= aes(y= p_d_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1., alpha=0.6) +
      geom_line( mapping= aes(y= p_d_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_d, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'darkgoldenrod1',
        'lasso' = 'darkred',
        'ridge' = 'darkgreen',
        #'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) +
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    fund_plt_3 = ggplot(data = df) + theme_bw() +  labs(color = '') + 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("log price-to-earnings ratio") +
      theme(legend.position = c(0.15,0.80)) +
      geom_line( mapping= aes(y= p_e_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_e_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      #geom_line( mapping= aes(y= p_e_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_e_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= p_e, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'darkgoldenrod1',
        'lasso' = 'darkred',
        'ridge' = 'darkgreen',
        # 'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) + 
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    fund_plt_4 = ggplot(data = df) + theme_bw() + labs(color = '')+ 
      geom_rect(data=recessions.trim, aes(NULL,NULL,xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)  +
      xlab("Time") + ylab("return") +
      geom_line( mapping= aes(y= ret_1y_pred_lasso, x= quarter_id, color = "lasso"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= ret_1y_pred_ridge, x= quarter_id, color = "ridge"), linetype="twodash",size=1.25, alpha=0.6) +
      # geom_line( mapping= aes(y= ret_1y_pred_elnet, x= quarter_id, color = "elastic net"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= ret_1y_pred_unc, x= quarter_id, color = "unconstrained"), linetype="twodash",size=1.25, alpha=0.6) +
      geom_line( mapping= aes(y= ret_1y, x= quarter_id, color = "data"), linetype="solid",size=1.25, alpha=0.6) +
      scale_color_manual(values = c(
        'data' = 'darkgoldenrod1',
        'lasso' = 'darkred',
        'ridge' = 'darkgreen',
        # 'elastic net' = 'darkgreen',
        'unconstrained' = "blue")) +
      theme(legend.position = c(0.15,0.80), legend.text = element_text(size=13))
    
    
  }
  
  plot_is =  ggarrange(fund_plt_1, fund_plt_2, fund_plt_3, fund_plt_4, nrow = 2, ncol = 2, common.legend = T)
  
  return( plot_is)
}