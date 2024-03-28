     vardesc_temp = vardesc("P_C_max_Di", "Diaz. Maximum Growth Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(DIAZO )%id_P_C_max = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("P_C_max_Lg", "Large Phyto. Maximum Growth Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(LARGE )%id_P_C_max = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("P_C_max_Md", "Medium Phyto. Maximum Growth Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(MEDIUM)%id_P_C_max = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("P_C_max_Sm", "Small Phyto. Maximum Growth Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(SMALL )%id_P_C_max = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("alpha_Di", "Diaz. Photo. vs Irrad. slope", 'h', 'L', 's', 'gC gChl-1 sec-1 (W m-2)-1', 'f')
     phyto(DIAZO )%id_alpha = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("alpha_Lg", "Large Phyto. Photo. vs Irrad. slope", 'h', 'L', 's', 'gC gChl-1 sec-1 (W m-2)-1', 'f')
     phyto(LARGE )%id_alpha = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("alpha_Md", "Medium Phyto. Photo. vs Irrad. slope", 'h', 'L', 's', 'gC gChl-1 sec-1 (W m-2)-1', 'f')
     phyto(MEDIUM)%id_alpha = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("alpha_Sm", "Small Phyto. Photo. vs Irrad. slope", 'h', 'L', 's', 'gC gChl-1 sec-1 (W m-2)-1', 'f')
     phyto(SMALL )%id_alpha = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("bresp_Di", "Diaz. Basal Respiration Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(DIAZO )%id_bresp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("bresp_Lg", "Large Phyto. Basal Respiration Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(LARGE )%id_bresp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("bresp_Md", "Medium Phyto. Basal Respiration Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(MEDIUM)%id_bresp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("bresp_Sm", "Small Phyto. Basal Respiration Rate", 'h', 'L', 's', 'sec-1', 'f')
     phyto(SMALL )%id_bresp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("def_fe_Di", "Diaz. Phyto. Fe Deficiency", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("def_fe_Lg", "Large Phyto. Fe Deficiency", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("def_fe_Md", "Medium Phyto. Fe Deficiency", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("def_fe_Sm", "Small Phyto. Fe Deficiency", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("felim_Di", "Diaz. Phyto. Fed uptake Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("felim_Lg", "Large Phyto. Fed uptake Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("felim_Md", "Medium Phyto. Fed uptake Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("felim_Sm", "Small Phyto. Fed uptake Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irrlim_Di", "Diaz. Phyto. Light Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irrlim_Lg", "Large Phyto. Light Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irrlim_Md", "Medium Phyto. Light Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irrlim_Sm", "Small Phyto. Light Limitation", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("theta_Di", "Diaz. Phyto. Chl:C", 'h', 'L', 's', 'g Chl (g C)-1', 'f')
     phyto(DIAZO )%id_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("theta_Lg", "Large Phyto. Chl:C", 'h', 'L', 's', 'g Chl (g C)-1', 'f')
     phyto(LARGE )%id_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("theta_Md", "Medium Phyto. Chl:C", 'h', 'L', 's', 'g Chl (g C)-1', 'f')
     phyto(MEDIUM)%id_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("theta_Sm", "Small Phyto. Chl:C", 'h', 'L', 's', 'g Chl (g C)-1', 'f')
     phyto(SMALL )%id_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("chl_Di", "Diaz. Phyto. Chlorophyll", 'h', 'L', 's', 'ug Kg-1', 'f')
     phyto(DIAZO )%id_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("chl_Lg", "Large Phyto. Chlorophyll", 'h', 'L', 's', 'ug Kg-1', 'f')
     phyto(LARGE )%id_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("chl_Md", "Medium Phyto. Chlorophyll", 'h', 'L', 's', 'ug Kg-1', 'f')
     phyto(MEDIUM)%id_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("chl_Sm", "Small Phyto. Chlorophyll", 'h', 'L', 's', 'ug Kg-1', 'f')
     phyto(SMALL )%id_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_Di", "Diaz. Phyto. Overall Growth Rate", 'h', 'L', 's', 's-1', 'f')
     phyto(DIAZO )%id_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_Lg", "Large Phyto. Overall Growth Rate", 'h', 'L', 's', 's-1', 'f')
     phyto(LARGE )%id_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_Md", "Medium Phyto. Overall Growth Rate", 'h', 'L', 's', 's-1', 'f')
     phyto(MEDIUM)%id_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_Sm", "Small Phyto. Growth Rate", 'h', 'L', 's', 's-1', 'f')
     phyto(SMALL )%id_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mem_Di", "Diaz. Phyto. Growth Memory", 'h', 'L', 's', 's-1', 'f')
     phyto(DIAZO )%id_f_mu_mem = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mem_Lg", "Large Phyto. Growth memory", 'h', 'L', 's', 's-1', 'f')
     phyto(LARGE )%id_f_mu_mem = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mem_Md", "Medium Phyto. Growth memory", 'h', 'L', 's', 's-1', 'f')
     phyto(MEDIUM)%id_f_mu_mem = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mem_Sm", "Small Phyto. Growth Memory", 'h', 'L', 's', 's-1', 'f')
     phyto(SMALL )%id_f_mu_mem = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Di", "Diaz. Phyto. movement", 'h', 'L', 's', 'm s-1', 'f')
     phyto(DIAZO )%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Lg", "Large Phyto. movement", 'h', 'L', 's', 'm s-1', 'f')
     phyto(LARGE )%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Md", "Medium Phyto. movement", 'h', 'L', 's', 'm s-1', 'f')
     phyto(MEDIUM)%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Sm", "Small Phyto. movement", 'h', 'L', 's', 'm s-1', 'f')
     phyto(SMALL )%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mix_Di", "Diaz. Phyto. ML ave", 'h', 'L', 's', 's-1', 'f')
     phyto(DIAZO )%id_mu_mix = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mix_Lg", "Large Phyto. ML ave", 'h', 'L', 's', 's-1', 'f')
     phyto(LARGE )%id_mu_mix = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mix_Md", "Medium Phyto. ML ave", 'h', 'L', 's', 's-1', 'f')
     phyto(MEDIUM)%id_mu_mix = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_mix_Sm", "Small Phyto. ML ave", 'h', 'L', 's', 's-1', 'f')
     phyto(SMALL )%id_mu_mix = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nh4lim_Lg", "Ammonia Limitation of Large Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nh4lim_Md", "Ammonia Limitation of Medium Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nh4lim_Sm", "Ammonia Limitation of Small Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nh4lim_Di", "Ammonia Limitation of Diazo", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("no3lim_Lg", "Nitrate Limitation of Large Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("no3lim_Md", "Nitrate Limitation of Medium Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("no3lim_Sm", "Nitrate Limitation of Small Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("no3lim_Di", "Ammonia Limitation of Diazo", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("po4lim_Di", "Phosphate Limitation of Diaz. Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("po4lim_Lg", "Phosphate Limitation of Large Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("po4lim_Md", "Phosphate Limitation of Medium Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("po4lim_Sm", "Phosphate Limitation of Small Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("liebig_Sm", "Overall (Liebig) nutrient lim., Small Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_liebig_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("liebig_Md", "Overall (Liebig) nutrient lim., Medium Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_liebig_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("liebig_Lg", "Overall (Liebig) nutrient lim., Large Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_liebig_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("liebig_Di", "Overall (Liebig) nutrient lim., Diazotrophs", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_liebig_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("o2lim_Di", "Oxygen Limitation of Diaz. Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_o2lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_fe_2_n_Di", "Fe:N ratio of Diaz. Phyto", 'h', 'L', 's', 'mol Fe/mol N', 'f')
     phyto(DIAZO )%id_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_fe_2_n_Lg", "Fe:N ratio of Large Phyto", 'h', 'L', 's', 'mol Fe/mol N', 'f')
     phyto(LARGE )%id_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_fe_2_n_Md", "Fe:N ratio of Medium Phyto", 'h', 'L', 's', 'mol Fe/mol N', 'f')
     phyto(MEDIUM)%id_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_fe_2_n_Sm", "Fe:N ratio of Small Phyto", 'h', 'L', 's', 'mol Fe/mol N', 'f')
     phyto(SMALL )%id_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_p_2_n_Di", "P:N ratio of Diaz. Phyto", 'h', 'L', 's', 'mol P/mol N', 'f')
     phyto(DIAZO )%id_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_p_2_n_Lg", "P:N ratio of Large Phyto", 'h', 'L', 's', 'mol P/mol N', 'f')
     phyto(LARGE )%id_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_p_2_n_Md", "P:N ratio of Medium Phyto", 'h', 'L', 's', 'mol P/mol N', 'f')
     phyto(MEDIUM)%id_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_p_2_n_Sm", "P:N ratio of Small Phyto", 'h', 'L', 's', 'mol P/mol N', 'f')
     phyto(SMALL )%id_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("silim_Lg", "SiO4 Limitation of Large Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_silim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("silim_Md", "SiO4 Limitation of Medium Phyto", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_silim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_si_2_n_Lg", "Si:N ratio of Large Phyto", 'h', 'L', 's', 'mol Si/mol N', 'f')
     phyto(LARGE )%id_q_si_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_si_2_n_Md", "Si:N ratio of Medium Phyto", 'h', 'L', 's', 'mol Si/mol N', 'f')
     phyto(MEDIUM)%id_q_si_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Di", "Diazotroph nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(DIAZO )%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Lg", "Large phyto nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(LARGE )%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Md", "Medium phyto nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(MEDIUM)%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Sm", "Small phyto nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(SMALL )%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_n_Di", "Diazotroph nitrogen loss to aggregation layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(DIAZO )%id_jaggloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_n_Lg", "Large phyto nitrogen loss to aggregation layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(LARGE )%id_jaggloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_n_Md", "Medium phyto nitrogen loss to aggregation layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(MEDIUM)%id_jaggloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_n_Sm", "Small phyto nitrogen loss to aggregation layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(SMALL )%id_jaggloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("stress_fac_Di", "Diazotroph stress factor", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_stress_fac = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("stress_fac_Lg", "Large phyto stress factor", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_stress_fac = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("stress_fac_Md", "Medium phyto stress factor", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(MEDIUM)%id_stress_fac = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("stress_fac_Sm", "Small phyto stress factor", 'h', 'L', 's', 'dimensionless', 'f')
     phyto(SMALL )%id_stress_fac = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_n_Di", "Diazotroph nitrogen loss to viruses layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(DIAZO )%id_jvirloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_n_Lg", "Large phyto nitrogen loss to viruses layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(LARGE )%id_jvirloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_n_Md", "Medium phyto nitrogen loss to viruses layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(MEDIUM)%id_jvirloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_n_Sm", "Small phyto nitrogen loss to viruses layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(SMALL )%id_jvirloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_n_Di", "Diazotroph nitrogen loss to mortality layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(DIAZO )%id_jmortloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_n_Lg", "Large phyto nitrogen loss to mortality layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(LARGE )%id_jmortloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_n_Md", "Medium phyto nitrogen loss to mortality layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(MEDIUM)%id_jmortloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_n_Sm", "Small phyto nitrogen loss to mortality layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(SMALL )%id_jmortloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_n_Di", "Diazotroph nitrogen loss via exudation", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(DIAZO )%id_jexuloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_n_Lg", "Large phyto nitrogen loss via exudation", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(LARGE )%id_jexuloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_n_Md", "Medium phyto nitrogen loss via exudation", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(MEDIUM)%id_jexuloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_n_Sm", "Small phyto nitrogen loss via exudation", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(SMALL )%id_jexuloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nlg_diatoms", "large phytoplankton nitrogen from diatoms", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_nlg_diatoms = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nmd_diatoms", "medium phytoplankton nitrogen from diatoms", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_nmd_diatoms = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_si_2_n_lg_diatoms", "Si:N ratio in large diatoms", 'h', 'L', 's', 'mol Si mol N', 'f')
     cobalt%id_q_si_2_n_lg_diatoms = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("q_si_2_n_md_diatoms", "Si:N ratio in medium diatoms", 'h', 'L', 's', 'mol Si mol N', 'f')
     cobalt%id_q_si_2_n_md_diatoms = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_n2_Di", "Nitrogen fixation layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     phyto(DIAZO )%id_juptake_n2 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_fe_Di", "Diaz. phyto. Fed uptake layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     phyto(DIAZO )%id_juptake_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_fe_Lg", "Large phyto. Fed uptake layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     phyto(LARGE )%id_juptake_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_fe_Md", "Medium phyto. Fed uptake layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     phyto(MEDIUM)%id_juptake_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_fe_Sm", "Small phyto. Fed uptake layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     phyto(SMALL )%id_juptake_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_nh4_Di", "Diaz. phyto. NH4 uptake layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     phyto(DIAZO )%id_juptake_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_nh4_Lg", "Large phyto. NH4 uptake layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     phyto(LARGE )%id_juptake_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_nh4_Md", "Medium phyto. NH4 uptake layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     phyto(MEDIUM)%id_juptake_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_nh4_Sm", "Small phyto. NH4 uptake layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     phyto(SMALL )%id_juptake_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_no3_Di", "Diaz. phyto. NO3 uptake layer integral", 'h', 'L', 's', 'mol NO3 m-2 s-1', 'f')
     phyto(DIAZO )%id_juptake_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_no3_Lg", "Large phyto. NO3 uptake layer integral", 'h', 'L', 's', 'mol NO3 m-2 s-1', 'f')
     phyto(LARGE )%id_juptake_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_no3_Md", "Medium phyto. NO3 uptake layer integral", 'h', 'L', 's', 'mol NO3 m-2 s-1', 'f')
     phyto(MEDIUM)%id_juptake_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_no3_Sm", "Small phyto. NO3 uptake layer integral", 'h', 'L', 's', 'mol NO3 m-2 s-1', 'f')
     phyto(SMALL )%id_juptake_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_po4_Di", "Diaz. phyto. PO4 uptake layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     phyto(DIAZO )%id_juptake_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_po4_Lg", "Large phyto. PO4 uptake layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     phyto(LARGE )%id_juptake_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_po4_Md", "Medium phyto. PO4 uptake layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     phyto(MEDIUM)%id_juptake_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_po4_Sm", "Small phyto. PO4 uptake layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     phyto(SMALL )%id_juptake_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_sio4_Lg", "Large phyto. SiO4 uptake layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_juptake_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_sio4_Md", "Medium phyto. SiO4 uptake layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_juptake_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndi", "Diazotroph Nitrogen production layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nsmp", "Small phyto. Nitrogen production layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nmdp", "Medium phyto. Nitrogen production layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nlgp", "Large phyto. Nitrogen production layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("net_phyto_resp", "Net phytoplankton respiration layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_net_phyto_resp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Smz", "Small zooplankton nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Mdz", "Medium-sized zooplankton nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Lgz", "Large zooplankton nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jhploss_n_Smz", "Small zooplankton nitrogen loss to higher predators layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jhploss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jhploss_n_Mdz", "Medium-sized zooplankton nitrogen loss to higher predators layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jhploss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jhploss_n_Lgz", "Large zooplankton nitrogen loss to higher predators layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jhploss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_Smz", "Ingestion of nitrogen by small zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jingest_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_Mdz", "Ingestion of nitrogen by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jingest_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_Lgz", "Ingestion of nitrogen by large zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jingest_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_p_Smz", "Ingestion of phosphorous by small zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(1)%id_jingest_p = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_p_Mdz", "Ingestion of phosphorous by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(2)%id_jingest_p = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_p_Lgz", "Ingestion of phosphorous by large zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(3)%id_jingest_p = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_sio2_Smz", "Ingestion of sio2 by small zooplankton, layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     zoo(1)%id_jingest_sio2 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_sio2_Mdz", "Ingestion of sio2 by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     zoo(2)%id_jingest_sio2 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_sio2_Lgz", "Ingestion of sio2 by large zooplankton, layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     zoo(3)%id_jingest_sio2 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_fe_Smz", "Ingestion of Fe by small zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(1)%id_jingest_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_fe_Mdz", "Ingestion of Fe by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(2)%id_jingest_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_fe_Lgz", "Ingestion of Fe by large zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(3)%id_jingest_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet_Smz", "Production of nitrogen detritus by small zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jprod_ndet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet_Mdz", "Production of nitrogen detritus by medium zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jprod_ndet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet_Lgz", "Production of nitrogen detritus by large zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jprod_ndet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_pdet_Smz", "Production of phosphorous detritus by small zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(1)%id_jprod_pdet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_pdet_Mdz", "Production of phosphorous detritus by medium zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(2)%id_jprod_pdet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_pdet_Lgz", "Production of phosphorous detritus by large zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(3)%id_jprod_pdet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sidet_Smz", "Production of opal detritus by small zooplankton, layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     zoo(1)%id_jprod_sidet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sidet_Mdz", "Production of opal detritus by medium zooplankton, layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     zoo(2)%id_jprod_sidet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sidet_Lgz", "Production of opal detritus by large zooplankton, layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     zoo(3)%id_jprod_sidet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sio4_Smz", "Production of sio4 through grazing/dissolution, layer integral", 'h', 'L', 's', 'mol SiO4 m-2 s-1', 'f')
     zoo(1)%id_jprod_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sio4_Mdz", "Production of sio4 through grazing/dissolution, layer integral", 'h', 'L', 's', 'mol SiO4 m-2 s-1', 'f')
     zoo(2)%id_jprod_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sio4_Lgz", "Production of sio4 through grazing/dissolution, layer integral", 'h', 'L', 's', 'mol SiO4 m-2 s-1', 'f')
     zoo(3)%id_jprod_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fedet_Smz", "Production of iron detritus by small zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(1)%id_jprod_fedet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fedet_Mdz", "Production of iron detritus by medium zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(2)%id_jprod_fedet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fedet_Lgz", "Production of iron detritus by large zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(3)%id_jprod_fedet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldon_Smz", "Production of labile dissolved organic nitrogen by small zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jprod_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldon_Mdz", "Production of labile dissolved organic nitrogen by medium zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jprod_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldon_Lgz", "Production of labile dissolved organic nitrogen by large zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jprod_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldop_Smz", "Production of labile dissolved organic phosphorous by small zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(1)%id_jprod_ldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldop_Mdz", "Production of labile dissolved organic phosphorous by medium zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(2)%id_jprod_ldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldop_Lgz", "Production of labile dissolved organic phosphorous by large zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(3)%id_jprod_ldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdon_Smz", "Production of semi-refractory dissolved organic nitrogen by small zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jprod_srdon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdon_Mdz", "Production of semi-refractory dissolved organic nitrogen by medium zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jprod_srdon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdon_Lgz", "Production of semi-refractory dissolved organic nitrogen by large zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jprod_srdon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdop_Smz", "Production of semi-refractory dissolved organic phosphorous by small zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(1)%id_jprod_srdop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdop_Mdz", "Production of semi-refractory dissolved organic phosphorous by medium zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(2)%id_jprod_srdop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdop_Lgz", "Production of semi-refractory dissolved organic phosphorous by large zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(3)%id_jprod_srdop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldon_Smz", "Production of semi-labile dissolved organic nitrogen by small zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jprod_sldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldon_Mdz", "Production of semi-labile dissolved organic nitrogen by medium zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jprod_sldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldon_Lgz", "Production of semi-labile dissolved organic nitrogen by large zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jprod_sldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldop_Smz", "Production of semi-labile dissolved organic phosphorous by small zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(1)%id_jprod_sldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldop_Mdz", "Production of semi-labile dissolved organic phosphorous by medium zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(2)%id_jprod_sldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldop_Lgz", "Production of semi-labile dissolved organic phosphorous by large zooplankton, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     zoo(3)%id_jprod_sldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fed_Smz", "Production of dissolved iron by small zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(1)%id_jprod_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fed_Mdz", "Production of dissolved iron by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(2)%id_jprod_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fed_Lgz", "Production of dissolved iron by large zooplankton, layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     zoo(3)%id_jprod_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_po4_Smz", "Production of phosphate by small zooplankton, layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     zoo(1)%id_jprod_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_po4_Mdz", "Production of phosphate by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     zoo(2)%id_jprod_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_po4_Lgz", "Production of phosphate by large zooplankton, layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     zoo(3)%id_jprod_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nh4_Smz", "Production of ammonia by small zooplankton, layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     zoo(1)%id_jprod_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nh4_Mdz", "Production of ammonia by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     zoo(2)%id_jprod_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nh4_Lgz", "Production of ammonia by large zooplankton, layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     zoo(3)%id_jprod_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nsmz", "Production of new biomass (nitrogen) by small zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(1)%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nmdz", "Production of new biomass (nitrogen) by medium-sized zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(2)%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nlgz", "Production of new biomass (nitrogen) by large zooplankton, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     zoo(3)%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("o2lim_Smz", "Oxygen limitation of small zooplankton", 'h', 'L', 's', 'dimensionless', 'f')
     zoo(1)%id_o2lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("o2lim_Mdz", "Oxygen limitation of medium-sized zooplankton", 'h', 'L', 's', 'dimensionless', 'f')
     zoo(2)%id_o2lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("o2lim_Lgz", "Oxygen limitation of large zooplankton", 'h', 'L', 's', 'dimensionless', 'f')
     zoo(3)%id_o2lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("temp_lim_Smz", "Temperature limitation of small zooplankton", 'h', 'L', 's', 'dimensionless', 'f')
     zoo(1)%id_temp_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("temp_lim_Mdz", "Temperature limitation of medium-sized zooplankton", 'h', 'L', 's', 'dimensionless', 'f')
     zoo(2)%id_temp_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("temp_lim_Lgz", "Temperature limitation of large zooplankton", 'h', 'L', 's', 'dimensionless', 'f')
     zoo(3)%id_temp_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Smz", "Small zoo movement", 'h', 'L', 's', 'm s-1', 'f')
     zoo(1)%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Mdz", "Medium zoo movement", 'h', 'L', 's', 'm s-1', 'f')
     zoo(2)%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("vmove_Lgz", "Large zoo movement", 'h', 'L', 's', 'm s-1', 'f')
     zoo(3)%id_vmove = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_n_Bact", "Bacterial nitrogen loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_n_Bact", "Bacterial nitrogen loss to viruses layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_jvirloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_ldon", "Bacterial uptake of labile dissolved organic nitrogen", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_juptake_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_ldop", "Bacterial uptake of labile dissolved organic phosphorous", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     bact(1)%id_juptake_ldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_po4_Bact", "Bacterial uptake of po4", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     bact(1)%id_juptake_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_po4_Bact", "Production of phosphate by bacteria, layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     bact(1)%id_jprod_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nh4_Bact", "Production of ammonia by bacteria, layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     bact(1)%id_jprod_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nbact", "Production of new biomass (nitrogen) by bacteria, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_jprod_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nbact_het", "Production of bacterial biomass (nitrogen) via heterotrophy, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_jprod_n_het = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nbact_amx", "Production of bacterial biomass (nitrogen) via anammox, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_jprod_n_amx = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nbact_nitrif", "Production of bacterial biomass (nitrogen) via nitrification, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     bact(1)%id_jprod_n_nitrif = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_h", "growth rate of heterotrophic bacteria", 'h', 'L', 's', 's-1', 'f')
     bact(1)%id_mu_h = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mu_cstar", "biomass turnover due to chemosynthetic processes", 'h', 'L', 's', 's-1', 'f')
     bact(1)%id_mu_cstar = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("bhet", "heterotrophic bacterial biomass", 'h', 'L', 's', 'moles N kg-1', 'f')
     bact(1)%id_bhet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("o2lim_Bact", "Oxygen limitation of bacteria", 'h', 'L', 's', 'dimensionless', 'f')
     bact(1)%id_o2lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ldonlim_Bact", "ldon limitation of bacteria", 'h', 'L', 's', 'dimensionless', 'f')
     bact(1)%id_ldonlim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("temp_lim_Bact", "Temperature limitation of bacteria", 'h', 'L', 's', 'dimensionless', 'f')
     bact(1)%id_temp_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("co3_sol_arag", "Carbonate Ion Solubility for Aragonite", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_co3_sol_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("co3_sol_calc", "Carbonate Ion Solubility for Calcite", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_co3_sol_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("omega_arag", "Carbonate Ion Saturation State for Aragonite", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_omega_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("omega_calc", "Carbonate Ion Saturation State for Calcite", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_omega_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("rho_test", "testing density", 'h', 'L', 's', 'kg-1 m-3', 'f')
     cobalt%id_rho_test = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_cadet_arag", "Aragonite CaCO3 production layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_cadet_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_cadet_calc", "Calcite CaCO3 production layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_cadet_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_lithdet", "Lithogenic detritus production layer integral", 'h', 'L', 's', 'g m-2 s-1', 'f')
     cobalt%id_jprod_lithdet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sidet", "opal detritus production layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     cobalt%id_jprod_sidet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sio4", "sio4 production layer integral", 'h', 'L', 's', 'mol SiO2 m-2 s-1', 'f')
     cobalt%id_jprod_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fedet", "Detrital Fedet production layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jprod_fedet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet", "Detrital PON production layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_jprod_ndet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_pdet", "Detrital phosphorus production layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_jprod_pdet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldon", "labile dissolved organic nitrogen production layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_jprod_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ldop", "labile dissolved organic phosphorous production layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_jprod_ldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdon", "refractory dissolved organic nitrogen production layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_jprod_srdon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_srdop", "refractory dissolved organic phosphorous production layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_jprod_srdop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldon", "semi-labile dissolved organic nitrogen production layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_jprod_sldon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sldop", "semi-labile dissolved organic phosphorous production layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_jprod_sldop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_po4", "phosphate production layer integral", 'h', 'L', 's', 'mol PO4 m-2 s-1', 'f')
     cobalt%id_jprod_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nh4", "NH4 production layer integral", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     cobalt%id_jprod_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nh4_plus_btm", "NH4 production layer integral plus bottom fluxes", 'h', 'L', 's', 'mol NH4 m-2 s-1', 'f')
     cobalt%id_jprod_nh4_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("det_jzloss_n", "nitrogen detritus loss to zooplankton layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_det_jzloss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("det_jhploss_n", "nitrogen detritus loss to higher predators layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_det_jhploss_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdiss_sidet", "SiO2 detritus dissolution, layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jdiss_sidet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdiss_cadet_arag", "CaCO3 detritus dissolution, layer integral", 'h', 'L', 's', 'mol CaCO3 m-2 s-1', 'f')
     cobalt%id_jdiss_cadet_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdiss_cadet_arag_plus_btm", "CaCO3 detritus dissolution plus bottom dissolution, layer integral", 'h', 'L', 's', 'mol CaCO3 m-2 s-1', 'f')
     cobalt%id_jdiss_cadet_arag_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdiss_cadet_calc", "CaCO3 detritus dissolution, layer integral", 'h', 'L', 's', 'mol CaCO3 m-2 s-1', 'f')
     cobalt%id_jdiss_cadet_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdiss_cadet_calc_plus_btm", "CaCO3 detritus dissolution plus bottom dissolution, layer integral", 'h', 'L', 's', 'mol CaCO3 m-2 s-1', 'f')
     cobalt%id_jdiss_cadet_calc_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_ndet", "Nitrogen detritus remineralization, layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_jremin_ndet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_pdet", "Phosphorous detritus remineralization, layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_jremin_pdet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_fedet", "Iron detritus remineralization, layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jremin_fedet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_fed", "dissolved iron production layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jprod_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jfed", "Dissolved Iron Change layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jfed = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jfedc", "Dissolved Iron Change concentration", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jfedc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jfe_ads", "Iron adsorption layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jfe_ads = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jfe_coast", "Coastal iron efflux layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jfe_coast = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jfe_iceberg", "iceberg iron efflux layer integral", 'h', 'L', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_jfe_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jno3_iceberg", "iceberg nitrate efflux layer integral", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_jno3_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jpo4_iceberg", "iceberg phosphate efflux layer integral", 'h', 'L', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_jpo4_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("kfe_eq_lig", "Effective ligand binding strength", 'h', 'L', 's', 'kg mol-1', 'f')
     cobalt%id_kfe_eq_lig = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("feprime", "Free iron concentration", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_feprime = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ligand", "ligand concentration", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_ligand = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fe_sol", "iron solubility", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_fe_sol = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("expkT", "Eppley temperature limitation factor", 'h', 'L', 's', 'dimensionless', 'f')
     cobalt%id_expkT = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("expkreminT", "Detritus remineralization temperature limitation factor", 'h', 'L', 's', 'dimensionless', 'f')
     cobalt%id_expkreminT = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("hp_o2lim", "Oxygen limitation of higher predators", 'h', 'L', 's', 'dimensionless', 'f')
     cobalt%id_hp_o2lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("hp_temp_lim", "Temperature limitation of higher predators", 'h', 'L', 's', 'dimensionless', 'f')
     cobalt%id_hp_temp_lim = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irr_inst", "Instantaneous Light", 'h', 'L', 's', 'W m-2', 'f')
     cobalt%id_irr_inst = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irr_mix", "Instantaneous light, avg over mixing layer", 'h', 'L', 's', 'W m-2', 'f')
     cobalt%id_irr_mix = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("irr_aclm_inst", "Instantaneous light, avg over photoadapt layer", 'h', 'L', 's', 'W m-2', 'f')
     cobalt%id_irr_aclm_inst = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("chl2sfcchl", "ratio of chl to surface chl", 'h', 'L', 's', 'dimensionless', 'f')
     cobalt%id_chl2sfcchl = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_no3nitrif", "Nitrification layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_no3nitrif = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_nh4nitrif", "NH4 uptake via Nitrification layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_juptake_nh4nitrif = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jno3denit_wc", "Water column Denitrification layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jno3denit_wc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_n2amx", "Fixed N loss via Anammox layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_n2amx = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_nh4amx", "NH4 uptake via Anammox layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_juptake_nh4amx = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_no3amx", "NO3 uptake via Anammox layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_juptake_no3amx = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jo2resp_wc", "Water column aerobic respiration layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jo2resp_wc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nphyto_tot", "Total N: Di+Lg+Md+Sm", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_nphyto_tot = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_c", "Total Carbon (DIC+OC+IC) boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_fe", "Total Iron (Fed_OFe) boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_n", "Total Nitrogen (NO3+NH4+ON) boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_p", "Total Phosphorus (PO4+OP) boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_p = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_si", "Total Silicon (SiO4+SiO2) boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_si = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_o2", "Total oxygen boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_o2 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("tot_layer_int_alk", "Total alkalinity boxwise layer integral", 'h', 'L', 's', 'mol m-2', 'f')
     cobalt%id_tot_layer_int_alk = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("total_filter_feeding", "Total filter feeding by large organisms", 'h', 'L', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_total_filter_feeding = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_dry_fed", "Dry Deposition of Iron to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_dry_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_dry_lith", "Dry Deposition of Lithogenic Material", 'h', '1', 's', 'g m-2 s-1', 'f')
     cobalt%id_dep_dry_lith = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_dry_nh4", "Dry Deposition of Ammonia to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_dry_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_dry_no3", "Dry Deposition of Nitrate to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_dry_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_dry_po4", "Dry Deposition of Phosphate to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_dry_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_wet_fed", "Wet Deposition of Iron to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_wet_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_wet_lith", "Wet Deposition of Lithogenic Material", 'h', '1', 's', 'g m-2 s-1', 'f')
     cobalt%id_dep_wet_lith = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_wet_nh4", "Wet Deposition of Ammonia to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_wet_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_wet_no3", "Wet Deposition of Nitrate to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_wet_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dep_wet_po4", "Wet Deposition of Phosphate to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_dep_wet_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("pka_nh3", "pKa of NH3", 'h', '1', 's', '', 'f')
     cobalt%id_pka_nh3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_alk", "Alkalinity runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_alk = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_dic", "Dissolved Inorganic Carbon runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_dic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_di14c", "Dissolved Inorganic Carbon 14 runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_di14c = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_fed", "Iron runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_lith", "Lithogenic runoff flux to the ocean", 'h', '1', 's', 'g m-2 s-1', 'f')
     cobalt%id_runoff_flux_lith = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_no3", "Nitrate runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_ldon", "LDON runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_sldon", "SLDON runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_sldon = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_srdon", "SRDON runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_srdon = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_ndet", "NDET runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_ndet = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_pdet", "PDET runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_pdet = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_po4", "PO4 runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_ldop", "LDOP runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_ldop = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_sldop", "SLDOP runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_sldop = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("runoff_flux_srdop", "SRDOP runoff flux to the ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_runoff_flux_srdop = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcadet_arag", "CaCO3 sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcadet_arag = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcadet_calc", "CaCO3 sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcadet_calc = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffedet", "fedet sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffedet = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("flithdet", "lithdet sinking flux", 'h', '1', 's', 'g m-2 s-1', 'f')
     cobalt%id_flithdet = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fndet", "ndet sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fndet = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpdet", "pdet sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fpdet = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsidet", "sidet sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsidet = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffetot", "total Fe sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffetot = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fntot", "total N sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fntot = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fptot", "total P sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fptot = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsitot", "total Si sinking flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsitot = register_diag_field(package_name, vardesc_temp%name, axesTi(1:1), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcadet_arag_btm", "Aragonite sinking flux at bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcadet_arag_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcadet_calc_btm", "Calcite sinking flux at bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcadet_calc_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcased_burial", "Calcite permanent burial flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcased_burial = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcased_redis", "Calcite redissolution from sediments", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcased_redis = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcased_redis_surfresp", "Calcite redissolution rom sediments, surfresp", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcased_redis_surfresp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("cased_redis_coef", "Calcite redissolution from sediments, deepresp coefficient, ", 'h', '1', 's', 's-1', 'f')
     cobalt%id_cased_redis_coef = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("cased_redis_delz", "Calcite redissolution from sediments, effective depth", 'h', '1', 's', 'none (0-1)', 'f')
     cobalt%id_cased_redis_delz = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffedet_btm", "fedet sinking flux burial", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffedet_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffedi_btm", "diazo Fe sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_ffe_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffelg_btm", "large phyto Fe sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_ffe_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffemd_btm", "medium phyto Fe sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_ffe_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffesm_btm", "small phyto Fe sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_ffe_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffetot_btm", "Total Fe sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffetot_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffe_sed", "Sediment iron efflux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffe_sed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffe_geotherm", "Geothermal iron efflux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffe_geotherm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffe_iceberg", "iceberg iron efflux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffe_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("flithdet_btm", "Lithogenic detrital sinking flux burial", 'h', '1', 's', 'g m-2 s-1', 'f')
     cobalt%id_flithdet_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fndet_btm", "ndet sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fndet_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fndi_btm", "diazo N sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_fn_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fnlg_btm", "large phyto N sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_fn_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fnmd_btm", "medium phyto N sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_fn_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fnsm_btm", "small phyto N sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_fn_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fntot_btm", "Total N sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fntot_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fnfeso4red_sed", "Sediment Ndet Fe and SO4 reduction flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fnfeso4red_sed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fno3denit_sed", "Sediment denitrification flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fno3denit_sed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fnoxic_sed", "Sediment oxic Ndet remineralization flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fnoxic_sed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpdet_btm", "pdet sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fpdet_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpdi_btm", "diazo P sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_fp_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fplg_btm", "large phyto P sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_fp_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpmd_btm", "medium phyto P sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_fp_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpsm_btm", "small phyto P sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_fp_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fptot_btm", "Total P sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fptot_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsidet_btm", "sidet sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsidet_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsilg_btm", "large phyto Si sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_fsi_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsimd_btm", "medium phyto Si sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_fsi_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsitot_btm", "Total Si sinking flux to bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsitot_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("frac_burial", "fraction of organic matter buried", 'h', '1', 's', 'dimensionless', 'f')
     cobalt%id_frac_burial = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fn_burial", "ndet burial flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fn_burial = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fp_burial", "pdet burial flux", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fp_burial = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("pco2surf", "Oceanic pCO2", 'h', '1', 's', 'uatm', 'f')
     cobalt%id_pco2surf = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("pnh3surf", "Oceanic pNH3", 'h', '1', 's', 'uatm', 'f')
     cobalt%id_pnh3surf = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_alk", "Surface Alkalinity", 'h', '1', 's', 'eq kg-1', 'f')
     cobalt%id_sfc_alk = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_cadet_arag", "Surface Detrital Aragonite", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_cadet_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_cadet_calc", "Surface Detrital Calcite", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_cadet_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_dic", "Surface Dissolved Inorganic Carbon", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_dic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_fed", "Surface Dissolved Iron", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_fed = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_ldon", "Surface Labile Dissolved Organic Nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_ldon = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_sldon", "Surface semi-labile Dissolved Organic Nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_sldon = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_srdon", "Surface semi-refractory Dissolved Organic Nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_srdon = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_no3", "Surface NO3", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nh4", "Surface NH4", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_po4", "Surface PO4", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_po4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_sio4", "Surface SiO4", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_sio4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_htotal", "Surface Htotal", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_htotal = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_o2", "Surface Oxygen", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_o2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_chl", "Surface Chl", 'h', '1', 's', 'ug kg-1', 'f')
     cobalt%id_sfc_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irr", "Surface Irradiance", 'h', '1', 's', 'W m-2', 'f')
     cobalt%id_sfc_irr = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irr_aclm", "Surface day irrad. over photacclim. time scale", 'h', '1', 's', 'W m-2', 'f')
     cobalt%id_sfc_irr_aclm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irr_mem_dp", "Surface Irradiance memory, diapause", 'h', '1', 's', 'W m-2', 'f')
     cobalt%id_sfc_irr_mem_dp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_temp", "Surface Temperature", 'h', '1', 's', 'deg C', 'f')
     cobalt%id_sfc_temp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_temp", "Bottom Temperature", 'h', '1', 's', 'deg C', 'f')
     cobalt%id_btm_temp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_temp_old", "Bottom Temperature (k=nk)", 'h', '1', 's', 'deg C', 'f')
     cobalt%id_btm_temp_old = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_o2", "Bottom Oxygen", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_o2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_o2_old", "Bottom Oxygen (k = nk)", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_o2_old = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_no3", "Bottom NO3", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_alk", "Bottom Alkalinity", 'h', '1', 's', 'eq kg-1', 'f')
     cobalt%id_btm_alk = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_dic", "Bottom Dissolved Inorganic Carbon", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_dic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_htotal", "Bottom Htotal", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_htotal = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_htotal_old", "Bottom Htotal (k=nk)", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_htotal_old = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_co3_ion", "Bottom Carbonate Ion", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_co3_ion = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_co3_ion_old", "Bottom Carbonate Ion (k=nk)", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_co3_ion_old = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_co3_sol_arag", "Bottom Aragonite Solubility", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_co3_sol_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_co3_sol_arag_old", "Bottom Aragonite Solubility (k=nk)", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_co3_sol_arag_old = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_co3_sol_calc", "Bottom Calcite Solubility", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_co3_sol_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_co3_sol_calc_old", "Bottom Calcite Solubility (k=nk)", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_btm_co3_sol_calc_old = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_omega_arag", "Bottom saturation state for aragonite", 'h', '1', 's', 'none', 'f')
     cobalt%id_btm_omega_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("btm_omega_calc", "Bottom saturation state for calcite", 'h', '1', 's', 'none', 'f')
     cobalt%id_btm_omega_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("cased_2d", "calcium carbonite in sediment", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_cased_2d = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_co3_ion", "Surface Carbonate Ion", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_co3_ion = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_co3_sol_arag", "Surface Carbonate Ion Solubility for Aragonite", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_co3_sol_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_co3_sol_calc", "Surface Carbonate Ion Solubility for Calcite ", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_sfc_co3_sol_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nsmp", "Surface small phyto. nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     phyto(SMALL )%id_sfc_f_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nmdp", "Surface medium phyto. nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     phyto(MEDIUM)%id_sfc_f_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nlgp", "Surface large phyto. nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     phyto(LARGE )%id_sfc_f_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_ndi", "Surface diazotroph nitrogen", 'h', '1', 's', 'mol kg-1', 'f')
     phyto(DIAZO )%id_sfc_f_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_chl_smp", "Surface small phyto. chlorophyll", 'h', '1', 's', 'ug kg-1', 'f')
     phyto(SMALL )%id_sfc_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_chl_mdp", "Surface medium phyto. chlorophyll", 'h', '1', 's', 'ug kg-1', 'f')
     phyto(MEDIUM)%id_sfc_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_chl_lgp", "Surface large phyto. chlorophyll", 'h', '1', 's', 'ug kg-1', 'f')
     phyto(LARGE )%id_sfc_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_chl_di", "Surface diazotroph chlorophyll", 'h', '1', 's', 'mol kg-1', 'f')
     phyto(DIAZO )%id_sfc_chl = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_def_fe_smp", "Surface small phyto. iron deficiency", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(SMALL )%id_sfc_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_def_fe_mdp", "Surface medium phyto. iron deficiency", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(MEDIUM)%id_sfc_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_def_fe_lgp", "Surface large phyto. iron deficiency", 'h', '1', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_sfc_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_def_fe_di", "Surface diazotroph iron deficiency", 'h', '1', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_sfc_def_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_felim_smp", "Surface small phyto. iron uptake limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(SMALL )%id_sfc_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_felim_mdp", "Surface medium phyto. iron uptake limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(MEDIUM)%id_sfc_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_felim_lgp", "Surface large phyto. iron uptake limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_sfc_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_felim_di", "Surface diazotroph iron uptake limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_sfc_felim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_fe_2_n_di", "Surface diazotroph iron:nitrogen", 'h', '1', 's', 'moles Fe (moles N)-1', 'f')
     phyto(DIAZO )%id_sfc_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_fe_2_n_smp", "Surface small phyto. iron:nitrogen", 'h', '1', 's', 'moles Fe (moles N)-1', 'f')
     phyto(SMALL )%id_sfc_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_fe_2_n_mdp", "Surface medium phyto. iron:nitrogen", 'h', '1', 's', 'moles Fe (moles N)-1', 'f')
     phyto(MEDIUM)%id_sfc_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_fe_2_n_lgp", "Surface large phyto. iron:nitrogen", 'h', '1', 's', 'moles Fe (moles N)-1', 'f')
     phyto(LARGE )%id_sfc_q_fe_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_p_2_n_di", "Surface diazotroph P:N", 'h', '1', 's', 'moles P (moles N)-1', 'f')
     phyto(DIAZO )%id_sfc_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_p_2_n_smp", "Surface small phyto. P:N", 'h', '1', 's', 'moles P (moles N)-1', 'f')
     phyto(SMALL )%id_sfc_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_p_2_n_mdp", "Surface medium phyto. P:N", 'h', '1', 's', 'moles P (moles N)-1', 'f')
     phyto(MEDIUM)%id_sfc_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_q_p_2_n_lgp", "Surface large phyto. P:N", 'h', '1', 's', 'moles P (moles N)-1', 'f')
     phyto(LARGE )%id_sfc_q_p_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irrlim_smp", "Surface small phyto. light limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(SMALL )%id_sfc_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irrlim_mdp", "Surface medium phyto. light limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(MEDIUM)%id_sfc_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irrlim_lgp", "Surface large phyto. light limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_sfc_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_irrlim_di", "Surface diazotroph light limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_sfc_irrlim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_theta_smp", "Surface small phyto. Chl:C", 'h', '1', 's', 'g Chl (g C)-1', 'f')
     phyto(SMALL )%id_sfc_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_theta_mdp", "Surface medium phyto. Chl:C", 'h', '1', 's', 'g Chl (g C)-1', 'f')
     phyto(MEDIUM)%id_sfc_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_theta_lgp", "Surface large phyto. Chl:C", 'h', '1', 's', 'g Chl (g C)-1', 'f')
     phyto(LARGE )%id_sfc_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_theta_di", "Surface diazotroph Chl:C", 'h', '1', 's', 'g Chl (g C)-1', 'f')
     phyto(DIAZO )%id_sfc_theta = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_mu_smp", "Surface small phyto. Chl:C", 'h', '1', 's', 'sec-1', 'f')
     phyto(SMALL )%id_sfc_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_mu_mdp", "Surface medium phyto. Chl:C", 'h', '1', 's', 'sec-1', 'f')
     phyto(MEDIUM)%id_sfc_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_mu_lgp", "Surface large phyto. Chl:C", 'h', '1', 's', 'sec-1', 'f')
     phyto(LARGE )%id_sfc_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_mu_di", "Surface diazotroph growth rate", 'h', '1', 's', 'sec-1', 'f')
     phyto(DIAZO )%id_sfc_mu = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_no3lim_smp", "Surface small phyto. nitrate limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(SMALL )%id_sfc_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_no3lim_mdp", "Surface medium phyto. nitrate limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(MEDIUM)%id_sfc_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_no3lim_lgp", "Surface large phyto. nitrate limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_sfc_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_no3lim_di", "Surface diazotroph nitrate limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_sfc_no3lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nh4lim_smp", "Surface small phyto. ammonia limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(SMALL )%id_sfc_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nh4lim_mdp", "Surface medium phyto. ammonia limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(MEDIUM)%id_sfc_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nh4lim_lgp", "Surface large phyto. ammonia limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_sfc_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_nh4lim_di", "Surface diazotroph ammonia limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_sfc_nh4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_po4lim_smp", "Surface small phyto. phosphate limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(SMALL )%id_sfc_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_po4lim_mdp", "Surface medium phyto. phosphate limitation", 'h', '1', 's', 'dimensionsless', 'f')
     phyto(MEDIUM)%id_sfc_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_po4lim_lgp", "Surface large phyto. phosphate limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(LARGE )%id_sfc_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("sfc_po4lim_di", "Surface diazotroph phosphate limitation", 'h', '1', 's', 'dimensionless', 'f')
     phyto(DIAZO )%id_sfc_po4lim = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_allphytos_100", "Total Nitrogen prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_allphytos_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_allphytos_200", "Total Nitrogen prim. prod. integral in upper 200m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_allphytos_200 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mld_aclm", "mixed layer for photacclimation", 'h', '1', 's', 'm', 'f')
     cobalt%id_mld_aclm = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_diat_100", "Diatom prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_diat_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndi_100", "Diazotroph nitrogen prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nsmp_100", "Small phyto. nitrogen  prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nmdp_100", "Medium phyto. nitrogen  prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nlgp_100", "Large phyto. nitrogen  prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndi_new_100", "Diazotroph new (NO3-based) prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jprod_n_new_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nsmp_new_100", "Small phyto. new (NO3-based) prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jprod_n_new_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nmdp_new_100", "Medium phyto. new (NO3-based) prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jprod_n_new_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nlgp_new_100", "Large phyto. new (NO3-based) prim. prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jprod_n_new_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndi_n2_100", "Diazotroph nitrogen fixation in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jprod_n_n2_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_ndi_100", "Diazotroph nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_nsmp_100", "Small phyto. nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_nmdp_100", "Medium phyto. nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_nlgp_100", "Large phyto. nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_ndi_100", "Diazotroph phyto. nitrogen aggregation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jaggloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_nsmp_100", "Small phyto. nitrogen aggregation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jaggloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_nmdp_100", "Medium phyto. nitrogen aggregation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jaggloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jaggloss_nlgp_100", "Large phyto. nitrogen aggregation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jaggloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_ndi_100", "Diazotroph nitrogen virus loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jvirloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_nsmp_100", "Small phyto. nitrogen virus loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jvirloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_nmdp_100", "Medium phyto. nitrogen virus loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jvirloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_nlgp_100", "Large phyto. nitrogen virus loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jvirloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_ndi_100", "Diazotroph nitrogen mortality loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jmortloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_nsmp_100", "Small phyto. nitrogen mortality loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jmortloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_nmdp_100", "Medium phyto. nitrogen mortality loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jmortloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jmortloss_nlgp_100", "Large phyto. nitrogen mortality loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jmortloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_ndi_100", "Diazotroph nitrogen exudation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(DIAZO )%id_jexuloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_nsmp_100", "Small phyto. nitrogen exudation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(SMALL )%id_jexuloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_nmdp_100", "Medium phyto. nitrogen exudation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(MEDIUM)%id_jexuloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jexuloss_nlgp_100", "Large phyto. nitrogen exudation loss integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     phyto(LARGE )%id_jexuloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nsmz_100", "Small zooplankton nitrogen prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(1)%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nmdz_100", "Medium zooplankton nitrogen prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nlgz_100", "Large zooplankton nitrogen prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(3)%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_nsmz_100", "Small zooplankton nitrogen ingestion integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(1)%id_jingest_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_nmdz_100", "Medium zooplankton nitrogen ingestion integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jingest_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_nlgz_100", "Large zooplankton nitrogen ingestion integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(3)%id_jingest_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_nsmz_100", "Small zooplankton nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(1)%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_nmdz_100", "Medium zooplankton nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jhploss_nmdz_100", "Medium zooplankton nitrogen loss to higher preds. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jhploss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jhploss_nlgz_100", "Large zooplankton nitrogen loss to higher preds. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(3)%id_jhploss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet_nmdz_100", "Medium zooplankton nitrogen detritus prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jprod_ndet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet_nlgz_100", "Large zooplankton nitrogen detritus prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(3)%id_jprod_ndet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_don_nsmz_100", "Small zooplankton dissolved org. nitrogen prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(1)%id_jprod_don_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_don_nmdz_100", "Medium zooplankton dissolved org. nitrogen prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jprod_don_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_n_nsmz_100", "Small zooplankton nitrogen remineralization integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(1)%id_jremin_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_n_nmdz_100", "Medium zooplankton nitrogen remineralization integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(2)%id_jremin_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_n_nlgz_100", "Large zooplankton nitrogen remineralization integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     zoo(3)%id_jremin_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_n_hp_100", "Higher predator nitrogen remineralization integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_hp_jremin_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jingest_n_hp_100", "Higher predator ingestion of nitrogen integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_hp_jingest_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_ndet_hp_100", "Higher predator nitrogen detritus prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_hp_jprod_ndet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_nbact_100", "Bacteria nitrogen prod. integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     bact(1)%id_jprod_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jzloss_nbact_100", "Bacteria nitrogen loss to zooplankton integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     bact(1)%id_jzloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jvirloss_nbact_100", "Bacteria nitrogen loss to viruses integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     bact(1)%id_jvirloss_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_n_nbact_100", "Bacteria nitrogen remineralization integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     bact(1)%id_jremin_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("juptake_ldon_nbact_100", "Bacterial uptake of labile dissolved org. nitrogen in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     bact(1)%id_juptake_ldon_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_lithdet_100", "Lithogenic detritus production integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_lithdet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_sidet_100", "Silica detritus production integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_sidet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_cadet_calc_100", "Calcite detritus production integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_cadet_calc_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_cadet_arag_100", "Aragonite detritus production integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_cadet_arag_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jremin_ndet_100", "Remineralization of nitrogen detritus integral in upper 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jremin_ndet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jprod_mesozoo_200", "Mesozooplankton Production, 200m integration", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jprod_mesozoo_200 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("dp_fac", "diapause factor", 'h', '1', 's', 'none', 'f')
     cobalt%id_dp_fac = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("daylength", "daylength", 'h', '1', 's', 'hours', 'f')
     cobalt%id_daylength = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_chemoautopp", "Water column chemoautrophy vertical integral", 'h', '1', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_wc_vert_int_chemoautopp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_net_phyto_resp", "Water column net phyto respiration vertical integral", 'h', '1', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_wc_vert_int_net_phyto_resp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_npp", "Water column net primary production vertical integral", 'h', '1', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_wc_vert_int_npp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jdiss_sidet", "Water column silica dissolution vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jdiss_sidet = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jdiss_cadet", "Water column calcium carbonate dissolution vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jdiss_cadet = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jo2resp", "Water column oxygen respired vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jo2resp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jprod_cadet", "Water column calcium carbonate production vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jprod_cadet = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jno3denit", "Water column denitrification vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jno3denit = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jprod_no3nitrif", "Water column nitrification vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jprod_no3nitrif = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jprod_n2amx", "Water column N2 from anammox vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jprod_n2amx = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_juptake_nh4", " Water column ammonia based NPP vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_juptake_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jprod_nh4", " Water column ammonia production vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jprod_nh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_juptake_no3", "Water column nitrate based NPP, vertical integral", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_wc_vert_int_juptake_no3 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_nfix", "Nitrogen fixation vertical integral", 'h', '1', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_wc_vert_int_nfix = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jfe_iceberg", "iceberg dissolved iron, vertical integral", 'h', '1', 's', 'mol Fe m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jfe_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jno3_iceberg", "iceberg nitrate, vertical integral", 'h', '1', 's', 'mol N m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jno3_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_jpo4_iceberg", "iceberg phosphate, vertical integral", 'h', '1', 's', 'mol P m-2 s-1', 'f')
     cobalt%id_wc_vert_int_jpo4_iceberg = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nsmp_100", "Small phytoplankton nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     phyto(SMALL )%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nmdp_100", "Medium phytoplankton nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     phyto(MEDIUM)%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nlgp_100", "Large phytoplankton nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     phyto(LARGE )%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ndi_100", "Diazotroph nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     phyto(DIAZO )%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nsmz_100", "Small zooplankton nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     zoo(1)%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nmdz_100", "Medium zooplankton nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     zoo(2)%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nlgz_100", "Large zooplankton nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     zoo(3)%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("nbact_100", "Bacterial nitrogen biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     bact(1)%id_f_n_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("simdp_100", "Medium phytoplankton silicon biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_f_simd_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("silgp_100", "Large phytoplankton silicon biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_f_silg_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ndet_100", "Nitrogen detritus biomass in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_f_ndet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("don_100", "Dissolved organic nitrogen (sr+sl+l) in upper 100m", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_f_don_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("mesozoo_200", "Mesozooplankton biomass, 200m integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_f_mesozoo_200 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_c", "Total carbon (DIC+OC+IC) vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_c = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_dic", "Dissolved inorganic carbon vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_dic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_doc", "Total dissolved organic carbon  vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_doc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_poc", "Total particulate organic carbon vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_poc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_n", "Total nitrogen vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_n = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_p", "Total phosphorus vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_p = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_fe", "Total iron vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_fe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_si", "Total silicon vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_si = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_o2", "Total oxygen vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_o2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("wc_vert_int_alk", "Total alkalinity vertical integral", 'h', '1', 's', 'mol m-2', 'f')
     cobalt%id_wc_vert_int_alk = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fndet_100", "Nitrogen detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fndet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpdet_100", "Phosphorous detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fpdet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffedet_100", "Iron detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffedet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsidet_100", "Silicon detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsidet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcadet_calc_100", "Calcite detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcadet_calc_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fcadet_arag_100", "Aragonite detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fcadet_arag_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("flithdet_100", "Lithogenic detritus sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_flithdet_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fntot_100", "total nitrogen sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fntot_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fptot_100", "total phosphorous sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fptot_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("ffetot_100", "total iron sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ffetot_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fsitot_100", "total silicon sinking flux @ 100m", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsitot_100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("o2min", "Minimum Oxygen", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_o2min = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("z_o2min", "Depth of Oxygen minimum", 'h', '1', 's', 'm', 'f')
     cobalt%id_z_o2min = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("z_sat_arag", "Depth of Aragonite Saturation", 'h', '1', 's', 'm', 'f')
     cobalt%id_z_sat_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          mask_variant = .TRUE.)
     vardesc_temp = vardesc("z_sat_calc", "Depth of Calcite Saturation", 'h', '1', 's', 'm', 'f')
     cobalt%id_z_sat_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          mask_variant = .TRUE.)
     if (do_14c) then
     vardesc_temp = vardesc("b_di14c", "Bottom flux of DI14C into sediment", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_b_di14c = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("c14_2_n", "Ratio of DI14C to N-nutrients", 'h', 'L', 's', 'mol kg-1', 'f')
     cobalt%id_c14_2_n = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("c14o2_alpha", "Saturation surface 14CO2* per uatm", 'h', '1', 's', 'mol kg-1 atm-1', 'f')
     cobalt%id_c14o2_alpha = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("c14o2_csurf", "14CO2* concentration at surface", 'h', '1', 's', 'mol kg-1', 'f')
     cobalt%id_c14o2_csurf = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("fpo14c", "PO14C sinking flux at layer bottom", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fpo14c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("j14c_decay_dic", "DI14C radioactive decay layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_j14c_decay_dic = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("j14c_decay_doc", "DO14C radioactive decay layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_j14c_decay_doc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("j14c_reminp", "Sinking PO14C remineralization layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_j14c_reminp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdi14c", "DI14C source layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jdi14c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdo14c", "DO14C source layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jdo14c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     endif
     vardesc_temp = vardesc("jalk", "Alkalinity source layer integral", 'h', 'L', 's', 'eq m-2 s-1', 'f')
     cobalt%id_jalk = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jalkc", "Alkalinity source layer concentration", 'h', 'L', 's', 'eq m-3 s-1', 'f')
     cobalt%id_jalkc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jalk_plus_btm", "Alkalinity source plus btm layer integral", 'h', 'L', 's', 'eq m-2 s-1', 'f')
     cobalt%id_jalk_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdic", "Dissolved Inorganic Carbon source layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jdic = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdicc", "Dissolved Inorganic Carbon source concentration", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jdicc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jno3c", "no3 source concentration", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jno3c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jpo4c", "po4 source concentration", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jpo4c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jsio4c", "sio4 source concentration", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jsio4c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jdic_plus_btm", "Dissolved Inorganic Carbon source plus btm layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jdic_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jnh4", "NH4 source layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jnh4 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jndet", "NDET source layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jndet = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jnh4_plus_btm", "NH4 source plus btm layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jnh4_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jo2_plus_btm", "O2 source plus btm layer integral", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_jo2_plus_btm = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jo2", "O2 source concentration", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_jo2 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("jo2c", "O2 source concentration", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_jo2c = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1)
     vardesc_temp = vardesc("temp", "Potential Temperature", 'h', 'L', 's', 'Celsius', 'f')
     cobalt%id_thetao = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "thetao", &
          cmor_units = "C", &
          cmor_standard_name = "sea_water_potential_temperature", &
          cmor_long_name = "Sea Water Potential Temperature")
     vardesc_temp = vardesc("dissic_raw", "Dissolved Inorganic Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_dissic = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissic", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon_in_sea_water", &
          cmor_long_name = "Dissolved Inorganic Carbon Concentration")
     vardesc_temp = vardesc("dissicnat_raw", "Natural Dissolved Inorganic Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_dissicnat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissicnat", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon_natural_analogue_in_sea_water", &
          cmor_long_name = "Natural Dissolved Inorganic Carbon Concentration")
     vardesc_temp = vardesc("dissicabio_raw", "Abiotic Dissolved Inorganic Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_dissicabio = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissicabio", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon_abiotic_analogue_in_sea_water", &
          cmor_long_name = "Abiotic Dissolved Inorganic Carbon Concentration")
     vardesc_temp = vardesc("dissi14cabio_raw", "Abiotic Dissolved Inorganic 14Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_dissi14cabio = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissi14cabio", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon14_abiotic_analogue_in_sea_water", &
          cmor_long_name = "Abiotic Dissolved Inorganic 14Carbon Concentration")
     vardesc_temp = vardesc("dissoc_raw", "Dissolved Organic Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_dissoc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissoc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_organic_carbon_in_sea_water", &
          cmor_long_name = "Dissolved Organic Carbon Concentration")
     vardesc_temp = vardesc("phyc_raw", "Phytoplankton Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phyc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phyc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Phytoplankton Carbon Concentration")
     vardesc_temp = vardesc("zooc_raw", "Zooplankton Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_zooc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zooc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_zooplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Zooplankton Carbon Concentration")
     vardesc_temp = vardesc("bacc_raw", "Bacterial Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_bacc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bacc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_bacteria_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Bacterial Carbon Concentration")
     vardesc_temp = vardesc("detoc_raw", "Detrital Organic Carbon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_detoc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "detoc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_organic_detritus_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Detrital Organic Carbon Concentration")
     vardesc_temp = vardesc("calc_raw", "Calcite Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_calc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "calc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_calcite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Calcite Concentration")
     vardesc_temp = vardesc("arag_raw", "Aragonite Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_arag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "arag", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Aragonite Concentration")
     vardesc_temp = vardesc("phydiat_raw", "Mole Concentration of Diatoms expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phydiat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phydiat", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_diatoms_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Diatoms expressed as Carbon in sea water")
     vardesc_temp = vardesc("phydiaz_raw", "Mole Concentration of Diazotrophs expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phydiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phydiaz", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_diazotrophs_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Diazotrophs expressed as Carbon in sea water")
     vardesc_temp = vardesc("phypico_raw", "Mole Concentration of Picophytoplankton expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phypico = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phypico", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_picophytoplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Picophytoplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("phymisc_raw", "Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phymisc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phymisc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_miscellaneous_phytoplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("zmicro_raw", "Mole Concentration of Microzooplankton expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_zmicro = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zmicro", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_microzooplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Microzooplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("zmeso_raw", "Mole Concentration of Mesozooplankton expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_zmeso = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zmeso", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_mesozooplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Mesozooplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("talk_raw", "Total Alkalinity", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_talk = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "talk", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "sea_water_alkalinity_expressed_as_mole_equivalent", &
          cmor_long_name = "Total Alkalinity")
     vardesc_temp = vardesc("talknat_raw", "Natural Total Alkalinity", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_talknat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "talknat", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "sea_water_alkalinity_natural_analogue_expressed_as_mole_equivalent", &
          cmor_long_name = "Natural Total Alkalinity")
     vardesc_temp = vardesc("ph_raw", "pH", 'h', 'L', 's', '1', 'f')
     cobalt%id_ph = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "ph", &
          cmor_units = "1", &
          cmor_standard_name = "sea_water_ph_reported_on_total_scale", &
          cmor_long_name = "pH")
     vardesc_temp = vardesc("phnat_raw", "Natural pH", 'h', 'L', 's', '1', 'f')
     cobalt%id_phnat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phnat", &
          cmor_units = "1", &
          cmor_standard_name = "sea_water_ph_natural_analogue_reported_on_total_scale", &
          cmor_long_name = "Natural pH")
     vardesc_temp = vardesc("phabio_raw", "Abiotic pH", 'h', 'L', 's', '1', 'f')
     cobalt%id_phabio = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phabio", &
          cmor_units = "1", &
          cmor_standard_name = "sea_water_ph_abiotic_analogue_reported_on_total_scale", &
          cmor_long_name = "Abiotic pH")
     vardesc_temp = vardesc("o2_raw", "Dissolved Oxygen Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_o2_cmip = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "o2_cmip", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", &
          cmor_long_name = "Dissolved Oxygen Concentration")
     vardesc_temp = vardesc("o2sat_raw", "Dissolved Oxygen Concentration at Saturation", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_o2sat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "o2sat", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water_at_saturation", &
          cmor_long_name = "Dissolved Oxygen Concentration at Saturation")
     vardesc_temp = vardesc("no3_raw", "Dissolved Nitrate Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_no3_cmip = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "no3_cmip", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_nitrate_in_sea_water", &
          cmor_long_name = "Dissolved Nitrate Concentration")
     vardesc_temp = vardesc("nh4_raw", "Dissolved Ammonium Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_nh4_cmip = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "nh4_cmip", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_ammonium_in_sea_water", &
          cmor_long_name = "Dissolved Ammonium Concentration")
     vardesc_temp = vardesc("po4_raw", "Total Dissolved Inorganic Phosphorus Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_po4_cmip = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "po4_cmip", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water", &
          cmor_long_name = "Total Dissolved Inorganic Phosphorus Concentration")
     vardesc_temp = vardesc("dfe_raw", "Dissolved Iron Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_dfe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dfe", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_iron_in_sea_water", &
          cmor_long_name = "Dissolved Iron Concentration")
     vardesc_temp = vardesc("si_raw", "Total Dissolved Inorganic Silicon Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_si = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "si", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_silicon_in_sea_water", &
          cmor_long_name = "Total Dissolved Inorganic Silicon Concentration")
     vardesc_temp = vardesc("chl_raw", "Mass Concentration of Total Phytoplankton expressed as Chlorophyll in sea water", 'h', 'L', 's', 'kg m-3', 'f')
     cobalt%id_chl_cmip = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chl_cmip", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Mass Concentration of Total Phytoplankton expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chldiat_raw", "Mass Concentration of Diatoms expressed as Chlorophyll in sea water", 'h', 'L', 's', 'kg m-3', 'f')
     cobalt%id_chldiat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chldiat", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_diatoms_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Mass Concentration of Diatoms expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chldiaz_raw", "Mass Concentration of Diazotrophs expressed as Chlorophyll in sea water", 'h', 'L', 's', 'kg m-3', 'f')
     cobalt%id_chldiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chldiaz", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_diazotrophs_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Mass Concentration of Diazotrophs expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chlpico_raw", "Mass Concentration of Picophytoplankton expressed as Chlorophyll in sea water", 'h', 'L', 's', 'kg m-3', 'f')
     cobalt%id_chlpico = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chlpico", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_picophytoplankton_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Mass Concentration of Picophytoplankton expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chlmisc_raw", "Mass Concentration of Other Phytoplankton expressed as Chlorophyll in sea water", 'h', 'L', 's', 'kg m-3', 'f')
     cobalt%id_chlmisc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chlmisc", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_miscellaneous_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Mass Concentration of Other Phytoplankton expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("poc_raw", "Mole Concentration of Particulate Organic Matter expressed as Carbon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_poc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "poc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Particulate Organic Matter expressed as Carbon in sea water")
     vardesc_temp = vardesc("pon_raw", "Mole Concentration of Particulate Organic Matter expressed as Nitrogen in sea water", 'h', 'L', 's', 'mol N m-3', 'f')
     cobalt%id_pon = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pon", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_nitrogen_in_sea_water", &
          cmor_long_name = "Mole Concentration of Particulate Organic Matter expressed as Nitrogen in sea water")
     vardesc_temp = vardesc("pop_raw", "Mole Concentration of Particulate Organic Matter expressed as Phosphorus in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_pop = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pop", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_phosphorus_in_sea_water", &
          cmor_long_name = "Mole Concentration of Particulate Organic Matter expressed as Phosphorus in sea water")
     vardesc_temp = vardesc("bfe_raw", "Mole Concentration of Particulate Organic Matter expressed as Iron in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_bfe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bfe", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_iron_in_sea_water", &
          cmor_long_name = "Mole Concentration of Particulate Organic Matter expressed as Iron in sea water")
     vardesc_temp = vardesc("bsi_raw", "Mole Concentration of Particulate Organic Matter expressed as Silicon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_bsi = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bsi", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_silicon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Particulate Organic Matter expressed as Silicon in sea water")
     vardesc_temp = vardesc("phyn_raw", "Mole Concentration of Total Phytoplankton expressed as Nitrogen in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phyn = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phyn", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_nitrogen_in_sea_water", &
          cmor_long_name = "Mole Concentration of Total Phytoplankton expressed as Nitrogen in sea water")
     vardesc_temp = vardesc("phyp_raw", "Mole Concentration of Total Phytoplankton expressed as Phosphorus in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phyp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phyp", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_phosphorus_in_sea_water", &
          cmor_long_name = "Mole Concentration of Total Phytoplankton expressed as Phosphorus in sea water")
     vardesc_temp = vardesc("phyfe_raw", "Mole Concentration of Total Phytoplankton expressed as Iron in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_phyfe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phyfe", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_iron_in_sea_water", &
          cmor_long_name = "Mole Concentration of Total Phytoplankton expressed as Iron in sea water")
     vardesc_temp = vardesc("physi_raw", "Mole Concentration of Total Phytoplankton expressed as Silicon in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_physi = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "physi", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_silicon_in_sea_water", &
          cmor_long_name = "Mole Concentration of Total Phytoplankton expressed as Silicon in sea water")
     vardesc_temp = vardesc("co3_raw", "Carbonate Ion Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_co3 = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Carbonate Ion Concentration")
     vardesc_temp = vardesc("co3nat_raw", "Natural Carbonate Ion Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_co3nat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3nat", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_natural_analogue_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Natural Carbonate Ion Concentration")
     vardesc_temp = vardesc("co3abio_raw", "Abiotic Carbonate Ion Concentration", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_co3abio = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3abio", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_abiotic_analogue_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Abiotic Carbonate Ion Concentration")
     vardesc_temp = vardesc("co3satcalc_raw", "Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_co3satcalc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3satcalc", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_calcite_in_sea_water", &
          cmor_long_name = "Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in sea water")
     vardesc_temp = vardesc("co3satarag_raw", "Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in sea water", 'h', 'L', 's', 'mol m-3', 'f')
     cobalt%id_co3satarag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3satarag", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_aragonite_in_sea_water", &
          cmor_long_name = "Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in sea water")
     vardesc_temp = vardesc("pp_raw", "Primary Carbon Production by Total Phytoplankton", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_pp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pp", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production", &
          cmor_long_name = "Primary Carbon Production by Total Phytoplankton")
     vardesc_temp = vardesc("pnitrate_raw", "Primary Carbon Production by Phytoplankton due to Nitrate Uptake Alone", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_pnitrate = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pnitrate", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_nitrate_utilization", &
          cmor_long_name = "Primary Carbon Production by Phytoplankton due to Nitrate Uptake Alone")
     vardesc_temp = vardesc("pbfe_raw", "Biogenic Iron Production", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_pbfe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pbfe", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_iron_in_sea_water_due_to_biological_production", &
          cmor_long_name = "Biogenic Iron Production")
     vardesc_temp = vardesc("pbsi_raw", "Biogenic Silicon Production", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_pbsi = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pbsi", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_silicon_in_sea_water_due_to_biological_production", &
          cmor_long_name = "Biogenic Silicon Production")
     vardesc_temp = vardesc("pcalc_raw", "Calcite Production", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_pcalc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pcalc", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_calcite_expressed_as_carbon_in_sea_water_due_to_biological_production", &
          cmor_long_name = "Calcite Production")
     vardesc_temp = vardesc("parag_raw", "Aragonite Production", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_parag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "parag", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water_due_to_biological_production", &
          cmor_long_name = "Aragonite Production")
     vardesc_temp = vardesc("expc_raw", "Sinking Particulate Organic Carbon Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_expc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "expc", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Sinking Particulate Organic Carbon Flux")
     vardesc_temp = vardesc("expn_raw", "Sinking Particulate Organic Nitrogen Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_expn = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "expn", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_organic_nitrogen_in_sea_water", &
          cmor_long_name = "Sinking Particulate Organic Nitrogen Flux")
     vardesc_temp = vardesc("expp_raw", "Sinking Particulate Organic Phosphorus Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_expp = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "expp", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water", &
          cmor_long_name = "Sinking Particulate Organic Phosphorus Flux")
     vardesc_temp = vardesc("expfe_raw", "Sinking Particulate Iron Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_expfe = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "expfe", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_iron_in_sea_water", &
          cmor_long_name = "Sinking Particulate Iron Flux")
     vardesc_temp = vardesc("expsi_raw", "Sinking Particulate Silicon Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_expsi = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "expsi", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_silicon_in_sea_water", &
          cmor_long_name = "Sinking Particulate Silicon Flux")
     vardesc_temp = vardesc("expcalc_raw", "Sinking Calcite Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_expcalc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "expcalc", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_calcite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Sinking Calcite Flux")
     vardesc_temp = vardesc("exparag_raw", "Sinking Aragonite Flux", 'h', 'L', 's', 'mol m-2 s-1', 'f')
     cobalt%id_exparag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "exparag", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_aragonite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Sinking Aragonite Flux")
     vardesc_temp = vardesc("remoc_raw", "Remineralization of Organic Carbon", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_remoc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "remoc", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic matter_expressed_as_carbon_in_sea_water_due_to_remineralization", &
          cmor_long_name = "Remineralization of Organic Carbon")
     vardesc_temp = vardesc("dcalc_raw", "Calcite Dissolution", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_dcalc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dcalc", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_calcite_expressed_as_carbon_in_sea_water_due_to_dissolution", &
          cmor_long_name = "Calcite Dissolution")
     vardesc_temp = vardesc("darag_raw", "Aragonite Dissolution", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_darag = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "darag", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water_due_to_dissolution", &
          cmor_long_name = "Aragonite Dissolution")
     vardesc_temp = vardesc("ppdiat_raw", "Net Primary Organic Carbon Production by Diatoms", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_ppdiat = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "ppdiat", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_diatoms", &
          cmor_long_name = "Net Primary Organic Carbon Production by Diatoms")
     vardesc_temp = vardesc("ppdiaz_raw", "Net Primary Mole Productivity of Carbon by Diazotrophs", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_ppdiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "ppdiaz", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_diazotrophs", &
          cmor_long_name = "Net Primary Mole Productivity of Carbon by Diazotrophs")
     vardesc_temp = vardesc("pppico_raw", "Net Primary Mole Productivity of Carbon by Picophytoplankton", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_pppico = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "pppico", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_picophytoplankton", &
          cmor_long_name = "Net Primary Mole Productivity of Carbon by Picophytoplankton")
     vardesc_temp = vardesc("ppmisc_raw", "Net Primary Organic Carbon Production by Other Phytoplankton", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_ppmisc = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "ppmisc", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_miscellaneous_phytoplankton", &
          cmor_long_name = "Net Primary Organic Carbon Production by Other Phytoplankton")
     vardesc_temp = vardesc("bddtdic_raw", "Rate of Change of Dissolved Inorganic Carbon due to Biological Activity", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_bddtdic = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bddtdic", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_inorganic_carbon_in_sea_water_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Carbon due to Biological Activity")
     vardesc_temp = vardesc("bddtdin_raw", "Rate of Change of Nitrogen Nutrient due to Biological Activity", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_bddtdin = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bddtdin", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_inorganic_nitrogen_in_sea_water_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Nitrogen Nutrient due to Biological Activity")
     vardesc_temp = vardesc("bddtdip_raw", "Rate of Change of Dissolved Phosphorus due to Biological Activity", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_bddtdip = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bddtdip", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Phosphorus due to Biological Activity")
     vardesc_temp = vardesc("bddtdife_raw", "Rate of Change of Dissolved Inorganic Iron due to Biological Activity", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_bddtdife = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bddtdife", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_inorganic_iron_in_sea_water_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Iron due to Biological Activity")
     vardesc_temp = vardesc("bddtdisi_raw", "Rate of Change of Dissolved Inorganic Silicon due to Biological Activity", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_bddtdisi = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bddtdisi", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_inorganic_silicon_in_sea_water_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Silicon due to Biological Activity")
     vardesc_temp = vardesc("bddtalk_raw", "Rate of Change of Alkalinity due to Biological Activity", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_bddtalk = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bddtalk", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_sea_water_alkalinity_expressed_as_mole_equivalent_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Alkalinity due to Biological Activity")
     vardesc_temp = vardesc("fescav_raw", "Nonbiogenic Iron Scavenging", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_fescav = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fescav", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_iron_in_sea_water_due_to_scavenging_by_inorganic_particles", &
          cmor_long_name = "Nonbiogenic Iron Scavenging")
     vardesc_temp = vardesc("fediss_raw", "Particle Source of Dissolved Iron", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_fediss = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fediss", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_dissolved_iron_in_sea_water_due_to_dissolution_from_inorganic_particles", &
          cmor_long_name = "Particle Source of Dissolved Iron")
     vardesc_temp = vardesc("graz_raw", "Total Grazing of Phytoplankton by Zooplankton", 'h', 'L', 's', 'mol m-3 s-1', 'f')
     cobalt%id_graz = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "graz", &
          cmor_units = "mol m-3 s-1", &
          cmor_standard_name = "tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_grazing_of_phytoplankton", &
          cmor_long_name = "Total Grazing of Phytoplankton by Zooplankton")
     vardesc_temp = vardesc("limndiat_raw", "Nitrogen Limitation of Diatoms", 'h', '1', 's', '1', 'f')
     cobalt%id_limndiat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limndiat", &
          cmor_units = "1", &
          cmor_standard_name = "nitrogen_growth_limitation_of_diatoms", &
          cmor_long_name = "Nitrogen Limitation of Diatoms")
     vardesc_temp = vardesc("limndiaz_raw", "Nitrogen Limitation of Diazotrophs", 'h', '1', 's', '1', 'f')
     cobalt%id_limndiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limndiaz", &
          cmor_units = "1", &
          cmor_standard_name = "nitrogen_growth_limitation_of_diazotrophs", &
          cmor_long_name = "Nitrogen Limitation of Diazotrophs")
     vardesc_temp = vardesc("limnpico_raw", "Nitrogen Limitation of Picophytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limnpico = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limnpico", &
          cmor_units = "1", &
          cmor_standard_name = "nitrogen_growth_limitation_of_picophytoplankton", &
          cmor_long_name = "Nitrogen Limitation of Picophytoplankton")
     vardesc_temp = vardesc("limnmisc_raw", "Nitrogen Limitation of Other Phytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limnmisc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limnmisc", &
          cmor_units = "1", &
          cmor_standard_name = "nitrogen_growth_limitation_of_miscellaneous_phytoplankton", &
          cmor_long_name = "Nitrogen Limitation of Other Phytoplankton")
     vardesc_temp = vardesc("limirrdiat_raw", "Irradiance Limitation of Diatoms", 'h', '1', 's', '1', 'f')
     cobalt%id_limirrdiat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limirrdiat", &
          cmor_units = "1", &
          cmor_standard_name = "growth_limitation_of_diatoms_due_to_solar_irradiance", &
          cmor_long_name = "Irradiance Limitation of Diatoms")
     vardesc_temp = vardesc("limirrdiaz_raw", "Irradiance Limitation of Diazotrophs", 'h', '1', 's', '1', 'f')
     cobalt%id_limirrdiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limirrdiaz", &
          cmor_units = "1", &
          cmor_standard_name = "growth_limitation_of_diazotrophs_due_to_solar_irradiance", &
          cmor_long_name = "Irradiance Limitation of Diazotrophs")
     vardesc_temp = vardesc("limirrpico_raw", "Irradiance Limitation of Picophytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limirrpico = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limirrpico", &
          cmor_units = "1", &
          cmor_standard_name = "growth_limitation_of_picophytoplankton_due_to_solar_irradiance", &
          cmor_long_name = "Irradiance Limitation of Picophytoplankton")
     vardesc_temp = vardesc("limirrmisc_raw", "Irradiance Limitation of Other Phytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limirrmisc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limirrmisc", &
          cmor_units = "1", &
          cmor_standard_name = "growth_limitation_of_miscellaneous_phytoplankton_due_to_solar_irradiance", &
          cmor_long_name = "Irradiance Limitation of Other Phytoplankton")
     vardesc_temp = vardesc("limfediat_raw", "Iron Limitation of Diatoms", 'h', '1', 's', '1', 'f')
     cobalt%id_limfediat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limfediat", &
          cmor_units = "1", &
          cmor_standard_name = "iron_growth_limitation_of_diatoms", &
          cmor_long_name = "Iron Limitation of Diatoms")
     vardesc_temp = vardesc("limfediaz_raw", "Iron Limitation of Diazotrophs", 'h', '1', 's', '1', 'f')
     cobalt%id_limfediaz = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limfediaz", &
          cmor_units = "1", &
          cmor_standard_name = "iron_growth_limitation_of_diazotrophs", &
          cmor_long_name = "Iron Limitation of Diazotrophs")
     vardesc_temp = vardesc("limfepico_raw", "Iron Limitation of Picophytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limfepico = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limfepico", &
          cmor_units = "1", &
          cmor_standard_name = "iron_growth_limitation_of_picophytoplankton", &
          cmor_long_name = "Iron Limitation of Picophytoplankton")
     vardesc_temp = vardesc("limfemisc_raw", "Iron Limitation of Other Phytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limfemisc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limfemisc", &
          cmor_units = "1", &
          cmor_standard_name = "iron_growth_limitation_of_miscellaneous_phytoplankton", &
          cmor_long_name = "Iron Limitation of Other Phytoplankton")
     vardesc_temp = vardesc("limpdiat_raw", "Phosphorus Limitation of Diatoms", 'h', '1', 's', '1', 'f')
     cobalt%id_limpdiat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limpdiat", &
          cmor_units = "1", &
          cmor_standard_name = "phosphorus_growth_limitation_of_diatoms", &
          cmor_long_name = "Phosphorus Limitation of Diatoms")
     vardesc_temp = vardesc("limpdiaz_raw", "Phosphorus Limitation of Diazotrophs", 'h', '1', 's', '1', 'f')
     cobalt%id_limpdiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limpdiaz", &
          cmor_units = "1", &
          cmor_standard_name = "phosphorus_growth_limitation_of_diazotrophs", &
          cmor_long_name = "Phosphorus Limitation of Diazotrophs")
     vardesc_temp = vardesc("limppico_raw", "Phosphorus Limitation of Picophytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limppico = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limppico", &
          cmor_units = "1", &
          cmor_standard_name = "phosphorus_growth_limitation_of_picophytoplankton", &
          cmor_long_name = "Phosphorus Limitation of Picophytoplankton")
     vardesc_temp = vardesc("limpmisc_raw", "Phosphorus Limitation of Other Phytoplankton", 'h', '1', 's', '1', 'f')
     cobalt%id_limpmisc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "limpmisc", &
          cmor_units = "1", &
          cmor_standard_name = "phosphorus_growth_limitation_of_miscellaneous_phytoplankton", &
          cmor_long_name = "Phosphorus Limitation of Other Phytoplankton")
     vardesc_temp = vardesc("dissicos_raw", "Surface Dissolved Inorganic Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_dissicos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissicos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon_in_sea_water", &
          cmor_long_name = "Surface Dissolved Inorganic Carbon Concentration")
     vardesc_temp = vardesc("dissicnatos_raw", "Surface Natural Dissolved Inorganic Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_dissicnatos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissicnatos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon_natural_analogue_in_sea_water", &
          cmor_long_name = "Surface Natural Dissolved Inorganic Carbon Concentration")
     vardesc_temp = vardesc("dissicabioos_raw", "Surface Abiotic Dissolved Inorganic Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_dissicabioos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissicabioos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon_abiotic_analogue_in_sea_water", &
          cmor_long_name = "Surface Abiotic Dissolved Inorganic Carbon Concentration")
     vardesc_temp = vardesc("dissi14cabioos_raw", "Surface Abiotic Dissolved Inorganic 14Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_dissi14cabioos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissi14cabioos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_carbon14_abiotic_analogue_in_sea_water", &
          cmor_long_name = "Surface Abiotic Dissolved Inorganic 14Carbon Concentration")
     vardesc_temp = vardesc("dissocos_raw", "Surface Dissolved Organic Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_dissocos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dissocos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_organic_carbon_in_sea_water", &
          cmor_long_name = "Surface Dissolved Organic Carbon Concentration")
     vardesc_temp = vardesc("phycos_raw", "Surface Phytoplankton Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phycos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phycos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Phytoplankton Carbon Concentration")
     vardesc_temp = vardesc("zoocos_raw", "Surface Zooplankton Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_zoocos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zoocos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_zooplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Zooplankton Carbon Concentration")
     vardesc_temp = vardesc("baccos_raw", "Surface Bacterial Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_baccos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "baccos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_bacteria_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Bacterial Carbon Concentration")
     vardesc_temp = vardesc("detocos_raw", "Surface Detrital Organic Carbon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_detocos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "detocos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_organic_detritus_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Detrital Organic Carbon Concentration")
     vardesc_temp = vardesc("calcos_raw", "Surface Calcite Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_calcos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "calcos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_calcite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Calcite Concentration")
     vardesc_temp = vardesc("aragos_raw", "Surface Aragonite Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_aragos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "aragos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Aragonite Concentration")
     vardesc_temp = vardesc("phydiatos_raw", "Surface Mole Concentration of Diatoms expressed as Carbon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phydiatos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phydiatos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_diatoms_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Diatoms expressed as Carbon in sea water")
     vardesc_temp = vardesc("phydiazos_raw", "Surface Mole Concentration of Diazotrophs expressed as Carbon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phydiazos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phydiazos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_diazotrophs_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Diazotrophs expressed as Carbon in sea water")
     vardesc_temp = vardesc("phypicoos_raw", "Surface Mole Concentration of Picophytoplankton expressed as Carbon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phypicoos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phypicoos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_picophytoplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Picophytoplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("phymiscos_raw", "Surface Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phymiscos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phymiscos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_miscellaneous_phytoplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("zmicroos_raw", "Surface Mole Concentration of Microzooplankton expressed as Carbon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_zmicroos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zmicroos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_microzooplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Microzooplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("zmesoos_raw", "Surface Mole Concentration of Mesozooplankton expressed as Carbon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_zmesoos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zmesoos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_mesozooplankton_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Mesozooplankton expressed as Carbon in sea water")
     vardesc_temp = vardesc("talkos_raw", "Surface Total Alkalinity", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_talkos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "talkos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "sea_water_alkalinity_expressed_as_mole_equivalent", &
          cmor_long_name = "Surface Total Alkalinity")
     vardesc_temp = vardesc("talknatos_raw", "Surface Natural Total Alkalinity", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_talknatos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "talknatos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "sea_water_alkalinity_natural_analogue_expressed_as_mole_equivalent", &
          cmor_long_name = "Surface Natural Total Alkalinity")
     vardesc_temp = vardesc("phos_raw", "Surface pH", 'h', '1', 's', '1', 'f')
     cobalt%id_phos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phos", &
          cmor_units = "1", &
          cmor_standard_name = "sea_water_ph_reported_on_total_scale", &
          cmor_long_name = "Surface pH")
     vardesc_temp = vardesc("phnatos_raw", "Surface Natural pH", 'h', '1', 's', '1', 'f')
     cobalt%id_phnatos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phnatos", &
          cmor_units = "1", &
          cmor_standard_name = "sea_water_ph_natural_analogue_reported_on_total_scale", &
          cmor_long_name = "Surface Natural pH")
     vardesc_temp = vardesc("phabioos_raw", "Surface Abiotic pH", 'h', '1', 's', '1', 'f')
     cobalt%id_phabioos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phabioos", &
          cmor_units = "1", &
          cmor_standard_name = "sea_water_ph_abiotic_analogue_reported_on_total_scale", &
          cmor_long_name = "Surface Abiotic pH")
     vardesc_temp = vardesc("o2os_raw", "Surface Dissolved Oxygen Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_o2os = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "o2os", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", &
          cmor_long_name = "Surface Dissolved Oxygen Concentration")
     vardesc_temp = vardesc("o2satos_raw", "Surface Dissolved Oxygen Concentration at Saturation", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_o2satos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "o2satos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water_at_saturation", &
          cmor_long_name = "Surface Dissolved Oxygen Concentration at Saturation")
     vardesc_temp = vardesc("no3os_raw", "Surface Dissolved Nitrate Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_no3os = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "no3os", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_nitrate_in_sea_water", &
          cmor_long_name = "Surface Dissolved Nitrate Concentration")
     vardesc_temp = vardesc("nh4os_raw", "Surface Dissolved Ammonium Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_nh4os = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "nh4os", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_ammonium_in_sea_water", &
          cmor_long_name = "Surface Dissolved Ammonium Concentration")
     vardesc_temp = vardesc("po4os_raw", "Surface Total Dissolved Inorganic Phosphorus Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_po4os = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "po4os", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water", &
          cmor_long_name = "Surface Total Dissolved Inorganic Phosphorus Concentration")
     vardesc_temp = vardesc("dfeos_raw", "Surface Dissolved Iron Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_dfeos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dfeos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_iron_in_sea_water", &
          cmor_long_name = "Surface Dissolved Iron Concentration")
     vardesc_temp = vardesc("sios_raw", "Surface Total Dissolved Inorganic Silicon Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_sios = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "sios", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_inorganic_silicon_in_sea_water", &
          cmor_long_name = "Surface Total Dissolved Inorganic Silicon Concentration")
     vardesc_temp = vardesc("chlos_raw", "Surface Mass Concentration of Total Phytoplankton expressed as Chlorophyll in sea water", 'h', '1', 's', 'kg m-3', 'f')
     cobalt%id_chlos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chlos", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Surface Mass Concentration of Total Phytoplankton expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chldiatos_raw", "Surface Mass Concentration of Diatoms expressed as Chlorophyll in sea water", 'h', '1', 's', 'kg m-3', 'f')
     cobalt%id_chldiatos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chldiatos", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_diatoms_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Surface Mass Concentration of Diatoms expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chldiazos_raw", "Surface Mass Concentration of Diazotrophs expressed as Chlorophyll in sea water", 'h', '1', 's', 'kg m-3', 'f')
     cobalt%id_chldiazos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chldiazos", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_diazotrophs_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Surface Mass Concentration of Diazotrophs expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chlpicoos_raw", "Surface Mass Concentration of Picophytoplankton expressed as Chlorophyll in sea water", 'h', '1', 's', 'kg m-3', 'f')
     cobalt%id_chlpicoos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chlpicoos", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_picophytoplankton_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Surface Mass Concentration of Picophytoplankton expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("chlmiscos_raw", "Surface Mass Concentration of Other Phytoplankton expressed as Chlorophyll in sea water", 'h', '1', 's', 'kg m-3', 'f')
     cobalt%id_chlmiscos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "chlmiscos", &
          cmor_units = "kg m-3", &
          cmor_standard_name = "mass_concentration_of_miscellaneous_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
          cmor_long_name = "Surface Mass Concentration of Other Phytoplankton expressed as Chlorophyll in sea water")
     vardesc_temp = vardesc("ponos_raw", "Surface Mole Concentration of Particulate Organic Matter expressed as Nitrogen in sea water", 'h', '1', 's', 'mol N m-3', 'f')
     cobalt%id_ponos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "ponos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_nitrogen_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Particulate Organic Matter expressed as Nitrogen in sea water")
     vardesc_temp = vardesc("popos_raw", "Surface Mole Concentration of Particulate Organic Matter expressed as Phosphorus in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_popos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "popos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_phosphorus_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Particulate Organic Matter expressed as Phosphorus in sea water")
     vardesc_temp = vardesc("bfeos_raw", "Surface Mole Concentration of Particulate Organic Matter expressed as Iron in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_bfeos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bfeos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_iron_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Particulate Organic Matter expressed as Iron in sea water")
     vardesc_temp = vardesc("bsios_raw", "Surface Mole Concentration of Particulate Organic Matter expressed as Silicon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_bsios = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "bsios", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_particulate_organic_matter_expressed_as_silicon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Particulate Organic Matter expressed as Silicon in sea water")
     vardesc_temp = vardesc("phynos_raw", "Surface Mole Concentration of Phytoplankton Nitrogen in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phynos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phynos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_nitrogen_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Phytoplankton Nitrogen in sea water")
     vardesc_temp = vardesc("phypos_raw", "Surface Mole Concentration of Total Phytoplankton expressed as Phosphorus in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phypos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phypos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_phosphorus_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Total Phytoplankton expressed as Phosphorus in sea water")
     vardesc_temp = vardesc("phyfeos_raw", "Surface Mole Concentration of Total Phytoplankton expressed as Iron in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_phyfeos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "phyfeos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_iron_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Total Phytoplankton expressed as Iron in sea water")
     vardesc_temp = vardesc("physios_raw", "Surface Mole Concentration of Total Phytoplankton expressed as Silicon in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_physios = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "physios", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_phytoplankton_expressed_as_silicon_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Total Phytoplankton expressed as Silicon in sea water")
     vardesc_temp = vardesc("co3os_raw", "Surface Carbonate Ion Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_co3os = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3os", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Carbonate Ion Concentration")
     vardesc_temp = vardesc("co3natos_raw", "Surface Natural Carbonate Ion Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_co3natos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3natos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_ion_natural_analogue_in_sea_water", &
          cmor_long_name = "Surface Natural Carbonate Ion Concentration")
     vardesc_temp = vardesc("co3abioos_raw", "Surface Abiotic Carbonate Ion Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_co3abioos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3abioos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_abiotic_analogue_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Surface Abiotic Carbonate Ion Concentration")
     vardesc_temp = vardesc("co3satcalcos_raw", "Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_co3satcalcos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3satcalcos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_calcite_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in sea water")
     vardesc_temp = vardesc("co3sataragos_raw", "Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in sea water", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_co3sataragos = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "co3sataragos", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_aragonite_in_sea_water", &
          cmor_long_name = "Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in sea water")
     vardesc_temp = vardesc("intpp_raw", "Primary Organic Carbon Production by All Types of Phytoplankton", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpp", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_phytoplankton", &
          cmor_long_name = "Primary Organic Carbon Production by All Types of Phytoplankton")
     vardesc_temp = vardesc("intppnitrate_raw", "Primary Organic Carbon Production by Phytoplankton Based on Nitrate Uptake Alone", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intppnitrate = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intppnitrate", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "net_primary_mole_productivity_of_biomass_expressed_as_carbon_due_to_nitrate_utilization", &
          cmor_long_name = "Primary Organic Carbon Production by Phytoplankton Based on Nitrate Uptake Alone")
     vardesc_temp = vardesc("intppdiat_raw", "Net Primary Organic Carbon Production by Diatoms", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intppdiat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intppdiat", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_diatoms", &
          cmor_long_name = "Net Primary Organic Carbon Production by Diatoms")
     vardesc_temp = vardesc("intppdiaz_raw", "Net Primary Mole Productivity of Carbon by Diazotrophs", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intppdiaz = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intppdiaz", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_diazotrophs", &
          cmor_long_name = "Net Primary Mole Productivity of Carbon by Diazotrophs")
     vardesc_temp = vardesc("intpppico_raw", "Net Primary Mole Productivity of Carbon by Picophytoplankton", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpppico = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpppico", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_picophytoplankton", &
          cmor_long_name = "Net Primary Mole Productivity of Carbon by Picophytoplankton")
     vardesc_temp = vardesc("intppmisc_raw", "Net Primary Organic Carbon Production by Other Phytoplankton", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intppmisc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intppmisc", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_miscellaneous_phytoplankton", &
          cmor_long_name = "Net Primary Organic Carbon Production by Other Phytoplankton")
     vardesc_temp = vardesc("intpbn_raw", "Nitrogen Production", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpbn = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpbn", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_nitrogen_due_to_biological_production", &
          cmor_long_name = "Nitrogen Production")
     vardesc_temp = vardesc("intpbp_raw", "Phosphorus Production", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpbp = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpbp", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_phosphorus_due_to_biological_production", &
          cmor_long_name = "Phosphorus Production")
     vardesc_temp = vardesc("intpbfe_raw", "Iron Production", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpbfe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpbfe", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_iron_due_to_biological_production", &
          cmor_long_name = "Iron Production")
     vardesc_temp = vardesc("intpbsi_raw", "Silicon Production", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpbsi = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpbsi", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_silicon_due_to_biological_production", &
          cmor_long_name = "Silicon Production")
     vardesc_temp = vardesc("intpcalcite_raw", "Calcite Production", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpcalcite = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpcalcite", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_calcite_expressed_as_carbon_due_to_biological_production", &
          cmor_long_name = "Calcite Production")
     vardesc_temp = vardesc("intparag_raw", "Aragonite Production", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intparag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intparag", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_aragonite_expressed_as_carbon_due_to_biological_production", &
          cmor_long_name = "Aragonite Production")
     vardesc_temp = vardesc("epc100_raw", "Downward Flux of Particulate Organic Carbon", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_epc100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "epc100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Downward Flux of Particulate Organic Carbon")
     vardesc_temp = vardesc("epn100_raw", "Downward Flux of Particulate Nitrogen", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_epn100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "epn100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_organic_nitrogen_in_sea_water", &
          cmor_long_name = "Downward Flux of Particulate Nitrogen")
     vardesc_temp = vardesc("epp100_raw", "Downward Flux of Particulate Phosphorus", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_epp100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "epp100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water", &
          cmor_long_name = "Downward Flux of Particulate Phosphorus")
     vardesc_temp = vardesc("epfe100_raw", "Downward Flux of Particulate Iron", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_epfe100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "epfe100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_iron_in_sea_water", &
          cmor_long_name = "Downward Flux of Particulate Iron")
     vardesc_temp = vardesc("epsi100_raw", "Downward Flux of Particulate Silicon", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_epsi100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "epsi100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_particulate_silicon_in_sea_water", &
          cmor_long_name = "Downward Flux of Particulate Silicon")
     vardesc_temp = vardesc("epcalc100_raw", "Downward Flux of Calcite", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_epcalc100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "epcalc100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_calcite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Downward Flux of Calcite")
     vardesc_temp = vardesc("eparag100_raw", "Downward Flux of Aragonite", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_eparag100 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "eparag100", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "sinking_mole_flux_of_aragonite_expressed_as_carbon_in_sea_water", &
          cmor_long_name = "Downward Flux of Aragonite")
     vardesc_temp = vardesc("intdic_raw", "Dissolved Inorganic Carbon Content", 'h', '1', 's', 'kg m-2', 'f')
     cobalt%id_intdic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intdic", &
          cmor_units = "kg m-2", &
          cmor_standard_name = "ocean_mass_content_of_dissolved_inorganic_carbon", &
          cmor_long_name = "Dissolved Inorganic Carbon Content")
     vardesc_temp = vardesc("intdoc_raw", "Dissolved Organic Carbon Content", 'h', '1', 's', 'kg m-2', 'f')
     cobalt%id_intdoc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intdoc", &
          cmor_units = "kg m-2", &
          cmor_standard_name = "ocean_mass_content_of_dissolved_organic_carbon", &
          cmor_long_name = "Dissolved Organic Carbon Content")
     vardesc_temp = vardesc("intpoc_raw", "Particulate Organic Carbon Content", 'h', '1', 's', 'kg m-2', 'f')
     cobalt%id_intpoc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpoc", &
          cmor_units = "kg m-2", &
          cmor_standard_name = "ocean_mass_content_of_particulate_organic_matter_expressed_as_carbon", &
          cmor_long_name = "Particulate Organic Carbon Content")
     vardesc_temp = vardesc("spco2_raw", "Surface Aqueous Partial Pressure of CO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_spco2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "spco2", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_partial_pressure_of_carbon_dioxide_in_sea_water", &
          cmor_long_name = "Surface Aqueous Partial Pressure of CO2")
     vardesc_temp = vardesc("spco2nat_raw", "Natural Surface Aqueous Partial Pressure of CO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_spco2nat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "spco2nat", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_partial_pressure_of_carbon_dioxide_natural_analogue_in_sea_water", &
          cmor_long_name = "Natural Surface Aqueous Partial Pressure of CO2")
     vardesc_temp = vardesc("spco2abio_raw", "Abiotic Surface Aqueous Partial Pressure of CO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_spco2abio = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "spco2abio", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_partial_pressure_of_carbon_dioxide_abiotic_analogue_in_sea_water", &
          cmor_long_name = "Abiotic Surface Aqueous Partial Pressure of CO2")
     vardesc_temp = vardesc("dpco2_raw", "Delta PCO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_dpco2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dpco2", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_carbon_dioxide_partial_pressure_difference_between_sea_water_and_air", &
          cmor_long_name = "Delta PCO2")
     vardesc_temp = vardesc("dpco2nat_raw", "Natural Delta PCO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_dpco2nat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dpco2nat", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_carbon_dioxide_natural_analogue_partial_pressure_difference_between_sea_water_and_air", &
          cmor_long_name = "Natural Delta PCO2")
     vardesc_temp = vardesc("dpco2abio_raw", "Abiotic Delta PCO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_dpco2abio = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dpco2abio", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_carbon_dioxide_abiotic_analogue_partial_pressure_difference_between_sea_water_and_air", &
          cmor_long_name = "Abiotic Delta PCO2")
     vardesc_temp = vardesc("dpo2_raw", "Delta PO2", 'h', '1', 's', 'Pa', 'f')
     cobalt%id_dpo2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "dpo2", &
          cmor_units = "Pa", &
          cmor_standard_name = "surface_molecular_oxygen_partial_pressure_difference_between_sea_water_and_air", &
          cmor_long_name = "Delta PO2")
     vardesc_temp = vardesc("fgco2_raw", "Surface Downward Flux of Total CO2", 'h', '1', 's', 'kg m-2 s-1', 'f')
     cobalt%id_fgco2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fgco2", &
          cmor_units = "kg m-2 s-1", &
          cmor_standard_name = "surface_downward_mass_flux_of_carbon_dioxide_expressed_as_carbon", &
          cmor_long_name = "Surface Downward Flux of Total CO2")
     vardesc_temp = vardesc("fgco2nat_raw", "Surface Downward Flux of Natural CO2", 'h', '1', 's', 'kg m-2 s-1', 'f')
     cobalt%id_fgco2nat = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fgco2nat", &
          cmor_units = "kg m-2 s-1", &
          cmor_standard_name = "surface_downward_mass_flux_of_carbon_dioxide_natural_analogue_expressed_as_carbon", &
          cmor_long_name = "Surface Downward Flux of Natural CO2")
     vardesc_temp = vardesc("fgco2abio_raw", "Surface Downward Flux of Abiotic CO2", 'h', '1', 's', 'kg m-2 s-1', 'f')
     cobalt%id_fgco2abio = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fgco2abio", &
          cmor_units = "kg m-2 s-1", &
          cmor_standard_name = "surface_downward_mass_flux_of_carbon_dioxide_abiotic_analogue_expressed_as_carbon", &
          cmor_long_name = "Surface Downward Flux of Abiotic CO2")
     vardesc_temp = vardesc("fg14co2abio_raw", "Surface Downward Flux of Abiotic 14CO2", 'h', '1', 's', 'kg m-2 s-1', 'f')
     cobalt%id_fg14co2abio = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fg14co2abio", &
          cmor_units = "kg m-2 s-1", &
          cmor_standard_name = "surface_downward_mass_flux_of_carbon14_dioxide_abiotic_analogue_expressed_as_carbon", &
          cmor_long_name = "Surface Downward Flux of Abiotic 14CO2")
     vardesc_temp = vardesc("fgo2_raw", "Surface Downward Flux of O2", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fgo2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fgo2", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "surface_downward_mole_flux_of_molecular_oxygen", &
          cmor_long_name = "Surface Downward Flux of O2")
     vardesc_temp = vardesc("icfriver_raw", "Flux of Inorganic Carbon Into Ocean Surface by Runoff", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_icfriver = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "icfriver", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_inorganic_carbon_due_to_runoff_and_sediment_dissolution", &
          cmor_long_name = "Flux of Inorganic Carbon Into Ocean Surface by Runoff")
     vardesc_temp = vardesc("fric_raw", "Downward Inorganic Carbon Flux at Ocean Bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fric = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fric", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_inorganic_carbon_due_to_sedimentation", &
          cmor_long_name = "Downward Inorganic Carbon Flux at Ocean Bottom")
     vardesc_temp = vardesc("ocfriver_raw", "Flux of Organic Carbon Into Ocean Surface by Runoff", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_ocfriver = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "ocfriver", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_organic_carbon_due_to_runoff_and_sediment_dissolution", &
          cmor_long_name = "Flux of Organic Carbon Into Ocean Surface by Runoff")
     vardesc_temp = vardesc("froc_raw", "Downward Organic Carbon Flux at Ocean Bottom", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_froc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "froc", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_organic_carbon_due_to_sedimentation", &
          cmor_long_name = "Downward Organic Carbon Flux at Ocean Bottom")
     vardesc_temp = vardesc("intpn2_raw", "Nitrogen Fixation Rate in Ocean", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_intpn2 = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "intpn2", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_elemental_nitrogen_due_to_fixation", &
          cmor_long_name = "Nitrogen Fixation Rate in Ocean")
     vardesc_temp = vardesc("fsn_raw", "Surface Downward Net Flux of Nitrogen", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsn = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fsn", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_elemental_nitrogen_due_to_deposition_and_fixation_and_runoff", &
          cmor_long_name = "Surface Downward Net Flux of Nitrogen")
     vardesc_temp = vardesc("frn_raw", "Nitrogen Loss to Sediments and through Denitrification", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_frn = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "frn", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_elemental_nitrogen_due_to_denitrification_and_sedimentation", &
          cmor_long_name = "Nitrogen Loss to Sediments and through Denitrification")
     vardesc_temp = vardesc("fsfe_raw", "Surface Downward Net Flux of Iron", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fsfe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fsfe", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_iron_due_to_deposition_and_runoff_and_sediment_dissolution", &
          cmor_long_name = "Surface Downward Net Flux of Iron")
     vardesc_temp = vardesc("frfe_raw", "Iron Loss to Sediments", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_frfe = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "frfe", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_iron_due_to_sedimentation", &
          cmor_long_name = "Iron Loss to Sediments")
     vardesc_temp = vardesc("o2min_raw", "Oxygen Minimum Concentration", 'h', '1', 's', 'mol m-3', 'f')
     cobalt%id_o2min = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "o2min", &
          cmor_units = "mol m-3", &
          cmor_standard_name = "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water_at_shallowest_local_minimum_in_vertical_profile", &
          cmor_long_name = "Oxygen Minimum Concentration")
     vardesc_temp = vardesc("zo2min_raw", "Depth of Oxygen Minimum Concentration", 'h', '1', 's', 'm', 'f')
     cobalt%id_zo2min = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zo2min", &
          cmor_units = "m", &
          cmor_standard_name = "depth_at_shallowest_local_minimum_in_vertical_profile_of_mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", &
          cmor_long_name = "Depth of Oxygen Minimum Concentration")
     vardesc_temp = vardesc("zsatcalc_raw", "Calcite Saturation Depth", 'h', '1', 's', 'm', 'f')
     cobalt%id_zsatcalc = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zsatcalc", &
          cmor_units = "m", &
          cmor_standard_name = "minimum_depth_of_calcite_undersaturation_in_sea_water", &
          cmor_long_name = "Calcite Saturation Depth")
     vardesc_temp = vardesc("zsatarag_raw", "Aragonite Saturation Depth", 'h', '1', 's', 'm', 'f')
     cobalt%id_zsatarag = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "zsatarag", &
          cmor_units = "m", &
          cmor_standard_name = "minimum_depth_of_aragonite_undersaturation_in_sea_water", &
          cmor_long_name = "Aragonite Saturation Depth")
     vardesc_temp = vardesc("fddtdic_raw", "Rate of Change of Net Dissolved Inorganic Carbon", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fddtdic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fddtdic", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_carbon", &
          cmor_long_name = "Rate of Change of Net Dissolved Inorganic Carbon")
     vardesc_temp = vardesc("fddtdin_raw", "Rate of Change of Net Dissolved Inorganic Nitrogen", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fddtdin = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fddtdin", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_nitrogen", &
          cmor_long_name = "Rate of Change of Net Dissolved Inorganic Nitrogen")
     vardesc_temp = vardesc("fddtdip_raw", "Rate of Change of Net Dissolved Inorganic Phosphorus", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fddtdip = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fddtdip", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_phosphorus", &
          cmor_long_name = "Rate of Change of Net Dissolved Inorganic Phosphorus")
     vardesc_temp = vardesc("fddtdife_raw", "Rate of Change of Net Dissolved Inorganic Iron", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fddtdife = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fddtdife", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_iron", &
          cmor_long_name = "Rate of Change of Net Dissolved Inorganic Iron")
     vardesc_temp = vardesc("fddtdisi_raw", "Rate of Change of Net Dissolved Inorganic Silicon", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fddtdisi = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fddtdisi", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_silicon", &
          cmor_long_name = "Rate of Change of Net Dissolved Inorganic Silicon")
     vardesc_temp = vardesc("fddtalk_raw", "Rate of Change of Total Alkalinity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fddtalk = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fddtalk", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "integral_wrt_depth_of_tendency_of_sea_water_alkalinity_expressed_as_mole_equivalent", &
          cmor_long_name = "Rate of Change of Total Alkalinity")
     vardesc_temp = vardesc("fbddtdic_raw", "Rate of Change of Dissolved Inorganic Carbon due to Biological Activity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fbddtdic = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fbddtdic", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_carbon_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Carbon due to Biological Activity")
     vardesc_temp = vardesc("fbddtdin_raw", "Rate of Change of Dissolved Inorganic Nitrogen due to Biological Activity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fbddtdin = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fbddtdin", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_nitrogen_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Nitrogen due to Biological Activity")
     vardesc_temp = vardesc("fbddtdip_raw", "Rate of Change of Dissolved Inorganic Phosphorus due to Biological Activity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fbddtdip = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fbddtdip", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_phosphorus_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Phosphorus due to Biological Activity")
     vardesc_temp = vardesc("fbddtdife_raw", "Rate of Change of Dissolved Inorganic Iron due to Biological Activity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fbddtdife = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fbddtdife", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_iron_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Iron due to Biological Activity")
     vardesc_temp = vardesc("fbddtdisi_raw", "Rate of Change of Dissolved Inorganic Silicon due to Biological Activity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fbddtdisi = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fbddtdisi", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "tendency_of_ocean_mole_content_of_dissolved_inorganic_silicon_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Dissolved Inorganic Silicon due to Biological Activity")
     vardesc_temp = vardesc("fbddtalk_raw", "Rate of Change of Biological Alkalinity due to Biological Activity", 'h', '1', 's', 'mol m-2 s-1', 'f')
     cobalt%id_fbddtalk = register_diag_field(package_name, vardesc_temp%name, axes(1:2), &
          init_time, vardesc_temp%longname, vardesc_temp%units, &
          missing_value = missing_value1, &
          cmor_field_name = "fbddtalk", &
          cmor_units = "mol m-2 s-1", &
          cmor_standard_name = "integral_wrt_depth_of_tendency_of_sea_water_alkalinity_expressed_as_mole_equivalent_due_to_biological_processes", &
          cmor_long_name = "Rate of Change of Biological Alkalinity due to Biological Activity")
