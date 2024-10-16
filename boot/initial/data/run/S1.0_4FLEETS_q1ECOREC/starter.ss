data.SS #_datfile
control.SS #_ctlfile
0 # 0=use init values in control file; 1=use ss.par
1 # run display detail (0,1,2)
1 # detailed output (0=minimal for data-limited, 1=high (w/ wtatage.ss_new), 2=brief, 3=custom) 
# custom report options: -100 to start with minimal; -101 to start with all; -number to remove, +number to add, -999 to end
0 # write 1st iteration details to echoinput.sso file (0,1) 
0 # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)
1 # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)
1 # Include prior_like for non-estimated parameters (0,1) 
1 # Use Soft Boundaries to aid convergence (0,1) (recommended)
#
3 # Number of datafiles to produce:  0 turns off all *.ss_new; 1st is data_echo.ss_new, 2nd is data_expval.ss, 3rd and higher are data_boot_**N.ss,
10 # Turn off estimation for parameters entering after this phase
#
10 # MCeval burn interval
2 # MCeval thin interval
0 # jitter initial parm value by this fraction
-1 # min yr for sdreport outputs (-1 for styr);
-1 # max yr for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs); 
0 # N individual STD years 
#vector of year values 

0.0001 # final convergence criteria (e.g. 1.0e-04) 
0 # retrospective year relative to end year (e.g. -4)
0 # min age for calc of summary biomass
3 # Depletion basis:  denom is: 0=skip; 1=rel X*SPBvirgin; 2=rel SPBmsy; 3=rel X*SPB_styr; 4=rel X*SPB_endyr; values; >=11 invoke N multiyr (up to 9!) with 10's digit; >100 invokes log(ratio)
1 # Fraction (X) for Depletion denominator (e.g. 0.4)
4 # SPR_report_basis:  0=skip; 1=(1-SPR)/(1-SPR_tgt); 2=(1-SPR)/(1-SPR_MSY); 3=(1-SPR)/(1-SPR_Btarget); 4=rawSPR
3 # F_reporting_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Apical_F's); 4=true F for range of ages; 5=unweighted avg. F for range of ages
 #COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4 or 5
0 # F_std_basis: 0=raw_annual_F; 1=F/Fspr; 2=F/Fmsy; 3=F/Fbtgt; where F means annual_F; values >=11 invoke N multiyr (up to 9!) with 10's digit; >100 invokes log(ratio)
0 # MCMC output detail: integer part (0=default; 1=adds obj func components; 2= +write_report_for_each_mceval); and decimal part (added to SR_LN(R0) on first call to mcmc)
0 # ALK tolerance ***disabled in code (example 0.0001)
-1 # random number seed for bootstrap data (-1 to use long(time) as seed): # 1669975898
3.30 # check value for end of file and for version control