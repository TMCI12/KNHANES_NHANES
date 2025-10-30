libname b "\\KHU_Server2\\KDH\\Data\\KNHANES"; 

data allyear; 
    set b.hn14_all b.hn16_all b.hn18_all b.hn20_all; 
run;

data wt;
    set allyear;
    if year in (2014, 2016, 2018, 2020) then wt_total = wt_itvex * (192/768);
run;

data age;
    set wt;
    if 20 <= age < 44 then age_g = 1;
    else if 45 <= age < 64 then age_g = 2;
    else if age >= 65 then age_g = 3;
    else age_g = .;
run;

data incm;
    set age;
    if ho_incm5 in (2,3,4,5) then incm_g = 1;
    else if ho_incm5 = 1 then incm_g = 2;
    else ho_incm5 = .;
run;

data educ;
    set incm;
    if educ in (7, 8, 9) then educ_g = 2;
    else if educ in (1, 2, 3, 4, 5, 6) then educ_g = 1;
    else educ_g = .; 
run;

data smk;
    set educ;
	if BS1_1 = 1 or BS1_1 = 3 then smk = 3;
	else if BS1_1 = 2 and BS3_1 = 3 then smk = 2;
	else if BS1_1 = 2 and BS3_1 in (1 2) then smk = 1;
	else smoking = .;
run;

data drink;
    set smk;
    if BD1 = 1 then alcohol = 3;
    else if BD1 = 2 and BD1_11 = 1 then alcohol = 2;
    else if BD1 = 2 and BD1_11 in (2 3 4 5 6) then alcohol = 1;
    else alcohol = .;
run;

data BMI;
    set drink;
    if HE_wt^=. & HE_ht^=. then BMI = HE_wt / ((HE_ht*0.01)**2);
    if BMI < 18.5 then BMI_g = 1;
    else if 18.5<=BMI<25 then BMI_g = 2;
    else if 25<=BMI<30 then BMI_g = 3;
    else if BMI>=30 then BMI_g = 4;
    else BMI = .;
run;

data phq;
    set BMI;
    array phq[9] BP_PHQ_1 - BP_PHQ_9;
    do i = 1 to 9;
        if phq[i] in (8, 9) then phq[i] = .;
    end;
    PHQ_score = sum(of phq[*]);
run;

data phq_group;
    set phq;
    if PHQ_score = 0 then PHQ_g = 0; 
    else if 1 <= PHQ_score <= 4 then PHQ_g = 1; 
    else if 5 <= PHQ_score <= 9 then PHQ_g = 2; 
    else if 10 <= PHQ_score <= 14 then PHQ_g = 3;
    else if 15 <= PHQ_score <= 27 then PHQ_g = 4; 
    else PHQ_g = .;
run;

proc freq data=phq_group; table PHQ_g; run;
proc freq data=phq_group; table DM3_dg; run;

data knh;
    set phq_group(keep=id sex year wt_total kstrata psu age_g incm_g educ_g smk alcohol PHQ_g PHQ_score BMI_g N_PUFA N_N3 N_N6 N_EN);
run;

data knh_filter;
    set knh;
    if age_g = . then delete;
run;

data knh_filter;
    set knh_filter;
    if PHQ_g = . then delete;
run;

data knh_final;
    set knh_filter;
    if cmiss(of _all_) = 0;
run;

data merged;
    set c.hn14_24RC c.hn16_24RC c.hn18_24RC c.hn20_24RC;
run;

proc summary data=merged nway;
    class ID;
    var NF_PUFA NF_N3 NF_N6 NF_EN NF_18_3N3 NF_18_4 NF_20_5N3 NF_22_5 NF_22_6N3 NF_18_2N6 NF_20_4N6;
    output out=sum_pufa(drop=_type_ _freq_)
        sum=total_pufa_mean n3_mean n6_mean energy_mean ALA_mean SDA_mean EPA_mean DPA_mean DHA_mean LA_mean AA_mean;
run;

data pufa_check;
    set sum_pufa;
    pufa_kcal = total_pufa_mean * 9;
    n3_kcal = n3_mean * 9;
    n6_kcal = n6_mean * 9;
    pufa_pct = (pufa_kcal / energy_mean) * 100;
    n3_pct   = (n3_kcal / energy_mean) * 100;
    n6_pct   = (n6_kcal / energy_mean) * 100;
run;

proc rank data=pufa_check out=pufa_check groups=4;
    var total_pufa_mean n3_mean n6_mean;
    ranks PUFA_q N3_q N6_q;
run;

proc sort data=pufa_check; by ID; run;
proc sort data=knh_final; by ID; run;

data knh_final_re;
    merge knh_final(in=a) pufa_check;
    by ID;
    if a;
run;

data knh_depression;
    set knh_final_re;
    if PHQ_g in (0 1 2) then depression = 0;
    else if PHQ_g in (3 4) then depression = 1;
run;

proc freq data=knh_depression; table n3_q; run;

ods select OddsRatios ParameterEstimates;
proc surveylogistic data=knh_depression;
  strata kstrata;
  cluster psu;
  weight wt_total;
  class n3_rec(ref='2') age_g incm_g educ_g smk alcohol BMI_g / param=ref;
  model depression(ref='1') = n3_rec age_g incm_g educ_g smk alcohol BMI_g;
run;

ods select OddsRatios ParameterEstimates;
proc surveylogistic data=knh_depression;
  strata kstrata;
  cluster psu;
  weight wt_total;
  class pufa_rec(ref='1') age_g incm_g educ_g smk alcohol BMI_g / param=ref;
  model depression(ref='1') = pufa_rec age_g incm_g educ_g smk alcohol BMI_g;
run;

ods select OddsRatios ParameterEstimates;
proc surveylogistic data=knh_depression;
  strata kstrata;
  cluster psu;
  weight wt_total;
  class pufa_rec(ref='1') age_g incm_g educ_g smk alcohol BMI_g / param=ref;
  model depression(ref='1') = N3_q age_g incm_g educ_g smk alcohol BMI_g;
run;

data knh_final; set knh_depression; run;

proc sort data=DR1tot_h; by SEQN; run;
proc sort data=DR2tot_h; by SEQN; run;
proc sort data=DR1tot_i; by SEQN; run;
proc sort data=DR2tot_i; by SEQN; run;
proc sort data=P_DR1tot; by SEQN; run;
proc sort data=P_DR2tot; by SEQN; run;

data total_merged;
    merge DR1tot_h DR2tot_h DR1tot_i DR2tot_i P_DR1tot P_DR2tot;
    by SEQN;
run;

data n3_n6_avg;
    set total_merged;
    n3_dr1_sum = sum(of DR1TP183 DR1TP184 DR1TP205 DR1TP225 DR1TP226);
    n3_dr2_sum = sum(of DR2TP183 DR2TP184 DR2TP205 DR2TP225 DR2TP226);
    n3_mean = mean(n3_dr1_sum, n3_dr2_sum);
    n6_dr1_sum = sum(of DR1TP182 DR1TP204);
    n6_dr2_sum = sum(of DR2TP182 DR2TP204);
    n6_mean = mean(n6_dr1_sum, n6_dr2_sum);
    energy_mean = mean(DR1TKCAL, DR2TKCAL);
    total_pufa_mean = mean(DR1TPFAT, DR2TPFAT);
run;

proc export data=knh_final
    outfile="knh.xlsx"
    dbms=xlsx
    replace;
run;

data nh; set nh_final; run;
data nh_pufa_subset;
    set drtot(keep=SEQN ALA_ALL EPA_ALL DPA_ALL DHA_ALL LA_ALL AA_ALL);
run;

proc sort data=nh; by SEQN; run;
proc sort data=nh_pufa_subset; by SEQN; run;

data nh_merged;
    merge nh(in=a) nh_pufa_subset;
    by SEQN;
    if a;
run;

data nh_final; set nh_merged; run;

proc export data=nh_final
    outfile="nh.xlsx"
    dbms=xlsx
    replace;
run;

data nh_final; set nh_final; run;

libname a "\\KHU_Server2\\KDH\\Data\\NHANES";

data nh; 
    set a.nh13_20; 
run;

data age;
    set nh;
    if 20 <= RIDAGEYR < 44 then age_g = 1;
    else if 45 <= RIDAGEYR < 64 then age_g = 2;
    else if RIDAGEYR >= 65 then age_g = 3;
    else age_g = .;
run;

data incm;
    set age;
    if INDFMPIR = 5 then incm_g = 1;
    else if 0 <= INDFMPIR < 5 then incm_g = 0;
    else incm_g = .;
run;

data educ;
    set incm;
    if DMDEDUC2 in (4,5) then educ_g = 2;
    else if DMDEDUC2 in (1,2,3) then educ_g = 1;
    else educ_g = .;
run;

data smk;
    set educ;
    if SMQ040 in (1,2) then smk = 1;
    else if SMQ040 = 3 and SMQ020 = 1 then smk = 2;
    else if SMQ020 = 2 then smk = 3;
    else smk = .;
run;

data drink;
    set smk;
    if ALQ101 = 1 and ALQ120Q >= 1 and ALQ120Q <= 365 then drink_g = 1;
    else if ALQ110 = 1 and ALQ101 = 2 then drink_g = 2;
    else if ALQ110 = 2 then drink_g = 3;
    else drink_g = .;
run;

data phq;
    set drink;
    array phq_items[9] DPQ010 DPQ020 DPQ030 DPQ040 DPQ050 DPQ060 DPQ070 DPQ080 DPQ090;
    do i = 1 to 9;
        if phq_items[i] in (7,9) then phq_items[i] = .;
    end;
    PHQ_score = sum(of DPQ010--DPQ090);
run;

data phq_group;
    set phq;
    if PHQ_score = 0 then PHQ_g = "0";
    else if 1 <= PHQ_score <= 4 then PHQ_g = "1";
    else if 5 <= PHQ_score <= 9 then PHQ_g = "2";
    else if 10 <= PHQ_score <= 14 then PHQ_g = "3";
    else if 15 <= PHQ_score <= 27 then PHQ_g = "4";
    else PHQ_g = "";
run;

data BMI;
    set phq_group;
    BMI = BMXBMI;
    if BMI < 18.5 then BMI_g = 1;
    else if 18.5 <= BMI < 25 then BMI_g = 2;
    else if 25 <= BMI < 30 then BMI_g = 3;
    else if BMI >= 30 then BMI_g = 4;
    else BMI_g = .;
run;

data nh;
    set BMI;
    if missing(WTMEC2YR) then WTMEC2YR = WTMECPRP / 2;
    weight = WTMEC2YR / 3;
run;

proc sort data=DR1tot_h; by SEQN; run;
proc sort data=DR2tot_h; by SEQN; run;
proc sort data=DR1tot_i; by SEQN; run;
proc sort data=DR2tot_i; by SEQN; run;
proc sort data=P_DR1tot; by SEQN; run;
proc sort data=P_DR2tot; by SEQN; run;

data total_merged;
    merge DR1tot_h DR2tot_h DR1tot_i DR2tot_i P_DR1tot P_DR2tot;
    by SEQN;
run;

data n3_n6_avg;
    set total_merged;
    ALA_mean = mean(DR1TP183, DR2TP183);
    SDA_mean = mean(DR1TP184, DR2TP184);
    EPA_mean = mean(DR1TP205, DR2TP205);
    DPA_mean = mean(DR1TP225, DR2TP225);
    DHA_mean = mean(DR1TP226, DR2TP226);
    n3_dr1_sum = sum(of DR1TP183 DR1TP184 DR1TP205 DR1TP225 DR1TP226);
    n3_dr2_sum = sum(of DR2TP183 DR2TP184 DR2TP205 DR2TP225 DR2TP226);
    n3_mean = mean(n3_dr1_sum, n3_dr2_sum);
    LA_mean = mean(DR1TP182, DR2TP182);
    AA_mean = mean(DR1TP204, DR2TP204);
    n6_dr1_sum = sum(of DR1TP182 DR1TP204);
    n6_dr2_sum = sum(of DR2TP182 DR2TP204);
    n6_mean = mean(n6_dr1_sum, n6_dr2_sum);
    energy_mean = mean(DR1TKCAL, DR2TKCAL);
    total_pufa_mean = mean(DR1TPFAT, DR2TPFAT);
run;

data PUFA;
    set n3_n6_avg(keep=SEQN ALA_mean SDA_mean EPA_mean DPA_mean DHA_mean LA_mean AA_mean n3_mean n6_mean energy_mean total_pufa_mean DR1TPFAT DR2TPFAT);
run;

data nh;
    set nh(keep=SEQN RIAGENDR SDMVSTRA SDMVPSU weight RIDAGEYR age_g incm_g educ_g smk drink_g PHQ_score PHQ_g BMI BMI_g);
run;

proc sort data=pufa; by SEQN; run;
proc sort data=nh; by SEQN; run;

data merged_data;
    merge pufa(in=a) nh(in=b);
    by SEQN;
    if a and b;
run;

data nh_filter;
    set merged_data;
    if age_g = . then delete;
run;

data nh_filter;
    set nh_filter;
    if PHQ_score = . then delete;
run;

data nh_final;
    set nh_filter;
    if cmiss(of _all_) = 0;
run;

proc rank data=nh_final out=nh_rank groups=4;
    var total_pufa_mean n3_mean n6_mean;
    ranks PUFA_q N3_q N6_q;
run;

data pufa_check;
    set nh_rank;
    pufa_kcal = total_pufa_mean * 9;
    n3_kcal = n3_mean * 9;
    n6_kcal = n6_mean * 9;
    pufa_pct = (pufa_kcal / energy_mean) * 100;
    n3_pct = (n3_kcal / energy_mean) * 100;
    n6_pct = (n6_kcal / energy_mean) * 100;
run;

data nh_depression;
    set pufa_check;
    if PHQ_g in (0 1 2) then depression = 0;
    else if PHQ_g in (3 4) then depression = 1;
run;

ods select OddsRatios ParameterEstimates;
proc surveylogistic data=nh_depression;
    strata SDMVSTRA;
    cluster SDMVPSU;
    weight weight;
    class PUFA_q(ref='0') age_g incm_g educ_g smk drink_g BMI_g / param=ref;
    model depression(ref='1') = PUFA_q age_g incm_g educ_g smk drink_g BMI_g;
run;

ods select OddsRatios ParameterEstimates;
proc surveylogistic data=nh_depression;
    strata SDMVSTRA;
    cluster SDMVPSU;
    weight weight;
    class N3_q(ref='0') age_g incm_g educ_g smk drink_g BMI_g / param=ref;
    model depression(ref='1') = N3_q age_g incm_g educ_g smk drink_g BMI_g;
run;

ods select OddsRatios ParameterEstimates;
proc surveylogistic data=nh_depression;
    strata SDMVSTRA;
    cluster SDMVPSU;
    weight weight;
    class N6_q(ref='0') age_g incm_g educ_g smk drink_g BMI_g / param=ref;
    model depression(ref='1') = N6_q age_g incm_g educ_g smk drink_g BMI_g;
run;
