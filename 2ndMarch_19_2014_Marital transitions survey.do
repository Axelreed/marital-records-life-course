
**********************
if regexm(c(os),"Mac") == 1 {
	global dir `"/Users/Alex/Google Drive/MA Thesis"' 
	}
	else if regexm(c(os),"Windows") == 1 global dir `"C:/Users/areda/Google Drive/MA Thesis"' 
cd "$dir"
capture log close
log using transitions_paper.txt, replace
global user "Alex Ayalu Reda"
display "Analysis run by $user at `c(current_time)' `c(current_date)'"
version 11.0
//Ayalu Alex Reda

*Next I will generate all the variables needed for the new analysis.*
clear all
clear matrix
set memory 3000m
set matsize 2000
set maxvar 10000
set more off

*then access the data from current directory*
use march_dhs_merge, clear

gen psu=v021
gen weight=(v005/1000000)
gen wave=source
gen year=wave
capture save contracept_merge, replace
egen psuXyear=group(psu year)
gen strata=v022
egen waveXstrata=group(wave strata)

*labeling year
label define jaar 2000 "2000" 2005 "2005" 2011 "2011"
label values year jaar


svyset psuXyear [pweight=weight]
svyset [pweight=weight]

*Since I am interested in the younger group, I am generating a variable to indicate the youth part of the data
gen young=(v013==2|v013==3) if v013<.
tab young v013

*to also be able to identify between the two young groups I generate another variable
gen youngcat = .
replace youngcat = 0 if v013==2 //this is age 20-24
replace youngcat = 1 if v013==3 //this is age 25-29

*I label the categories
label define ages 0 "20-24" 1 "25-29"
label values youngcat ages
label variable youngcat "young groups by age range"

******Socio-economic variables

*Religion is constructed

gen religion=v130
recode religion (1/3=0)(4=1)(5/9=2)(96/99=.)
label define religion 0 "christian" 1 "moslem" 2 "other and traditional"
label values religion religion

*Residence
codebook v025
gen reside = v025
recode reside 1=0 2=1
label define resid 0 "urban" 1 "rural"
label variable reside resid

*Ethnicity. 
*Note that the 2011 is using a different coding from the onses used in 2000 and 2005*

gen ethnicity = v131
recode ethnicity (1=1) (4=4) (25=25) (60=60) (67 = 67) (68=68) (70=70) (72=72) (2/3=0) (5/24=0) (28/59=0) (61/69=0) (71=0) (73/99=0) (999=.) if source!=2011
recode ethnicity (1=1) (5=5) (35=35) (66=66) (74=74) (76=76) (79=79) (83=83) (2/4=0) (6/34=0) (36/65=0) (67/73=0) (75=0) (77/78=0) (80/82=0) (84/98=0) (999=.) if source==2011
recode ethnicity (1=1) (5=4) (35=25) (66=60) (74=67) (76=68) (79=70) (83=72) if source==2011
label define ethnic2 0 "others" 1 "afar" 4 "Amhara" 25 "guragie" 60 "oromo" 67 "sidama" 68 "Somali" 70 "tigray" 72 "welaita" 
label values ethnicity ethnic2
tab ethnicity

*Wealth index

gen wealth = v190

*Age

gen age = v012

*Education. 
*I created a new variable for education and collapsed values of secondary and higher since 
*the cells became sparce when I cross tabulated them with other variables

gen education = v106
recode education 2/3 = 2
label define educ 0 "no education" 1 "primary education" 2 "secondary or higher education"
label values education educ

*Region
*I am coding Amhara region to 0 so that it becomes the reference in the multivariate analysis
*I have also recoded harar into diredawa
codebook v024, tab(20)
gen region = v024
recode region 14=0 //Making Addis Ababa the reference
*Working on contraceptive use
*Working on the outcome variables: v312 - current contraceptive method; and v313 - current contraceptive use by method type
*V312 and V313 are basically the same variables except that v313 merges the categories of v312. 
*In v313 - 'periodic abstinence' and 'withdrawal' categores in v312 are classified into the category of 'traditional methods'.
*In v313 - the 'other' category are entered as 'folkloric' methods.
*in v313 - 'modern method' is compiled from the categories of 'standard days method',
*, 'female condom', 'norplant', 'lactational amenorrhea', 'female sterilization',
*'condom', 'injections', 'iud', and 'pill'. 
 
tab v313 v312
*Here i am generating rate of any contraceptive use including modern, folkloric and traditional methods.

codebook v313
gen anyuse=v313
recode anyuse 0 = 0 1/3 = 1

*Next since the traditional and folkloric methods of contracetion are very small I will convert them into non-use and contstruct
*a new variable for current modern contraceptive method use.

gen modern = (v313==3) if v313<.
label variable modern "modern contraceptive use"
label define use1 1 "modern use" 0 "non use"
label values modern use1

*Knowledge of contraceptive methods.

*The variable v301 has responses about knowledge of contraception by respondents
*Knowledge is recorded as 'knows no method', 'knows only folkloric method', knows only trad. meth', and 'knows modern method'.
*When I quietly tabulated the responses, folklorik method and traditional method are very minor, 134 people out of 45,000 responded as such. Due to this I removed it.
*I therefore recoded folklorik and traditional methods into knows no method.

codebook v301
gen know = v301
recode know 1/2=0 3=1
label variable know "knowledge of modern contraceptive method"
label define know 0 "does not know method" 1 "knows modern method"
label values know know

***************************************
**Constructing outcome variables

*I - Sexual intercourse and age

*EVER had SEX

gen eversex=.
replace eversex = 0 if v525==0
recode v525 (7/10=10)
replace eversex = 1 if v525 >= 10 & v525 <=96
label define sex 0 "no, ever intercourse" 1 "yes, ever intercourse" 
label values eversex sex

*Multiple outcome format for sexual intercourse
*the categories are never had sex, premarital sex, marital sex

gen eversex2 = .
replace eversex2 = 0 if v525==0
recode v525 (7/10=10)
replace eversex2 = 1 if v525 >=7 & v525<96  //premarital sex
replace eversex2 = 2 if v525==96
recode eversex2 1=2 if v511<=v525 & v525 >=10 & v525<96
tab eversex2
*Note that the age at first sex below applies to both.

*Age at first sex - this is done into two and three categories

*I generate the values from the age at ever sex, and the age at marriage for those
*Who had sex at first marriage
gen ageatfs = .
replace ageatfs = v525 if v525 >= 7 & v525 <96
replace ageatfs=v511 if v525==96
replace ageatfs=v012 if v525==0
recode ageatfs (1/10=10) (35/49=35)

replace ageatfs=. if v531==97 //removing inconsistent responses flagged by the DHS

*II - Marriage and age

*EVER MARRIAGE

*So next I focus my analysis on the ever married sample
*Now I generate ever married women.
codebook v502
gen evermar=v502
recode evermar 0=0 1=1 2=1  //2 is formerly married
label variable evermar "ever married"
label define status 0 "Not ever in union" 1 "yes, ever union"
label values evermar status

*currently married
gen cumar=v502
recode cumar 0=0 2=0 1=1
label variable cumar "current marrital status"
label define curstatus 0 "Not in union" 1 "In union"
label values cumar curstatus


*Age at first marriage
*For those not married yet their age is replaced with their current age

gen ageatfm=v511
replace ageatfm=v012 if v511==. //those not married are assigned their current age
recode ageatfm (min/10=10)
recode ageatfm (35/49=35)
sum v511, det

*II - Birth

*Everbirth

gen everbirth = .
replace everbirth = 1 if v212 >= 8 & v212 <=42
replace everbirth = 0 if v212 == .
label define varlab  0 "never had a child" 1 "ever had a child"
label values everbirth varlab

*Age at first birth

gen ageatfb = .
replace ageatfb = v212
replace ageatfb = v012 if ageatfb==.

*************************************************************
*Restricting sample next
*************************************************************

mark nomiss
markout nomiss young reside educ religion source ethnicity eversex eversex2 evermar everbirth ageatfs cumar ageatfm ageatfb		
tab nomiss
tab nomiss source
*Checking missing observation profile.
misschk modern young religion ethnicity source young reside educ religion source ethnicity region ageatfs eversex evermar everbirth

misschk young reside educ religion source ethnicity region age eversex evermar ageatfs cumar, extmiss

foreach val in young reside educ religion source ethnicity eversex evermar everbirth ageatfs cumar {
drop if `val' == .
}

gen anasample=0
replace anasample = 1 if nomiss==1&young==1		
		
*Constructing descriprive table

***Tabulation to create table of descriptive statistics - SOCIOECONOMIC.***

svyset psuXyear [pweight=weight]

*Note that evermar indicates ever in union young women.

svy, subpop(anasample): tab v013 source, col
svy, subpop(anasample): tab educ source, col
svy, subpop(anasample): tab v025 source, col
svy, subpop(anasample): tab religion source, col
svy, subpop(anasample): tab ethnicity source, col
svy, subpop(anasample): tab reside source, col

*Contraceptive use by year and current marital status
svy, subpop(anasample): tab modern cumar if source==2000, col
svy, subpop(anasample): tab modern cumar if source==2005, col
svy, subpop(anasample): tab modern cumar if source==2011, col

*Contraceptive use among never married but with premarital sex
svy, subpop(anasample): tab modern if source==2000&evermar==0&eversex2==1, col
svy, subpop(anasample): tab modern if source==2005&evermar==0&eversex2==1, col
svy, subpop(anasample): tab modern if source==2011&evermar==0&eversex2==1, col

*premarital sex
svy, subpop(anasample): tab eversex2 source if anasample, col

*****************************
******************************
*Survival analysis

*Age at first sex
preserve
stset ageatfs [pweight=weight] if anasample, failure(eversex)
stsum if anasample, by(source)
sts graph if anasample, by(year) xlabel(10(5)30) noorigin tmin(10) tmax(30) xtitle(" ") title(" ") ///
	legend(off) graphregion(color(white)) ///
	plot1opts(lpattern(dash_dot) lcolor(black) lwidth(thin)) ///
	plot2opts(lpattern(solid) lcolor(black) lwidth(thin)) ///
	plot3opts(lpattern(shortdash) lcolor(black) lwidth(thin))

*Age at first marriage

stset ageatfm [pweight=weight] if anasample, failure(evermar)
stsum if anasample, by(source)
sts graph if anasample, by(year) xlabel(10(5)30) noorigin tmin(10) tmax(30) xtitle(" ") title(" ") ///
	legend(off) graphregion(color(white)) ///
	plot1opts(lpattern(dash_dot) lcolor(black) lwidth(thin)) ///
	plot2opts(lpattern(solid) lcolor(black) lwidth(thin)) ///
	plot3opts(lpattern(shortdash) lcolor(black) lwidth(thin)) 

*Age at first birth

stset ageatfb [pweight=weight] if anasample, failure(everbirth) 
stsum if anasample, by(source)
sts graph if anasample, by(year) xlabel(10(5)30) noorigin tmin(10) tmax(30) xtitle(" ") title(" ") ///
	legend(off) graphregion(color(white)) ///
	plot1opts(lpattern(dash_dot) lcolor(black) lwidth(thin)) ///
	plot2opts(lpattern(solid) lcolor(black) lwidth(thin)) ///
	plot3opts(lpattern(shortdash) lcolor(black) lwidth(thin))

//save insamp, replace
//use insamp, clear
//use insamp, clear

*Time from premarital sex to first marriage 
gen premasextomar = .
replace premasextomar = (ageatfm-ageatfs) if eversex2==1
replace premasextomar=. if premasextomar<=0
gen premasextomar2 = premasextomar
*ageatfm - includes ages for those married and not married. If not married, their current age is entered.

stset premasextomar2 [pweight=weight] if anasample & eversex2==1, failure(evermar)
stsum if anasample & eversex2==1, by(source)
sts graph if anasample&eversex2==1, by(year) xlabel(1(1)8) tmin(0) tmax(8) /// 
	xtitle("Risk period in years") title(" ") legend(cols(3) rows(1) size(small)) ///
	legend(region(lwidth(none))) graphregion(color(white)) ///
	plot1opts(lpattern(dash_dot) lcolor(black) lwidth(thin) msize(*3)) ///
	plot2opts(lpattern(solid) lcolor(black) lwidth(thin) msize(*3)) ///
	plot3opts(lpattern(shortdash) lcolor(black) lwidth(thin) msize(*3)) 
	
*******************************************************************
**Now I save my work from previous operations
*******************************************************************
//saving data before I expand it for the next analysis

save insamp2, replace

*=================================================================
*Next I am constructing the person files for the second model

tab eversex2 source if anasample, col
tab evermar source if anasample, col
gen evermar2 = 0 
gen id_n = _n
expand premasextomar
bysort id_n: gen pyear = _n
bysort id_n: replace evermar2 = evermar if _n == _N &premasextomar !=.&anasample==1
tab evermar2
gen pyearsq = pyear^2

logistic evermar2 i.reside i.educ i.religion i.source ib4.ethnicity pyear pyearsq if anasample&eversex2==1

order caseid pyear premasextomar evermar eversex,first
tab evermar2 if premasextomar !=.&anasample==1
tab eversex2 if premasextomar !=.&anasample==1
/*I open my previously saved data after the above expand operation
which damaged the data */

*Time from first marriage to first birth
use insamp2, clear 

/*Below are the analyses for values of the cells in the table entitled "Sexual intercourse, pre-marital birth, 
and time to reproductive event among women in Ethiopia".*/

//Descriptive statistics for the second panel in Table 1

svyset psuXyear [pweight=weight]

*Generating percentage eversex, ever married, everbirth in the age group included for the study.
svy, subpop(anasample): tab eversex2 source, col 
svy, subpop(anasample): tab evermar source, col
svy, subpop(anasample): tab everbirth source, col
*Note that evermar indicates ever in union young women.

*premarital birth

gen birthdiff = ageatfb-ageatfm if eversex==1&everbirth==1
generate premabirthcat=.
replace premabirthcat = 1 if birthdiff<0
replace premabirthcat = 0 if birthdiff>=0

svy, subpop(anasample): tab premabirthcat source if anasample, col

*rechecking values of the descriprive statistics table

***Tabulation to create table of descriptive statistics - SOCIOECONOMIC.***

svyset psuXyear [pweight=weight]

*Note that evermar indicates ever in union young women.

svy, subpop(anasample): tab v013 source, col
svy, subpop(anasample): tab educ source, col
svy, subpop(anasample): tab v025 source, col
svy, subpop(anasample): tab religion source, col
svy, subpop(anasample): tab ethnicity source, col
svy, subpop(anasample): tab reside source, col

*Contraceptive use by year and current marital status
svy, subpop(anasample): tab modern cumar if source==2000, col
svy, subpop(anasample): tab modern cumar if source==2005, col
svy, subpop(anasample): tab modern cumar if source==2011, col

*Contraceptive use among never married but with premarital sex
svy, subpop(anasample): tab modern if source==2000&evermar==0&eversex2==1, col
svy, subpop(anasample): tab modern if source==2005&evermar==0&eversex2==1, col
svy, subpop(anasample): tab modern if source==2011&evermar==0&eversex2==1, col

*premarital sex
svy, subpop(anasample): tab eversex2 source if anasample, col


****************************************************
*Descrete time logistic regression
****************************************************


order caseid young reside educ religion source ethnicity region age /// 
	wealth anasample eversex eversex2 ageatfs youngcat ///
	evermar ageatfm everbirth ageatfb modern source weight source strat psu-nomiss, first

*My laptop's memory is limited to the expand operation to work quickly so I drop variables tht I don't need

drop mmidx_21 - mm15_23
drop  s110e-  s623cc
drop   s623da- s628d
drop s1000a- s538d_6
drop  m1b_1- hw73_6
drop   v027 v028 v029 v030 v031 v032 v033 v034 v042 v043 v044  v119 awfactt awfactu awfactr awfacte v121 v122 v123 v124 v125 v127 v129 v128 v129
drop  v166- b16_20
drop  midx_1- m56_6
drop   v426- hw58_6
drop   s712a -g231ac_6
drop  s482 - s489ax
drop  s227cg-sy234b5
drop  vcol_1 - vcal_9
drop  v785a -  v786z
drop   v770a-v773x
drop   v762aa-v762bx
drop  v759a-v759i
drop   v744a-v744e
drop v532-s929d

*Restricting sample for analysis
drop if anasample == 0	

**Creating personfile data==0

order caseid age ageatfs eversex2, first

tab eversex2 if anasample
tab eversex2 source if anasample, col

gen eversex2n = 0 

gen id = _n

expand ageatfs
bysort id: gen pyear = _n
bysort id: replace eversex2n = eversex2 if _n == _N
drop if pyear <10  //removing ageatfs for those below the start of the follow up
tab eversex2n

*recoding educ as a time varying variable

codebook educ
replace educ = 1 if educ==2 & (pyear >=10 & pyear<15)

****Multivariate model

gen pyearsq = pyear^2

**Modle inluded in table

xi: svy: mlogit eversex2n i.reside i.educ i.religion ib4.ethnicity i.source pyear pyearsq, base(0) rr
xi: svy: mlogit eversex2n i.reside i.educ i.religion ib4.ethnicity i.source pyear pyearsq, base(2) rr
xi: svy: mlogit eversex2n i.source, base(0) rr
xi: svy: mlogit eversex2n i.source, base(1) rr
xi: svy: mlogit eversex2n i.source, base(2) rr

//Final model included in the paper
******************************************
//xi: svy: mlogit eversex2n i.reside i.religion i.educ*i.source ib4.ethnicity pyear pyearsq, base(0) rr
//xi: svy: mlogit eversex2n i.reside i.ethnicity i.educ*i.source ib4.ethnicity pyear pyearsq, base(1) rr

erase insamp2.dta, clear
	
