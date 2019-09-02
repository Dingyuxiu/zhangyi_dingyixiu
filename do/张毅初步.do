*-------------------------------------------------------------
**         学生戴镜与心理健康  
**Goal:    视力矫正与学习焦虑和心理健康 & 学业表现
**Data:    Seeing is learning I data
**Author:  张毅
**Created:  20190614   
**Last Modified: 20190828
*-----------------------------------------------------------------
	
	capture	clear
	capture log close
	set	more off
	set memo 200m
		
*将数据集放在电脑上，相应代码放在github私有仓库中托管
	cd "/Users/zhangyi/Documents/数据集/看清未来/252数据/"
	global outdir "/Users/zhangyi/Documents/GitHub/ceee_vision/output"

	use eyeglasses_xibei_full.dta

	codebook schname,m //252所学校


*==================*
* data preparation *
*==================*
*年龄
	tab1 age cln_st3,m
	replace age = cln_st3
	label var age "Age(Year)"
	tab age,m

*male 
	tab  male,m
	gen female = male
	recode female 1=0 0=1
	label var female "Female,1=Yes"


*Baseline glasses wear 
	ta baseusage,m

*Baseline aware of being myopic
	tab aware0,m

*Baseline misinformation  
	tab harmvision0,m //baesline believe glasses harm vision

*Gansu Residence
	gen gansu=0
	replace gansu = 1  if province=="甘肃" 
	label var gansu "Gansu Residence"
	label values gansu yesno
	tab gansu,m

**Refractive Error (Diopters) cutoffs:
*SE2
	gen RE2 = 0 if missing(SE2) == 0
	replace RE2 = 1 if SE2 < -2 & missing(SE2) == 0
	replace RE2 = 2 if SE2 >= -2 & SE2 < -0.5 & missing(SE2) == 0
	replace RE2 = 3 if SE2 >= -0.5 & SE2 < 0.5 & missing(SE2) == 0
	replace RE2 = 4 if SE2 >=0.5 & missing(SE2) ==0 
	label define RE2 1 "<-2.00" 2 ">=-2.00 <-0.50" 3 ">=-0.50 <0.50" 4 " >=0.50", modify
	label values RE2 RE2

	label var RE2 "Refractive Error (Diopters) Based on SE2, most myopic eye"
	tab1 SE2 RE2,m

*VA of better eye <=6/18

	tab1 vision*,m
	gen va18 = 0 if missing(vision1) == 0
	replace va18 = 1 if vision1 > 18 & missing(vision1) == 0
	label var va18 "Baseline VA of Better Seeing Eye < 6/18"
	label values va18 yesno
	tab va18,m

	gen logmar = -log10(6/vision2)
	gen elogmar = -log10(6/evision2)
	gen deltalogmar = elogmar - logmar
	ta deltalogmar ,m
	label var logmar "Baseline_LogMAR"
	label var elogmar "Endline_LogMAR"
	label var deltalogmar "Change of LogMAR"
	format logmar elogmar deltalogmar %3.1f
	tab1 logmar elogmar deltalogmar


*round logmar to 1st digit where it is supposed to be
	foreach x of varlist logmar elogmar deltalogmar{
	replace `x' = round(`x',0.1)
	tab `x'
	}


*Refractive error
	rename RE2 RE 

	codebook RE

	label var RE "Refractive Error"
	gen RE1 = 0 if missing(RE) == 0
	replace RE1 = 1 if RE == 1
	label var RE1 "RE < -2.00"
	gen RE2 = 0 if missing(RE) == 0
	replace RE2 =1 if RE == 2
	label var RE2 "RE >= -2.00 <-0.50" 
	gen RE3 = 0 if missing(RE) == 0
	replace RE3 =1 if RE == 3 
	label var RE3 "RE >=-0.50 <= 0.5"
	gen RE4 = 0 if missing(RE) == 0
	replace RE4 =1 if RE == 4
	label var RE4 "RE >= 0.50"
	tab1 RE*

*baseline score on mathmatics test
	ta zmath,m
	egen math_cut = cut(zmath) ,group(4)
	table math_cut , contents(n zmath min zmath max zmath)
	label var math_cut "Math Score"

	gen best_score = 0 if missing(math_cut) == 0
	replace best_score = 1 if math_cut ==3
	label var best_score "top 25% tercile"
	label values best_score yesno
	tab best_score ,m 
	gen worst_score = 0 if missing(math_cut) ==0
	replace worst_score =1 if math_cut ==0
	label var worst_score "Bottom 25% tercile"
	label values worst_score yesno
	ta worst_score,m
**note:这里使用标准化数学成绩中间的50%样本作为参照



*One or both parents with >=12 years of education
	tab1 dadedu momedu
	codebook dadedu momedu
	gen pedu12 =0 if missing(dadedu) ==0 & missing(momedu)==0
	replace pedu12 =1 if (dadedu == 3 | momedu==3) & (missing(dadedu) == 0 & missing(momedu) == 0)
	label var pedu12 "One or Both Parents with >=12 Years of Edu"
	label values pedu12 yesno
	bro pedu momedu dadedu

	gen dadsch=0 if missing(dadedu)==0
	replace dadsch=1 if dadedu==3
	label var dadsch "Father has high school education or above"
*通过missing()生成dadsch更严谨

	gen momsch=0 if missing(momedu)==0
	replace momsch=1 if momedu==3
	*replace momsch=0 if momsch!=3 同上dadedu
	label var momsch "Mother has high school education or above"

**Boarding at school
	ta bboarding,m
	label var bboarding "Boarding at School"
	label values bboarding yesno
	ta bboarding,m

**Both parents out_migrated for work
	gen momout=est12
	recode momout 1=0 2=1
	gen dadout=est11
	recode dadout 1=0 2=1
	label values momout yesno
	label values dadout yesno




*clean missing value . cln_pa1 is who filled the parentform. if dad fill it out,the dad should be at home
	replace momout=0 if momout==. & cln_pa1==2
	replace momout=1 if momout==. & cln_pa1!=2
	replace dadout=0 if dadout==. & cln_pa1==1
	replace dadout=1 if dadout==. & cln_pa1!=1
	label var momout "Mother migranted for work"
	label var dadout "Father migranted for work"
	
	gen dadhome = 0
	replace dadhome=1 if dadout==0 & momout==1
	label var dadhome "Father at home,mother migrated"
	
	gen momhome=0
	replace momhome=1 if momout==0 & dadout==1
	label var momhome "Mother at home,father migrated"
	
	gen anyout=.
	replace anyout=1 if momout==1 | dadout==1
	replace anyout=0 if momout==0 & dadout==0
	label var anyout "Any parent migrated"
	
	gen bothout =0 if (missing(est12)==0 | missing(est11)==0)
	label var bothout "Both parents migranted for work"
	replace bothout=1 if momout==1 & dadout==1
*replace bothout=0 if bothout!=1  //存在疑问，==0 所生成的对吗？
	tab bothout,m
	
	sum momout dadout bothout
	
	
	gen bothhome=.
	replace bothhome=1 if momout==0 & dadout==0
	replace bothhome=0 if momout!=0 | dadout!=0
	label var bothhome "Children living with both parents"


*seat change 
	gen seatchange = est4
	label define seatchange 1"move forward"2"no change"3"move backward",modify
	label values seatchange seatchange
	label var seatchange "seat change between baseline and endline "
	ta seatchange,m

*fraction of teaching on blackboard
	ta emtea_14,m //您平时上课时给学生出的测试题有多大比例是写在黑板上的
	gen blackboard = 1 if emtea_14 == 4 | emtea_14 == 5
	replace blackboard = 2 if emtea_14 == 3
	replace blackboard = 3 if emtea_14 == 1 |emtea_14 ==2
	label define blackboard 1"less than half"2"half"3"more than half"
	label values blackboard blackboard
	label var blackboard "proportion of material taughe on blackboard"
	ta blackboard,m


*sibling 
	drop sibling
	gen older_sib = cln_st7
	label var older_sib "Number of older sibling"
	gen younger_sib = cln_st8
	label var younger_sib "younger sibling"
	gen sib = cln_st7 + cln_st8
	la var sib "Number of siblings" //兄弟姐妹个数

*only kid
	gen onlykid = .
	label var onlykid "Student is the only kids"
	replace onlykid =1 if cln_st7 ==0 &cln_st8 ==0
	replace onlykid =0 if onlykid !=1
	label values onlykid yesno

*Q36 : Do you usually blink your eyes or turn your head to see things clearly? 
	label var cln_st36 " Do you usually blink your eyes or turn your head to see things clearly?" //你平时要眯眼睛或侧着头才能看清楚东西吗？
	gen blink=. 
	replace blink = 0 if cln_st36 == 1
	replace blink = 1 if cln_st36 == 2 
	replace blink = 1 if cln_st36 == 3
	label values blink yesno

 
*Q37 :can you see the blackboard from your seat?  你平时在座位上能不能看得清楚黑板上的字？
	gen seat_vision = .
	label var seat_vision "student can see the blackboard from his/her seat"
	replace seat_vision = 1 if cln_st37==1
	replace seat_vision = 0 if cln_st37==2
	label values seat_vision yesno

*Q38:when do you start to have blurry vision?
	gen blurry_vision=real(cln_st38) //直接将字符型数字转换为数值型，文字变为缺失值
	label var blurry_vision "when do you start to have blurry vision ?" //单位为年级

*Q39 : seat change request
	tab cln_st39,m
	la var cln_st39 "Have you ever changed the blackboard to let the teacher change seats? 1=yes"

*Q40: Seat change teacher
	la var cln_st40 "Did the teacher change you to the front according to your request? 1=yes"

*Q41: Seat change family
	la var cln_st41 "Have you told your family that you can't see the blackboard in your seat? 1=yes"

*Q42: family check
	la var cln_st42 "Does the family take you to check your eyesight?, 1=yes"


*Q45:do you think you are near sighted? 你觉得你近视吗？
	gen selfns=0 if missing(cln_st45)==0
	label var selfns "student think he/she is near sighted"
	replace selfns = 1 if cln_st45 == 1 
	replace selfns = 0 if cln_st45 != 1 & selfns==0
	label values selfns yesno

*Q46:student think that wearing glasses is the best way to cure near sighter
	gen cure=0 if missing(cln_st46)==0
	label var cure "student think that wearing glasses is the best way to cure near sigher"
	label values cure yesno
	replace cure = 1 if cln_st46==4
	replace cure = 0 if cln_st46!=4 & cure==0


*Q48:appearance
	gen appearance = 0  if missing(cln_st48)==0
	replace appearance = 1 if cln_st48 ==3
	replace appearance = 0 if cln_st48 !=3 &appearance==0
	label var appearance "student think wearing glasses looks not good"
	label values appearance yesno


*Q49: do you have eyeglasses?
	label var cln_st49 "Do you have glasses?"
	label values cln_st49 yesno

*Q50: how many years have you been wearing glasses?
	label var cln_st50 "how many years have you been wearing glasses?"

*Q51 :do you wear glasses?
	gen wear = 0 if missing(cln_st51)==0
	replace wear = 1 if cln_st51==3
	replace wear = 0 if cln_st51!=3 & wear==0
	label var wear "student often wears glasses"
	label values wear yesno

*Q52 :student worries about wearing glasses
	gen worry=0 if missing(cln_st52)==0
	label var worry "student worries about wearing glasses"
	replace worry =1 if cln_st52==2 | cln_st52==3
	label values worry yesno

*Q53:学生应该经常检查视力
	gen Q53=0 if missing(cln_st53)==0
	replace Q53=1 if cln_st53==1
	label var Q53"学生应该经常检查视力"

*Q54：近视是看不清近处的东西
	gen Q54=0 if missing(cln_st54)==0
	replace Q54=1 if cln_st54==2
	label var Q54 "近视是看不清近处的东西"

*Q55:近视是因为眼球的形状变形造成的
	gen Q55=0 if missing(cln_st55)==0
	replace Q55=1 if cln_st55==1
	label var Q55 "近视是因为眼球的形状变形造成的"

*Q56 做眼保健操可以解决近视问题
	gen Q56=0 if missing(cln_st56)==0
	replace Q56=1 if cln_st56==2
	label var Q56 "做眼保健操可以解决近视问题"

*Q57: 近视问题可以通过戴眼镜解决
	gen Q57 = 0 if missing(cln_st57)==0
	replace Q57=1 if cln_st57==1
	label var Q57 "近视问题可以通过戴眼镜解决"


*Q58 近视不戴眼镜会影响学习
	gen Q58=0 if missing(cln_st58)==0
	replace Q58=1 if cln_st58==1
	label var Q58 "近视不戴眼镜会影响学习"
	label values Q58 yesno


	foreach x in 59 60 61 {
		gen Q`x' = 0 if missing(cln_st`x') == 0
		replace Q`x' = 1 if cln_st`x' == 2
		}

	label var Q59 "近视度数比较小的时候没必要配眼镜"
	label var Q60 "戴眼镜会导致视力越来越差"
	label var Q61 "小学生不应该戴眼镜"


*total
	egen knowledge = rowtotal(Q53-Q61)
	label var knowledge "Knowledge of myopia"

*============*
*==distance==*
*============*

	egen hhdis = cut(distance),group(4)
	table hhdis ,contents(n distance min distance max distance)
	label var hhdis "household distance"
	
	gen closedis = 0 if missing(hhdis)==0
	replace closedis = 1 if hhdis == 0
	label var closedis "close distance tercile" 
	label values closedis yesno
	
	gen fardis = 0 if missing(hhdis) == 0
	replace fardis = 1 if hhdis == 3 
	label var fardis "far distance tercile" 
	label values fardis yesno
	tab fardis,m
*note：此处将中等距离的样本作为参照点

*===============*
*=====wealth====*
*===============*
	egen hhwealth = cut(wealth),group(4)
	table hhwealth , contents(n wealth min wealth max wealth)
	label var hhwealth "Household wealth"
	
	gen hhpoor = 0 if missing(hhwealth) == 0 
	replace hhpoor = 1 if hhwealth == 0
	label var hhpoor "low tercile for wealth"
	label values hhpoor yesno
	tab hhpoor,m
	
	
	gen hhrich = 0 if missing(hhwealth) == 0
	replace hhrich = 1 if hhwealth == 3
	label var hhrich "Top tercile for wealth"
	label values hhrich yesno
	ta hhrich,m
*note：此处将中等收入家庭作为参照点

/*
/*creat asset measure  */
	misstable summarize cln_pa54-cln_pa69
		foreach var of varlist cln_pa54-cln_pa69{
			replace `var' =0 if `var'==.
		}
		polychoricpca cln_pa54-cln_pa69,nscore(1) score(ses)
	label var ses "Household assets (Indes)"

*/




*==================================================================
*=======================Gen group var=============================
*=======================Gen group var=============================
*==================================================================

*note:如果仅仅做ttest，那么最重要的还是进行分组，将不同组别清晰的划分出

*将baseline & endline 区分开来分析 [zmath MHT]
*{baseline} [zmath MHT ]
* 总体可划分为以下几个样本：不近视学生；近视学生[近视戴眼镜、近视不戴眼镜]
*1.不近视学生 VS 近视学生 
*2.不近视学生 VS 近视学生戴眼镜
*3.近视戴眼镜 VS 近视不戴眼镜
*但又有以下想法，直接分为两组1.视力健康[不近视，近视有眼镜但矫正好了]2.视力不健康[近视没眼镜，近视有眼镜但不达标]

///不建议用endline数据
*{endline} [ezmath eMHT] 
*首先明确：endline的样本只是收集了baseline中近视学生的视力状况
*endline能做近视矫正和近视未矫正学生之间的检验
*易红梅老师的定义：deﬁne poor vision as uncorrected visual acuity less than 6/12 in the better eye,
*关老师那篇：(the cutoff is a VA of either eye of less than or equal to 6/12, or 20/40)


*本次定义，参照前面的文章，以及这篇dofile中的信息，决定使用vision2来作为视力的表征  [Baseline_uncorrected VA in worse seeing eye]
*vision2  较差眼的未矫正视力
*VA 小于6/12 就认为视力不良,[vision2 >= 12]
*vision2 <12 则表明视力正常


*MHT:The larger the MHT score, the larger one's anxiety level



*只使用基线数据
*分三组，不近视，近视有眼镜，近视没眼镜


	gen give = 1 if givegl_st =="G"
	replace give = 0 if give ==.
	
	tab give,m

*G1

*group1 : 基线不近视，基线近视（控制组，干预组中【直接获得免费眼镜、获得优惠券】）
	gen group1 = 0 if  give==0 //基线不近视学生
	replace group1 = 1 if con==1 & give==1 //基线近视，控制组学生
	replace group1 = 2 if fre==1 & give==1 //基线近视，直接获得眼镜
	replace group1 = 3 if vou==1 & give==1 //基线近视，干预是获得优惠券
	tab group1,m

/*
*group 2
	gen group2 = 0 if give == 0 //基线不近视学生
	replace group2 = 1 if con==1 & give==1 //基线近视，控制组
	replace group2 = 2 if subsidies==1 & give ==1 //基线近视，眼镜兑换券 + 免费眼镜的学生 
	tab group2,m

*
	gen no_give = give 
	recode no_give 1=0 0=1
	lab var no_give "Not Eligible for Glasses"

*
	gen e_con = 0
	replace e_con =1 if con==1 & give==1
*/
*group3
	gen group3=0 if missing(vision2)==0
	replace group3 = 1 if vision2 <= 12 & group3 ==0 //视力正常
	replace group3 = 0 if vision2 > 12 & group3==0 //视力不正常


*group4
	gen group4 = 0 if missing(vision2)==0
	replace group4 = 1  if vision2 <=12 & group4==0
	replace group4 = 2  if vision2 > 12 & baseown==1  & group4==0
	replace group4 = 3  if vision2 > 12 & baseown!=1  & group4==0
	label define group4 1"基线不近视学生"2"基线近视有眼镜学生"3"基线近视没有眼镜"
	label values group4 group4

*group5
	gen group5 = 0 if missing(vision2)==0
	replace group5 = 1  if vision2 <=12 & group5==0
	replace group5 = 2  if vision2 > 12 & vision3!=.  & group5==0
	replace group5 = 3  if vision2 > 12 & vision3==. & group5==0
	label define group5 1"基线不近视学生"2"基线近视有眼镜学生"3"基线近视没有眼镜"
	label values group5 group5

*group6
	





*==============================================
*
*Baseline characteristics balance check across experimental group
*
*==============================================

*==========mental health===============*
	global mentalhealth mht studyanxiety personanxiety lonely guilty sensitive physical fear impulsive
	global ementalhealth emht estudyanxiety epersonanxiety elonely eguilty esensitive ephysical efear eimpulsive
	tab1 $mentalhealth $ementalhealth,m
	
	global baseline age female bboarding grade4 baseusage harmvision0 dadsch momsch bothout faglass distance 
	global banlance logmar aware0 baseusage harmvision0 glassestreat0 RE*


*基线，不近视学生，近视有眼镜，近视没眼镜
	iebaltab $mentalhealth , grpvar(group4) save($outdir/Table1.xls) replace rowvarlabel pttest
	iebaltab $mentalhealth , grpvar(group4) vce(cluster schid) save($outdir/Table1_1.xls) replace rowvarlabel pttest
	
	iebaltab $mentalhealth , grpvar(group5) save($outdir/Table2.xls) replace rowvarlabel pttest
	iebaltab $mentalhealth , grpvar(group5) vce(cluster schid) save($outdir/Table2_1.xls) replace rowvarlabel pttest
	
	iebaltab $baseline , grpvar(group5) save($outdir/Table3.xls) replace rowvarlabel pttest




akjfabvjbvk.jv




















