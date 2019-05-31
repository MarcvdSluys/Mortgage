# Mortgage #

`Mortgage` is a simple Fortran program that computes the monthly and yearly payments for a given mortgage,
taking into account tax deductions and property tax.  In particular, it was designed to solve annuity
mortgages iteratively.  In addition, you can use this program to see the effect of extra monthly payments or
provide an amount of money you wish to spend on housing and use all leftovers to repay your loan more
quickly, possibly saving more that you would expect on interest.


`Mortgage` was written by [Marc van der Sluys](http://marc.vandersluys.nl "Marc van der Sluys's homepage") and
can be used under the conditions of version 3 of the [GPL](http://www.gnu.org/licenses/gpl.html).


## Dependencies ##

`Mortgage` is written in Fortran and can be compiled using the free, open-source
[gfortran](http://gcc.gnu.org/wiki/GFortran), [g95](http://www.g95.org) and paid, commercial
[ifort](http://software.intel.com/en-us/fortran-compilers) compilers.  (You could then use `Mortgage` to
compute how much interest and interest on interest you would save over 30 years by making an early mortgage
repayment of the money you saved using open-source software.)  You will need to install the Fortran library
[libSUFR](http://libsufr.sourceforge.net) before compiling `Mortgage`.  A sample `Makefile` is provided in
`etc/Makefile`.


## Using `Mortgage` ##

Running `Mortgage` without settings file will remind you to use a settings file and show example output,
by default consisting of a listing of the input parameters, a table of yearly values (mostly for monthly
amounts of money for payment, interest, tax, etc.) and a summarising list of total numbers:

	Syntax:  mortgage  <Settings file>
	
	Example settings: 
	House price:                       150000.00
	Total loan:                        100000.00
	...etc...
	
	#year,mn,   #,        Loan,    Interest,       repay,   Gross pay,         Tax,  Net intrst,     Net pay,  Add. repay,  - per year, Monthly pay,  Yearly tax, Mnt.eff.pay,
	2010,  9,   1,    99828.40,      250.00,      171.60,      421.60,      -65.00,      185.00,      356.60,        0.00,        0.00,      421.60,     -780.00,      356.60,
	2010, 12,   4,    99311.01,      248.71,      172.89,      421.60,      -64.48,      184.23,      357.12,        0.00,        0.00,      421.60,     -773.81,      357.12,
	2011, 12,  16,    97202.25,      243.45,      178.15,      421.60,      -62.38,      181.07,      359.22,        0.00,        0.00,      421.60,     -748.56,      359.22,
	...
	2039, 12, 352,     3335.21,        9.37,      412.24,      421.60,       31.25,       40.62,      452.86,        0.00,        0.00,      421.60,      375.03,      452.86,
	2040,  8, 360,        0.01,        1.05,      420.55,      421.60,       34.58,       35.63,      456.18,        0.00,        0.00,      421.60,      414.95,      456.18,
	
	Total,   , 360,        0.01,    51777.45,   100000.00,   151777.45,    -8110.98,    43666.47,   143666.47,
		
	Gross monthly payment:          421.6040
	Extra yearly repayment:           0.00  (0.00 per month)
	Total gross payments:        151777.45
	Total net payments:          143666.47
	Total tax (<0: credit):       -8110.98
	Total cost (interest+tax):     43666.47
	Total relative cost:             43.67%
	13 cycles needed to converge.
		
	Note: this was an example run - specify a settings file for a true run

For useful output, grab the included settings file `mortgage_example_settings.dat`, rename it to something
sensible and fill out the parameters.  Then run the code specifying the file as input, e.g.: `$ ./mortgage
myHouse.dat`.


## Settings.dat ##

The file `settings.dat` contains the input parameters, with a description, which should be mostly self
explanatory.  However, notr that the variable `prop_tax_perc` is the percentage of the value of the part of
the house that you own (value - remaining loan) that can be taxed.  The actual tax is then `tax_perc` times
that value (/100).  It is assumed that `tax_perc` of the interest can be deducted from the income tax.  If the
tax regulations in your country are very different, you can always set all tax percentages to 0 and compute
gross values.


## Output variables ##

`Mortgage` prints plain text to the standard output (screen, usually) in formatted columns, including commas
in order to make it easier to create a csv file for further analysis.  While the listings at the top and
bottom should be readily interpreted, here is a brief description for the columns of the table in the body of
the output:

+ **year**:  Year number;
+ **mn**:    Month of year number;
+ **#**:  Line number/total month number;
+ **Loan**:  Remaining loan after payment this month;
+ **Interest**:  Interest paid to bank this month;
+ **Repay**:  Part of loan repaid to bank this month;
+ **Gross pay**:  Interest + loan repayment: total amount of money paid to bank this month;
+ **Tax**:  Tax credit - property tax this month;
+ **Net interest**:  Interest + tax this month;
+ **Net pay**:  Gross payment to bank + tax this month;
+ **Extra repay**:  Extra repayment made to bank this month (yearly amount / 12);
+ **- per year**:  Extra repayment made to bank this year to repay loan faster;
+ **Monthly pay**:  Amount of money actually paid this month: monthly interest + monthly repayment + extra
  repayment, but without taxes, which are typically paid per year;
+ **Yearly tax**:  Yearly taxes to be paid to your revenue agenc(y/ies);
+ **Mnt.eff.pay**:  Effective monthly payment this month: actual montly payment + yearly taxes / 12.

