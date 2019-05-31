!  Copyright (c) 2011-2019  Marc van der Sluys - marc.vandersluys.nl
!   
!  This file is part of the Mortgage package,
!  see: https://github.com/MarcvdSluys/Mortgage
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see
!  <http://www.gnu.org/licenses/>.


!***********************************************************************************************************************************
program mortgage
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: syntax_print, quit_program,quit_program_error, find_free_io_unit,file_open_error_quit, file_read_error_quit
  use SUFR_text, only: d2s
  implicit none
  
  integer :: year, start_year,end_year, month,mnt_count, n_cycles, verbose, start_month,print_month, ip,ioStat
  real(double) :: total_loan,loan, desired_repay,repay_perc, interest_perc, total_time, house_price_tax
  real(double) :: repay,total_repay,add_repay_year, current_interest,total_interest
  real(double) :: monthly_payment,old_monthly_payment, max_monthly_expense,max_allowed_yearly_repay, convergence_factor
  real(double) :: gross_pay,gross_pay_tot, tax_mnt,tax_year, tax_perc,prop_tax_perc,net_interest,net_pay,net_pay_tot
  real(double) :: total_gross_pay,total_tax_mnt,total_net_interest,total_net_pay
  character :: inFile*(99), mortgage_type*(99), ioMsg*(99)
  logical :: example, final_run
  
  namelist /settings/ house_price_tax, total_loan, mortgage_type, interest_perc, total_time, repay_perc, tax_perc, &
       prop_tax_perc, add_repay_year,max_monthly_expense, max_allowed_yearly_repay, start_year,start_month,print_month, verbose
  
  call set_SUFR_constants()  ! Set libSUFR constants
  
  example = .true.  ! This is an example run, unless a settings file is selected
  
  ! Get input settings file name and read it:
  write(*,*)
  if(command_argument_count().eq.1) then
     call get_command_argument(1, inFile)
     
     ! Open and read settings file:
     call find_free_io_unit(ip)
     open(unit=ip,form='formatted', status='old', action='read', position='rewind', file=trim(inFile), iostat=ioStat)
     if(ioStat.ne.0) call file_open_error_quit(trim(inFile), 1, 1)  ! 1: input file, 1: status: not ok
     read(ip, nml=settings, iostat=ioStat, iomsg=ioMsg)
     if(ioStat.ne.0) call file_read_error_quit (inFile, 0, 1, 'mortgage', ioStat, ioMsg)  ! status=1: not ok
     close(ip)
     
     example = .false.  ! This is no example run
     
  else  ! Use default (example) values:
     call syntax_print('<Settings file>', 'mortgage computes monthly or yearly and total loan repayments and interest payments for annuity or linear mortgages')
     
     ! Default/example values:
     house_price_tax            =  150000.0   ! House price according to the tax authority (e.g. 200000)
     total_loan                 =  100000.0   ! Total initial loan for house (e.g. 100000)
     mortgage_type              =  "annuity"  ! Mortgage type: "annuity", "linear"
     interest_perc              =  3.00       ! Interest percentage (e.g. 3.0)
     total_time                 =  30.0       ! Total mortgage run time in years (30)
     repay_perc                 =  100.0      ! Percentage of the loan you have to repay in that time (e.g. 100)
     tax_perc                   =  40.0       ! Percentage of tax paid = fraction of unpaid interest (e.g. 40)
     prop_tax_perc              =  0.7        ! Percentage of tax added value of house over which you pay taxes in Nl (e.g. 0.7)
     add_repay_year             =  0.0        ! Yearly additional loan repayment (0)
     max_monthly_expense        =  0.0        ! Monthly amount of money you want to spend on housing - set to 0 to ignore
     max_allowed_yearly_repay   =  20.d0      ! Maximum allowed yearly additional repayment as a percentage of the original loan (20)
     start_year                 =  2010       ! Year where mortgage begins (e.g. 2010)
     start_month                =  9          ! Month where (assumed on the first day) the mortgage begins (1-12)
     print_month                =  12         ! Month to print yearly data for: 12 for end of year; =start_month for data every 12 months
     verbose                    =  3          ! Verbosity: 0-show end result, 1- +input, 2- +monthly costs per year, 3-+additional repayment per year, 4- +every month for first and last year, 5- +every month, 9 +convergence
     
     if(verbose.ge.1) write(*,'(/,A)') 'Example settings: '
  end if
  
  
  ! Check settings consistency:
  if(add_repay_year.gt.0.1d0 .and. max_monthly_expense.gt.0.1d0) call quit_program_error("add_repay_year and max_monthly_expense cannot BOTH be > 0", 1)
  
  
  ! Print settings:
  if(verbose.ge.1) then
     write(*,'(A,F12.2)')     ' House price:                    ', house_price_tax
     write(*,'(A,F12.2)')     ' Total loan:                     ', total_loan
     write(*,'(A,F12.2,A)')   '   fraction of price:            ', total_loan/house_price_tax*100,'%'
     write(*,'(A,A12)')       ' Mortgage type:                  ', trim(mortgage_type)
     write(*,'(A,F12.2,A)')   ' Interest percentage:            ', interest_perc,'%'
     write(*,'(A,F12.2,A)')   ' Run time:                       ', total_time,' years'
     write(*,'(A,F12.2,A)')   ' Desired repayments:             ', repay_perc,'%'
     write(*,'(A,F12.2,A)')   ' Tax percentage:                 ', tax_perc,'%'
     write(*,'(A,F12.2,A)')   ' Property tax percentage:        ', prop_tax_perc,'%'
     if(add_repay_year.gt.0.1d0)  write(*,'(A,F12.2,A)')        ' Additional yearly repay:           ', add_repay_year
     if(max_monthly_expense.gt.0.1d0) write(*,'(A,F12.2,A)')    ' Max. monthly expense:           ', max_monthly_expense
     write(*,'(A,F12.2,A)')   ' Maximum allowed yearly repay:   ', max_allowed_yearly_repay,'%'
     write(*,'(A,I12,A)')     ' Start year:                     ', start_year
     write(*,'(A,I12,A)')     ' Start month:                    ', start_month
     write(*,'(A,I12,A)')     ' Print month:                    ', print_month
     write(*,*)
  end if
  
  
  current_interest = total_loan * interest_perc / (100.d0*12.d0)
  !convergence_factor = 0.1d0  ! 0<cf<1, smaller for larger percentages
  convergence_factor = (1.d0/interest_perc)**(interest_perc/15.d0)  ! Seems to work well...
  
  
  ! Initial guess for monthly payment, needed for annuity mortgage:
  total_interest   = current_interest * total_time*12/2.d0
  desired_repay = total_loan*repay_perc/100.d0
  monthly_payment = (desired_repay+total_interest)/(total_time*12)
  
  
  n_cycles = 1
  final_run = .false.
  end_year = start_year + nint(total_time)  ! Continue one more year, since we typically don't start in month 1
  converge: do  ! While trying to converge...
     loan = total_loan
     total_repay = 0.d0
     total_interest = 0.d0
     total_gross_pay = 0.d0
     total_tax_mnt = 0.d0
     total_net_interest = 0.d0
     total_net_pay = 0.d0
     old_monthly_payment = monthly_payment  !< Use to converge annuity mortgages
     
     mnt_count = 0  !< Total month number
     yr: do year = start_year,end_year
        
        mnt: do month = 1,12
           if(year.eq.start_year .and. month.lt.start_month) cycle mnt  ! Mortgage hasn't started yet
           if(year.eq.end_year .and. month.ge.start_month) exit yr        ! Mortgage period has finished
           
           mnt_count = mnt_count + 1
           
           current_interest = loan * interest_perc / (100.d0*12.d0)
           
           select case(trim(mortgage_type))
           case("annuity")
              repay = monthly_payment - current_interest   ! For annuity, the repay fraction of the monthly payments is variable.  If we can, we repay the whole loan in what turns out to be the last month
              if(final_run) repay = min(repay, loan)    ! This would disturb the convergence process, since you never end up with a negative loan...
              
           case("linear")
              repay = total_loan / total_time / 12.d0     ! For linear, the monthly repay is fixed ...
              if(final_run) repay = min(repay, loan)    ! ...(except in the last month)...               -  This would disturb the convergence process, since you never end up with a negative loan.
              monthly_payment = repay + current_interest   ! ...and the monthly payments vary
              
           case default
              call quit_program_error("Wrong mortgage_type: "//trim(mortgage_type), 1)
           end select
           
           ! Pay this month's repayment and interest:
           if(.not.final_run .or. loan.gt.0.d0) then
              loan = loan - repay
              total_repay = total_repay + repay
              total_interest = total_interest + current_interest
           end if
           
           ! Compute gross, net, tax, etc. parts:
           gross_pay     = current_interest + repay             !< Money monthly paid to bank
           !tax_mnt      = max(current_interest*tax_perc/100.d0 - house_price_tax*prop_tax_perc/100.d0/12.d0*tax_perc/100.d0, 0.d0)  !< tax credit - added to income by IRS because of house ownership - cannot be < 0 - old
           tax_mnt       = house_price_tax*prop_tax_perc/100.d0/12.d0*tax_perc/100.d0 - current_interest*tax_perc/100.d0  !< property tax because of house ownership - tax credit = yearly tax / 12
           net_interest   = current_interest + tax_mnt             !< Effective monthly net *cost*.  Note that typically interest is paid per month and tax per year
           net_pay       = gross_pay + tax_mnt                   !< Effective monthly *payments*.  Note that typically gross pay is paid per month to the bank and tax per year to the state
           
           if(max_monthly_expense.gt.0.1d0) add_repay_year = max( min( (max_monthly_expense - monthly_payment - tax_mnt)*12,  total_loan*max_allowed_yearly_repay/100.d0 ), 0.d0)  ! Determine yearly additional repayment in order to achieve the maximum possible yearly payments
           net_pay_tot   = net_pay   + add_repay_year/12.d0  !< Total effective monthly payments, including additional (voluntary) repay
           gross_pay_tot = gross_pay + add_repay_year/12.d0  !< Total actual monthly payments to the bank, including additional (voluntary) repay
           tax_year      = tax_mnt * 12                      !< Actual yearly tax to pay to IRS
           
           
           ! Sum up totals and print yearly line:
           if(final_run) then
              ! Summing over total run time (e.g. 30 years):
              total_gross_pay    = total_gross_pay   + gross_pay
              total_tax_mnt      = total_tax_mnt     + tax_mnt
              total_net_interest  = total_net_interest + net_interest
              total_net_pay      = total_net_pay     + net_pay
              

              ! Print header line
              if(verbose.ge.2.and.year.eq.start_year.and.month.eq.start_month) &
                   write(*,'(A6,",", A3,",", A4,",", 13(A12,","))') '#year','mn','#', 'Loan','Interest', 'repay','Gross pay','Tax', &
                   'Net intrst','Net pay','Add. repay','- per year','Monthly pay','Yearly tax','Mnt.eff.pay'
              
              ! Print yearly line:
              if( &
                   (verbose.ge.2 .and. (month.eq.print_month .or. year.eq.start_year.and.month.eq.start_month .or. year.eq.end_year.and.month.eq.start_month-1 .or. loan.le.0.d0) ) &
                   .or. (verbose.ge.4 .and. (year.eq.start_year .or. year.eq.start_year+1.and.month.lt.start_month .or. year.eq.end_year-1.and.month.gt.start_month .or. year.eq.end_year)) &
                   .or. verbose.ge.5) &
                   
                   write(*,'(I6,",",I3,",",I4,",",13(F12.2,","))') year,month,mnt_count, loan,current_interest,repay,gross_pay, tax_mnt, &
                   net_interest,net_pay, add_repay_year/12.d0, add_repay_year, gross_pay_tot,tax_year, net_pay_tot
              
              if(loan.le.0.d0) exit yr
           end if  ! Final run
           
        end do mnt   ! month
        
        
        ! Pay yearly additional repayments:
        if(final_run .and. add_repay_year.gt.0.1d0) then
           repay = min(add_repay_year,loan)
           total_repay = total_repay + repay
           loan = loan - repay
           
           if(verbose.ge.3) write(*,'(A,I3,2(A,F12.2))') ' Additional repayment year',year-start_year+1,': ', repay, ',  new loan: ', loan
        end if
        
     end do yr  ! year
     
     
     if(final_run) exit converge  ! This was the last run to print the result - exit the convergence loop
     
     
     ! Adjust monthly payment in order to converge:
     if(trim(mortgage_type).eq."annuity") monthly_payment = monthly_payment + (loan-(total_loan-desired_repay))/(total_time*12) * convergence_factor
     
     
     ! Check convergence:
     if(abs(old_monthly_payment - monthly_payment) .lt. 1.d-4) final_run = .true.
     
     ! Print convergence info:
     if(verbose.ge.9) write(*,'(A,I6,99F12.2)') 'Old,new:',n_cycles,old_monthly_payment,monthly_payment,loan, &
          loan/(total_time*12*2), total_repay,total_loan,total_repay/total_loan*100, add_repay_year
     n_cycles = n_cycles + 1
     
     
     ! Can you afford this loan?
     if(max_monthly_expense.gt.0.1d0 .and. monthly_payment.gt.max_monthly_expense) then
        write(*,'(/,A,/)') '  *** YOU CANNOT AFFORD THIS LOAN ***'
        write(*,'(A,F12.2,A)') ' Gross monthly payment:  ',monthly_payment
        write(*,'(A,F12.2,A)') ' Max. monthly expense:   ',max_monthly_expense
        write(*,*)
        stop
     end if
     
     
     ! Computation does not seem to be converging:
     if(n_cycles.gt.1000) call quit_program('Computation converges too slowly - increase convergence_factor')
     if(abs(monthly_payment).gt.10000.d0) call quit_program('Computation diverges - decrease convergence_factor')
     
  end do converge  ! Convergence loop
  
  
  total_repay = total_repay + loan
  
  
  ! Print totals:
  if(verbose.ge.2) write(*,'(/,A6,",",3x,",",I4,",",9(F12.2,","))') 'Total',mnt_count, loan,total_interest,total_repay, &
       total_gross_pay,total_tax_mnt,total_net_interest, total_net_pay
  
  
  if(verbose.ge.1) then
     write(*,*)
     if(trim(mortgage_type).eq.'annuity') write(*,'(A,F14.4,A)') ' Gross monthly payment:       ', monthly_payment
     write(*,'(A,F12.2,A,A,A)') ' Additional yearly repayment: ', add_repay_year, '  (', d2s(add_repay_year/12.d0,2), ' per month)'
     write(*,'(A,F12.2,A)')     ' Total gross payments:        ', total_gross_pay
     write(*,'(A,F12.2,A)')     ' Total net payments:          ', total_net_pay
  end if
  
  write(*,'(A,F12.2,A)') ' Total tax (<0: credit):      ', total_tax_mnt
  write(*,'(A,F12.2,A)') ' Total cost (interest+tax):   ', total_net_interest
  write(*,'(A,F12.2,A)') ' Total relative cost:         ', (total_net_interest/total_loan)*100,'%'
  
  if(verbose.ge.1) write(*,'(I6,A)') n_cycles,' cycles needed to converge.'
  if(example) write(*,'(/,A)') ' Note: this was an example run - specify a settings file for a true run'
  write(*,*)
  
  
end program mortgage
!***********************************************************************************************************************************



