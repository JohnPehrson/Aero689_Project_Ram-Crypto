function [datetime_outs] = Time2DateTime(time_list)
%This function inputs a time in terms of YYYYMMDD and outputs it as a
%standard datetime

time_year = floor(time_list/1E4);
diff = time_list-(time_year*1E4);
time_month = floor(diff/1E2);
time_days = mod(diff,1E2);
datetime_outs = datetime(time_year,time_month,time_days);

end