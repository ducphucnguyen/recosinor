% read Calera from raw csv file
% step1: data clean for each pill

% Example df_all = f_readcalera('abc.csv')

% Version: SBS.0.0.1
% Version: SBS.0.0.2 (calculate angle)
% Date 24 Jan 2023
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function df_output = f_readcalera(fn)
plt = 1;
% read all data from csv file
df = readtable(fn, ...
    'VariableNamingRule', 'preserve',...
    'Range','A14');

df = readtable(fn);

%% Rename data table and convert units
allVars = 1:width(df);

df = renamevars(df,allVars,...
    ["time","timestamp", "hf_a0", "temp_a0","hf_a1","temp_a1",...
    "ax", "ay", "az", "battery_voltage", "cbt"]) ;



df.cbt = df.cbt/1000; % convert to oC
df.temp_a0 = df.temp_a0/1000;

const = sqrt(sum((df.ax.^2 + df.ay.^2 + df.az.^2),2));

df.ax = df.ax./const;
df.ay = df.ay./const;
df.az = df.az./const;

df.roll = atan2(df.ay, df.az )*57.3;
df.pitch = atan2(df.ax, sqrt(df.ay.^2 + df.az.^2) )*57.3;



%% Data cleaning

cbt = df.cbt;

% remove cbt drop and increase 0.4 oC per minute (0.2 per 30 seconds)
% 10 minutes before and after will be removed
t_thres = 0.1;
time_thres = 1;
id = find(abs( diff( cbt )) >= t_thres);

rm_id_1 = [];
for i=1:length(id)
    id_i = id(i)-time_thres*2 :  id(i)+time_thres*2;
    rm_id_1 = [rm_id_1, id_i];
end

% remove data <35 and >38.5 oC
rm_id_2 = find(cbt<=35 | cbt>=38.5);


% combine all remove index
rm_id = [rm_id_1, rm_id_2'];
rm_id = unique(rm_id); % remove possible overlaping range
id = (rm_id>=1 & rm_id<=height(df)); % negative index and max length
rm_id = rm_id(id);

df_all = df;
df_all.cbt(rm_id) = NaN; % artifact == nan

%% final output

df_output = df_all(:, [1 4 7 8 9 11 12 13]);
df_output = table2timetable(df_output);

%% Validation plots

if plt==1
    figure
plot(df.time, df.cbt); hold on
plot(df_output.time, df_output.cbt, '--')
end

end








